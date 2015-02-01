//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007, 2008.
//

// Patch file

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include "key.hh"

char *memsearch(char *filemap, unsigned char *pattern, int file_len, int pat_len)
{
    char *current = filemap;
    char *p;
    while(file_len - (current - filemap) > pat_len) {
	if(!memcmp(current, pattern, pat_len))
	    return current;

	++current;
    }

    return NULL;
}


/*
 *  This alogorithm is used to encrypt the compiled device key
 *  THIS CODE is replicate in patchkey.cc!!!! FIX changes
 *
 *  Encrypt pubkey with password and store it in 
 *  the location out_buf with max length len
 *
 *  The key data layout (Total 1024 bytes)
 *
 *     +------------------ +
 *     | Random data (16)  |
 *     +-------------------+
 *     | KO=Key offset (2) |  KO = [0-15] but stored as 16 bit
 *     +-------------------+
 *     | KL=Key len (2)    |  16 bit data
 *     +-------------------+
 *     | Random data (KO)  |
 *     +-------------------+
 *     |                   |
 *     |   Public Key      |
 *     |                   |
 *     +-------------------+
 *     |  Random data      |
 *     |                   |
 *     +-------------------+
 *
 */
int encrypt_pubkey(unsigned char* pubkey, unsigned int pubkey_len,
		   unsigned char* out_buf, unsigned int out_len,
		   unsigned char* pass, unsigned int pass_len)
{
    EVP_MD_CTX md_ctx;
    const EVP_MD* sha1 = EVP_sha1();
    const EVP_MD* md5  = EVP_md5();
    unsigned char md_buf[EVP_MAX_MD_SIZE*2];
    unsigned int md1_len;
    unsigned int md2_len;
    unsigned char key_buf[EVP_MAX_KEY_LENGTH];
    unsigned char iv_buf[EVP_MAX_IV_LENGTH];
    const EVP_CIPHER* crypto = EVP_des_ede3_cbc();
    EVP_CIPHER_CTX cipher_ctx;
    unsigned int key_len;
    unsigned int iv_len;
    unsigned char* enc_buf;
    int enc_len;
    unsigned char* enc_ptr;
    unsigned int key_offs;
    int outl;

    //
    // create a sha1+md5 of the password (input key) 
    // to generate a "real" key and iv of the correct size 
    // Gives 20+16 = 36 password data bytes
    //
    EVP_DigestInit(&md_ctx, sha1);
    EVP_DigestUpdate(&md_ctx, pass, pass_len);
    EVP_DigestFinal(&md_ctx, md_buf, &md1_len);
    EVP_MD_CTX_cleanup(&md_ctx);

    EVP_DigestInit(&md_ctx, md5);
    EVP_DigestUpdate(&md_ctx, pass, pass_len);
    EVP_DigestFinal(&md_ctx, md_buf+md1_len, &md2_len);
    EVP_MD_CTX_cleanup(&md_ctx);

    memset(key_buf, 0, sizeof(key_buf));
    memset(iv_buf,  0, sizeof(iv_buf));

    // Generate a random pubkey offset [0-15]
    RAND_bytes((unsigned char*)&key_offs, sizeof(key_offs));
    key_offs %= 0xf;
    
    // copy the md content to key and iv 
    key_len = EVP_CIPHER_key_length(crypto);
    iv_len  = EVP_CIPHER_iv_length(crypto);
    if (key_len + iv_len > md1_len+md2_len) {
	// To few byte for encrytption to work 
	fprintf(stderr, "key_len+iv_len>md_len\n");
	return -1;
    }
    memcpy(key_buf, md_buf, key_len);
    memcpy(iv_buf,  md_buf+key_len, iv_len);

    if (KEY_PATCH_PUBKEY_OFFSET+4+key_offs+pubkey_len > out_len) {
	fprintf(stderr, "public key to big to fit KEY_PATCH_PUBKEY_OFFSET[%d]+4+key_offs[%d]+pubkey_len[%d] > out_len[%d]\n",
		KEY_PATCH_PUBKEY_OFFSET, key_offs, pubkey_len, out_len);
	return -1;
    }
    
    if ((enc_buf = (unsigned char*) malloc(out_len)) == NULL) {
	fprintf(stderr, "unable to allocate %d bytes\n", out_len);
	return -1;
    }


    // Initialize encryption area with random bytes
    RAND_bytes(enc_buf, out_len);
    
    // Write the length & offset
    enc_buf[KEY_PATCH_PUBKEY_OFFSET]   = (key_offs & 0xff);
    enc_buf[KEY_PATCH_PUBKEY_OFFSET+1] = ((key_offs>>8) & 0xff);

    enc_buf[KEY_PATCH_PUBKEY_OFFSET+2] = (pubkey_len & 0xff);
    enc_buf[KEY_PATCH_PUBKEY_OFFSET+3] = ((pubkey_len>>8) & 0xff);

    // copy the public key
    memcpy(enc_buf+KEY_PATCH_PUBKEY_OFFSET+4+key_offs,
	   pubkey, pubkey_len);

    // Encrypt the enc_buf 
    enc_ptr = enc_buf;
    enc_len = 0;
    EVP_EncryptInit(&cipher_ctx, crypto, key_buf, iv_buf);
    // Disable padding!
    EVP_CIPHER_CTX_set_padding(&cipher_ctx, 0);
    EVP_CipherUpdate(&cipher_ctx, out_buf, &outl, enc_ptr, out_len);
    enc_len += outl;
    if (!EVP_EncryptFinal(&cipher_ctx, out_buf+enc_len, &outl)) {
	fprintf(stderr, "encryption final failed (check padding)\n");
	free(enc_buf);
	return -1;
    }
    enc_len += outl;

    EVP_CIPHER_CTX_cleanup(&cipher_ctx);

    if ((unsigned int)enc_len > out_len) {
	// Should never happend when len % 8 = 0!!!
	fprintf(stderr, "enc_len > out_len");
	return -1;
    }
    free(enc_buf);
    return enc_len;
}

void init()
{
    ERR_load_crypto_strings();
    OpenSSL_add_all_ciphers();
}

int main(int argc, char *argv[])
{
//     static char pattern[KEY_PATCH_FINGERPRINT_SIZE] = {
// 	KEY_PATCH_FINGERPRINT
//     };
    static unsigned char *oldkey = 0;
    unsigned char *newkey;
    int  newkey_len;
    int  enc_len;
    struct stat b_stat;
    struct stat n_stat;
    struct stat o_stat;
    int b_fd;
    int n_fd;
    int o_fd;
    char *b_start, *org_start;;
    struct timeval tv;

    init();

    // Usage packtst root installdb-dir packfile-dir
    if (argc < 5) {
	printf("Usage: %s binary newkey(devkey.bin) DOM-serial oldkey(demo.bin)\n", argv[0]);
	exit(-1);
    }

    // Get time of day, use microsec stamp as random number
    gettimeofday(&tv, 0);
    tv.tv_usec ^= getpid(); // Paranoid?
    srandom(((unsigned int) tv.tv_usec) & 0xFFFFFFFF);

    // Open binary file to patch
    if ((b_fd = open(argv[1], O_RDWR)) == -1) {
	perror(argv[1]);
	exit(-1);
    }
    
    if (fstat(b_fd, &b_stat) == -1) {
	fprintf(stderr, "Could not stat(2) %s: %s\n", argv[1], strerror(errno));
	exit(-1);
    }

    // Open old key file, which is in the binary today.
    if ((o_fd = open(argv[4], O_RDWR)) == -1) {
	perror(argv[4]);
	exit(-1);
    }

   
    if (fstat(o_fd, &o_stat) == -1) {
	fprintf(stderr, "Could not stat(2) %s: %s\n", argv[4], strerror(errno));
	exit(-1);
    }
    // Alloc and read old key file
    if (!(oldkey = (unsigned char *) malloc(o_stat.st_size + 1))) {
	perror("malloc");
	exit(-1);
    }
    if (read(o_fd, oldkey, o_stat.st_size) != o_stat.st_size) {
	fprintf(stderr, "Could not read old key file %s: %s\n", argv[4], strerror(errno));
	exit(-1);
    }
	
    close(o_fd);

    // Open patch file
    if ((n_fd = open(argv[2], O_RDONLY)) == -1) {
	perror(argv[2]);
	exit(-1);
    }

    if (fstat(n_fd, &n_stat) == -1) {
	fprintf(stderr, "Could not stat(2) %s: %s\n", argv[2], strerror(errno));
	exit(-1);
    }

    if (n_stat.st_size > o_stat.st_size) {
	fprintf(stderr, "new key size (%d) is greater than old key size (%d), which would overrun buffer in binary file %s. Abort.\n",
		n_stat.st_size, o_stat.st_size, argv[1]);
	exit(-1);
    }

    newkey_len = n_stat.st_size;
    if (!(newkey = (unsigned char *) malloc(newkey_len + 1))) {
	perror("malloc");
	exit(-1);
    }
	
    if (read(n_fd, newkey, newkey_len) < newkey_len) {
	fprintf(stderr, "new key file could not be read (%s)\n", strerror(errno));
	exit(1);
    }

    close(n_fd);

    // Map in binary file in address space.
    org_start = b_start = (char *) mmap(NULL, b_stat.st_size, PROT_READ | PROT_WRITE, MAP_SHARED, b_fd, 0); 
    if (b_start == MAP_FAILED) {
	fprintf(stderr, "Could not mmap(2) %s: %s\n", argv[1], strerror(errno));
	exit(-1);
    }
	
    if (!(b_start = memsearch(b_start, oldkey, b_stat.st_size, o_stat.st_size))) {
	fprintf(stderr, "No pattern match found in file %s\n", argv[1]);
	exit(-1);
    }

    enc_len = encrypt_pubkey(newkey, newkey_len, 
			     (unsigned char*) b_start, o_stat.st_size,
			     (unsigned char*) argv[3], strlen(argv[3])); // Password.

    if (enc_len < o_stat.st_size) {
	fprintf(stderr, "encryption failed\n");
	exit(1);
    }

    // Sync
    if (msync(org_start, b_stat.st_size, MS_SYNC) == -1) {
	perror("msync");
	exit(-1);
    }

    // Unmap
    if (munmap(org_start, b_stat.st_size) == -1) {
	perror("munmap");
	exit(-1);
    }

    close(b_fd);

    //    printf("%d bytes copied from %s to %s at address %p\n", o_stat.st_size, argv[2], argv[1], org_start);
    exit(0);
}
