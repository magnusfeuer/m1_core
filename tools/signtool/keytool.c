/*
 * All rights reserved. Reproduction, modification, use or disclosure
 * to third parties without express authority is forbidden.
 * Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/rand.h>
#ifndef DARWIN
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include "key.hh"
#include "bio_stream.hh"

/* Magden key generator 
 *
 * Key-Name:    <key-name>
 * Key-Serial:  <key-serial>
 * Key-Usage: key,image,m1,font,plugin,driver,exec | hex-code
 * Key-Size:  1024 | 2048 ...
 * Key-Public:   <keydata-base64>    ( Encryption: may be encrypted )
 * Key-Private:  <keydata-base64>
 * --- Added by signtool (-encrypt) ----
 * Key-Cipher: <cipher>
 * Key-Credentials: <credentials-hex>    ( encrypted with signer key )
 * --- Added by signtool ----
 * Signer-Id: <key-id>
 * Signature: <signature-hex>
 * ----------------------------
 *
 */
void usage()
{
    fprintf(stderr, "usage: keytool -name <name> [opts]\n"
	    "  opts:\n"
	    "   -debug        Debug output\n"
	    "   -size         RSA key size (1024)\n"
	    "   -serial       Set key serial number\n"
	    "   -password     Specify password for output files\n"
	    "   -h            Generate C header file\n"
	    "   -H            Generate C header file (password protected)\n"
	    "   -b            Generate raw output file\n"
	    "   -B            Generate raw output file (password protected)\n"
	    "   -u <item>,*   specify key usage e.g image,m1,font..\n"
	);
    exit(1);
}

void init()
{

    ERR_load_crypto_strings();
    OpenSSL_add_all_ciphers();
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
	fprintf(stderr, "public key to big to fit\n");
	return -1;
    }
    
    if ((enc_buf = (unsigned char*) malloc(out_len)) == NULL) {
	fprintf(stderr, "unable to allocate %d bytes\n", out_len);
	return -1;
    }

    // Make valgrind happy ?
    memset(enc_buf, 0, out_len);

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

/* usage: keytool options
 *   cmd:
 *     -size [<size>]       (Default -size 1024)
 *     -u <item>*           One or more key usage items
 *     -name <name>         The print name of the key
 *     -serial <serial>     The serial number
 *     -o fname             The file name (name is used if not given)
 *     -h                   Emit public key in a C header file
 *                          named <name>.h  as unsigned char data 
 *     -b                   Emit public key as raw binary in a file
 *                          <name>.bin
 *
 */

#define is_prefix(buf, str) (strncmp((buf), (str), strlen((str)))==0)
#define streq(a, b) (strcmp((a),(b)) == 0)

int debug = 0;

int main(int argc, char** argv)
{
    CKey*  key;
    string filename;
    string key_file;
    int    i;
    int    key_size = DEFAULT_KEY_SIZE;
    unsigned int key_usage = 0;
    int    hdr_file = 0;
    int    bin_file = 0;
    int    hdr_encrypted = 0;
    int    bin_encrypted = 0;
    char*  passwd = NULL;
    CBioStream* bo;
    FILE* f;
    unsigned char* dbuf = NULL;
    int   dlen;
    unsigned char  ebuf[KEY_PATCH_AREA_SIZE];
    int   elen;

    init();

    key = new CKey();
    i = 1;
    while(i < argc) {
	if (streq(argv[i], "-debug")) {
	    debug = 1;
	    i++;
	}
	else if (streq(argv[i], "-h")) {
	    hdr_file = 1;
	    i++;
	}
	else if (streq(argv[i], "-H")) {
	    hdr_file = 1;
	    hdr_encrypted = 1;
	    i++;
	}
	else if (streq(argv[i], "-b")) {
	    bin_file = 1;
	    i++;
	}
	else if (streq(argv[i], "-B")) {
	    bin_file = 1;
	    bin_encrypted = 1;
	    i++;
	}
	else if (streq(argv[i], "-size")) {
	    if (argv[i+1] && isdigit(argv[i+1][0])) {
		key_size = atoi(argv[i+1]);
		i++;
	    }
	    i++;
	}
	else if (streq(argv[i], "-name") && argv[i+1]) {
	    key->setName(string(argv[i+1]));
	    i += 2;
	}
	else if (streq(argv[i], "-serial") && argv[i+1]) {
	    key->setSerial(string(argv[i+1]));
	    i += 2;
	}
	else if (streq(argv[i], "-password") && argv[i+1]) {
	    passwd = argv[i+1];
	    i += 2;
	}
	else if (streq(argv[i], "-o") && argv[i+1]) {
	    key_file = string(argv[i+1]);
	    i += 2;
	}
	else if (is_prefix(argv[i], "-u")) {
	    char* ptr = argv[i] + 2;
	    i++;
	    if (*ptr == '\0') {
		if ((ptr = argv[i]) == NULL)
		    ptr = "";
		else
		    i++;
	    }
	    while(*ptr != '\0') {
		if (is_prefix(ptr, "key")) {
		    key_usage |= M1_KEY_USAGE_KEY;
		    ptr += 3;
		}
		else if (is_prefix(ptr, "content")) {
		    key_usage |= M1_KEY_USAGE_CONTENT;
		    ptr += 7;
		}
		else if (is_prefix(ptr, "os")) {
		    key_usage |= M1_KEY_USAGE_OS;
		    ptr += 2;
		}
		else if (is_prefix(ptr, "any")) {
		    key_usage |= M1_KEY_USAGE_ANY;
		    ptr += 3;
		}
		else if (is_prefix(ptr, "m1")) {
		    key_usage |= M1_KEY_USAGE_M1;
		    ptr += 2;
		}
		else if (is_prefix(ptr, "font")) {
		    key_usage |= M1_KEY_USAGE_FONT;
		    ptr += 4;
		}
		else if (is_prefix(ptr, "image")) {
		    key_usage |= M1_KEY_USAGE_IMAGE;
		    ptr += 5;
		}
		else if (is_prefix(ptr, "plugin")) {
		    key_usage |= M1_KEY_USAGE_PLUGIN;
		    ptr += 6;
		}
		else if (is_prefix(ptr, "exec")) {
		    key_usage |= M1_KEY_USAGE_EXEC;
		    ptr += 4;
		}
		else if (is_prefix(ptr, "driver")) {
		    key_usage |= M1_KEY_USAGE_DRIVER;
		    ptr += 6;
		}
		else if (isxdigit(*ptr)) {
		    long u;
		    char* endptr;
		    u = strtol(ptr, &endptr, 16);
		    if (endptr && ((*endptr == '\0')||(*endptr == ',')))
			key_usage |= u;
		    ptr = endptr;
		}
		else {
		    fprintf(stderr, "unknown usage type %s", ptr);
		    exit(-1);
		}
		if (ptr && *ptr == ',')
		    ptr++;
	    }
	}
	else {
	    usage();
	}
    }

    if (key->getName() == "") {
	fprintf(stderr, "keytool: -name is mandatory\n");
	exit(1);
    }
    if (key_usage == 0) {
	fprintf(stderr, "keytool: you need to use the for something (-u)\n");
	exit(1);
    }

    // Force seeding of PRNG 
    bo = new CBioStream();

    key->setUsage(key_usage);

    if (key->generate(key_size) < 0) {
	fprintf(stderr, "RSA key generation failed\n");
	exit(1);
    }
    if (key_file == "")
	key_file = key->getName();

    /* Save the private key */
    filename = key_file + ".key";
    if (bo->open_file_write(filename) < 0) {
	fprintf(stderr, "can not open file %s (%s)\n", 
		filename.c_str(), strerror(errno));
	exit(1);
    }
    if (key->write_private(bo) < 0) {
	fprintf(stderr, "can not save private key %s\n", filename.c_str());
	exit(1);
    }
    bo->close();

    filename = key_file + ".pub";
    if (bo->open_file_write(filename) < 0) {
	fprintf(stderr, "can not open file %s (%s)\n", 
		filename.c_str(), strerror(errno));
	exit(1);
    }
    if (key->write_public(bo, false) < 0) {
	fprintf(stderr, "can not save public key %s\n", filename.c_str());
	exit(1);
    }
    bo->close();

    dlen = key->i2d_public(&dbuf);

    /* Encrypt the public key */
    if (hdr_encrypted || bin_encrypted) {
	unsigned char* pass;
	unsigned int   pass_len;

	if (!passwd) {
	    pass = (unsigned char*) "DEMO";
	    pass_len = 4;
	}
	else {
	    pass = (unsigned char*) passwd;
	    pass_len = strlen(passwd);
	}

	elen = encrypt_pubkey(dbuf, dlen, ebuf, sizeof(ebuf),
			      pass, pass_len);
	if (elen<0) {
	    fprintf(stderr, "encryption failed\n");
	    exit(1);
	}
    }

    if (bin_file) {
	filename = key_file + ".bin";
	if ((f = fopen(filename.c_str(), "w")) == NULL) {
	    fprintf(stderr, "can not open file %s (%s)\n", 
		    filename.c_str(), strerror(errno));
	    exit(1);
	}
	if (bin_encrypted)
	    fwrite(ebuf, 1, elen, f);
	else
	    fwrite(dbuf, 1, dlen, f);
	fclose(f);
    }

    if (hdr_file) {
	unsigned char* ptr;
	int len;

	filename = key_file + ".h";
	if ((f = fopen(filename.c_str(), "w")) == NULL) {
	    fprintf(stderr, "can not open file %s (%s)\n", 
		    filename.c_str(), strerror(errno));
	    exit(1);
	}
	if (hdr_encrypted) {
	    ptr = ebuf;
	    len = elen;
	}
	else {
	    ptr = dbuf;
	    len = dlen;
	}
	fprintf(f, "/* Public key '%s' : generated by keytool */\n", 
		(key->getName()).c_str());
	fprintf(f, "static unsigned char PUBKEY_data[%d] = {\n  ",
		len);
	for (i = 0; i < (int) len-1; i++) {
	    if (i && ((i % 15)==0))
		fprintf(f, "\n  ");
	    fprintf(f, "0x%02X,", ptr[i]);
	}
	fprintf(f, "0x%02X };\n", ptr[len-1]);
	fclose(f);
    }
    free(dbuf);
    exit(0);
}

