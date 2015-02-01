//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#include <sqlcommon.hh>
#include <openssl/rand.h>
#include "key.hh"
#include "key_store.hh"

bool hasEncryptionSuffix(string aPath, CStringList &aList)
{
    for (CStringListIterator iter = aList.begin();
	 iter != aList.end();
	 ++iter) 
	if (aPath.size() >= iter->size() && !aPath.compare(aPath.size() - iter->size(), iter->size(), *iter)) 
	    return true;

    return false;
}

char *memsearch(char *start, unsigned char *pattern, int file_len, int pat_len)
{
    char *current = start;
    char *p;
    while(file_len - (current - start) > pat_len) {
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

bool patch_m1e_devkey(char *content, 
		      int content_len,
		      CKeyData *old_key,
		      CKeyData *new_key,
		      const char *passwd,
		      int passwd_len)

{ 
    int enc_len;
    char *key_start;

    if (!(key_start = memsearch(content, (unsigned char *) old_key->mBinKeyData, content_len, old_key->mBinKeyDataLength))) {
	printf("No %s key pattern found in file\n", old_key->mKeyName.c_str());
	return false;
    }

    enc_len = encrypt_pubkey((unsigned char *) new_key->mBinKeyData, new_key->mBinKeyDataLength, 
			     (unsigned char*) key_start, old_key->mBinKeyDataLength,
			     (unsigned char*) passwd, passwd_len); // Password.

    if (enc_len < old_key->mBinKeyDataLength) {
	printf("encryption failed\n");
	return false;
    }
    //    printf("Content had [%s] key replaced with [%s] using password[%s]\n", old_key->mKeyName.c_str(), new_key->mKeyName.c_str(), passwd);
    return true;
}


char *sign_pubkey(char *content, 
		  unsigned long content_size, 
		  unsigned long &new_content_size, 
		  CKeyData *magden_key_data, 
		  CKey *dev_key_obj)
{
    CBioStream bo, bi;
    CKey magden_pub_key;
    char *ptr;
    long len;
    // Setup magden public key
    bi.open_mem_read(magden_key_data->mKeyName.c_str(), magden_key_data->mPubKeyData, magden_key_data->mPubKeyDataLength, 0, false, false);
 
    if (!magden_pub_key.read_key(&bi, false)) {
	printf("sign_pubkey(): Could not read public magden key from data block:\n%s\n", magden_key_data->mPubKeyData);
	return 0;
    }
    
    // Sign magden key with private key.
    //    printf("Setting fingerprint[%s]\n", dev_key_obj->fingerprint().c_str());
    magden_pub_key.setSigner(dev_key_obj->fingerprint());
    // Setup output
    bo.open_mem_write("magden.pub");
    magden_pub_key.write_public(&bo, true);
    bo.flush();
    bi.close();
    // Copy the memory used by bo to content/content_size
    len = BIO_get_mem_data(bo.get_bio(), &ptr);
    if (len > content_size) {
	if (content)
	    free(content);

	content = (char *) malloc(len);
	new_content_size = len;
    }
    memcpy(content, ptr, len);
    bo.close();

    return content;

}



CKey *setup_priv_key(MYSQL *db, string key_name)
{
    CKeyData key_data;
    CKey *res;

    // Load given that we are to encypt with
    if (!db_get_encryption_key(db, key_name, key_data)) {
	fprintf(stderr, "Could not load encryption key [%s]\n", key_name.c_str());
	return 0;
    }

    // Add magden key to key store.
    m1_keys().load_key_mem(key_data.mPrivKeyData, key_data.mPrivKeyDataLength);
    //	m1_keys().load_key_mem(enc_key_data.mPubKeyData, enc_key_data.mPubKeyDataLength);

    //
    // Locate encryption key object of the key added above
    //
    if (!(res = m1_keys().key_by_name(key_name.c_str()))) {
	fprintf(stderr, "Encryption key '%s' not found", key_name.c_str());
	return 0;
    }
    return res;
}
