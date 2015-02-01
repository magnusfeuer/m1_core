//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <unistd.h>

#if defined(M1_DRM) && !defined(DARWIN)
#include <linux/hdreg.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#endif

#include "key.hh"
#include "key_store.hh"
#include "key_util.hh"


//
// Create an empty key
//
CKey::CKey()
{
    mPKEY = NULL;
    mKeyData = NULL;
    mCredData = NULL;
    mSigData = NULL;
    mResolved = false;
}

//
// Code used to verify/decrypt various data files
// Also handles key storage
//

CKey::CKey(string aName,string aSerial, string aSigner,
	   unsigned int aUsage, bool aIsPrivate, 
	   EVP_PKEY* aPKEY)
{
    mName     = aName;     // The key name
    mSerial   = aSerial;   // The key serial number
    mSigner   = aSigner;   // The signer hex finger-print
    mUsage    = aUsage;    // Key usage

    mPKEY        = aPKEY;
    mFingerPrint = make_fingerprint();
    mIsPrivate   = aIsPrivate;
    
    mResolved = (aPKEY != NULL);
    mKeyData = NULL;
    mCredData = NULL;
    mSigData = NULL;
}

CKey::~CKey()
{
    if (mPKEY)
	EVP_PKEY_free(mPKEY);
    m1Release(OctetBuffer, mKeyData);
    m1Release(OctetBuffer, mCredData);
    m1Release(OctetBuffer, mSigData);
}

string CKey::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "Key:%s #%lu 0x%lx", 
	    mName.c_str(), refCount(), (unsigned long) this);
    return string(ndata);
}

// Generate RSA key of size bits
int CKey::generate(int size)
{
    RSA* rsa;

    rsa = RSA_generate_key(size,DEFAULT_RSA_EXPONENT,NULL,NULL);
    if (rsa == NULL)
	return -1;
    if (mPKEY) {
	EVP_PKEY_free(mPKEY);
	mPKEY = NULL;
    }
    mPKEY = EVP_PKEY_new();
    EVP_PKEY_assign_RSA(mPKEY, rsa);
    mIsPrivate   = true;
    mFingerPrint = make_fingerprint();
    return 0;
}

//
// This is the key storage to get patched.
//

//
// Public key. Will be patched by patchkey, which will search the binary for the pattern below.
// In an unpatched system this data MUST an demo key encrypted
// with the password "DEMO"
//

#include "../keys/demo.h"


/*
 *  This alogorithm is used to decrypt the compiled device key
 *  The encryption routine is located in:
 *    keytool.cc   - generate the demo.h!!! 
 *    patchkey.cc  - patch the m1e binary with real device keys
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
static int decrypt_pubkey(unsigned char* pubkey_buf, int pubkey_buf_len,
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
    unsigned char dec_buf[KEY_PATCH_AREA_SIZE];
    int dec_len;
    unsigned char* dec_ptr;
    int key_offs;
    int outl;
    int pubkey_len;

    // Generate the pasword data (36 byte - need at least 3*8 + 8=24)
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

    // copy the md content to key and iv 
    key_len = EVP_CIPHER_key_length(crypto);
    iv_len  = EVP_CIPHER_iv_length(crypto);
    if (key_len + iv_len > md1_len+md2_len)
	return -1;
    memcpy(key_buf, md_buf, key_len);
    memcpy(iv_buf,  md_buf+key_len, iv_len);

    // Encrypt the enc_buf 
    dec_ptr = dec_buf;
    dec_len = 0;
    EVP_DecryptInit(&cipher_ctx, crypto, key_buf, iv_buf);
    // Disable padding!
    EVP_CIPHER_CTX_set_padding(&cipher_ctx, 0);
    EVP_CipherUpdate(&cipher_ctx, dec_buf, &outl, 
		     PUBKEY_data, sizeof(PUBKEY_data));
    dec_len += outl;
    if (!EVP_DecryptFinal(&cipher_ctx, dec_buf+dec_len, &outl)) {
	printf("Could not decrypt compiled in public key\n");
	return -1;
    }
    dec_len += outl;
    EVP_CIPHER_CTX_cleanup(&cipher_ctx);

    // Read the length & offset
    key_offs = dec_buf[KEY_PATCH_PUBKEY_OFFSET] + 
	(dec_buf[KEY_PATCH_PUBKEY_OFFSET+1] << 8);
    pubkey_len = dec_buf[KEY_PATCH_PUBKEY_OFFSET+2] + 
	(dec_buf[KEY_PATCH_PUBKEY_OFFSET+3] << 8);
    
    if (KEY_PATCH_PUBKEY_OFFSET+4+key_offs+pubkey_len > dec_len)
	return -1;
    if (pubkey_len > pubkey_buf_len)
	return -1;
    memcpy(pubkey_buf, dec_buf+KEY_PATCH_PUBKEY_OFFSET+4+key_offs, pubkey_len);
    return pubkey_len;
}

// Only do DRM if requested and not on darwin.
#if defined(M1_DRM) && !defined(DARWIN)
static unsigned int device_password(unsigned char* buf, unsigned int buf_len)
{
    int hd_des = open("/dev/hda", O_RDONLY);
    struct hd_driveid id;
    int len;

    if (hd_des == -1) 
	return 0;

    // Simsalabim.
    if (ioctl(hd_des, HDIO_GET_IDENTITY, &id) == -1) {
	close(hd_des);
	return 0;
    }
	    
    close(hd_des);

    if (id.serial_no[0] == 0)
	return 0;

    // Protect against buf overflow.
    len = (sizeof(id.serial_no)>buf_len)?buf_len:sizeof(id.serial_no);

    // Copy.
    memcpy(buf, id.serial_no, len); 

    // debug add 0
    buf[len] = 0;
    
    return len;
}
#else
static unsigned int device_password(unsigned char* buf, unsigned int buf_len)
{
    strcpy((char *) buf, "DEMO");
    return 4;
}
#endif

#ifdef DEBUG
#define DBG_WRITE(...) dbg_write(__VA_ARGS__)
// device_key is run in static constructor sometimes
static void dbg_write(const char* fmt, ...)
{
    va_list ap;
    char buf[1024];

    va_start(ap, fmt);
    vsnprintf(buf, sizeof(buf), fmt, ap);
    va_end(ap);
    write(2, buf, strlen(buf));
}
#else
#define DBG_WRITE(...)
#endif

//
// Extract the built-in device key.
//
CKey* CKey::device_key(void)
{
    EVP_PKEY* pkey;
    unsigned char pbdata[KEY_PATCH_AREA_SIZE];
    int pbkey_len;
    const unsigned char* ptr;
    unsigned char password[KEY_MAX_PASSWORD_SIZE];
    unsigned int pass_len;

    pass_len = device_password(password, sizeof(password));
	
    // DBG_WRITE("device_key: password: '%s'\n", password);

    pbkey_len = decrypt_pubkey(pbdata, sizeof(pbdata),
			       password, pass_len);
    // DBG_WRITE("device_key: pubkey_len=%d\n", pbkey_len);
    memset(password, 0, sizeof(password));
    ptr = pbdata;
    pkey = d2i_PublicKey(EVP_PKEY_RSA, NULL, &ptr, pbkey_len);
    // DBG_WRITE("device_key: pkey=%p\n", pkey);
#if defined(M1_DRM) && !defined(DARWIN)
    return new CKey("devkey", "", "", M1_KEY_USAGE_ANY, false, pkey);
#else
    return new CKey("demo", "", "", M1_KEY_USAGE_ANY, false, pkey);
#endif
}


// Generate ASN.1 encoded form of key (MUST be free'd with OPENSSL_free !)
int CKey::i2d_public(unsigned char** ptr)
{
    if (mPKEY)
	return i2d_PublicKey(mPKEY, ptr);
    *ptr = NULL;
    return -1;	
}

int CKey::i2d_private(unsigned char** ptr)
{
    if (mPKEY && mIsPrivate)
	return i2d_PrivateKey(mPKEY, ptr);
    *ptr = NULL;
    return -1;
}

int CKey::keySize(void)
{
    if (mPKEY)
	return EVP_PKEY_size(mPKEY);
    return 0;
}

RSA* CKey::rsaKey(void)
{
    if (mPKEY)
	return mPKEY->pkey.rsa;
    return NULL;
}

EVP_PKEY* CKey::PKEY(void)
{
    return mPKEY;
}

void CKey::setPKEY(EVP_PKEY* aPKEY, bool aIsPrivate)
{
    if (mPKEY)
	EVP_PKEY_free(mPKEY);
    mPKEY = aPKEY;
    mIsPrivate = aIsPrivate;
    mFingerPrint = make_fingerprint();
}
//
// Digest a key 
//
// The signature is calculated by:
//  Hash = SHA-1 ( key-Id + Key-Usage + signer-fingerprint + binary(Key-RSA)
//  Then Hash is RSA_verifi'ed with the signature with the signers key
//
//  The signer key must have Key-Usage: key inorder to verify a key.
//  Key-Usage: content is for keys that can verify content only
// 
//  The key use can only be a subset of what the signer key has
//  so the the use mask is always bit anded with signers use mask 
//  before the operation.
// 
//  content = image,code,font... (all except key,driver)
//  os
//  all     = full key usage
//

int CKey::digest(EVP_MD_CTX* ctx, CKey* signer, unsigned char* data, int len)
{
    unsigned char buf16[2];
    string signer_id = signer->fingerprint();

    // Add Key-id
    EVP_DigestUpdate(ctx, mName.c_str(), mName.length());

    // Add Key-Usage as big endian 16 bit
    buf16[0] = (mUsage >> 8) & 0xff;
    buf16[1] = mUsage & 0xff;	
    EVP_DigestUpdate(ctx, buf16, 2);
    
    // Add Signer-Id
    EVP_DigestUpdate(ctx, signer_id.c_str(), signer_id.length());

    // Add Key asn1 encoded key data
    EVP_DigestUpdate(ctx, data, len);
    return 0;
}

int CKey::digest(EVP_MD_CTX* ctx, CKey* signer)
{
    unsigned char* data;
    int len;
    int r;

    if ((len = i2d_public(&data)) < 0)
	return -1;
    r = digest(ctx, signer, data, len);
    free(data);
    return r;
}

int CKey::verify(EVP_MD_CTX* ctx, unsigned char* sigbuf, unsigned int siglen)
{
    return EVP_VerifyFinal(ctx,sigbuf,siglen,mPKEY);
}

//
// Generate signature: return length of signature on sucess, -1 on failure
//
int CKey::signature(EVP_MD_CTX* ctx, unsigned char* sigbuf, unsigned int siglen)
{
    unsigned int len = siglen;
    if (EVP_SignFinal(ctx,sigbuf,&len,mPKEY))
	return (int) len;
    return -1;
}

int CKey::decrypt(unsigned char* src, int src_len, unsigned char* dst)
{
    if (!mPKEY)
	return -1;
    return RSA_public_decrypt(src_len,src,dst,mPKEY->pkey.rsa,
			      RSA_PKCS1_PADDING);
}

int CKey::encrypt(unsigned char* src, int src_len, unsigned char* dst)
{
    if (!mPKEY)
	return -1;
    return RSA_private_encrypt(src_len,src,dst,mPKEY->pkey.rsa,
			       RSA_PKCS1_PADDING);
}

//
// Generate ascii hex finger print on form xx:xx:xx:
// return length of finger print
//
string CKey::make_fingerprint(void)
{
    EVP_MD_CTX ctx;
    unsigned char md[EVP_MAX_MD_SIZE];
    unsigned int md_len;
    char buf[EVP_MAX_MD_SIZE*3];
    unsigned char* dbuf = NULL;
    int dlen;

    if (((dlen = i2d_public(&dbuf)) < 0) || (dbuf == NULL))
	return "";
    
    EVP_MD_CTX_init(&ctx);
    EVP_DigestInit_ex(&ctx, M1_FINGER_ALGORITHM, NULL);

    EVP_DigestUpdate(&ctx, dbuf, dlen);
    EVP_DigestFinal(&ctx, md, &md_len);
    OPENSSL_free(dbuf);

    EVP_MD_CTX_cleanup(&ctx);

    if (encode_fingerprint(md, md_len, buf, sizeof(buf)) <= 0)
	return "";
    return string(buf);
}

static string concat(string s1, char delim, string s2)
{
    if (s1 == "")
	return s2;
    else
	return s1 + delim + s2;
}

string CKey::make_usage(void)
{
    string str;

    if ((mUsage & M1_KEY_USAGE_ANY) == M1_KEY_USAGE_ANY)
	str = "any";
    else {
	if (mUsage & M1_KEY_USAGE_KEY)
	    str = concat(str, ',', "key");
	if ((mUsage & M1_KEY_USAGE_CONTENT) == M1_KEY_USAGE_CONTENT)
	    str = concat(str, ',', "content");
	else {
	    if (mUsage & M1_KEY_USAGE_M1)
		str = concat(str, ',', "m1");
	    if (mUsage & M1_KEY_USAGE_FONT)
		str = concat(str, ',', "font");
	    if (mUsage & M1_KEY_USAGE_IMAGE)
		str = concat(str, ',', "image");
	    if (mUsage & M1_KEY_USAGE_PLUGIN)
		str = concat(str, ',', "plugin");
	}
	if ((mUsage & M1_KEY_USAGE_OS) == M1_KEY_USAGE_OS)
	    str = concat(str, ',', "os");
	else {
	    if (mUsage & M1_KEY_USAGE_EXEC)
		str = concat(str, ',', "exec");
	    if (mUsage & M1_KEY_USAGE_DRIVER)
		str = concat(str, ',', "driver");
	}
    }
    return str;
}

static int write_key_value(CBioStream* bf, string key, string value)
{
    bf->write((void*)key.c_str(), key.length());
    bf->write((void*) ": ", 2);
    bf->write((void*) value.c_str(), value.length());
    bf->write((void*) "\n", 1);
    return key.length() + 2 + value.length() + 1;
}

static int encrypt_key(CKey* signer, CKey* key, 
		       unsigned char* cipher_key,
		       int   cipher_key_len,
		       unsigned char* cipher_iv,
		       int   cipher_iv_len,
		       unsigned char* key_data,
		       unsigned int   key_len,
		       unsigned char** enc_data)
{
    const EVP_CIPHER* cipher;
    EVP_CIPHER_CTX cipher_ctx;
    RSA* rsa;
    unsigned char* out;
    unsigned char* outbuf;
    int outbuf_size = 0;
    int outl;
    int outlen = 0;

    if ((rsa = signer->rsaKey()) == NULL) {
	fprintf(stderr, "Signer is not a RSA key\n");
	return -1;
    }
    /* key_len + iv_len <= 48 bytes ! */
    if (cipher_key_len + cipher_iv_len > RSA_size(rsa)) {
	fprintf(stderr, "Credentials has bad length\n");	
	return -1;
    }

    cipher = M1_CIPHER_ALGORITHM;
    EVP_CIPHER_CTX_init(&cipher_ctx);
    if (!EVP_EncryptInit_ex(&cipher_ctx, cipher, NULL,cipher_key, cipher_iv)) {
	fprintf(stderr, "unable to init encryption context\n");
	goto error;
    }

    outbuf_size = key_len + 2*EVP_CIPHER_block_size(cipher);
    outbuf = (unsigned char*) malloc(outbuf_size);
    out = outbuf;
    outlen = 0;
    
    /* encrypt key_data + key_len */
    EVP_EncryptUpdate(&cipher_ctx, out, &outl, key_data, key_len);
    outlen += outl;
    out += outl;
    EVP_EncryptFinal_ex(&cipher_ctx, out, &outl);
    outlen += outl;

    *enc_data = outbuf;
    EVP_CIPHER_CTX_cleanup(&cipher_ctx);
    return outlen;
error:
    ERR_print_errors_fp(stderr);
    ERR_clear_error();
    EVP_CIPHER_CTX_cleanup(&cipher_ctx);
    return -1;
}

//
// Decrypt & verify a key give signer key
//
bool CKey::resolve_verify(CKey* signer, bool verify,
			  unsigned char* sig_data,
			  int            sig_len,
			  unsigned char* cred_data,
			  int            cred_len,
			  unsigned char* key_data,
			  int            key_data_len)
{
    unsigned char xpubkey[4096];
    int           xpubkey_len;

    if (isPublic() && (mKeyCipher != "")) {
	const EVP_CIPHER* cipher;
	EVP_CIPHER_CTX cipher_ctx;
	unsigned char cipher_key[EVP_MAX_KEY_LENGTH];
	unsigned char cipher_iv[EVP_MAX_IV_LENGTH];
	int cipher_key_len;
	int cipher_iv_len;
	unsigned char to[1024];
	int outl;

	cipher = EVP_get_cipherbyname(mKeyCipher.c_str());
	cipher_key_len = EVP_CIPHER_key_length(cipher);
	cipher_iv_len = EVP_CIPHER_iv_length(cipher);
	EVP_CIPHER_CTX_init(&cipher_ctx);

	if (!signer || signer->decrypt(cred_data, cred_len, to) < 0) {
	    ERRFMT("could not decrypt credentials");
	    return false;
	}

	memcpy(cipher_key, to, cipher_key_len);
	memcpy(cipher_iv, to+cipher_key_len, cipher_iv_len);

	if (!EVP_DecryptInit_ex(&cipher_ctx, cipher, NULL,
				cipher_key, cipher_iv)) {
	    ERRFMT("could not initialize decryption");
	    return false;
	}

	xpubkey_len = 0;
	EVP_DecryptUpdate(&cipher_ctx,xpubkey,&outl,key_data,key_data_len);
	xpubkey_len += outl;
	EVP_DecryptFinal_ex(&cipher_ctx,xpubkey+outl, &outl);
	xpubkey_len += outl;

	key_data = xpubkey;
	key_data_len = xpubkey_len;
    }

    if (isPublic() && signer && verify && key_data_len) {
	EVP_MD_CTX ctx;
	int result;

	// Verify public key data
	EVP_MD_CTX_init(&ctx);
	EVP_VerifyInit_ex(&ctx, M1_DIGEST_ALGORITHM, NULL);
	digest(&ctx, signer, key_data, key_data_len);
	result=signer->verify(&ctx,sig_data,sig_len);
	EVP_MD_CTX_cleanup(&ctx);

	if (result <= 0) {
	    ERRFMT("key '%s' could not be verified by '%s' fingerprint='%s'",
		   mName.c_str(), signer->getName().c_str(), 
		   mSigner.c_str());
	    return false;
	}
    }

    if (isPrivate()) {
	const unsigned char* key_ptr = key_data;
	EVP_PKEY* pkey;
	pkey = d2i_PrivateKey(EVP_PKEY_RSA, NULL, &key_ptr, key_data_len);
	setPKEY(pkey, true);
    }
    else { 
	const unsigned char* key_ptr = key_data;
	EVP_PKEY* pkey;
	pkey = d2i_PublicKey(EVP_PKEY_RSA, NULL, &key_ptr, key_data_len);
	if (!pkey)  {
	    long err;
	    char errmsg[121];
	    while((err = ERR_get_error())) {
		ERR_error_string(err, errmsg);
		DBGFMT("d2i_PublicKey(%s) failed: %s",  mName.c_str(), errmsg);
	    }
	}
	    
	    
	setPKEY(pkey, false);
    }
    mResolved = true;
    return true;
}




// Load keys from a directory
// Each file contain:
//
// Key-Name:    <key-id>         (was Key-Id)
// Key-Serial:  <serial-number>
// Key-Usage: all,content,os,key,image,m1,font,plugin,driver,exec | hex
// Key-Size:  1024 | 2048 ...
// Key-Public:   <keydata-base64>
// Key-Private:  <keydata-base64>
// -- encrypted keys
// Key-Cipher: <algorithm name>
// Key-Credentials: <hex>
// -- signed keys
// Signer-Id: <key-id>
// Signature: <signature-hex>

#define is_prefix(buf, str) (strncmp((buf), (str), strlen((str)))==0)

//
// Load a key by BioStream
//

bool CKey::read_key(CBioStream* bi, bool verify)
{
    char buf[4096];
    char* ptr;
    unsigned int  key_size = 0;
    CKey*         signer = NULL;
    unsigned char sig_data[1024];
    int           sig_len = 0;
    unsigned char cred_data[1024];
    int           cred_len = 0;
    unsigned char key_data[4096];
    int           key_data_len = 0;

    mIsPrivate = false;
    mName   = "";
    mSerial = "";
    mUsage  = 0;
    mResolved = false;

    m1Release(OctetBuffer, mKeyData);
    m1Release(OctetBuffer, mCredData);
    m1Release(OctetBuffer, mSigData);
    mKeyData = NULL;
    mCredData = NULL;
    mSigData = NULL;

    while(bi->gets(buf, sizeof(buf)) > 0) {
	ptr = buf;
	if (is_prefix(ptr, "Key-Id:")) {
	    ptr += strlen("Key-Id:");
	    mName = string(string_trim(ptr));
	}
	else if (is_prefix(ptr, "Key-Name:")) {
	    ptr += strlen("Key-Name:");
	    mName = string(string_trim(ptr));
	}
	else if (is_prefix(ptr, "Key-Serial:")) {
	    ptr += strlen("Key-Serial:");
	    mSerial = string(string_trim(ptr));
	}
	else if (is_prefix(ptr, "Key-Size:")) {
	    char* endptr = NULL;
	    ptr += strlen("Key-Size:");
	    ptr = string_trim(ptr);
	    key_size = strtol(ptr, &endptr, 10);
	    if ((endptr == NULL) || (*endptr != '\0') || 
		(key_size <= 0) || (key_size >= 4096)) {
		ERRFMT("bad Key-Size '%s'", ptr);
		return false;
	    }
	}
	else if (is_prefix(ptr, "Key-Usage:")) {
	    ptr += strlen("Key-Usage:");
	    ptr = string_trim(ptr);
	    while(*ptr != '\0') {
		if (is_prefix(ptr, "any")) {
		    mUsage |= M1_KEY_USAGE_ANY;
		    ptr += 3;
		}
		else if (is_prefix(ptr, "key")) {
		    mUsage |= M1_KEY_USAGE_KEY;
		    ptr += 3;
		}
		else if (is_prefix(ptr, "content")) {
		    mUsage |= M1_KEY_USAGE_CONTENT;
		    ptr += 7;
		}
		else if (is_prefix(ptr, "os")) {
		    mUsage |= M1_KEY_USAGE_OS;
		    ptr += 2;
		}
		else if (is_prefix(ptr, "m1")) {
		    mUsage |= M1_KEY_USAGE_M1;
		    ptr += 2;
		}
		else if (is_prefix(ptr, "font")) {
		    mUsage |= M1_KEY_USAGE_FONT;
		    ptr += 4;
		}
		else if (is_prefix(ptr, "image")) {
		    mUsage |= M1_KEY_USAGE_IMAGE;
		    ptr += 5;
		}
		else if (is_prefix(ptr, "plugin")) {
		    mUsage |= M1_KEY_USAGE_PLUGIN;
		    ptr += 6;
		}
		else if (is_prefix(ptr, "exec")) {
		    mUsage |= M1_KEY_USAGE_EXEC;
		    ptr += 4;
		}
		else if (is_prefix(ptr, "driver")) {
		    mUsage |= M1_KEY_USAGE_DRIVER;
		    ptr += 6;
		}
		else if (isxdigit(*ptr)) {
		    long usage;
		    char* endptr;
		    usage = strtol(ptr, &endptr, 16);
		    if (endptr && ((*endptr == '\0')||(*endptr == ',')))
			mUsage |= usage;
		    ptr = endptr;
		}
		else {
		    ERRFMT("unknown Key-Usage %s", ptr);
		    return false;
		}
		
		if (ptr && *ptr == ',')
		    ptr++;
	    }
	}
	else if (is_prefix(ptr, "Key-Public:")) {
	    ptr += strlen("Key-Public:");
	    ptr = string_trim(ptr);
	    if ((key_data_len=decode_base64(ptr,key_data,sizeof(key_data)))<0) {
		ERRFMT("bad Key_RSA base64 data '%s'", ptr);
		return false;
	    }
	    mIsPrivate = false;
	}
	else if (is_prefix(ptr, "Key-Private:")) {
	    ptr += strlen("Key-Private:");
	    ptr = string_trim(ptr);
	    if ((key_data_len=decode_base64(ptr,key_data,sizeof(key_data)))<0) {
		ERRFMT("bad Key_RSA base64 data '%s'", ptr);
		return false;
	    }
	    mIsPrivate = true;
	}
	else if (is_prefix(ptr, "Key-Cipher:")) {
	    ptr += strlen("Key-Cipher:");
	    mKeyCipher = string(string_trim(ptr));
	}
	else if (is_prefix(ptr, "Key-Credentials:")) {
	    ptr += strlen("Key-Credentials:");
	    ptr = string_trim(ptr);
	    if ((cred_len = decode_hex(ptr, strlen(ptr),
				       cred_data, sizeof(cred_data))) < 0) {
		ERRFMT("bad Credentials data");
		return false;
	    }
	}
	else if (is_prefix(ptr, "Signer-Id:")) {
	    ptr += strlen("Signer-Id:");
	    mSigner = string(string_trim(ptr));
	}
	else if (is_prefix(ptr, "Signature:")) {
	    sig_len = 0;
	    ptr += strlen("Signature:");
	    ptr = string_trim(ptr);
	    if ((sig_len = decode_hex(ptr, strlen(ptr),
				      sig_data, sizeof(sig_data))) < 0) {
		ERRFMT("bad Signature data");
		return false;
	    }
	}
	else {
	    ptr = string_trim(ptr);
	    if (strlen(ptr) != 0) {  // Allow empty lines
		ERRFMT("bad data '%s'", ptr);
		return false;
	    }
	}
    }

    // Display the findings
    {
	char xdata[4096];

	encode_hex(sig_data, sig_len, xdata, sizeof(xdata));

	DBGFMT("Key-Name: '%s'",    mName.c_str());
	DBGFMT("Key-Serial: '%s'",  mSerial.c_str());
	DBGFMT("Key-Usage: %x",     mUsage);
	DBGFMT("Key-Cipher: %s",    mKeyCipher.c_str());
	if (signer) {
	    DBGFMT("Signer-Id: '%s'",      mSigner.c_str());
	    DBGFMT("Signature[%d] '%s': ", sig_len, xdata);
	}
    }

    if (key_data_len <= 0) {
	ERRFMT("no key data found");
	return false;
    }

    if (isPublic() && verify) {
	if (mSigner == "") {
	    DBGFMT("no signer key given for key '%s'", mName.c_str());
	    return false;
	}
	signer = m1_keys().key_by_fingerprint(mSigner);
	if ((signer == NULL) || (signer->PKEY() == NULL)) {
	    ERRFMT("signer key '%s' not %s", 
		   mSigner.c_str(), 
		   ((signer==NULL) ? "loaded" : "resolved"),
		   mName.c_str());

	    // Save the data for later resolve
	    mKeyData   = new OctetBuffer(key_data, key_data_len);
	    mCredData  = new OctetBuffer(cred_data, cred_len);
	    mSigData   = new OctetBuffer(sig_data, sig_len);

	    m1Retain(OctetBuffer, mKeyData);
	    m1Retain(OctetBuffer, mCredData);
	    m1Retain(OctetBuffer, mSigData);
	    return true;  // But unresolved
	}
    }

    if (isPublic() && verify && !(signer->getUsage() & M1_KEY_USAGE_KEY)) {
	ERRFMT("signer key can not be used to sign keys");
	return false;
    }

    return resolve_verify(signer, verify,
			  sig_data, sig_len,
			  cred_data, cred_len,
			  key_data, key_data_len);
}


bool CKey::write_public(CBioStream* bf, bool encrypt)
{
    int   len = 0;
    char  fmtbuf[4096];
    char  value[1024];
    unsigned char* dbuf = NULL;
    int dlen;
    int key_len;
    unsigned char* key_data;
    CKey* signer = NULL;

    write_key_value(bf, "Key-Name",  mName);
    if (mSerial != "")
	write_key_value(bf, "Key-Serial",  mSerial);
    write_key_value(bf, "Key-Usage", formatUsage());
    sprintf(value, "%d", keySize()*8);
    write_key_value(bf, "Key-Size", string(value));

    if ((dlen = i2d_public(&dbuf)) < 0)
	return false;
    key_len = dlen;
    key_data = (unsigned char*) malloc(key_len);
    memcpy(key_data, dbuf, key_len);
    OPENSSL_free(dbuf);

    signer = m1_keys().key_by_fingerprint(mSigner);

    if (!mIsPrivate && signer && encrypt) {
	EVP_MD_CTX ctx;
	const EVP_CIPHER* cipher = M1_CIPHER_ALGORITHM;
	const char* cipher_name = EVP_CIPHER_name(cipher);
	unsigned char cipher_key[EVP_MAX_KEY_LENGTH];
	unsigned char cipher_iv[EVP_MAX_IV_LENGTH];
	int cipher_key_len;
	int cipher_iv_len;
	unsigned char cred_buf[1024];
	int           cred_len;
	unsigned char sig_data[1024];
	int           sig_len;
	unsigned char enc_buf[1024];
	unsigned char* enc_data = NULL;
	int enc_len;

	write_key_value(bf, "Key-Cipher", cipher_name);

	CBioStream::make_credentials(cipher, cipher_key, cipher_iv);
	cipher_key_len = EVP_CIPHER_key_length(cipher);
	cipher_iv_len  = EVP_CIPHER_iv_length(cipher);

	memcpy(enc_buf, cipher_key, cipher_key_len);
	memcpy(enc_buf+key_len, cipher_iv, cipher_iv_len);
	cred_len = signer->encrypt(enc_buf, cipher_key_len + cipher_iv_len, 
				   cred_buf);

	if (cred_len > 0) {
	    encode_hex(cred_buf, cred_len, fmtbuf, sizeof(fmtbuf));
	    write_key_value(bf, "Key-Credentials", fmtbuf);
	}

	EVP_MD_CTX_init(&ctx);
	EVP_SignInit_ex(&ctx, M1_DIGEST_ALGORITHM, NULL);
	digest(&ctx, signer, key_data, key_len);
	sig_len=signer->signature(&ctx,sig_data,sizeof(sig_data));
	EVP_MD_CTX_cleanup(&ctx);

	write_key_value(bf, "Signer-Id", signer->fingerprint());
	if (sig_len > 0) {
	    encode_hex(sig_data, sig_len, fmtbuf, sizeof(fmtbuf));
	    write_key_value(bf, "Signature", fmtbuf);
	}
	enc_len = encrypt_key(signer, this, cipher_key, cipher_key_len,
			      cipher_iv, cipher_iv_len, key_data, key_len,
			      &enc_data);
	len = encode_base64(enc_data, enc_len, fmtbuf, sizeof(fmtbuf));
	write_key_value(bf, "Key-Public", fmtbuf);
	free(enc_data);
    }
    else {
	len = encode_base64(key_data, key_len, fmtbuf, sizeof(fmtbuf));
	write_key_value(bf, "Key-Public", fmtbuf);
    }
    free(key_data);
    return true;
}


// Try resolve the key
bool CKey::resolve()
{
    CKey*         signer = NULL;

    if (mResolved)
	return true;

    if ((signer = m1_keys().key_by_fingerprint(mSigner)) == NULL)
	return false;

    return resolve_verify(signer, true,
			  mSigData->base(), mSigData->size(),
			  mCredData->base(), mCredData->size(),
			  mKeyData->base(), mKeyData->size());
}



bool CKey::write_private(CBioStream* bf)
{
    int   len = 0;
    char  fmtbuf[4096];
    char  value[1024];
    unsigned char* dbuf = NULL;
    int dlen;
    int key_len;
    unsigned char* key_data;

    if (isPublic())
	return false;

    write_key_value(bf, "Key-Name",  mName);
    if (mSerial != "")
	write_key_value(bf, "Key-Serial",  mSerial);
    write_key_value(bf, "Key-Usage", formatUsage());
    sprintf(value, "%d", keySize()*8);
    write_key_value(bf, "Key-Size", string(value));

    if ((dlen = i2d_private(&dbuf)) < 0)
	return false;
    key_len = dlen;
    key_data = (unsigned char*) malloc(key_len);
    memcpy(key_data, dbuf, key_len);
    OPENSSL_free(dbuf);

    len = encode_base64(key_data, key_len, fmtbuf, sizeof(fmtbuf));
    write_key_value(bf, "Key-Private", fmtbuf);
    free(key_data);
    return true;
}
