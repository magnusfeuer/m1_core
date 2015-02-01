//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include <openssl/evp.h>
#include <openssl/bio.h>
#include <openssl/rand.h>
#include <unistd.h>

#include "bio_stream.hh"
#include "key_store.hh"
#include "bf_zlib.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


bool CBioStream::mSeeded = false;

CBioStream::CBioStream()
{
    mIsCompressed = mIsEncrypted = mIsSigned = false;
    mBio = NULL;
    if (!mSeeded) {
	int rand_des;
	// Get some sweet sweet random data from /dev/random if we are using linux
	if ((rand_des = open("/dev/urandom", O_RDONLY)) != -1 ||
	    (rand_des = open("/dev/random", O_RDONLY)) != -1) {
	    char rand_data[2048];
	    int rand_len = 0;
	
	    while(rand_len != 2048) {
		int len;
		len = ::read(rand_des, rand_data + rand_len, 2048 - rand_len);

		if (len <= 0) break;

		rand_len += len;
	    }
	    ::close(rand_des);
	    RAND_seed(rand_data, rand_len);
	}
	if (!RAND_status())
	    DBGFMT("Warning: PRNG not seeded");
	mSeeded = true;
    }
}

CBioStream::~CBioStream()
{
    if (mBio)
	BIO_free_all(mBio);
}


string CBioStream::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "BioStream: %s bio=%lx #%lu 0x%lx", 
	    mFileName.c_str(),
	    (unsigned long) mBio,
	    refCount(), (unsigned long) this);
    return string(ndata);
}


// Open a possibly encrypted file for reading
// aUsage is matched with the SIGNED header & SIGNATURE header
// The SIGNED header is a prematch to do validation and early exit
// The SIGNATURE header will do the proper thing and verify that the
// usage really match the supplied key usage.
// If no signature exist then the usage must match the ENCRYPTION key usage.
//
int CBioStream::open_file_read(string aFileName, unsigned int aUsage,
			       bool aRequireEncryption,
			       bool aRequireSignature)
{
    if (mBio) // Maybe warn?
	BIO_free_all(mBio);
    if (!(mBio = BIO_new_file(aFileName.c_str(), "rb")))
	return -1;
    // Scan for magic and encryption headers 
    // run forward until DATA blocks begin
    mFileName = aFileName;
    return read_init(aRequireEncryption, aRequireSignature);
}

// Open memory for reading.
int CBioStream::open_mem_read(string aFileName, 
			      char *aData,
			      int aDataLen,
			      unsigned int aUsage,
			      bool aRequireEncryption,
			      bool aRequireSignature)
{
    if (mBio) // Maybe warn?
	BIO_free_all(mBio);

    if (!(mBio = BIO_new_mem_buf(aData, aDataLen)))
	return -1;

    // Scan for magic and encryption headers 
    // run forward until DATA blocks begin
    mFileName = aFileName;
    return read_init(aRequireEncryption, aRequireSignature);
}


// Open memory for writing.
int CBioStream::open_mem_write(string aFileName)

{
    if (mBio) // Maybe warn?
	BIO_free_all(mBio);

    if (!(mBio =  BIO_new(BIO_s_mem())))
	return -1;

    // Scan for magic and encryption headers 
    // run forward until DATA blocks begin
    mFileName = aFileName;
    return 0;
}


int CBioStream::open_fp_read(string aFileName, FILE* fp, unsigned int aUsage,
			     bool aRequireEncryption,
			     bool aRequireSignature)
{
    if (mBio) // Maybe warn?
	BIO_free_all(mBio);
    if (!(mBio = BIO_new_fp(fp, BIO_NOCLOSE)))
	return -1;


    // Scan for magic and encryption headers 
    // run forward until DATA blocks begin
    mFileName = aFileName;
    BIO_seek(mBio, ftell(fp));
    return read_init(aRequireEncryption, aRequireSignature);
}

int CBioStream::open_file_write(string aFileName)
{
    if (mBio) // Maybe warn?
	BIO_free_all(mBio);
    if (!(mBio = BIO_new_file(aFileName.c_str(), "wb")))
	return -1;
    mFileName = aFileName;
    return 0;
}

int CBioStream::open_fp_write(string aFileName, FILE* fp)
{
    if (mBio) // Maybe warn?
	BIO_free_all(mBio);
    if (!(mBio = BIO_new_fp(fp, BIO_NOCLOSE)))
	return -1;
    mFileName = aFileName;
    return 0;
}

int CBioStream::read(void* aBuffer, int aLen)
{
    if (mBio)
	return BIO_read(mBio, aBuffer, aLen);
    return -1;
}

int CBioStream::gets(char* aBuffer, int aLen)
{
    if (mBio)
	return BIO_gets(mBio, aBuffer, aLen);
    return -1;
}

int CBioStream::write(void* aBuffer, int aLen)
{
    if (mBio)
	return BIO_write(mBio, aBuffer, aLen);
    return -1;
}

void CBioStream::close()
{
    BIO_free_all(mBio);
    mBio = NULL;
    mFileName = "";
}

int CBioStream::flush()
{
    if (mBio)
	return BIO_flush(mBio);
    return -1;
}

bool CBioStream::should_retry()
{
    if (mBio)
	return BIO_should_retry(mBio);
    return false;
}

//
// Push a new bio on to the bio chain
//
void CBioStream::push(BIO* aBf)
{
    if (mBio)
	mBio = BIO_push(aBf, mBio);
}

//
// Pop the last pushed bio
//
void CBioStream::pop(void)
{
    BIO* bf = mBio;
    if (bf) {
	mBio = BIO_pop(mBio);
	BIO_free(bf);
    }
}

BIO* CBioStream::find(int type)
{
    if (mBio)
	return BIO_find_type(mBio, type); 
    return NULL;
}

u_int32_t CBioStream::read_tag(u_int32_t* len)
{
    Tag_t t;

    if (BIO_read(mBio, (void*)&t, sizeof(t)) != sizeof(t))
	return 0;
    *len = tag_u32(t.len);
    return t.tag;
}

// Seek forward (may be use seek?, not always supported!!!)
int CBioStream::skip(int aLen)
{
    char buffer[4096];

    while(aLen > 0) {
	int n = (aLen < (int)sizeof(buffer)) ? aLen : sizeof(buffer);
	if (BIO_read(mBio, buffer, n) != n)
	    return -1;
	aLen -= n;
    }
    return 0;
}

// Load section data 
int CBioStream::read_params(u_int32_t len, Param_t* params, int n)
{
    int i;
    int found = 0;

    // initialize parameters
    for (i = 0; i < n; i++)
	params[i].data = NULL;

    // scan complete section
again:
    while(len >= sizeof(Tag_t)) {
	u_int32_t item_len = 0;
	u_int32_t tag = read_tag(&item_len);

	len -= sizeof(Tag_t);
	for (i = 0; i < n; i++) {
	    if ((params[i].tag == tag) && (params[i].data == NULL)) {
		int n;
		if (params[i].len > item_len)
		    n = item_len; // warn for trunction ?
		else
		    n = params[i].len;
		if (BIO_read(mBio, params[i].mem, n) != n)
		    return -1;
		if (n < (int) item_len)
		    skip(item_len - n);
		if (n < (int) params[i].len)
		    ((char*)params[i].mem)[n] = '\0';
		params[i].data = params[i].mem;
		params[i].len  = n;
		len -= item_len;
		found++;
		goto again;
	    }
	}
	// not requested - skip item
	skip(item_len);
	len -= item_len;
    }
    return found;
}

// Read section data after ENCRYPTION sectio tag is seen
int CBioStream::read_ENCRYPTION(u_int32_t len)
{
    char algo[1024];
    char keyid[1024];
    char cred[1024];
    unsigned char key_buf[1024];
    int           key_buf_len;
    int           key_len;
    int           iv_len;
    BIO*          bf;
    CKey*         key;

    const EVP_CIPHER* cipher;
    Param_t params[] = {
	{ M1SC_ITEM_algo,  1024, algo, NULL},
	{ M1SC_ITEM_keyi, 1024, keyid, NULL},
	{ M1SC_ITEM_cred,  1024, cred, NULL} };
    
    if (read_params(len, params, NPARAMS(params)) <= 0) {
	ERRFMT("%s: encryption params not found", mFileName.c_str());
	return -1;
    }
    
    // lookup cipher used
    cipher  = EVP_get_cipherbyname((char*)params[0].data);
    if (cipher == 0) {
	ERRFMT("%s: cipher %s not found", mFileName.c_str(), (char *) params[0].data);
	return -1;
    }

    key_len = EVP_CIPHER_key_length(cipher);
    iv_len  = EVP_CIPHER_iv_length(cipher);

    key = m1_keys().key_by_fingerprint(string((char*)params[1].data));
    if (!key) {
	ERRFMT("%s: encryption signer key not found", mFileName.c_str());
	return -1;
    }

    // decrypt credentials 
    key_buf_len = key->decrypt((unsigned char*) params[2].data,params[2].len,
			       key_buf);
    if (key_buf_len < 0) {
	ERRFMT("%s: unable to pub decrypt", mFileName.c_str());
	return -1;
    }
    bf = BIO_new(BIO_f_cipher());
    BIO_set_cipher(bf, cipher, key_buf, key_buf+key_len, 0);
    push(bf);
    return 0;
}

int CBioStream::read_SIGNED(u_int32_t len)
{
    const EVP_MD* md;
    BIO* bf;
    char algo[1024];
    Param_t params[] = {
	{ M1SC_ITEM_algo,  1024, algo, NULL} };

    if (read_params(len, params, NPARAMS(params)) <= 0) {
	ERRFMT("%s: signed section not found", mFileName.c_str());
	return -1;
    }
    md = EVP_get_digestbyname((char*)params[0].data);

    bf = BIO_new(BIO_f_md());
    BIO_set_md(bf, md);
    push(bf);
    return 0;
}

int CBioStream::read_SIGNATURE(u_int32_t len)
{
    char keyid[1024];
    char sign[1024];
    EVP_MD_CTX* ctx;
    BIO* md;
    BIO* bn;
    CKey* key;
    Param_t params[] = {
	{ M1SC_ITEM_keyi, 1024, keyid, NULL},
	{ M1SC_ITEM_sign,  1024, sign, NULL} };

    if (read_params(len, params, NPARAMS(params)) <= 0) {
	ERRFMT("%s: signature section not found", mFileName.c_str());
	return -1;
    }

    // find the first MD bio in chain
    if ((md = find(BIO_TYPE_MD)) == NULL) {
	ERRFMT("%s: content is not signed", mFileName.c_str());
	return -1;
    }
    bn = BIO_next(md);

    key = m1_keys().key_by_fingerprint(string((char*)params[0].data));
    if (!key) {
	ERRFMT("%s: signer key not found", mFileName.c_str());
	return -1;
    }
    DBGFMT("signer key: %s", (char*)params[0].data);
	    
    BIO_get_md_ctx(md, &ctx);
    // Should we drop md here ?
    return key->verify(ctx, (unsigned char*)params[1].data, params[1].len);
}

int CBioStream::read_COMPRESSION(u_int32_t len)
{
    BIO* bf;
    char algo[1024];
    u_int32_t parm;
    u_int64_t orig_len;
    int  compression_level;

    Param_t params[] = {
	{ M1SC_ITEM_algo, 1024, algo, NULL},
	{ M1SC_ITEM_parm, 4,    (char*)&parm, NULL},
	{ M1SC_ITEM_len,  8,    (char*)&orig_len, NULL} };

    if (read_params(len, params, NPARAMS(params)) <= 0) {
	ERRFMT("%s: compress section not found", mFileName.c_str());
	return -1;
    }
    if (strcmp(algo, "zlib") != 0)  {
	ERRFMT("%s: compress alogorithm %s not supported", 
	       mFileName.c_str(), algo);
	return -1;
    }
    compression_level = (int) int_u32(parm);
    DBGFMT("compression algo=%s", (char*)params[0].data);
    DBGFMT("compression level=%d", compression_level);

    bf = BIO_new(BIO_f_zlib());
    BIO_set_compression(mBio, compression_level);
    push(bf);
    return 1;
}

// Initialize reading of a file 
int CBioStream::read_init(bool aRequireEncryption, bool aRequireSignature)
{
    u_int32_t tag;
    u_int32_t len;
    char version[4];
    FILE *fp;

    BIO_get_fp(mBio, &fp);

    if (read_tag(&len) != M1SC_MAGIC) {
	// Not encrypted not signed
	BIO_reset(mBio);
	goto done;
    }
    if ((BIO_read(mBio, version, sizeof(version)) != sizeof(version)) ||
	(memcmp(version, M1SC_VERSION, 4) != 0)) {
	ERRFMT("%s: warning wrong file version", version);
	BIO_reset(mBio);
	goto done;
    }

    mIsEncrypted = mIsCompressed = mIsSigned = false;
    while((tag = read_tag(&len)) != 0) {
	int r;
	switch(tag) {
	case M1SC_ENCRYPTION:

	    if ((r = read_ENCRYPTION(len)) < 0)
		return -1;

	    mIsEncrypted = true;
	    break;

	case M1SC_SIGNED:
	    if ((r = read_SIGNED(len)) < 0)
		return -1;
	    mIsSigned = true;
	    break;

	case M1SC_SIGNATURE:
	    if ((r = read_SIGNATURE(len)) < 0)
		return -1;
	    if (r == 0) {
		ERRFMT("%s: verification failed", mFileName.c_str());
		return -1;
	    }
	    break;

	case M1SC_COMPRESSION:
	    if ((r = read_COMPRESSION(len)) < 0)
		return -1;

	    mIsCompressed = true;
	    break;
	case M1SC_DATA:
	    // len == 0 means that data will run until eof
	    // len > 0 means we have a length tag eihter 32bit or 64bit
	    goto done;

	default:
	    goto done;
	}
    }
    return 0;
done:
    if (aRequireEncryption && !mIsEncrypted)
	return -1;
    if (aRequireSignature && !mIsSigned)
	return -1;
    return 0;
}


int CBioStream::write_tag(u_int32_t tag, u_int32_t len)
{
    Tag_t t;
    t.tag = tag;
    t.len = tag_u32(len);
    return BIO_write(mBio, (void*)&t, sizeof(t));
}

int CBioStream::write_tag_data(u_int32_t tag, void* data, u_int32_t len)
{
    int r;
    if ((r=write_tag(tag, len)) <= 0)
	return r;
    return BIO_write(mBio, data, len);
}

int CBioStream::write_MAGIC(void)
{
    return write_tag_data(M1SC_MAGIC, (void*)M1SC_VERSION, 4);
}

int CBioStream::write_DATA(u_int32_t len)
{
    return write_tag(M1SC_DATA, len);
}

//
// Write SIGN header and add digest filter on input stream
// optionally return the pushed filter in bfp
//
int CBioStream::write_SIGNED(CBioStream* aIn)
{
    BIO* bf;
    const EVP_MD* md = M1_DIGEST_ALGORITHM;
    const char* digest_name = EVP_MD_name(md);
    u_int32_t len;
    int r;

    bf = BIO_new(BIO_f_md());
    BIO_set_md(bf, md);
    aIn->push(bf);

    len = 
	sizeof(Tag_t) + strlen(digest_name);
    if ((r = write_tag(M1SC_SIGNED, len)) <= 0)
	return r;

    return write_tag_data(M1SC_ITEM_algo,(void*)digest_name,strlen(digest_name));
}

int CBioStream::write_SIGNATURE(CKey* sigkey, CBioStream* aIn)
{
    BIO* md;
    BIO* bn;
    EVP_MD_CTX* ctx;
    unsigned char sign_buf[1024];
    int  sign_len = 0;
    string keyid;
    u_int32_t len;

    md = aIn->find(BIO_TYPE_MD);
    bn = BIO_next(md);
    BIO_get_md_ctx(md, &ctx);

    if ((sign_len = sigkey->signature(ctx,sign_buf,sizeof(sign_buf))) < 0) {
	fprintf(stderr, "%s: unable to sign the data\n", mFileName.c_str());
	return -1;
    }
    keyid = sigkey->fingerprint();

    // Should we write this in clear text ?
    len = 
	sizeof(Tag_t) + keyid.length() +
	sizeof(Tag_t) + sign_len;
    write_tag(M1SC_SIGNATURE, len);
    write_tag_data(M1SC_ITEM_keyi, (void*) keyid.c_str(), keyid.length());
    write_tag_data(M1SC_ITEM_sign,  sign_buf, sign_len);
    mIsSigned = true;
    return 1;
}

int CBioStream::make_credentials(const EVP_CIPHER* cipher,
				      unsigned char* key, unsigned char* iv)
{
    int key_len = EVP_CIPHER_key_length(cipher);
    int iv_len  = EVP_CIPHER_iv_length(cipher);

    if (!RAND_bytes(key,key_len)) {
	fprintf(stderr, "unable to generate password (check PRNG)\n");
	exit(1);
    }
    if (!RAND_bytes(iv, iv_len)) {
	fprintf(stderr, "unable to generate iv (check PRNG)\n");
	exit(1);
    }
    return 0;
}

int CBioStream::write_ENCRYPTION(CKey* sigkey)
{
    BIO* bf;
    const EVP_CIPHER* cipher = M1_CIPHER_ALGORITHM;
    unsigned char enc_buf[1024];
    unsigned char cred_buf[1024];
    int           cred_len;
    unsigned char key[EVP_MAX_KEY_LENGTH];
    unsigned char iv[EVP_MAX_IV_LENGTH];
    string keyid;
    int key_len = EVP_CIPHER_key_length(cipher);
    int iv_len  = EVP_CIPHER_iv_length(cipher);
    const char* cipher_name = EVP_CIPHER_name(cipher);
    u_int32_t len;

    make_credentials(cipher, key, iv);

    memcpy(enc_buf, key, key_len);
    memcpy(enc_buf+key_len, iv, iv_len);

    cred_len = sigkey->encrypt(enc_buf, key_len + iv_len, cred_buf);
    if (cred_len < 0) {
	fprintf(stderr, "%s: unabled RSA_private_encrypt\n", mFileName.c_str());
	return 0;
    }

    keyid = sigkey->fingerprint();
    
    // Add ENCRYPTION header
    len = 
	sizeof(Tag_t) + strlen(cipher_name) +
	sizeof(Tag_t) + keyid.length() + 
	sizeof(Tag_t) + cred_len;
    write_tag(M1SC_ENCRYPTION, len);
    write_tag_data(M1SC_ITEM_algo, (void*) cipher_name, strlen(cipher_name));
    write_tag_data(M1SC_ITEM_keyi, (void*) keyid.c_str(), keyid.length());
    write_tag_data(M1SC_ITEM_cred,  cred_buf, cred_len);

    // Push encryption module
    bf = BIO_new(BIO_f_cipher());
    BIO_set_cipher(bf, cipher, key, iv, 1);
    push(bf);
    mIsEncrypted = true;
    return 1;  // FIXME: return value
}

/*
 * Push compression onto stream
 * 0 = no compression
 * 1 = best speed
 * 9 = best compression
 * -1 default compression
 */
int CBioStream::write_COMPRESSION(int compression_level, u_int64_t orig_len)
{
    BIO* bf;
    int len;
    u_int64_t orig_len64;
    int32_t comp32;

    if (compression_level < -1)
	compression_level = -1;
    else if (compression_level > 9)
	compression_level = 9;
    comp32 = int_u32(compression_level);
    orig_len64 = int_u64(orig_len);

    len = sizeof(Tag_t) + strlen("zlib") +
	  sizeof(Tag_t) + sizeof(comp32) +
	  sizeof(Tag_t) + sizeof(orig_len64);
    write_tag(M1SC_COMPRESSION, len);
    write_tag_data(M1SC_ITEM_algo, (void*) "zlib", strlen("zlib"));
    write_tag_data(M1SC_ITEM_parm, (void*) &comp32, sizeof(comp32));
    write_tag_data(M1SC_ITEM_len32, (void*) &orig_len64, sizeof(orig_len64));

    bf = BIO_new(BIO_f_zlib());
    BIO_set_compression(mBio, compression_level);
    push(bf);
    mIsCompressed = true;
    return 1;
}


int CBioStream::stream_DATA(CBioStream* aIn)
{
    unsigned char buffer[4096];
    int n;

    /* lets stream read write data */

    while((n = aIn->read(buffer, sizeof(buffer))) > 0) {
	if (BIO_write(mBio, buffer, n) != n) {
	    fprintf(stderr, "%s: unable to write %d bytes\n", 
		    mFileName.c_str(), n);
	    return -1;
	}
    }
    return n;
}
