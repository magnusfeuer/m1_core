//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#ifndef __KEY_HH__
#define __KEY_HH__

#include <stdio.h>
#include <string.h>
#include <openssl/sha.h>
#include <openssl/md5.h>
#include <openssl/rsa.h>
#include <openssl/dsa.h>

#include "m1rt.hh"
#include "octet_buffer.hh"
#include "bio_stream.hh"

#define M1_KEY_USAGE_KEY     0x8000
#define M1_KEY_USAGE_CONTENT 0x00FF
#define M1_KEY_USAGE_OS      0x0F00
#define M1_KEY_USAGE_ANY     0xFFFF

#define M1_KEY_USAGE_M1      0x0001
#define M1_KEY_USAGE_FONT    0x0002
#define M1_KEY_USAGE_IMAGE   0x0004
#define M1_KEY_USAGE_PLUGIN  0x0008

#define M1_KEY_USAGE_EXEC    0x0100
#define M1_KEY_USAGE_DRIVER  0x0200

#define M1_FINGER_ALGORITHM  EVP_sha1()
#define M1_DIGEST_ALGORITHM  EVP_sha1()
#define M1_CIPHER_ALGORITHM  EVP_aes_128_ecb()

#define DEFAULT_KEY_SIZE     1024
#define DEFAULT_RSA_EXPONENT 65537

// Total array size of patch area.
#define KEY_PATCH_AREA_SIZE 1024
// Offset into key patch area to dump public key. (> 2)
#define KEY_PATCH_PUBKEY_OFFSET 16
#define KEY_MAX_PASSWORD_SIZE   32


class CKey : public CRuntime {
public:
    CKey();

    CKey(string aName, string aSerial, 
	 string aSigner, unsigned int aUsage,
	 bool aIsPrivate, EVP_PKEY* aPKEY);
    ~CKey();

    // Generate a RSA key
    int generate(int size);

    // Return the "builtin" device key
    static CKey* device_key(void);

    string debugName(void);

    string  getName(void)         { return mName; }
    void    setName(string aName) { mName = aName; }

    string  getSerial(void)           { return mSerial; }
    void    setSerial(string aSerial) { mSerial = aSerial; }

    unsigned int getUsage(void)        { return mUsage; }
    void setUsage(unsigned int aUsage) { mUsage = aUsage; }

    string getSigner(void)             { return mSigner; }
    void   setSigner(string aSigner)   { mSigner=aSigner; }


    string       fingerprint(void)  { return mFingerPrint; }
    string       formatUsage(void)  { return make_usage(); }
    bool         isPrivate(void)    { return mIsPrivate; }
    bool         isPublic(void)     { return !mIsPrivate; }
    bool         isResolved(void)   { return mResolved; }

    int          keySize(void);
    RSA*         rsaKey(void);
    EVP_PKEY*    PKEY(void);
    void         setPKEY(EVP_PKEY* aPKEY, bool aIsPrivate);

    int i2d_private(unsigned char** ptr);
    int i2d_public(unsigned char** ptr);

    int verify(EVP_MD_CTX* ctx, unsigned char* sigbuf, unsigned int siglen);
    int signature(EVP_MD_CTX* ctx,unsigned char* sigbuf, unsigned int siglen);

    int decrypt(unsigned char* src, int src_len, unsigned char* dst);
    int encrypt(unsigned char* src, int src_len, unsigned char* dst);

    int digest(EVP_MD_CTX* ctx, CKey* signer);
    int digest(EVP_MD_CTX* ctx, CKey* signer, unsigned char* data, int len);

    bool resolve();
    bool read_key(CBioStream* bi, bool verify);
    bool write_private(CBioStream* bf);
    bool write_public(CBioStream* bf, bool encrypt);

private:
    string make_fingerprint();

    string make_usage();

    bool  resolve_verify(CKey* signer, bool verify,
			 unsigned char* sig_data,
			 int            sig_len,
			 unsigned char* cred_data,
			 int            cred_len,
			 unsigned char* key_data,
			 int            key_data_len);

    /* Field always be present */
    string       mName;        // The key name ascii string
    string       mSigner;      // The signer fingerprint
    string       mSerial;      // Serial number
    unsigned int mUsage;       // Usage mask   
    bool         mIsPrivate;   // true when we have a private key
    bool         mResolved;    // true when signer is not loaded/resolved

    /* Resolved fields */
    string       mFingerPrint; // Generated finger print (public key)
    EVP_PKEY*    mPKEY;        // the public/private key

    string       mKeyCipher;   // Name of cipher used

    /* Unresolve fields (stored until resolved) */
    OctetBuffer* mKeyData;
    OctetBuffer* mCredData;
    OctetBuffer* mSigData;
};

#endif
