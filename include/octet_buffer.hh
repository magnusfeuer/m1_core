//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __OCTET_BUFFER_HH__
#define __OCTET_BUFFER_HH__

#include <stdio.h>
#include <openssl/evp.h>

#include "m1rt.hh"

class OctetBuffer : public CRuntime {
public:
    OctetBuffer(unsigned char* aData, size_t aDataLen);
    OctetBuffer(char* aData);
    OctetBuffer(size_t aDataLen);
    OctetBuffer(void);

    ~OctetBuffer(void);

    void print(FILE* f, int base = 0);

    size_t size(void) { return mDataLen; }
    unsigned char* base(void) { return mData; }

    unsigned char& at(int aIndex) { return mData[aIndex]; }

    void resize(size_t aNewDataLen);
    void reserve(size_t aNewCapacity);

    bool load_file(char* filename);
    bool save_file(char* filename);

    OctetBuffer* copy(void);
    OctetBuffer* copy(int aPos, size_t aLength);

    // Inline encode/decode hex 
    bool decode_hex(int delimiter = 0);
    void encode_hex(int delimiter = 0);

    // Inline encode/decode base64
    bool decode_base64(void);
    void encode_base64(int aLineSize = 0);

    // Encode/Decode ASN.1 data to public key
    EVP_PKEY* decode_PublicKey(void);
    bool      encode_PublicKey(EVP_PKEY* pkey);

    // Encode/Decode ASN.1 data to private key
    EVP_PKEY* decode_PrivateKey(void);
    bool      encode_PrivateKey(EVP_PKEY* pkey);

private:
    unsigned char* mData;
    size_t         mDataLen;
    size_t         mDataCapacity;
};


#endif
