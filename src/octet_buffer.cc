//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "octet_buffer.hh"

/* unsigned char => b64 value */
static int b64_value[256] = {
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, 52, 53, 54, 55, 56, 57, 
    58, 59, 60, 61, -1, -1, -1, 64, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 
    9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, 
    -1, -1, -1, -1, -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 
    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1 };

/* unsigned char => hex value */
static int x_value[256] =
{
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 
    8, 9, -1, -1, -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
    -1, -1 };


OctetBuffer::OctetBuffer()
{
    mData = NULL;
    mDataLen = 0;
    mDataCapacity = 0;
}

OctetBuffer::OctetBuffer(size_t aDataLen)
{
    mData = (unsigned char*) malloc(aDataLen);
    mDataLen = aDataLen;
    mDataCapacity = aDataLen;
}

OctetBuffer::OctetBuffer(char* aData)
{
    mDataLen = strlen(aData);
    mData = (unsigned char*) malloc(mDataLen);
    mDataCapacity = mDataLen;
    memcpy(mData, aData, mDataLen);
}


OctetBuffer::OctetBuffer(unsigned char* aData, size_t aDataLen)
{
    mData = (unsigned char*) malloc(aDataLen);
    mDataLen = aDataLen;
    mDataCapacity = aDataLen;
    memcpy(mData, aData, aDataLen);
}

OctetBuffer::~OctetBuffer(void)
{
    if (mData) 
	free(mData);
}

void OctetBuffer::print(FILE* f, int base)
{
    int i;

    switch(base) {
    case 2:
	for (i = 0; i < (int) mDataLen; i++) {
	    int j;
	    unsigned char bits = mData[i];
	    for (j = 0; j < 8; j++) {
		fprintf(f, "%d", ((bits & 0x80) != 0));
		bits <<= 1;
	    }
	    fprintf(f, " ");
	}
	fprintf(f, "\n");
	break;
    case 8:
	for (i = 0; i < (int) mDataLen; i++) 
	    fprintf(f, "%o ", mData[i]);
	fprintf(f, "\n");
	break;
    case 10:
	for (i = 0; i < (int) mDataLen; i++) 
	    fprintf(f, "%d ", mData[i]);
	fprintf(f, "\n");
	break;
    case 16:
	for (i = 0; i < (int) mDataLen; i++) 
	    fprintf(f, "%02X ", mData[i]);
	fprintf(f, "\n");
	break;
    default:
	for (i = 0; i < (int) mDataLen; i++) {
	    if (isprint(mData[i])||isspace(mData[i]))
		fputc(mData[i], f);
	    else
		fputc('?', f);
	}
	fprintf(f, "\n");
	break;
    }
}


void  OctetBuffer::resize(size_t aNewDataLen)
{
    mData = (unsigned char*) realloc(mData, aNewDataLen);
    mDataLen = aNewDataLen;
    mDataCapacity = aNewDataLen;
}

void  OctetBuffer::reserve(size_t aNewCapacity)
{
    if (mDataCapacity <  aNewCapacity) {
	mData = (unsigned char*) realloc(mData, aNewCapacity);
	mDataCapacity = aNewCapacity;
    }
}

bool OctetBuffer::load_file(char* filename)
{
    FILE* f;
    long sz;

    if ((f = fopen(filename, "r")) == NULL)
	return false;
    if (fseek(f, 0, SEEK_END) < 0) goto error;
    if ((sz = ftell(f)) < 0)	goto error;
    if (fseek(f, 0, SEEK_SET) < 0) goto error;
    reserve(sz);
    if (fread(mData, sizeof(unsigned char), sz, f) < (size_t)sz) goto error;
    mDataLen = sz;
    fclose(f);
    return true;
error:
    fclose(f);
    return false;
}

bool OctetBuffer::save_file(char* filename)
{
    FILE* f;
    
    if ((f = fopen(filename, "w")) == NULL)
	return false;
    if (fwrite(mData, sizeof(unsigned char), mDataLen, f) < mDataLen)
	goto error;
    fclose(f);
    return true;
error:
    fclose(f);
    return false;
}


OctetBuffer* OctetBuffer::copy()
{
    return new OctetBuffer(mData, mDataLen);
}

OctetBuffer* OctetBuffer::copy(int aPos, size_t aLength)
{
    if (aPos < (int) mDataLen) {
	size_t remainLength = mDataLen - aPos;
	if (aLength > remainLength)
	    aLength = remainLength;
	return new OctetBuffer(mData+aPos, aLength);
    }
    return NULL;
}

//
// The decoder will stop on error and return false
// but will update any way and set length to what ever was decoded
// The decode accept space (nl cr sp tab and delimiter) between hex codes
//  
bool OctetBuffer::decode_hex(int delimiter)
{
    unsigned char* src = mData;
    unsigned char* dst = mData;
    int sLen = mDataLen;
    int c1, c2;

    while (sLen) {
	c1 = c2 = -1;
	while(sLen && *src && (isspace(*src)||(*src==delimiter))) {
	    src++; sLen--;
	}
	if (sLen) { c1 = x_value[*src++]; sLen--; }

	while(sLen && *src && (isspace(*src)||(*src==delimiter))) {
	    src++; sLen--; 
	}
	if (sLen) { c2 = x_value[*src++]; sLen--; }

	if ((c1 < 0) || (c2 < 0))
	    goto done;
	*dst++ = (c1 << 4) | c2;
    }
    mDataLen = (dst - mData);
    return true;

done:
    mDataLen = (dst - mData);
    return false;
}

//
// Encode the buffer (inline) to ascii hex
// delimiter can be used to place a format character between octet hex code
// (like a ':')
//
void OctetBuffer::encode_hex(int delimiter)
{
    size_t sLen = mDataLen;  // save length before operation
    size_t dLen;             // target length
    unsigned char* src;
    unsigned char* dst;
    char hc[17] = "0123456789ABCDEF";

    if (delimiter)
	dLen = mDataLen*3-1;
    else
	dLen = mDataLen*2;

    reserve(dLen+1);  // make sure content will fit
    mDataLen = dLen;
    src = mData + sLen;
    dst = mData + dLen;
    dst[0] = '\0';  // FEATURE nil terminate the encoded buffer
    if (delimiter) {
	if (sLen) {
	    int c = *--src;
	    dst -= 2;
	    dst[0] = hc[c >> 4];
	    dst[1] = hc[c & 0xf];
	    sLen--;
	}
	while(sLen) {
	    int c = *--src;
	    dst -= 3;
	    dst[0] = hc[c >> 4];
	    dst[1] = hc[c & 0xf];
	    dst[2] = ':';
	    sLen--;
	}
    }
    else {
	while(sLen) {
	    int c = *--src;
	    dst -= 2;
	    dst[0] = hc[c >> 4];
	    dst[1] = hc[c & 0xf];
	    sLen--;
	}
    }
}


//
// The decoder will stop on error and return false
// but will update any way and set length to what ever was decoded
//  
bool OctetBuffer::decode_base64(void)
{
    unsigned char* src = mData;
    unsigned char* dst = mData;
    size_t sLen = mDataLen;
    int c0,c1,c2,c3;

    while(sLen) {
	c0=c1=c2=c3=-1;

	while(sLen && isspace(*src)) { src++; sLen--; }
	if (sLen) { c0 = b64_value[*src++]; sLen--; }
	
	while(sLen && isspace(*src)) { src++; sLen--; }
	if (sLen) { c1 = b64_value[*src++]; sLen--; }

	while(sLen && isspace(*src)) { src++; sLen--; }
	if (sLen) { c2 = b64_value[*src++]; sLen--; }

	while(sLen && isspace(*src)) { src++; sLen--; }
	if (sLen) { c3 = b64_value[*src++]; sLen--; }

	if ((c0<0)||(c1<0)||(c2<0)||(c3<0)) goto error;
	if ((c0==64)||(c1==64)) goto error;
	*dst++ = (c0<<2)|(c1>>4);
	if ((c2==64) && (c3==64))
	    break;
	if (c2==64) goto error;
	*dst++ = (c1<<4)|(c2>>2);
	if (c3==64) break;
	*dst++ = (c2<<6)|c3;
    }
    mDataLen = (dst - mData);
    return true;

error:
    mDataLen = (dst - mData);
    return false;
}

//
// Encode the buffer (inline) to base64
// if aLineSize is given then a newline is inserted 
// between each line chunk (rounded down to multiple of 4)
// a LineSize of 0 means no newline is inserted
//
void OctetBuffer::encode_base64(int aLineSize)
{
    size_t sLen = mDataLen;              // save length before operation
    size_t dLen = ((sLen+2) / 3)*4;  // target length
    size_t cSize = aLineSize / 4;        // number of "chunks" per line
    unsigned char* src;
    unsigned char* dst;
    char b64c[65] = 
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"abcdefghijklmnopqrstuvwxyz"
	"0123456789"
	"+/";
    int c0, c1, c2;
    int cCur = 0;

    if (cSize) {
	size_t cLen = dLen / 4;             // total number of chunks
	size_t newLines = (cLen / cSize);   // number of newlines
	size_t lastLine = cLen % cSize;     // trailing chunks
	dLen += newLines;                   // update number of chars
	if (lastLine)
	    dLen++;
	cCur = lastLine;
    }

    reserve(dLen+1);
    mDataLen = dLen;
    src = mData + sLen;
    dst = mData + dLen;
    dst[0] = '\0';  // FEATURE nil terminate the encoded buffer
    if (cSize) *--dst = '\n';

    // Since this algorithm run backwards we
    // first treat the trailing bytes if needed
    switch(sLen % 3) {
    case 1:
	src -= 1;
	dst -= 4;
	c0 = src[0];
	dst[0] = b64c[c0>>2];
	dst[1] = b64c[((c0<<4)&0x3f)];
	dst[2] = '=';
	dst[3] = '=';
	sLen -= 1;
	break;
    case 2:
	src -= 2;
	dst -= 4;
	c0 = src[0];
	c1 = src[1];

	dst[0] = b64c[c0>>2];
	dst[1] = b64c[((c0<<4)&0x3f)|(c1>>4)];
	dst[2] = b64c[((c1<<2)&0x3f)];
	dst[3] = '=';
	sLen -= 2;
	break;
    default:
	break;
    }

    while(sLen >= 3) {
	if (cSize) {
	    if (cCur <= 0) {
		*--dst = '\n';
		cCur = cSize;
	    }
	    else
		cCur--;
	}
	src -= 3;
	dst -= 4;
	c0 = src[0];
	c1 = src[1];
	c2 = src[2];
	dst[0] = b64c[c0>>2];
	dst[1] = b64c[((c0<<4)&0x3f)|(c1>>4)];
	dst[2] = b64c[((c1<<2)&0x3f)|(c2>>6)];
	dst[3] = b64c[(c2 & 0x3f)];
	sLen -= 3;
    }
    if (dst != mData) fprintf(stderr, "encode_bas64 (FIXME!!!)\n");
}

//
// Try decode the buffer as a ASN.1 encoded PublicKey
//
EVP_PKEY* OctetBuffer::decode_PublicKey(void)
{
    const unsigned char* ptr = mData;

    return d2i_PublicKey(EVP_PKEY_RSA, NULL, &ptr, mDataLen);
}

// Encode public key as ASN.1 octet data
bool OctetBuffer::encode_PublicKey(EVP_PKEY* pkey)
{
    unsigned char* ptr = NULL;
    int len;

    if ((len = i2d_PublicKey(pkey, &ptr)) < 0)
	return false;
    resize(len);
    memcpy(mData, ptr, len);
    OPENSSL_free(ptr);
    return true;
}

//
// Try decode the buffer as a ASN.1 encoded PrivateKey
//
EVP_PKEY* OctetBuffer::decode_PrivateKey(void)
{
    const unsigned char* ptr = mData;

    return d2i_PrivateKey(EVP_PKEY_RSA, NULL, &ptr, mDataLen);
}

// Encode public key as ASN.1 octet data
bool OctetBuffer::encode_PrivateKey(EVP_PKEY* pkey)
{
    unsigned char* ptr = NULL;
    int len;

    if ((len = i2d_PrivateKey(pkey, &ptr)) < 0)
	return false;
    resize(len);
    memcpy(mData, ptr, len);
    OPENSSL_free(ptr);
    return true;
}

