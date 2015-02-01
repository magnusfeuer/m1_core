//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __BIO_STREAM_HH
#define __BIO_STREAM_HH

#include <openssl/evp.h>
#include "m1rt.hh"

#define M1SC_VERSION  "1000"

class CKey;

/*
 * Content signing / compression / encryption 
 *
 */
#define VM_U32_BE(a,b,c,d) (((a) << 24)|((b)<<16)|((c)<<8)|(d))
#define VM_U32_LE(a,b,c,d) (((d) << 24)|((c)<<16)|((b)<<8)|(a))
#define U_32(a) ((u_int32_t)(a))
#define U_64(a) ((u_int64_t)(a))
#define U_8P(p) ((u_int8_t*)(p))

#define U_32_SWAP(xl) ( ((U_32(xl) & 0xff) << 24) |      \
		      ((U_32(xl) & 0xff00) << 8) |     \
		      ((U_32(xl) & 0xff0000) >> 8) |   \
		      ((U_32(xl) >> 24)))

#define U_64_SWAP(xl) ( ((U_64(xl) & 0xff) << 56) |			\
			((U_64(xl) & 0xff00) << 48) |			\
			((U_64(xl) & 0xff0000) << 24) |			\
			((U_64(xl) & 0xff000000) << 8) |		\
			((U_64(xl) & 0xff00000000) >> 8) |		\
			((U_64(xl) & 0xff0000000000) >> 24) |		\
			((U_64(xl) & 0xff000000000000) >> 48) |		\
			((U_64(xl) >> 56)))



#if BYTE_ORDER == BIG_ENDIAN
#define tag(a,b,c,d)  VM_U32_BE(a,b,c,d)
#define tag_u32(x)    U_32_SWAP(x)
#define tag_u64(x)    U_64_SWAP(x)
#define int_u32(x)    U_32_SWAP(x)
#define int_u64(x)    U_64_SWAP(x)
#else
#define tag(a,b,c,d)  VM_U32_LE(a,b,c,d)
#define tag_u32(x)    U_32(x)
#define tag_u64(x)    U_64(x)
#define int_u32(x)    U_32(x)
#define int_u64(x)    U_64(x)
#endif

typedef struct {
    u_int32_t tag;
    u_int32_t len;
} Tag_t;

typedef struct {
    u_int32_t tag;   // request tag
    u_int32_t len;   // max len -> actual len
    void*     mem;   // memory to use (max len)
    void*    data;   // data filled in by read section NULL if not found
} Param_t;

#define NPARAMS(array) (sizeof(array) / sizeof(Param_t))
//
//  ENCRYPTION ( 'CRPT' <len> )
//           'algo' <algotithm-name>
//           'keyi' <fingerprint>    (key encrypting the credentials)
//           'cred' <credentials>
//
//  SIGNED  ('SIGN' <len> )
//           'algo' <algorithm-name>
//
//  SIGNATURE ('SIGA' <len> )
//           'keyi' <fingerprint>   (key that signed the content)
//           'sign' <signature>
//           
//  COMPRESSION ( 'COMP' <len> )
//           'algo'  <algorithm-name>
//           'parm'  <compression-level>
//            'lu32' <original-data-length>
//          | 'lu64' <original-data-length>
//
//  DATA ( 'DATA' <len> )
//
//
#define M1SC_MAGIC     tag('M','1','S','C')   // data is version[4]

#define M1SC_ENCRYPTION     tag('C','I','P','H')
#define M1SC_SIGNED         tag('S','I','G','N')
#define M1SC_SIGNATURE      tag('S','I','G','A')
#define M1SC_COMPRESSION    tag('C','O','M','P')
#define M1SC_DATA           tag('D','A','T','A')

#define M1SC_ITEM_algo      tag('a','l','g','o')
#define M1SC_ITEM_keyi      tag('k','e','y','i')
#define M1SC_ITEM_sign      tag('s','i','g','n')
#define M1SC_ITEM_cred      tag('c','r','e','d')
#define M1SC_ITEM_len32     tag('l','u','3','2')
#define M1SC_ITEM_len64     tag('l','u','6','4')
#define M1SC_ITEM_len       tag('l','u','x','x')
#define M1SC_ITEM_parm      tag('p','a','r','m')

class CBioStream : public CRuntime {
public:
    CBioStream();
    ~CBioStream();

    int open_file_read(string aFileName, unsigned int aUsage,
		       bool aRequireEncryption,
		       bool aRequireSignature);
    int open_fp_read(string aFileName, FILE* fp, unsigned int aUsage,
		     bool aRequireEncryption,
		     bool aRequireSignatrue);

    int open_mem_read(string aFileName, 
		      char *aData,
		      int aDataLen,
		      unsigned int aUsage,
		      bool aRequireEncryption,
		      bool aRequireSignature);

    int open_mem_write(string aFileName);

    int open_file_write(string aFileName);
    int open_fp_write(string aFileName, FILE* fp);
    
    int read(void* aBuffer, int aLen);
    int gets(char* aBuffer, int aLen);
    int write(void* aBuffer, int aLen);
    int skip(int aLen);
    bool is_encrypted(void) { return mIsEncrypted; }
    bool is_compressed(void) { return mIsCompressed; }
    bool is_signed(void) { return mIsSigned; }
    void close();
    int  flush();
    bool should_retry();
    // CBioStream utils
    static int make_credentials(const EVP_CIPHER* cipher,
				unsigned char* key, unsigned char* iv);
    
    void push(BIO* aBf);
    void pop();
    BIO* find(int type);
    BIO *get_bio(void) { return mBio; }
    int write_tag(u_int32_t tag, u_int32_t len);
    int write_tag_data(u_int32_t tag, void* data, u_int32_t len);

    // operation on out stream
    int write_MAGIC(void);
    int write_DATA(u_int32_t len);
    int write_SIGNED(CBioStream* aIn);
    int write_SIGNATURE(CKey* sigkey, CBioStream* aIn);
    int write_ENCRYPTION(CKey* sigkey);
    int write_COMPRESSION(int compression_level, u_int64_t orig_len);
    int stream_DATA(CBioStream* aIn);

    // operation on input stream
    u_int32_t read_tag(u_int32_t* len);
    int       read_params(u_int32_t len, Param_t* params, int n);
    int       read_init(bool aRequireEncryption, bool aRequireSignature);

    int read_MAGIC(void);
    int read_SIGNED(u_int32_t len);
    int read_SIGNATURE(u_int32_t len);
    int read_ENCRYPTION(u_int32_t len);
    int read_COMPRESSION(u_int32_t len);

    string debugName(void);
private:
    BIO* mBio;
    string mFileName;
    static bool mSeeded;
    bool mIsEncrypted;
    bool mIsCompressed;
    bool mIsSigned;
};

#endif
