//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
// BIO zlib filter interface
//

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <memory.h>
#include <zlib.h>
#include <openssl/buffer.h>
#include <openssl/err.h>
#include <openssl/bio.h>

#include "bf_zlib.h"

static int zlib_write(BIO *h, const char *buf,int num);
static int zlib_read(BIO *h, char *buf, int size);
/* static int zlib_puts(BIO *h, const char *str); */
/* static int zlib_gets(BIO *h, char *str, int size); */
static long zlib_ctrl(BIO *h, int cmd, long arg1, void *arg2);
static int zlib_new(BIO *h);
static int zlib_free(BIO *data);
static long zlib_callback_ctrl(BIO *h, int cmd, bio_info_cb *fp);

#define ZLIB_BUFFER_SIZE	(1024*4)


typedef struct bio_f_zlib_ctx_struct
{
    int compression_level;
    int zerrno;
    int zeof;

    char *ibuf;
    unsigned long ibuf_size;
    z_stream istream;

    char *obuf;
    unsigned long obuf_size;
    z_stream ostream;
} BIO_F_ZLIB_CTX;

static BIO_METHOD methods_buffer=
{
    BIO_TYPE_ZLIB,
    "zlib",
    zlib_write,
    zlib_read,
    NULL, /* zlib_puts, */
    NULL, /* zlib_gets, */
    zlib_ctrl,
    zlib_new,
    zlib_free,
    zlib_callback_ctrl,
};

static void* zalloc(void* data, unsigned int items, unsigned int size)
{
    return (void*) calloc(items,size);
}

static void zfree(void* data, void* addr)
{
    free(addr);
}

BIO_METHOD *BIO_f_zlib(void)
{
    return(&methods_buffer);
}

static int zlib_new(BIO *bi)
{
    BIO_F_ZLIB_CTX *ctx;

    if ((ctx=(BIO_F_ZLIB_CTX *)OPENSSL_malloc(sizeof(BIO_F_ZLIB_CTX))) == NULL)
	return 0;
    if ((ctx->ibuf=(char *)OPENSSL_malloc(ZLIB_BUFFER_SIZE)) == NULL) {
	OPENSSL_free(ctx); 
	return 0;
    }
    if ((ctx->obuf=(char *)OPENSSL_malloc(ZLIB_BUFFER_SIZE)) == NULL) {
	OPENSSL_free(ctx->ibuf); 
	OPENSSL_free(ctx); 
	return 0; 
    }
    ctx->ibuf_size=ZLIB_BUFFER_SIZE;
    ctx->obuf_size=ZLIB_BUFFER_SIZE;

    ctx->compression_level=Z_DEFAULT_COMPRESSION;
    ctx->zerrno=Z_OK;
    ctx->zeof=0;

    ctx->ostream.zalloc = zalloc;
    ctx->ostream.zfree = zfree;
    ctx->ostream.opaque = Z_NULL;
    ctx->ostream.next_in=Z_NULL;
    ctx->ostream.next_out=Z_NULL;
    ctx->ostream.avail_in=ctx->ostream.avail_out=0;
    inflateInit(&ctx->ostream);

    ctx->istream.zalloc = zalloc;
    ctx->istream.zfree = zfree;
    ctx->istream.opaque = Z_NULL;
    ctx->istream.next_in=Z_NULL;
    ctx->istream.next_out=Z_NULL;
    ctx->istream.avail_in=ctx->istream.avail_out=0;
    deflateInit(&ctx->istream, ctx->compression_level);
    ctx->istream.next_out=(Bytef*)ctx->ibuf;
    ctx->istream.avail_out=ZLIB_BUFFER_SIZE;

    bi->init=1;
    bi->ptr=(void *)ctx;
    bi->flags=0;
    return(1);
}

static int zlib_free(BIO *b)
{
    BIO_F_ZLIB_CTX *ctx;

    if (b == NULL) return(0);
    ctx=(BIO_F_ZLIB_CTX *)b->ptr;
    deflateEnd(&(ctx->ostream));
/* FIXME with valgrind ... */
/*    inflateEnd(&(ctx->istream)); */
    if (ctx->ibuf != NULL) OPENSSL_free(ctx->ibuf);
    if (ctx->obuf != NULL) OPENSSL_free(ctx->obuf);
    OPENSSL_free(b->ptr);
    b->ptr=NULL;
    b->init=0;
    b->flags=0;
    return(1);
}
	
static int zlib_read(BIO *b, char *out, int outl)
{
    int i,num=0;
    BIO_F_ZLIB_CTX *ctx;

    if (out == NULL) return(0);
    ctx=(BIO_F_ZLIB_CTX *)b->ptr;

    if ((ctx == NULL) || (b->next_bio == NULL)) return(0);
    if ((ctx->zerrno == Z_DATA_ERROR) || (ctx->zerrno == Z_ERRNO)) return(-1);
    if (ctx->zerrno == Z_STREAM_END) return(0);

    num=0;
    BIO_clear_retry_flags(b);

    ctx->ostream.next_out=(Bytef*)out;
    ctx->ostream.avail_out=outl;
    
    while (ctx->ostream.avail_out != 0) {
	if (ctx->ostream.avail_in == 0 && !ctx->zeof) {
	    i=BIO_read(b->next_bio,ctx->obuf,ctx->obuf_size);
	    if (i <= 0) {
		BIO_copy_next_retry(b);
		if (i < 0)
		    return((num > 0)?num:i);
		if (i == 0) {
		    ctx->zeof=1;
		    if (BIO_eof(b)) ctx->zerrno = Z_ERRNO;
		    return(num);
		}
	    }
	    ctx->ostream.avail_in=i;
	    ctx->ostream.next_in=(Bytef*)ctx->obuf;
	}
	ctx->zerrno=inflate(&(ctx->ostream), Z_NO_FLUSH);
	if (ctx->zerrno != Z_OK || ctx->zeof) break;
    }
    num=outl-ctx->ostream.avail_out;
    return(num);
}

static int zlib_write(BIO *b, const char *in, int inl)
{
    int i,num=0;
    BIO_F_ZLIB_CTX *ctx;

    if ((in == NULL) || (inl <= 0)) return(0);
    ctx=(BIO_F_ZLIB_CTX *)b->ptr;
    if ((ctx == NULL) || (b->next_bio == NULL)) return(0);

    BIO_clear_retry_flags(b);
    
    ctx->istream.next_in=(Bytef*)in;
    ctx->istream.avail_in=inl;

    while (ctx->istream.avail_in != 0) {
	if (ctx->istream.avail_out == 0) {
	    ctx->istream.next_out=(Bytef*)ctx->ibuf;
	    i=BIO_write(b->next_bio,ctx->ibuf,ctx->ibuf_size);
	    if (i <= 0) {
		BIO_copy_next_retry(b);
		if (i < 0) return((num > 0)?num:i);
		if (i == 0) return(num);
	    }
	    ctx->istream.avail_out=ctx->ibuf_size;
	}
	ctx->zerrno=deflate(&(ctx->istream), Z_NO_FLUSH);
	if (ctx->zerrno != Z_OK) break;
    }
    num=(inl-ctx->istream.avail_in);
    return(num);
}

static long zlib_ctrl(BIO *b, int cmd, long num, void *ptr)
{
    BIO_F_ZLIB_CTX *ctx;
    unsigned int i,len;
    long ret=1,done=0;

    ctx=(BIO_F_ZLIB_CTX *)b->ptr;

    switch (cmd) {
    case BIO_CTRL_FLUSH:
	if (b->next_bio == NULL) return(0);
	ctx->istream.avail_in=0;

	for (;;) {
	    len=ctx->ibuf_size-ctx->istream.avail_out;
	    if (len != 0) {
		i=BIO_write(b->next_bio,ctx->ibuf,len);
		if (i != len) {
		    ctx->zerrno = Z_ERRNO;
		    break;
		}
		ctx->istream.next_out=(Bytef*)ctx->ibuf;
		ctx->istream.avail_out=ctx->ibuf_size;
	    }
	    if (done) break;
	    ctx->zerrno=deflate(&(ctx->istream), Z_FINISH);
	    
	    if ((len == 0) && (ctx->zerrno == Z_BUF_ERROR))
		ctx->zerrno=Z_OK;
	    done=((ctx->istream.avail_out != 0) || 
		  (ctx->zerrno == Z_STREAM_END));
	    if ((ctx->zerrno != Z_OK) && (ctx->zerrno != Z_STREAM_END)) break;
	}
	ret=BIO_ctrl(b->next_bio,cmd,num,ptr);
	break;

    case BIO_CTRL_ZLIB_SET_COMPRESSION:
	if (num < 0 || num > 9) num=Z_DEFAULT_COMPRESSION;
	ctx->compression_level=num;
	ret=deflateParams(&ctx->istream, ctx->compression_level, Z_DEFAULT_STRATEGY);
	if (ret != Z_OK)
	    ret=0;
	else
	    ret=1;
	break;
    default:
	ret=BIO_ctrl(b->next_bio,cmd,num,ptr);
	break;
    }
    return ret;

}

static long zlib_callback_ctrl(BIO *b, int cmd, bio_info_cb *fp)
{
    long ret=1;

    if (b->next_bio == NULL) return 0;
    switch (cmd)
    {
    default:
	ret=BIO_callback_ctrl(b->next_bio,cmd,fp);
	break;
    }
    return ret;
}
