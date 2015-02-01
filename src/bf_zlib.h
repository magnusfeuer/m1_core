#ifndef __BF_ZLIB_H__
#define __BF_ZLIB_H__

#define BIO_TYPE_ZLIB		(12|0x0200)		/* filter BIO */
#define BIO_CTRL_ZLIB_SET_COMPRESSION   224

#ifdef __cplusplus
extern "C" {
#endif

BIO_METHOD *BIO_f_zlib(void);

#define BIO_set_compression(b,level)    BIO_ctrl(b,BIO_CTRL_ZLIB_SET_COMPRESSION,level,NULL);

#ifdef __cplusplus
}
#endif

#endif
