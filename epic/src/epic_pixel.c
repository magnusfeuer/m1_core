
#include "epic.h"

/* unpack functions */
EPixel_t epic_unpack_a8(u_int8_t* src)
{
    EPixel_t p;
    p.px = src[0]<<24;
    return p;
}

EPixel_t epic_unpack_r8(u_int8_t* src)
{
    EPixel_t p;
    p.px = (0xff00 + src[0]) << 16;
    return p;
}

EPixel_t epic_unpack_g8(u_int8_t* src)
{
    EPixel_t p;
    p.px = (0xff0000 + src[0]) << 8;
    return p;
}

EPixel_t epic_unpack_b8(u_int8_t* src)
{
    EPixel_t p;
    p.px = (0xff000000 + src[0]);
    return p;
}

EPixel_t epic_unpack_r8g8b8(u_int8_t* src)
{
    EPixel_t p;
    p.px = ((0xff00+src[0]) << 16) + (src[1]<<8) + src[2];
    return p;
}

EPixel_t epic_unpack_r8g8b8a8(u_int8_t* src)
{
    EPixel_t p;    
    p.r=src[0]; p.g=src[1]; p.b=src[2]; p.a=src[3];
    return p;
}


EPixel_t epic_unpack_a8r8g8b8(u_int8_t* src)
{
    EPixel_t p;
#if BYTE_ORDER == BIG_ENDIAN
    p.px = *((u_int32_t*) src);
#else
    u_int32_t t = *((u_int32_t*) src);
    p.px = (t << 24) | (t >> 24) |
	((t >> 8) & 0xFF00) |
	((t << 8) & 0xFF0000);
#endif
    return p;
}

EPixel_t epic_unpack_b8g8r8(u_int8_t* src)
{
    EPixel_t p;
    p.b=src[0]; p.g=src[1]; p.r=src[2]; p.a = 255;
    return p;
}

EPixel_t epic_unpack_b8g8r8a8(u_int8_t* src)
{
    EPixel_t p;
#if BYTE_ORDER == BIG_ENDIAN
    p.v[PX_B]=src[0]; 
    p.v[PX_G]=src[1]; 
    p.v[PX_R]=src[2]; 
    p.v[PX_A]=src[3];
#else
    p.px = *((u_int32_t*) src);
#endif
    return p;
}

EPixel_t epic_unpack_a8b8g8r8(u_int8_t* src)
{
    EPixel_t p;
    p.a=src[0]; p.b=src[1]; p.g=src[2]; p.r=src[3];
    return p;
}

EPixel_t epic_unpack_a8y8u8v8(u_int8_t* src)
{
    EPixelYUV_t yuv;
    EPixel_t p;
    yuv.a = src[0];
    yuv.y = src[1];
    yuv.u = src[2];
    yuv.v = src[3];
    p = EPixelYUV2RGB(yuv);
    return p;
}

EPixel_t epic_unpack_r5g5b5a1(u_int8_t* src)
{
    EPixel_t p;
    u_int16_t v = src[0]<<8 | src[1];
    p.r = (v >> 8) & 0xf8;
    p.g = (v >> 3) & 0xf8;
    p.b = (v << 2) & 0xf8;
    p.a = (v&1) ? 255 : 0;
    return p;
}

EPixel_t epic_unpack_r5g5b5x1(u_int8_t* src)
{
    EPixel_t p;
    u_int16_t v = src[0]<<8 | src[1];
    p.r = (v >> 8) & 0xf8;
    p.g = (v >> 3) & 0xf8;
    p.b = (v << 2) & 0xf8;
    p.a = 255;
    return p;
}

EPixel_t epic_unpack_a1r5g5b5(u_int8_t* src)
{
    EPixel_t p;
    u_int16_t v = src[0]<<8 | src[1];
    p.r = (v >> 7) & 0xf8;
    p.g = (v >> 2) & 0xf8;
    p.b = (v << 3) & 0xf8;
    p.a = (v >> 15) ? 255 : 0;
    return p;
}

EPixel_t epic_unpack_x1r5g5b5(u_int8_t* src)
{
    EPixel_t p;
    u_int16_t v = src[0]<<8 | src[1];
    p.r = (v >> 7) & 0xf8;
    p.g = (v >> 2) & 0xf8;
    p.b = (v << 3) & 0xf8;
    p.a = 255;
    return p;
}

EPixel_t epic_unpack_r5g6b5_be(u_int8_t* src)
{
    EPixel_t p;
    u_int16_t v = src[0]<<8 | src[1];
    /* shift and scale to range [0-255] */
    p.r=(v >> 8) & 0xf8;
    p.g=(v >> 3) & 0xfc;
    p.b=(v & 0x1f) << 3;
    p.a=255;
    return p;
}

EPixel_t epic_unpack_r5g6b5_le(u_int8_t* src)
{
    EPixel_t p;
    u_int16_t v = src[1]<<8 | src[0];
    /* shift and scale to range [0-255] */
    p.r=(v >> 8) & 0xf8;
    p.g=(v >> 3) & 0xfc;
    p.b=(v & 0x1f) << 3;
    p.a=255;
    return p;
}

EPixelUnpack_t EPixelUnpackFunc(int pixel_type)
{
    switch (pixel_type) {
    case EPIXEL_TYPE_A8: return epic_unpack_a8;
    case EPIXEL_TYPE_R8: return epic_unpack_r8;
    case EPIXEL_TYPE_G8: return epic_unpack_g8;
    case EPIXEL_TYPE_B8: return epic_unpack_b8;
    case EPIXEL_TYPE_R8G8B8: return epic_unpack_r8g8b8; 
    case EPIXEL_TYPE_R8G8B8A8: return epic_unpack_r8g8b8a8;
    case EPIXEL_TYPE_A8R8G8B8: return epic_unpack_a8r8g8b8;
    case EPIXEL_TYPE_B8G8R8: return epic_unpack_b8g8r8;
    case EPIXEL_TYPE_B8G8R8A8: return epic_unpack_b8g8r8a8;
    case EPIXEL_TYPE_A8B8G8R8: return epic_unpack_a8b8g8r8;
    case EPIXEL_TYPE_A8Y8U8V8: return epic_unpack_a8y8u8v8;
    case EPIXEL_TYPE_R5G5B5A1: return epic_unpack_r5g5b5a1;
    case EPIXEL_TYPE_R5G5B5X1: return epic_unpack_r5g5b5x1;
    case EPIXEL_TYPE_A1R5G5B5: return epic_unpack_a1r5g5b5;
    case EPIXEL_TYPE_X1R5G5B5: return epic_unpack_x1r5g5b5;
    case EPIXEL_TYPE_565_BE: return epic_unpack_r5g6b5_be;
    case EPIXEL_TYPE_565_LE: return epic_unpack_r5g6b5_le;
    default:
	fprintf(stderr, "EPixelUnpackFunc: undefined func: %x\n", pixel_type);
	return NULL;
    }
}


void epic_pack_a8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.a;
}

void epic_pack_r8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.r;
}

void epic_pack_g8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.g;
}

void epic_pack_b8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.b;
}

void epic_pack_r8g8b8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.r;
    dst[1]=p.g;
    dst[2]=p.b;
}

void epic_pack_r8g8b8a8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.r; 
    dst[1]=p.g;
    dst[2]=p.b;
    dst[3]=p.a;
}

void epic_pack_a8r8g8b8(EPixel_t p, u_int8_t* dst)
{
#if BYTE_ORDER == BIG_ENDIAN
    *((u_int32_t*) dst) = p.px;
#else
    dst[0]=p.a;
    dst[1]=p.r;
    dst[2]=p.g;
    dst[3]=p.b;
#endif
}

void epic_pack_b8g8r8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.b;
    dst[1]=p.g;
    dst[2]=p.r;
}

void epic_pack_b8g8r8a8(EPixel_t p, u_int8_t* dst)
{
#if BYTE_ORDER == BIG_ENDIAN
    dst[0]=p.b;
    dst[1]=p.g;
    dst[2]=p.r;
    dst[3]=p.a;
#else
    *((u_int32_t*) dst) = p.px;
#endif
}

void epic_pack_a8b8g8r8(EPixel_t p, u_int8_t* dst)
{
    dst[0]=p.a;
    dst[1]=p.b;
    dst[2]=p.g;
    dst[3]=p.r; 
}

void epic_pack_r5g5b5a1(EPixel_t p, u_int8_t* dst)
{
    u_int16_t v = ((p.r>>3)<<11)|((p.g>>3)<<6)|((p.b>>3) <<1)|((p.a)!=0);
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epic_pack_r5g5b5x1(EPixel_t p, u_int8_t* dst)
{
    u_int16_t v = ((p.r>>3)<<11)|((p.g>>3)<<6)|((p.b>>3) <<1);
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epic_pack_a1r5g5b5(EPixel_t p, u_int8_t* dst)
{
    u_int16_t v = ((p.r>>3)<<10)|((p.g>>3)<<5)|((p.b>>3))|(((p.a)!=0)<<15);
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epic_pack_x1r5g5b5(EPixel_t p, u_int8_t* dst)
{
    u_int16_t v = ((p.r>>3)<<10)|((p.g>>3)<<5)|((p.b>>3));
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epic_pack_r5g6b5_be(EPixel_t p, u_int8_t* dst)
{
    u_int16_t v = ((p.r>>3) << 11) | ((p.g>>2) << 5)  | ((p.b>>3));
    dst[0] = (v >> 8);
    dst[1] = v & 0xff;
}

void epic_pack_r5g6b5_le(EPixel_t p, u_int8_t* dst)
{
    u_int16_t v = ((p.r>>3) << 11) | ((p.g>>2) << 5)  | ((p.b>>3));
    dst[0] = v & 0xff;
    dst[1] = (v >> 8);
}

void epic_pack_a8y8u8v8(EPixel_t p, u_int8_t* dst)
{
    EPixelYUV_t yuv = EPixelRGB2YUV(p);
    dst[0] = yuv.a;
    dst[1] = yuv.y;
    dst[2] = yuv.u;
    dst[3] = yuv.v;
}

EPixelPack_t EPixelPackFunc(int pixel_type)
{
    switch (pixel_type) {
    case EPIXEL_TYPE_A8: return epic_pack_a8;
    case EPIXEL_TYPE_R8: return epic_pack_r8;
    case EPIXEL_TYPE_G8: return epic_pack_g8;
    case EPIXEL_TYPE_B8: return epic_pack_b8;
    case EPIXEL_TYPE_R8G8B8: return epic_pack_r8g8b8; 
    case EPIXEL_TYPE_R8G8B8A8: return epic_pack_r8g8b8a8;
    case EPIXEL_TYPE_A8R8G8B8: return epic_pack_a8r8g8b8;
    case EPIXEL_TYPE_B8G8R8: return epic_pack_b8g8r8;
    case EPIXEL_TYPE_B8G8R8A8: return epic_pack_b8g8r8a8;
    case EPIXEL_TYPE_A8B8G8R8: return epic_pack_a8b8g8r8;
    case EPIXEL_TYPE_A8Y8U8V8: return epic_pack_a8y8u8v8;
    case EPIXEL_TYPE_R5G5B5A1: return epic_pack_r5g5b5a1;
    case EPIXEL_TYPE_R5G5B5X1: return epic_pack_r5g5b5x1;
    case EPIXEL_TYPE_A1R5G5B5: return epic_pack_a1r5g5b5;
    case EPIXEL_TYPE_X1R5G5B5: return epic_pack_x1r5g5b5;
    case EPIXEL_TYPE_565_BE: return epic_pack_r5g6b5_be;
    case EPIXEL_TYPE_565_LE: return epic_pack_r5g6b5_le;
    default:
	fprintf(stderr, "EPixelUnpackFunc: undefined func: %x\n", pixel_type);
	return NULL;
    }
}



