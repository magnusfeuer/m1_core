/*
 * SSE2 support
 *
 */
#include "epic.h"
#include "epic_simd.h"
#include "epic_simd_sse2.h"

#define SSE2_MIN_BLOCK_LEN 64

void ESimdCopy_sse2(const u_int8_t* src, u_int8_t* dst, size_t n)
{
    if (n >= SSE2_MIN_BLOCK_LEN) {
	unsigned int offs;
	int m;

	offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
	if (offs) {
	    ESimdCopy_x86(src, dst, offs);
	    src += offs;
	    dst += offs;
	    n -= offs;
	}
	offs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
	m = n / (ESimdVectorSize*4);
	n &= ((ESimdVectorSize*4)-1);
	if (offs) {
	    while(m--) {
		ESimdVectorU32_t a,b,c,d;

		ESimdPrefetch(src+256);
		a = ESimdVectorLoadUA32(src);
		b = ESimdVectorLoadUA32(src+ESimdVectorSize);
		c = ESimdVectorLoadUA32(src+ESimdVectorSize*2);
		d = ESimdVectorLoadUA32(src+ESimdVectorSize*3);

		*((ESimdVectorU8_t*)dst) = a;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize)) = b;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*2)) = c;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*3)) = d;
		src += ESimdVectorSize*4;
		dst += ESimdVectorSize*4;
	    }
	}
	else {
	    while(m--) {
		ESimdVectorU32_t a,b,c,d;

		ESimdPrefetch(src+256);
		a=*((ESimdVectorU8_t*)(src));
		b=*((ESimdVectorU8_t*)(src+ESimdVectorSize));
		c=*((ESimdVectorU8_t*)(src+ESimdVectorSize*2));
		d=*((ESimdVectorU8_t*)(src+ESimdVectorSize*3));

		*((ESimdVectorU8_t*)dst) = a;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize)) = b;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*2)) = c;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*3)) = d;
		src += ESimdVectorSize*4;
		dst += ESimdVectorSize*4;
	    }
	}
    }
    if (n)
	ESimdCopy_x86(src, dst, n);
}

/* This code assumes dst i at least 32 bit aligned (FIXME?) */
void ESimdFill32_sse2(u_int8_t* dst, u_int32_t v, size_t n)
{
    if (n < 4)
	ESimdFill32_x86((u_int32_t*)dst, v, n);
    else {
	ESimdVectorU8_t s8;
	unsigned int offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
	int walign = offs ? EMinInt((offs/4), n) : 0;

	s8 = ESimdVectorSet32(v,v,v,v);

	if (walign) {
	    ESimdFill32_x86((u_int32_t*)dst, v, walign);
	    dst += offs;
	    n -= walign;
	}

	while(n > ESimdVectorSize) {
	    *((ESimdVectorU8_t*)dst) = s8;
	    *((ESimdVectorU8_t*)(dst+ESimdVectorSize)) = s8;
	    *((ESimdVectorU8_t*)(dst+ESimdVectorSize*2)) = s8;
	    *((ESimdVectorU8_t*)(dst+ESimdVectorSize*3)) = s8;
	    dst += ESimdVectorSize*4;
	    n -= ESimdVectorSize;
	}

	while(n >= ESimdVectorSize/4) {
	    *((ESimdVectorU8_t*)dst) = s8;
	    dst += ESimdVectorSize;
	    n -= ESimdVectorSize/4;
	}
	if (n)
	    ESimdFill32_x86((u_int32_t*)dst, v, n);
    }
}


/*
 *  Add source with color and blend it with dst, using fade as a scaling factor
 */
void ESimdAddBlendAreaRGBA32_sse2(u_int8_t* src, int src_wb,
				  u_int8_t* dst, int dst_wb,
				  u_int8_t af, EPixel_t color,
				  unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    int iaf = af;
    ESimdVectorI16_t fv = ESimdVectorSplat16(iaf);
    ESimdVectorI8_t  c8 = ESimdVectorSetPIXEL(color.b,color.g,color.r,color.a);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAddBlendRowRGBA32(src1,dst1,af,color,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		ts = _mm_adds_epu8(c8, ts);
		td = ESimdFadeRGBA32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAddBlendRowRGBA32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAddBlendRowRGBA32(src1,dst1,af,color,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		ts = _mm_adds_epu8(c8, ts);
		td = ESimdFadeRGBA32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAddBlendRowRGBA32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
}


void ESimdAddBlendAreaARGB32_sse2(u_int8_t* src, int src_wb,
				  u_int8_t* dst, int dst_wb,
				  u_int8_t af, EPixel_t color,
				  unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    int iaf = af;
    ESimdVectorI16_t fv = ESimdVectorSplat16(iaf);
    ESimdVectorI8_t  c8 = ESimdVectorSetPIXEL(color.a,color.r,color.g,color.b);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAddBlendRowARGB32(src1,dst1,af,color,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		ts = _mm_adds_epu8(c8, ts);
		td = ESimdFadeARGB32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAddBlendRowARGB32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAddBlendRowARGB32(src1,dst1,af,color,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		ts = _mm_adds_epu8(c8, ts);
		td = ESimdFadeARGB32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAddBlendRowARGB32(src1,dst1,af,color,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
}

/*
 *  Add source (a8) with color and blend it with dst, 
 *  using fade as a scaling factor.
 */
void ESimdAddBlendAreaA8_RGBA32_sse2(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, EPixel_t color,
				     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = 0;
    int iaf = af;
    /* FIXME: af=255 => 0x0100 */
    ESimdVectorI16_t fv = ESimdVectorSplat16(iaf);
    ESimdVectorI8_t  c8 = ESimdVectorSetPIXEL(color.r,color.g,color.b,color.a);

    if (doffs != 0)
	walign = EMinInt((doffs/4), width);
	
    while(height > 0) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	if (walign) {
	    EDirectAddBlendRowA8_RGBA32(src1,dst1,af,color,walign);
	    src1 += walign;
	    dst1 += doffs;
	    width1 -= walign;
	}
	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    u_int32_t a0 = src1[0] << 24;
	    u_int32_t a1 = src1[1] << 24;
	    u_int32_t a2 = src1[2] << 24;
	    u_int32_t a3 = src1[3] << 24;
	    ESimdVectorU32_t ts = ESimdVectorSet32(a0,a1,a2,a3);
//	    ESimdVectorU32_t ts = ESimdVectorSet8(0,0,0,src1[0],
//					  0,0,0,src1[1],
//						  0,0,0,src1[2],
//						  0,0,0,src1[3]);
	    ts = _mm_adds_epu8(c8, ts);
	    td = ESimdFadeRGBA32_sse2(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += 4;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	if (width1)
	    EDirectAddBlendRowA8_RGBA32(src1,dst1,af,color,width1);
	src += src_wb;
	dst += dst_wb;
	height--;
    }
}


void ESimdAddBlendAreaA8_ARGB32_sse2(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, EPixel_t color,
				     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = 0;
    int iaf = af;
    ESimdVectorI16_t fv = ESimdVectorSplat16(iaf);
    ESimdVectorI8_t  c8 = ESimdVectorSetPIXEL(color.a,color.r,color.g,color.b);

    if (doffs != 0)
	walign = EMinInt((doffs/4), width);
	
    while(height > 0) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	if (walign) {
	    EDirectAddBlendRowA8_ARGB32(src1,dst1,af,color,walign);
	    src1 += walign;
	    dst1 += doffs;
	    width1 -= walign;
	}
	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    u_int32_t a0 = src1[0];
	    u_int32_t a1 = src1[1];
	    u_int32_t a2 = src1[2];
	    u_int32_t a3 = src1[3];
	    ESimdVectorU32_t ts = ESimdVectorSet32(a0,a1,a2,a3);
//	    ESimdVectorU32_t ts = ESimdVectorSet8(src1[0],0,0,0,
//						  src1[1],0,0,0,
//						  src1[2],0,0,0,
//						  src1[3],0,0,0);
	    ts = _mm_adds_epu8(c8, ts);
	    td = ESimdFadeARGB32_sse2(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += 4;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	if (width1)
	    EDirectAddBlendRowA8_ARGB32(src1,dst1,af,color,width1);
	src += src_wb;
	dst += dst_wb;
	height--;
    }
}

void ESimdAlphaAreaARGB32_sse2(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t a, unsigned int width,
			      unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI8_t a8 = ESimdVectorSetPIXEL(0,a,a,a);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAlphaRowARGB32(src1,dst1,a,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_sse2(a8,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAlphaRowARGB32(src1, dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);
	
	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAlphaRowARGB32(src1,dst1,a,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
		
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_sse2(a8,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAlphaRowARGB32(src1,dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }    
}

void ESimdAlphaAreaRGBA32_sse2(u_int8_t* src, int src_wb,
			       u_int8_t* dst, int dst_wb,
			       u_int8_t a, unsigned int width,
			       unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI8_t a8 = ESimdVectorSetPIXEL(a,a,a,0);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);

	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectAlphaRowRGBA32(src1,dst1,a,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
		    
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_sse2(a8,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAlphaRowRGBA32(src1, dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);
	
	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;
	    if (walign) {
		EDirectAlphaRowRGBA32(src1,dst1,a,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
		
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_sse2(a8,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectAlphaRowRGBA32(src1,dst1,a,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }    
}


void ESimdBlendAreaRGBA32_sse2(u_int8_t* src, int src_wb, 
			       u_int8_t* dst, int dst_wb,
			       unsigned int width, unsigned int height)
{
    int walign = 0;
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);

    if (soffs != doffs) { // UNALIGNABLE align dst
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectBlendRowRGBA32(src1, dst1, walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t  td = *((ESimdVectorU8_t*)dst1);
		td = ESimdBlendRGBA32_sse2(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowRGBA32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectBlendRowRGBA32(src1, dst1, walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdBlendRGBA32_sse2(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowRGBA32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
}

void ESimdBlendAreaARGB32_sse2(u_int8_t* src, int src_wb, 
			       u_int8_t* dst, int dst_wb,
			       unsigned int width, unsigned int height)
{
    int walign = 0;
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectBlendRowARGB32(src1, dst1, walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdBlendARGB32_sse2(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowARGB32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;
	    
	    if (walign) {
		EDirectBlendRowARGB32(src1, dst1, walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdBlendARGB32_sse2(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowARGB32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
}


void ESimdFadeAreaRGBA32_sse2(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t af, 
			      unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    int iaf = af;
    ESimdVectorI16_t fv = ESimdVectorSplat16(iaf);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectFadeRowRGBA32(src1,dst1,af,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdFadeRGBA32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowRGBA32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);

	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectFadeRowRGBA32(src1,dst1,af,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdFadeRGBA32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowRGBA32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
		    height--;
	}
    }
}

void ESimdFadeAreaARGB32_sse2(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t af, 
			      unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    int iaf = af;
    ESimdVectorI16_t fv = ESimdVectorSplat16(iaf);


    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectFadeRowARGB32(src1,dst1,af,walign);
		src1 += doffs;
		dst1 += doffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdFadeARGB32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowARGB32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
    else {
	if (soffs != 0)
	    walign = EMinInt((soffs/4), width);
	while(height > 0) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    if (walign) {
		EDirectFadeRowARGB32(src1,dst1,af,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdFadeARGB32_sse2(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowARGB32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	    height--;
	}
    }
}



/* DO  RGB or BGR (BGR is done by swapping r and b in p) */
void ESimdFillAreaBlendRGB24_sse2(u_int8_t* dst,int dst_wb,
				 unsigned int width, unsigned int height, 
				 EPixel_t p)
{
    /* Maybe use one rotating register ? instead of 3 ... */
    ESimdVectorI8_t s8_0 = ESimdVectorSet8(p.r,p.g,p.b,p.r,p.g,p.b,p.r,p.g,
					   p.b,p.r,p.g,p.b,p.r,p.g,p.b,p.r);
    ESimdVectorI8_t s8_1 = ESimdVectorSet8(p.g,p.b,p.r,p.g,p.b,p.r,p.g,p.b,
					   p.r,p.g,p.b,p.r,p.g,p.b,p.r,p.g);
    ESimdVectorI8_t s8_2 = ESimdVectorSet8(p.b,p.r,p.g,p.b,p.r,p.g,p.b,p.r,
					   p.g,p.b,p.r,p.g,p.b,p.r,p.g,p.b);
    ESimdVectorI8_t a8 = ESimdVectorSplat8(p.a);
    unsigned int offs = EPIC_ALIGN_OFFS(dst, ESimdVectorAlign);
    unsigned int wb = width*3; // Number of bytes

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int offs1  = offs;
	unsigned int wb1  = wb;
	
	// Align data pointer
	while((offs1>=3) && (wb1>=3)) {
	    dst1[0] = eblend(p.a,p.r,dst1[0]);
	    dst1[1] = eblend(p.a,p.g,dst1[1]);
	    dst1[2] = eblend(p.a,p.b,dst1[2]);
	    dst1  += 3;
	    offs1 -= 3;
	    wb1   -= 3;
	}
	offs1 = EMinInt(offs1, wb1);
	switch(offs1) {
	case 1:
	    dst1[0] = eblend(p.a,p.r,dst1[0]);
	    dst1 += 1;
	    wb1  -= 1;
	    break;
	case 2:
	    dst1[0] = eblend(p.a,p.r,dst1[0]);
	    dst1[1] = eblend(p.a,p.g,dst1[1]);
	    dst1 += 2;
	    wb1  -= 2;
	    break;
	default:
	    /* either offs=0 or wb=0 */
	    break;
	}
	while(wb1 >= ESimdVectorSize) {
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    switch(offs1) {
	    case 0: td = ESimdAlpha32_sse2(a8,s8_0,td); break;
	    case 1: td = ESimdAlpha32_sse2(a8,s8_1,td); break;
	    case 2: td = ESimdAlpha32_sse2(a8,s8_2,td); break;
	    default: return;
	    }
	    *((ESimdVectorU8_t*)dst1) = td;
	    dst1 += ESimdVectorSize;
	    wb1  -= ESimdVectorSize;
	    offs1 = (offs1+ESimdVectorSize) % 3;
	}
	// Do remaining RGB/BGR components
	while(wb1--) {
	    switch(offs1) {
	    case 0: *dst1 = eblend(p.a,p.r,dst1[0]); break;
	    case 1: *dst1 = eblend(p.a,p.g,dst1[0]); break;
	    case 2: *dst1 = eblend(p.a,p.b,dst1[0]); break;
	    default: return;
	    }
	    dst1++;
	    offs1 = (offs1+1) % 3;
	}
	dst += dst_wb;
    }
}


void ESimdFillAreaBlendARGB32_sse2(u_int8_t* dst,int dst_wb,
				  unsigned int width, unsigned int height, 
				  EPixel_t p)
{
    ESimdVectorI8_t s8, a8;
    unsigned int offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = offs ? EMinInt((offs/4), width) : 0;

    s8 = ESimdVectorSetPIXEL(0,p.r,p.g,p.b);
    a8 = ESimdVectorSplat8(p.a);

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;

	if (width1 < 4)
	    EDirectFillRowBlendARGB32(dst1,width1,p.a,p.r,p.g,p.b);
	else {
	    if (walign) {
		EDirectFillRowBlendARGB32(dst1,walign,p.a,p.r,p.g,p.b);
		dst1   += offs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_sse2(a8,s8,td);
		*((ESimdVectorU8_t*)dst1) = td;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFillRowBlendARGB32(dst1,width1,p.a,p.r,p.g,p.b);
	}
	dst += dst_wb;
    }
}

void ESimdFillAreaBlendRGBA32_sse2(u_int8_t* dst,int dst_wb,
				  unsigned int width, unsigned int height, 
				  EPixel_t p)
{
    ESimdVectorI8_t s8, a8;
    unsigned int offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = offs ? EMinInt((offs/4), width) : 0;

    s8 = ESimdVectorSetPIXEL(p.r,p.g,p.b,p.a);
    a8 = ESimdVectorSplat8(p.a);

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;
    
	if (width1 < 4)
	    EDirectFillRowBlendRGBA32(dst1,width1,p.a,p.r,p.g,p.b);
	else {
	    if (walign) {
		EDirectFillRowBlendRGBA32(dst1,walign,p.a,p.r,p.g,p.b);
		dst1   += offs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_sse2(a8,s8,td);
		*((ESimdVectorU8_t*)dst1) = td;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFillRowBlendRGBA32(dst1,width1,p.a,p.r,p.g,p.b);
	    dst += dst_wb;
	}
    }
}
