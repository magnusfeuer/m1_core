/*
 * MMX support
 *
 */
#include "epic.h"
#include "epic_simd.h"
#include "epic_simd_mmx.h"

#define MIN_LEN 0x800

void ESimdCopy_mmx(const u_int8_t* src, u_int8_t* dst, size_t n)
{
    if (n >= MIN_LEN) {
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
	m = n / (ESimdVectorSize*8);
	n &= ((ESimdVectorSize*8)-1);
	if (offs) {
	    while(m) {
		ESimdVectorU32_t a = ESimdVectorLoadUA32(src);
		ESimdVectorU32_t b = ESimdVectorLoadUA32(src+ESimdVectorSize);
		ESimdVectorU32_t c = ESimdVectorLoadUA32(src+ESimdVectorSize*2);
		ESimdVectorU32_t d = ESimdVectorLoadUA32(src+ESimdVectorSize*3);
		ESimdVectorU32_t e = ESimdVectorLoadUA32(src+ESimdVectorSize*4);
		ESimdVectorU32_t f = ESimdVectorLoadUA32(src+ESimdVectorSize*5);
		ESimdVectorU32_t g = ESimdVectorLoadUA32(src+ESimdVectorSize*6);
		ESimdVectorU32_t h = ESimdVectorLoadUA32(src+ESimdVectorSize*7);
		
		*((ESimdVectorU8_t*)dst) = a;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize)) = b;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*2)) = c;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*3)) = d;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*4)) = e;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*5)) = f;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*6)) = g;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*7)) = h;
		src += ESimdVectorSize*8;
		dst += ESimdVectorSize*8;
		m--;
	    }
	}
	else {
	    while(m) {
		ESimdVectorU32_t a=*((ESimdVectorU8_t*)(src));
		ESimdVectorU32_t b=*((ESimdVectorU8_t*)(src+ESimdVectorSize));
		ESimdVectorU32_t c=*((ESimdVectorU8_t*)(src+ESimdVectorSize*2));
		ESimdVectorU32_t d=*((ESimdVectorU8_t*)(src+ESimdVectorSize*3));
		ESimdVectorU32_t e=*((ESimdVectorU8_t*)(src+ESimdVectorSize*4));
		ESimdVectorU32_t f=*((ESimdVectorU8_t*)(src+ESimdVectorSize*5));
		ESimdVectorU32_t g=*((ESimdVectorU8_t*)(src+ESimdVectorSize*6));
		ESimdVectorU32_t h=*((ESimdVectorU8_t*)(src+ESimdVectorSize*7));
		*((ESimdVectorU8_t*)dst) = a;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize)) = b;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*2)) = c;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*3)) = d;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*4)) = e;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*5)) = f;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*6)) = g;
		*((ESimdVectorU8_t*)(dst+ESimdVectorSize*7)) = h;
		src += ESimdVectorSize*8;
		dst += ESimdVectorSize*8;
		m--;
	    }
	}
    }
    if (n)
	ESimdCopy_x86(src, dst, n);
}


/* This code assumes dst i at least 32 bit aligned (FIXME?) */
void ESimdFill32_mmx(u_int8_t* dst, u_int32_t v, size_t n)
{
    ESimdVectorU8_t s8;
    unsigned int offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = offs ? EMinInt((offs/4), n) : 0;

    s8 = ESimdVectorSet32(v,v);

    if (n < 2)
	ESimdFill32_x86((u_int32_t*)dst, v, n);
    else {
	if (walign) {
	    ESimdFill32_x86((u_int32_t*)dst, v, walign);
	    dst += offs;
	    n -= walign;
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
void ESimdAddBlendAreaRGBA32_mmx(u_int8_t* src, int src_wb,
				  u_int8_t* dst, int dst_wb,
				  u_int8_t af, EPixel_t color,
				  unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
    ESimdVectorI8_t  c8 = ESimdVectorSetPIXEL(color.r,color.g,color.b,color.a);

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
		ts = _mm_adds_pu8(c8, ts);
		td = ESimdFadeRGBA32_mmx(fv,ts,td);
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
		ts = _mm_adds_pu8(c8, ts);
		td = ESimdFadeRGBA32_mmx(fv,ts,td);
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


void ESimdAddBlendAreaARGB32_mmx(u_int8_t* src, int src_wb,
				  u_int8_t* dst, int dst_wb,
				  u_int8_t af, EPixel_t color,
				  unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
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
		ts = _mm_adds_pu8(c8, ts);
		td = ESimdFadeARGB32_mmx(fv,ts,td);
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
		ts = _mm_adds_pu8(c8, ts);
		td = ESimdFadeARGB32_mmx(fv,ts,td);
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
void ESimdAddBlendAreaA8_RGBA32_mmx(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, EPixel_t color,
				     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
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
	    ESimdVectorU32_t ts = ESimdVectorSet8(0,0,0,src1[0],
						  0,0,0,src1[1]);
	    ts = _mm_adds_pu8(c8, ts);
	    td = ESimdFadeRGBA32_mmx(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += 2;
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


void ESimdAddBlendAreaA8_ARGB32_mmx(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, EPixel_t color,
				     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
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
	    ESimdVectorU32_t ts = ESimdVectorSet8(src1[0],0,0,0,
						  src1[1],0,0,0);
	    ts = _mm_adds_pu8(c8, ts);
	    td = ESimdFadeARGB32_mmx(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += 2;
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



void ESimdAlphaAreaARGB32_mmx(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t a, unsigned int width,
			      unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    ESimdVectorI8_t a8 = ESimdVectorSetPIXEL(0,a,a,a);

    if (soffs != doffs) {
	int n = EMinInt((doffs/4), width);

	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    EDirectAlphaRowARGB32(src1,dst1,a,n);
	    src1 += doffs;
	    dst1 += doffs;
	    width1 -= n;
		    
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_mmx(a8,ts,td);
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
	int n = EMinInt((soffs/4), width);
	
	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;
	    EDirectAlphaRowARGB32(src1,dst1,a,n);
	    src1 += soffs;
	    dst1 += soffs;
	    width1 -= n;
		
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_mmx(a8,ts,td);
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
    ESimdEmptyState();
}

void ESimdAlphaAreaRGBA32_mmx(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t a, unsigned int width,
			      unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    ESimdVectorI8_t a8 = ESimdVectorSetPIXEL(a,a,a,0);

    if (soffs != doffs) {
	int n = EMinInt((doffs/4), width);

	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;

	    EDirectAlphaRowRGBA32(src1,dst1,a,n);
	    src1 += doffs;
	    dst1 += doffs;
	    width1 -= n;
		    
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU32_t ts = ESimdVectorLoadUA32(src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_mmx(a8,ts,td);
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
	int n = EMinInt((soffs/4), width);
	
	while(height--) {
	    unsigned int width1 = width;
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;
	    EDirectAlphaRowRGBA32(src1,dst1,a,n);
	    src1 += soffs;
	    dst1 += soffs;
	    width1 -= n;
		
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_mmx(a8,ts,td);
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
    ESimdEmptyState();
}


void ESimdBlendAreaRGBA32_mmx(u_int8_t* src, int src_wb, 
			      u_int8_t* dst, int dst_wb,
			      unsigned int width, unsigned int height)
{
    int walign = 0;
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);

    if (soffs != doffs) { // UNALIGNABLE align dst
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height--) {
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
		td = ESimdBlendRGBA32_mmx(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowRGBA32(src1, dst1, width1);
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
		EDirectBlendRowRGBA32(src1, dst1, walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdBlendRGBA32_mmx(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowRGBA32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    ESimdEmptyState();
}

void ESimdBlendAreaARGB32_mmx(u_int8_t* src, int src_wb, 
			      u_int8_t* dst, int dst_wb,
			      unsigned int width, unsigned int height)
{
    int walign = 0;
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	while(height--) {
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
		td = ESimdBlendARGB32_mmx(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowARGB32(src1, dst1, width1);
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
		EDirectBlendRowARGB32(src1, dst1, walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdBlendARGB32_mmx(ts, td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectBlendRowARGB32(src1, dst1, width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    ESimdEmptyState();
}


void ESimdFadeAreaRGBA32_mmx(u_int8_t* src, int src_wb,
			     u_int8_t* dst, int dst_wb,
			     u_int8_t af, 
			     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);

    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height--) {
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
		td = ESimdFadeRGBA32_mmx(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowRGBA32(src1,dst1,af,width1);
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
		EDirectFadeRowRGBA32(src1,dst1,af,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdFadeRGBA32_mmx(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowRGBA32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    ESimdEmptyState();
}

void ESimdFadeAreaARGB32_mmx(u_int8_t* src, int src_wb,
			     u_int8_t* dst, int dst_wb,
			     u_int8_t af, 
			     unsigned int width, unsigned int height)
{
    unsigned int doffs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);
    unsigned int soffs = EPIC_ALIGN_OFFS(src,ESimdVectorAlign);
    int walign = 0;
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);


    if (soffs != doffs) {
	if (doffs != 0)
	    walign = EMinInt((doffs/4), width);
	
	while(height--) {
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
		td = ESimdFadeARGB32_mmx(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowARGB32(src1,dst1,af,width1);
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
		EDirectFadeRowARGB32(src1,dst1,af,walign);
		src1 += soffs;
		dst1 += soffs;
		width1 -= walign;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdFadeARGB32_mmx(fv,ts,td);
		*((ESimdVectorU8_t*)dst1) = td;
		src1 += ESimdVectorSize;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFadeRowARGB32(src1,dst1,af,width1);
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    ESimdEmptyState();
}



/* DO  RGB or BGR (BGR is done by swapping r and b in p) */
void ESimdFillAreaBlendRGB24_mmx(u_int8_t* dst,int dst_wb,
				 unsigned int width, unsigned int height, 
				 EPixel_t p)
{
    /* Maybe use one rotating register ? instead of 3 ... */
    ESimdVectorI8_t s8_0 = ESimdVectorSet8(p.r,p.g,p.b,p.r,p.g,p.b,p.r,p.g);
    ESimdVectorI8_t s8_1 = ESimdVectorSet8(p.g,p.b,p.r,p.g,p.b,p.r,p.g,p.b);
    ESimdVectorI8_t s8_2 = ESimdVectorSet8(p.b,p.r,p.g,p.b,p.r,p.g,p.b,p.r);
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
	    case 0: td = ESimdAlpha32_mmx(a8,s8_0,td); break;
	    case 1: td = ESimdAlpha32_mmx(a8,s8_1,td); break;
	    case 2: td = ESimdAlpha32_mmx(a8,s8_2,td); break;
	    default: break;
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
	    default: break;
	    }
	    dst1++;
	    offs1 = (offs1+1) % 3;
	}
	dst += dst_wb;
    }
    ESimdEmptyState();
}


void ESimdFillAreaBlendARGB32_mmx(u_int8_t* dst,int dst_wb,
				  unsigned int width, unsigned int height, 
				  EPixel_t p)
{
    ESimdVectorI8_t s8, a8;
    unsigned int offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);

    s8 = ESimdVectorSetPIXEL(0,p.r,p.g,p.b);
    a8 = ESimdVectorSplat8(p.a);

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;

	if (width1 < 4)
	    EDirectFillRowBlendARGB32(dst1,width1,p.a,p.r,p.g,p.b);
	else {
	    if (offs) {
		int n = EMinInt((offs/4), width1);
		EDirectFillRowBlendARGB32(dst1,n,p.a,p.r,p.g,p.b);
		dst1 += offs;
		width1 -= n;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_mmx(a8,s8,td);
		*((ESimdVectorU8_t*)dst1) = td;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFillRowBlendARGB32(dst1,width1,p.a,p.r,p.g,p.b);
	}
	dst += dst_wb;
    }
    ESimdEmptyState();
}

void ESimdFillAreaBlendRGBA32_mmx(u_int8_t* dst,int dst_wb,
				  unsigned int width, unsigned int height, 
				  EPixel_t p)
{
    ESimdVectorI8_t s8, a8;
    unsigned int offs = EPIC_ALIGN_OFFS(dst,ESimdVectorAlign);

    s8 = ESimdVectorSetPIXEL(p.r,p.g,p.b,0);
    a8 = ESimdVectorSplat8(p.a);

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;
    
	if (width1 < 4)
	    EDirectFillRowBlendRGBA32(dst1,width1,p.a,p.r,p.g,p.b);
	else {
	    if (offs) {
		int n = EMinInt((offs/4), width1);
		EDirectFillRowBlendRGBA32(dst1,n,p.a,p.r,p.g,p.b);
		dst1 += offs;
		width1 -= n;
	    }
	    while(width1 >= ESimdVectorSize/4) {
		ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
		td = ESimdAlpha32_mmx(a8,s8,td);
		*((ESimdVectorU8_t*)dst1) = td;
		dst1 += ESimdVectorSize;
		width1 -= ESimdVectorSize/4;
	    }
	    if (width1)
		EDirectFillRowBlendRGBA32(dst1,width1,p.a,p.r,p.g,p.b);
	    dst += dst_wb;
	}
    }
    ESimdEmptyState();
}
