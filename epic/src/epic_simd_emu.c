/*
 * Emulator version of SIMD functions
 */
#include "epic.h"
#include "epic_simd.h"
#include "epic_simd_emu.h"

void ESimdCopy_emu(const u_int8_t* src, u_int8_t* dst, size_t n)
{
    memcpy(dst, src, n);
}

void ESimdFill32_emu(u_int8_t* dst, u_int32_t v, size_t n)
{
    switch((unsigned long)dst & 0x3) {
    case 0: {	/* 32 bit aligned */
	u_int32_t* ptr32 = (u_int32_t*) dst;
	while(n--) 
	    *ptr32++ = v;
	break;
    }

    case 2: { /* 16 bit aligned */
	u_int16_t* ptr16 = (u_int16_t*) dst;
	u_int16_t v1 = v >> 16;
	u_int16_t v0 = v & 0xffff;
	while(n--) {
	    *ptr16++ = v1;
	    *ptr16++ = v0;
	}
	break;
    }

    default: { /* 8 bit aligned */
	u_int8_t v3 = v >> 24;
	u_int8_t v2 = (v >> 16) & 0xff;
	u_int8_t v1 = (v >> 8) & 0xff;
	u_int8_t v0 = v & 0xff;
	while(n--) {
	    *dst++ = v3;
	    *dst++ = v2;
	    *dst++ = v1;
	    *dst++ = v0;
	}
	break;
    }

    }
}


void ESimdAddBlendAreaRGBA32_emu(u_int8_t* src, int src_wb,
				  u_int8_t* dst, int dst_wb,
				  u_int8_t af, EPixel_t color,
				  unsigned int width, unsigned int height)
{
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
    ESimdVectorU8_t  c8 = ESimdVectorSetPIXEL(color.r,color.g,color.b,color.a);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    ts = ESimdAddU8_emu(ts,c8);
	    td = ESimdFadeRGBA32_emu(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdAddBlendAreaARGB32_emu(u_int8_t* src, int src_wb,
				  u_int8_t* dst, int dst_wb,
				  u_int8_t af, EPixel_t color,
				  unsigned int width, unsigned int height)
{
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
    ESimdVectorU8_t  c8 = ESimdVectorSetPIXEL(color.a,color.r,color.g,color.b);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    ts = ESimdAddU8_emu(ts,c8);
	    td = ESimdFadeARGB32_emu(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdAddBlendAreaA8_RGBA32_emu(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, EPixel_t color,
				     unsigned int width, unsigned int height)
{
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
    ESimdVectorU8_t  c8 = ESimdVectorSetPIXEL(color.r,color.g,color.b,color.a);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = ESimdVectorSetPIXEL(0,0,0,src1[0]);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    ts = ESimdAddU8_emu(ts,c8);
	    td = ESimdFadeRGBA32_emu(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += 1;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdAddBlendAreaA8_ARGB32_emu(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, EPixel_t color,
				     unsigned int width, unsigned int height)
{
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);
    ESimdVectorU8_t  c8 = ESimdVectorSetPIXEL(color.a,color.r,color.g,color.b);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = ESimdVectorSetPIXEL(src1[0],0,0,0);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    ts = ESimdAddU8_emu(ts,c8);
	    td = ESimdFadeARGB32_emu(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += 1;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdAlphaAreaARGB32_emu(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t a, unsigned int width,
			      unsigned int height)
{
    ESimdVectorI8_t a8 = ESimdVectorSetPIXEL(0,a,a,a);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorPixelsARGB32) {
	    ESimdVectorU32_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t  td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdAlpha32_emu(a8,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorPixelsARGB32;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdAlphaAreaRGBA32_emu(u_int8_t* src, int src_wb,
			      u_int8_t* dst, int dst_wb,
			      u_int8_t a, unsigned int width,
			      unsigned int height)
{
    ESimdVectorI8_t a8 = ESimdVectorSetPIXEL(a,a,a,0);
	
    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorPixelsARGB32) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdAlpha32_emu(a8,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorPixelsARGB32;
	}
	src += src_wb;
	dst += dst_wb;
    }
}


void ESimdBlendAreaRGBA32_emu(u_int8_t* src, int src_wb, 
			      u_int8_t* dst, int dst_wb,
			      unsigned int width, unsigned int height)
{
    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdBlendRGBA32_emu(ts, td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdBlendAreaARGB32_emu(u_int8_t* src, int src_wb, 
			      u_int8_t* dst, int dst_wb,
			      unsigned int width, unsigned int height)
{
    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdBlendARGB32_emu(ts, td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}


void ESimdFadeAreaRGBA32_emu(u_int8_t* src, int src_wb,
			     u_int8_t* dst, int dst_wb,
			     u_int8_t af, 
			     unsigned int width, unsigned int height)
{
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdFadeRGBA32_emu(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void ESimdFadeAreaARGB32_emu(u_int8_t* src, int src_wb,
			     u_int8_t* dst, int dst_wb,
			     u_int8_t af, 
			     unsigned int width, unsigned int height)
{
    ESimdVectorI16_t fv = ESimdVectorSplat16(af);

    while(height--) {
	unsigned int width1 = width;
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t ts = *((ESimdVectorU8_t*)src1);
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdFadeARGB32_emu(fv,ts,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    src1 += ESimdVectorSize;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	src += src_wb;
	dst += dst_wb;
    }
}



/* DO  RGB or BGR (BGR is done by swapping r and b in p) */
void ESimdFillAreaBlendRGB24_emu(u_int8_t* dst,int dst_wb,
				 unsigned int width, unsigned int height, 
				 EPixel_t p)
{
    /* Maybe use one rotating register ? instead of 3 ... */
    ESimdVectorI8_t s8_0 = ESimdVectorSet8(p.r,p.g,p.b,p.r);
    ESimdVectorI8_t s8_1 = ESimdVectorSet8(p.g,p.b,p.r,p.g);
    ESimdVectorI8_t s8_2 = ESimdVectorSet8(p.b,p.r,p.g,p.b);
    ESimdVectorI8_t a8 = ESimdVectorSplat8(p.a);
    unsigned int offs = EPIC_ALIGN_OFFS(dst, ESimdVectorAlign);
    unsigned int wb = width*3; // Number of bytes

    while(height--) {
	u_int8_t* dst1      = dst;
	unsigned int offs1  = offs;
	unsigned int wb1    = wb;
	
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
	    case 0: td = ESimdAlpha32_emu(a8,s8_0,td); break;
	    case 1: td = ESimdAlpha32_emu(a8,s8_1,td); break;
	    case 2: td = ESimdAlpha32_emu(a8,s8_2,td); break;
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


void ESimdFillAreaBlendARGB32_emu(u_int8_t* dst,int dst_wb,
				  unsigned int width, unsigned int height, 
				  EPixel_t p)
{
    ESimdVectorI8_t s8, a8;
    s8 = ESimdVectorSetPIXEL(0,p.r,p.g,p.b);
    a8 = ESimdVectorSplat8(p.a);

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;
	
	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdAlpha32_emu(a8,s8,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	dst += dst_wb;
    }
}

void ESimdFillAreaBlendRGBA32_emu(u_int8_t* dst,int dst_wb,
				  unsigned int width, unsigned int height, 
				  EPixel_t p)
{
    ESimdVectorI8_t s8, a8;

    s8 = ESimdVectorSetPIXEL(p.r,p.g,p.b,0);
    a8 = ESimdVectorSplat8(p.a);

    while(height--) {
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;

	while(width1 >= ESimdVectorSize/4) {
	    ESimdVectorU8_t td = *((ESimdVectorU8_t*)dst1);
	    td = ESimdAlpha32_emu(a8,s8,td);
	    *((ESimdVectorU8_t*)dst1) = td;
	    dst1 += ESimdVectorSize;
	    width1 -= ESimdVectorSize/4;
	}
	dst += dst_wb;
    }
}

