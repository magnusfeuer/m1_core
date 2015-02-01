/*
 * Acceleration primitives
 *
 * DataTypes:
 *   ESimdVectorI8_t
 *   ESimdVectorU8_t
 *   ESimdVectorI16_t
 *   ESimdVectorU16_t
 *   ESimdVectorI32_t
 *   ESimdVectorU32_t
 *
 * Constants:
 *   ESimdVectorLength
 *   ESimdVectorAlign 
 *
 * Macros:
 *   ESimdVectorSet8(x1,...xi)
 *   ESimdVectorSet16(x1,...Xj)
 *   ESimdVectorSet32(x1,...Xk)
 *   ESimdVectorLoadUA32(ptr)    - load unaligned 
 *   ESimdVectorSplat8(i8)
 *   ESimdVectorSplat16(i16)
 *   ESimdVectorSetPixel(a8,r8,g8,b8)
 *   ESimdEmptyState()
 *   ESimdPrefetch(ptr)
 *
 * Inline Functions:
 *
 *   ESimdVector8_t ESimdAlpha32(ESimdVectorU8_t alpha,
 *                               ESimdVectorU8_t src,
 *                               ESimdVectorU8_t dst);
 *
 *   ESimdVector8_t ESimdBlendARGB32(ESimdVectorU8_t src,
 *                                   ESimdVectorU8_t dst);
 *
 *   ESimdVector8_t ESimdBlendRGBA32(ESimdVectorU8_t src, 
 *                                   ESimdVectorU8_t dst);
 *
 *
 *   ESimdVector8_t ESimdFadeARGB32(ESimdVectorU16_t fade,
 *                                  ESimdVectorU8_t src,
 *                                  ESimdVectorU8_t dst);
 *
 *
 *   ESimdVector8_t ESimdFadeRGBA32(ESimdVectorU16_t fade,
 *                                  ESimdVectorU8_t src,
 *                                  ESimdVectorU8_t dst);
 *
 */

#ifndef __EPIC_SIMD_H__
#define __EPIC_SIMD_H__

#include <sys/types.h>

#if defined(__VEC__) && defined(__ALTIVEC__)

extern void ESimdCopy_altivec(const u_int8_t* src, u_int8_t* dst, size_t n);
extern void ESimdFill32_altivec(u_int8_t* dst, u_int32_t v, size_t n);

extern void ESimdAddBlendAreaRGBA32_altivec(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, EPixel_t color,
					unsigned int width, 
					unsigned int height);
extern void ESimdAddBlendAreaARGB32_altivec(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, EPixel_t color,
					unsigned int width, 
					unsigned int height);
extern void ESimdAddBlendAreaA8_RGBA32_altivec(u_int8_t* src, int src_wb, 
					   u_int8_t* dst, int dst_wb,
					   u_int8_t af, EPixel_t color,
					   unsigned int width, 
					   unsigned int height);
extern void ESimdAddBlendAreaA8_ARGB32_altivec(u_int8_t* src, int src_wb,
					   u_int8_t* dst, int dst_wb,
					   u_int8_t af, EPixel_t color,
					   unsigned int width, 
					   unsigned int height);

extern void ESimdAlphaAreaARGB32_altivec(u_int8_t* src, int src_wb,
				      u_int8_t* dst, int dst_wb,
				      u_int8_t a, unsigned int width,
				      unsigned int height);
extern void ESimdAlphaAreaRGBA32_altivec(u_int8_t* src, int src_wb,
				      u_int8_t* dst, int dst_wb,
				      u_int8_t a, unsigned int width,
				      unsigned int height);
extern void ESimdBlendAreaRGBA32_altivec(u_int8_t* src, int src_wb, 
				      u_int8_t* dst, int dst_wb,
				      unsigned int width, unsigned int height);
extern void ESimdBlendAreaARGB32_altivec(u_int8_t* src, int src_wb, 
				      u_int8_t* dst, int dst_wb,
				      unsigned int width, unsigned int height);
extern void ESimdFadeAreaRGBA32_altivec(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, 
				     unsigned int width, unsigned int height);
extern void ESimdFadeAreaARGB32_altivec(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, 
					unsigned int width, unsigned int height);
extern void ESimdFillAreaBlendRGB24_altivec(u_int8_t* dst,int dst_wb,
				      unsigned int width, unsigned int height, 
				      EPixel_t p);

extern void ESimdFillAreaBlendARGB32_altivec(u_int8_t* dst,int dst_wb,
					  unsigned int width, 
					  unsigned int height, 
					  EPixel_t p);
extern void ESimdFillAreaBlendRGBA32_altivec(u_int8_t* dst,int dst_wb,
					  unsigned int width, 
					 unsigned int height, 
					 EPixel_t p);
#endif

#if defined(__MMX_) || defined(__SSE2__)

static inline void ESimdCopy_x86(const u_int8_t* src, u_int8_t* dst, size_t n)
{
    register unsigned long int dummy;
    __asm__ __volatile__(
        "cld; rep; movsb"				
        :"=&D"(dst), "=&S"(src), "=&c"(dummy)
        :"0" (dst), "1" (src),"2" (n)
        : "memory");
}

static inline void ESimdFill8_x86(const u_int8_t* dst, u_int8_t v, size_t n)
{
    int d0, d1;
    __asm__ __volatile__(
        "cld; rep; stosb"
	: "=&c"(d0), "=&D"(d1)
	: "a" (v), "1" (dst), "0" (n)
        : "memory");
}

static inline void ESimdFill32_x86(u_int32_t* dst, u_int32_t v, size_t n)
{
    int d0, d1;
    __asm__ __volatile__(
        "cld; rep; stosl"
	: "=&c"(d0), "=&D"(d1)
	: "a" (v), "1" (dst), "0" (n)
        : "memory");
}

#endif


#if defined(__MMX__) && defined(__USE_MMX__)

extern void ESimdCopy_mmx(const u_int8_t* src, u_int8_t* dst, size_t n);
extern void ESimdFill32_mmx(u_int8_t* dst, u_int32_t v, size_t n);

extern void ESimdAddBlendAreaRGBA32_mmx(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, EPixel_t color,
					unsigned int width, 
					unsigned int height);
extern void ESimdAddBlendAreaARGB32_mmx(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, EPixel_t color,
					unsigned int width, 
					unsigned int height);
extern void ESimdAddBlendAreaA8_RGBA32_mmx(u_int8_t* src, int src_wb, 
					   u_int8_t* dst, int dst_wb,
					   u_int8_t af, EPixel_t color,
					   unsigned int width, 
					   unsigned int height);
extern void ESimdAddBlendAreaA8_ARGB32_mmx(u_int8_t* src, int src_wb,
					   u_int8_t* dst, int dst_wb,
					   u_int8_t af, EPixel_t color,
					   unsigned int width, 
					   unsigned int height);

extern void ESimdAlphaAreaARGB32_mmx(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t a, unsigned int width,
				     unsigned int height);
extern void ESimdAlphaAreaRGBA32_mmx(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t a, unsigned int width,
				     unsigned int height);
extern void ESimdBlendAreaRGBA32_mmx(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     unsigned int width, unsigned int height);
extern void ESimdBlendAreaARGB32_mmx(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     unsigned int width, unsigned int height);
extern void ESimdFadeAreaRGBA32_mmx(u_int8_t* src, int src_wb,
				    u_int8_t* dst, int dst_wb,
				    u_int8_t af, 
				    unsigned int width, unsigned int height);
extern void ESimdFadeAreaARGB32_mmx(u_int8_t* src, int src_wb,
				    u_int8_t* dst, int dst_wb,
				    u_int8_t af, 
				    unsigned int width, unsigned int height);
extern void ESimdFillAreaBlendRGB24_mmx(u_int8_t* dst,int dst_wb,
				     unsigned int width, unsigned int height, 
				     EPixel_t p);

extern void ESimdFillAreaBlendARGB32_mmx(u_int8_t* dst,int dst_wb,
					 unsigned int width, 
					 unsigned int height, 
					 EPixel_t p);
extern void ESimdFillAreaBlendRGBA32_mmx(u_int8_t* dst,int dst_wb,
					 unsigned int width, 
					 unsigned int height, 
					 EPixel_t p);
#endif


#if defined(__SSE2__) && defined(USE_SSE2)

extern void ESimdCopy_sse2(const u_int8_t* src, u_int8_t* dst, size_t n);
extern void ESimdFill32_sse2(u_int8_t* dst, u_int32_t v, size_t n);

extern void ESimdAddBlendAreaRGBA32_sse2(u_int8_t* src, int src_wb,
					 u_int8_t* dst, int dst_wb,
					 u_int8_t af, EPixel_t color,
					 unsigned int width, 
					 unsigned int height);
extern void ESimdAddBlendAreaARGB32_sse2(u_int8_t* src, int src_wb,
					 u_int8_t* dst, int dst_wb,
					 u_int8_t af, EPixel_t color,
					 unsigned int width, 
					 unsigned int height);
extern void ESimdAddBlendAreaA8_RGBA32_sse2(u_int8_t* src, int src_wb, 
					    u_int8_t* dst, int dst_wb,
					    u_int8_t af, EPixel_t color,
					    unsigned int width, 
					    unsigned int height);
extern void ESimdAddBlendAreaA8_ARGB32_sse2(u_int8_t* src, int src_wb,
					    u_int8_t* dst, int dst_wb,
					    u_int8_t af, EPixel_t color,
					    unsigned int width, 
					    unsigned int height);


extern void ESimdAlphaAreaARGB32_sse2(u_int8_t* src, int src_wb,
				      u_int8_t* dst, int dst_wb,
				      u_int8_t a, unsigned int width,
				      unsigned int height);
extern void ESimdAlphaAreaRGBA32_sse2(u_int8_t* src, int src_wb,
				      u_int8_t* dst, int dst_wb,
				      u_int8_t a, unsigned int width,
				      unsigned int height);
extern void ESimdBlendAreaRGBA32_sse2(u_int8_t* src, int src_wb, 
				      u_int8_t* dst, int dst_wb,
				      unsigned int width, unsigned int height);
extern void ESimdBlendAreaARGB32_sse2(u_int8_t* src, int src_wb, 
				      u_int8_t* dst, int dst_wb,
				      unsigned int width, unsigned int height);
extern void ESimdFadeAreaRGBA32_sse2(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, 
				     unsigned int width, unsigned int height);
extern void ESimdFadeAreaARGB32_sse2(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t af, 
				     unsigned int width, unsigned int height);
extern void ESimdFillAreaBlendRGB24_sse2(u_int8_t* dst,int dst_wb,
					 unsigned int width, unsigned int height, 
					 EPixel_t p);

extern void ESimdFillAreaBlendARGB32_sse2(u_int8_t* dst,int dst_wb,
					  unsigned int width, 
					  unsigned int height, 
					  EPixel_t p);
extern void ESimdFillAreaBlendRGBA32_sse2(u_int8_t* dst,int dst_wb,
					  unsigned int width, 
					 unsigned int height, 
					 EPixel_t p);
#endif

/* EMULATION code */
extern void ESimdCopy_emu(const u_int8_t* src, u_int8_t* dst, size_t n);
extern void ESimdFill32_emu(u_int8_t* dst, u_int32_t v, size_t n);

extern void ESimdAddBlendAreaRGBA32_emu(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, EPixel_t color,
					unsigned int width, 
					unsigned int height);
extern void ESimdAddBlendAreaARGB32_emu(u_int8_t* src, int src_wb,
					u_int8_t* dst, int dst_wb,
					u_int8_t af, EPixel_t color,
					unsigned int width, 
					unsigned int height);
extern void ESimdAddBlendAreaA8_RGBA32_emu(u_int8_t* src, int src_wb, 
					   u_int8_t* dst, int dst_wb,
					   u_int8_t af, EPixel_t color,
					   unsigned int width, 
					   unsigned int height);
extern void ESimdAddBlendAreaA8_ARGB32_emu(u_int8_t* src, int src_wb,
					   u_int8_t* dst, int dst_wb,
					   u_int8_t af, EPixel_t color,
					   unsigned int width, 
					   unsigned int height);

extern void ESimdAlphaAreaARGB32_emu(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t a, unsigned int width,
				     unsigned int height);
extern void ESimdAlphaAreaRGBA32_emu(u_int8_t* src, int src_wb,
				     u_int8_t* dst, int dst_wb,
				     u_int8_t a, unsigned int width,
				     unsigned int height);
extern void ESimdBlendAreaRGBA32_emu(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     unsigned int width, unsigned int height);
extern void ESimdBlendAreaARGB32_emu(u_int8_t* src, int src_wb, 
				     u_int8_t* dst, int dst_wb,
				     unsigned int width, unsigned int height);
extern void ESimdFadeAreaRGBA32_emu(u_int8_t* src, int src_wb,
				    u_int8_t* dst, int dst_wb,
				    u_int8_t af, 
				    unsigned int width, unsigned int height);
extern void ESimdFadeAreaARGB32_emu(u_int8_t* src, int src_wb,
				    u_int8_t* dst, int dst_wb,
				    u_int8_t af, 
				    unsigned int width, unsigned int height);
extern void ESimdFillAreaBlendRGB24_emu(u_int8_t* dst,int dst_wb,
					unsigned int width, unsigned int height, 
					EPixel_t p);

extern void ESimdFillAreaBlendARGB32_emu(u_int8_t* dst,int dst_wb,
					 unsigned int width, 
					 unsigned int height, 
					 EPixel_t p);
extern void ESimdFillAreaBlendRGBA32_emu(u_int8_t* dst,int dst_wb,
					 unsigned int width, 
					 unsigned int height, 
					 EPixel_t p);

/*
 * Helper functions for align operations
 */

static inline void EDirectAlphaRowARGB32(u_int8_t* src, u_int8_t* dst, 
					 u_int8_t a, int width)
{
    while(width--) {
	// dst[0]=src[0]; // eblend(a,src[0],dst[0]);
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	dst[3]=eblend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

static inline void EDirectAlphaRowRGBA32(u_int8_t* src, u_int8_t* dst, 
					 u_int8_t a, int width)
{
    while(width--) {
	dst[0]=eblend(a,src[0],dst[0]);
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	// dst[3]=src[3]; // eblend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

/* Formats: R8G8B8 / B8G8R8 */
static inline void EDirectAlphaRowRGB24(u_int8_t* src, u_int8_t* dst, 
					u_int8_t a, int width)
{
    while(width--) {
	dst[0]=eblend(a,src[0],dst[0]);
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	src += 3;
	dst += 3;
    }
}

/* Formats: ARGB/ABGR */
static inline void EDirectFadeRowARGB32(u_int8_t* src,u_int8_t* dst,
					u_int8_t af, int width)
{
    while(width--) {
	u_int8_t a = (src[0]*af >> 8);
	dst[0]=eblend(a,src[0],dst[0]); // NEW
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	dst[3]=eblend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

/* fade RGBA/BGRA */
static inline void EDirectFadeRowRGBA32(u_int8_t* src,u_int8_t* dst,
					u_int8_t af, int width)
{
    while(width--) {
	u_int8_t a = (src[3]*af >> 8);
	dst[0]=eblend(a,src[0],dst[0]);
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	src += 4;
	dst += 4;
    }
}

/* blend ARGB/ABGR  (dst alpha is untouched) */
static inline void EDirectBlendRowARGB32(u_int8_t* src,u_int8_t* dst,int width)
{
    while(width--) {
	u_int8_t a = src[0];
	dst[0]=eblend(a,src[0],dst[0]); // NEW
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	dst[3]=eblend(a,src[3],dst[3]);
	src += 4;
	dst += 4;
    }
}

/* blend RGBA/BGRA (dst alpha is untouched) */
static inline void EDirectBlendRowRGBA32(u_int8_t* src,u_int8_t* dst,int width)
{
    while(width--) {
	u_int8_t a = src[3];
	dst[0]=eblend(a,src[0],dst[0]);
	dst[1]=eblend(a,src[1],dst[1]);
	dst[2]=eblend(a,src[2],dst[2]);
	dst[3]=eblend(a,src[3],dst[3]); // NEW
	src += 4;
	dst += 4;
    }
}

static inline void EDirectSumRow8(u_int8_t* src,u_int8_t* dst,int width)
{
    while(width--) {
	dst[0] = esum(src[0],dst[0]);
	src++;
	dst++;
    }
}


static inline void EDirectFillRowBlendARGB32(u_int8_t* dst, int width,
					     u_int8_t a,
					     u_int8_t r,u_int8_t g, u_int8_t b)
{
    while(width--) {
	dst[0]=eblend(a,a,dst[0]);  // NEW
	dst[1]=eblend(a,r,dst[1]);
	dst[2]=eblend(a,g,dst[2]);
	dst[3]=eblend(a,b,dst[3]);
	dst += 4;
    }
}

/* blend dst with RGBA/BGRA color (dst alpha is untouched) 
 * caller MUST swap B/R for BGRA format
 */
static inline void EDirectFillRowBlendRGBA32(u_int8_t* dst, int width,
					     u_int8_t a,
					     u_int8_t r, u_int8_t g, u_int8_t b)
{
    while(width--) {
	dst[0]=eblend(a,r,dst[0]);
	dst[1]=eblend(a,g,dst[1]);
	dst[2]=eblend(a,b,dst[2]);
	dst[3]=eblend(a,a,dst[3]);  // NEW
	dst += 4;
    }
}

/* blend dst with RGB/BGR color (dst alpha is untouched) */
static inline void EDirectFillRowBlendRGB24(u_int8_t* dst, int width,
					    u_int8_t a,
					    u_int8_t r, u_int8_t g, u_int8_t b)
{
    while(width--) {
	dst[0]=eblend(a,r,dst[0]);
	dst[1]=eblend(a,g,dst[1]);
	dst[2]=eblend(a,b,dst[2]);
	dst += 3;
    }
}

/* Add color & blend RGBA/BGRA (if color is swapped correct) */
static inline void EDirectAddBlendRowRGBA32(u_int8_t* src, u_int8_t* dst,
					    u_int8_t af, EPixel_t color,
					    unsigned int width)
{
    while(width--) {
	u_int8_t a,r,g,b;
	r = esum(src[0], color.r);
	g = esum(src[1], color.g);
	b = esum(src[2], color.b);
	a = esum(src[3], color.a);
	a = ((a * af) >> 8);
	dst[0]=eblend(a,r,dst[0]);
	dst[1]=eblend(a,g,dst[1]);
	dst[2]=eblend(a,b,dst[2]);
	dst[3]=eblend(a,a,dst[3]);
	src += 4;
	dst += 4;
    }
}

/* Add color & blend RGBA/BGRA when source is A8 */
static inline void EDirectAddBlendRowA8_RGBA32(u_int8_t* src, u_int8_t* dst,
					       u_int8_t af, EPixel_t color,
					       unsigned int width)
{
    while(width--) {
	u_int8_t a;
	a = esum(src[0], color.a);
	a = ((a * af) >> 8);
	dst[0]=eblend(a,color.r,dst[0]);
	dst[1]=eblend(a,color.g,dst[1]);
	dst[2]=eblend(a,color.b,dst[2]);
	dst[3]=eblend(a,a,dst[3]);
	src += 1;
	dst += 4;
    }
}


/* Add color & blend ARGB/ABGR (if color is swapped correct) */
static inline void EDirectAddBlendRowARGB32(u_int8_t* src, u_int8_t* dst,
					    u_int8_t af, EPixel_t color,
					    unsigned int width)
{
    while(width--) {
	u_int8_t a,r,g,b;
	a = esum(src[0], color.a);
	r = esum(src[1], color.r);
	g = esum(src[2], color.g);
	b = esum(src[3], color.b);
	a = ((a * af) >> 8);
	dst[0]=eblend(a,a,dst[0]);
	dst[1]=eblend(a,r,dst[1]);
	dst[2]=eblend(a,g,dst[2]);
	dst[3]=eblend(a,b,dst[3]);
	src += 4;
	dst += 4;
    }
}

/* Add color & blend ARGB/ABGR when source is A8 */
static inline void EDirectAddBlendRowA8_ARGB32(u_int8_t* src, u_int8_t* dst,
					       u_int8_t af, EPixel_t color,
					       unsigned int width)
{
    while(width--) {
	u_int8_t a;
	a = esum(src[0], color.a);
	a = ((a * af) >> 8);
	dst[0]=eblend(a,a,dst[0]);
	dst[1]=eblend(a,color.r,dst[1]);
	dst[2]=eblend(a,color.g,dst[2]);
	dst[3]=eblend(a,color.b,dst[3]);
	src += 1;
	dst += 4;
    }
}


#endif
