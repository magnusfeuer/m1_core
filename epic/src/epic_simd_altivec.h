/*
 * SIMD primitives using Altivec
 *
 */
#ifndef __EPIC_SIMD_ALTIVEC__
#define __EPIC_SIMD_ALTIVEC__

#if defined(__VEC__) && defined(__ALTIVEC__)
#include <altivec.h>

typedef vector int8_t    ESimdVectorI8_t;
typedef vector u_int8_t  ESimdVectorU8_t;
typedef vector int16_t   ESimdVectorI16_t;
typedef vector u_int16_t ESimdVectorU16_t;
typedef vector int32_t   ESimdVectorI32_t;
typedef vector u_int32_t ESimdVectorU32_t;

#define ESimdVectorSize  16
#define ESimdVectorAlign 16
#define ESimdVectorPixelsARGB32 4  /* number of A8R8G8B8 pixels per vector */
#define ESimdVectorPixelsARGB16 8  /* number of R5G6B5 pixels per vector */
#define ESimdVectorPixelsARGB15 8  /* number of A1R5G5B5 pixels per vector */


#define ESimdVectorSet8(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16) \
    ((ESimdVectorI8_t)((x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),		\
		   (x9),(x10),(x11),(x12),(x13),(x14),(x15),(x16)))

#define ESimdVectorSet16(y1,y2,y3,y4,y5,y6,y7,y8) \
    ((ESimdVectorI16_t)((y1),(y2),(y3),(y4),(y5),(y6),(y7),(y8)))

#define ESimdVectorSet32(y1,y2,y3,y4) \
    ((ESimdVectorI32_t)((y1),(y2),(y3),(y4)))

#define ESimdVectorLoadUA32(ptr) \
    ESimdVectorSet32(((u_int32_t*)(ptr))[0],	\
		     ((u_int32_t*)(ptr))[1],	\
		     ((u_int32_t*)(ptr))[2],	\
		     ((u_int32_t*)(ptr))[3])

#define ESimdVectorSplat8(v) \
    ESimdVectorSet8((v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v))

#define ESimdVectorSplat16(v) \
    ESimdVectorSet16((v),(v),(v),(v),(v),(v),(v),(v))

#define ESimdVectorSetPIXEL(a,r,g,b) \
    ESimdVectorSet8((a),(r),(g),(b),(a),(r),(g),(b), \
		(a),(r),(g),(b),(a),(r),(g),(b))

#define ESimdEmptyState() 
#define ESimdPrefetch(p) 

static inline ESimdVectorU8_t ESimdAlpha32_altivec(ESimdVectorU8_t alpha,
						   ESimdVectorU8_t src,
						   ESimdVectorU8_t dst)
{
    vector int16_t   d16, s16, a16, t16;
    vector u_int16_t l16, h16;
    vector u_int8_t  zero = vec_xor(src, src);

    /* LOW 64 */
    s16 = vec_mergel(zero, src);
    a16 = vec_mergel(zero, alpha);
    d16 = vec_mergel(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector u_int16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    l16 = vec_sr(t16, ((vector u_int16_t)(8)));

    /* HIGH 64 */
    s16 = vec_mergeh(zero, src);
    a16 = vec_mergeh(zero, alpha);
    d16 = vec_mergeh(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector u_int16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    h16 = vec_sr(t16, ((vector u_int16_t)(8)));
    return (vector u_int8_t) vec_pack(h16, l16);
}

static inline ESimdVectorU8_t ESimdBlendARGB32_altivec(ESimdVectorU8_t src,
						       ESimdVectorU8_t dst)
{
    vector int16_t   d16, s16, a16, t16;
    vector u_int16_t l16, h16;
    vector u_int8_t  zero = vec_xor(src, src);

    /* LOW 64 */
    /* | a1 | r1 | g1 | b1 | a2 | r2 | g2 | b2 | */
    s16 = vec_mergel(zero, src);
    a16 = vec_sr((vector u_int64_t)s16, (vector u_int16_t)(48));
    a16 = vec_sl((vector u_int64_t)a16, (vector u_int16_t)(16));
    a16 = vec_sl((vector u_int64_t)a16, (vector u_int16_t)(32));

    /* | A1 | R1 | G1 | B1 | A2 | R2 | G2 | B2 | */
    d16 = vec_mergel(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector u_int16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    l16 = vec_sr(t16, ((vector u_int16_t)(8)));

    /* HIGH 64 */
    /* | a3 | r3 | g3 | b3 | a4 | r4 | g4 | b4 | */
    s16 = vec_mergeh(zero, src);
    a16 = vec_sr((vector u_int64_t)s16, (vector u_int16_t)(48));
    a16 = vec_sl((vector u_int64_t)a16, (vector u_int16_t)(16));
    a16 = vec_sl((vector u_int64_t)a16, (vector u_int16_t)(32));

    /* | A1 | R1 | G1 | B1 | A2 | R2 | G2 | B2 | */
    d16 = vec_mergeh(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector u_int16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    h16 = vec_sr(t16, ((vector u_int16_t)(8)));

    return (vector u_int8_t) vec_pack(h16, l16);
}

static inline ESimdVectorU8_t ESimdFadeARGB32_sse2(ESimdVectorU16_t fade,
						  ESimdVectorU8_t src,
						  ESimdVectorU8_t dst)
{
    /* FIXME */
}


static inline ESimdVectorU8_t ESimdBlendRGBA32_altivec(ESimdVectorU8_t src,
						       ESimdVectorU8_t dst)
{
    vector int16_t   d16, s16, a16, t16;
    vector u_int16_t l16, h16;
    vector u_int8_t  zero = vec_xor(src, src);

    /* LOW 64 */
    /* | r1 | g1 | b1 | a1 | r2 | g2 | b2 | a2 | */
    s16 = vec_mergel(zero, src);
    a16 = vec_sl((vector u_int64_t)s16, (vector u_int16_t)(48));
    a16 = vec_sr((vector u_int64_t)a16, (vector u_int16_t)(16));
    a16 = vec_sr((vector u_int64_t)a16, (vector u_int16_t)(32));

    /* | R1 | G1 | B1 | A1 | R2 | G2 | B2 | A2 |*/
    d16 = vec_mergel(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector u_int16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    l16 = vec_sr(t16, ((vector u_int16_t)(8)));

    /* HIGH 64 */
    /* | r3 | g3 | b3 | a3 | r4 | g4 | b4 | a4 | */
    s16 = vec_mergeh(zero, src);
    a16 = vec_sl((vector u_int64_t)s16, (vector u_int16_t)(48));
    a16 = vec_sr((vector u_int64_t)a16, (vector u_int16_t)(16));
    a16 = vec_sr((vector u_int64_t)a16, (vector u_int16_t)(32));

    /* | R3 | G3 | B3 | A3 | R4 | G4 | B4 | A4 | */
    d16 = vec_mergeh(zero, dst);
    t16 = vec_sub(s16, d16);
    d16 = vec_sl(d16, ((vector u_int16_t)(8)));
    t16 = vec_mladd(t16, a16, d16);
    h16 = vec_sr(t16, ((vector u_int16_t)(8)));

    return (vector u_int8_t) vec_pack(h16, l16);
}

static inline ESimdVectorU8_t ESimdFadeRGBA32_sse2(ESimdVectorU16_t fade,
						   ESimdVectorU8_t src,
						   ESimdVectorU8_t dst)
{
    /* FIXME */
}

#endif
#endif

