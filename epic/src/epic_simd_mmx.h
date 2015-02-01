/*
 * SIMD/MMX functions
 */
#ifndef __EPIC_SIMD_MMX__
#define __EPIC_SIMD_MMX__

#if defined(__MMX__)
#include <mmintrin.h>

typedef __m64 ESimdVectorI8_t;
typedef __m64 ESimdVectorU8_t;
typedef __m64 ESimdVectorI16_t;
typedef __m64 ESimdVectorU16_t;
typedef __m64 ESimdVectorI32_t;
typedef __m64 ESimdVectorU32_t;

#define ESimdVectorSize  8
#define ESimdVectorAlign 4         /* data alignment */
#define ESimdVectorPixelsARGB32 2  /* number of A8R8G8B8 pixels per vector */
#define ESimdVectorPixelsARGB16 4  /* number of R5G6B5 pixels per vector */
#define ESimdVectorPixelsARGB15 4  /* number of A1R5G5B5 pixels per vector */

#define ESimdVectorSet8(x1,x2,x3,x4,x5,x6,x7,x8) \
    _mm_setr_pi8((x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8))

#define ESimdVectorSet16(y1,y2,y3,y4) \
    _mm_setr_pi16((y1),(y2),(y3),(y4))

#define ESimdVectorSet32(y1,y2) \
    _mm_setr_pi32((y1),(y2))

#define ESimdVectorLoadUA32(ptr) \
    ESimdVectorSet32(((u_int32_t*)(ptr))[0],((u_int32_t*)(ptr))[1])

#define ESimdVectorSplat8(v) \
    ESimdVectorSet8((v),(v),(v),(v),(v),(v),(v),(v))

#define ESimdVectorSplat16(v) \
    ESimdVectorSet16((v),(v),(v),(v))

#define ESimdVectorSetPIXEL(a,r,g,b) \
    ESimdVectorSet8((a),(r),(g),(b),(a),(r),(g),(b))

#define ESimdEmptyState() _mm_empty()

#define ESimdPrefetch(p)

/* blend src and dst using a fixed alpha value 
 *  alpha is may vary for the components!
 */
static inline ESimdVectorU8_t ESimdAlpha32_mmx(ESimdVectorU8_t alpha,
					       ESimdVectorU8_t src,
					       ESimdVectorU8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_unpacklo_pi8(alpha, zero);
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_unpackhi_pi8(alpha, zero);
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packs_pu16(l16, h16);
}

/* blend src and dst using inline alpha value:
 * format: ARGB | ABGR
 */
static inline ESimdVectorU8_t ESimdBlendARGB32_mmx(ESimdVectorU8_t src,
						   ESimdVectorU8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    /* src = ARGB argb */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packs_pu16(l16, h16);
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline ESimdVectorU8_t ESimdFadeARGB32_mmx(ESimdVectorU16_t fade,
						  ESimdVectorU8_t src,
						  ESimdVectorU8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_slli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_srli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packs_pu16(l16, h16);
}

// Format: R8G8B8A8 and B8G8R8A8
// src = r1,g1,b1,a1,r2,g2,b2,a2,r3,g3,b3,a3,r4,g4,b4,a4
// dst = R1,G1,B1,A1,R2,G2,B2,A2,R3,G3,B3,A3,R4,G4,B4,A4
//

static inline ESimdVectorU8_t ESimdBlendRGBA32_mmx(ESimdVectorU8_t src,
						   ESimdVectorU8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);

    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    return _mm_packs_pu16(l16, h16);
}

static inline ESimdVectorU8_t ESimdFadeRGBA32_mmx(ESimdVectorU16_t fade,
						  ESimdVectorU8_t src,
						  ESimdVectorU8_t dst)
{
    __m64 d16, s16, a16, t16;
    __m64 l16, h16;
    __m64 zero = _mm_setzero_si64();

    /* LOW 64 */
    s16 = _mm_unpacklo_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpacklo_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    l16 = _mm_srli_pi16(t16, 8);

    /* HIGH 64 */
    s16 = _mm_unpackhi_pi8(src, zero);
    a16 = _mm_srli_si64(s16, 48);
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 16));
    a16 = _mm_or_si64(a16, _mm_slli_si64(a16, 32));
    a16 = _mm_mullo_pi16(fade,a16);
    a16 = _mm_srli_pi16(a16, 8);
    d16 = _mm_unpackhi_pi8(dst, zero);
    t16 = _mm_sub_pi16(s16, d16);
    d16 = _mm_slli_pi16(d16, 8);
    t16 = _mm_add_pi16(_mm_mullo_pi16(t16,a16), d16);
    h16 = _mm_srli_pi16(t16, 8);
    return _mm_packs_pu16(l16, h16);
}


#endif
#endif
