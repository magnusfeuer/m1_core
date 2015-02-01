/*
 * SIMD privitives using SSE2
 *
 */
#ifndef __EPIC_SIMD_SSE2
#define __EPIC_SIMD_SSE2

#if defined(__SSE2__)
#include <emmintrin.h>

typedef __m128i ESimdVectorI8_t;
typedef __m128i ESimdVectorU8_t;
typedef __m128i ESimdVectorI16_t;
typedef __m128i ESimdVectorU16_t;
typedef __m128i ESimdVectorI32_t;
typedef __m128i ESimdVectorU32_t;

#define ESimdVectorSize  16
#define ESimdVectorAlign 16
#define ESimdVectorPixelsARGB32 4  /* number of A8R8G8B8 pixels per vector */
#define ESimdVectorPixelsARGB16 8  /* number of R5G6B5 pixels per vector */
#define ESimdVectorPixelsARGB15 8  /* number of A1R5G5B5 pixels per vector */


#define ESimdVectorSet8(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16) \
    _mm_setr_epi8((x1),(x2),(x3),(x4),(x5),(x6),(x7),(x8),		\
		 (x9),(x10),(x11),(x12),(x13),(x14),(x15),(x16))

#define ESimdVectorSet16(y1,y2,y3,y4,y5,y6,y7,y8) \
    _mm_setr_epi16((y1),(y2),(y3),(y4),(y5),(y6),(y7),(y8))

#define ESimdVectorSet32(y1,y2,y3,y4) \
    _mm_setr_epi32((y1),(y2),(y3),(y4))

#define ESimdVectorLoadUA32(ptr) _mm_loadu_si128((__m128i const*)(ptr))

#define ESimdVectorSplat8(v) \
    ESimdVectorSet8((v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v),(v))

#define ESimdVectorSplat16(v) \
    ESimdVectorSet16((v),(v),(v),(v),(v),(v),(v),(v))

#define ESimdVectorSetPIXEL(a,r,g,b) \
    ESimdVectorSet8((a),(r),(g),(b),(a),(r),(g),(b), \
		(a),(r),(g),(b),(a),(r),(g),(b))

#define ESimdEmptyState() 
#define ESimdPrefetch(p) \
    _mm_prefetch((void*)(p),_MM_HINT_NTA)


static inline ESimdVectorU8_t __attribute__((__always_inline__)) 
ESimdAlpha32_sse2(ESimdVectorU8_t alpha,
		  ESimdVectorU8_t src,
		  ESimdVectorU8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_unpacklo_epi8(alpha, zero);
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_unpackhi_epi8(alpha, zero);
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packus_epi16(l16, h16);
}


static inline ESimdVectorU8_t __attribute__((__always_inline__)) 
ESimdBlendARGB32_sse2(ESimdVectorU8_t src,
		      ESimdVectorU8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packus_epi16(l16, h16);
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline ESimdVectorU8_t __attribute__((__always_inline__)) 
ESimdFadeARGB32_sse2(ESimdVectorU16_t fade,
		     ESimdVectorU8_t src,
		     ESimdVectorU8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);
    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    // Pack (will not work if l16 or h16 is not in range [0..255]!!!
    return _mm_packus_epi16(l16, h16);
}

// Format: R8G8B8A8 and B8G8R8A8
// src = r1,g1,b1,a1,r2,g2,b2,a2,r3,g3,b3,a3,r4,g4,b4,a4
// dst = R1,G1,B1,A1,R2,G2,B2,A2,R3,G3,B3,A3,R4,G4,B4,A4
//
static inline ESimdVectorU8_t __attribute__((__always_inline__)) 
ESimdBlendRGBA32_sse2(ESimdVectorU8_t src,
		      ESimdVectorU8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);

    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    return _mm_packus_epi16(l16, h16);
}

static inline ESimdVectorU8_t __attribute__((__always_inline__)) 
ESimdFadeRGBA32_sse2(ESimdVectorU16_t fade,
		     ESimdVectorU8_t src,
		     ESimdVectorU8_t dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpacklo_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);

    /* HIGH 64 */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));
    a16 = _mm_mullo_epi16(fade,a16);
    a16 = _mm_srli_epi16(a16, 8);
    d16 = _mm_unpackhi_epi8(dst, zero);
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);
    return _mm_packus_epi16(l16, h16);
}

#endif
#endif
