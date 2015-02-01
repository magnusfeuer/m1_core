/*
 *  Test all combinations for blend function
 */
#include <stdio.h>
#include "epic.h"

#include <emmintrin.h>

u_int8_t reference_blend(u_int8_t a, u_int8_t s, u_int8_t d)
{
    return epixel_blend8_1(a, s, d);   // the 256 version
}

#ifdef __SSE2__

// format _m128i as u_int8_t vector (hex)

char* format_epu8(__m128i v, char* buf)
{
    u_int8_t __attribute__ ((aligned (16))) v8[16];
    char* ptr = buf;
    int i, j;

    _mm_store_si128((__m128i*) v8, v);

    *ptr++ = '{';
    for (i = 0; i < 16; i++) {
	j = sprintf(ptr, "%u,", v8[i]);
	ptr += j;
    }
    ptr--;
    *ptr++ = '}';
    *ptr = '\0';
    return buf;
}

char* format_epu16(__m128i v, char* buf)
{
    u_int16_t __attribute__ ((aligned (16))) v16[8];
    char* ptr = buf;
    int i, j;

    _mm_store_si128((__m128i*) v16, v);

    *ptr++ = '{';
    for (i = 0; i < 8; i++) {
	j = sprintf(ptr, "%u,", v16[i]);
	ptr += j;
    }
    ptr--;
    *ptr++ = '}';
    *ptr = '\0';
    return buf;
}

// Format: A8R8G8B8 and A8B8G8R8
// src = a1,r1,g1,b1,a2,r2,g2,b2,a3,r3,g3,b3,a4,r4,g4,b4
// dst = A1,R1,G1,B1,A2,R2,G2,B2,A3,R3,G3,B3,A4,R4,G4,B4
//
__m128i blend_argb_sse_32(__m128i src, __m128i dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    /* | a1 | r1 | g1 | b1 | a2 | r2 | g2 | b2 | */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));

    /* | A1 | R1 | G1 | B1 | A2 | R2 | G2 | B2 | */
    d16 = _mm_unpacklo_epi8(dst, zero);
    /*  (((xi - Xi)*a + (Xi<<8)) >> 8)  */
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);

    /* HIGH 64 */
    /* | a3 | r3 | g3 | b3 | a4 | r4 | g4 | b4 | */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_slli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_srli_epi64(a16, 32));

    /* | A3 | R3 | G3 | B3 | A4 | R4 | G4 | B4 | */
    d16 = _mm_unpackhi_epi8(dst, zero);

    /*  (((xi - Xi)*a + (Xi<<8)) >> 8)  */
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);

    // Pack 
    return _mm_packus_epi16(l16, h16);
}

// Format: R8G8B8A8 and B8G8R8A8
// src = r1,g1,b1,a1,r2,g2,b2,a2,r3,g3,b3,a3,r4,g4,b4,a4
// dst = R1,G1,B1,A1,R2,G2,B2,A2,R3,G3,B3,A3,R4,G4,B4,A4
//

__m128i blend_rgba_sse_32(__m128i src, __m128i dst)
{
    __m128i d16, s16, a16, t16;
    __m128i l16, h16;
    __m128i zero = _mm_xor_si128(src, src);

    /* LOW 64 */
    /* | r1 | g1 | b1 | a1 | r2 | g2 | b2 | a2 | */
    s16 = _mm_unpacklo_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));

    /* | R1 | G1 | B1 | A1 | R2 | G2 | B2 | A2 |*/
    d16 = _mm_unpacklo_epi8(dst, zero);
    /*  (((xi - Xi)*a + (Xi<<8)) >> 8)  */
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    l16 = _mm_srli_epi16(t16, 8);

    /* HIGH 64 */
    /* | r3 | g3 | b3 | a3 | r4 | g4 | b4 | a4 | */
    s16 = _mm_unpackhi_epi8(src, zero);
    a16 = _mm_srli_epi64(s16, 48);
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 16));
    a16 = _mm_or_si128(a16, _mm_slli_epi64(a16, 32));

    /* | R3 | G3 | B3 | A3 | R4 | G4 | B4 | A4 | */
    d16 = _mm_unpackhi_epi8(dst, zero);

    /*  (((xi - Xi)*a + (Xi<<8)) >> 8)  */
    t16 = _mm_sub_epi16(s16, d16);
    d16 = _mm_slli_epi16(d16, 8);
    t16 = _mm_add_epi16(_mm_mullo_epi16(t16,a16), d16);
    h16 = _mm_srli_epi16(t16, 8);

    // Pack 
    return _mm_packus_epi16(l16, h16);
}




u_int8_t blend_2(u_int8_t a, u_int8_t s, u_int8_t d)
{
    /* construct a source and destination vector */

    u_int8_t __attribute__ ((aligned (16))) src[16] =
	{a, s,   s+1, s+2, a+1, s+3,  s+4,   s+5,
	 a+2, s+7, s+8, s+9, a+3, s+10, s+11, s+12 };
    u_int8_t __attribute__ ((aligned (16))) dst[16] =
	{1, d,   d+1, d+2, 2, d+3,  d+4,   d+5,
	 3, d+7, d+8, d+9, 4, d+10, d+11, d+12 };
    u_int8_t  __attribute__ ((aligned (16))) res[16];

    __m128i sv, dv, rv;

    sv = _mm_load_si128((__m128i const *)src);
    dv = _mm_load_si128((__m128i const *)dst);

    rv = blend_argb_sse_32(sv, dv);
    _mm_store_si128((__m128i*) res, rv);
    return res[1];
}

#endif




main()
{
    int a, s, d;
    u_int8_t r0;
    u_int8_t r1;

    /* first a ref test for debugging */
    r0 = reference_blend(15, 100, 200);
    r1 = blend_2(15, 100, 200);
    
    /* then run them all */
    for (a = 0; a < 256; a++) {
	for (s = 0; s < 256; s++) {
	    for (d = 0; d < 256; d++) {
		r0 = reference_blend(a, s, d);
		r1 = blend_2(a, s, d);
		if (r1 != r0) {
		    fprintf(stderr, "blend(s=%d,d=%d,a=%d)  r=%d  [r'=%d]\n", 
			    s, d, a, r0, r1);
		}
	    }
	}
    }
    exit(0);
}
