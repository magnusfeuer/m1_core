/*
 * SIMD/EMU functions
 */
#ifndef __EPIC_SIMD_EMU__
#define __EPIC_SIMD_EMU__

#include <sys/types.h>

#define ESimdVectorSize         4  /* use int32 */
#define ESimdVectorAlign        4
#define ESimdVectorPixelsARGB32 1  /* number of A8R8G8B8 pixels per vector */
#define ESimdVectorPixelsARGB16 2  /* number of R5G6B5 pixels per vector */
#define ESimdVectorPixelsARGB15 2  /* number of A1R5G5B5 pixels per vector */

typedef u_int32_t ESimdVectorI8_t;
typedef u_int32_t ESimdVectorU8_t;
typedef u_int32_t ESimdVectorI16_t;
typedef u_int32_t ESimdVectorU16_t;
typedef u_int32_t ESimdVectorI32_t;
typedef u_int32_t ESimdVectorU32_t;

#if BYTE_ORDER == BIG_ENDIAN
#define S3 24
#define S2 16
#define S1 8
#define S0 0
#define W1 16
#define W0 0
#else
#define S3 0
#define S2 8
#define S1 16
#define S0 24
#define W1 0
#define W0 16
#endif

#define ESimdVectorSet8(x3,x2,x1,x0) \
    (((x3) << S3) | ((x2)<<S2) | ((x1)<<S1) | ((x0)<<S0))

#define ESimdVectorSet16(y1,y2) \
    (((y2) << W0) | ((y1) << W1))

#define ESimdVectorSet32(y1) \
    ((y1))

#define ESimdVectorLoadUA32(ptr) \
    ESimdVectorSet32(((u_int32_t*)(ptr))[0])

#define ESimdVectorSplat8(v) \
    ESimdVectorSet8((v),(v),(v),(v))

#define ESimdVectorSplat16(v) \
    ESimdVectorSet16((v),(v))

#define ESimdVectorSetPIXEL(a,r,g,b) \
    ESimdVectorSet8((a),(r),(g),(b))

#define ESimdEmptyState()
#define ESimdPrefetch(p)

// Adjust alpha [0-255] -> [0-256] by adding the high bit
#define AADJUST(a) (a) // ((a) + ((a)>>7))

// 32 bit data interpretation
//  X = x3*256^3 + x2*256^2 + x1*256^1 + x0*256^0
#define B3(x) (((x) >> S3) & 0xff)
#define B2(x) (((x) >> S2) & 0xff)
#define B1(x) (((x) >> S1) & 0xff)
#define B0(x) (((x) >> S0) & 0xff)

static inline ESimdVectorU8_t ESimdAddU8_emu(ESimdVectorU8_t a, 
					     ESimdVectorU8_t b)
{
    u_int16_t s;
    ESimdVectorU8_t r;

    s = B0(a)+B0(b); if (s>255) s=255;
    r = s << S0;

    s = B1(a)+B1(b); if (s>255) s=255;
    r |= (s << S1);

    s = B2(a)+B2(b); if (s>255) s=255;
    r |= (s << S2);

    s = B3(a)+B3(b); if (s>255) s=255;
    r |= (s << S3);
    return r;
}

/* blend src and dst using a fixed alpha value 
 *  alpha may vary for the components!
 */
static inline ESimdVectorU8_t ESimdAlpha32_emu(ESimdVectorU8_t alpha,
					       ESimdVectorU8_t src,
					       ESimdVectorU8_t dst)
{
    u_int8_t a, s, d;
    ESimdVectorU8_t r;

    a = B0(alpha);
    a = AADJUST(a);
    s = B0(src);
    d = B0(dst);
    r = (eblend(a, s, d) << S0);

    a = B1(alpha);
    a = AADJUST(a);
    s = B1(src);
    d = B1(dst);
    r |= (eblend(a, s, d) << S1);

    a = B2(alpha);
    a = AADJUST(a);
    s = B2(src);
    d = B2(dst);
    r |= (eblend(a, s, d) << S2);

    a = B3(alpha);
    a = AADJUST(a);
    s = B3(src);
    d = B3(dst);
    r |= (eblend(a, s, d) << S3);
    return r;
}

/* blend src and dst using inline alpha value:
 * format: ARGB | ABGR
 */
static inline ESimdVectorU8_t ESimdBlendARGB32_emu(ESimdVectorU8_t src,
						   ESimdVectorU8_t dst)
{
    u_int8_t a, s, d;
    ESimdVectorU8_t r;

    s = a = B3(src);
    a = AADJUST(a);
    d = B3(dst);
    r = (eblend(a, s, d) << S3);

    s = B2(src);
    d = B2(dst);
    r |= (eblend(a, s, d) << S2);

    s = B1(src);
    d = B1(dst);
    r |= (eblend(a, s, d) << S1);

    s = B0(src);
    d = B0(dst);
    r |= (eblend(a, s, d) << S0);
    return r;
}

// fade is a vector of 8 bit fixnum fraction to be multiplied with alpha.
// fade is packed in the low part of 16-bit entities.
static inline ESimdVectorU8_t ESimdFadeARGB32_emu(ESimdVectorU16_t fade,
						  ESimdVectorU8_t src,
						  ESimdVectorU8_t dst)
{
    u_int8_t a, s, d;
    ESimdVectorU8_t r;

    s = B3(src);
    a = (s * (fade >> W1)) >> 8;
    a = AADJUST(a);
    d = B3(dst);
    r = (eblend(a, s, d) << S3);

    s = B2(src);
    d = B2(dst);
    r |= (eblend(a, s, d) << S2);

    s = B1(src);
    d = B1(dst);
    r |= (eblend(a, s, d) << S1);

    s = B0(src);
    d = B0(dst);
    r |= (eblend(a, s, d) << S0);
    return r;
}

static inline ESimdVectorU8_t ESimdBlendRGBA32_emu(ESimdVectorU8_t src,
						   ESimdVectorU8_t dst)
{
    u_int8_t a, s, d;
    ESimdVectorU8_t r;

    a = B0(src);
    a = AADJUST(a);
    s = B3(src);
    d = B3(dst);
    r = (eblend(a, s, d) << S3);

    s = B2(src);
    d = B2(dst);
    r |= (eblend(a, s, d) << S2);

    s = B1(src);
    d = B1(dst);
    r |= (eblend(a, s, d) << S1);

    s = a;
    d = B0(dst);
    r |= (eblend(a, s, d) << S0);
    return r;
}

static inline ESimdVectorU8_t ESimdFadeRGBA32_emu(ESimdVectorU16_t fade,
						  ESimdVectorU8_t src,
						  ESimdVectorU8_t dst)
{
    u_int8_t a, s, d;
    ESimdVectorU8_t r;

    s = B0(src);
    a = (s * (fade >> W1)) >> 8;
    a = AADJUST(a);
    d = B0(dst);    
    r = (eblend(a, s, d) << S0);

    s = B1(src);
    d = B1(dst);
    r |= (eblend(a, s, d) << S1);

    s = B2(src);
    d = B2(dst);
    r |= (eblend(a, s, d) << S2);

    s = B3(src);
    d = B3(dst);
    r |= (eblend(a, s, d) << S3);
    return r;
}

#endif
