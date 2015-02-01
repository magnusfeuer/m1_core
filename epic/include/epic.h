/*
 * EPIC Pixels for all
 *
 */
#ifndef __EPIC_H__
#define  __EPIC_H__

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <memory.h>

#ifdef __cplusplus
extern "C" {
#endif

extern void epicEmitError(char* file, int line, ...);
extern int  epic_debug_mask;

#define EDBG_ALL    0xFFFF
#define EDBG_INFO   0x0001
#define EDBG_WARN   0x0010  // emit warnings
#define EDBG_MEM    0x4000  // detailed memory debug

#if defined(debug) || defined(DEBUG)
#define EDBG_IS_SET(flags) ((epic_debug_mask & (flags)) == (flags))
#define EDBGFMT_mask(mask,...)						\
    do {								\
	if (EDBG_IS_SET((mask)))					\
	    epicEmitError(__FILE__, __LINE__, __VA_ARGS__);		\
    } while(0)
#else
#define EDBG_IS_SET(flags) 0
#define EDBGFMT_mask(mask,...)
#endif

#define EDBGFMT(...)      EDBGFMT_mask(EDBG_INFO,__VA_ARGS__)
#define EDBGFMT_MEM(...)  EDBGFMT_mask(EDBG_MEM,__VA_ARGS__)
#define EDBGFMT_WARN(...) EDBGFMT_mask(EDBG_WARN,__VA_ARGS__)


#ifndef EHANDLE_T
#define EHANDLE_T void*
#endif

#define INVALID_HANDLE ((EHANDLE_T)-1)

/* align pointer (a MUST be a power of 2) (and constant ;-) */
#define EPIC_ALIGN_OFFS(p,a) (((a) - (((unsigned int)p) % (a))) % (a))
#define EPIC_ALIGN(p,a) ((void*)(((u_int8_t*)p)+EPIC_ALIGN_OFFS(p,a)))

#define EPIC_SIMD_AUTO     0x00
#define EPIC_SIMD_EMU      0x01
#define EPIC_SIMD_MMX      0x02
#define EPIC_SIMD_SSE2     0x04
#define EPIC_SIMD_ALTIVEC  0x08

extern void ESimdInit(int accel);
extern void epic_init(int accel);
extern void epic_debug(int mask);

extern int cpuSerialNumber(unsigned char* buf, size_t maxlen);
extern int cpuVendorName(char* buf, size_t maxlen);


/* keyboard modifiers */
#define EKBD_MOD_NONE  		0x0000
#define EKBD_MOD_LSHIFT		0x0001
#define EKBD_MOD_RSHIFT		0x0002
#define EKBD_MOD_LCTRL 		0x0040
#define EKBD_MOD_RCTRL 		0x0080
#define EKBD_MOD_LALT  		0x0100
#define EKBD_MOD_RALT  		0x0200
#define EKBD_MOD_LMETA 		0x0400
#define EKBD_MOD_RMETA 		0x0800
#define EKBD_MOD_NUM   		0x1000
#define EKBD_MOD_CAPS  		0x2000
#define EKBD_MOD_ALTGR 		0x4000
#define EKBD_MOD_SCR		0x8000

#define EKBD_MOD_CTRL	(EKBD_MOD_LCTRL|EKBD_MOD_RCTRL)
#define EKBD_MOD_SHIFT	(EKBD_MOD_LSHIFT|EKBD_MOD_RSHIFT)
#define EKBD_MOD_ALT	(EKBD_MOD_LALT|EKBD_MOD_RALT)
#define EKBD_MOD_META	(EKBD_MOD_LMETA|EKBD_MOD_RMETA)

#define EKBD_KEY_UNKNOWN	0
/* Following special control keysyms are mapped to ASCII*/
#define EKBD_KEY_BACKSPACE	8
#define EKBD_KEY_TAB		9
#define EKBD_KEY_ENTER		13
#define EKBD_KEY_ESCAPE		27
/* Keysyms from 32-126 are mapped to ASCII*/

#define EKBD_KEY_NONASCII_MASK	0xFF00
/* Following keysyms are mapped to private use portion of Unicode-16*/
/* arrows + home/end pad*/
#define EKBD_KEY_FIRST		0xF800

#define EKBD_KEY_LEFT		0xF800
#define EKBD_KEY_RIGHT		0xF801
#define EKBD_KEY_UP		0xF802
#define EKBD_KEY_DOWN		0xF803
#define EKBD_KEY_INSERT		0xF804
#define EKBD_KEY_DELETE		0xF805
#define EKBD_KEY_HOME		0xF806
#define EKBD_KEY_END		0xF807
#define EKBD_KEY_PAGEUP		0xF808
#define EKBD_KEY_PAGEDOWN	0xF809

/* Numeric keypad*/
#define EKBD_KEY_KP0		0xF80A
#define EKBD_KEY_KP1		0xF80B
#define EKBD_KEY_KP2		0xF80C
#define EKBD_KEY_KP3		0xF80D
#define EKBD_KEY_KP4		0xF80E
#define EKBD_KEY_KP5		0xF80F
#define EKBD_KEY_KP6		0xF810
#define EKBD_KEY_KP7		0xF811
#define EKBD_KEY_KP8		0xF812
#define EKBD_KEY_KP9		0xF813
#define EKBD_KEY_KP_PERIOD	0xF814
#define EKBD_KEY_KP_DIVIDE	0xF815
#define EKBD_KEY_KP_MULTIPLY	0xF816
#define EKBD_KEY_KP_MINUS	0xF817
#define EKBD_KEY_KP_PLUS	0xF818
#define EKBD_KEY_KP_ENTER	0xF819
#define EKBD_KEY_KP_EQUALS	0xF81A

/* Function keys */
#define EKBD_KEY_F1		0xF81B
#define EKBD_KEY_F2		0xF81C
#define EKBD_KEY_F3		0xF81D
#define EKBD_KEY_F4		0xF81E
#define EKBD_KEY_F5		0xF81F
#define EKBD_KEY_F6		0xF820
#define EKBD_KEY_F7		0xF821
#define EKBD_KEY_F8		0xF822
#define EKBD_KEY_F9		0xF823
#define EKBD_KEY_F10		0xF824
#define EKBD_KEY_F11		0xF825
#define EKBD_KEY_F12		0xF827

/* Key state modifier keys*/
#define EKBD_KEY_NUMLOCK	0xF828
#define EKBD_KEY_CAPSLOCK	0xF829
#define EKBD_KEY_SCROLLOCK	0xF82A
#define EKBD_KEY_LSHIFT		0xF82B
#define EKBD_KEY_RSHIFT		0xF82C
#define EKBD_KEY_LCTRL		0xF82D
#define EKBD_KEY_RCTRL		0xF82E
#define EKBD_KEY_LALT		0xF82F
#define EKBD_KEY_RALT		0xF830
#define EKBD_KEY_LMETA		0xF831
#define EKBD_KEY_RMETA		0xF832
#define EKBD_KEY_ALTGR		0xF833

/* Misc function keys*/
#define EKBD_KEY_PRINT		0xF834
#define EKBD_KEY_SYSREQ		0xF835
#define EKBD_KEY_PAUSE		0xF836
#define EKBD_KEY_BREAK		0xF837
#define EKBD_KEY_QUIT		0xF838	/* virtual key*/
#define EKBD_KEY_MENU		0xF839	/* virtual key*/
#define EKBD_KEY_REDRAW		0xF83A	/* virtual key*/

/* Mouse button */
#define EBUT_LEFT               0x0001
#define EBUT_MIDDLE             0x0002
#define EBUT_RIGHT              0x0004

/* Handheld function keys*/
/* #define EKBD_KEY_RECORD		0xF840 -- Replaced by HAVi code */
/* #define EKBD_KEY_PLAY		0xF841 -- Replaced by HAVi code */
#define EKBD_KEY_CONTRAST	0xF842
#define EKBD_KEY_BRIGHTNESS	0xF843
#define EKBD_KEY_SELECTUP	0xF844
#define EKBD_KEY_SELECTDOWN	0xF845
#define EKBD_KEY_ACCEPT		0xF846
#define EKBD_KEY_CANCEL		0xF847
#define EKBD_KEY_APP1		0xF848
#define EKBD_KEY_APP2		0xF849
#define EKBD_KEY_APP3           0xF84A
#define EKBD_KEY_APP4           0xF84B
#define EKBD_KEY_SUSPEND        0xF84C
#define EKBD_KEY_END_NORMAL	0xF84D	/* insert additional keys before this*/

#define DABS(a,b) ((a)<(b)) ? ((b)-(a)) : ((a)-(b))


/* R=red, G=green, B=blue, A=alpha, X=ignore, L=luminance (gray level) */

/*
 * Layout
 *       Format:4          (RGB4/RGB5/..)
 *       BGR:1             (Pixels stored in BGR)
 *       Alpha:1           (Use alpha)
 *       AlphaFirst:1      (Or Skip First if Alplha=0)
 *       LittleEndian:1    (Pixels stored in little endian)
 *       BitsPerPixel:6    (+1)
 */
#define EPIXEL_F_Bgr         0x0200
#define EPIXEL_F_Alpha       0x0100
#define EPIXEL_F_AFirst      0x0080
#define EPIXEL_F_Little      0x0040
#define EPIXEL_M_Size        0x003f
#define EPIXEL_M_Fmt         0xf000

#define EPIXEL_FMT_RGB4   0
#define EPIXEL_FMT_RGB5   1
#define EPIXEL_FMT_RGB8   2
#define EPIXEL_FMT_RGB10  3
#define EPIXEL_FMT_RGB12  4
#define EPIXEL_FMT_RGB16  5
#define EPIXEL_FMT_RGB332 6
#define EPIXEL_FMT_RGB232 7
#define EPIXEL_FMT_RGB565 8
#define EPIXEL_FMT_YUV8   9
#define EPIXEL_FMT_ALPHA  10
#define EPIXEL_FMT_GRAY   11
#define EPIXEL_FMT_RED    12
#define EPIXEL_FMT_GREEN  13
#define EPIXEL_FMT_BLUE   14

#define EPIXEL_FMT(Fmt,Bgr,Alpha,AlphaFirst,Little,BitsPerPixel) \
    (((Fmt)<<12)       |						\
     ((Bgr) << 9)      |						\
     ((Alpha)<<8)      |						\
     ((AlphaFirst)<<7) |						\
     ((Little)<<6)     |						\
     ((BitsPerPixel)-1))

#define EPIXEL_BE_FMT(Fmt,Bgr,Alpha,AlphaFirst,BitsPerPixel) \
    EPIXEL_FMT(Fmt,Bgr,Alpha,AlphaFirst,0,BitsPerPixel)

#define EPIXEL_LE_FMT(Fmt,Bgr,Alpha,AlphaFirst,BitsPerPixel) \
    EPIXEL_FMT(Fmt,Bgr,Alpha,AlphaFirst,1,BitsPerPixel)

/* 64 bit */
#define EPIXEL_TYPE_R16G16B16A16 EPIXEL_BE_FMT(EPIXEL_FMT_RGB16,0,1,0,64)
#define EPIXEL_TYPE_R16G16B16X16 EPIXEL_BE_FMT(EPIXEL_FMT_RGB16,0,0,0,64)
#define EPIXEL_TYPE_A16R16G16B16 EPIXEL_BE_FMT(EPIXEL_FMT_RGB16,0,1,1,64)
#define EPIXEL_TYPE_X16R16G16B16 EPIXEL_BE_FMT(EPIXEL_FMT_RGB16,0,0,1,64)

/* 48 bit */
#define EPIXEL_TYPE_R16G16B16 EPIXEL_BE_FMT(EPIXEL_FMT_RGB16,0,0,0,48)

/* 32 bit */
#define EPIXEL_TYPE_R8G8B8A8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,0,1,0,32)
#define EPIXEL_TYPE_R8G8B8X8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,0,0,0,32)
#define EPIXEL_TYPE_A8R8G8B8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,0,1,1,32)
#define EPIXEL_TYPE_X8R8G8B8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,0,0,1,32)

#define EPIXEL_TYPE_B8G8R8A8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,1,1,0,32)
#define EPIXEL_TYPE_B8G8R8X8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,1,0,0,32)
#define EPIXEL_TYPE_A8B8G8R8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,1,1,1,32)
#define EPIXEL_TYPE_X8B8G8R8 EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,1,0,1,32)

#define EPIXEL_TYPE_L16A16   EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,1,0,32)
#define EPIXEL_TYPE_A16L16   EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,1,1,32)

#define EPIXEL_TYPE_A8Y8U8V8 EPIXEL_BE_FMT(EPIXEL_FMT_YUV8,0,1,1,32)
#define EPIXEL_TYPE_X8Y8U8V8 EPIXEL_BE_FMT(EPIXEL_FMT_YUV8,0,0,1,32)

#define EPIXEL_TYPE_A2R10G10B10 EPIXEL_BE_FMT(EPIXEL_FMT_RGB10,0,1,0,32)

/* 24 bit  */
#define EPIXEL_TYPE_R8G8B8   EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,0,0,0,24)
#define EPIXEL_TYPE_B8G8R8   EPIXEL_BE_FMT(EPIXEL_FMT_RGB8,1,0,0,24)
#define EPIXEL_TYPE_Y8U8V8   EPIXEL_BE_FMT(EPIXEL_FMT_YUV8,0,0,0,24)

/* 16 bit  */
#define EPIXEL_TYPE_R5G5B5A1 EPIXEL_BE_FMT(EPIXEL_FMT_RGB5,0,1,0,16)
#define EPIXEL_TYPE_A1R5G5B5 EPIXEL_BE_FMT(EPIXEL_FMT_RGB5,0,1,1,16)
#define EPIXEL_TYPE_R5G5B5X1 EPIXEL_BE_FMT(EPIXEL_FMT_RGB5,0,0,0,16)
#define EPIXEL_TYPE_X1R5G5B5 EPIXEL_BE_FMT(EPIXEL_FMT_RGB5,0,0,1,16)
#define EPIXEL_TYPE_R5G6B5   EPIXEL_BE_FMT(EPIXEL_FMT_RGB565,0,0,0,16)
#define EPIXEL_TYPE_L8A8     EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,1,0,16)
#define EPIXEL_TYPE_A8L8     EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,1,1,16)
#define EPIXEL_TYPE_L16      EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,0,0,16)

/* 8 bit */
#define EPIXEL_TYPE_R2G3B2A1 EPIXEL_BE_FMT(EPIXEL_FMT_RGB232,0,1,0,8)
#define EPIXEL_TYPE_A1R2G3B2 EPIXEL_BE_FMT(EPIXEL_FMT_RGB232,0,1,1,8)
#define EPIXEL_TYPE_R2G3B2X1 EPIXEL_BE_FMT(EPIXEL_FMT_RGB232,0,0,0,8)
#define EPIXEL_TYPE_X1R2G3B2 EPIXEL_BE_FMT(EPIXEL_FMT_RGB232,0,0,1,8)
#define EPIXEL_TYPE_R3G3B2   EPIXEL_BE_FMT(EPIXEL_FMT_RGB332,0,0,0,8)
#define EPIXEL_TYPE_L8       EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,0,0,8)
#define EPIXEL_TYPE_A8       EPIXEL_BE_FMT(EPIXEL_FMT_ALPHA,0,1,0,8)
#define EPIXEL_TYPE_R8       EPIXEL_BE_FMT(EPIXEL_FMT_RED,0,1,0,8)
#define EPIXEL_TYPE_G8       EPIXEL_BE_FMT(EPIXEL_FMT_GREEN,0,1,0,8)
#define EPIXEL_TYPE_B8       EPIXEL_BE_FMT(EPIXEL_FMT_BLUE,0,1,0,8)

#define EPIXEL_TYPE_L4       EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,0,0,4)
#define EPIXEL_TYPE_A4       EPIXEL_BE_FMT(EPIXEL_FMT_ALPHA,0,0,0,4)
#define EPIXEL_TYPE_L2       EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,0,0,2)
#define EPIXEL_TYPE_A2       EPIXEL_BE_FMT(EPIXEL_FMT_ALPHA,0,0,0,2)
#define EPIXEL_TYPE_L1       EPIXEL_BE_FMT(EPIXEL_FMT_GRAY,0,0,0,1)
#define EPIXEL_TYPE_A1       EPIXEL_BE_FMT(EPIXEL_FMT_ALPHA,0,0,0,1)

/* ALIASES */
#define EPIXEL_TYPE_ALPHA  EPIXEL_TYPE_A8
#define EPIXEL_TYPE_232    EPIXEL_TYPE_R2G3B2

#define EPIXEL_TYPE_565_LE EPIXEL_LE_FMT(EPIXEL_FMT_RGB565,0,0,0,16)
#define EPIXEL_TYPE_565_BE EPIXEL_TYPE_R5G6B5

#define EPIXEL_TYPE_RGB    EPIXEL_TYPE_R8G8B8
#define EPIXEL_TYPE_BGR    EPIXEL_TYPE_B8G8R8

#define EPIXEL_TYPE_RGBA   EPIXEL_TYPE_R8G8B8A8
#define EPIXEL_TYPE_ARGB   EPIXEL_TYPE_A8R8G8B8
#define EPIXEL_TYPE_BGRA   EPIXEL_TYPE_B8G8R8A8
#define EPIXEL_TYPE_ABGR   EPIXEL_TYPE_A8B8G8R8

#define EPIXEL_BIT_SIZE(pt)   (((pt)&EPIXEL_M_Size)+1)
#define EPIXEL_SIZE(pt)       (EPIXEL_BIT_SIZE(pt)>>3)
#define EPIXEL_HAS_ALPHA(pt)  (((pt)&EPIXEL_F_Alpha) != 0)

/* The is the basic color stucture.
 * It is optimised to read/write ARGB in that memory order
 *
*/

#if BYTE_ORDER == BIG_ENDIAN
#define PX_A 0
#define PX_R 1
#define PX_G 2
#define PX_B 3
#else
#define PX_B 0
#define PX_G 1
#define PX_R 2
#define PX_A 3
#endif

typedef union {
    u_int8_t v[4];
    u_int32_t px;   /* px = (A<<24)|(R<<16)|(G<<8)|B */
    struct {
#if BYTE_ORDER == BIG_ENDIAN
	u_int8_t a;
	u_int8_t r;
	u_int8_t g;
	u_int8_t b;
#else
	u_int8_t b;
	u_int8_t g;
	u_int8_t r;
	u_int8_t a;
#endif
    };
} EPixel_t;

typedef struct {
    u_int8_t a;
    u_int8_t y;
    u_int8_t u;
    u_int8_t v;
} EPixelYUV_t;

#define W16(x) ((u_int16_t)(x))
#define W32(x) ((u_int32_t)(x))

static inline u_int16_t u16REV(u_int16_t x)
{
    return (x<<8) | (x>>8);
}

static inline u_int32_t u32REV(u_int32_t x)
{
    return (x<<24) | (x>>24) | ((x<<8) & 0x00ff0000) | ((x>>8) & 0x0000ff00);
}

#if BYTE_ORDER == BIG_ENDIAN

#define I16LE(x)  ((int16_t) u16REV(x))
#define I32LE(x)  ((int32_t) u32REV(x))
#define U16LE(x)  U16REV(x)
#define U32LE(x)  U32REV(x)

#define I16BE(x)  ((int16_t)(x))
#define I32BE(x)  ((int32_t)(x))
#define U16BE(x)  ((u_int16_t)(x))
#define U32BE(x)  ((u_int32_t)(x))

#else

#define I16LE(x)  ((int16_t)(x))
#define I32LE(x)  ((int32_t)(x))
#define U16LE(x)  ((u_int16_t)(x))
#define U32LE(x)  ((u_int32_t)(x))

#define I16BE(x)  ((int16_t) u16REV(x))
#define I32BE(x)  ((int32_t) u32REV(x))
#define U16BE(x)  U16REV(x)
#define U32BE(x)  U32REV(x)

#endif


#define EALPHA_TRANSPARENT 0
#define EALPHA_OPAQUE      255

static inline EPixel_t epixel_argb(u_int8_t a, u_int8_t r, 
				   u_int8_t g, u_int8_t b)
{
    EPixel_t p;
    p.px = (a<<24)|(r<<16)|(g<<8)|(b);
    return p;
}

// Swap R and B component
static inline EPixel_t epixel_swap(EPixel_t p)
{
    return epixel_argb(p.a, p.b, p.g, p.r);
/*    u_int8_t t = p.r;
    p.r = p.b;
    p.b = t;
    return p; */
}

static inline EPixel_t epixel_rgb(u_int8_t r, u_int8_t g, u_int8_t b)
{
    return epixel_argb(EALPHA_OPAQUE, r, g, b);
}

static inline EPixel_t epixel_transparent()
{
    return epixel_argb(EALPHA_TRANSPARENT, 0, 0, 0);
}

static inline u_int8_t eblend(u_int8_t a, u_int8_t s, u_int8_t d)
{
    return (a*(s-d) + (d<<8))>>8;
}

static inline u_int8_t esum(u_int8_t a, u_int8_t b)
{
    u_int16_t s = a+b;
    if (s > 255) return 255; // saturate
    return s;
}

static inline u_int8_t emultiply(u_int8_t a, u_int8_t b)
{
    u_int16_t p = a*b;
    return p >> 8;
}

// epixel_shadow8_1(a,d) == epixel_blend8_1(a,0,d)
static inline u_int8_t eshadow(u_int8_t a, u_int8_t d)
{
    return ((d<<8) - a*d)>>8;
}

static inline EPixel_t EPixelBlend(u_int8_t a, EPixel_t s, EPixel_t d)
{
    EPixel_t p;
    p.r = eblend(a, s.r, d.r);
    p.g = eblend(a, s.g, d.g);
    p.b = eblend(a, s.b, d.b);
    p.a = eblend(a, s.a, d.a); // d.a;
    return p;
}

static inline EPixel_t EPixelSum(EPixel_t a, EPixel_t b)
{
    EPixel_t s;
    s.r = esum(a.r, b.r);
    s.g = esum(a.g, b.g);
    s.b = esum(a.b, b.b);
    s.a = esum(a.a, b.a);
    return s;
}

/*
 * Multiply pixel channel by channel.
 */
static inline EPixel_t EPixelMultiply(EPixel_t a, EPixel_t b)
{
    EPixel_t p;
    p.r = emultiply(a.r, b.r);
    p.g = emultiply(a.g, b.g);
    p.b = emultiply(a.b, b.b);
    p.a = emultiply(a.a, b.a);
    return p;
}


static inline EPixel_t EPixelShadow(u_int8_t g, EPixel_t d)
{
    EPixel_t p;
    p.r = eshadow(g, d.r);
    p.g = eshadow(g, d.g);
    p.b = eshadow(g, d.b);
    p.a = eshadow(g, d.a); // d.a;  
    return p;
}

/* luminance */
static inline u_int8_t EPixelLuminance(EPixel_t p)
{
    return (p.r*299 + p.g*587 + p.b*114) / 1000;
    // return (p.r*30 + p.g*59 + p.b*11) / 100;
    // return (p.r*2989 + p.g*5870 + p.b*1140) / 10000;
}

// Y := min(abs(r * 2104 + g * 4130 + b * 802 + 4096 + 131072) >> 13, 235)
// U := min(abs(r * -1214 + g * -2384 + b * 3598 + 4096 + 1048576) >> 13, 240)
// V := min(abs(r * 3598 + g * -3013 + b * -585 + 4096 + 1048576) >> 13, 240)
static inline EPixelYUV_t EPixelRGB2YUV(EPixel_t p)
{
    EPixelYUV_t q;

    q.a = p.a;
    q.y = (p.r*299 + p.g*587 + p.b*114) / 1000;
    q.u = (p.r*701 - p.g*587 - p.b*115) / 1000;
    q.v = (-p.r*299 - 587*p.g + 886+p.b) / 1000;
    return q;
}

static inline EPixel_t EPixelYUV2RGB(EPixelYUV_t q)
{
    EPixel_t p;
    p.a = q.a;
    p.r = q.y + q.u;
    p.g = q.y - (510*q.u - 186*q.v)/1000;
    p.b = q.y + q.v;
    return p;
}

typedef EPixel_t (*EPixelUnpack_t)(u_int8_t* src);
typedef void     (*EPixelPack_t)(EPixel_t p, u_int8_t* dst);

extern EPixelUnpack_t EPixelUnpackFunc(int pixel_type);

static inline EPixel_t EPixelUnpack(int pixel_type, u_int8_t* src)
{
    EPixel_t p;
    switch (pixel_type) {
    case EPIXEL_TYPE_A8:
	p.a = src[0]; p.r = p.g = p.b = 0;
	break;
    case EPIXEL_TYPE_R8:
	p.r = src[0]; p.g = p.b = 0; p.a = 255;
	break;
    case EPIXEL_TYPE_G8:
	p.g = src[0]; p.r = p.b = 0; p.a = 255;
	break;
    case EPIXEL_TYPE_B8:
	p.b = src[0]; p.r = p.g = 0; p.a = 255;
	break;
    case EPIXEL_TYPE_R8G8B8:
	p.r=src[0]; p.g=src[1]; p.b=src[2]; p.a=255;
	break;
    case EPIXEL_TYPE_R8G8B8A8:
	p.r=src[0]; p.g=src[1]; p.b=src[2]; p.a=src[3];
	break;
    case EPIXEL_TYPE_A8R8G8B8:
	p.a=src[0]; p.r=src[1]; p.g=src[2]; p.b=src[3];
	break;
    case EPIXEL_TYPE_B8G8R8:
	p.b=src[0]; p.g=src[1]; p.r=src[2]; p.a = 255; 
	break;
    case EPIXEL_TYPE_B8G8R8A8:
	p.b=src[0]; p.g=src[1]; p.r=src[2]; p.a=src[3];
	break;
    case EPIXEL_TYPE_A8B8G8R8:
	p.a=src[0]; p.b=src[1]; p.g=src[2]; p.r=src[3];
	break;

    case EPIXEL_TYPE_A8Y8U8V8: {
	EPixelYUV_t yuv;
	yuv.a = src[0];
	yuv.y = src[1];
	yuv.u = src[2];
	yuv.v = src[3];
	p = EPixelYUV2RGB(yuv);
	break;
    }
    case EPIXEL_TYPE_R5G5B5A1: {
	u_int16_t v = src[0]<<8 | src[1];
	p.r = (v >> 8) & 0xf8;
	p.g = (v >> 3) & 0xf8;
	p.b = (v << 2) & 0xf8;
	p.a = (v&1) ? 255 : 0;
	break;
    }
    case EPIXEL_TYPE_R5G5B5X1: {
	u_int16_t v = src[0]<<8 | src[1];
	p.r = (v >> 8) & 0xf8;
	p.g = (v >> 3) & 0xf8;
	p.b = (v << 2) & 0xf8;
	p.a = 255;
	break;
    }
    case EPIXEL_TYPE_A1R5G5B5: {
	u_int16_t v = src[0]<<8 | src[1];
	p.r = (v >> 7) & 0xf8;
	p.g = (v >> 2) & 0xf8;
	p.b = (v << 3) & 0xf8;
	p.a = (v >> 15) ? 255 : 0;
	break;
    }
    case EPIXEL_TYPE_X1R5G5B5: {
	u_int16_t v = src[0]<<8 | src[1];
	p.r = (v >> 7) & 0xf8;
	p.g = (v >> 2) & 0xf8;
	p.b = (v << 3) & 0xf8;
	p.a = 255;
	break;
    }
    case EPIXEL_TYPE_565_BE: {
	u_int16_t v = src[0]<<8 | src[1];
	/* shift and scale to range [0-255] */
	p.r=(v >> 8) & 0xf8;
	p.g=(v >> 3) & 0xfc;
	p.b=(v & 0x1f) << 3;
	p.a=255;
	break;
    }
    case EPIXEL_TYPE_565_LE: {
	u_int16_t v = src[1]<<8 | src[0];
	/* shift and scale to range [0-255] */
	p.r=(v >> 8) & 0xf8;
	p.g=(v >> 3) & 0xfc;
	p.b=(v & 0x1f) << 3;
	p.a=255;
	break;
    }

    default:
	fprintf(stderr, "epixel_read: bad color spec: %x\n", pixel_type);
	p.r = p.g = p.b = p.a = 0;
	break;
    }
    return p;
}

extern EPixelPack_t EPixelPackFunc(int pixel_type);

static inline void EPixelPack(int pixel_type, EPixel_t p, u_int8_t* dst)
{
    switch (pixel_type) {
    case EPIXEL_TYPE_A8:
	dst[0]=p.a;
	break;
    case EPIXEL_TYPE_R8:
	dst[0]=p.r;
	break;
    case EPIXEL_TYPE_G8:
	dst[0]=p.g;
	break;
    case EPIXEL_TYPE_B8:
	dst[0]=p.b;
	break;
    case EPIXEL_TYPE_R8G8B8:
	dst[0]=p.r;
	dst[1]=p.g;
	dst[2]=p.b;
	break;
    case EPIXEL_TYPE_R8G8B8A8:
	dst[0]=p.r; 
	dst[1]=p.g;
	dst[2]=p.b;
	dst[3]=p.a;
	break;
    case EPIXEL_TYPE_A8R8G8B8:
	dst[0]=p.a;
	dst[1]=p.r;
	dst[2]=p.g;
	dst[3]=p.b;
	break;
    case EPIXEL_TYPE_B8G8R8:
	dst[0]=p.b;
	dst[1]=p.g;
	dst[2]=p.r;
	break;
    case EPIXEL_TYPE_B8G8R8A8:
	dst[0]=p.b;
	dst[1]=p.g;
	dst[2]=p.r;
	dst[3]=p.a;
	break;
    case EPIXEL_TYPE_A8B8G8R8:
	dst[0]=p.a;
	dst[1]=p.b;
	dst[2]=p.g;
	dst[3]=p.r; 
	break;
    case EPIXEL_TYPE_R5G5B5A1: { 
	u_int16_t v = ((p.r>>3)<<11)|((p.g>>3)<<6)|((p.b>>3) <<1)|((p.a)!=0);
	dst[0] = (v >> 8);
	dst[1] = v & 0xff;
	break;
    }
    case EPIXEL_TYPE_R5G5B5X1: {
	u_int16_t v = ((p.r>>3)<<11)|((p.g>>3)<<6)|((p.b>>3) <<1);
	dst[0] = (v >> 8);
	dst[1] = v & 0xff;
	break;
    }
    case EPIXEL_TYPE_A1R5G5B5: {
	u_int16_t v = ((p.r>>3)<<10)|((p.g>>3)<<5)|((p.b>>3))|(((p.a)!=0)<<15);
	dst[0] = (v >> 8);
	dst[1] = v & 0xff;
	break;
    }
    case EPIXEL_TYPE_X1R5G5B5: {
	u_int16_t v = ((p.r>>3)<<10)|((p.g>>3)<<5)|((p.b>>3));
	dst[0] = (v >> 8);
	dst[1] = v & 0xff;
	break;
    }

    case EPIXEL_TYPE_565_BE: {
	u_int16_t v =
	    ((p.r>>3) << 11) | ((p.g>>2) << 5)  | ((p.b>>3));
	dst[0] = (v >> 8);
	dst[1] = v & 0xff;
	break;
    }
    case EPIXEL_TYPE_565_LE: {
	u_int16_t v =
	    ((p.r>>3) << 11) | ((p.g>>2) << 5)  | ((p.b>>3));
	dst[0] = v & 0xff;
	dst[1] = (v >> 8);
	break;
    }
    default:
	fprintf(stderr, "epixel_write: bad color spec: %x\n", pixel_type);
    }
}

extern void (*ESimdCopy)(const u_int8_t* src, u_int8_t* dst, size_t n);

extern void (*ESimdFill32)(u_int8_t* dst, u_int32_t v, size_t n);

extern void (*ESimdAddBlendAreaRGBA32)(u_int8_t* src, int src_wb,
				       u_int8_t* dst, int dst_wb,
				       u_int8_t af, EPixel_t color,
				       unsigned int width, 
				       unsigned int height);
extern void (*ESimdAddBlendAreaARGB32)(u_int8_t* src, int src_wb,
				       u_int8_t* dst, int dst_wb,
				       u_int8_t af, EPixel_t color,
				       unsigned int width, 
				       unsigned int height);
extern void (*ESimdAddBlendAreaA8_RGBA32)(u_int8_t* src, int src_wb, 
					  u_int8_t* dst, int dst_wb,
					  u_int8_t af, EPixel_t color,
					  unsigned int width, 
					  unsigned int height);
extern void (*ESimdAddBlendAreaA8_ARGB32)(u_int8_t* src, int src_wb,
					  u_int8_t* dst, int dst_wb,
					  u_int8_t af, EPixel_t color,
					  unsigned int width, 
					  unsigned int height);

extern void (*ESimdAlphaAreaARGB32)(u_int8_t* src, int src_wb,
				    u_int8_t* dst, int dst_wb,
				    u_int8_t a, unsigned int width,
				    unsigned int height);
extern void (*ESimdAlphaAreaRGBA32)(u_int8_t* src, int src_wb,
				    u_int8_t* dst, int dst_wb,
				    u_int8_t a, unsigned int width,
				    unsigned int height);
extern void (*ESimdBlendAreaRGBA32)(u_int8_t* src, int src_wb, 
				    u_int8_t* dst, int dst_wb,
				    unsigned int width, unsigned int height);
extern void (*ESimdBlendAreaARGB32)(u_int8_t* src, int src_wb, 
				    u_int8_t* dst, int dst_wb,
				    unsigned int width, unsigned int height);
extern void (*ESimdFadeAreaRGBA32)(u_int8_t* src, int src_wb,
				   u_int8_t* dst, int dst_wb,
				   u_int8_t af, 
				   unsigned int width, unsigned int height);
extern void (*ESimdFadeAreaARGB32)(u_int8_t* src, int src_wb,
				   u_int8_t* dst, int dst_wb,
				   u_int8_t af, 
				   unsigned int width, unsigned int height);
extern void (*ESimdFillAreaBlendRGB24)(u_int8_t* dst,int dst_wb,
				       unsigned int width, unsigned int height, 
				       EPixel_t p);
extern void (*ESimdFillAreaBlendARGB32)(u_int8_t* dst,int dst_wb,
					unsigned int width, 
					unsigned int height, 
					EPixel_t p);
extern void (*ESimdFillAreaBlendRGBA32)(u_int8_t* dst,int dst_wb,
					unsigned int width, 
					unsigned int height, 
					EPixel_t p);


extern int EPixelTypeFromName(char* name);
extern EPixel_t EPixelFromString(char* name);

#define EPIXEL_WHITE       {{255,255,255,255}}
#define EPIXEL_BLACK       {{255,0,0,0}}
#define EPIXEL_RED         {{255,255,0,0}}
#define EPIXEL_GREEN       {{255,0,255,0}}
#define EPIXEL_BLUE        {{255,0,0,255}}
#define EPIXEL_TRANSPARENT {{0,255,255,255}}

#define epixel_white  epixel_rgb(255,255,255)
#define epixel_black  epixel_rgb(0,0,0)
#define epixel_red    epixel_rgb(255,0,0)
#define epixel_green  epixel_rgb(0,255,0)
#define epixel_blue   epixel_rgb(0,0,255)

#define EPIXEL_ADDR(map,x,y) \
    ((map)->data + ((y)*(map)->bytesPerRow) + ((x)*(map)->bytesPerPixel))

typedef struct epic_point {
    int x;
    int y;
} EPoint_t;

typedef struct epic_dimension {
    unsigned int width;
    unsigned int height;
} EDimension_t;

typedef struct epic_rect {
    EPoint_t xy;
    EDimension_t wh;
} ERect_t;

typedef struct epic_filter {
    EDimension_t wh;
    u_int32_t fsum;    /* sum(abs(factor[i]))  */
    u_int8_t* factor;  /* factors */
    u_int8_t  data[1]; /* factor if stored */
} EFilter_t;

#define ESwapInt(a,b) do { int _swap_t = (a); a = (b); b = _swap_t; } while(0)
#define ESwapInt8(a,b) do { u_int8_t _swap_t = (a); a = (b); b = _swap_t; } while(0)
#define ESwapFloat(a,b) do { float _swap_t = (a); a = (b); b = _swap_t; } while(0)

static inline int EMaxInt(int a, int b)
{
    return (a>b) ? a : b;
}

static inline int EMinInt(int a, int b)
{
    return (a<b) ? a : b;
}

static inline float EMaxFloat(float a, float b)
{
    return (a>b) ? a : b;
}

static inline float EMinFloat(float a, float b)
{
    return (a<b) ? a : b;
}

/* check if |a|<|b| */
static inline int EAbsLess(int a, int b)
{
    return (((a<0) ? -a : a) < ((b<0) ? -b : b));
}

static inline int EClipRange(int a, int low, int high)
{
    if (a < low) return low;
    if (a > high) return high;
    return a;
}

// Return length of the Intersection of range [a1,a2] with [b1,b2]
// Assertion a1 <= a2, b1 <= b2 !!!
static inline int EIntersectRangeLength(int a1, int a2, int b1, int b2)
{
    a1 = EMaxInt(a1,b1);
    a2 = EMinInt(a2,b2);
    if (a1 > a2) return 0;
    return (a2-a1)+1;
}

// Intersect range [a1,a2] with [b1,b2] and put the result in [c1,c2]
// and return the length of the result. Zero is returned if there
// is not intersection
// Assertion a1 <= a2, b1 <= b2 !!!
static inline int EIntersectRange(int a1, int a2, int b1, int b2,
				  int* c1, int* c2)
{
    a1 = EMaxInt(a1,b1);
    a2 = EMinInt(a2,b2);
    if (c1) *c1 = a1;
    if (c2) *c2 = a2;
    if (a1 > a2)
	return 0;
    return (a2-a1)+1;
}

// Intersect two segments [a,a+al-1] and [b,b+bl-1]
// put the result the intersection start point in c and 
// return its length. 
//
static inline int EIntersectSegments(int a, int al, int b, int bl, int* c)
{
    int ct = EMaxInt(a,b);
    b = EMinInt(a+al-1,b+bl-1);
    if (c) *c = ct;
    if (ct > b) return 0;
    return (b-ct)+1;
}

static inline int EInRange(int a, int low, int high)
{
    return ((a >= low) && (a <= high));
}

static inline void EPointSet(EPoint_t* p, int x, int y)
{
    p->x = x;
    p->y = y;
}

static inline int EPointEqual(EPoint_t* p, EPoint_t* q)
{
    return ((p->x == q->x) && (p->y == q->y));
}


static inline void EDimensionSet(EDimension_t* d, 
				 unsigned int width, unsigned int height)
{
    d->width  = width;
    d->height = height;
}

static inline int EDimensionEqual(EDimension_t* d, EDimension_t* e)
{
    return ((d->width == e->width) && (d->height == e->height));
}


static inline void ERectSet(ERect_t* r, int x, int y, 
			    unsigned int width, unsigned int height)
{
    EPointSet(&r->xy, x, y);
    EDimensionSet(&r->wh, width, height);
}

static inline int ERectEqual(ERect_t* a, ERect_t* b)
{
    return EPointEqual(&a->xy, &b->xy) && EDimensionEqual(&a->wh, &b->wh);
}

static inline void ERectSetXY(ERect_t* r, int x1, int y1, int x2, int y2)
{
    if (x1 < x2) { 
	r->xy.x = x1; 
	r->wh.width = (x2-x1)+1;
    }
    else {
	r->xy.x = x2; 
	r->wh.width = (x1-x2)+1;
    }
    if (y1 < y2) { 
	r->xy.y = y1; 
	r->wh.height = (y2-y1)+1;
    }
    else {
	r->xy.y = y2; 
	r->wh.height = (y1-y2)+1;
    }
}

static inline int ERectLeft(const ERect_t* r)
{
    return r->xy.x;
}

static inline int ERectRight(const ERect_t* r)
{
    return r->xy.x+r->wh.width-1;
}

static inline int ERectTop(const ERect_t* r)
{
    return r->xy.y;
}

static inline int ERectBottom(const ERect_t* r)
{
    return r->xy.y+r->wh.height-1;
}

static inline unsigned int ERectWidth(const ERect_t* r)
{
    return r->wh.width;
}

static inline unsigned int ERectHeight(const ERect_t* r)
{
    return r->wh.height;
}

// Intersect rect's a and b put the resule in c return 0 if c is empty
static inline int ERectIntersect(const ERect_t* a, const ERect_t* b, ERect_t* r)
{
    int left, right, top, bottom;

    left  = EMaxInt(ERectLeft(a), ERectLeft(b));
    right = EMinInt(ERectRight(a), ERectRight(b));
    if (left > right) goto empty;

    top    = EMaxInt(ERectTop(a), ERectTop(b));
    bottom = EMinInt(ERectBottom(a), ERectBottom(b));
    if (top > bottom) goto empty;

    EPointSet(&r->xy, left, top);
    EDimensionSet(&r->wh, (right-left)+1, (bottom-top)+1);
    return 1;
empty:
    memset(r, 0, sizeof(ERect_t));
    return 0;
}

static inline void ERectUnion(const ERect_t* a, const ERect_t* b, ERect_t* r)
{
    int left, right, top, bottom;

    left   = EMinInt(ERectLeft(a), ERectLeft(b));
    right  = EMaxInt(ERectRight(a), ERectRight(b));
    top    = EMinInt(ERectTop(a), ERectTop(b));
    bottom = EMaxInt(ERectBottom(a), ERectBottom(b));

    EPointSet(&r->xy, left, top);
    EDimensionSet(&r->wh, (right-left)+1, (bottom-top)+1);
}

// Check if a is a proper sub-rect of b (e.g the intersection(a,b) = a)
static inline int ERectIsSubRect(const ERect_t* a, const ERect_t* b)
{
    return (ERectLeft(a) >= ERectLeft(b)) &&
	(ERectRight(a) <= ERectRight(b)) &&
	(ERectTop(a) >= ERectTop(b)) &&
	(ERectBottom(a) <= ERectBottom(b));
}

// Form a union of ERect and A point
static inline void ERectAddPoint(const ERect_t* a,const EPoint_t* p,ERect_t* r)
{
    int left, right, top, bottom;

    left   = EMinInt(ERectLeft(a),  p->x);
    right  = EMaxInt(ERectRight(a), p->x+1);
    top    = EMinInt(ERectTop(a),   p->y);
    bottom = EMaxInt(ERectBottom(a), p->y+1);

    EPointSet(&r->xy, left, top);
    EDimensionSet(&r->wh, (right-left)+1, (bottom-top)+1);
}

static inline int EPointInRect(const EPoint_t* p, ERect_t* r)
{
    return ((p->x >= ERectLeft(r)) && (p->x <= ERectRight(r)) &&
	    (p->y >= ERectTop(r)) && (p->y <= ERectBottom(r)));
}

static inline int EPointXYInRect(int x, int y, ERect_t* r)
{
    return ((x >= ERectLeft(r)) && (x <= ERectRight(r)) &&
	    (y >= ERectTop(r)) && (y <= ERectBottom(r)));
}


typedef enum
{
    EBACKEND_TYPE,
    EWINDOW_TYPE,
    EPIXMAP_TYPE,
    EBITMAP_TYPE,
    EGC_TYPE,
    EDICT_TYPE,
    EFONT_TYPE
} EObjectType_t;

extern void EBACKEND_TYPE_RELEASE(void*);
extern void EWINDOW_TYPE_RELEASE(void*);
extern void EPIXMAP_TYPE_RELEASE(void*);
extern void EBITMAP_TYPE_RELEASE(void*);
extern void EGC_TYPE_RELEASE(void*);
extern void EDICT_TYPE_RELEASE(void*);
extern void EFONT_TYPE_RELEASE(void*);

typedef unsigned long hash_value_t;

typedef struct _EBucket {
    struct _EBucket* next;
    hash_value_t hvalue;
} EBucket;


typedef struct {
    hash_value_t (*hash)(void*);  /* calculate hash */
    int (*cmp)(void*, void*);     /* compare data items */
    void (*release)(void*);       /* data release (free) */
    void* (*copy)(void*);         /* copy (may be used with insert) */

    int is_allocated;
    char* name;

    unsigned int thres;        /* Medium bucket chain len, for grow */
    unsigned int szm;          /* current size mask */
    unsigned int nactive;      /* Number of "active" slots */
    unsigned int nslots;       /* Total number of slots */
    unsigned int nitems;       /* Total number of items */
    unsigned int p;            /* Split position */
    unsigned int nsegs;        /* Number of segments */
    unsigned int n_resize;     /* Number of index realloc calls */
    unsigned int n_seg_alloc;  /* Number of segment allocations */
    unsigned int n_seg_free;   /* Number of segment destroy */
    EBucket*** seg;
} EHash;

extern EHash* EHashNew(char* name, int thres,
		       hash_value_t (*hash)(void*),
		       int (*cmp)(void*, void*),
		       void (*release)(void*),
		       void* (*copy)(void*));
extern EHash* EHashInit(EHash* lh, char* name, int thres, 
			hash_value_t (*hash)(void*),
			int (*cmp)(void*, void*),
			void (*release)(void*),
			void* (*copy)(void*));
extern void EHashDelete(EHash* lh);
extern void* EHashLookup(EHash* lh, void* key);
extern void* EHashInsert(EHash* lh, void* key, void* data);
extern void* EHashInsertNew(EHash* lh, void* key, void* data);
extern void* EHashErase(EHash* lh, void* key);
extern void EHashEach(EHash* lh, 
		      void (elem)(EHash* lh, void* elem, void* arg),
		      void* arg);
extern void EHashInfo(EHash* lh);


#define EOBJECT_INIT(obj,Type)			\
    (obj)->on_heap = 0;				\
    (obj)->refc    = 0;				\
    (obj)->release = Type##_RELEASE;		\
    (obj)->opaque = NULL;			\
    (obj)->next   = NULL;			\
    (obj)->type   = (Type)

#define EOBJECT_MEMBERS(Type_t) \
    EBucket        hbucket;			\
    int            on_heap;			\
    unsigned int   refc;			\
    void           (*release)(void*);		\
    void*          opaque;			\
    EObjectType_t  type;			\
    Type_t*        next

/* generic object */
typedef struct epic_object {
    EOBJECT_MEMBERS(struct epic_object);
} EObject;

static inline void EObjectRef(void* arg)
{
    EObject* obj = (EObject*) arg;
    obj->refc++;
}

static inline void EObjectUnRef(void* arg)
{
    EObject* obj = (EObject*) arg;
    if (obj) {
	if (obj->refc <= 1) {
	    if (obj->release)
		obj->release(obj);
	}
	else
	    obj->refc--;
    }
}

static inline void EObjectLink(void* list, void* obj)
{
    ((EObject*)obj)->next = (EObject*)*((void**)list);
    *((void**)list) = obj;
}

static inline void EObjectUnlink(void* list, void* obj)
{
    EObject** pp = (EObject**)list;
    while((*pp) && (*pp) != (EObject*)obj) {
	pp = &(*pp)->next;
    }
    if ((*pp) == (EObject*)obj)
	*pp = ((EObject*)obj)->next;
}


/* GENERAL FLAGS */
/* EPIC Flags (MOVE ALL TO GC!) */


/* GENERAL FLAGS */
#define EFLAG_NONE                 0x00000000
#define EFLAG_SOLID                0x00000001
#define EFLAG_BLEND                0x00000002
#define EFLAG_SUM                  0x00000004
#define EFLAG_AALIAS               0x00000008
#define EFLAG_TEXTURED             0x00000010
#define EFLAG_NFIRST               0x00001000
#define EFLAG_NLAST                0x00002000

/* FILL FLAGS */
#define EPIC_FILL_STYLE_NONE       EFLAG_NONE     /* Do not fill */
#define EPIC_FILL_STYLE_SOLID      EFLAG_SOLID
#define EPIC_FILL_STYLE_BLEND      EFLAG_BLEND
#define EPIC_FILL_STYLE_SUM        EFLAG_SUM
#define EPIC_FILL_STYLE_AALIAS     EFLAG_AALIAS
#define EPIC_FILL_STYLE_TEXTURED   EFLAG_TEXTURED

/* LINE FLAGS */
#define EPIC_LINE_STYLE_SOLID      EFLAG_SOLID
#define EPIC_LINE_STYLE_BLEND      EFLAG_BLEND
#define EPIC_LINE_STYLE_SUM        EFLAG_SUM
#define EPIC_LINE_STYLE_AALIAS     EFLAG_AALIAS
#define EPIC_LINE_STYLE_TEXTURED   EFLAG_TEXTURED
#define EPIC_LINE_STYLE_DASHED     0x00000100
#define EPIC_LINE_STYLE_NFIRST     EFLAG_NFIRST
#define EPIC_LINE_STYLE_NLAST      EFLAG_NLAST

/* BORDER FLAGS */
#define EPIC_BORDER_STYLE_SOLID    EFLAG_SOLID
#define EPIC_BORDER_STYLE_BLEND    EFLAG_BLEND
#define EPIC_BORDER_STYLE_SUM      EFLAG_SUM
#define EPIC_BORDER_STYLE_AALIAS   EFLAG_AALIAS
#define EPIC_BORDER_STYLE_TEXTURED EFLAG_TEXTURED
#define EPIC_BORDER_STYLE_DASHED   0x00010000
#define EPIC_BORDER_STYLE_NBORDER1 0x00100000
#define EPIC_BORDER_STYLE_NBORDER2 0x00200000
#define EPIC_BORDER_STYLE_NBORDER3 0x00400000
#define EPIC_BORDER_STYLE_NBORDER4 0x00800000
#define EPIC_BORDER_STYLE_NBORDER  0x00F00000

typedef enum {
    EPIC_CAP_STYLE_NONE  = 0,
    EPIC_CAP_STYLE_BUTT  = 1,
    EPIC_CAP_STYLE_ROUND = 2,
    EPIC_CAP_STYLE_PROJECTING = 3
} ECapStyle_t;

/* JOIN ENUMERATION */
typedef enum {
    EPIC_JOIN_STYLE_MITER = 0,
    EPIC_JOIN_STYLE_ROUND = 1,
    EPIC_JOIN_STYLE_BEVEL = 2
} EJoinStyle_t;

/* ALPHA FACTOR (fixnum masks) */
#define ALPHA_FACTOR_0     0x00  /* 1 */
#define ALPHA_FACTOR_1     0xFF  /* 1 */
#define ALPHA_FACTOR_1_2   0x80  /* 1/2 */
#define ALPHA_FACTOR_1_4   0x40  /* 1/4 */
#define ALPHA_FACTOR_1_8   0x20  /* 1/8 */
#define ALPHA_FACTOR_1_16  0x10  /* 1/16 */
#define ALPHA_FACTOR_1_32  0x08  /* 1/32 */
#define ALPHA_FACTOR_1_64  0x04  /* 1/64 */
#define ALPHA_FACTOR_1_128 0x02  /* 1/128 */
#define ALPHA_FACTOR_1_256 0x01  /* 1/256 */


typedef u_int32_t EFlags_t;
struct epic_pixmap;
struct epic_font;

typedef struct epic_gc {
    EOBJECT_MEMBERS(struct epic_gc);

    /* Fill Options */
    /*! EPIC_FILL_STYLE_x flags */
    EFlags_t            fill_style;
    /*! Color for fill operations. */    
    EPixel_t            fill_color;
    /*! Texture for filling. */
    struct epic_pixmap* fill_texture;

    /* LINE DRAWING */
    /*! EPIC_FILL_STYLE_x flags */
    EFlags_t     line_style;
    /*! EPIC_JOIN_STYLE_x enum */
    EJoinStyle_t line_join_style;
    /*! EPIC_CAP_STYLE_x enum */
    ECapStyle_t  line_cap_style;
    /*! Line width. */
    unsigned int line_width;
    /*! Texture for drawing lines */
    struct epic_pixmap* line_texture;

    /* BORDER DRAWING */
    /*! EPIC_BORDER_STYLE_x flags */
    EFlags_t     border_style;
    /*! EPIC_JOIN_STYLE_x enum */
    EJoinStyle_t border_join_style;
    /*! EPIC_CAP_STYLE_x enum */
    ECapStyle_t  border_cap_style;
    /*! Border base color */
    EPixel_t     border_color;
    /*! Border=0 means not used (when fill) */
    unsigned int border_width;
    /*! Texture for drawing borders. */
    struct epic_pixmap* border_texture;  

    /* Base Colors */
    /*! Line drawing/font foreground. */
    EPixel_t     foreground_color;
    /*! Text background and others */
    EPixel_t     background_color;
    /* Alpha fader (fixnum 8 bit) 255 = 1.0 */
    u_int8_t     fader_value;

    /* Text Drawing */
    /*! Font used for drawing text */
    struct epic_font* font; 
    
    /*! Added extra horizontal amount after glyph is draw */
    int16_t      glyph_delta_x;
    /*! Added extra vertical amount after glyph is draw */
    int16_t      glyph_delta_y;
    /*! Override glyph with for  glyph drawing only used if not zero */
    u_int16_t    glyph_fixed_width;
    /*! Special dot kerning for char '.' code = 46, */
    int16_t      glyph_dot_kern;
} EGc;

extern EGc default_gc;


static inline void EGcSetForegroundColor(EGc* gc, EPixel_t color)
{
    gc->foreground_color = color;
}

static inline void EGcSetBackgroundColor(EGc* gc, EPixel_t color)
{
    gc->background_color = color;
}

static inline void EGcSetFillStyle(EGc* gc, unsigned int style)
{
    gc->fill_style = style;
}

static inline void EGcSetFillColor(EGc* gc, EPixel_t color)
{
    gc->fill_color = color;
}

static inline void EGcSetFillTexture(EGc* gc, struct epic_pixmap* texture)
{
    if (texture)  EObjectRef(texture);
    EObjectUnRef(gc->fill_texture);
    gc->fill_texture = texture;
}

static inline void EGcSetLineStyle(EGc* gc, unsigned int style)
{
    gc->line_style = style;
}

static inline void EGcSetLineJoinStyle(EGc* gc, EJoinStyle_t style)
{
    gc->line_join_style = style;
}

static inline void EGcSetLineCapStyle(EGc* gc, ECapStyle_t style)
{
    gc->line_cap_style = style;
}


static inline void EGcSetLineWidth(EGc* gc, unsigned int width)
{
    gc->line_width = width;
}

static inline void EGcSetLineTexture(EGc* gc, struct epic_pixmap* texture)
{
    if (texture)  EObjectRef(texture);
    EObjectUnRef(gc->line_texture);
    gc->line_texture = texture;
}

static inline void EGcSetBorderStyle(EGc* gc, EFlags_t style)
{
    gc->border_style = style;
}

static inline void EGcSetBorderJoinStyle(EGc* gc, EJoinStyle_t style)
{
    gc->border_join_style = style;
}

static inline void EGcSetBorderCapStyle(EGc* gc, ECapStyle_t style)
{
    gc->border_cap_style = style;
}

static inline void EGcSetBorderWidth(EGc* gc, unsigned int width)
{
    gc->border_width = width;
}

static inline void EGcSetBorderColor(EGc* gc, EPixel_t color)
{
    gc->border_color = color;
}

static inline void EGcSetBorderTexture(EGc* gc, struct epic_pixmap* texture)
{
    if (texture)  EObjectRef(texture);
    EObjectUnRef(gc->border_texture);
    gc->border_texture = texture;
}

static inline void EGcSetFaderValue(EGc* gc, u_int8_t fade)
{
    gc->fader_value = fade;
}

static inline void EGcSetFaderFloat(EGc* gc, float fade)
{
    if (fade >= 1.0)
	gc->fader_value = 255;
    else if (fade <= 0.0)
	gc->fader_value = 0;
    else
      gc->fader_value = (u_int8_t)(fade*256);
}

static inline void EGcSetFont(EGc* gc, struct epic_font* font)
{
    if (font) EObjectRef(font);
    EObjectUnRef(gc->font);
    gc->font = font;
}

static inline void EGcSetGlyphDelta(EGc* gc, int16_t x, int16_t y)
{
    gc->glyph_delta_x = x;
    gc->glyph_delta_y = y;
}

static inline void EGcSetGlyphWidth(EGc* gc, u_int16_t width)
{
    gc->glyph_fixed_width = width;
}

static inline void EGcSetGlyphDotKern(EGc* gc, u_int16_t kern)
{
    gc->glyph_dot_kern = kern;
}


typedef struct epic_pixmap {
    EOBJECT_MEMBERS(struct epic_pixmap);
    struct epic_backend* backend;  /* backend pointer if attached */

    /*! Clip rectangle restrict pixels drawn within boundary */
    ERect_t clip;               
    /*! Width of pixmap in pixels  */
    unsigned int width;
    /*! Width of pixmap in bytes   */   
    unsigned int bytesPerRow;
    /*! Height of pixmap in pixels */
    unsigned int height;        
    /*! Size of pixel in bits, including pad */
    unsigned int bitsPerPixel;
    /*! Pixel type used for pixmap */
    unsigned int pixelType;
    /*! Number of bytes per pixel, may also be calculated from pixelType */
    unsigned int bytesPerPixel;
    /*! Size of pixel area in bytes */
    size_t      sz;
    /*! Unaligned pixel data, DO NOT USED */
    u_int8_t* data0;            
    /*! 16 byte aligned pixel data */
    u_int8_t* data;
} EPixmap;

typedef struct epic_bitmap {
    EOBJECT_MEMBERS(struct epic_bitmap);
    /*! Clip rectangle restrict pixels drawn within boundary */
    ERect_t clip;               
    /*! total width in pixels  */
    unsigned int width;         
    /*! total width in bytes  */
    unsigned int bytesPerRow;   
    /*! total height in pixels */
    unsigned int height;
    /*! 1 */
    unsigned int d;
    /*! size of bitmap area in bytes */
    size_t      sz;
    /*! data, each row is padded to byte size */
    u_int8_t* data;    
} EBitmap;

typedef enum {
    EFONT_WEIGHT_NONE     = 0,
    EFONT_WEIGHT_MEDIUM   = 1,
    EFONT_WEIGHT_BOLD     = 2,
    EFONT_WEIGHT_DEMIBOLD = 3
} EFontWeight;

typedef enum {
    EFONT_SLANT_NONE            = 0,
    EFONT_SLANT_ROMAN           = 1,
    EFONT_SLANT_ITALIC          = 2,
    EFONT_SLANT_OBLIQUE         = 3,
    EFONT_SLANT_REVERSE_ITALIC  = 4,
    EFONT_SLANT_REVERSE_OBLIQUE = 5,
    EFONT_SLANT_OTHER           = 6
} EFontSlant;

typedef enum {
    EFONT_WIDTH_NONE         = 0,
    EFONT_WIDTH_NORMAL       = 1,
    EFONT_WIDTH_CONDENSED    = 2,
    EFONT_WIDTH_NARROW       = 3,
    EFONT_WIDTH_DOUBLE_WIDE  = 4
} EFontWidth;

typedef enum {
    EFONT_STYLE_NONE       = 0,
    EFONT_STYLE_SERIF      = 1,
    EFONT_STYLE_SANS_SERIF = 2,
    EFONT_STYLE_INFORMAL   = 3,
    EFONT_STYLE_DECORATED  = 4,
} EFontStyle;

typedef enum {
    EFONT_SPACING_NONE         = 0,
    EFONT_SPACING_PROPORTIONAL = 1,
    EFONT_SPACING_MONOSPACED   = 2,
    EFONT_SPACING_CHAR_CELL    = 3
} EFontSpacing;

/* Note resolution and point size is given in
 * decipoints (10*) where 72.27 points equals 1 inch
 */
typedef struct epic_font_info {
    /*! font weight - EFontWeight */
    u_int32_t weight;
    /*! font slant - EFontSlant */
    u_int32_t slant;
    /*! font width - EFontWidth */
    u_int32_t width;
    /*! font style - EFontStyle */
    u_int32_t style;
    /*! font spacing - EFontSpacing */
    u_int32_t spacing;
    /*! font pixel type pixmap data stored with glyphs */
    u_int32_t pixel_type;
    /*! font size in pixels pixel_size=points* resolution_x/dpi */
    u_int32_t pixel_size;
    /*! font size in points */
    u_int32_t point_size;
    /*! resolution x (Points per inch) */
    u_int32_t resolution_x;
    /*! resolution y (Points per inch) */
    u_int32_t resolution_y;
    /* ! font descent */
    u_int32_t descent;
    /*! font ascent */
    u_int32_t ascent;
    /*  NOTE! add pad to 16 byte alignment if needed */
} EFontInfo;

/*! Font File format */
typedef struct epic_font_file {
    /*! EFNT file magic */
    char magic[4];
    /*! offset to the font foundry string (data) */
    u_int32_t foundry_offset;
    /*! offset to the font family string (data) */
    u_int32_t family_offset;
    /*! font info structure */
    EFontInfo font_info;
    /*! First glyph code */
    u_int32_t encoding_start; 
    /*! Last glyph code */
    u_int32_t encoding_stop;
    /*! default glyph (typically '?') */
    u_int32_t encoding_default;
    /*! string table start */
    u_int32_t string_table_start;
    /*! string table length */
    u_int32_t string_table_length;
    /*! offset table start */
    u_int32_t offset_table_start;
    /*! offset table length */
    u_int32_t offset_table_length;
    /*! glyph table start */
    u_int32_t glyph_table_start;
    /*! glyph table length */
    u_int32_t glyph_table_length;
    /*  NOTE! add pad to 16 byte alignment if needed */
    /*! start of data table area */
    u_int32_t data[0];
} EFontFile;

/* 16 byte glyph overhead */
typedef struct epic_glyph {
    /*! offset to glyph name in font file string table */
    u_int32_t name_offset;
    /*! glyph width in pixels */
    u_int16_t width;
    /*! glyph height in pixels */
    u_int16_t height;
    /*! x offset from origin */
    int16_t   xoffs;
    /*! y offset from origin */
    int16_t   yoffs;
    /* ! delta x value when drawing glyph */
    int16_t   dwx;
    /* ! delta y value when drawing glyph */
    int16_t   dwy;
    /* ! glyph pixel data */
    u_int32_t data[0];
} EGlyph;

/* The pixels are? stored as a EPIXEL_FMT_ALPHA (8 bits)
 * then we will render with
 *     fully opaque a=255  as foreground_color
 *     
 * and optionally render a=0 as background
 */
typedef struct epic_font {
    EOBJECT_MEMBERS(struct epic_font);
    /*! Font file name (strdup) */
    char* file_name;
    /*! File size - store size of font file */
    u_int32_t file_size;
    /*! Font foundry name (strdup) */
    char* foundry_name;
    /*! Font family name (stdup) */
    char* family_name;
    /*! Font info - converted to host order endian */
    EFontInfo font_info;
    /*!  Font Raw Data (when mapped or loaded) */
    EFontFile* font_file;
    /*!  Allocated font data (when using FontLoad) */
    EFontFile* font_data;
    /*!  Memory mapped data (when using FontMap)  */
    EFontFile* font_map;
} EFont;


#define EANIM_SKIP      0x01  // skip count DestPixelType pixels
#define EANIM_RGBA      0x02  // count * RGBA data
#define EANIM_COPY      0x03  // count * SourcePixelType pixels
#define EANIM_SHADOW    0x04  // count * ALPHA data
#define EANIM_INDIRECT  0x05  // count * indirect blocks
#define EANIM_BGRA      0x06  // count * BGRA data
#define EANIM_FILL      0x07  // fill count*color (ARGB format) pixels


typedef struct {
    u_int8_t    type;         /* Draw type */
    u_int8_t    itype;   /* Indirect Draw type */
    u_int16_t   nblocks; /* #linked blocks (mType==EPIC_ANIM_INDIRECT) */
    u_int32_t   count;   /* Number of pixels | offset INDIRECT */
} EAnimPixels;

typedef struct epic_window {
    EOBJECT_MEMBERS(struct epic_window);
    struct epic_backend* backend;        /* backend pointer if attached */
    u_int16_t mask;                      /* event mask */

    int x;
    int y;
    unsigned int width;
    unsigned int height;
} EWindow;

/* used both as event type and event mask */
#define EEVENT_KEY_PRESS        0x0001
#define EEVENT_KEY_RELEASE      0x0002
#define EEVENT_POINTER_MOTION   0x0004
#define EEVENT_BUTTON_PRESS     0x0008
#define EEVENT_BUTTON_RELEASE   0x0010
#define EEVENT_FOCUS_IN         0x0020
#define EEVENT_FOCUS_OUT        0x0040
#define EEVENT_FOCUS            0x0060

#define EEVENT_CLOSE            0x4000  /* window mangaer close event */
#define EEVENT_DESTROYED        0x8000  /* object destroyed notification */
#define EEVENT_ALL              0xFFFF

typedef struct epic_event {
    int type;                 /* event type */
    EWindow* window;          /* Event window */
    union {
	struct {
	    unsigned short button;
	    int x;
	    int y;
	    int z;
	} pointer;
	
	struct {
	    unsigned short mod;   /* modifier keys */
	    unsigned short code;  /* internal code */
	    unsigned short sym;   /* key code */
	} key;
    };
} EEvent;

/* backend */
typedef struct epic_backend {
    EOBJECT_MEMBERS(struct epic_backend);    
    /*! Number of pending events */
    int pending;
    /*! List of attached windows */
    EWindow*  window_list;
    /*! List of attached pixmaps */
    EPixmap*  pixmap_list;
    /*! Event handle             */
    EHANDLE_T event;
    /*! Backend callbacks        */
    struct epic_callbacks* cb;
} EBackend;

typedef enum {
    EDICT_ANY     = 0,
    EDICT_STRING  = 1,
    EDICT_INTEGER = 2,
    EDICT_FLOAT   = 3,
    EDICT_BINARY  = 4
} EDictType;

typedef struct {
    EDictType type;
    size_t    len;
    void*     ptr;
} EDictData;

/* dictionary */
typedef struct _epic_dict_entry {
    EDictData key;
    EDictData data;
    /*! Total size in mem area. */
    size_t    mem_size;
    /*! Data storage area. */
    u_int8_t mem[1];    
} EDictEntry;
   
typedef struct epic_dict {
    EOBJECT_MEMBERS(struct epic_dict);
    /*! Number allocated stuff in dict. */
    u_int32_t    entries;
    /*! Number used stuff in dict. */
    u_int32_t    used;
    /*! Bsearch possible predicate. */
    int          is_sorted;
    /*! Dictionary entries. */
    EDictEntry** entry;      
} EDict;
    

/* callbacks */
typedef struct epic_callbacks {
    int (*finish)(struct epic_backend*);
    int (*pic_attach)(struct epic_backend*, EPixmap*);
    int (*pic_detach)(struct epic_backend*, EPixmap*);
    int (*pic_draw)(struct epic_backend*, EPixmap*, EWindow*, int,
		    int, int, int, int, unsigned int, unsigned int);
    int (*win_attach)(struct epic_backend*, EWindow*);
    int (*win_detach)(struct epic_backend*, EWindow*);
    EHANDLE_T (*evt_attach)(struct epic_backend*);
    int (*evt_detach)(struct epic_backend*);
    int (*evt_read)(struct epic_backend*, EEvent*, u_int16_t mask);
    int (*adjust)(struct epic_backend*, EDict *);
    int (*win_swap)(struct epic_backend*, EWindow*);
} EPicCallbacks;

/* Direct memory operations */
extern void EDirectCopyRow(u_int8_t* src, int src_pt,
			   u_int8_t* dst, int dst_pt,
			   int width);

extern void EDirectCopyArea(u_int8_t* src, int src_wb, int src_pt,
			    u_int8_t* dst, int dst_wb, int dst_pt,
			    int width, int height);
extern void EDirectFillRow(u_int8_t* dst,int pt,int width,EPixel_t p);
extern void EDirectFillArea(u_int8_t* dst, int dst_wb, int dst_pt, 
			    int width, int height, EPixel_t fill);
extern void EDirectFillRowBlend(u_int8_t* dst, int pt, int width, EPixel_t p);
extern void EDirectFillAreaBlend(u_int8_t* dst, int dst_wb, int dst_pt,
				 int width, int height, EPixel_t p);
extern void EDirectShadeArea(u_int8_t* dst, int dst_wb, int dst_pt, 
			     int width, int height, 
			     EPixel_t Px0, EPixel_t Px1,
			     EPixel_t Py0, EPixel_t Py1);
extern void EDirectBlendRow(u_int8_t* src, int src_pt,
			    u_int8_t* dst, int dst_pt,
			    unsigned int width);
extern void EDirectBlendArea(u_int8_t* src, int src_wb, int src_pt,
			     u_int8_t* dst, int dst_wb, int dst_pt,
			     unsigned int width, unsigned int height);
extern void EDirectAlphaRow(u_int8_t* src, int src_pt,
			    u_int8_t* dst, int dst_pt,
			    u_int8_t a, int width);
extern void EDirectAlphaArea(u_int8_t* src, int src_wb, int src_pt,
			     u_int8_t* dst, int dst_wb, int dst_pt,
			     u_int8_t alpha, 
			     unsigned int width, unsigned int height);

extern void EDirectFadeRow(u_int8_t* src, int src_pt,
			   u_int8_t* dst, int dst_pt,
			   u_int8_t fade, unsigned int width);
extern void EDirectFadeArea(u_int8_t* src, int src_wb, int src_pt,
			    u_int8_t* dst, int dst_wb, int dst_pt,
			    u_int8_t fade, 
			    unsigned int width, unsigned int height);

extern void EDirectShadowRow(u_int8_t* src, int src_pt,
			     u_int8_t* dst, int dst_pt,
			     unsigned int width, int flags);
extern void EDirectShadowArea(u_int8_t* src, int src_wb, int src_pt,
			      u_int8_t* dst, int dst_wb, int dst_pt,
			      unsigned int width, unsigned int height,
			      int flags);

extern void EDirectAddColorArea(u_int8_t* src, int src_wb, int src_pt,
				u_int8_t* dst, int dst_wb, int dst_pt,
				u_int8_t fade, EPixel_t color, 
				unsigned int width, unsigned int height,
				int flags);
extern void EDirectAddColorRow(u_int8_t* src, int src_pt,
			       u_int8_t* dst, int dst_pt,
			       u_int8_t fade, EPixel_t color,
			       unsigned int width,int flags);



/* Drawing backend inteface */
extern char*     EBackendName(int i);
extern EBackend* EBackendCreate(char* name, EDict* param);

static inline void EBackendDestroy(EBackend* be) { EObjectUnRef(be); }
#define EBackendFinish(be) ((be)->cb->finish((be)))
#define EBackendPixmapAttach(be,pic) ((be)->cb->pic_attach((be),(pic)))
#define EBackendPixmapDetach(be,pic) ((be)->cb->pic_detach((be),(pic)))
#define EBackendPixmapDraw(be,pic,win,os,xs,ys,xd,yd,w,h)		\
    ((be)->cb->pic_draw((be),(pic),(win),(os),(xs),(ys),(xd),(yd),(w),(h)))
#define EBackendWindowAttach(be,win) ((be)->cb->win_attach((be),(win)))
#define EBackendWindowDetach(be,win) ((be)->cb->win_detach((be),(win)))
#define EBackendWindowSwap(be,win)   ((be)->cb->win_swap((be),(win)))
#define EBackendEventAttach(be)    ((be)->cb->evt_attach((be)))
#define EBackendEventDetach(be)    ((be)->cb->evt_detach((be)))
#define EBackendEventRead(be,e,mask) ((be)->cb->evt_read((be),(e),(mask)))

/* Window inteface */
extern EWindow* EWindowCreate(int x, int y,
			      unsigned int width,
			      unsigned int height);
static inline void EWindowDestroy(EWindow* win) { EObjectUnRef(win); }
extern int  EWindowAttach(EWindow* win, EBackend* be);
extern int  EWindowDetach(EWindow* win);
extern int  EWindowSwap(EWindow* win);
extern int  EBackendAdjust(EBackend *be, EDict *param);


extern void link_window(EBackend* backend, EWindow* window);
extern void unlink_window(EBackend* backend, EWindow* window);

/* GC interface */
extern EGc* EGcCreate(void);
extern EGc* EGcCopy(EGc* gc);
extern void EGcInit(EGc* gc);
static inline void EGcDestroy(EGc* gc) { EObjectUnRef(gc); }

/* Dict interface */
extern void   EDictInit(EDict* dict);
extern EDict* EDictCreate(void);
extern EDict* EDictCopy(EDict* dict);
extern void   EDictSort(EDict* dict);
static inline void EDictDestroy(EDict* dict) { EObjectUnRef(dict); }

extern int EDictLookupIx(EDict* dict, EDictData* key, EDictType data_type);
extern EDictEntry* EDictLookupEnt(EDict* dict,EDictData* key, 
				  EDictType data_type);
extern int EDictSetEnt(EDict* dict, EDictData* key, EDictData* data);
extern int EDictUnsetEnt(EDict* dict, EDictData* key, EDictType data_type);
extern int EDictUnset(EDict* dict, char* key);
extern int EDictLookupString(EDict* dict, char* key, char** value);
extern int EDictLookupInteger(EDict* dict, char* key, int* value);
extern int EDictLookupFloat(EDict* dict, char* key, double* value);
extern int EDictLookupBinary(EDict* dict, char* key,void** value,size_t* len);

extern int EDictSetString(EDict* dict, char* key, char* value);
extern int EDictSetInteger(EDict* dict, char* key, int value);
extern int EDictSetFloat(EDict* dict, char* key, double value);
extern int EDictSetBinary(EDict* dict, char* key, void* value, size_t len);

/* Font interface */
extern EFont* EFontCreate(void);
extern void EFontInit(EFont* font);
static inline void EFontDestroy(EFont* font) { EObjectUnRef(font); }
extern EFont* EFontOpen(char* file);

extern int  EFontLoad(EFont* font);
extern int  EFontUnload(EFont* font);
extern int  EFontMap(EFont* font);
extern int  EFontUnmap(EFont* font);
#define EFontIsMapped(font) (font->font_map != NULL)
#define EFontIsLoaded(font) (font->font_data != NULL)

extern void EFontDrawGlyph(EGc*gc, EPixmap* dst, 
			   int* x, int* y, int c);
extern void EFontDrawString(EGc* gc, EPixmap* dst,
			    int* x, int* y, char* string);
extern void EFontDrawUTF8(EGc* gc, EPixmap* dst,
			  int* x, int* y, char* string);
extern void EFontDrawWideString(EGc* gc, EPixmap* dst,
				int* x, int* y, wchar_t* string);

/* Pixmap animation interface */
extern void EAnimCopyFrame(EGc* gc, EPixmap* pic, int x, int y,
			   int width, int height, int src_pt,
			   EAnimPixels* base, EAnimPixels* current);
extern void EAnimDrawFrame(EGc* gc, EPixmap* pic, int x, int y,
			   int width, int height, int src_pt,
			   EAnimPixels* base, EAnimPixels* current);


/* Pixmap inteface */
extern EPixmap* EPixmapCreate(unsigned int width, unsigned int height,
			      int pixel_type);
extern int  EPixmapAttach(EPixmap* pic, EBackend* be);
extern int  EPixmapDetach(EPixmap* pic);
static inline void EPixmapDestroy(EPixmap* pic)  { EObjectUnRef(pic); }

extern void link_pixmap(EBackend* backend, EPixmap* pixmap);
extern void unlink_pixmap(EBackend* backend, EPixmap* pixmap);

extern void EPixmapSetClip(EPixmap* pic, ERect_t* clip);
extern void EPixmapFill(EPixmap* pic, EPixel_t p);
extern void EPixmapCopy(EPixmap* src, EPixmap* dst);

/* Render attached pic on attached win */
extern int EPixmapDrawWindow(EPixmap* pic, EWindow* win, 
			     int off_screen,
			     int x_src, int y_src, int x_dst, int y_dst, 
			     unsigned int width, unsigned int height);

/* Pixels */

extern void EPixmapPutPixel(EPixmap* pic, int x, int y, int flags, EPixel_t px);
extern void EPixmapPutPixels(EPixmap* pic, int x, int y,
			     unsigned int width,  unsigned int height,
			     int pixel_type, int flags,
			     void* data, unsigned int len);
extern EPixel_t EPixmapGetPixel(EPixmap* pic, int x, int y);


extern EPixel_t EPixmapReadPixel(int pixel_type, u_int8_t* src);
extern void EPixmapWritePixel(int pixel_type, EPixel_t p, u_int8_t* dst);

extern void EPixmapDrawPoint(EPixmap* pic, EGc* gc, int x, int y);

/* Copy, this will copy src to dest with blending */
extern void EPixmapCopyArea(EPixmap* src, EPixmap* dst,
			    int x_src, int y_src, int x_dst, int y_dst,
			    unsigned int width, unsigned int height,
			    int flags);


/* Like copy but will use a fixed alpha to blend pixmaps */
extern void EPixmapAlphaArea(EPixmap* src, EPixmap* dst,
			     u_int8_t alpha,
			     int x_src, int y_src, int x_dst, int y_dst,
			     unsigned int width, unsigned int height);

/* Like copy but will use a fader multiplier for alpha to blend pixmaps */
extern void EPixmapFadeArea(EPixmap* src,EPixmap* dst, u_int8_t fade,
			    int x_src, int y_src, int x_dst, int y_dst,
			    unsigned int width, unsigned int height);

extern void EPixmapShadowArea(EPixmap* src,EPixmap* dst,
			      int x_src, int y_src, int x_dst, int y_dst,
			      unsigned int width, unsigned int height,
			      int flags);

extern void EPixmapAddColorArea(EPixmap* src,EPixmap* dst, u_int8_t fade,
				EPixel_t color,
				int x_src, int y_src, int x_dst, int y_dst,
				unsigned int width, unsigned int height, 
				int flags);


extern void EPixmapFilterArea(EPixmap* src, EPixmap* dst, EFilter_t* flt,
			      int x_src, int y_src, int x_dst, int y_dst,
			      unsigned int width, unsigned int height,
			      int flags);

extern void EPixmapRotateArea(EPixmap* src, EPixmap* dst, float angle,
			      int x_src, int y_src, int xc_src, int yc_src,
			      int xc_dst, int yc_dst,
			      unsigned int width, unsigned int height, 
			      int flags);

extern void EPixmapScrollLeft(EPixmap* src, EPixmap* dst,
			      int rotate, unsigned int amount, EPixel_t fill);
extern void EPixmapScrollRight(EPixmap* src, EPixmap* dst, 
			       int rotate, unsigned int amount, EPixel_t fill);
extern void EPixmapScrollUp(EPixmap* src, EPixmap* dst, 
			    int rotate, unsigned int amount, EPixel_t fill);
extern void EPixmapScrollDown(EPixmap* src, EPixmap* dst, 
			      int rotate, unsigned int amount, EPixel_t fill);

extern void EPixmapScroll(EPixmap* src, EPixmap* dst, 
			  int horizontal, int vertical, 
			  int rotate, EPixel_t fill);

extern void EPixmapDrawTriangle(EPixmap* pixmap, EGc* gc,
				int x0, int y0,
				int x1, int y1,
				int x2, int y2);

/* Rectangles */
extern void EPixmapDrawRectangle(EPixmap* pixmap, EGc* gc,
				 int x, int y,
				 unsigned int width, 
				 unsigned int height);

/* Line */
extern void EPixmapDrawLine(EPixmap* pixmap, EGc* gc,
			    int x0, int y0, 
			    int x1, int y1);
extern void EPixmapDrawTwinLine(EPixmap* pixmap, EGc* gc,
				int x0, int y0, 
				int x1, int y1,
				int x2, int y2, 
				int x3, int y3);

extern void EPixmapTexLine(EPixmap* pic, EGc* gc,
			   int x0, int y0, 
			   int x1, int y1,
			   EPixmap* tex,
			   int tx0, int tx1, float ty);

/* Ellipse */
extern void EPixmapDrawEllipse(EPixmap* pic, EGc* gc,
			       int x0, int y0,
			       unsigned int width, unsigned int height);
/*
 *  BITMAPS
 */

extern EBitmap* EBitmapCreate(unsigned int width, unsigned int height);
extern void EBitmapCopy(EBitmap* src, EBitmap* dst);
static inline void EBitmapDestroy(EBitmap* bmp) { EObjectUnRef(bmp); }

extern void EBitmapFill(EBitmap* bmp, u_int8_t bit);
extern void EBitmapPutBit(EBitmap* bmp, int x, int y, u_int8_t bit);
extern void EBitmapPutBits(EBitmap* bmp, int x, int y,
			   unsigned int width,  unsigned int height,
			   void* data, unsigned int len);
extern u_int8_t EBitmapGetBit(EBitmap* bmp, int x, int y);

extern int EBitmapDraw(EBitmap* src, EPixmap* dst,
		       int x_src, int y_src, int x_dst, int y_dst,
		       unsigned int width, unsigned int height,
		       EPixel_t fg, EPixel_t bg);

/* Copy, this will copy src to dest with blending */
extern int ebitmap_copy_area(EBitmap* src, EBitmap* dst,
			     int x_src, int y_src, int x_dst, int y_dst,
			     unsigned int width, unsigned int height);

extern void ebitmap_scroll_left(EBitmap* src, EBitmap* dst,
				int rotate, unsigned int amount, 
				u_int8_t fill);
extern void ebitmap_scroll_right(EBitmap* src, EBitmap* dst, 
				 int rotate, unsigned int amount, 
				 u_int8_t fill);
extern void ebitmap_scroll_up(EBitmap* src, EBitmap* dst, 
			      int rotate, unsigned int amount, 
			      u_int8_t fill);
extern void ebitmap_scroll_down(EBitmap* src, EBitmap* dst, 
				int rotate, unsigned int amount, 
				u_int8_t fill);

extern void ebitmap_scroll(EBitmap* src, EBitmap* dst, 
			   int horizontal, int vertical, 
			   int rotate, u_int8_t fill);

/* Rectangles */
extern int ebitmap_draw_rectangle(EBitmap* bmp, int x, int y,
				  unsigned int width, 
				  unsigned int height, u_int8_t bit);
extern int ebitmap_fill_rectangle(EBitmap* bmp, int x, int y,
				  unsigned int width, 
				  unsigned int height, u_int8_t bit);

/* Line */
extern int ebitmap_draw_line(EBitmap* bmp,
			     int x0, int y0, 
			     int x1, int y1, 
			     int flags,
			     u_int8_t bit);

/* Ellipse */
extern int ebitmap_draw_ellipse(EBitmap* bmp, int x0, int y0,
				unsigned int a, unsigned int b, u_int8_t bit);
extern int ebitmap_fill_ellipse(EBitmap* bmp, int x0, int y0,
				unsigned int a, unsigned int b, u_int8_t bit);


#ifdef __cplusplus
}
#endif

#endif
