/*
 * EPIC Pixmap functions
 *
 */
#include <math.h>
#include "epic.h"
#include <wchar.h>
#include <stdarg.h>
#include "epic_simd.h"

EGc default_gc =
{
    {0, 0},
    0,            // Not on heap
    0,            // No references
    NULL,         // No release function
    NULL,         // Opaque
    EGC_TYPE,     // Type
    NULL,         // Next

    EPIC_FILL_STYLE_SOLID,  // fill_style
    EPIXEL_RED,             // fill_color
    NULL,                   // fill_texture

    EPIC_LINE_STYLE_SOLID,  // line_style
    EPIC_JOIN_STYLE_MITER,  // line_join_style
    EPIC_CAP_STYLE_NONE,    // line_cap_style
    1,                      // line_width
    NULL,                   // line_texture

    EPIC_BORDER_STYLE_SOLID, // border_style
    EPIC_JOIN_STYLE_MITER,   // border_join_style
    EPIC_CAP_STYLE_NONE,     // border_cap_style
    EPIXEL_BLACK,            // border_color
    0,                       // border_width
    NULL,                    // border_texture

    /* Color */
    EPIXEL_BLUE,             // forground_color
    EPIXEL_GREEN,            // background_color
    // Alpha
    255,                     // alpha factor = 1.0
    // Text
    NULL,                    // Font used for text drawing
    // Glyphs
    0,                       // glyph_delta_x
    0,                       // glyph_delta_y
    0,                       // glyph_fixed_width
    0                        // glyph_dot_kern
};

#ifdef debug
#include <assert.h>
#define ASSERT_RECTANGLE(r, x, y, w, h) \
    assert(EPointXYInRect((x), (y), r) &&	\
	   EPointXYInRect((x+w-1), (y+h-1), r));
#else
#define ASSERT_RECTANGLE(r, x, y, w, h)
#endif


static void draw_triangle(EPixmap* pic, EGc* gc,
			  int x, int y, 
			  int x1, int y1, int x2, int y2);

extern void draw_line_plain(EPixmap* pic, int x1, int y1, int x2, int y2,
			    int flags, EPixel_t fg);
extern void draw_line_twin(EPixmap* pic, 
			   int x1, int y1, int x2, int y2,
			   int x3, int y3, int x4, int y4,
			   int flags, EPixel_t fg);
#if 0
static void draw_line_plain_old(EPixmap* pic,
				int x0, int y0, 
				int x1, int y1, 
				int flags, EPixel_t fg);
#endif
static void draw_line_thick(EPixmap* pic,
			    int x0, int y0, 
			    int x1, int y1, int line_width,
			    int flags, EPixel_t fg);
#if 0
static void draw_line_aalias_old(EPixmap* pic,
				 int x0, int y0, 
				 int x1, int y1, 
				 int flags, EPixel_t fg);
#endif

static void draw_line(EPixmap* pic,
		      int x0, int y0, 
		      int x1, int y1, 
		      unsigned int line_width,
		      int flags, 
		      EPixel_t p);

void draw_line_horizontal(EPixmap* pic, int x1, int x2, int y, 
			  int flags, EPixel_t fg);

int epic_debug_mask = 0;

void epicEmitError(char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stderr, "%s:%d: ", file, line); 
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
}

void epic_init(int simd)
{
    ESimdInit(simd);
}

void epic_debug(int debug_mask)
{
    epic_debug_mask = debug_mask;
}

#define INLINE_COPY_SIZE 32
#define INLINE_FILL_SIZE 16

#define ECOPY(src,dst,n) do { \
	if ((n) < INLINE_COPY_SIZE) {		\
	    u_int8_t* __srcp = (src);		\
	    u_int8_t* __dstp = (dst);		\
	    unsigned int __n = (n);		\
	    while(__n--) *__dstp++ = *__srcp++;	\
	} \
	else { \
	    ESimdCopy((src),(dst),(n)); \
	} \
    } while(0)

#define EFILL(dst,v,n) do {			\
	if ((n) < INLINE_FILL_SIZE) {		\
	    u_int8_t* __dstp = (dst);		\
	    unsigned int __n = (n);		\
	    u_int32_t __v = (v);		\
	    u_int8_t __v3 = ((u_int8_t*)&__v)[3];	\
	    u_int8_t __v2 = ((u_int8_t*)&__v)[2];	\
	    u_int8_t __v1 = ((u_int8_t*)&__v)[1];	\
	    u_int8_t __v0 = ((u_int8_t*)&__v)[0];	\
	    while(__n--) {			\
		*__dstp++ = __v0;		\
		*__dstp++ = __v1;		\
		*__dstp++ = __v2;		\
		*__dstp++ = __v3;		\
	    }					\
	}					\
	else {					\
	    ESimdFill32((dst),(v),(n));		\
	}					\
    } while(0)

/*  Set 24 bit value
 *    the 24 bit occupy the high 24 bits, low 8 MUST be 0 
 */
static void EFill24(u_int8_t* ptr, u_int32_t c24, size_t n)
{
    /* Check alignment and 32 bit optimization */
    if (((((unsigned long)ptr) & 0x3) == 0) && (n > 3)) {
	u_int32_t* ptr32 = (u_int32_t*) ptr;
	u_int32_t v3 = c24 | (c24 >> 24);
	u_int32_t v2 = (c24 << 8) | (c24 >> 16);
	u_int32_t v1 = (c24 >> 8) | (c24 << 16);
	while(n > 3) {
	    *ptr32++ = v3;
	    *ptr32++ = v2;
	    *ptr32++ = v1;
	    n -= 3;
	}
    }
    if (n) {
	u_int8_t a = c24 >> 24;
	u_int8_t b = c24 >> 16;
	u_int8_t c = c24 >> 8;
	while(n--) {
	    *ptr++ = a;
	    *ptr++ = b;
	    *ptr++ = c;
	}
    }
}


static void EFill16(u_int8_t* ptr, u_int16_t c16, size_t n)
{
    if (n >= 2) {  /* FIXME better treshold value */
	size_t n32 = n / 2;
	EFILL(ptr, (c16 << 16) | c16, n32);
	ptr += n32*2;
	n %= 2;
    }
    if (n) {
	if ((((unsigned long) ptr) & 0x1) == 0) {
	    u_int16_t* ptr16 = (u_int16_t*) ptr;
	    while(n--)
		*ptr16++ = c16;
	}
	else {
	    u_int8_t v1 = c16 >> 8;
	    u_int8_t v0 = c16 & 0xff;

	    while(n--) {
		*ptr++ = v1;
		*ptr++ = v0;
	    }
	}
    }
}

static void EFill8(u_int8_t* ptr, u_int8_t c8, size_t n)
{
    if (n >= 4) { /* FIXME: better treshold value */
	size_t n32 = n/4;
	u_int32_t c32 = (c8 << 8) | c8;
	c32 = (c32 << 16) | c32;
	EFILL(ptr, c32, n32);
	ptr += n32*4;
	n %= 4;
    }
    while(n--)
	*ptr++ = c8;
}

/* copy pixel area */
void EDirectCopyArea(u_int8_t* src, int src_wb, int src_pt,
		     u_int8_t* dst, int dst_wb, int dst_pt,
		     int width, int height)
{
    if ((dst < src) || (height <= 1)) {
	if (src_pt == dst_pt) {
	    unsigned int src_psz = EPIXEL_SIZE(src_pt);
	    unsigned int n = src_psz*width;
	    
	    while(height--) {
		ECOPY(src, dst, n);
		// memmove(dst, src, n);
		src += src_wb;
		dst += dst_wb;
	    }
	}
	else {
	    int src_psz = EPIXEL_SIZE(src_pt);
	    int dst_psz = EPIXEL_SIZE(dst_pt);
	    EPixelUnpack_t unpack_src = EPixelUnpackFunc(src_pt);
	    EPixelPack_t   pack_dst   = EPixelPackFunc(dst_pt);

	    while(height--) {
		u_int8_t* src1 = src;
		u_int8_t* dst1 = dst;
		unsigned width1 = width;
		while(width1--) {
		    EPixel_t p = unpack_src(src1);
		    pack_dst(p, dst1);
		    src1 += src_psz;
		    dst1 += dst_psz;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	}
    }
    else {
	src += (src_wb*height);
	dst += (dst_wb*height);

	if (src_pt == dst_pt) {
	    unsigned int n = EPIXEL_SIZE(src_pt)*width;
	    
	    while(height--) {
		src -= src_wb;
		dst -= dst_wb;
		ECOPY(src, dst, n);
		// memmove(dst, src, n);
	    }
	}
	else {
	    int src_psz = EPIXEL_SIZE(src_pt);
	    int dst_psz = EPIXEL_SIZE(dst_pt);
	    EPixelUnpack_t unpack_src = EPixelUnpackFunc(src_pt);
	    EPixelPack_t   pack_dst   = EPixelPackFunc(dst_pt);

	    while(height--) {
		u_int8_t* src1 = src-src_wb;
		u_int8_t* dst1 = dst-dst_wb;
		unsigned int width1 = width;
		src = src1;
		dst = dst1;
		while(width1--) {
		    EPixel_t p = unpack_src(src1);
		    pack_dst(p, dst1);
		    src1 += src_psz;
		    dst1 += dst_psz;
		}
	    }
	}
    }
}

void EDirectCopyRow(u_int8_t* src, int src_pt,
		    u_int8_t* dst, int dst_pt,
		    int width)
{
    EDirectCopyArea(src, 0, src_pt, dst, 0, dst_pt, width, 1);
}


/* fill pixel area (FIXME acceleration) */
void EDirectFillArea(u_int8_t* dst, int dst_wb, int dst_pt, 
		     int width, int height, EPixel_t fill)
{
    unsigned int psz = EPIXEL_SIZE(dst_pt);
    u_int32_t    cv = 0;

    EPixelPack(dst_pt, fill, (u_int8_t*)&cv);
    switch(psz) {
    case 1: 
	cv = (cv >> 24);
	while(height--) {
	    EFill8(dst, cv, width);
	    dst += dst_wb;
	}
	break;
    case 2: 
	cv = (cv >> 16);
	while(height--) {
	    EFill16(dst, cv, width);
	    dst += dst_wb;
	}
	break;
    case 3:
	while(height--) {
	    EFill24(dst, cv, width);
	    dst += dst_wb;
	}
	break;
    case 4: 
	while(height--) {
	    EFILL(dst, cv, width);
	    dst += dst_wb;
	}
	break;
    default: 
	break;
    }
}

void EDirectFillRow(u_int8_t* dst,int dst_pt,int width,EPixel_t fill)
{
    EDirectFillArea(dst, 0, dst_pt, width, 1, fill);
}

/* blend dst with ARGB/ABGR color (dst alpha is untouched) 
 * caller MUST swap B/R for ABGR format
*/

/* fill pixel row with blending */
void EDirectFillAreaBlend(u_int8_t* dst, int dst_wb, int dst_pt, 
			  int width, int height, EPixel_t p)
{
    int psz;
    EPixelUnpack_t unpack_dst;
    EPixelPack_t   pack_dst;

    switch(dst_pt) {
    case EPIXEL_TYPE_R8G8B8:
	ESimdFillAreaBlendRGB24(dst,dst_wb,width,height,p);
	break;
    case EPIXEL_TYPE_B8G8R8:
	ESimdFillAreaBlendRGB24(dst,dst_wb,width,height,epixel_swap(p));
	break;
    case EPIXEL_TYPE_A8R8G8B8:
    case EPIXEL_TYPE_X8R8G8B8:
	ESimdFillAreaBlendARGB32(dst,dst_wb,width,height,p);
	break;
    case EPIXEL_TYPE_A8B8G8R8:
    case EPIXEL_TYPE_X8B8G8R8:
	ESimdFillAreaBlendARGB32(dst,dst_wb,width,height,epixel_swap(p));
	break;
    case EPIXEL_TYPE_R8G8B8A8:
    case EPIXEL_TYPE_R8G8B8X8:
	ESimdFillAreaBlendRGBA32(dst,dst_wb,width,height,p);
	break;
    case EPIXEL_TYPE_B8G8R8A8:
    case EPIXEL_TYPE_B8G8R8X8:
	ESimdFillAreaBlendRGBA32(dst,dst_wb,width,height,epixel_swap(p));
	break;
    default:
	psz = EPIXEL_SIZE(dst_pt);
	unpack_dst = EPixelUnpackFunc(dst_pt);
	pack_dst   = EPixelPackFunc(dst_pt);

	while(height--) {
	    u_int8_t* dst1 = dst;
	    int width1 = width;
	    while(width1--) {
		EPixel_t d = unpack_dst(dst1);
		d = EPixelBlend(p.a, p, d);
		pack_dst(d, dst1);
		dst1 += psz;
	    }
	    dst += dst_wb;
	}
	break;
    }
}

void EDirectFillRowBlend(u_int8_t* dst, int dst_pt, int width, EPixel_t p)
{
    EDirectFillAreaBlend(dst, 0, dst_pt, width, 1, p);
}

/*! Experimental */
void EDirectShadeArea(u_int8_t* dst, int dst_wb, int dst_pt, 
		      int width, int height, 
		      EPixel_t Px0, EPixel_t Px1,
		      EPixel_t Py0, EPixel_t Py1)
{
    int psz = EPIXEL_SIZE(dst_pt);
    int height1 = height;
    int y = 0;
    EPixelUnpack_t unpack_dst = EPixelUnpackFunc(dst_pt);
    EPixelPack_t   pack_dst = EPixelPackFunc(dst_pt);
    
    while(height1--) {
	u_int8_t* dst1 = dst;
	int width1 = width;
	int x = 0;
	while(width1--) {
	    EPixel_t d = unpack_dst(dst1);
	    EPixel_t c0;
	    EPixel_t c1;
	    EPixel_t c2;

	    c0 = EPixelBlend((x*255)/width, Px0, Px1);
	    c1 = EPixelBlend((y*255)/height, Py0, Py1);
	    c2 = EPixelSum(c0,c1);

	    d = EPixelBlend(c2.a, c2, d);
	    pack_dst(d, dst1);
	    dst1 += psz;
	    x++;
	}
	dst += dst_wb;
	y++;
    }
}



void EDirectBlendArea(u_int8_t* src, int src_wb, int src_pt,
		      u_int8_t* dst, int dst_wb, int dst_pt,
		      unsigned int width, 
		      unsigned int height)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    EPixelUnpack_t unpack_dst;
    EPixelUnpack_t unpack_src;
    EPixelPack_t   pack_dst;

    if (!EPIXEL_HAS_ALPHA(src_pt)) {
	EDirectCopyArea(src, src_wb, src_pt, dst, dst_wb, dst_pt, 
			width, height);
	return;
    }

    if ((dst_pt == src_pt) && (width>=8)) {
	switch(src_pt) {
	case EPIXEL_TYPE_R8G8B8A8:
	case EPIXEL_TYPE_B8G8R8A8:
	    ESimdBlendAreaRGBA32(src, src_wb, dst, dst_wb, width, height);
	    return;
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_A8B8G8R8:
	    ESimdBlendAreaARGB32(src, src_wb, dst, dst_wb, width, height);
	    return;
	default:
	    break;
	}
    }

    src_psz = EPIXEL_SIZE(src_pt);
    dst_psz = EPIXEL_SIZE(dst_pt);
    unpack_src = EPixelUnpackFunc(src_pt);
    unpack_dst = EPixelUnpackFunc(dst_pt);
    pack_dst   = EPixelPackFunc(dst_pt);

    while(height--) {
	u_int8_t* dst1 = dst;
	u_int8_t* src1 = src;
	unsigned int width1 = width;

	if (src_pt == dst_pt) {
	    switch(src_pt) {
	    case EPIXEL_TYPE_A8R8G8B8:
	    case EPIXEL_TYPE_A8B8G8R8: 
		EDirectBlendRowARGB32(src1, dst1, width1);
		break;
	    case EPIXEL_TYPE_B8G8R8A8:
	    case EPIXEL_TYPE_R8G8B8A8:
		EDirectBlendRowRGBA32(src1, dst1, width1);
		break;
	    default:
		while(width1--) {
		    EPixel_t d = unpack_dst(dst1);
		    EPixel_t s = unpack_src(src1);
		    d = EPixelBlend(s.a, s, d);
		    pack_dst(d, dst1);
		    src1 += src_psz;
		    dst1 += dst_psz;
		}
		break;
	    }
	}
	else {
	    while(width1--) {
		EPixel_t d = unpack_dst(dst1);
		EPixel_t s = unpack_src(src1);
		d = EPixelBlend(s.a, s, d);
		pack_dst(d, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void EDirectBlendRow(u_int8_t* src, int src_pt,
		     u_int8_t* dst, int dst_pt,
		     unsigned int width)
{
    EDirectBlendArea(src, 0, src_pt, dst, 0, dst_pt, width, 1);
}


void EDirectSumArea(u_int8_t* src, int src_wb, int src_pt,
		    u_int8_t* dst, int dst_wb, int dst_pt,
		    unsigned int width, 
		    unsigned int height)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    EPixelUnpack_t unpack_dst;
    EPixelUnpack_t unpack_src;
    EPixelPack_t   pack_dst;

    src_psz = EPIXEL_SIZE(src_pt);
    dst_psz = EPIXEL_SIZE(dst_pt);
    unpack_src = EPixelUnpackFunc(src_pt);
    unpack_dst = EPixelUnpackFunc(dst_pt);
    pack_dst   = EPixelPackFunc(dst_pt);

    while(height--) {
	u_int8_t* dst1 = dst;
	u_int8_t* src1 = src;
	unsigned int width1 = width;

	if (src_pt == dst_pt)
	    EDirectSumRow8(src1, dst1, width*src_psz);
	else {
	    while(width1--) {
		EPixel_t d = unpack_dst(dst1);
		EPixel_t s = unpack_src(src1);
		d = EPixelSum(s, d);
		pack_dst(d, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void EDirectSumRow(u_int8_t* src, int src_pt,
		   u_int8_t* dst, int dst_pt,
		   unsigned int width)
{
    EDirectSumArea(src, 0, src_pt, dst, 0, dst_pt, width, 1);
}



void EDirectAlphaArea(u_int8_t* src, int src_wb, int src_pt,
		      u_int8_t* dst, int dst_wb, int dst_pt,
		      u_int8_t alpha, unsigned int width, unsigned int height)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    EPixelUnpack_t unpack_dst;
    EPixelUnpack_t unpack_src;
    EPixelPack_t   pack_dst;

    if ((dst_pt == src_pt) && (width>=8)) {
	switch(src_pt) {
	case EPIXEL_TYPE_R8G8B8A8:
	case EPIXEL_TYPE_B8G8R8A8:
	case EPIXEL_TYPE_R8G8B8X8:
	case EPIXEL_TYPE_B8G8R8X8:
	    ESimdAlphaAreaRGBA32(src,src_wb,dst,dst_wb,alpha,width,height);
	    return;
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_A8B8G8R8:
	case EPIXEL_TYPE_X8R8G8B8:
	case EPIXEL_TYPE_X8B8G8R8:
	    ESimdAlphaAreaARGB32(src,src_wb,dst,dst_wb,alpha,width,height);
	    return;
	default:
	    goto generic_area;
	}
    }

generic_area:
    if (src_pt == dst_pt) {
	switch(src_pt) {
	case EPIXEL_TYPE_X8R8G8B8:
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_X8B8G8R8:
	case EPIXEL_TYPE_A8B8G8R8:
	    while(height--) {
		EDirectAlphaRowARGB32(src,dst,alpha,width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	case EPIXEL_TYPE_B8G8R8X8:
	case EPIXEL_TYPE_B8G8R8A8:
	case EPIXEL_TYPE_R8G8B8X8:
	case EPIXEL_TYPE_R8G8B8A8:
	    while(height--) {
		EDirectAlphaRowRGBA32(src,dst,alpha,width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	case EPIXEL_TYPE_R8G8B8:
	case EPIXEL_TYPE_B8G8R8:
	    while(height--) {
		EDirectAlphaRowRGB24(src,dst,alpha,width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	default:
	    break;
	}
    }

    src_psz = EPIXEL_SIZE(src_pt);
    dst_psz = EPIXEL_SIZE(dst_pt);
    unpack_src = EPixelUnpackFunc(src_pt);
    unpack_dst = EPixelUnpackFunc(dst_pt);
    pack_dst   = EPixelPackFunc(dst_pt);

    while(height--) {
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;

	while(width1--) {
	    EPixel_t d = unpack_dst(dst1);
	    EPixel_t s = unpack_src(src1);
	    d = EPixelBlend(alpha, s, d);
	    pack_dst(d, dst1);
	    src1 += src_psz;
	    dst1 += dst_psz;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void EDirectAlphaRow(u_int8_t* src, int src_pt,
		     u_int8_t* dst, int dst_pt,
		     u_int8_t a, int width)
{
    EDirectAlphaArea(src, 0, src_pt, dst, 0, dst_pt, a, width, 1);
}


void EDirectFadeArea(u_int8_t* src, int src_wb, int src_pt,
		     u_int8_t* dst, int dst_wb, int dst_pt,
		     u_int8_t fade, unsigned int width, unsigned int height)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    EPixelUnpack_t unpack_dst;
    EPixelUnpack_t unpack_src;
    EPixelPack_t   pack_dst;

    if (fade == ALPHA_FACTOR_0) 
	return;
    if (fade == ALPHA_FACTOR_1)
	EDirectBlendArea(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
    else if ((dst_pt == src_pt) && (width>=8)) {
	switch(src_pt) {
	case EPIXEL_TYPE_R8G8B8A8:
	case EPIXEL_TYPE_B8G8R8A8:
	    ESimdFadeAreaRGBA32(src, src_wb, dst, dst_wb, fade, width, height);
	    return;
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_A8B8G8R8:
	    ESimdFadeAreaARGB32(src, src_wb, dst, dst_wb, fade, width, height);
	    return;
	default:
	    break;
	}
    }

    if (src_pt == dst_pt) {
	switch(src_pt) {
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_A8B8G8R8:
	    while(height--) {
		EDirectFadeRowARGB32(src, dst, fade, width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	case EPIXEL_TYPE_B8G8R8A8:
	case EPIXEL_TYPE_R8G8B8A8:
	    while(height--) {
		EDirectFadeRowRGBA32(src, dst, fade, width);
		src += src_wb;
		dst += dst_wb;
	    }
	    return;
	default:
	    goto generic_area;
	}
    }
    else if (src_pt == EPIXEL_TYPE_A8) {
	// source r=g=b=0
	switch(dst_pt) {
	case EPIXEL_TYPE_B8G8R8:
	case EPIXEL_TYPE_R8G8B8:
	    while(height--) {
		u_int8_t* src1 = src;
		u_int8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    u_int8_t a = (*src1*fade >> 8);
		    dst1[0]=eblend(a,0,dst1[0]);
		    dst1[1]=eblend(a,0,dst1[1]);
		    dst1[2]=eblend(a,0,dst1[2]);
		    src1++;
		    dst1 += 3;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    break;
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_A8B8G8R8:
	    while(height--) {
		u_int8_t* src1 = src;
		u_int8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    u_int8_t a = ((*src1)*fade >> 8);
		    dst1[0]=eblend(a,0,dst1[0]); // NEW
		    dst1[1]=eblend(a,0,dst1[1]);
		    dst1[2]=eblend(a,0,dst1[2]);
		    dst1[3]=eblend(a,0,dst1[3]);
		    src1++;
		    dst1 += 4;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    break;
	case EPIXEL_TYPE_B8G8R8A8:
	case EPIXEL_TYPE_R8G8B8A8:
	    while(height--) {
		u_int8_t* src1 = src;
		u_int8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    u_int8_t a = ((*src1)*fade >> 8);
		    dst1[0]=eblend(a,0,dst1[0]);
		    dst1[1]=eblend(a,0,dst1[1]);
		    dst1[2]=eblend(a,0,dst1[2]);
		    dst1[3]=eblend(a,0,dst1[3]); // NEW
		    src1++;
		    dst1 += 4;
		}
		src += src_wb;
		dst += dst_wb;
	    }
	    break;
	default:
	    dst_psz = EPIXEL_SIZE(dst_pt);
	    unpack_dst = EPixelUnpackFunc(dst_pt);
	    pack_dst   = EPixelPackFunc(dst_pt);
	    while(height--) {
		u_int8_t* src1 = src;
		u_int8_t* dst1 = dst;
		unsigned int width1 = width;
		while(width1--) {
		    EPixel_t d = unpack_dst(dst1);
		    u_int8_t a = (*src1*fade >> 8);
		    d.r = eblend(a,0,d.r);
		    d.g = eblend(a,0,d.g);
		    d.b = eblend(a,0,d.b);
		    d.a = eblend(a,0,d.a);  // NEW
		    pack_dst(d, dst1);
		    src1++;
		    dst1 += dst_psz;
		}
	    }
	}
	return;
    }

generic_area:
    src_psz = EPIXEL_SIZE(src_pt);
    dst_psz = EPIXEL_SIZE(dst_pt);
    unpack_src = EPixelUnpackFunc(src_pt);
    unpack_dst = EPixelUnpackFunc(dst_pt);
    pack_dst   = EPixelPackFunc(dst_pt);

    while(height--) {
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;
	unsigned int width1 = width;
	
	while(width1--) {
	    EPixel_t d = unpack_dst(dst1);
	    EPixel_t s = unpack_src(src1);
	    u_int8_t a = (s.a)*fade >> 8;
	    d.r = eblend(a,s.r,d.r);
	    d.g = eblend(a,s.g,d.g);
	    d.b = eblend(a,s.b,d.b);
	    d.a = eblend(a,s.a,d.a);  // NEW
	    pack_dst(d, dst1);
	    src1 += src_psz;
	    dst1 += dst_psz;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

void EDirectFadeRow(u_int8_t* src, int src_pt,
		    u_int8_t* dst, int dst_pt,
		    u_int8_t fade, unsigned int width)
{
    if (fade == ALPHA_FACTOR_0)
	return;
    else if (fade == ALPHA_FACTOR_1)
	EDirectBlendArea(src, 0, src_pt, dst, 0, dst_pt, width, 1);
    else
	EDirectFadeArea(src, 0, src_pt, dst, 0, dst_pt, fade, width, 1);
}



void EDirectShadowRow(u_int8_t* src, int src_pt,
		      u_int8_t* dst, int dst_pt,
		      unsigned int width, int flags)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    EPixelUnpack_t unpack_dst;
    EPixelUnpack_t unpack_src;
    EPixelPack_t   pack_dst;

    src_psz = EPIXEL_SIZE(src_pt);
    dst_psz = EPIXEL_SIZE(dst_pt);
    unpack_src = EPixelUnpackFunc(src_pt);
    unpack_dst = EPixelUnpackFunc(dst_pt);
    pack_dst   = EPixelPackFunc(dst_pt);

    if (!(flags & EFLAG_BLEND)) {
	while(width--) {
	    EPixel_t s = unpack_src(src);
	    EPixel_t d = unpack_dst(dst);
	    u_int8_t g = EPixelLuminance(s);
	    d = EPixelShadow(g, d);
	    pack_dst(d, dst);
	    src += src_psz;
	    dst += dst_psz;
	}
    }
    else {
	while(width--) {
	    EPixel_t s = unpack_src(src);
	    u_int8_t g;
	    if (s.a == EALPHA_OPAQUE) {
		EPixel_t d = unpack_dst(dst);
		g = EPixelLuminance(s);
		d = EPixelShadow(g, d);
		pack_dst(d, dst);
	    }
	    else if (s.a == EALPHA_TRANSPARENT) {
		;
	    }
	    else {
		EPixel_t d = unpack_dst(dst);
		d = EPixelBlend(s.a, s, d);
		g = EPixelLuminance(d);
		d = EPixelShadow(g, d);
		pack_dst(d, dst);
	    }
	    src += src_psz;
	    dst += dst_psz;
	}
    }
}

void EDirectShadowArea(u_int8_t* src, int src_wb, int src_pt,
		       u_int8_t* dst, int dst_wb, int dst_pt,
		       unsigned int width, unsigned int height, int flags)
{
    while(height--) {
	EDirectShadowRow(src, src_pt, dst, dst_pt, width, flags);
	src += src_wb;
	dst += dst_wb;	
    }
}

/*
 *  Add color (saturate) to each pixel in src and interpret the
 *  the use the alpha channel in color as fader value (i.e multiplier)
 *
 */
void EDirectAddColorArea(u_int8_t* src, int src_wb, int src_pt,
			 u_int8_t* dst, int dst_wb, int dst_pt,
			 u_int8_t fader, EPixel_t color,
			 unsigned int width, unsigned int height,int flags)
{
    unsigned int src_psz;
    unsigned int dst_psz;
    EPixelUnpack_t unpack_src;
    EPixelUnpack_t unpack_dst;
    EPixelPack_t   pack_dst;

    if (fader == ALPHA_FACTOR_0)
	return;
    if ((fader == ALPHA_FACTOR_1) && (color.px == 0) &&
	(src_pt != EPIXEL_TYPE_A8)) {  // FIXME!!!
	if (flags&EFLAG_BLEND)
	    EDirectBlendArea(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
	else
	    EDirectCopyArea(src,src_wb,src_pt,dst,dst_wb,dst_pt,width,height);
    }
    
    if ((flags&EFLAG_BLEND) == 0) // only blend sofar
	goto generic_area;

    if ((dst_pt == src_pt) && (width>=8)) {
	switch(src_pt) {
	case EPIXEL_TYPE_R8G8B8A8:
	case EPIXEL_TYPE_R8G8B8X8:
	    ESimdAddBlendAreaRGBA32(src,src_wb,dst,dst_wb,fader,
				    color,width,height);
	    return;
	case EPIXEL_TYPE_B8G8R8A8:
	case EPIXEL_TYPE_B8G8R8X8:
	    ESimdAddBlendAreaRGBA32(src,src_wb,dst,dst_wb,fader,
				    epixel_swap(color),width,height);
	    return;
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_X8R8G8B8:
	    ESimdAddBlendAreaARGB32(src,src_wb,dst,dst_wb,fader,
				    color,width,height);
	    return;
	case EPIXEL_TYPE_A8B8G8R8:
	case EPIXEL_TYPE_X8B8G8R8:
	    ESimdAddBlendAreaARGB32(src,src_wb,dst,dst_wb,fader,
				    epixel_swap(color),width,height);
	    return;
	default:
	    goto generic_area;
	}
    }
    else if (src_pt == EPIXEL_TYPE_A8) {
	switch(dst_pt) {
	case EPIXEL_TYPE_R8G8B8A8:
	case EPIXEL_TYPE_R8G8B8X8:
	    ESimdAddBlendAreaA8_RGBA32(src,src_wb,dst,dst_wb,fader,
				       color,width,height);
	    return;
	case EPIXEL_TYPE_B8G8R8A8:
	case EPIXEL_TYPE_B8G8R8X8:
	    ESimdAddBlendAreaA8_RGBA32(src,src_wb,dst,dst_wb,fader,
				       epixel_swap(color),width,height);
	    return;
	case EPIXEL_TYPE_A8R8G8B8:
	case EPIXEL_TYPE_X8R8G8B8:
	    ESimdAddBlendAreaA8_ARGB32(src,src_wb,dst,dst_wb,fader,
				       color,width,height);
	    return;
	case EPIXEL_TYPE_A8B8G8R8:
	case EPIXEL_TYPE_X8B8G8R8:
	    ESimdAddBlendAreaA8_ARGB32(src,src_wb,dst,dst_wb,fader,
				       epixel_swap(color),width,height);
	    return;
	default:
	    goto generic_area;
	}
    }
	    
generic_area:
    unpack_src = EPixelUnpackFunc(src_pt);
    unpack_dst = EPixelUnpackFunc(dst_pt);
    pack_dst   = EPixelPackFunc(dst_pt);

    src_psz = EPIXEL_SIZE(src_pt);
    dst_psz = EPIXEL_SIZE(dst_pt);

    if ((flags&EFLAG_BLEND) == 0) {
	while(height--) {
	    u_int8_t* dst1 = dst;
	    u_int8_t* src1 = src;
	    unsigned int width1 = width;

	    while(width1--) {
		EPixel_t s = unpack_src(src1);
		s = EPixelSum(color,s);
		s.a = ((s.a * fader) >> 8);
		s = EPixelShadow(s.a, s);
		pack_dst(s, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
    else {
	while(height--) {
	    u_int8_t* dst1 = dst;
	    u_int8_t* src1 = src;
	    unsigned int width1 = width;

	    while(width1--) {
		EPixel_t d = unpack_dst(dst1);
		EPixel_t s = unpack_src(src1);
		/* add the color to source */
		s = EPixelSum(color,s);
		s.a = ((s.a * fader) >> 8);
		d = EPixelBlend(s.a, s, d);
		pack_dst(d, dst1);
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
}

void EDirectAddColorRow(u_int8_t* src, int src_pt,
			u_int8_t* dst, int dst_pt,
			u_int8_t fade, EPixel_t color,
			unsigned int width,int flags)
{
    EDirectAddColorArea(src, 0, src_pt, dst, 0, dst_pt, 
			fade, color, width, 1, flags);
}

#define EPIC_AVG_MAX_N 128
/* special filter N_1 average N pixels */
static void filter_avg_N_1_area(u_int8_t* src, int src_wb, int src_pt,
				u_int8_t* dst, int dst_wb, int dst_pt,
				unsigned int width, unsigned int height,
				int n,
				EFlags_t flags)
{
    int src_psz = EPIXEL_SIZE(src_pt);
    int dst_psz = EPIXEL_SIZE(dst_pt);
    EPixelUnpack_t unpack_src = EPixelUnpackFunc(src_pt);
    EPixelUnpack_t unpack_dst = EPixelUnpackFunc(dst_pt);
    EPixelPack_t   pack_dst   = EPixelPackFunc(dst_pt);
    int height1 = height;


    while(height1--) {
	u_int8_t* src1 = src;
	u_int8_t* dst1 = dst;
	u_int32_t rs = 0;
	u_int32_t gs = 0;
	u_int32_t bs = 0;
	EPixel_t  rgb[EPIC_AVG_MAX_N];
	int i = 0;
	unsigned int width1 = (n-1)/2;
	
	memset(rgb, 0, sizeof(rgb));
	
	/* Do head part loading avg buffer */
	while(width1--) {
	    EPixel_t s = unpack_src(src1);
	    rs = (rs + s.r) - rgb[i].r;
	    gs = (gs + s.g) - rgb[i].g;
	    bs = (bs + s.b) - rgb[i].b;
	    rgb[i] = s;
	    i = (i==n-1) ? 0 : (i + 1);
	    src1 += src_psz;
	}
    
	/* Run middle part */
	width1 = width - (n-1)/2;
	while(width1--) {
	    EPixel_t s = unpack_src(src1);

	    rs = (rs + s.r) - rgb[i].r;
	    gs = (gs + s.g) - rgb[i].g;
	    bs = (bs + s.b) - rgb[i].b;
	    rgb[i] = s;
	    i = (i==n-1) ? 0 : (i + 1);

	    s.r = rs / n;
	    s.g = gs / n;
	    s.b = bs / n;

	    if (flags & EFLAG_BLEND) {
		EPixel_t d = unpack_dst(dst1);
		d = EPixelBlend(s.a, s, d);
		pack_dst(d, dst1);
	    }
	    else { 
		pack_dst(s, dst1);
	    }
	    src1 += src_psz;
	    dst1 += dst_psz;
	}

	/* Do tail part writing rest of dst */
	width1 = n - ((n-1)/2);
	while(width1--) {
	    EPixel_t s;

	    rs = rs - rgb[i].r;
	    gs = gs - rgb[i].g;
	    bs = bs - rgb[i].b;
	    i = (i==n-1) ? 0 : (i + 1);

	    s.r = rs / n;
	    s.g = gs / n;
	    s.b = bs / n;

	    if (flags & EFLAG_BLEND) {
		EPixel_t d = unpack_dst(dst1);
		d = EPixelBlend(s.a, s, d);
		pack_dst(d, dst1);
	    }
	    else { 
		pack_dst(s, dst1);
	    }
	    dst1 += dst_psz;
	}
	src += src_wb;
	dst += dst_wb;
    }
}

/* FIXME: make it possible to have src = dst ! */
static void filter_area(u_int8_t* src, int src_wb, int src_pt,
			u_int8_t* dst, int dst_wb, int dst_pt,
			EFilter_t* filter,
			unsigned int width, unsigned int height,
			EFlags_t flags)
{
    int n = filter->wh.width;
    if ((filter->wh.height == 1) && (n <= EPIC_AVG_MAX_N) &&
	(filter->fsum == (unsigned int) n)) { /* NOT Completly true ! */
	filter_avg_N_1_area(src, src_wb, src_pt, dst, dst_wb, dst_pt,
			    width, height, n, flags);
    }
    else {
	int src_psz = EPIXEL_SIZE(src_pt);
	int dst_psz = EPIXEL_SIZE(dst_pt);
	EPixelUnpack_t unpack_src = EPixelUnpackFunc(src_pt);
	EPixelUnpack_t unpack_dst = EPixelUnpackFunc(dst_pt);
	EPixelPack_t   pack_dst   = EPixelPackFunc(dst_pt);
	int height1 = height -= (filter->wh.height-1);

	/* do special treat y=0..m2-1 and  y=height-m2-1..height-1 */

	/* do y=hh ... height-hh */
	while(height1--) {
	    u_int8_t* src1 = src;
	    u_int8_t* dst1 = dst;
	    unsigned int width1 = width - (filter->wh.width-1);
	    
	    src1 += (filter->wh.width >> 1)*src_psz;
	    dst1 += (filter->wh.width >> 1)*dst_psz;
	    /* do x=ww ... width-ww */
	    while(width1--) {
		int fh = filter->wh.height;
		EPixel_t d;
		EPixel_t s;
		u_int8_t* src2  = src1;
		u_int8_t* fptr  = filter->factor;
		// u_int32_t acc_a = 0;
		u_int32_t acc_r = 0;
		u_int32_t acc_g = 0;
		u_int32_t acc_b = 0;
		
		while(fh--) {
		    int fw = filter->wh.width;
		    u_int8_t* sptr  = src2;
		    
		    // Should be able to use SIMD here!
		    while(fw--) {
			u_int8_t factor = *fptr++;
			EPixel_t t = unpack_src(sptr);
			// acc_a += factor*t.a;
			acc_r += factor*t.r;
			acc_g += factor*t.g;
			acc_b += factor*t.b;
			sptr += src_psz;
		    }
		    src2 += src_wb; // next source row
		}
		// s.a = acc_a / filter->fsum;
		s.r = acc_r / filter->fsum;
		s.g = acc_g / filter->fsum;
		s.b = acc_b / filter->fsum;
		if (flags & EFLAG_BLEND) {
		    d = unpack_dst(dst1);
		    d = EPixelBlend(s.a, s, d);
		    pack_dst(d, dst1);
		}
		else { 
		    pack_dst(s, dst1);
		}
		src1 += src_psz;
		dst1 += dst_psz;
	    }
	    src += src_wb;
	    dst += dst_wb;
	}
    }
}


/* interpolate a pixel (used for antialias and scaling etc) 
 *  blend the color at pixels in the surrounding of
 *  float coordinate (x,y) 
*/
static inline EPixel_t epixel_interp(EPixmap* pic, float x, float y)
{
    // To get gcc 4.1.2 to shut up about implicit declaration
    // without having to resort to -std=99, which triggers a shitload
    // of compile errors.
    extern float truncf(float);    
    extern float roundf(float);    

    int y0 = truncf(y-0.5);
    int y1 = truncf(y+0.5);
    int x0 = truncf(x-0.5);
    int x1 = truncf(x+0.5);
    float not_used;
    float fy = modff(y-0.5, &not_used);
    float fx = modff(x-0.5, &not_used);
    float f0 = (1-fx)*(1-fy);
    float f1 = fx*(1-fy);
    float f2 = (1-fx)*fy;
    float f3 = fx*fy;
    EPixel_t p0,p1,p2,p3;
    u_int8_t* ptr;
    EPixel_t p;

    if (EPointXYInRect(x0,y0, &pic->clip)) {
	ptr = EPIXEL_ADDR(pic, x0, y0);
	p0 = EPixelUnpack(pic->pixelType, ptr);
    }
    else
	p0 = epixel_transparent();

    if (EPointXYInRect(x1,y0, &pic->clip)) {
	ptr = EPIXEL_ADDR(pic, x1, y0);
	p1 = EPixelUnpack(pic->pixelType, ptr);
    }
    else
	p1 = epixel_transparent();

    if (EPointXYInRect(x0,y1, &pic->clip)) {
	ptr = EPIXEL_ADDR(pic, x0, y1);
	p2 = EPixelUnpack(pic->pixelType, ptr);
    }
    else
	p2 = epixel_transparent();


    if (EPointXYInRect(x1,y1, &pic->clip)) {
	ptr = EPIXEL_ADDR(pic, x1, y1);
	p3 = EPixelUnpack(pic->pixelType, ptr);
    }
    else
	p3 = epixel_transparent();


    // This could probably be done in ALTIVEC || SSE2
    p.r = roundf(f0*p0.r + f1*p1.r + f2*p2.r + f3*p3.r);
    p.g = roundf(f0*p0.g + f1*p1.g + f2*p2.g + f3*p3.g);
    p.b = roundf(f0*p0.b + f1*p1.b + f2*p2.b + f3*p3.b);
    p.a = roundf(f0*p0.a + f1*p1.a + f2*p2.a + f3*p3.a);

    return p;
}

static struct {
    const char* name;
    const int pixel_type;
} epixel_type[] =
{ 
    { "a8r8g8b8", EPIXEL_TYPE_ARGB }, { "argb", EPIXEL_TYPE_ARGB },  
    { "r8g8b8a8", EPIXEL_TYPE_RGBA }, { "rgba", EPIXEL_TYPE_RGBA },  
    { "a8b8g8r8", EPIXEL_TYPE_ABGR },  { "abgr", EPIXEL_TYPE_ABGR },  
    { "b8g8r8a8", EPIXEL_TYPE_BGRA },  { "bgra", EPIXEL_TYPE_BGRA },  
    { "r8g8b8",  EPIXEL_TYPE_RGB  },   { "rgb",  EPIXEL_TYPE_RGB  },  
    { "b8g8r8",  EPIXEL_TYPE_BGR  },   { "bgr",  EPIXEL_TYPE_BGR  },  
    { "r5g6b5", EPIXEL_TYPE_565_BE },  { "565",  EPIXEL_TYPE_565_BE },
    { "r5g6b5BE", EPIXEL_TYPE_565_BE },  { "565BE",  EPIXEL_TYPE_565_BE },
    { "r5g6b5LE", EPIXEL_TYPE_565_LE },  { "565LE",  EPIXEL_TYPE_565_LE },
    { "a1r5g5b",  EPIXEL_TYPE_A1R5G5B5 },  { "1555",  EPIXEL_TYPE_A1R5G5B5 },
    { "gray8a8", EPIXEL_TYPE_A8L8 },
    { "gray16",  EPIXEL_TYPE_L16 },
    { "alpha8",  EPIXEL_TYPE_A8 },
    { "gray8",   EPIXEL_TYPE_L8 },
    { NULL, 0 } };

int EPixelTypeFromName(char* name)
{
    int i = 0;
    
    while(epixel_type[i].name != NULL) {
	if (strcasecmp(name, epixel_type[i].name) == 0)
	    return epixel_type[i].pixel_type;
	i++;
    }
    return -1;
}

EPixel_t EPixelFromString(char* name)
{
    char* eptr;
    unsigned long c;

    c = strtol(name,  &eptr, 16);
    if (*eptr == '\0') {
	EPixel_t p;
	int a = (c >> 24);
	p.a = (a==0) ? 255 : a;
	p.r = (c >> 16) & 0xff;
	p.g = (c >> 8) & 0xff;
	p.b = c & 0xff;
	return p;
    }
    else if (strcasecmp(name, "black") == 0) 
	return epixel_black;
    else if (strcasecmp(name, "blue") == 0) 
	return epixel_blue;
    else if (strcasecmp(name, "red") == 0) 
	return epixel_red;
    else if (strcasecmp(name, "green") == 0) 
	return epixel_green;
    else if (strcasecmp(name, "transparent") == 0) 
	return epixel_transparent();
    else if (strcasecmp(name, "white") == 0) 
	return epixel_white;
    return epixel_black;
}



EPixmap* EPixmapCreate(unsigned int width, unsigned int height, int pixelType)
{
    u_int8_t* data0;
    EPixmap* pic;
    unsigned int bytesPerPixel = EPIXEL_SIZE(pixelType);
    unsigned int bytesPerRow   = bytesPerPixel*width;

    // Each row must by a multiple of 16!
    bytesPerRow += EPIC_ALIGN_OFFS(bytesPerRow,16);

    if ((data0 = (u_int8_t*) malloc(bytesPerRow*height+15)) == NULL)
	return NULL;
    if ((pic = (EPixmap*) malloc(sizeof(EPixmap))) == NULL) {
	free(data0);
	return NULL;
    }
    EOBJECT_INIT(pic, EPIXMAP_TYPE);
    pic->backend = NULL;
    pic->on_heap = 1;
    pic->refc = 1;

    ERectSet(&pic->clip, 0, 0, width, height);
    pic->width          = width;
    pic->bytesPerRow    = bytesPerRow;
    pic->height         = height;
    pic->bitsPerPixel   = bytesPerPixel*8;
    pic->pixelType      = pixelType;
    pic->bytesPerPixel  = bytesPerPixel;
    /* total number of bytes, not including padding */
    pic->sz             = bytesPerRow*height;
    pic->data0          = data0;
    pic->data           = data0 + EPIC_ALIGN_OFFS(data0,16);
    return pic;
}

int EPixmapAttach(EPixmap* pic, EBackend* be)
{
    return EBackendPixmapAttach(be, pic);
}

int EPixmapDetach(EPixmap* pic)
{
    if (pic->backend == NULL)
	return -1;
    return EBackendPixmapDetach(pic->backend, pic);
}

void EPIXMAP_TYPE_RELEASE(void* arg)
{
    EPixmap* pic = (EPixmap*) arg;

    EDBGFMT_MEM("EPIXMAP_TYPE_RELEASE: %p", arg);
    EPixmapDetach(pic);
    if (pic->data0 != NULL) {
	free(pic->data0);
	pic->data0 = NULL;
	pic->data = NULL;
    }
    if (pic->on_heap)
	free(pic);
}


void EPixmapSetClip(EPixmap* pic, ERect_t* clip)
{
    ERect_t physRect;

    ERectSet(&physRect, 0, 0, pic->width, pic->height);
    ERectIntersect(clip, &physRect, &pic->clip);
}

/*
 * Draw attached-pixmap on attached-window
 *
 */
int EPixmapDrawWindow(EPixmap* pic, EWindow* win, int off_screen,
		      int x_src, int y_src, int x_dst, int y_dst, 
		      unsigned int width, unsigned int height)
{
    if ((pic->backend == win->backend) && (pic->backend != NULL)) {
	return EBackendPixmapDraw(pic->backend, pic, win, off_screen,
				  x_src, y_src, x_dst, y_dst, width, height);
    }
    return -1;
}

/* put pixel given address */
static inline void put_apixel(u_int8_t* addr, int pt, int flags, EPixel_t s)
{
    if (((flags & EFLAG_BLEND)==0) || (s.a == EALPHA_OPAQUE))
	EPixelPack(pt, s, addr);
    else if (s.a != EALPHA_TRANSPARENT) {
	EPixel_t d = EPixelUnpack(pt, addr);
	d = EPixelBlend(s.a, s, d);
	EPixelPack(pt, d, addr);
    }
}

/* plot pixel, with fixed color */
void EPixmapPutPixel(EPixmap* pic, int x, int y, int flags, EPixel_t p)
{
    u_int8_t* dst;

    if (!EPointXYInRect(x, y, &pic->clip))
	return;
    dst = EPIXEL_ADDR(pic,x,y);
    put_apixel(dst,pic->pixelType,flags,p);
}

/* draw a point at (x,y) in foreground color (using line style) */
void EPixmapDrawPoint(EPixmap* pic, EGc* gc, int x, int y)
{
    u_int8_t* dst;

    /* First check clip */
    if (!EPointXYInRect(x, y, &pic->clip))
	return;
    dst = EPIXEL_ADDR(pic,x,y);
    if (gc == NULL) gc = &default_gc;
    put_apixel(dst,pic->pixelType,gc->line_style,gc->foreground_color);
}

static inline u_int8_t wu_blend(u_int8_t w, u_int8_t fg, u_int8_t bg)
{
    return (bg > fg) ? eblend(w, bg, fg) : eblend(w, fg, bg);
}

static inline void put_wu_apixel(u_int8_t* ptr, int pixelType,
				 u_int8_t w,int flags, EPixel_t fg,
				 u_int8_t lfg)
{
    EPixel_t bg;
    u_int8_t lbg;

    bg  = EPixelUnpack(pixelType, ptr);
    lbg = EPixelLuminance(bg);
    if (lfg > lbg)
	w = w ^ 255;
    fg.r = wu_blend(w, fg.r, bg.r);
    fg.g = wu_blend(w, fg.g, bg.g);
    fg.b = wu_blend(w, fg.b, bg.b);

    if (((flags & EFLAG_BLEND)==0) || (fg.a == EALPHA_OPAQUE))
	EPixelPack(pixelType, fg, ptr);
    else if (fg.a != EALPHA_TRANSPARENT) {
	bg = EPixelBlend(fg.a, fg, bg); // maybe do wu_blend & blend ?
	EPixelPack(pixelType, bg, ptr);
    }
}


/* EPixmapPutPixels:
 *   copy pixel data (0,0,width,height) => (x_dst,y_dst,width,height)
 *   using blending function 
 */

void EPixmapPutPixels(EPixmap* dst, int x_dst, int y_dst,
		      unsigned int width, unsigned int height,
		      int pixelType, int flags,
		      void* data, unsigned int len)
{
    u_int8_t* src_ptr;
    u_int8_t* src_end;
    unsigned int src_psz = EPIXEL_SIZE(pixelType);
    unsigned int src_wb  = width*src_psz;
    ERect_t sr;
    ERect_t dr;
    ERect_t dr0 = {{x_dst,y_dst},{width,height}};

    // Clip destination and check if we have any thing to copy
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    sr.wh = dr.wh;
    sr.xy.x = (ERectLeft(&dr0) - ERectLeft(&dr));
    sr.xy.y = (ERectTop(&dr0) - ERectTop(&dr));

    src_ptr = ((u_int8_t*)data) + 
	(ERectTop(&sr)*src_wb) + (ERectLeft(&sr)*src_psz);
    src_end  = ((u_int8_t*)data) + 
	(ERectBottom(&sr)*src_wb) + (ERectRight(&sr)*src_psz);
    // width,height pixelType and/or len is not matching
    if (src_end >= (((u_int8_t*)data)+len))
	return;

    if ((flags & EFLAG_BLEND)==0)
	EDirectCopyArea(src_ptr, src_wb, pixelType,
			EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			dst->bytesPerRow, dst->pixelType,
			dr.wh.width, dr.wh.height);
    else
	EDirectBlendArea(src_ptr, src_wb, pixelType,
			 EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			 dst->bytesPerRow, dst->pixelType,
			 dr.wh.width, dr.wh.height);
}


/* read a pixel */
EPixel_t EPixmapGetPixel(EPixmap* pic, int x, int y)
{
    u_int8_t* src;

    if (!EPointXYInRect(x,y,&pic->clip))
	return epixel_black;
    src = EPIXEL_ADDR(pic,x,y);
    return EPixelUnpack(pic->pixelType, src);
}

/* Fill EPixmap with colors from p */
void EPixmapFill(EPixmap* pic, EPixel_t p)
{
    u_int32_t  cv = 0;

    EPixelPack(pic->pixelType, p, (u_int8_t*)&cv);

    switch(pic->bytesPerPixel) {
    case 1: EFill8(pic->data, cv>>24, pic->sz); break;
    case 2: EFill16(pic->data, cv>>16, pic->sz/2); break;
    case 3: EFill24(pic->data, cv, pic->sz/3); break;
    case 4: EFILL(pic->data, cv, pic->sz/4); break;
    default: break;
    }
}

/* FIXME make this buffer thread safe */
static unsigned int pixel_buffer_size = 0;
static u_int8_t* pixel_buffer = NULL;

static u_int8_t* pixel_buffer_alloc(unsigned int n)
{
    if (pixel_buffer_size  < n) {
	pixel_buffer = (u_int8_t*) realloc(pixel_buffer, n);
	pixel_buffer_size = n;
    }
    return pixel_buffer;
}


/* copy area and shift lines left or right */
static inline void shift_area(u_int8_t* src, int src_wb, int src_pt,
			      u_int8_t* dst, int dst_wb, int dst_pt,
			      unsigned int width, unsigned int height, 
			      int amount)
{
    if (amount > 0) {
	int psz = EPIXEL_SIZE(src_pt);
	src   += amount*psz;
	width -= amount;
    }
    else {
	int psz = EPIXEL_SIZE(dst_pt);
	dst   += (-amount)*psz;
	width -= (-amount);
    }

    while(height--) {
	EDirectCopyRow(src, src_pt, dst, dst_pt, width);
	src += src_wb;
	dst += dst_wb;
    }
}


/* copy area and shift lines left or right */
static inline void rotate_area(u_int8_t* src, int src_wb, int src_pt,
			       u_int8_t* dst, int dst_wb, int dst_pt,
			       unsigned int width, unsigned int height, 
			       int amount)
{
    int a = (amount < 0) ? -amount : amount;
    int src_psz = EPIXEL_SIZE(src_pt);
    int dst_psz = EPIXEL_SIZE(dst_pt);
    int n = width;
    int is_inline = (src == dst);
    u_int8_t* src_from;
    u_int8_t* dst_to;

    if (amount == 0)
	return;
    else if (amount > 0) {
	src_from = src;
	src += a*src_psz;
	n -= a;
	dst_to = dst + n*dst_psz;
    }
    else {
	n  -= a;
	src_from = src+n*src_psz;
	dst_to = dst;
	dst += a*dst_psz;
    }

    n *= src_psz;
    a *= src_psz;

    if (is_inline) {
	u_int8_t* save = pixel_buffer_alloc(a);

	while(height--) {
	    memcpy(save, src_from, a);
	    memmove(dst, src, n);
	    memcpy(dst_to, save, a);
	    src      += src_wb;
	    src_from += src_wb;
	    dst      += dst_wb;
	    dst_to   += dst_wb;
	}
    }
    else {  /* not inline */
	while(height--) {
	    EDirectCopyRow(src, src_pt, dst, dst_pt, n);
	    EDirectCopyRow(src_from, src_pt, dst_to, dst_pt, a);
	    src      += src_wb;
	    src_from += src_wb;
	    dst      += dst_wb;
	    dst_to   += dst_wb;
	}
    }
}

/*
 * Copy pixmap data from src to dst, ignore clip region
 * if pixmap size is the same just memcpy
 * otherwise calculate the min area and copy that
 */
void EPixmapCopy(EPixmap* src, EPixmap* dst)
{
    if ((src->width==dst->width) && (src->height == dst->height) && 
	(src->sz == dst->sz) && (src->pixelType == dst->pixelType)) {
	ESimdCopy(src->data, dst->data, src->sz);
	// memcpy(dst->data, src->data, src->sz);
    }
    else {
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h = (src->height < dst->height) ? src->height : dst->height;
	EDirectCopyArea(src->data,src->bytesPerRow,src->pixelType,
			dst->data,dst->bytesPerRow,dst->pixelType, w,h);
    }
}


void EPixmapScrollLeft(EPixmap* src, EPixmap* dst,
		       int rotate, unsigned int amount, EPixel_t fill)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	rotate_area(src->data, src->bytesPerRow, src->pixelType, 
		    dst->data, dst->bytesPerRow, dst->pixelType, w, h, a);
    else {
	shift_area(src->data, src->bytesPerRow, src->pixelType,
		   dst->data, dst->bytesPerRow, dst->pixelType, w, h, a);
	EDirectFillArea(dst->data+dst->bytesPerPixel*(w-amount),
			dst->bytesPerRow,dst->pixelType, 
			amount, h, fill);
    }
}

void EPixmapScrollRight(EPixmap* src, EPixmap* dst,
			int rotate, unsigned int amount, EPixel_t fill)
{
    int w = (src->width < dst->width) ? src->width : dst->width;
    int h = (src->height < dst->height) ? src->height : dst->height;
    int a = amount;

    if (rotate)
	rotate_area(src->data, src->bytesPerRow, src->pixelType,
		    dst->data, dst->bytesPerRow, dst->pixelType, w, h, -a);
    else {
	shift_area(src->data, src->bytesPerRow, src->pixelType,
		   dst->data, dst->bytesPerRow, dst->pixelType, w, h, -a);
	EDirectFillArea(dst->data, dst->bytesPerRow, dst->pixelType, amount, h, fill);
    }
}

void EPixmapScrollUp(EPixmap* src, EPixmap* dst, 
		    int rotate, unsigned int amount, EPixel_t fill)
{
    if ((amount >= src->height) && !rotate)
	EPixmapFill(dst, fill);
    else {
	u_int8_t* dst_ptr;
	u_int8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EPIXEL_ADDR(src,0,amount);
	dst_ptr = EPIXEL_ADDR(dst,0,0);

	if (rotate) {
	    if (src == dst) {
		u_int8_t* save = pixel_buffer_alloc(amount*src->width*dst->bytesPerPixel);
		u_int8_t* dst_save = EPIXEL_ADDR(dst,0,0);

		EDirectCopyArea(dst_save, dst->bytesPerRow, dst->pixelType,
				save, dst->bytesPerRow, dst->pixelType, 
				dst->width, amount);
		EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, h);
		dst_ptr = EPIXEL_ADDR(dst,0,h);
		EDirectCopyArea(save, dst->bytesPerRow, dst->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, amount);
	    }
	    else {
		EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, h);
		dst_ptr = EPIXEL_ADDR(dst,0,h);
		src_ptr = EPIXEL_ADDR(src,0,0);
		EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, amount);
	    }
	}
	else {
	    EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
			    dst_ptr, dst->bytesPerRow, dst->pixelType, w, h);
	    dst_ptr = EPIXEL_ADDR(dst,0,h);
	    EDirectFillArea(dst_ptr, dst->bytesPerRow, dst->pixelType, 
			    dst->width, amount, fill);
	}
    }
}

void EPixmapScrollDown(EPixmap* src, EPixmap* dst, 
		      int rotate, unsigned int amount, EPixel_t fill)
{
    if ((amount >= src->height) && !rotate)
	EPixmapFill(dst, fill);
    else {
	u_int8_t* dst_ptr;
	u_int8_t* src_ptr;
	int w = (src->width < dst->width) ? src->width : dst->width;
	int h;

	amount %= src->height;
	h = (src->height - amount);

	src_ptr = EPIXEL_ADDR(src,0,0);
	dst_ptr = EPIXEL_ADDR(dst,0,amount);

	if (rotate) {
	    if (src == dst) {
		u_int8_t* save = pixel_buffer_alloc(amount*src->width*dst->bytesPerPixel);
		u_int8_t* dst_save = EPIXEL_ADDR(dst,0,h);

		EDirectCopyArea(dst_save, dst->bytesPerRow, dst->pixelType,
				save, dst->bytesPerRow, dst->pixelType, 
				dst->width, amount);
		EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, h);
		dst_ptr = EPIXEL_ADDR(dst,0,0);
		EDirectCopyArea(save, dst->bytesPerRow, dst->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, amount);
	    }
	    else {
		EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, h);
		src_ptr = EPIXEL_ADDR(src,0,h);
		dst_ptr = EPIXEL_ADDR(dst,0,0);
		EDirectCopyArea(src_ptr, src->bytesPerRow, dst->pixelType,
				dst_ptr, dst->bytesPerRow, dst->pixelType,
				w, amount);
	    }
	}
	else {
	    EDirectCopyArea(src_ptr, src->bytesPerRow, src->pixelType,
			    dst_ptr, dst->bytesPerRow, dst->pixelType,
			    w, h);
	    dst_ptr = EPIXEL_ADDR(dst,0,0);
	    EDirectFillArea(dst_ptr, dst->bytesPerRow, dst->pixelType, 
			    dst->width, amount, fill);
	}
    }
}

/* scroll pixmap up/dow  left/right rotate/fill */
void EPixmapScroll(EPixmap* src, EPixmap* dst, 
		 int horizontal, int vertical, 
		 int rotate, EPixel_t fill)
{
    if (vertical>0)
	EPixmapScrollUp(src, dst, rotate, vertical, fill);
    else if (vertical < 0)
	EPixmapScrollDown(src, dst, rotate, -vertical, fill);
    if (horizontal>0)
	EPixmapScrollRight(src, dst, rotate, horizontal, fill);
    else if (horizontal < 0)
	EPixmapScrollLeft(src, dst, rotate, -horizontal, fill);
}


/* f1 => f2, f2 => f1 */
static inline int swap_flags(int flags, int f1, int f2)
{
    int t = 0;

    if (f1 & flags) t |= f2;
    if (f2 & flags) t |= f1;
    t |= (flags & ~(f1|f2));
    return t;
}

/* if flag f1 => t1, f2 => t2, f3 => t3 */
static inline int map_flags(int flags, 
			    int f1, int t1,
			    int f2, int t2,
			    int f3, int t3)
{
    int t = 0;

    if (f1 & flags) t |= t1;
    if (f2 & flags) t |= t2;
    if (f3 & flags) t |= t3;
    t |= (flags & ~(f1|f2|f3));
    return t;
}

			    
/* Fill triangle (x0,y0) EDGE1 (x1,y1) EDGE3 (x2,y2) EDGE2 (x0,y0)
 * 
 */
extern void fill_triangle(EPixmap* pic, 
			  int x0, int y0,
			  int x1, int y1,
			  int x2, int y2,
			  int flags, EPixel_t fg);

void EPixmapDrawTriangle(EPixmap* pic, EGc* gc,
			 int x0, int y0,
			 int x1, int y1,
			 int x2, int y2) 
{
    if (gc == NULL) gc = &default_gc;

    if (gc->fill_style == EPIC_FILL_STYLE_NONE) {
	EFlags_t flags  = gc->line_style| EPIC_LINE_STYLE_NFIRST;
	unsigned int lw = gc->line_width;
	EPixel_t     fc = gc->foreground_color;
	draw_line(pic,x0,y0,x1,y1,lw,flags,fc);
	draw_line(pic,x1,y1,x2,y2,lw,flags,fc);
	draw_line(pic,x2,y2,x0,y0,lw,flags,fc);
	return;
    }
    fill_triangle(pic, x0, y0, x1, y1, x2, y2, 
		  gc->fill_style,
		  gc->fill_color);
}

void EPixmapDrawTriangle_old(EPixmap* pic, EGc* gc,
			 int x0, int y0,
			 int x1, int y1,
			 int x2, int y2) 
{
    int save_flags;
    
    if (gc == NULL) gc = &default_gc;

    if (gc->fill_style == EPIC_FILL_STYLE_NONE) {
	EFlags_t flags  = gc->line_style| EPIC_LINE_STYLE_NFIRST;
	unsigned int lw = gc->line_width;
	EPixel_t     fc = gc->foreground_color;
	draw_line(pic,x0,y0,x1,y1,lw,flags,fc);
	draw_line(pic,x1,y1,x2,y2,lw,flags,fc);
	draw_line(pic,x2,y2,x0,y0,lw,flags,fc);
	return;
    }

    save_flags = gc->border_style;
    /* First FIND min Y then min X */
    if (y2 < y1) { /* swap 1 & 2 */
	ESwapInt(x1,x2);
	ESwapInt(y1,y2);
 	gc->border_style = swap_flags(gc->border_style,
				      EPIC_BORDER_STYLE_NBORDER1,
				      EPIC_BORDER_STYLE_NBORDER3);
    }
    if (y1 < y0) {  /* swap 0 & 1 */
	ESwapInt(x0,x1);
	ESwapInt(y0,y1);
	gc->border_style = swap_flags(gc->border_style,
				      EPIC_BORDER_STYLE_NBORDER1,
				      EPIC_BORDER_STYLE_NBORDER2);
    }
    /* At this point y0 is the min y */
    if (x2 < x1) {   /* swap 1 & 2 */
	ESwapInt(x1,x2);
	ESwapInt(y1,y2);
	gc->border_style = swap_flags(gc->border_style,
				      EPIC_BORDER_STYLE_NBORDER1,
				      EPIC_BORDER_STYLE_NBORDER3);
    }
    draw_triangle(pic, gc, x0, y0, x1, y1, x2, y2);
    gc->border_style = save_flags;
}

/* Trace lines (x,y) -> (x1,y1) and  (x,y) -> (x2,y2)
 *  (y <= y1) && (y <= y2)
 * while filling horizontal lines from x1 to x2 (x1 <= x2!) 
 */


static void draw_triangle(EPixmap* pic, EGc* gc,
			  int x, int y, int x1, int y1, int x2, int y2)
{
    int xt1, yt1;
    int dx1, dy1;
    int sx1=0, sxw1=0;
    int xt2, yt2;
    int dx2, dy2;
    int sx2=0, sxw2=0;
    int cx2, cy2;
    int cx3, cy3;
    int f1=0, f2=0;
    int ys;
    int y_max;
    u_int8_t* ptr1;
    u_int8_t* ptr2;
    int bytesPerPixel = pic->bytesPerPixel;
    int bytesPerRow   = pic->bytesPerRow;
    int setup = 3;
    int bf1 = EPIC_BORDER_STYLE_NBORDER1;
    int bf2 = EPIC_BORDER_STYLE_NBORDER2;
    EPixel_t bc = gc->border_color;
    EPixel_t fc = gc->fill_color;
    int bflags = gc->border_style;
    int fflags = gc->fill_style;

    cx2 = ERectLeft(&pic->clip);
    cy2 = ERectTop(&pic->clip);
    cx3 = ERectRight(&pic->clip);
    cy3 = ERectBottom(&pic->clip);

    ptr1 = EPIXEL_ADDR(pic,x,y);
    ptr2 = ptr1;

    xt1 = xt2 = x;
    yt1 = yt2 = y;

    dy1 = y1 - y;     /* dy1 >= 0 ! */
    dx1 = x1 - x;

    dy2 = y2 - y;     /* dy2 >= 0 ! */
    dx2 = x2 - x;

    y_max = (y1 > y2) ? y1 : y2;   /* max y for the two lines */
    ys    = (y1 < y2) ? y1 : y2;   /* min y for the two lines */

    /* check border1 AND border2 */
    if (!(bflags & (bf1|bf2)) &&
	EInRange(x, cx2, cx3) && EInRange(y, cy2, cy3))
	put_apixel(ptr1,pic->pixelType,bflags,bc);

setup:
    if (setup & 1) {
	if (dx1 < 0) { dx1=-dx1; sx1=-1; sxw1=-bytesPerPixel; } else 
	{ sx1=1; sxw1=bytesPerPixel; }
	dy1 <<= 1;
	dx1 <<= 1;
	f1 = (dx1 > dy1) ? dy1 - (dx1 >> 1) : dx1 - (dy1 >> 1);
    }

    if (setup & 2) {
	if (dx2 < 0) { dx2=-dx2; sx2=-1; sxw2=-bytesPerPixel; } else 
	{ sx2=1; sxw2=bytesPerPixel; }
	dy2 <<= 1;
	dx2 <<= 1;
	f2 = (dx2 > dy2) ? dy2 - (dx2 >> 1) : dx2 - (dy2 >> 1);
    }

    setup = 0;

    while (y <= ys) {
	/* Step Line1 */
	if (yt1 <= y) {
	    if (dx1 > dy1) {  /* step x */
		while((xt1 != x1)) {
		    // LINESTEP(f1,ptr1,xt1,dx1,sx1,swx1,yt1,dy1,sy1,syw1);
		    if (f1 >= 0) {
			if (yt1 > y) break;
			ptr1 += bytesPerRow;
			yt1++;
			f1 -= dx1;
		    }
		    ptr1 += sxw1;
		    xt1 += sx1;
		    f1 += dy1;
		    if (!(bflags & bf1) &&
			EInRange(xt1, cx2, cx3) && EInRange(yt1, cy2, cy3))
			put_apixel(ptr1,pic->pixelType,bflags, bc);
		}
	    }
	    else {    /* step y */
		while((yt1 <= y)) {
		    if (f1 >= 0) {
			ptr1 += sxw1;
			xt1 += sx1;
			f1 -= dy1;
		    }
		    ptr1 += bytesPerRow;
		    yt1++;
		    f1 += dx1;
		    if (!(bflags & bf1) &&
			EInRange(xt1, cx2, cx3) && EInRange(yt1, cy2, cy3))
			put_apixel(ptr1,pic->pixelType,bflags,bc);
		}
	    }
	}

	/* Step Line 2 */
	if (yt2 <= y) {
	    if (dx2 > dy2) { /* step x */
		while((xt2 != x2)) {
		    if (f2 >= 0) {
			if (yt2 > y) break;
			ptr2 += bytesPerRow;
			yt2++;
			f2 -= dx2;
		    }
		    ptr2 += sxw2;
		    xt2 += sx2;
		    f2 += dy2;
		    if (!(bflags & bf2) &&
			EInRange(xt2, cx2, cx3) && EInRange(yt2, cy2, cy3))
			put_apixel(ptr2,pic->pixelType,bflags,bc);
		}
	    }
	    else { /* step y */
		while ((yt2 <= y)) {
		    if (f2 >= 0) { 
			ptr2 += sxw2;
			xt2 += sx2;
			f2 -= dy2;
		    }
		    ptr2 += bytesPerRow;
		    yt2++;
		    f2 += dx2;
		    if (!(bflags & bf2) &&
			EInRange(xt2, cx2, cx3) && EInRange(yt2, cy2, cy3))
			put_apixel(ptr2,pic->pixelType,bflags,bc);
		}
	    }
	}
	draw_line(pic,xt1,y,xt2,y,1,
		  fflags|EPIC_LINE_STYLE_NFIRST|EPIC_LINE_STYLE_NLAST,
		  fc);
	y++;
    }

    if (y >= y_max) {
	if (xt1 != xt2) {
	    if (!(bflags & EPIC_BORDER_STYLE_NBORDER3)) {
		draw_line(pic,xt1,y_max,xt2,y_max,1,
			  bflags|EPIC_LINE_STYLE_NFIRST|EPIC_LINE_STYLE_NLAST,
			  bc);
	    }
	}
	return;
    }

    if ((yt1 >= y1)) {	/* line1 done, now continue to (x2,y2) */
	dx1 = x2 - x1;
	dy1 = y2 - y1;
	ys  = y2;
	y1  = y2;
	x1  = x2;
	setup |= 1;
	bf1 = EPIC_BORDER_STYLE_NBORDER3;
	goto setup;
    }
    
    if ((yt2 >= y2)) {  /* line2 done, continue to (x1,y1) */
	dx2 = x1 - x2;
	dy2 = y1 - y2;
	ys  = y1;
	y2  = y1;
	x2  = x1;
	setup |= 2;
	bf2 = EPIC_BORDER_STYLE_NBORDER3;
	goto setup;	    
    }
}

/* Draw rectangle (x,y,w,h)  */
void EPixmapDrawRectangle(EPixmap* pic, EGc* gc,
			  int x, int y,
			  unsigned int width, 
			  unsigned int height)
{
    ERect_t r = {{x, y}, {width, height}};
    EFlags_t ff;

    if (gc == NULL) gc = &default_gc;

    if (!ERectIntersect(&r, &pic->clip, &r))  // FIXME add border
	return;

    x = ERectLeft(&r);
    y = ERectTop(&r);
    ff = gc->fill_style;
    if (ff == EPIC_FILL_STYLE_NONE) {
	EFlags_t bf     = gc->border_style | EPIC_LINE_STYLE_NFIRST;
	EPixel_t bc     = gc->border_color;
	unsigned int bw = gc->border_width;
	int x1 = ERectRight(&r);
	int y1 = ERectBottom(&r);
	draw_line(pic,x,y,x1,y,bw,bf,bc);
	draw_line(pic,x1,y,x1,y1,bw,bf,bc);
	draw_line(pic,x1,y1,x,y1,bw,bf,bc);
	draw_line(pic,x,y1,x,y,bw,bf,bc);
	return;
    }
    else {
	u_int8_t* ptr;
	int x1, y1;
	EFlags_t  bf  = gc->border_style;
	EPixel_t fc  = gc->fill_color;
	int bw       = gc->border_width;
	x1 = ERectRight(&r);
	y1 = ERectBottom(&r);

	if ((bw > 0) && 
	    ((bf & EPIC_BORDER_STYLE_NBORDER) != EPIC_BORDER_STYLE_NBORDER)) {
	    EPixel_t bc = gc->border_color;
	    if (!(bf & EPIC_BORDER_STYLE_NBORDER1))
		draw_line(pic,x,y-1,x1,y-1,bw,bf,bc);
	    if (!(bf & EPIC_BORDER_STYLE_NBORDER2))
		draw_line(pic,x1+1,y,x1+1,y1,bw,bf,bc);
	    if (!(bf & EPIC_BORDER_STYLE_NBORDER3))
		draw_line(pic,x1,y1+1,x,y1+1,bw,bf,bc);
	    if (!(bf & EPIC_BORDER_STYLE_NBORDER4))
		draw_line(pic,x-1,y1,x-1,y,bw,bf,bc);
	}
	ptr = EPIXEL_ADDR(pic,x,y);
	width  = ERectWidth(&r);
	height = ERectHeight(&r);

	if (((ff & EFLAG_BLEND)==0) || (fc.a == EALPHA_OPAQUE))
	    EDirectFillArea(ptr,pic->bytesPerRow,pic->pixelType,width,height,fc);
	else if (fc.a != EALPHA_TRANSPARENT) {
	    EDirectFillAreaBlend(ptr,pic->bytesPerRow,pic->pixelType,width,height,fc);
	}
    }
}

/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * blending the src with the dest 
 */
void EPixmapCopyArea(EPixmap* src,EPixmap* dst,
		     int x_src, int y_src, int x_dst, int y_dst,
		     unsigned int width, unsigned int height,
		     int flags)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr0, dr;

    // Clip source and check that there is anything to copy
    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to copy to
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    if ((flags & (EFLAG_BLEND|EFLAG_SUM))==0)
	EDirectCopyArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			src->bytesPerRow, src->pixelType,
			EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			dst->bytesPerRow, dst->pixelType,
			dr.wh.width, dr.wh.height);
    else if  ((flags & (EFLAG_BLEND|EFLAG_SUM))==EFLAG_SUM) {
	EDirectSumArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		       src->bytesPerRow, src->pixelType,
		       EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		       dst->bytesPerRow, dst->pixelType,
		       dr.wh.width, dr.wh.height);
    }
    else
	EDirectBlendArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			 src->bytesPerRow, src->pixelType,
			 EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			 dst->bytesPerRow, dst->pixelType,
			 dr.wh.width, dr.wh.height);
}


/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * blending using alpha.
 */
void EPixmapAlphaArea(EPixmap* src,EPixmap* dst, u_int8_t alpha,
		     int x_src, int y_src, int x_dst, int y_dst,
		     unsigned int width, unsigned int height)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr0, dr;

    // Clip source and check that there is anything to blend
    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to blend to
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    if (alpha == 255)
	EDirectCopyArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			src->bytesPerRow, src->pixelType,
			EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			dst->bytesPerRow, dst->pixelType,
			dr.wh.width, dr.wh.height);
    else 
	EDirectAlphaArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			 src->bytesPerRow, src->pixelType,
			 EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			 dst->bytesPerRow, dst->pixelType,
			 alpha, dr.wh.width, dr.wh.height);
}

/* copy src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * fade using fader value
 */
void EPixmapFadeArea(EPixmap* src,EPixmap* dst, u_int8_t fade,
		     int x_src, int y_src, int x_dst, int y_dst,
		     unsigned int width, unsigned int height)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr0, dr;

    // Clip source and check that there is anything to blend
    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to blend to
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    if (fade == ALPHA_FACTOR_0) /* 0.0 */
	return;
    else if (fade == ALPHA_FACTOR_1) /* ~1.0 */
	EDirectBlendArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			 src->bytesPerRow, src->pixelType,
			 EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			 dst->bytesPerRow, dst->pixelType,
			 dr.wh.width, dr.wh.height);
    else
	EDirectFadeArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			src->bytesPerRow, src->pixelType,
			EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			dst->bytesPerRow, dst->pixelType,
			fade, dr.wh.width, dr.wh.height);
}


/* Shadow src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * this function will blend the pixels from source with
 * the luminance value as alpha.
 */
void EPixmapShadowArea(EPixmap* src,EPixmap* dst,
		       int x_src, int y_src, int x_dst, int y_dst,
		       unsigned int width, unsigned int height,
		       int flags)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr0, dr;

    // Clip source and check that there is anything to blend
    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to blend to
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    EDirectShadowArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		      src->bytesPerRow, src->pixelType,
		      EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		      dst->bytesPerRow, dst->pixelType,
		      dr.wh.width, dr.wh.height, flags);
}


void EPixmapAddColorArea(EPixmap* src,EPixmap* dst, u_int8_t fade,
			 EPixel_t color,
			 int x_src, int y_src, int x_dst, int y_dst,
			 unsigned int width, unsigned int height, int flags)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr0, dr;

    // Clip source and check that there is anything to blend
    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to blend to
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    EDirectAddColorArea(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
			src->bytesPerRow, src->pixelType,
			EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
			dst->bytesPerRow, dst->pixelType,
			fade, color, dr.wh.width, dr.wh.height, flags);
}


/* filter src rectangle (x1,y1,w,h) to dst rectangle (x2,y2,w,h) 
 * blending using alpha.
 */
void EPixmapFilterArea(EPixmap* src,EPixmap* dst,EFilter_t* filter,
		       int x_src, int y_src, int x_dst, int y_dst,
		       unsigned int width, unsigned int height,
		       int flags)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr0, dr;

    // Clip source and check that there is anything to blend
    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    // The destination must have the same dimension as source
    dr0.xy.x = x_dst;
    dr0.xy.y = y_dst;
    dr0.wh   = sr.wh;
    // Clip destination and check that there is anything to blend to
    if (!ERectIntersect(&dr0, &dst->clip, &dr))
	return;
    // Update sr with the width and relativ change dx, dy
    sr.xy.x -= (dr0.xy.x - dr.xy.x);
    sr.xy.y -= (dr0.xy.y - dr.xy.y);
    sr.wh = dr.wh;

    filter_area(EPIXEL_ADDR(src,sr.xy.x,sr.xy.y), 
		src->bytesPerRow, src->pixelType,
		EPIXEL_ADDR(dst,dr.xy.x,dr.xy.y),
		dst->bytesPerRow, dst->pixelType,
		filter,dr.wh.width, dr.wh.height,flags);
}

/*
 *   T = [ A  B Tx ] [x]
 *       [ C  D Ty ] [y]
 *                   [1]
 *
 *   x' = A*x + B*y + Tx
 *   y' = C*x + D*y + Ty
 */

#define TRANSFORM(xp,yp,A,B,C,D,Tx,Ty,x,y) \
    xp = (A)*(x) + (B)*(y) + (Tx); \
    yp = (C)*(x) + (D)*(y) + (Ty)

/*
 * Rotate a Pixmap x_dst and y_dst point out the rotation center
 * in the destination Pixmap
 *
 *     R: [ cos(a)   sin(a) ]
 *        [-sin(a)   cos(a) ]
 *
 *     D = R * S
 *
 *     R':[ cos(a)  -sin(a) ]
 *        [ sin(a)   cos(a) ]
 *
 *     S = R' * D
 *
 */

void EPixmapRotateArea(EPixmap* src, EPixmap* dst, float angle,
		       int x_src, int y_src, int xc_src, int yc_src,
		       int xc_dst, int yc_dst,
		       unsigned int width, unsigned int height, int flags)
{
    ERect_t sr = {{x_src,y_src}, {width,height}};
    ERect_t dr;
    float sa = sinf(angle);
    float ca = cosf(angle);
    int src_pt = src->pixelType;
    int dst_pt = dst->pixelType;
    float min_dx, max_dx;
    float min_dy, max_dy;
    float x, y;
    float xo, yo;

    if (!ERectIntersect(&sr, &src->clip, &sr))
	return;
    dr = dst->clip;

    /* calculate desitionan 
     *
     * min_dx, max_dx
     * min_dy, max_dy
     */
    xo = xc_src - x_src;
    yo = yc_src - y_src;

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, -xo, -yo);
    min_dx = max_dx = x;
    min_dy = max_dy = y;

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, width-xo, -yo);
    min_dx = EMinFloat(min_dx, x);
    max_dx = EMaxFloat(max_dx, x);
    min_dy = EMinFloat(min_dy, y);
    max_dy = EMaxFloat(max_dy, y);

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, width-xo, height-yo);
    min_dx = EMinFloat(min_dx, x);
    max_dx = EMaxFloat(max_dx, x);
    min_dy = EMinFloat(min_dy, y);
    max_dy = EMaxFloat(max_dy, y);

    TRANSFORM(x,y, ca, sa, -sa, ca, 0, 0, -xo, height-yo);
    min_dx = EMinFloat(min_dx, x);
    max_dx = EMaxFloat(max_dx, x);
    min_dy = EMinFloat(min_dy, y);
    max_dy = EMaxFloat(max_dy, y);


    for (y = min_dy; y <= max_dy; y++) {
	for (x = min_dx; x <= max_dx; x++) {
	    float xsf, ysf;
	    int xs, ys;

	    TRANSFORM(xsf,ysf, ca, -sa, sa, ca, xc_src, yc_src, x, y);
	    xs = xsf; //nearbyintf(xsf); // round(xsf);
	    ys = ysf; // nearbyintf(ysf); // round(ysf);

	    if (EPointXYInRect(xs, ys, &sr)) {
		int xd = x + xc_dst;
		int yd = y + yc_dst;

		if (EPointXYInRect(xd, yd, &dr)) {
		    u_int8_t* dst_addr = EPIXEL_ADDR(dst,xd,yd);
		    EPixel_t p;
		    if (flags & EFLAG_AALIAS)
			p = epixel_interp(src, xsf, ysf);
		    else {
			u_int8_t* src_addr = EPIXEL_ADDR(src,xs,ys);
			p = EPixelUnpack(src_pt, src_addr);
		    }
		    put_apixel(dst_addr, dst_pt, flags, p);
		}
	    }
	}
    }
}


void EPixmapDrawLine(EPixmap* pic, EGc* gc,
		     int x0, int y0,
		     int x1, int y1)
{
    if (gc == NULL) gc = &default_gc;

    if (gc->line_width == 1) {
	if (y1 == y0)
	    draw_line_horizontal(pic,x0,x1,y0,
				 gc->line_style,
				 gc->foreground_color);
	else
	    draw_line_plain(pic,x0,y0,x1,y1,
			    gc->line_style,
			    gc->foreground_color);
    }
    else if (gc->line_width > 1)
	draw_line_thick(pic,x0,y0,x1,y1,gc->line_width,gc->line_style,
			gc->foreground_color);
}

extern void EPixmapDrawTwinLine(EPixmap* pixmap, EGc* gc,
				int x0, int y0, 
				int x1, int y1,
				int x2, int y2, 
				int x3, int y3)
{
    draw_line_twin(pixmap,x0,y0,x1,y1,x2,y2,x3,y3,
		   gc->line_style, 
		   gc->foreground_color);
}

/*
 * Basic bresenham step, step 
 *   ptr is the current pixel address
 *   x, y are corresponding coordinate (use for range check etc)
 *   sx,sy are steps in x and y (+1 or -1)
 *   syw,sxw  are used to step ptr (rowlength or pixel size)
 *   dx,dy are the original line delta values (constant)
 *   
 */
#define LINESTEP(f,ptr,x,dx,sx,sxw,y,dy,sy,syw)		\
    do {						\
	if ((f) >= 0) {					\
	    ptr += (syw);				\
	    y   += (sy);				\
	    f   -= (dx);				\
	}						\
	ptr += (sxw);					\
	x += (sx);					\
	f += (dy);					\
    } while(0)

/* Flavor of LINESTEP using fixpoint error accumulator
 *  ea and ed MUST be u_int16_t and 
 *  ea must be initialized to 0 before loop
 *  ed must be initialized to ((u_int32_t)dy << 16) / (u_int32_t) dx
 */

#define LINESTEPW(ea,ed,ptr,x,sx,sxw,y,sy,syw)			\
    do {							\
	u_int16_t _tmp = (ea);					\
	ea += (ed);						\
	if ((ea) <= (_tmp)) {	/* wrapped */			\
	    y   += (sy);					\
	    ptr += (syw);					\
	}							\
	x += (sx);						\
	ptr += (sxw);						\
    } while(0)

#if 0
static void draw_line_plain_old(EPixmap* pic,
			    int x0, int y0, 
			    int x1, int y1, int flags, 
			    EPixel_t p)
{
    int dy = y1 - y0;
    int dx = x1 - x0;
    int sx, sy, sxw, syw;
    int x2, x3, y2, y3;
    u_int8_t* ptr;

    x2 = ERectLeft(&pic->clip);
    y2 = ERectTop(&pic->clip);
    x3 = ERectRight(&pic->clip);
    y3 = ERectBottom(&pic->clip);

    if (dy < 0) { dy=-dy; sy=-1; syw=-pic->bytesPerRow; }
    else { sy=1; syw=pic->bytesPerRow; }

    if (dx < 0) { dx=-dx; sx=-1; sxw=-pic->bytesPerPixel; }
    else { sx=1; sxw=pic->bytesPerPixel; }

    if (dx == 0) {
	if (!EInRange(x0, x2, x3))
	    return;
	if (flags&EPIC_LINE_STYLE_NFIRST) { y0 += sy; dy--; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { y1 -= sy; dy--; }
	ptr = EPIXEL_ADDR(pic,x0,y0);
	while(dy > 0) {
	    if (EInRange(y0, y2, y3))
		put_apixel(ptr,pic->pixelType,flags,p);
	    dy--;
	    y0  += sy;
	    ptr += syw;
	}
	return;
    }

    if (dy == 0) {
	int width;
	if (!EInRange(y0, y2, y3))
	    return;
	if (flags&EPIC_LINE_STYLE_NFIRST) { x0 += sx; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { x1 -= sx; }
	x0 = EClipRange(x0, x2, x3);
	x1 = EClipRange(x1, x2, x3);
	if (x0 <= x1) {
	    width = (x1-x0)+1;
	    ptr = EPIXEL_ADDR(pic,x0,y0);
	}
	else {
	    width = (x0-x1)+1;
	    ptr = EPIXEL_ADDR(pic,x1,y0);
	}
	if (((flags&EFLAG_BLEND)==0) || (p.a == EALPHA_OPAQUE))
	    EDirectFillRow(ptr, pic->pixelType, width, p);
	else if (p.a != EALPHA_TRANSPARENT)
	    EDirectFillRowBlend(ptr,pic->pixelType,width,p);
	return;
    }

    ptr = EPIXEL_ADDR(pic,x0,y0);
    dy <<= 1; 
    dx <<= 1;
    if (!(flags&EPIC_LINE_STYLE_NFIRST) && 
	EInRange(x0, x2, x3) && EInRange(y0, y2, y3))
	put_apixel(ptr,pic->pixelType,flags,p);
    if (dx > dy) {
	int n = (dx>>1)-1;
	int f = dy - (dx>>1);
	if (n && (flags&EPIC_LINE_STYLE_NLAST)) n--;
	while (n--) {
	    LINESTEP(f,ptr,x0,dx,sx,sxw,y0,dy,sy,syw);
	    if (EInRange(x0, x2, x3) && EInRange(y0, y2, y3))
		put_apixel(ptr,pic->pixelType,flags,p);
	}
    }
    else {
	int n = (dy>>1)-1;
	int f = dx - (dy>>1);
	if (n && (flags&EPIC_LINE_STYLE_NLAST)) n--;
	while (n--) {
	    LINESTEP(f,ptr,y0,dy,sy,syw,x0,dx,sx,sxw);
	    if (EInRange(x0, x2, x3) && EInRange(y0, y2, y3))
		put_apixel(ptr,pic->pixelType,flags,p);
	}
    }
}
#endif

/*
 * draw_dline:  orthongonal delta line
 *
 */

static void draw_dline(u_int8_t* ptr, int pt, float tk,
		       int d0, int d1,
		       int kt, int ks, int kd, int ku, int kv,
		       int x0, int y0,
		       int sx, int sy,
		       int sxw, int syw,
		       int x2, int x3,
		       int y2, int y3,
		       int w,
		       int flags, EPixel_t p)
{
    (void) w;
    if (ku > kv) {
	while (d0 <= tk) {
	    if (EInRange(x0, x2, x3) && EInRange(y0, y2, y3))
		put_apixel(ptr,pt,flags,p);
	    if (d1 < kt) {
		d1 += kv;
		d0 += ku;
	    }
	    else {
		x0 -= sx; ptr -= sxw;
		d1 += kd;
		d0 += ks;
	    }
	    y0 += sy; ptr += syw;
	}
    }
    else {
	while (d0 <= tk) {
	    if (EInRange(x0, x2, x3) && EInRange(y0, y2, y3))
		put_apixel(ptr,pt,flags,p);
	    if (d1 < kt) {
		d1 += ku;
		d0 += kv;
	    }
	    else {
		y0 -= sy; ptr -= syw;
		d1 += kd;
		d0 += ks;
	    }
	    x0 += sx; ptr += sxw;
	}
    }
}

/*
 *  Draw line where line_width > 1
 */

static void draw_line_thick(EPixmap* pic,
			    int x0, int y0, 
			    int x1, int y1, int line_width,
			    int flags, EPixel_t fg)
{
    int dy = y1 - y0;
    int dx = x1 - x0;
    int ku,kv;
    int sx, sy, sxw, syw;
    int x2, x3, y2, y3; /* used for clipping */
    u_int8_t* ptr;
    EPixel_t ag = fg;
    u_int8_t  wl = EPixelLuminance(fg);


    x2 = ERectLeft(&pic->clip);
    y2 = ERectTop(&pic->clip);
    x3 = ERectRight(&pic->clip);
    y3 = ERectBottom(&pic->clip);

    if (dy < 0) { dy=-dy; sy=-1; syw=-pic->bytesPerRow; }
    else { sy=1; syw=pic->bytesPerRow; }

    if (dx < 0) { dx=-dx; sx=-1; sxw=-pic->bytesPerPixel; }
    else { sx=1; sxw=pic->bytesPerPixel; }

    if (dx == 0) {
	int height;

	if (flags&EPIC_LINE_STYLE_NFIRST) { y0 += sy; dy--; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { y1 -= sy; dy--; }
	x1 = x0 + line_width;
	// FIXME mega fat lines (or tiny clip area)
	if (!EInRange(x0,x2,x3) && !EInRange(x1,x2,x3))
	    return;
	x0 = EClipRange(x0, x2, x3);
	x1 = EClipRange(x1, x2, x3);
	line_width = x1-x0;  // new line_width after clip
	y0 = EClipRange(y0, y2, y3);
	y1 = EClipRange(y1, y2, y3);
	if (y0 <= y1) {
	    height = (y1-y0)+1;
	    ptr = EPIXEL_ADDR(pic,x0,y0);
	    ASSERT_RECTANGLE(&pic->clip, x0, y0, line_width, dy);
	}
	else {
	    height = (y0-y1)+1;
	    ptr = EPIXEL_ADDR(pic,x0,y1);
	    ASSERT_RECTANGLE(&pic->clip, x0, y1, line_width, dy);
	}

	if (((flags&EFLAG_BLEND)==0) || (fg.a == EALPHA_OPAQUE))
	    EDirectFillArea(ptr,pic->bytesPerRow,pic->pixelType,
			    line_width,height,fg);
	else if (fg.a != EALPHA_TRANSPARENT)
	    EDirectFillAreaBlend(ptr,pic->bytesPerRow,pic->pixelType,
				 line_width, height,fg);
	return;
    }

    if (dy == 0) {
	int width;

	if (flags&EPIC_LINE_STYLE_NFIRST) { x0 += sx; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { x1 -= sx; }
	y1 = y0 + line_width;
	if (!EInRange(y0,y2,y3) && !EInRange(y1,y2,y3))
	    return;
	y0 = EClipRange(y0, y2, y3);
	y1 = EClipRange(y1, y2, y3);
	line_width = (y1-y0);
	x0 = EClipRange(x0, x2, x3);
	x1 = EClipRange(x1, x2, x3);
	if (x0 <= x1) {
	    width = (x1-x0)+1;
	    ptr = EPIXEL_ADDR(pic,x0,y0);
	    ASSERT_RECTANGLE(&pic->clip, x0, y0, width, line_width);
	}
	else {
	    width = (x0-x1)+1;
	    ptr = EPIXEL_ADDR(pic,x1,y0);
	    ASSERT_RECTANGLE(&pic->clip, x1, y0, width, line_width);
	}
	if (sx < 0) ptr -= ((line_width-1)*pic->bytesPerRow);

	if (((flags&EFLAG_BLEND)==0) || (fg.a == EALPHA_OPAQUE))
	    EDirectFillArea(ptr,pic->bytesPerRow,pic->pixelType,
			    width,line_width,fg);
	else if (fg.a != EALPHA_TRANSPARENT)
	    EDirectFillAreaBlend(ptr,pic->bytesPerRow,pic->pixelType,
				 width,line_width,fg);
	return;
    }

    ptr = EPIXEL_ADDR(pic,x0,y0);

    ku = dx+dx;
    kv = dy+dy;
    if (dx > dy) {
	u_int16_t eacc = 0;
	u_int16_t eadj = ((u_int32_t) dy << 16) / (u_int32_t) dx;
	int kd = kv-ku;  // 2(dy - dx)
	int ks = kv+ku;  // 2(dy + dx)
	int kt = dx-kv;  // dx - 2dy
	int d0=0, d1=0;
	float thickvar = 0.0;
	float tk = 0.0;
	int i=0;

	tk = 2*(line_width+thickvar*i/dx)*sqrt(dx*dx + dy*dy);

	if (flags&EPIC_LINE_STYLE_NLAST) dx--;

	while(i < dx) {
	    u_int8_t w;
	    eacc += eadj;
	    w = eacc >> 8;
	    if (((i != 0) || !(flags&EPIC_LINE_STYLE_NFIRST))) {
		draw_dline(ptr, pic->pixelType, tk,
			   -d0, d1, kt, ks, kd, ku, kv,
			   x0, y0, sx, sy, sxw, syw,
			   x2, x3, y2, y3, line_width,flags,fg);
		if (EInRange(y0-sy, y2, y3))
		    put_wu_apixel(ptr-syw,pic->pixelType,w^255,flags,ag,wl);
	    }
	    if (d0 >= kt) {
		d0 -= ku;
		if (d1 < kt) {
		    y0 += sy; ptr += syw;
		    d1 += kv;
		}
		else {
		    y0 += sy; ptr += syw;
		    d1 += kd;
		    draw_dline(ptr, pic->pixelType, tk,
			       -d0, d1, kt, ks, kd, ku, kv,
			       x0, y0, sx, sy, sxw, syw,
			       x2, x3, y2, y3, line_width,flags,fg);
		}
	    }
	    x0 += sx; ptr += sxw;
	    d0 += kv;
	    i++;
	}
    }
    else {
	u_int16_t eacc = 0;
	u_int16_t eadj = ((u_int32_t) dx << 16) / (u_int32_t) dy;

	int kd = ku-kv;  // 2(dx - dy)
	int ks = ku+kv;  // 2(dx + dy)
	int kt = dy-ku;  // dy - 2dx
	int d0=0, d1=0;
	float thickvar = 0.0;
	float tk = 0.0;
	int i=0;

	tk = 2*(line_width+thickvar*i/dx)*sqrt(dx*dx + dy*dy);

	if (flags&EPIC_LINE_STYLE_NLAST) dy--;

	while(i < dy) {
	    u_int8_t w;
	    eacc += eadj;
	    w = eacc >> 8;
	    if (((i != 0) || !(flags&EPIC_LINE_STYLE_NFIRST))) {
		draw_dline(ptr, pic->pixelType, tk,
			   -d0, d1, kt, ks, kd, ku, kv,
			   x0, y0, sx, sy, sxw, syw,
			   x2, x3, y2, y3, line_width,flags,fg);
		if (EInRange(x0-sx,x2,x3))
		    put_wu_apixel(ptr-sxw,pic->pixelType,w^255,flags,ag,wl);
	    }
	    if (d0 >= kt) {
		d0 -= kv;
		if (d1 < kt) {
		    x0 += sx; ptr += sxw;
		    d1 += ku;
		}
		else {
		    x0 += sx; ptr += sxw;
		    d1 += kd;
		    draw_dline(ptr, pic->pixelType, tk,
			       -d0, d1, kt, ks, kd, ku, kv,
			       x0, y0, sx, sy, sxw, syw,
			       x2, x3, y2, y3,line_width,flags,fg);
		}
	    }
	    y0 += sy; ptr += syw;
	    d0 += ku;
	    i++;
	}
    }
}

void draw_line_horizontal(EPixmap* pic, int x1, int x2, int y, 
			  int flags, EPixel_t fg)
{
    int xl, xr;
    u_int8_t* ptr;

    if (y < ERectTop(&pic->clip))
	return;
    if (y > ERectBottom(&pic->clip))
	return;
    if (x1 > x2) ESwapInt(x1,x2);

    if (x2 < (xl = ERectLeft(&pic->clip)))
	return;
    if (x1 > (xr = ERectRight(&pic->clip)))
	return;

    x1 = EClipRange(x1, xl, xr);
    x2 = EClipRange(x2, xl, xr);
    ptr = EPIXEL_ADDR(pic,x1,y);
    if (((flags&EFLAG_BLEND)==0) || (fg.a == EALPHA_OPAQUE))
	EDirectFillRow(ptr, pic->pixelType, (x2-x1)+1, fg);
    else if (fg.a != EALPHA_TRANSPARENT)
	EDirectFillRowBlend(ptr,pic->pixelType,(x2-x1)+1,fg);
}

static void draw_line(EPixmap* pic,
		      int x0, int y0, 
		      int x1, int y1, 
		      unsigned int line_width,
		      int flags, 
		      EPixel_t p)
{
    if (line_width <= 1) {
/*	if (flags & EFLAG_AALIAS)
	    draw_line_aalias(pic, x0, y0, x1, y1, flags, p); 
	 else */
	    draw_line_plain(pic, x0, y0, x1, y1, flags, p);
    }
    else
	draw_line_thick(pic, x0, y0, x1, y1, line_width, flags, p);
}

/* aalias line:
 *    antialiased line drawing
 */
#if 0
void draw_line_aalias_old(EPixmap* pic,
			  int x0, int y0, 
			  int x1, int y1,
			  int flags, EPixel_t fg)
{
    int dy = y1 - y0;
    int dx = x1 - x0;
    int sx, sy, sxw, syw;
    int x2, x3, y2, y3; /* used for clipping */
    int pt = pic->pixelType;
    u_int8_t* ptr;

    x2 = ERectLeft(&pic->clip);
    y2 = ERectTop(&pic->clip);
    x3 = ERectRight(&pic->clip);
    y3 = ERectBottom(&pic->clip);

    if (dy < 0) { dy=-dy; sy=-1; syw=-pic->bytesPerRow; }
    else { sy=1; syw=pic->bytesPerRow; }

    if (dx < 0) { dx=-dx; sx=-1; sxw=-pic->bytesPerPixel; }
    else { sx=1; sxw=pic->bytesPerPixel; }

    // Vertical line
    if (dx == 0) {
	if (!EInRange(x0, x2, x3))
	    return;
	if (flags&EPIC_LINE_STYLE_NFIRST) { y0 += sy; dy--; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { y1 -= sy; dy--; }
	ptr = EPIXEL_ADDR(pic,x0,y0);
	while(dy > 0) {
	    if (EInRange(y0, y2, y3))
		put_apixel(ptr,pt,flags,fg);
	    dy--;
	    y0  += sy;
	    ptr += syw;
	}
	return;
    }

    // Horizontal line
    if (dy == 0) {
	int width;
	if (!EInRange(y0, y2, y3))
	    return;
	if (flags&EPIC_LINE_STYLE_NFIRST) { x0 += sx; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { x1 -= sx; }
	/* FIXME */
	x0 = EClipRange(x0, x2, x3);
	x1 = EClipRange(x1, x2, x3);
	if (x0 <= x1) {
	    width = (x1-x0)+1;
	    ptr = EPIXEL_ADDR(pic,x0,y0);
	}
	else {
	    width = (x0-x1)+1;
	    ptr = EPIXEL_ADDR(pic,x1,y0);
	}
	if (((flags&EFLAG_BLEND)==0) || (fg.a == EALPHA_OPAQUE))
	    EDirectFillRow(ptr, pt, width, fg);
	else if (fg.a != EALPHA_TRANSPARENT)
	    EDirectFillRowBlend(ptr,pt,width,fg);
	return;
    }

    // Diagonal line
    if (dx == dy) {
	if (flags&EPIC_LINE_STYLE_NFIRST) { x0 += sx; dy--; }
	if (flags&EPIC_LINE_STYLE_NLAST)  { x1 -= sx; dy--; }
	ptr = EPIXEL_ADDR(pic,x0,y0);
	while(dy > 0) {
	    if (EInRange(x0,x2,x3) && EInRange(y0, y2, y3))
		put_apixel(ptr,pt,flags,fg);
	    dy--;
	    y0  += sy;
	    x0  += sx;
	    ptr += syw;
	    ptr += sxw;
	}
	return;
    }

    ptr = EPIXEL_ADDR(pic,x0,y0);
    
    if (!(flags&EPIC_LINE_STYLE_NFIRST) && 
	EInRange(x0, x2, x3) && EInRange(y0, y2, y3))
	put_apixel(ptr,pt,flags,fg);
    
    if (dx > dy) {
	u_int16_t eacc = 0;
	u_int16_t eadj = ((u_int32_t) dy << 16) / (u_int32_t) dx;
	u_int8_t  wl = EPixelLuminance(fg);

	while (--dx) {
	    u_int8_t w;

	    LINESTEPW(eacc,eadj,ptr,x0,sx,sxw,y0,sy,syw);
	    w = eacc >> 8;

	    if (EInRange(x0,x2,x3)) {
		if (EInRange(y0,y2,y3))
		    put_wu_apixel(ptr,pt,w,flags,fg,wl);
		if (EInRange(y0+sy, y2, y3))
		    put_wu_apixel(ptr+syw,pt,w^255,flags,fg,wl);
	    }
	}
	if (!(flags&EPIC_LINE_STYLE_NLAST) && 
	    EInRange(x0,x2,x3) && EInRange(y0,y2,y3))
	    put_apixel(ptr,pt,flags,fg);
    }
    else {  /* dy > dx */
	u_int16_t eacc = 0;
	u_int16_t eadj = ((u_int32_t) dx << 16) / (u_int32_t) dy;
	u_int8_t wl = EPixelLuminance(fg);

	while (--dy) {
	    u_int8_t w;
	    
	    LINESTEPW(eacc,eadj,ptr,y0,sy,syw,x0,sx,sxw);
	    w = eacc >> 8;
	    if (EInRange(y0, y2, y3)) {
		if (EInRange(x0,x2,x3))
		    put_wu_apixel(ptr,pt,w,flags,fg,wl);
		if (EInRange(x0+sx,x2,x3))
		    put_wu_apixel(ptr+sxw,pt,w^255,flags,fg,wl);
	    }
	}
	if (!(flags&EPIC_LINE_STYLE_NLAST) && 
	    EInRange(x0,x2,x3) && EInRange(y0,y2,y3))
	    put_apixel(ptr,pt,flags,fg);
    }
}
#endif

/*
 * tex_line:
 *
 * Draw a line with pixel from an other pixmap (texture)
 * The texture pixels are taken from 
 *   position tx0 -> tx0
 * from a row determined by ty
 * 
 * FIXME user interpolation on y
 */

void EPixmapTexLine(EPixmap* pic, EGc* gc,
		    int x0, int y0, 
		    int x1, int y1,
		    EPixmap* tex,
		    int tx0, int tx1, float ty)
{
    int dy = y1 - y0;
    int dx = x1 - x0;
    int dd;
    int sx, sy, sxw, syw;
    int x2, x3, y2, y3; /* used for clipping */
    u_int8_t* ptr;
    EPixel_t p;
    float ts, tx;
    int tix, tiy;
    int flags;

    if (gc == NULL) gc = &default_gc;
    flags = gc->line_style;

    tx0 = EClipRange(tx0, 0, tex->width-1);
    tx1 = EClipRange(tx1, 0, tex->width-1);

    x2 = ERectLeft(&pic->clip);
    y2 = ERectTop(&pic->clip);
    x3 = ERectRight(&pic->clip);
    y3 = ERectBottom(&pic->clip);

    if (dy < 0) { 
	dy=-dy; 
	sy=-1;
	syw=-pic->bytesPerRow; 
    } 
    else { 
	sy=1;
	syw=pic->bytesPerRow; 
    }
    if (dx < 0) { 
	dx=-dx;
	sxw =-pic->bytesPerPixel; 
	sx=-1; 
    } 
    else 
    {
	sxw=pic->bytesPerPixel; 
	sx=1;
    }
    /* calculate the texture step */
    dd = (dx > dy) ? dx : dy;   /* max of abs(dx) and abd(dy) */
    ts = (dd == 0) ? 0.0 : ((tx1-tx0) / ((float)dd));
    tx = tx0;  /* inital tx    */

    dy <<= 1;
    dx <<= 1;

    ptr = EPIXEL_ADDR(pic,x0,y0);
    if (!(flags&EPIC_LINE_STYLE_NFIRST) &&
	EInRange(x0, x2, x3) && EInRange(y0, y2, y3)) {
	tix = tx; tiy = ty;
	p = EPixelUnpack(tex->pixelType, EPIXEL_ADDR(tex, tix, tiy));
	put_apixel(ptr,pic->pixelType,flags,p);
    }
    tx += ts;

    if (dx > dy) {
	int f = dy - (dx >> 1);
	while (x0 != x1) {
	    LINESTEP(f,ptr,x0,dx,sx,sxw,y0,dy,sy,syw);
	    if ((x0 == x1) && (flags&EPIC_LINE_STYLE_NLAST))
		break;
	    if (EInRange(x0, x2, x3) && EInRange(y0, y2, y3)) {
		tix = tx; tiy = ty;
		p = EPixelUnpack(tex->pixelType, EPIXEL_ADDR(tex, tix, tiy));
		put_apixel(ptr,pic->pixelType,flags,p);
	    }
	    tx += ts;
	}
    }
    else {
	int f = dx - (dy >> 1);
	while (y0 != y1) {
	    LINESTEP(f,ptr,y0,dy,sy,syw,x0,dx,sx,sxw);
	    if ((y0 == y1) && (flags&EPIC_LINE_STYLE_NLAST))
		break;
	    if (EInRange(x0, x2, x3) && EInRange(y0, y2, y3)) {
		tix = tx; tiy = ty;
		p = EPixelUnpack(tex->pixelType, EPIXEL_ADDR(tex, tix, tiy));
		put_apixel(ptr,pic->pixelType,flags,p);
	    }
	    tx += ts;
	}
    }
}

/*
 * Draw linear curve segment: (also called line)
 *   A=(x0,y0),  B=(x1,y1)
 *
 *  P(t) = A(1-t) + Bt       (0 <= t <= 1)
 *  P'(t) = B - A
 *
 *  P(0) = A
 *  P(1) = B
 */
void draw_linear_curve(EPixmap*pic,
		       int x0, int y0,
		       int x1, int y1,
		       int flags, EPixel_t p)
{
    (void) pic;
    (void) x0;
    (void) y0;
    (void) x1;
    (void) y1;
    (void) flags;
    (void) p;
}

/*
 * Draw quadratic curve segment:
 *   A=(x0,y0),  B=(x1,y1),  C=(x2,y2)
 *
 *   P(t)  = A(1-t)^2 + 2Bt(1-t) + Ct^2
 *         = A(1 - 2t + t^2) + B(2t-2t^2) + Ct^2
 *         = (A-2B+C)t^2 + 2(B-A)t + A
 *
 *   P(0)   = A
 *   P(1/2) = 1/4(A-2B+C)+(B-A)+A = 1/4(A + 2B + C)
 *   P(1)   = C
 *
 *   P'(t)   = 2(A-2B+C)t + 2(B-A)
 *   P'(0)   = 2(B-A)
 *   P'(1/2) = A-2B+C+2B-2A = (C-A)
 *   p'(1)   = 2A-4B+2C+2B-2A = 2(C-B)
 *
 *  Idea: try trace the P'(t) line
 *
 *  P(0) = A
 *  next: A1 point bresenham next on line P'(0)
 *  find t1 from A1
 *  P(t1) 
 *  next: A2 point bresenham next on line P'(t1)
 *  ...
 */
void draw_quadratic_curve(EPixmap* pic,
			  int x0, int y0,
			  int x1, int y1,
			  int x2, int y2,
			  int flags, EPixel_t p)
{
    (void) pic;
    (void) x0;
    (void) y0;
    (void) x1;
    (void) y1;
    (void) x2;
    (void) y2;
    (void) flags;
    (void) p;
}

/* 
 *  P(t) = A(1-t)^3 + 3Bt(1-t)^2 + 3Ct^2(1-t) + Dt^3
 */
void draw_cubic_curve(EPixmap* pic,
		      int x0, int y0,
		      int x1, int y1,
		      int x2, int y2,
		      int x3, int y3,
		      int flags, EPixel_t p)
{
    (void) pic;
    (void) x0;
    (void) y0;
    (void) x1;
    (void) y1;
    (void) x2;
    (void) y2;
    (void) x3;
    (void) y3;
    (void) flags;
    (void) p;

}
