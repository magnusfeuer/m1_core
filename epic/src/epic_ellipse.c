/*
 * Ellipse drawing algorithms
 *
 */

#include "epic.h"
#include <math.h>

extern void draw_line_horizontal(EPixmap* pic, int x1, int x2, int y, 
				 int flags, EPixel_t fg);

/* plot 4 pixels - one in each quadrant */
static inline void plot_ellipse4(EPixmap* pic,
				 int xc, int yc,
				 int x, int y, int flags, 
				 EPixel_t color)
{
    EPixmapPutPixel(pic,xc-x,yc+y,flags,color);
    EPixmapPutPixel(pic,xc+x,yc+y,flags,color);
    EPixmapPutPixel(pic,xc-x,yc-y,flags,color);
    EPixmapPutPixel(pic,xc+x,yc-y,flags,color);
}

/* draw upper and lower ellipse fill line */
static inline void line_ellipse2(EPixmap* pic,
				 int xc, int yc,
				 int x, int y, int flags,
				 EPixel_t color)
{
    draw_line_horizontal(pic,xc-x,xc+x,yc-y,flags,color);
    draw_line_horizontal(pic,xc-x,xc+x,yc+y,flags,color);
}


// General idea:
// trace two ellipses and draw border from 
// outer to inner x and fill interior from inner x1, to inner x2
//  x1   x2           x3  x4
//   | B  |    FILL   | B |
//   |      BORDER        |   (inner y is done)
//
void draw_ellipse_border(EPixmap* pic, EGc* gc,
			 int x, int y,
			 unsigned int width, unsigned int height)
{
    unsigned int a = width  >> 1;
    unsigned int b = height >> 1;
    int xc = x + a;
    int yc = y + b;
    unsigned int border_width = gc->border_width;
    EPixel_t border = gc->border_color;
    EPixel_t fill   = gc->fill_color;
    EFlags_t ff     = gc->fill_style;
    EFlags_t bf     = gc->border_style;
    int do_fill = (ff != EPIC_FILL_STYLE_NONE);
    // Inner circle
    int xi  = a;
    int yi  = 0;
    // FIXME: only allow a,b to be in 16 bit range to avoid overflow...
    unsigned long a2_i = a*a;
    unsigned long b2_i = b*b;
    long pya_i = a2_i;
    long pxb_i = (2*a-1)*b2_i;
    long f_i = 0;
    long fx_i, fxy_i, fy_i;
    int do_yi = 1;
    // Outer circle
    int xo  = a+border_width;
    int yo  = 0;
    unsigned long a2_o = (a+border_width)*(a+border_width);
    unsigned long b2_o = (b+border_width)*(b+border_width);
    long pya_o = a2_o;
    long pxb_o = (2*(a+border_width)-1)*b2_o;
    long f_o = 0;
    long fx_o, fxy_o, fy_o;
    int do_yo = 1;

    if ((a==0) || (b==0))
	return;

    while(xi >= 0) {
	if ((xi == 0) && (yi == 0)) {
	    if (do_fill) {
		EPixmapPutPixel(pic, xc, yc, bf, border);
	    }
	    else
		EPixmapPutPixel(pic, xc, yc, bf, border);
	}
	else if (xi == 0) {
	    if (do_fill) {
		EPixmapPutPixel(pic,xc,yc+yi, bf, border);
		EPixmapPutPixel(pic,xc,yc-yi, bf, border);
	    }
	    else {
		EPixmapPutPixel(pic,xc,yc+yi, bf, border);
		EPixmapPutPixel(pic,xc,yc-yi, bf, border);
	    }
	}
	else if (yi == 0) {
	    if (border_width > 1) {
		if (do_yi) {
		    draw_line_horizontal(pic,xc-xo,xc-xi,yc,bf,border);
		    draw_line_horizontal(pic,xc+xi,xc+xo,yc,bf,border);
		}
	    }
	    else {
		EPixmapPutPixel(pic,xc-xo,yc,bf,border);
		EPixmapPutPixel(pic,xc+xo,yc,bf,border);
	    }
	    if (do_fill) {
		if (do_yi)
		    draw_line_horizontal(pic,xc-xi+1,xc+xi-1,yc,ff,fill);
	    }
	}
	else {
	    if (border_width > 1) {
		if (do_yi) {
		    draw_line_horizontal(pic,xc-xo,xc-xi,yc+yi,bf,border);
		    draw_line_horizontal(pic,xc+xi,xc+xo,yc+yi,bf,border);
		    draw_line_horizontal(pic,xc-xo,xc-xi,yc-yi,bf,border);
		    draw_line_horizontal(pic,xc+xi,xc+xo,yc-yi,bf,border);
		}
	    }
	    else {
		plot_ellipse4(pic, xc, yc, xo, yi, bf, border);
	    }
	    if (do_fill) {
		if (do_yi) {
		    line_ellipse2(pic, xc, yc, xi-1, yi, ff, fill);
		}
	    }
	}

	// Update Inner circle
	fx_i  = f_i - pxb_i;
	fxy_i = f_i - pxb_i + pya_i;
	fy_i  = f_i + pya_i;

	if (EAbsLess(fx_i,fxy_i) && EAbsLess(fx_i,fy_i)) {
	    do_yi=0; xi--; f_i = fx_i;
	    pxb_i -= (b2_i + b2_i);
	}
	else if (EAbsLess(fxy_i,fy_i)) {
	    do_yi=1; xi--; yi++; f_i=fxy_i;
	    pya_i += (a2_i + a2_i); pxb_i -= (b2_i + b2_i);
	}
	else {
	    do_yi=1; yi++; f_i=fy_i; pya_i += (a2_i + a2_i);
	}

	while (yo < yi) {
	    // Update Outer circle
	    fx_o  = f_o - pxb_o;
	    fxy_o = f_o - pxb_o + pya_o;
	    fy_o  = f_o + pya_o;

	    if (EAbsLess(fx_o,fxy_o) && EAbsLess(fx_o,fy_o)) {
		do_yo=0; xo--; f_o = fx_o;
		pxb_o -= (b2_o + b2_o);
	    }
	    else if (EAbsLess(fxy_o,fy_o)) {
		do_yo=1; xo--; yo++; f_o=fxy_o;
		pya_o += (a2_o + a2_o); pxb_o -= (b2_o + b2_o);
	    }
	    else {
		do_yo=1; yo++; f_o=fy_o; pya_o += (a2_o + a2_o);
	    }
	}
    }

    /* Do remaining outer cirlce */
    while(xo >= 0) {
	// fprintf(stderr, "xo=%d, yo=%d, yi=%d\n", xo, yo, yi);
	if ((xo == 0) && (yo == 0)) {
	    EPixmapPutPixel(pic, xc, yc, bf, border);
	}
	else if (xo == 0) {
	    EPixmapPutPixel(pic,xc,yc+yo, bf, border);
	    EPixmapPutPixel(pic,xc,yc-yo, bf, border);
	}
	else if (yo == 0) {
	    if (border_width > 1) {
		if (do_yo) {
		    draw_line_horizontal(pic,xc-xo,xc+xo,yc,bf,border);
		}
	    }
	    else {
		EPixmapPutPixel(pic,xc-xo,yc,bf,border);
		EPixmapPutPixel(pic,xc+xo,yc,bf,border);
	    }
	}
	else {
	    if (border_width > 1) {
		if (do_yo) {
		    line_ellipse2(pic, xc, yc, xo, yo, bf, border);
		}
	    }
	    else {
		plot_ellipse4(pic, xc, yc, xo, yo, bf, border);
	    }
	}

	// Update Outer circle
	fx_o  = f_o - pxb_o;
	fxy_o = f_o - pxb_o + pya_o;
	fy_o  = f_o + pya_o;

	if (EAbsLess(fx_o,fxy_o) && EAbsLess(fx_o,fy_o)) {
	    do_yo=0; xo--; f_o = fx_o;
	    pxb_o -= (b2_o + b2_o);
	}
	else if (EAbsLess(fxy_o,fy_o)) {
	    do_yo=1; xo--; yo++; f_o=fxy_o;
	    pya_o += (a2_o + a2_o); pxb_o -= (b2_o + b2_o);
	}
	else {
	    do_yo=1; yo++; f_o=fy_o; pya_o += (a2_o + a2_o);
	}
    }
}


/*
 * Draw ellipse when:
 *   border_width = 0   (or no border)
 * and
 *   no fill & line_width=1
 * or fill
 *  x^2/a^2 + y^2/b^2 = 1
 * ==
 *  x^2*b^2 + y^2*a^2 = a^2*b^2
 *  x = a*sqrt(1 - y^2/b^2)
 *
 */


void draw_ellipse(EPixmap* pic, EGc* gc, 
		  int x, int y,
		  unsigned int width, unsigned int height)
{
    unsigned int a = width  >> 1;
    unsigned int b = height >> 1;
    int xc = x + a;
    int yc = y + b;
    int do_y = 1;
    int do_fill;
    int do_aalias;
    // FIXME: only allow a,b to be in 16 bit range to avoid overflow...
    unsigned long a2 = a*a;
    unsigned long b2 = b*b;
    long pya = a2;
    long pxb = (2*a-1)*b2;
    long f = 0;
    long fx, fxy, fy;
    unsigned long ax, axy, ay;
    EFlags_t ff = gc->fill_style;
    EFlags_t lf = gc->line_style;
    EPixel_t fc = gc->fill_color;
    EPixel_t lc = gc->foreground_color;
    int xo  = a;
    int yo = 0;

    if ((a==0) || (b==0))
	return;

    do_fill = (ff != EPIC_FILL_STYLE_NONE);
    do_aalias = (ff & EPIC_FILL_STYLE_AALIAS);  // FILL


    while((xo >= 0) && (yo <= (int)b)) {
	if ((xo == 0) && (yo == 0)) {
	    if (do_fill) {
		if (do_y)
		    EPixmapPutPixel(pic, xc, yc, ff, fc);
	    }
	    else
		EPixmapPutPixel(pic, xc, yc, lf, lc);
	}
	else if (xo == 0) {
	    if (do_fill) {
		if (do_y) {
		    EPixmapPutPixel(pic,xc,yc+yo, ff, fc);
		    EPixmapPutPixel(pic,xc,yc-yo, ff, fc);
		}
	    }
	    else {
		EPixmapPutPixel(pic,xc,yc+yo, lf, lc);
		EPixmapPutPixel(pic,xc,yc-yo, lf, lc);
	    }
	}
	else if (yo == 0) {
	    if (do_fill) {
		if (do_y) {
		    if (do_aalias) {
			EPixel_t c = lc;
			double xt = a*sqrt(1-(double)(yo*yo)/(double)b2);
			int alpha = c.a*(xo - xt);
			draw_line_horizontal(pic,xc-xo,xc+xo,yc,ff,c);

			c.a = (c.a-alpha);
			EPixmapPutPixel(pic,xc-xo-1,yc,ff,c);
			EPixmapPutPixel(pic,xc+xo+1,yc,ff,c);			
		    }
		    else {
			draw_line_horizontal(pic,xc-xo,xc+xo,yc,ff,fc);
		    }
		}
	    }
	    else {
		EPixmapPutPixel(pic,xc-xo,yc,lf,lc);
		EPixmapPutPixel(pic,xc+xo,yc,lf,lc);
	    }
	}
	else {
	    if (do_fill) {
		if (do_y) {
		    if (do_aalias) {
			EPixel_t c = fc;
			double xt = a*sqrt(1-(double)(yo*yo)/(double)b2);
			int alpha = abs(90*(xo-xt)+37); // fc.a*(xo - xt);

			line_ellipse2(pic, xc, yc, xo, yo, ff, fc);

			c.a = alpha; // fc.a - alpha;
			plot_ellipse4(pic, xc, yc, xo+1, yo, ff, c);
		    }
		    else {
			line_ellipse2(pic, xc, yc, xo, yo, ff, fc);
		    }
		}
	    }
	    else {
		if (do_aalias) {
		    if (do_y) {
			EPixel_t c = lc;
			double xt = a*sqrt(1.0-(double)(yo*yo)/(double)b2);
			int alpha = c.a*(xo - xt);

			c.a = alpha;
			plot_ellipse4(pic, xc, yc, xo, yo, lf, c);

			c.a = (lc.a-alpha);
			plot_ellipse4(pic, xc, yc, xo+1, yo, lf, c);
		    }
		    else {
			EPixel_t c = lc;
			double yt = b*sqrt(1.0-(double)(xo*xo)/(double)a2);
			int alpha = c.a*(yo - yt);

			c.a = alpha;
			plot_ellipse4(pic, xc, yc, xo, yo, lf, c);

			c.a = (lc.a-alpha);
			plot_ellipse4(pic, xc, yc, xo, yo+1, lf, c);
		    }
		}
		else {
		    plot_ellipse4(pic, xc, yc, xo, yo, lf, lc);
		}
	    }
	}

	fx  = f - pxb;
	fxy = f - pxb + pya;
	fy  = f + pya;
	ax  = labs(fx);
	axy = labs(fxy);
	ay  = labs(fy);

	if ((ax < axy) && (ax < ay)) {
	    do_y=0; xo--; f = fx; pxb -= (b2 + b2);
	}
	else if (axy < ay) {
	    do_y=1; xo--; yo++; f=fxy; pya += (a2 + a2); pxb -= (b2 + b2);
	}
	else {
	    do_y=1; yo++; f=fy; pya += (a2 + a2);
	}
    }
}

void EPixmapDrawEllipse(EPixmap* pic, EGc* gc, 
			int x, int y,
			unsigned int width, unsigned int height)
{
    if (gc == NULL) gc = &default_gc;    

    if (((gc->border_style & EPIC_BORDER_STYLE_NBORDER) == 
	 EPIC_BORDER_STYLE_NBORDER) || (gc->border_width == 0)) {
	draw_ellipse(pic, gc, x, y, width, height);
    }
    else {
	draw_ellipse_border(pic, gc, x, y, width, height);
    }
}
	

