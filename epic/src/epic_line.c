/*
 * Line segment drawing algorithms
 *
 */

#include "epic.h"

#define L(name,sfx) L_##name##_##sfx

#define LINE_DECL(i)			      \
    int L(x0,i);			      \
    int L(y0,i);			      \
    int L(x1,i);			      \
    int L(y1,i);			      \
    int L(dx,i);			      \
    int L(dy,i);			      \
    int L(sx,i);			      \
    int L(sy,i);			      \
    int L(wsx,i);			      \
    int L(wsy,i);			      \
    int L(err,i);			      \
    u_int8_t* L(ptr,i)

#define LINE_SWAPIN(line,i) do {		    \
	L(x0,i) = (line)->p0.x;			    \
	L(y0,i) = (line)->p0.y;			    \
	L(x1,i) = (line)->p1.x;			    \
	L(y1,i) = (line)->p1.y;				    \
	L(dx,i) = (line)->delta.x;			    \
	L(dy,i) = (line)->delta.y;			    \
	L(sx,i) = (line)->s.x;				    \
	L(sy,i) = (line)->s.y;				    \
	L(wsx,i) = (line)->ws.x;			    \
	L(wsy,i) = (line)->ws.y;			    \
	L(err,i) = (line)->err;				    \
	L(ptr,i) = (line)->ptr;				    \
    } while(0)

#define LINE_SWAPOUT(line,i) do {		\
    (line)->p0.x = L(x0,i);			\
    (line)->p0.y = L(y0,i);			\
    (line)->err  = L(err,i);			\
    (line)->ptr  = L(ptr,i);			\
    } while(0)

/* step on x axes until it's time to move on */
#define LINE_STEP_AXIS(err,ptr,x,dy,sx,wsx) \
    do { \
	ptr += (wsx);					\
	x += (sx);					\
	err += (dy);					\
    } while(0)

/* one line step, run both ways just reverse arguments */
#define LINE_STEP(err,ptr,x,dx,sx,wsx,y,dy,sy,wsy)	\
    do {						\
	if ((err) >= 0) {				\
	    ptr += (wsy);				\
	    y   += (sy);				\
	    err -= (dx);				\
	}						\
	LINE_STEP_AXIS(err,ptr,x,dy,sx,wsx);		\
    } while(0)


typedef struct {
    EPoint_t p0;     /* current/start point */
    EPoint_t p1;     /* Stop  point */
    EPoint_t s;      /* x, y increament +1 -1 or 0 */
    EPoint_t ws;     /* ptr increament bytesPerPixel/bytesPerRow */
    EPoint_t delta;  /* 2*|dx|, 2*|dy| */
    int      err;    /* 2*error distance from line */
    u_int8_t* ptr;  /* current pixel address */
} ELine_t;

extern void draw_line_horizontal(EPixmap* pic, int x1, int x2, int y, 
				 int flags, EPixel_t fg);

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

/* Setup inital point and pixel address */
static void set_p0(EPixmap* pixmap, ELine_t* line, int x, int y)
{
    memset(line, 0, sizeof(ELine_t));
    EPointSet(&line->p0, x, y);
    line->ptr = EPIXEL_ADDR(pixmap,x,y);
}

/* Setup next point, step and delta parameters */
static void set_p1(EPixmap* pixmap, ELine_t* line, int x, int y)
{
    int dy = y - line->p0.y;
    int dx = x - line->p0.x;

    EPointSet(&line->p1, x, y);

    if (dy < 0) {
	line->delta.y  = (dy = -dy << 1);
	line->s.y      = -1;
	line->ws.y     = -pixmap->bytesPerRow;
    }
    else {
	line->delta.y = (dy = dy << 1);
	line->s.y     = 1;
	line->ws.y    = pixmap->bytesPerRow;
    }

    if (dx < 0) {
	line->delta.x = (dx = -dx << 1);
	line->s.x     = -1;
	line->ws.x    = -pixmap->bytesPerPixel; 
    }
    else { 
	line->delta.x = (dx = dx<<1);
	line->s.x     = 1;
	line->ws.x    = pixmap->bytesPerPixel;
    }
    if (dx > dy)
	line->err = dy - (dx>>1);
    else
	line->err = dx - (dy>>1);
}


/*
 * Trace a bresenham line a also obey clipping region
 */
void break_here()
{
    fprintf(stderr, "BREAK HERE\n");
}

/* Trace line with alpha blending and antialiasing 
 * The L = luminance
 *     L(fg,1.0) = luminance of the foreground pixel with alpha=1
 *     L(fg,0.5) + L(fg,0.5) = L(fg,1.0) ?
 *
 *
 */
void trace_aalias_line_1(EPixmap* pic, ELine_t* line, int flags, EPixel_t fg)
{
    int pt = pic->pixelType;
    EPixel_t fa = fg;
    int a  = fa.a;
    int ae;

    LINE_DECL(0);
    LINE_SWAPIN(line,0);
    
    if (L(dx,0) > L(dy,0)) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0));
	    /* FIXME more scaling needed for luminance to be correct ! */
	    ae = (L(dx,0) == 0) ? 0 : ((a*L(err,0)) / L(dx,0));

	    if (ae < 0) {
		/* put alias point below */
		if (EPointXYInRect(L(x0,0),L(y0,0)-L(sy,0), &pic->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-L(wsy,0), pt, flags, fa);
		}
		if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0), pt, flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point abow */
		if (EPointXYInRect(L(x0,0),L(y0,0)+L(sy,0), &pic->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+L(wsy,0), pt, flags, fa);
		}
		if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pt, flags, fa);
		}
	    }
	    else if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pt, flags, fg);
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0));
	    /* FIXME more scaling needed for luminance to be correct ! */
	    ae = (L(dy,0) == 0) ? 0 : ((a*L(err,0)) / L(dy,0));
	    if (ae < 0) {
		/* put alias point left */
		if (EPointXYInRect(L(x0,0)-L(sx,0),L(y0,0), &pic->clip)) {
		    fa.a = -ae;
		    put_apixel(L(ptr,0)-L(wsx,0), pt, flags, fa);
		}
		if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a+ae;
		    put_apixel(L(ptr,0), pt, flags, fa);
		}
	    }
	    else if (ae > 0) {
		/* put alias point right */
		if (EPointXYInRect(L(x0,0)+L(sx,0),L(y0,0), &pic->clip)) {
		    fa.a = ae;
		    put_apixel(L(ptr,0)+L(wsx,0), pt, flags, fa);
		}
		if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		    fa.a = a-ae;
		    put_apixel(L(ptr,0), pt, flags, fa);
		}
	    }
	    else if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pt, flags, fg);
	    }
	}
    }
    LINE_SWAPOUT(line,0);
}

/* Trace a line with no antialiasing */
void trace_line_1(EPixmap* pic, ELine_t* line, int flags, EPixel_t fg)
{
    int pt = pic->pixelType;
    LINE_DECL(0);
    LINE_SWAPIN(line,0);

    if (L(dx,0) > L(dy,0)) {
	while(L(x1,0) != L(x0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0));
	    if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pt, flags, fg);
	    }
	}
    }
    else {
	while(L(y1,0) != L(y0,0)) {
	    LINE_STEP(L(err,0),L(ptr,0),
		      L(y0,0),L(dy,0),L(sy,0),L(wsy,0),
		      L(x0,0),L(dx,0),L(sx,0),L(wsx,0));
	    if (EPointXYInRect(L(x0,0),L(y0,0), &pic->clip)) {
		put_apixel(L(ptr,0), pt, flags, fg);
	    }
	}
    }
    LINE_SWAPOUT(line,0);
}


/*
 *  draw horizontal line between line trace 1 and line trace 2
 *  (if possible)
 */
void trace_line_2(EPixmap* pic, ELine_t* line1, ELine_t* line2, 
		  int flags, EPixel_t fg)
{
    int swapped;
    int n;
    LINE_DECL(1);
    LINE_DECL(2);

    /* Lines are assumed to run in the same y direction */
    if ((line1->s.y != line2->s.y) ||
	(line1->delta.y == 0))
	return;

    if ((n = EIntersectRangeLength(line1->p0.y, line1->p1.y,
				   line2->p0.y, line2->p1.y)) == 0)
	return;

    /* Swap in lines so that line1 starts first */
    if (((line1->p0.y <= line2->p0.y) && (line1->s.y > 0)) ||
	((line1->p0.y >= line2->p0.y) && (line1->s.y < 0))) {
	swapped = 0;
	LINE_SWAPIN(line1,1);
	LINE_SWAPIN(line2,2);
    }
    else {
	swapped = 1;
	LINE_SWAPIN(line1,2);
	LINE_SWAPIN(line2,1);
    }
#ifdef DEBUG
    printf("n=%d\n", n);
    printf("<= (x1=%d,y1=%d), (x2=%d,y2=%d), (x3=%d,y3=%d), (x4=%d,y4=%d)\n",
	   L(x0,1), L(y0,1), L(x1,1), L(y1,1),
	   L(x0,2), L(y0,2), L(x1,2), L(y1,2));
#endif
    /* step line 1 until it catch up with line 2 */
    if (L(dx,1) > L(dy,1)) {
	while(L(y0,1) != L(y0,2)) {
	    LINE_STEP(L(err,1),L(ptr,1),
		      L(x0,1),L(dx,1),L(sx,1),L(wsx,1),
		      L(y0,1),L(dy,1),L(sy,1),L(wsy,1));
	}
    }
    else {
	while(L(y0,1) != L(y0,2)) {
	    LINE_STEP(L(err,1),L(ptr,1),
		      L(y0,1),L(dy,1),L(sy,1),L(wsy,1),
		      L(x0,1),L(dx,1),L(sx,1),L(wsx,1));
	}
    }

    if (flags & EFLAG_NLAST)
	n--;
#ifdef DEBUG
    printf("n=%d\n", n);
    printf("<> (x1=%d,y1=%d), (x2=%d,y2=%d), (x3=%d,y3=%d), (x4=%d,y4=%d)\n",
	   L(x0,1), L(y0,1), L(x1,1), L(y1,1),
	   L(x0,2), L(y0,2), L(x1,2), L(y1,2));
#endif
    while(n--) {
	// adjust line1 x position
	if ((L(dx,1) != 0) && (L(dx,1) > L(dy,1)) && 
	    (((L(x0,1) < L(x0,2)) && (L(sx,1) < 0)) ||
	     ((L(x0,1) > L(x0,2)) && (L(sx,1) > 0)))) {
	    /* Move to 'LAST' x position line 1 */
	    while((L(err,1) < 0) && (L(x0,1) != L(x1,1))) {
		LINE_STEP_AXIS(L(err,1),L(ptr,1),
			       L(x0,1),L(dy,1),L(sx,1),L(wsx,1));
	    }
	}

	// adjust line2 x position
	if ((L(dx,2) != 0) && (L(dx,2) > L(dy,2)) && 
	    (((L(x0,1) < L(x0,2)) && (L(sx,2) > 0)) ||
	     ((L(x0,1) > L(x0,2)) && (L(sx,2) < 0)))) {
	    /* Move to 'LAST' x position line 2 */
	    while((L(err,2) < 0) && (L(x0,2) != L(x1,2))) {
		LINE_STEP_AXIS(L(err,2),L(ptr,2),
			       L(x0,2),L(dy,2),L(sx,2),L(wsx,2));
	    }
	}

	draw_line_horizontal(pic, L(x0,1), L(x0,2), L(y0,1), flags, fg);

	/* step line 2 until y is moved (it wont move if dy=0) */
	if (L(dy,2) != 0) {
	    if (L(dx,2) > L(dy,2)) {
		while(L(y0,1) == L(y0,2)) {
		    if (L(x0,1) == L(x1,1))
			break;
		    LINE_STEP(L(err,2),L(ptr,2),
			      L(x0,2),L(dx,2),L(sx,2),L(wsx,2),
			      L(y0,2),L(dy,2),L(sy,2),L(wsy,2));
		}
	    }
	    else {
		while(L(y0,1) == L(y0,2)) {
		    LINE_STEP(L(err,2),L(ptr,2),
			      L(y0,2),L(dy,2),L(sy,2),L(wsy,2),
			      L(x0,2),L(dx,2),L(sx,2),L(wsx,2));
		}
	    }
	}

	// step line1
	if (L(dx,1) > L(dy,1)) {
	    while(L(y0,1) != L(y0,2)) {
		if (L(x0,1) == L(x1,1))
		    break;
		LINE_STEP(L(err,1),L(ptr,1),
			  L(x0,1),L(dx,1),L(sx,1),L(wsx,1),
			  L(y0,1),L(dy,1),L(sy,1),L(wsy,1));
	    }
	}
	else {
	    while(L(y0,1) != L(y0,2)) {
		LINE_STEP(L(err,1),L(ptr,1),
			  L(y0,1),L(dy,1),L(sy,1),L(wsy,1),
			  L(x0,1),L(dx,1),L(sx,1),L(wsx,1));
	    }
	}
    }
#ifdef DEBUG
    printf("=> (x1=%d,y1=%d), (x2=%d,y2=%d), (x3=%d,y3=%d), (x4=%d,y4=%d)\n",
	   L(x0,1), L(y0,1), L(x1,1), L(y1,1),
	   L(x0,2), L(y0,2), L(x1,2), L(y1,2));
#endif
    if (swapped) {
	LINE_SWAPOUT(line1,2);
	LINE_SWAPOUT(line2,1);
    }
    else {
	LINE_SWAPOUT(line1,1);
	LINE_SWAPOUT(line2,2);
    }
}


void draw_line_plain(EPixmap* pic, int x1, int y1, int x2, int y2,
		     int flags, EPixel_t fg)
{
    ELine_t line;
    set_p0(pic, &line, x1, y1);
    set_p1(pic, &line, x2, y2);
    if (flags & EFLAG_AALIAS)
	trace_aalias_line_1(pic, &line, flags, fg);
    else
	trace_line_1(pic, &line, flags, fg);
}

void draw_line_twin(EPixmap* pic, 
		    int x1, int y1, int x2, int y2,
		    int x3, int y3, int x4, int y4,
		    int flags, EPixel_t fg)
{
    ELine_t line1;
    ELine_t line2;

    if (y2 < y1) { ESwapInt(x1,x2); ESwapInt(y1,y2);  }
    set_p0(pic, &line1, x1, y1);
    set_p1(pic, &line1, x2, y2);

    if (y4 < y3) { ESwapInt(x3,x4); ESwapInt(y3,y4);  }
    set_p0(pic, &line2, x3, y3);
    set_p1(pic, &line2, x4, y4);
    trace_line_2(pic, &line1, &line2, flags, fg);
}


void fill_triangle(EPixmap* pic, 
		   int x0, int y0,
		   int x1, int y1,
		   int x2, int y2,
		   int flags, EPixel_t fg)
{
    ELine_t line1;
    ELine_t line2;

    if (y2 < y1) { ESwapInt(x1,x2); ESwapInt(y1,y2);  }
    if (y1 < y0) { ESwapInt(x0,x1); ESwapInt(y0,y1);  }
    if (x2 < x1) { ESwapInt(x1,x2); ESwapInt(y1,y2);  }

    set_p0(pic, &line1, x0, y0);
    set_p1(pic, &line1, x1, y1);

    set_p0(pic, &line2, x0, y0);
    set_p1(pic, &line2, x2, y2);

    trace_line_2(pic, &line1, &line2, flags|EFLAG_NLAST, fg);

    if (EPointEqual(&line1.p0, &line1.p1)) {
	if (EPointEqual(&line2.p0, &line2.p1))
	    return;
	set_p1(pic, &line1, x2, y2);
	trace_line_2(pic, &line1, &line2, flags, fg);
    }
    else if (EPointEqual(&line2.p0, &line2.p1)) {
	set_p1(pic, &line2, x1, y1);
	trace_line_2(pic, &line1, &line2, flags, fg);
    }
    else {
	// fprintf(stderr, "NO LINE DONE\n");
    }
}

#if 0

void AALine(int x0, int y0, int x1, int y1)
{
    int addr = (y0*640+x0)*4;
    int dx = x1-x0;
    int dy = y1-y0;
    /* By switching to (u,v), we combine all eight octants */
    if (abs(dx) > abs(dy))
    {
	/* Note: If this were actual C, these integers would be lost
	 * at the closing brace.  That's not what I mean to do.  Do what
	 * I mean. */
	int du = abs(dx);
	int dv = abs(dy);
	int u = x1;
	int v = y1;
	int uincr = 4;
	int vincr = 640*4;
	if (dx < 0) uincr = -uincr;
	if (dy < 0) vincr = -vincr;
    }
    else
    {
	int du = abs(dy);
	int dv = abs(dx);
	int u = y1;
	int v = x1;
	int uincr = 640*4;
	int vincr = 4;
	if (dy < 0) uincr = -uincr;
	if (dx < 0) vincr = -vincr;
    }
    int uend = u + 2 * du;
    int d = (2 * dv) - du;	    /* Initial value as in Bresenham's */
    int incrS = 2 * dv;	/* Δd for straight increments */
    int incrD = 2 * (dv - du);	/* Δd for diagonal increments */
    int twovdu = 0;	/* Numerator of distance; starts at 0 */
    double invD = 1.0 / (2.0*sqrt(du*du + dv*dv));   /* Precomputed inverse denominator */
    double invD2du = 2.0 * (du*invD);   /* Precomputed constant */
    do
    {
	/* Note: this pseudocode doesn't ensure that the address is
	 * valid, or that it even represents a pixel on the same side of
	 * the screen as the adjacent pixel */
	DrawPixelD(addr, twovdu*invD);
	DrawPixelD(addr + vincr, invD2du - twovdu*invD);
	DrawPixelD(addr - vincr, invD2du + twovdu*invD);

	if (d < 0)
	{
	    /* choose straight (u direction) */
	    twovdu = d + du;
	    d = d + incrS;
	}
	else
	{
	    /* choose diagonal (u+v direction) */
	    twovdu = d - du;
	    d = d + incrD;
	    v = v+1;
	    addr = addr + vincr;
	}
	u = u+1;
	addr = addr+uincr;
    } while (u < uend);
}
#endif
