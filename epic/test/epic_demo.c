/*
 * Demo EPIC functions
 *
 */
#include <unistd.h>
#include <stdarg.h>
#include <sys/time.h>
#include <math.h>
#include <png.h>

#include "epic.h" 

#define PIXEL_TYPE  EPIXEL_TYPE_BGRA
#define BACKGROUND  epixel_rgb(127,127,127)
#define BORDER      epixel_black


#define SPRITES    0
#define ELLIPSE    1
#define RECTANGLE  2
#define TRIANGLE   3
#define LINE       4
#define LINE2      5

typedef struct {
    int method;
    int orbit;
    int angle;
    int count;
    EGc gc;
} Demo;

static void error(const char * s, ...)
{
    va_list args;
    va_start(args, s);
    vfprintf(stderr, s, args);
    fprintf(stderr, "\n");
    va_end(args);
}

#define YALIGN_TOP    0x01
#define YALIGN_BOTTOM 0x02
#define YALIGN_CENTER 0x03
#define YALIGN_MASK   0x0f

#define XALIGN_LEFT   0x10
#define XALIGN_RIGHT  0x20
#define XALIGN_CENTER 0x30
#define XALIGN_MASK   0xf0


// Load png image into (or create) pic
EPixmap* load_png(char* file_name, EPixmap* pic, int pixel_type,
		  int flags, int align)
{
    unsigned int width;   // image width
    unsigned int height;  // image height
    int xoffs = 0;
    int yoffs = 0;
    unsigned int y;
    png_byte header[8];	// 8 is the maximum size that can be checked
    png_bytep* row_pointers;
    int rowbytes;
    png_byte color_type;
    png_byte bit_depth;
    png_structp png_ptr;
    png_infop info_ptr;

    /* open file and test for it being a png */
    FILE *fp = fopen(file_name, "rb");

    if (!fp) {
	error("%s: could not be opened for reading", file_name);
	goto error;
    }

    fread(header, 1, 8, fp);
    if (png_sig_cmp(header, 0, 8)) {
	error("%s: is not recognized as a PNG file", file_name);
	goto error;
    }

    /* initialize stuff */
    if ((png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,NULL,NULL,NULL)) == NULL) {
	error("%s: png_create_read_struct failed", file_name);
	goto error;
    }

    if ((info_ptr = png_create_info_struct(png_ptr)) == NULL) {
	error("%s: png_create_info_struct failed", file_name);
	goto error;
    }

    if (setjmp(png_jmpbuf(png_ptr))) {
	error("%s: Error during init_io",  file_name);
	goto error;
    }
    
    png_init_io(png_ptr, fp);
    png_set_sig_bytes(png_ptr, 8);

    png_read_info(png_ptr, info_ptr);

    width       = info_ptr->width;
    height      = info_ptr->height;
    color_type  = info_ptr->color_type;
    bit_depth   = info_ptr->bit_depth;

    png_read_update_info(png_ptr, info_ptr);

    rowbytes = info_ptr->rowbytes;

    if (pic == NULL)
	pic = EPixmapCreate(width, height, pixel_type);

    /* read file */
    if (setjmp(png_jmpbuf(png_ptr))) {
	error("%s: Error during read_image", file_name);
	goto error;
    }

    row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
    if (row_pointers == NULL) {
	error("%s: unable to allocate %d bytes", 
	      file_name, sizeof(png_bytep)*height);
	goto error;
    }

    memset(row_pointers, 0, sizeof(png_bytep)*height);
    for (y=0; y < height; y++) {
	if ((row_pointers[y] = (png_byte*) malloc(rowbytes)) == NULL) {
	    error("%s: unable to allocate %d bytes", 
		  file_name, rowbytes);
	    goto error;
	}
    }
    png_read_image(png_ptr, row_pointers);
    fclose(fp);

    // png_destroy_info_struct(png_ptr, info_ptr);

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    if (width < pic->width) {
	switch(align & XALIGN_MASK) {
	case XALIGN_LEFT:    xoffs = 0; break;
	case XALIGN_RIGHT: xoffs = (pic->width - width); break;
	case XALIGN_CENTER: xoffs = (pic->width - width)/2; break;
	default: break;
	}
    }

    if (height < pic->height) {
	switch(align & YALIGN_MASK) {
	case YALIGN_TOP:   yoffs = 0; break;
	case YALIGN_BOTTOM:  yoffs = (pic->height - height); break;
	case YALIGN_CENTER: yoffs = (pic->height - height)/2; break;
	default: break;
	}
    }
    
    for (y = 0; y < height; y++) {
	static int c_warn = 1;
	
	switch(color_type) {
	case PNG_COLOR_TYPE_RGBA:
	    EPixmapPutPixels(pic, xoffs, y+yoffs, width, 1, 
			     EPIXEL_TYPE_RGBA, flags, row_pointers[y], width*4);
	    break;
	case PNG_COLOR_TYPE_RGB:
	    EPixmapPutPixels(pic, xoffs, y+yoffs, width, 1, 
			     EPIXEL_TYPE_RGB, flags, row_pointers[y], width*3);
	    break;
	default:
	    if (c_warn) {
		error("%s; unknown color type %d", file_name, color_type);
		c_warn = 0;
	    }
	    break;
	}
	free(row_pointers[y]);
    }
    return pic;

error:
    if (fp != NULL)
	fclose(fp);
    return NULL;
}




static inline EPixel_t random_pixel(void)
{
    u_int32_t r;
    EPixel_t c;

next:
    r = ((unsigned long)random());
    c.a = 255;
    c.r = r >> 16;
    c.g = r >> 8;
    c.b = r;
    /* avoid generating background */
    if (c.px == BACKGROUND.px) 
	goto next;
    /* avoid very transparent */
    while((c.a = r>>24) < 30)
	r = ((unsigned long)random());
    return c;
}

void update_win(EPixmap* pic, EWindow* win)
{
    EPixmapDrawWindow(pic, win, 0, 0, 0, 0, 640, 480);
}


void random_point(EPoint_t* pt,
		  int min_x, int max_x,
		  int min_y, int max_y) 
{
    int nx = (max_x - min_x) + 1;
    int ny = (max_y - min_y) + 1;
    
    pt->x = (((unsigned long)random()) % nx) + min_x;
    pt->y = (((unsigned long)random()) % ny) + min_y;
}

void rotate_pt(EPoint_t* pt, EPoint_t* orig, double ca, double sa)
{
    int x = pt->x - orig->x;
    int y = pt->y - orig->y;

    pt->x = x*ca - y*sa + orig->x;
    pt->y = y*ca + x*sa + orig->y;
}

void rotate_point(EPoint_t* pt, EPoint_t* orig, int angle)
{
    double a, ca, sa;

    a = (((angle % 360)+360)%360)*(2*M_PI/360);
    ca = cos(a);
    sa = sin(a);

    rotate_pt(pt, orig, ca, sa);
}

void demo_init(Demo* demo, int method, int count)
{
    demo->method = method;
    demo->angle  = 0;
    demo->count  = count;
    demo->orbit   = 0;
    EGcInit(&demo->gc);
    demo->gc.fill_color = epixel_transparent();
    demo->gc.foreground_color = epixel_transparent();
    demo->gc.background_color = epixel_transparent();
    demo->gc.border_color = epixel_transparent();
}

void demo_line(EPixmap* pic, EGc* gc, EWindow* win, Demo* demo)
{
    int i;
    int set_color = (gc->foreground_color.a == 0);

    if (demo->orbit) {
	EPoint_t orig;
	EPoint_t p1, p2;
	EPixel_t color;

	color = epixel_rgb(200,230,100);
	orig.x = 320;
	orig.y = 240;

	p1.x = orig.x - 80;
	p1.y = orig.y;

	p2.x = orig.x + 80;
	p2.y = orig.y;

	for (i = 0; i < demo->count; i++) {
	    EPoint_t q1=p1, q2=p2;

	    rotate_point(&q1, &orig, demo->angle+i);
	    rotate_point(&q2, &orig, demo->angle+i);

	    EPixmapFill(pic, BACKGROUND);
	    color.a = 230; // a;
	    if (set_color)
		EGcSetForegroundColor(gc, color);

	    EPixmapDrawLine(pic,gc,q1.x,q1.y,q2.x,q2.y);
	    update_win(pic, win);
	}
    }
    else {
	for (i = 0; i < demo->count; i++) {
	    EPoint_t p1, p2;
	    EPixel_t color = random_pixel();

	    random_point(&p1, 0, 630, 0, 475);
	    random_point(&p2, 0, 630, 0, 475);

	    if (set_color)
		EGcSetForegroundColor(gc, color);

	    EPixmapDrawLine(pic,gc,p1.x,p1.y, p2.x,p2.y);
	    if (i % 10 == 0)
		update_win(pic, win);
	}
    }
    update_win(pic, win);
}


void demo_line2(EPixmap* pic, EGc* gc, EWindow* win, Demo* demo)
{
    int i;
    int set_color = (gc->foreground_color.a == 0);
    EGc* debug_gc = EGcCreate();
    EPixel_t color;

    EGcSetForegroundColor(debug_gc, epixel_blue);
    EGcSetLineStyle(debug_gc, EPIC_LINE_STYLE_SOLID);

    if (demo->orbit) {
	EPoint_t orig;
	EPoint_t p1, p2;
	EPoint_t p3, p4;

	orig.x = 320;
	orig.y = 240;

	p1.x = orig.x - 80;
	p1.y = orig.y;

	p2.x = orig.x + 80;
	p2.y = orig.y;

	p3.x = orig.x - 80;
	p3.y = orig.y - 20;

	p4.x = orig.x + 80;
	p4.y = orig.y - 20;

	for (i = 0; i < demo->count; i++) {
	    EPoint_t q1=p1, q2=p2;
	    EPoint_t q3=p3, q4=p4;

	    rotate_point(&q1, &orig, demo->angle+i);
	    rotate_point(&q2, &orig, demo->angle+i);
	    rotate_point(&q3, &orig, demo->angle+i);
	    rotate_point(&q4, &orig, demo->angle+i);

	    color = epixel_rgb(200,230,100);
	    color.a = 230; // a;
	    EPixmapFill(pic, BACKGROUND);

	    if (set_color)
		EGcSetForegroundColor(gc, color);

	    EPixmapDrawTwinLine(pic,gc,
				q1.x,q1.y,q2.x,q2.y,
				q3.x,q3.y,q4.x,q4.y);

	    EPixmapDrawLine(pic, debug_gc, 
			    q1.x,q1.y,q2.x,q2.y);
	    EPixmapDrawLine(pic, debug_gc, 
			    q3.x,q3.y,q4.x,q4.y);
	    // usleep(100000);
	    update_win(pic, win);
	}
    }
    else {
	EPixmapFill(pic, BACKGROUND);
	EGcSetLineStyle(gc, EPIC_LINE_STYLE_BLEND);

	for (i = 0; i < demo->count; i++) {
	    EPoint_t p1, p2;
	    EPoint_t p3, p4;

	    color = random_pixel();
	    random_point(&p1, 0, 630, 0, 475);
	    random_point(&p2, 0, 630, 0, 475);

	    random_point(&p3, 0, 630, 0, 475);
	    random_point(&p4, 0, 630, 0, 475);

	    if (set_color)
		EGcSetForegroundColor(gc, color);

	    EPixmapDrawTwinLine(pic,gc,
				p1.x,p1.y, p2.x,p2.y,
				p3.x,p3.y, p4.x,p4.y);
	    update_win(pic, win);
	}
    }
    update_win(pic, win);
}


void demo_triangle(EPixmap* pic, EGc* gc, EWindow* win, Demo* demo)
{
    int i;
    int set_color = (gc->fill_color.a == 0);

    if (set_color)
	EGcSetFillStyle(gc, EPIC_FILL_STYLE_BLEND);

    if (demo->orbit) {
	EPoint_t orig;
	EPoint_t p1, p2, p3;
	EPixel_t color;
	int v = 60;
	int w = 40;
	int alpha;

	color = epixel_blue;
	orig.x = 320;
	orig.y = 240;
	alpha = color.a;

	p1.x = orig.x;
	p1.y = orig.y - v;

	p2.x = orig.x - w;
	p2.y = orig.y + v;

	p3.x = orig.x + w;
	p3.y = orig.y + v;

	for (i = 0; i < demo->count; i++) {
	    EPoint_t q1=p1, q2=p2, q3=p3;
	    int a = i+demo->angle;
	    rotate_point(&q1, &orig, a);
	    rotate_point(&q2, &orig, a);
	    rotate_point(&q3, &orig, a);

	    EPixmapFill(pic, BACKGROUND);
	    color.a = (alpha + i) & 255;

	    if (set_color)
		EGcSetFillColor(gc, color);
	    EGcSetForegroundColor(gc, color);

	    EPixmapDrawTriangle(pic,gc,q1.x,q1.y,q2.x,q2.y,q3.x,q3.y);
	    update_win(pic, win);
	}
    }
    else {
	for (i = 0; i < demo->count; i++) {
	    EPoint_t orig, p1, p2, p3;
	    EPixel_t color = random_pixel();
	    int tsz = 40;

	    random_point(&orig, 0, 600, 0, 400);

	    random_point(&p1, -tsz, tsz, -tsz, tsz);
	    random_point(&p2, -tsz, tsz, -tsz, tsz);
	    random_point(&p3, -tsz, tsz, -tsz, tsz);

	    p1.x += orig.x;
	    p1.y += orig.y;

	    p2.x += orig.x;
	    p2.y += orig.y;

	    p3.x += orig.x;
	    p3.y += orig.y;

	    if (set_color)
		EGcSetFillColor(gc, color);
	    EGcSetForegroundColor(gc, color);

	    EPixmapDrawTriangle(pic,gc,p1.x,p1.y, p2.x,p2.y, p3.x,p3.y);
	    if (i % 10 == 0)
		update_win(pic, win);
	}
    }
    update_win(pic, win);
}


void demo_rectangle(EPixmap* pic, EGc* gc, EWindow* win, Demo* demo)
{
    int i;
    int set_color = (gc->fill_color.a == 0);

    if (set_color)
	EGcSetFillStyle(gc, EPIC_FILL_STYLE_BLEND);

    if (demo->orbit) {
	EPoint_t orig, pt, ab;
	EPixel_t color = random_pixel();

	pt.x = 320 - 50;
	pt.y = 240 - 50;
	ab.x = 25;
	ab.y = 25;
	orig.x = 50;
	orig.y = 50;

	for (i = 0; i < demo->count; i++) {
	    EPoint_t cd = ab;
	    int a = i*2;
	    rotate_point(&cd, &orig, a);

	    EPixmapFill(pic, BACKGROUND);
	    if (set_color)
		EGcSetFillColor(gc, color);
	    EGcSetForegroundColor(gc, color);

	    EPixmapDrawRectangle(pic,gc,pt.x,pt.y,cd.x,cd.y);
	    update_win(pic, win);
	}
    }
    else {
	for (i = 0; i < demo->count; i++) {
	    EPoint_t pt;
	    int w = 10+2*(((unsigned long)random()) % 50);
	    int h = 10+2*(((unsigned long)random()) % 50);
	    EPixel_t color = random_pixel();

	    random_point(&pt, 0, 600, 0, 440);

	    if (set_color)
		EGcSetFillColor(gc,color);
	    EGcSetForegroundColor(gc,color);
	    EPixmapDrawRectangle(pic,gc,pt.x,pt.y,w,h);
	    if (i % 10 == 0)
		update_win(pic, win);
	}
    }
    update_win(pic, win);
}

void demo_ellipse(EPixmap* pic, EGc* gc, EWindow* win, Demo* demo)
{
    int i;
    int set_color = (gc->fill_color.a == 0);

    if (set_color)
	EGcSetFillStyle(gc, EPIC_FILL_STYLE_BLEND|EPIC_FILL_STYLE_AALIAS);

    if (demo->orbit) {
	EPoint_t pt, ab, orig;
	EPixel_t color = random_pixel();

	pt.x = 320 - 50;
	pt.y = 240 - 50;
	ab.x = 25;
	ab.y = 25;
	orig.x = 50;
	orig.y = 50;

	for (i = 0; i < demo->count; i++) {
	    EPoint_t cd = ab;
	    int a = i*2;
	    rotate_point(&cd, &orig, a);

	    EPixmapFill(pic, BACKGROUND);
	    if (set_color)
		EGcSetFillColor(gc,color);
	    EGcSetForegroundColor(gc,color);
	    EPixmapDrawEllipse(pic,gc,pt.x, pt.y, cd.x, cd.y);
	    update_win(pic, win);
	}
    }
    else {
	for (i = 0; i < demo->count; i++) {
	    EPoint_t pt;
	    int w = 10+2*(((unsigned long)random()) % 50);
	    int h = 10+2*(((unsigned long)random()) % 50);
	    EPixel_t color = random_pixel();

	    random_point(&pt, 0, 579, 0, 419);
	    if (set_color)
		EGcSetFillColor(gc,color);
	    EGcSetForegroundColor(gc,color);
	    EPixmapDrawEllipse(pic,gc,pt.x,pt.y,w,h);
	    if (i % 10 == 0)
		update_win(pic, win);
	}
    }
    update_win(pic, win);
}


/*
 * Run n sprites
 *  A sprite is a pixmap rendered on a background
 *
 */

typedef struct epic_sprite {
    double x, y;    /* position */
    double vx, vy;  /* speed    */
    float  fade;    /* fader value */
    float  fs;      /* fader step */
    EPixmap* def;   /* sprite definition */
} ESprite;

#define GLASS_ON    0
#define GLASS_OFF   1
#define GLASS_OUT   2
#define GLASS_IN    3

#define GLASS_SPEED 16
#define GLASS_SHOW  100
#define GLASS_WAIT  500
/*
 * Do a blur overlay
 * 
 */
void do_overlay(EPixmap* fg, EPixmap* layer, EFilter_t* filter,
		EPixmap* copy, int x, int width)
{
    EPixmapFilterArea(fg,copy,filter,x,0,0,0,width,480,0);
    EPixmapCopyArea(layer,copy,0,0,0,0,width,480,EFLAG_BLEND);
    EPixmapCopyArea(copy,fg,0,0,x,0,width,480,0);
}

#define PVECTOR 1
// #define PRANDOM 1
/*
 *  Test:
 *     1 - display PNG image
 *     1a - rotate PNG image
 *     2 - color separation => color mix
 *     3 - image explosion (sprites)
 *
 */
void test_picture(EPixmap* fg, EPixmap* picture, EWindow* win)
{
    ESprite** sprite;
    EPixmap* r;
    EPixmap* g;
    EPixmap* b;
    unsigned int x, y;
    unsigned int px, py;
    unsigned int cx, cy;
    unsigned int rx, ry;
    unsigned int gx, gy;
    unsigned int bx, by;
    int rd, gd, bd;
    int m, n, i;

    cx = fg->width/2;
    cy = fg->height/2;
    px = (picture->width > fg->width) ? 0 : ((fg->width-picture->width)/2);
    py = (picture->height > fg->height) ? 0 : ((fg->height-picture->height)/2);
    rx = gx = bx = px;
    ry = gy = by = py;

    /* diplay picture for 5 - secods */
    EPixmapFill(fg, epixel_black); 
    EPixmapCopyArea(picture, fg, 0, 0, px, py, picture->width, picture->height,
		    EFLAG_NONE);
    update_win(fg, win);
    sleep(2);

    /* rotate picture 360 degrees */
    for (i = 0; i < 360; i++) {
	float angle = (i*2*M_PI)/360;
	EPixmapFill(fg, epixel_black);
	EPixmapRotateArea(picture, fg, angle, 0, 0,
			  picture->width/2, picture->height/2,
			  fg->width/2, fg->height/2,
			  picture->width, picture->height, 
			  EFLAG_BLEND|EFLAG_AALIAS);
	update_win(fg, win);
    }
	
    /* split picture in R/G/B component pictures */
    r = EPixmapCreate(picture->width,picture->height,EPIXEL_TYPE_R8);
    EPixmapCopy(picture, r);
    g = EPixmapCreate(picture->width,picture->height,EPIXEL_TYPE_G8);
    EPixmapCopy(picture, g);
    b = EPixmapCreate(picture->width,picture->height,EPIXEL_TYPE_B8);
    EPixmapCopy(picture, b);
    rd = 1;
    gd = -1;
    bd = 1;

    n  = 100;
    m = EMaxInt(picture->width,picture->height);
    n = 2*m+1;
    i = 1;

    while(n--) {
	EPixmapFill(fg, epixel_black);
	EPixmapCopyArea(r,fg,0,0,rx,ry,r->width,r->height,EFLAG_SUM);
	EPixmapCopyArea(g,fg,0,0,gx,gy,g->width,g->height,EFLAG_SUM);
	EPixmapCopyArea(b,fg,0,0,bx,by,b->width,b->height,EFLAG_SUM);
	update_win(fg, win);
	usleep(10000);
	rx += rd;
	gx += gd;
	by += bd;
	if ((i % m) == 0) {
	    rd = -rd;
	    gd = -gd;
	    bd = -bd;
	}
	i++;
    }
    sleep(2);
    EPixmapDestroy(r);
    EPixmapDestroy(g);
    EPixmapDestroy(b);

    // Generate 1x1 pixel pictures for each pixel in picture 
    // and let them loose
    n = picture->width*picture->height;
    if ((sprite = (ESprite**) malloc(n*sizeof(ESprite*))) == NULL) {
	fprintf(stderr, "malloc: failed\n");
	return;
    }
    i = 0;
    for (y = 0; y < picture->height; y++) {
	for (x = 0; x < picture->width; x++) {
	    ESprite* sp;
	    double vlen = 0.0;
	    if ((sp = (ESprite*) malloc(sizeof(ESprite))) == NULL) {
		fprintf(stderr, "malloc: failed\n");
		return;
	    }
	    sp->def = EPixmapCreate(1, 1, picture->pixelType);
	    EPixmapCopyArea(picture, sp->def, x, y, 0, 0, 1, 1, 0);
	    sp->x = (double) px+x;
	    sp->y = (double) py+y;
	    sp->fade = 1.0;
	    sp->fs   = 0.01 + 0.01*(random() % 90);
#ifdef PRAND
	    sp->vx = (((random() % 21) - 10)) / 10.0;
	    if (fabs(sp->vx) < 0.01)
		sp->vx = 0.1;
	    sp->vy = (((random() % 21) - 10)) / 10.0;
	    if (fabs(sp->vy) < 0.01)
		sp->vy = 0.1;
#elif PVECTOR
	    sp->vx = sp->x - cx;
	    sp->vy = sp->y - cy;
	    vlen = sqrt(sp->vx*sp->vx + sp->vy*sp->vy);
	    sp->vx = sp->vx / vlen;
	    sp->vy = sp->vy / vlen;
#endif
	    sprite[i++] = sp;
	}
    }

    while(1) {
	EPixmapFill(fg, epixel_black);
	for (i = 0; i < n; i++) {
	    ESprite* sp = sprite[i];
	    int x = sp->x;
	    int y = sp->y;
	    u_int8_t fade;

	    if ((x < 0)||(x >= (int)fg->width))
		sp->vx = -sp->vx;
	    if ((y < 0)||(y >= (int)fg->height))
		sp->vy = -sp->vy;
	    sp->x += sp->vx;
	    sp->y += sp->vy;

	    sp->fade += sp->fs;
	    if (sp->fade > 1.0) {
		sp->fade = 1.0;
		sp->fs = -sp->fs;
	    }
	    else if (sp->fade < 0.0) {
		sp->fade = 0.0;
		sp->fs = -sp->fs;
	    }
	    if (sp->fade >= 1.0)
		fade = ALPHA_FACTOR_1;
	    else if (sp->fade <= 0.0)
		fade = ALPHA_FACTOR_0;
	    else
		fade = truncf(sp->fade*256);
	    // EPixmapCopyArea(sp->def, fg, 0, 0, x, y, 1, 1, 0);
	    EPixmapFadeArea(sp->def, fg, fade, 0, 0, x, y, 1, 1);
	}
	update_win(fg, win);
    }

}


void test_sprites(int n, int gparam, EPixmap* fg, EPixmap* bg, EWindow* win)
{
    ESprite** sprite;
    int i;
    unsigned int frame = 0;
    int hdir = 1; // 1;
    int vdir = 1; // 1;
    int bg_hspeed = 1;
    int bg_vspeed = 1;
    int use_shadow = 0;
    EPoint_t shadow_pt;
    EPoint_t shadow;
    int shadow_a = 45;
    EGc gc;
    struct timeval t0, t1;
    int glass_state       = GLASS_OFF;
    int glass_speed       = GLASS_SPEED;
    int glass_x           = 640;
    int glass_width       = 0;
    int glass_wait_frames = GLASS_WAIT;
    int glass_show_frames = GLASS_SHOW;
    int glass_enable = (gparam > 0);
    // u_int8_t blur_factors[9] = {1,2,1, 2,4,2, 1,2,1};
    u_int8_t blur_factors[32] = {
	1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1
    };

    EFilter_t blur = {{32,1},32,blur_factors,{0}};
    EPixmap* glass;
    EPixmap* copy;
    EPixmap* scopy;
    EPixmap* acopy;
    int x, y;

    EGcInit(&gc);

    if ((sprite = (ESprite**) malloc(n*sizeof(ESprite*))) == NULL) {
	fprintf(stderr, "malloc: failed\n");
	return;
    }

    copy = EPixmapCreate(640,480,PIXEL_TYPE);
    // Create "glass" layer
    
    glass = EPixmapCreate(640,480,PIXEL_TYPE);

    scopy = EPixmapCreate(32, 32, PIXEL_TYPE);
    acopy = EPixmapCreate(32, 32, EPIXEL_TYPE_A8);

    for (y = 0; y < 480; y++) {
	for (x = 0; x < 640; x++) {
	    EPixel_t wc = epixel_argb(0, 200, 200, 200);
	    switch(gparam) {
	    case 1:
		wc.a = 100;
		break;
	    case 2:
		wc.a = 180 + (y % 20);         // Horizontal stripes
		break;
	    case 3:
		wc.a = 180 + (x % 20);         // Vertical stripes
		break;
	    case 4:
		wc.a = 200 + ((x+y) % 20);  // Diag stripes
		break;
	    case 5:
		wc.a = 180+((unsigned long)random()) % 20; //  Random
		break;
	    case 6:
		wc.a = ((x+y)&1)*230;
		break;
	    default:
		break;
	    }
	    EPixmapPutPixel(glass,x,y,0,wc);
	}
    }

    load_png("Qwerty.png",glass,PIXEL_TYPE,EFLAG_BLEND,
	     XALIGN_CENTER|YALIGN_BOTTOM);


    // Edge on the glas layer
    EGcSetFillColor(&gc, epixel_argb(200, 20, 20, 20));
    EGcSetFillStyle(&gc, EPIC_FILL_STYLE_SOLID);
    EPixmapDrawRectangle(glass, &gc, 0, 0, 6, 480);

    // Generate sprites
    EGcSetBorderWidth(&gc, 0);
    EGcSetFillStyle(&gc, EPIC_FILL_STYLE_SOLID); // |EPIC_FILL_STYLE_AALIAS);

    for (i = 0; i < n; i++) {
	u_int8_t c1;
	u_int8_t c2;
	u_int8_t c3;
	ESprite* spr;

	if ((spr = (ESprite*) malloc(sizeof(ESprite))) == NULL) {
	    fprintf(stderr, "malloc: failed\n");
	    return;
	}
	spr->def = EPixmapCreate(32, 32, PIXEL_TYPE);
	/* set sprite background to white, transparent A=0 */
	if ((i % 4) == 0) {
	    int j;
	    EPixmapFill(spr->def, epixel_transparent());
	    for (j = 15; j > 0; j--) {
		EGcSetFillColor(&gc, epixel_argb(j*15,255,255,255));
		EPixmapDrawEllipse(spr->def,&gc,15,15,2*j,2*j);
	    }
	}
	else {
	    EPixmapFill(spr->def, epixel_transparent());
	    /* draw a circle with a random color */
	    c1 = 75 + ((unsigned long)random()) % 150;
	    c2 = 75 + ((unsigned long)random()) % 150;
	    c3 = 75 + ((unsigned long)random()) % 150;

	    EGcSetFillColor(&gc, epixel_rgb(c1,c2,c3));
	    EPixmapDrawEllipse(spr->def,&gc,15,15,30,30);

	    EGcSetFillColor(&gc,epixel_transparent());
	    EPixmapDrawEllipse(spr->def,&gc,15,15,24,24);

	    EGcSetFillColor(&gc,epixel_rgb(c3,c2,c1));
	    EPixmapDrawEllipse(spr->def,&gc,15,15,18,18);

	    EGcSetFillColor(&gc,epixel_transparent());
	    EPixmapDrawEllipse(spr->def,&gc,15,15,12,12);

	    EGcSetFillColor(&gc,epixel_rgb(c1,c2,c3));
	    EPixmapDrawEllipse(spr->def,&gc,15,15,6,6);
	}
	spr->x = (double) (random() % (640-32));
	spr->y = (double) (random() % (480-32));
	spr->fade = (random() % 256) / 255.0;
	spr->fs   = 0.001 + 0.0001*(random() % 1000);
	spr->vx = (((random() % 21) - 10)) / 10.0;
	if (fabs(spr->vx) < 0.01)
	    spr->vx = 0.1;
	spr->vy = (((random() % 21) - 10)) / 10.0;
	if (fabs(spr->vy) < 0.01)
	    spr->vy = 0.1;
	sprite[i] = spr;
    }

    shadow_pt.x = 7;
    shadow_pt.y = 7;
    shadow = shadow_pt;

    gettimeofday(&t0, NULL);

    while(1) {
	EPixel_t none = epixel_rgb(0,0,0);
	/* For this test we use a simple backgroud BLACK */
/*	EPixmapFill(bg, EPIXEL_BLACK); */
	frame++;

	EPixmapCopy(bg, fg);
	if (hdir > 0)
	    EPixmapScrollLeft(bg, bg,  1, bg_hspeed, none);
	else if (hdir < 0)
	    EPixmapScrollRight(bg, bg, 1, bg_hspeed, none);
	if (vdir > 0)
	    EPixmapScrollUp(bg, bg,    1, bg_vspeed, none);
	else if (vdir < 0)
	    EPixmapScrollDown(bg, bg,  1, bg_vspeed, none);
	
	if ((frame % 640) == 0)
	    hdir = -hdir;
	if ((frame % 480) == 0)
	    vdir = -vdir;
	if ((frame % 100) == 0) {
	    bg_hspeed++;
	    bg_vspeed++;
	    if (bg_hspeed > 8)
		bg_hspeed = 1;
	    if (bg_vspeed > 8)
		bg_vspeed = 1;
	}
	if ((frame % 10) == 0) {
	    EPoint_t orig;
	    orig.x = orig.y = 0;
	    shadow_a = (shadow_a + 10)  % 360;
	    shadow = shadow_pt;
	    rotate_point(&shadow, &orig, shadow_a);
	}

	for (i = 0; i < n; i++) {
	    ESprite* sp = sprite[i];
	    int x = sp->x;
	    int y = sp->y;
	    u_int8_t fade;

	    if ((x < 0)||(x >= (640-32)))
		sp->vx = -sp->vx;
	    if ((y < 0)||(y >= (480-32)))
		sp->vy = -sp->vy;
	    sp->x += sp->vx;
	    sp->y += sp->vy;

	    sp->fade += sp->fs;
	    if (sp->fade > 1.0) {
		sp->fade = 1.0;
		sp->fs = -sp->fs;
	    }
	    else if (sp->fade < 0.0) {
		sp->fade = 0.0;
		sp->fs = -sp->fs;
	    }
	    if (sp->fade >= 1.0)
		fade = ALPHA_FACTOR_1;
	    else if (sp->fade <= 0.0)
		fade = ALPHA_FACTOR_0;
	    else
		fade = truncf(sp->fade*256);
	    if (use_shadow) {
		EPixmapCopy(sp->def, acopy); // copy alpha channel */
		EPixmapCopy(acopy,   scopy); // make a (a,0,0,0)   */
		EPixmapFadeArea(sp->def, scopy, fade, 0, 0, x, y, 32, 32);
		
		EPixmapShadowArea(scopy, fg, 0, 0,
				  x+shadow.x,
				  y+shadow.y, 32, 32, EFLAG_BLEND);
		EPixmapCopyArea(scopy, fg, 0, 0, x, y, 32, 32, EFLAG_BLEND);
	    }
	    else {
		EPixmapFadeArea(sp->def, fg, fade, 0, 0, x, y, 32, 32);
	    }
	}

	switch(glass_state) {
	case GLASS_OFF:
	    if (glass_enable) {
		glass_wait_frames--;
		if (glass_wait_frames <= 0) {
		    glass_state = GLASS_IN;
		    glass_x     = 640;
		    glass_width = 0;
		}
	    }
	    break;
	case GLASS_ON:
	    glass_show_frames--;
	    if (glass_show_frames <= 0)
		glass_state = GLASS_OUT;
	    do_overlay(fg,glass,&blur,copy,glass_x,glass_width);
	    break;
	case GLASS_IN:
	    if (glass_x == 0) {
		glass_state = GLASS_ON;
		glass_show_frames = GLASS_SHOW;
	    }
	    else {
		glass_x -= glass_speed;
		glass_width += glass_speed;
		if (glass_x < 0) glass_x = 0;
		if (glass_width > 640) glass_width = 640;
	    }
	    do_overlay(fg,glass,&blur,copy,glass_x,glass_width);
	    break;
	case GLASS_OUT:
	    if (glass_x == 639) {
		glass_state = GLASS_OFF;
		glass_wait_frames = GLASS_WAIT;
	    }
	    else {
		glass_x += glass_speed;
		glass_width -= glass_speed;
		if (glass_width < 0) glass_width = 0;
		if (glass_x > 639) glass_x= 639;
		do_overlay(fg,glass,&blur,copy,glass_x,glass_width);
	    }
	    break;
	default:
	    break;
	}

	update_win(fg, win);

	if ((frame % 100) == 0) {
	    struct timeval t;
	    gettimeofday(&t1, NULL);

	    timersub(&t1, &t0, &t);
	    t0 = t1;
	    printf("F: %f/s\n", (100.0/(t.tv_sec+(t.tv_usec/1000000.0))));
	}
    }
}

int main(int argc, char** argv)
{
    EBackend* be;
    EPixmap* attached = NULL;
    EPixmap* picture = NULL;
    EWindow* win = NULL;
    char* be_name = NULL;
    char* accel_name = NULL;
    char* picture_name = NULL;
    int accel;
    int count = 1;
    int c;
    int i;
    int di = -1;
    int glass_param = 0;
    int wait_time = 0;
    Demo demo[100];
    int pt = PIXEL_TYPE;

    // Flag settings: Fx
    // f = fill, a = antialias, n=no border, b=no blend
    // Color settings: Cx<color>
    // f = fillcolor, a=foreground, b=border, c=background
    while((c = getopt(argc, argv, "P:B:A:g:c:n:F:C:W:T:a:otrelL")) != -1) {
	switch (c) {
	case 'g':
	    if (optarg != NULL)
		glass_param = atoi(optarg);
	    break;
	case 'c':
	    if (optarg != NULL)
		pt = EPixelTypeFromName(optarg);
	    break;
	case 'o':
	    if (di >= 0) demo[di].orbit = 1;
	    break;
	case 'n':
	    count = ((optarg == NULL) ? 1 : atoi(optarg));
	    if (di >= 0)
		demo[di].count = count;
	    break;
	case 'a':
	    if (di >= 0)
		demo[di].angle = ((optarg == NULL) ? 0 : atoi(optarg));
	    break;
	case 'B':
	    be_name = optarg;
	    break;
	case 'A':
	    accel_name = optarg;
	    break;
	case 'P':
	    picture_name = optarg;
	    break;
	case 'F':
	    if ((optarg != NULL) && (di >= 0)) {
		switch(*optarg++) {
		case 'f':
		    while(*optarg != '\0') {
			switch(*optarg) {
			case 'f': 
			    demo[di].gc.fill_style |= EPIC_FILL_STYLE_SOLID;
			    break;
			case 'b':
			    demo[di].gc.fill_style |= EPIC_FILL_STYLE_BLEND;
			    break;
			case 'a':
			    demo[di].gc.fill_style |= EPIC_FILL_STYLE_AALIAS;
			    break;
			default:
			    break;
			}
			optarg++;
		    }
		    break;

		case 'l':
		    while(*optarg != '\0') {
			switch(*optarg) {
			case 'f': 
			    demo[di].gc.line_style |= EPIC_LINE_STYLE_SOLID;
			    break;
			case 'b':
			    demo[di].gc.line_style |= EPIC_LINE_STYLE_BLEND;
			    break;
			case 'a':
			    demo[di].gc.line_style |= EPIC_LINE_STYLE_AALIAS;
			    break;
			default:
			    break;
			}
			optarg++;
		    }
		    break;
		    
		case 'b':
		    while(*optarg != '\0') {
			switch(*optarg) {
			case 'f':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_SOLID;
			    break;
			case 'b':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_BLEND;
			    break;
			case 'a':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_AALIAS;
			    break;
			case '1':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_NBORDER1;
			    break;
			case '2':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_NBORDER2;
			    break;
			case '3':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_NBORDER3;
			    break;
			case '4':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_NBORDER4;
			    break;
			case 'n':
			    demo[di].gc.border_style |= EPIC_BORDER_STYLE_NBORDER;
			    break;
			default:
			    break;
			}
			optarg++;
		    }
		    break;
		default:
		    break;
		}
	    }
	    break;

	case 'C':
	    if ((optarg != NULL) && (di >= 0)) {
		switch(*optarg++) {
		case 'f':
		    EGcSetFillColor(&demo[di].gc,EPixelFromString(optarg));
		    break;
		case 'a':
		    EGcSetForegroundColor(&demo[di].gc,EPixelFromString(optarg));
		    break;
		case 'b':
		    EGcSetBorderColor(&demo[di].gc,EPixelFromString(optarg));
		    break;
		case 'c':
		    EGcSetBackgroundColor(&demo[di].gc,EPixelFromString(optarg));
		    break;
		default:
		    break;
		}
	    }
	    break;
	case 'W':
	    if ((optarg != NULL) && (di >= 0)) {
		switch(*optarg++) {
		case 'b':
		    demo[di].gc.border_width = atoi(optarg);
		    break;
		case 'l':
		    demo[di].gc.line_width = atoi(optarg);
		    break;
		default:
		    break;
		}
	    }
	    break;
	case 'l':
	    di++;
	    demo_init(&demo[di], LINE, count);
	    break;
	case 'L':
	    di++;
	    demo_init(&demo[di], LINE2, count);
	    break;
	case 't':
	    di++;
	    demo_init(&demo[di], TRIANGLE, count);
	    break;
	case 'r':
	    di++;
	    demo_init(&demo[di], RECTANGLE, count);
	    break;
	case 'e':
	    di++;
	    demo_init(&demo[di], ELLIPSE, count);
	    break;
	case 'T':
	    if (optarg != NULL)
		wait_time = atoi(optarg);
	    else
		wait_time = 0;
	    break;
	default:
	    break;
	}
    }
	    
    if ((be = EBackendCreate(be_name, NULL)) == NULL) {
	fprintf(stderr, "no backend found\n"),
	exit(1);
    }
    if (accel_name == NULL)
	accel = EPIC_SIMD_AUTO;  // select automatically
    else if (strcmp(accel_name, "auto")==0)
	accel = EPIC_SIMD_AUTO;  // select automatically
    else if (strcmp(accel_name, "emu")==0)
	accel = EPIC_SIMD_EMU;
    else if (strcmp(accel_name, "mmx")==0)
	accel = EPIC_SIMD_MMX;
    else if (strcmp(accel_name, "sse2")==0)
	accel = EPIC_SIMD_SSE2;
    else if (strcmp(accel_name, "altivec")==0)
	accel = EPIC_SIMD_ALTIVEC;
    else
	accel = EPIC_SIMD_AUTO;  // select automatically	

    epic_init(accel);

    win = EWindowCreate(50, 50, 640, 480);
    EWindowAttach(win, be);

    srandom(time(NULL));

    /* This is the attached pixmap */
    attached = EPixmapCreate(640, 480, pt);
    EPixmapAttach(attached, be);

    /* This is the background */

    if (di == -1) {
	Demo dmo;

	EGcInit(&dmo.gc);
	EGcSetBorderWidth(&dmo.gc, 1);
	EGcSetBorderColor(&dmo.gc, BORDER);
	// EGcSetLineStyle(&dmo.gc, EPIC_LINE_STYLE_AALIAS);


	if (picture_name != NULL) {
	    picture = load_png(picture_name, NULL, pt, 0, 0);
	    test_picture(attached, picture, win);
	}
	else {
	    picture = EPixmapCreate(640, 480, pt);
	    EPixmapFill(picture, epixel_black);
	    dmo.count  = 250;
	    dmo.orbit = 0;
	    EGcSetFillColor(&dmo.gc, epixel_transparent());
	    demo_rectangle(picture,&dmo.gc,win,&dmo);
	    EGcSetFillColor(&dmo.gc, epixel_transparent());
	    demo_triangle(picture,&dmo.gc,win,&dmo);
	    EGcSetFillColor(&dmo.gc, epixel_transparent());
	    demo_ellipse(picture,&dmo.gc,win,&dmo);
	    EDirectShadeArea(picture->data, 
			     picture->bytesPerRow,
			     picture->pixelType,
			     640, 480,
			     epixel_argb(100,255,255,255),
			     epixel_argb(100,0,255,0),
			     epixel_argb(0,255,255,255),
			     epixel_argb(0,255,0,0));

	    test_sprites(count, glass_param, attached, picture, win);
	}
    }
    else {
	for (i = 0; i <= di; i++) {
	    EPixmapFill(attached, BACKGROUND);
	    update_win(attached, win);

	    switch(demo[i].method) {
	    case LINE:
		demo_line(attached, &demo[i].gc, win, &demo[i]);
		break;
	    case LINE2:
		demo_line2(attached, &demo[i].gc, win, &demo[i]);
		break;
	    case TRIANGLE:
		demo_triangle(attached,&demo[i].gc,win,&demo[i]);
		break;
	    case RECTANGLE:
		demo_rectangle(attached,&demo[i].gc,win,&demo[i]);
		break;
	    case ELLIPSE:
		demo_ellipse(attached,&demo[i].gc,win,&demo[i]);
		break;
	    default:
		break;
	    }
	}
    }

    if (wait_time > 0)
	sleep(wait_time);

    EPixmapDetach(attached);
    EWindowDetach(win);

    EPixmapDestroy(attached);
    if (picture != NULL)
	EPixmapDestroy(picture);
    EWindowDestroy(win);

    EBackendDestroy(be);
    exit(0);
}
