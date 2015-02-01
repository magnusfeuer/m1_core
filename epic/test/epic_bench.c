/*
 *
 * Bench marking/test
 *
 */

#include <unistd.h>
#include <stdarg.h>
#include <sys/time.h>
#include <math.h>

#include "epic.h"


void test_dict()
{
    EDict dict;
    char* s;
    int i;
    double f;
    
    EDictInit(&dict);

    EDictSetString(&dict, "foo.s", "hej");
    EDictSetInteger(&dict, "foo.i", 1234);
    EDictSetFloat(&dict, "foo.f", 3.14);

    EDictLookupString(&dict, "foo.s", &s);
    printf("foo.s = %s\n", s);

    EDictLookupInteger(&dict, "foo.i", &i);
    printf("foo.i = %d\n", i);

    EDictLookupFloat(&dict, "foo.f", &f);
    printf("foo.f = %f\n", f);

    EDictSort(&dict);

    EDictLookupString(&dict, "foo.s", &s);
    printf("foo.s = %s\n", s);

    EDictLookupInteger(&dict, "foo.i", &i);
    printf("foo.i = %d\n", i);

    EDictLookupFloat(&dict, "foo.f", &f);
    printf("foo.f = %f\n", f);
}

/* Time the copy area function */
void bench_copy(int n)
{
    EPixmap* a;
    EPixmap* b;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark blending function */

    a = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);
    b = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    EPixmapCopyArea(a, b, 0, 0, 0, 0, 640, 480, 0);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("COPY Avg: %f/s\n", tfsum/n);
}

static EPixel_t unpack_ARGB(u_int8_t* ptr)
{
    EPixel_t p;
    p.px = *((unsigned long*) ptr);
    return p;
}

static void pack_ARGB(EPixel_t p,unsigned char* ptr)
{
    *((unsigned long*) ptr) = p.px;
}


void bench_plot1(int n,
		 EPixel_t (*src_unpack)(u_int8_t*),
		 void (*dst_pack)(EPixel_t, u_int8_t*))
{
    EPixmap* a;
    EGc* gc;
    int j;
    double tfsum = 0.0;
    u_int8_t* src_ptr;
    unsigned int bytesPerRow;
    unsigned int bytesPerPixel;
    int x, y;

    /* Bench mark fill function */
    gc = EGcCreate();
    a = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);
    
    EGcSetFillStyle(gc, EPIC_FILL_STYLE_SOLID);
    EGcSetBorderWidth(gc, 0);
    EGcSetFillColor(gc, epixel_red);

    bytesPerRow = a->bytesPerRow;
    bytesPerPixel = a->bytesPerPixel;

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);

	src_ptr = EPIXEL_ADDR(a, 0, 0);
	for (y = 0; y < 480; y++) {
	    u_int8_t* src1 = src_ptr;
	    for (x = 0; x < 640; x++) {
		EPixel_t p;
		p = src_unpack(src1);
		p.a = 100; p.r = 1; p.g = 1; p.b = 1;
		dst_pack(p, src1);
		src1 += bytesPerPixel;
	    }
	    src_ptr += bytesPerRow;
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("PLOT1 Avg: %f/s\n", tfsum/n);
}

/* Time the fill area function */
void bench_plot(int n, int src_pt, int dst_pt)
{
    EPixmap* a;
    EGc* gc;
    int j;
    double tfsum = 0.0;
    u_int8_t* src_ptr;
    unsigned int bytesPerRow;
    unsigned int bytesPerPixel;
    int x, y;

    /* Bench mark fill function */
    gc = EGcCreate();
    a = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);
    
    EGcSetFillStyle(gc, EPIC_FILL_STYLE_SOLID);
    EGcSetBorderWidth(gc, 0);
    EGcSetFillColor(gc, epixel_red);

    bytesPerRow = a->bytesPerRow;
    bytesPerPixel = a->bytesPerPixel;

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);

	src_ptr = EPIXEL_ADDR(a, 0, 0);
	for (y = 0; y < 480; y++) {
	    u_int8_t* src1 = src_ptr;
	    for (x = 0; x < 640; x++) {
		EPixel_t p = EPixelUnpack(src_pt, src1);
		p.a = 100; p.r = 1; p.g = 1; p.b = 1;
		EPixelPack(dst_pt, p, src1);
		src1 += bytesPerPixel;
	    }
	    src_ptr += bytesPerRow;
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("PLOT Avg: %f/s\n", tfsum/n);
}

/* Time the fill area function */
void bench_fill(int n)
{
    EPixmap* a;
    EGc* gc;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark fill function */
    gc = EGcCreate();
    a = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);

    EGcSetFillStyle(gc, EPIC_FILL_STYLE_SOLID);
    EGcSetBorderWidth(gc, 0);
    EGcSetFillColor(gc, epixel_red);

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    EPixmapDrawRectangle(a, gc, 0, 0, 640, 480);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("FILL Avg: %f/s\n", tfsum/n);
}

/* Time the blend area function */
void bench_blend(int n)
{
    EPixmap* a;
    EPixmap* b;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark blending function */

    a = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);
    b = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);

    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    EPixmapCopyArea(a, b, 0, 0, 0, 0, 640, 480, EFLAG_BLEND);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	// printf("BLEND: %f/s\n", tf);
	tfsum += tf;
    }
    printf("BLEND Avg: %f/s\n", tfsum/n);
}

/* Time the blend fill area function */
void bench_blend_fill(int n)
{
    EPixmap* a;
    EGc* gc;
    int i, j;
    double tfsum = 0.0;

    /* Bench mark blending function */
    gc = EGcCreate();
    a = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);

    EGcSetFillStyle(gc, EPIC_FILL_STYLE_BLEND);
    EGcSetBorderWidth(gc, 0);
    EGcSetFillColor(gc, epixel_argb(127,100,200,150));
    
    for (j = 0; j < n; j++) {
	struct timeval t, t0, t1;
	double tf;
	gettimeofday(&t0, NULL);
	for (i = 0; i < 100; i++) {
	    EPixmapDrawRectangle(a, gc, 0, 0, 640, 480);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 100.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("BFILL: Avg: %f/s\n", tfsum/n);
}

void bench_line(int n)
{
    EPixmap* a;
    EGc* gc;
    int x0 = 0, x1 = 639;
    int y0, y1;
    int i;
    double tfsum = 0.0;

    gc = EGcCreate();
    a  = EPixmapCreate(640, 480, EPIXEL_TYPE_ARGB);

    for (i = 0; i < n; i++) {
	struct timeval t, t0, t1;
	double tf;

	gettimeofday(&t0, NULL);
	for (y0 = 0; y0 < 480; y0++) {
	    y1 = 439-y0;
	    EPixmapDrawLine(a, gc, x0, y0, x1, y1);
	}
	gettimeofday(&t1, NULL);
	timersub(&t1, &t0, &t);
	tf = 480.0/(t.tv_sec+(t.tv_usec/1000000.0));
	tfsum += tf;
    }
    printf("LINE Avg: %f/s\n", tfsum/n);
}


int main(int argc, char** argv)
{
    char* accel_name = NULL;
    int accel;
    int c;

    while((c = getopt(argc, argv, "A:")) != -1) {
	switch(c) {
	case 'A':
	    accel_name = optarg;
	    break;
	default:
	    break;
	}
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

    bench_copy(10);
    bench_plot(10, EPIXEL_TYPE_ARGB, EPIXEL_TYPE_BGRA);
    bench_plot1(10, unpack_ARGB, pack_ARGB);
    bench_fill(10);
    bench_blend(10);
    bench_blend_fill(10);
    bench_line(10);
    // test_dict();
    exit(0);
}
