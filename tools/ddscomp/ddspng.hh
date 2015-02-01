#ifndef __DDSPNG_HH__
#define __DDSPNG_HH__

#include <png.h>
#include "ddsimg.hh"

class DDSPng : public DDSImage
{
public:
    DDSPng(void);
    ~DDSPng(void);

    int width(void)  { return mPixmap->width; };
    int height(void) { return mPixmap->height; };
    int frames(void) { return 1; };

    EPixel_t pixel(int i, int x, int y) {
	return EPixmapGetPixel(mPixmap, x, y);
    };

    EPixmap* getPixmap(int i) {
	if (i == 0) return mPixmap;
	return NULL;
    }

    void setPixmap(int i, EPixmap* aPixmap) {
	if (i == 0) {
	    if (mPixmap) 
		EPixmapDestroy(mPixmap);
	    mPixmap = aPixmap;
	}
    }

    int      useAlpha(int i) { return mUseAlpha; }
    
    u_int8_t red(int i, int x, int y)   { return pixel(i,x,y).r; };
    u_int8_t green(int i, int x, int y) { return pixel(i,x,y).g; };
    u_int8_t blue(int i, int x, int y)  { return pixel(i,x,y).b; };
    u_int8_t alpha(int i, int x, int y) { return pixel(i,x,y).a; };

    int  load(char* file_name, int start, int stop);
    void unload(void);

private:
    EPixmap* mPixmap;
    int mUseAlpha;
};


#endif




