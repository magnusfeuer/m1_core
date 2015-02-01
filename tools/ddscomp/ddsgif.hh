#ifndef __DDSGIF_HH__
#define __DDSGIF_HH__

#include <gif_lib.h>

#include "ddsimg.hh"

class DDSGif : public DDSImage 
{
public:
    DDSGif(void);
    ~DDSGif(void);

    int width(void)  { return mPixmaps[0]->width; };
    int height(void) { return mPixmaps[0]->height; };
    int frames(void) { return mFrames; };
    
    EPixel_t pixel(int i, int x, int y) { 
	return EPixmapGetPixel(mPixmaps[i], x, y);
    };

    EPixmap* getPixmap(int i) {
	if ((i >= 0) && (i < mFrames))
	    return mPixmaps[i];
	return NULL;
    }

    void setPixmap(int i, EPixmap* aPixmap) {
	if ((i >= 0) && (i < mFrames)) {
	    EPixmap* old = mPixmaps[i];
	    if (old)
		EPixmapDestroy(old);
	    mPixmaps[i] = aPixmap;
	}
    }
	
    int      useAlpha(int i) {  return mUseAlpha[i]; }

    u_int8_t red(int i, int x, int y)   { return pixel(i,x,y).r; };
    u_int8_t green(int i, int x, int y) { return pixel(i,x,y).g; };
    u_int8_t blue(int i, int x, int y)  { return pixel(i,x,y).b; };
    u_int8_t alpha(int i, int x, int y) { return pixel(i,x,y).a; };

    int  load(char* file_name, int start, int stop);
    void unload(void);
    
private:
    int mFrames;
    int mFrameStart;
    int mFrameStop;
    EPixmap** mPixmaps;
    int* mUseAlpha;
};

#endif

