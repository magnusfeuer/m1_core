//
// Image Data
//

#ifndef __DDSIMG_HH__
#define __DDSIMG_HH__

#include <sys/types.h>
#include "epic.h"

class DDSImage
{
public:
    virtual ~DDSImage(void) {}
    
    virtual int width(void) = 0;
    virtual int height(void) = 0;
    virtual int frames(void) = 0;

    virtual EPixmap* getPixmap(int i) = 0;
    virtual void     setPixmap(int i, EPixmap* aPixmap) = 0;
    virtual int      useAlpha(int i) = 0;
    virtual EPixel_t  pixel(int frm, int x, int y) = 0;

    virtual u_int8_t red(int frm, int x, int y) = 0;
    virtual u_int8_t green(int frm, int x, int y) = 0;
    virtual u_int8_t blue(int frm, int x, int y) = 0;
    virtual u_int8_t alpha(int frm, int x, int y) = 0;

    virtual int  load(char* filename, int start, int stop) = 0;
    virtual void unload(void) = 0;
private:
};

#endif
