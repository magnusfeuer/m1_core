#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "ddsgif.hh"

static void error(const char * s, ...)
{
    va_list args;
    va_start(args, s);
    vfprintf(stderr, s, args);
    fprintf(stderr, "\n");
    va_end(args);
}

DDSGif::DDSGif(void)
{
    mFrames = 0;
    mPixmaps = NULL;
}

DDSGif::~DDSGif(void)
{
    if (mPixmaps) {
	for (int i = 0; i < mFrames; i++)
	    EPixmapDestroy(mPixmaps[i]);
	free(mPixmaps);
    }
}

void DDSGif::unload()
{
    if (mPixmaps) {
	for (int i = 0; i < mFrames; i++)
	    EPixmapDestroy(mPixmaps[i]);
	free(mPixmaps);
    }
    mFrames = 0;
    mPixmaps = NULL; 
}

int DDSGif::load(char* file_name, int start, int stop)
{
    GifFileType* ft;
    size_t imageSize = 0;
    int i;
    int width;
    int height;

    if ((ft = DGifOpenFileName(file_name)) == NULL) {
	error("%s: could not be opened for reading", file_name);
	return -1;
    }

    if (DGifSlurp(ft) == GIF_ERROR) {
	error("%s: error reading gif file",  file_name);
	goto error;
    }
#ifdef DEBUG
    fprintf(stderr, "width: %d, height: %d\n", ft->SWidth, ft->SHeight);
    fprintf(stderr, "resolution: %d\n", ft->SColorResolution);
    fprintf(stderr, "background: %d\n", ft->SBackGroundColor);
    fprintf(stderr, "colormap: %p\n", ft->SColorMap);
    fprintf(stderr, "ImageCount: %d\n", ft->ImageCount);
#endif

    width      = ft->SWidth;
    height     = ft->SHeight;
    mFrames     = ft->ImageCount;
    mPixmaps   = (EPixmap**) malloc(sizeof(EPixmap*)*ft->ImageCount);
    mUseAlpha  = (int*) malloc(sizeof(int)*ft->ImageCount);

    imageSize = width*height*sizeof(EPixel_t);

    for (i = 0; i < mFrames; i++) {
	EPixmap* pixmap = NULL;
	SavedImage* im = &ft->SavedImages[i];
	int Transparent = 0;
	int TransparentColor = -1;
	int DelayTime = 0;
	ColorMapObject* cmap;
	int x, y;
	int j;
	u_int8_t* ptr;
	int useAlpha = 0;

	mUseAlpha[i] = 0;
	pixmap = EPixmapCreate(width, height, EPIXEL_TYPE_RGBA);
	mPixmaps[i] = pixmap;

	cmap = (im->ImageDesc.ColorMap != NULL) ? im->ImageDesc.ColorMap :
	    ft->SColorMap;
	
	/* Copy previous image and render subimage on top */
	if (i > 0)
	    EPixmapCopy(mPixmaps[i-1], mPixmaps[i]);
#ifdef DEBUG
	fprintf(stderr, "Image data: %d\n", i);
#endif
	for (j = 0; j < im->ExtensionBlockCount; j++) {
	    u_int8_t* bp = (u_int8_t*) im->ExtensionBlocks[j].Bytes;
#ifdef DEBUG
	    fprintf(stderr, "Extension: %d size=%d\n", 
		    im->ExtensionBlocks[j].Function,
		    im->ExtensionBlocks[j].ByteCount);
#endif
	    if (im->ExtensionBlocks[j].Function == GRAPHICS_EXT_FUNC_CODE) {
		Transparent = bp[0] & 1;
		DelayTime   = bp[1] + (bp[2]<<8);
		TransparentColor = bp[3];
	    }
	}
#ifdef DEBUG
	fprintf(stderr, "     left:%d,\n",  im->ImageDesc.Left);
	fprintf(stderr, "      top: %d,\n", im->ImageDesc.Top);
	fprintf(stderr, "    width: %d,\n", im->ImageDesc.Width);
	fprintf(stderr, "   height: %d,\n", im->ImageDesc.Height);
	fprintf(stderr, "interlace: %d,\n", im->ImageDesc.Interlace);
	fprintf(stderr, "colormap: %p\n",   im->ImageDesc.ColorMap);
	fprintf(stderr, "Transparent: %d\n", Transparent);
	fprintf(stderr, "TransparentColor: %d\n", TransparentColor);
	fprintf(stderr, "DelayTime: %d\n", DelayTime);
#endif
	ptr = im->RasterBits;
	for (y = 0; y < im->ImageDesc.Height; y++) {
	    for (x = 0; x < im->ImageDesc.Width; x++) {
		int xi = x + im->ImageDesc.Left;
		int yi = y + im->ImageDesc.Top;
		EPixel_t px;
		u_int8_t p = *ptr++;
		
		if (!Transparent || (p != TransparentColor)) {
		    GifColorType* cp = &cmap->Colors[p];
		    px.a = 255;
		    px.r = cp->Red;
		    px.g = cp->Green;
		    px.b = cp->Blue;
		    EPixmapPutPixel(pixmap, xi, yi, 0, px);
		}
		else if ((i == 0) && Transparent && (p==TransparentColor)) {
		    px.a = 0;
		    px.r = px.g = px.b = 0;
		    EPixmapPutPixel(pixmap, xi, yi, 0, px);
		    useAlpha = 1;
		}
	    }
	}
	mUseAlpha[i] = useAlpha;
    }

    DGifCloseFile(ft);
    return 0;

error:
    DGifCloseFile(ft);
    return -1;
}
