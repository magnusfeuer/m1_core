//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#include "m1.hh"
#include "image_component.hh"
#include "key.hh"
#include "bio_stream.hh"

#include "epic.h"
#include <math.h> // for nearbyintf() rounding function. Link with -lm.
#include <stdarg.h>
#include <png.h>

#ifdef HAVE_LIBJPEG
extern "C" {
#include <jpeglib.h>
}
#endif

XOBJECT_TYPE_BOOTSTRAP(CImageComponent);

/* read image data with BIO structure */
static void png_bio_read(png_structp png_ptr, png_bytep buffer, png_size_t size)
{
    CBioStream* bio = (CBioStream*) png_get_io_ptr(png_ptr);
    int result;

    result = bio->read((void*) buffer, (int) size);
    if (result != (int) size) {
	png_error(png_ptr, "Read Error");
    }
}

// Only load png files, but we should add jpeg and gif!
static EPixmap* load_png(CBioStream* bio, char* file_name, int pixel_type, bool* use_alpha)
{
    unsigned int width;   // image width
    unsigned int height;  // image height
    unsigned int y;
    png_byte header[8];	// 8 is the maximum size that can be checked
    png_bytep* row_pointers;
    int rowbytes;
    png_byte color_type;
    png_byte bit_depth;
    png_structp png_ptr;
    png_infop info_ptr;
    EPixmap* pic = NULL;
    int has_alpha = 0;

    bio->read(header, sizeof(header));
    if (png_sig_cmp(header, 0, 8)) {
	ERRFMT("%s: is not recognized as a PNG file", file_name);
	goto error;
    }

    /* initialize stuff */
    if ((png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,NULL,NULL,NULL)) == NULL) {
	ERRFMT("%s: png_create_read_struct failed", file_name);
	goto error;
    }

    if ((info_ptr = png_create_info_struct(png_ptr)) == NULL) {
	ERRFMT("%s: png_create_info_struct failed", file_name);
	goto error;
    }

    if (setjmp(png_jmpbuf(png_ptr))) {
	ERRFMT("%s: Error during init_io",  file_name);
	goto error;
    }
    
    // png_init_io(png_ptr, fp);
    png_set_read_fn(png_ptr, (png_voidp)bio, png_bio_read);
    png_set_sig_bytes(png_ptr, 8);

    png_read_info(png_ptr, info_ptr);

    width       = info_ptr->width;
    height      = info_ptr->height;
    color_type  = info_ptr->color_type;
    bit_depth   = info_ptr->bit_depth;

    png_read_update_info(png_ptr, info_ptr);

    rowbytes = info_ptr->rowbytes;

    pic = EPixmapCreate(width, height, pixel_type);

    /* read file */
    if (setjmp(png_jmpbuf(png_ptr))) {
	ERRFMT("%s: Error during read_image", file_name);
	goto error;
    }

    row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
    if (row_pointers == NULL) {
	ERRFMT("%s: unable to allocate %d bytes", 
	       file_name, sizeof(png_bytep)*height);
	goto error;
    }

    memset(row_pointers, 0, sizeof(png_bytep)*height);
    for (y=0; y < height; y++) {
	if ((row_pointers[y] = (png_byte*) malloc(rowbytes)) == NULL) {
	    ERRFMT("%s: unable to allocate %d bytes", 
		   file_name, rowbytes);
	    goto error;
	}
    }
    png_read_image(png_ptr, row_pointers);

    // png_destroy_info_struct(png_ptr, info_ptr);

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    
    for (y = 0; y < height; y++) {
	static int c_warn = 1;
	
	switch(color_type) {
	case PNG_COLOR_TYPE_RGBA:
	    if (!has_alpha) {
		u_int8_t* aptr = row_pointers[y];
		int an = width;
		while(!has_alpha && an) {
		    if (aptr[3] != 255) 
			has_alpha = 1;
		    aptr += 4;
		    an--;
		}
	    }
	    EPixmapPutPixels(pic, 0, y, width, 1, 
			     EPIXEL_TYPE_RGBA, 0, row_pointers[y], width*4);
	    break;

	case PNG_COLOR_TYPE_RGB:
	    EPixmapPutPixels(pic, 0, y, width, 1,
			     EPIXEL_TYPE_RGB, 0, row_pointers[y], width*3);
	    break;
	default:
	    if (c_warn) {
		ERRFMT("%s; unknown color type %d", file_name, color_type);
		c_warn = 0;
	    }
	    break;
	}
	free(row_pointers[y]);
    }
    *use_alpha = has_alpha;
    return pic;

error:
    if (pic != NULL)
	EPixmapDestroy(pic);
    *use_alpha = 0;
    return NULL;
}

#ifdef HAVE_LIBJPEG
			   
struct my_error_mgr {
    struct jpeg_error_mgr pub;    /* "public" fields */
    jmp_buf setjmp_buffer;        /* for return to caller */
};

typedef struct my_error_mgr*  my_error_ptr;

static void my_error_exit (j_common_ptr cinfo)
{
  /* cinfo->err really points to a my_error_mgr struct, so coerce pointer */
  my_error_ptr myerr = (my_error_ptr) cinfo->err;

  /* Always display the message. */
  /* We could postpone this until after returning, if we chose. */
  (*cinfo->err->output_message) (cinfo);

  /* Return control to the setjmp point */
  longjmp(myerr->setjmp_buffer, 1);
}


static EPixmap* load_jpeg(char * file_name, int pixel_type, bool* use_alpha)
{
  /* This struct contains the JPEG decompression parameters and pointers to
   * working space (which is allocated as needed by the JPEG library).
   */
  struct jpeg_decompress_struct cinfo;
  /* We use our private extension JPEG error handler.
   * Note that this struct must live as long as the main JPEG parameter
   * struct, to avoid dangling-pointer problems.
   */
  struct my_error_mgr jerr;
  /* More stuff */
  FILE * infile;                /* source file */
  JSAMPARRAY buffer;            /* Output row buffer */
  int row_stride;               /* physical row width in output buffer */
  EPixmap* pic = NULL;

  /* In this example we want to open the input file before doing anything else,
   * so that the setjmp() error recovery below can assume the file is open.
   * VERY IMPORTANT: use "b" option to fopen() if you are on a machine that
   * requires it in order to read binary files.
   */

  if ((infile = fopen(file_name, "rb")) == NULL) {
      fprintf(stderr, "can't open %s\n", file_name);
      return NULL;
  }

  /* Step 1: allocate and initialize JPEG decompression object */
  
  /* We set up the normal JPEG error routines, then override error_exit. */
  cinfo.err = jpeg_std_error(&jerr.pub);
  jerr.pub.error_exit = my_error_exit;
  /* Establish the setjmp return context for my_error_exit to use. */
  if (setjmp(jerr.setjmp_buffer)) {
  error:
      /* If we get here, the JPEG code has signaled an error.
       * We need to clean up the JPEG object, close the input file, and return.
       */
      jpeg_destroy_decompress(&cinfo);
      if (pic) EPixmapDestroy(pic);
      fclose(infile);
      return NULL;
  }
  /* Now we can initialize the JPEG decompression object. */
  jpeg_create_decompress(&cinfo);


  /* Step 2: specify data source (eg, a file) */

  jpeg_stdio_src(&cinfo, infile);

  /* Step 3: read file parameters with jpeg_read_header() */

  (void) jpeg_read_header(&cinfo, TRUE);
  /* We can ignore the return value from jpeg_read_header since
   *   (a) suspension is not possible with the stdio data source, and
   *   (b) we passed TRUE to reject a tables-only JPEG file as an error.
   * See libjpeg.doc for more info.
   */
  fprintf(stderr, "JPEG: image %dx%d\n", 
	  cinfo.image_width, cinfo.image_height);
  /* Step 4: set parameters for decompression */
  cinfo.out_color_space = JCS_RGB;
  /* scale image 1/10 */
  cinfo.scale_num = 1;
  cinfo.scale_denom = 10;


  /* In this example, we don't need to change any of the defaults set by
   * jpeg_read_header(), so we do nothing here.
   */

  /* Step 5: Start decompressor */

  (void) jpeg_start_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* We may need to do some setup of our own at this point before reading
   * the data.  After jpeg_start_decompress() we have the correct scaled
   * output image dimensions available, as well as the output colormap
   * if we asked for color quantization.
   * In this example, we need to make an output work buffer of the right size.
   */ 
  /* JSAMPLEs per row in output buffer */
  row_stride = cinfo.output_width * cinfo.output_components;
  /* Make a one-row-high sample array that will go away when done with image */
  buffer = (*cinfo.mem->alloc_sarray)
      ((j_common_ptr) &cinfo, JPOOL_IMAGE, row_stride, 1);

  pic = EPixmapCreate(cinfo.output_width, cinfo.output_height, pixel_type);
  if (pic == NULL)
      goto error;

  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */

  /* Here we use the library's state variable cinfo.output_scanline as the
   * loop counter, so that we don't have to keep track ourselves.
   */
  while (cinfo.output_scanline < cinfo.output_height) {
      int y = cinfo.output_scanline;
      /* jpeg_read_scanlines expects an array of pointers to scanlines.
       * Here the array is only one element long, but you could ask for
       * more than one scanline at a time if that's more convenient.
       */
      (void) jpeg_read_scanlines(&cinfo, buffer, 1);
      /* Assume put_scanline_someplace wants a pointer and sample count. */
      EPixmapPutPixels(pic, 0, y, cinfo.output_width, 1,
		       EPIXEL_TYPE_RGB, 0, buffer[0], 
		       3*cinfo.output_width);
  }

  /* Step 7: Finish decompression */

  (void) jpeg_finish_decompress(&cinfo);
  /* We can ignore the return value since suspension is not possible
   * with the stdio data source.
   */

  /* Step 8: Release JPEG decompression object */

  /* This is an important step since it will release a good deal of memory. */
  jpeg_destroy_decompress(&cinfo);

  /* After finish_decompress, we can close the input file.
   * Here we postpone it until after no more JPEG errors are possible,
   * so as to simplify the setjmp error logic above.  (Actually, I don't
   * think that jpeg_destroy can do an error exit, but why assume anything...)
   */
  fclose(infile);

  /* At this point you may want to check to see whether any corrupt-data
   * warnings occurred (test whether jerr.pub.num_warnings is nonzero).
   */

  /* And we're done! */
  *use_alpha = false;
  return pic;
}
#endif


static string extension(string filename)
{
    string::size_type loc = filename.rfind('.');
    if (loc == string::npos)
	return "";
    else
	return filename.substr(loc);
}


CImageComponent::CImageComponent(CExecutor* aExec, CBaseType *aType):
    CLayerComponent(aExec, aType),
    mImageFile(this),
    mPixelTypeString(this),
    mIgnoreAlpha(this),
    mBorderWidth(this),
    mBorderColor(this),
    mImage(NULL),
    mPixelType(EPIXEL_TYPE_BGRA),
    mUseAlpha(false)
{
    mIgnoreAlpha.putValue(aExec, false);
    mPixelTypeString.putValue(aExec, "bgra");

    eventPut(aExec,XINDEX(CImageComponent, imageFile), &mImageFile);
    eventPut(aExec,XINDEX(CImageComponent, pixelType), &mPixelTypeString);
    eventPut(aExec,XINDEX(CImageComponent, ignoreAlpha), &mIgnoreAlpha);
    eventPut(aExec,XINDEX(CImageComponent, borderWidth), &mBorderWidth);
    eventPut(aExec,XINDEX(CImageComponent, borderColor), &mBorderColor);
}

CImageComponent::~CImageComponent(void)
{
    DBGFMT("CImageComponent::~CShapeComponent(): Called");
    if (mImage != NULL)
	EPixmapDestroy(mImage);
}

void CImageComponent::loadImage(CExecutor* aExec, bool aStart)
{
    if (mPixelTypeString.assigned()) {
	string name = mPixelTypeString.value();
	mPixelType = EPixelTypeFromName((char*)name.c_str());
    }

    if (aStart && mIgnoreAlpha.assigned())
	mIgnoreAlpha.cancel(aExec);

    if (mImageFile.assigned() || mPixelTypeString.assigned()) {
	string filename = mImageFile.value();
	string ext = extension(filename);
	DBGFMT("mImageFile set to [%s]",(char*) filename.c_str());
	if (mImage != NULL) {
	    EPixmapDestroy(mImage);
	    mImage = NULL;
	}
	if (strcasecmp(ext.c_str(), ".png") == 0) {
	    CBioStream* bio = new CBioStream();
	    char* file_name = (char*) filename.c_str();
	    if (bio->open_file_read(filename, M1_KEY_USAGE_IMAGE, 
				    false, false) < 0) {
		ERRFMT("can't open %s\n", file_name);
		mImage = NULL;
	    }
	    else {
		mImage = load_png(bio, file_name, mPixelType, &mUseAlpha);
		bio->close();  // delete will be preformed by sweeper !!!
	    }
	}
#ifdef HAVE_LIBJPEG
	else if ((strcasecmp(ext.c_str(), ".jpeg") == 0) ||
		 (strcasecmp(ext.c_str(), ".jpg") == 0)) {
	    mImage = load_jpeg((char*) filename.c_str(),mPixelType,&mUseAlpha);
	}
#endif
	DBGFMT("Image %s useAlpha=%d", filename.c_str(), mUseAlpha);
	if (mUseAlpha && mIgnoreAlpha.value()) {
	    DBGFMT("Image %s alpha ignored", filename.c_str(), mUseAlpha);
	    mUseAlpha = false;
	}
	
	if (mImage != NULL) {
	    mContentWidth.putValue(aExec, mImage->width);
	    mContentHeight.putValue(aExec, mImage->height);
	}
	else {
	    mContentWidth.putValue(aExec, 0);
	    mContentHeight.putValue(aExec, 0);
	}
	if (aStart && mImageFile.assigned())
	    mImageFile.cancel(aExec);
	if (aStart && mPixelTypeString.assigned())
	    mPixelTypeString.cancel(aExec);
    }
}


void CImageComponent::start(CExecutor* aExec)
{
    DBGFMT("CImageComponent::start called");
    loadImage(aExec, true);
}

void CImageComponent::execute(CExecutor* aExec)
{
    DBGFMT("CImageComponent::execute called");

    loadImage(aExec, false);
}


void CImageComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    EGc* gc;
    u_int8_t fader;
    EPixel_t color;
    EPixmap* sImage;

    if (!aContext || !aContext->mPixmap) {
	printf("CImageComponent::redraw(): No context or pixmap provided.\n");
	return;
    }

    // Nil image = no joy.    
    if (!mImage) 
	return;
	
    sImage = mImage;

    if (needScale(aContext) && canScale(aContext)) {
	EPixmap*  dImage;

	if ((dImage = EPixmapCreate(int(aContext->cWidth),
				    int(aContext->cHeight),
				    aContext->mPixmap->pixelType)) != NULL) {
	    scaleImage(mImage, dImage, true);
	    sImage = dImage;
	}
    }
    gc = aContext->mGc;
    fader = gc->fader_value;

    EGcSetBorderWidth(gc, mBorderWidth.value());
    if (mBorderWidth.value()) {
	color.px = mBorderColor.value();
	color.a  = 255; // FIXME
	if (fader != ALPHA_FACTOR_1)
	    color.a = (color.a * fader) >> 8;
	EGcSetBorderColor(gc, color);
    }

    if (mUseAlpha)
	// Image contain alpha values
	EPixmapFadeArea(sImage, aContext->mPixmap,
			fader,
			0, 0,
			int(aContext->lLeft), 
			int(aContext->lTop),
			sImage->width, sImage->height);
    else
	// Image do not contain alpha values
	EPixmapAlphaArea(sImage, aContext->mPixmap,
			 fader,
			 0, 0,
			 int(aContext->lLeft), 
			 int(aContext->lTop),
			 sImage->width, sImage->height);
    if (sImage != mImage)
	EPixmapDestroy(sImage);
}


