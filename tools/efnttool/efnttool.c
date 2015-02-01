/*
 * efnttool.c
 *
 *   Convert fonts to efnt format
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <png.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#ifdef HAVE_FREETYPE_2
#include <ft2build.h>
#include FT_FREETYPE_H
#endif


#include "epic.h"

int debug = 0;

#define ERRFMT(...)      emit(stderr,1,__FILE__, __LINE__, __VA_ARGS__)

#define DBGFMT(...) do { \
	if (debug) emit(stdout,0,__FILE__, __LINE__, __VA_ARGS__); \
} while(0)

#define DEFAULT_XRES 75
#define DEFAULT_YRES 75
#define MM_INCH 0.0393700787

void usage()
{
    fprintf(stderr,
	    "usage:\n"
	    "  efnttool [-o <outfile>] [-xfs <name> <size>] [-xres <dpi>] [-yres <dpi>] [-autores] [-d <displayname>] [-ft <filename> <fontsize>] [[-edf | -bdf] <file>] \n");
    exit(1);
}

// Output helpers
void emit(FILE* f, int info, char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);
    if (info)
	fprintf(f, "%s:%d: ", file, line); 
    vfprintf(f, fmt, ap);
    va_end(ap);
}



static EPixmap* load_image(char* file_name, int pixel_type)
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

    /* open file and test for it being a png */
    FILE *fp = fopen(file_name, "rb");

    if (!fp) {
	ERRFMT("%s: could not be opened for reading\n", file_name);
	goto error;
    }

    fread(header, 1, 8, fp);
    if (png_sig_cmp(header, 0, 8)) {
	ERRFMT("%s: is not recognized as a PNG file\n", file_name);
	goto error;
    }

    /* initialize stuff */
    if ((png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING,NULL,NULL,NULL)) == NULL) {
	ERRFMT("%s: png_create_read_struct failed\n", file_name);
	goto error;
    }

    if ((info_ptr = png_create_info_struct(png_ptr)) == NULL) {
	ERRFMT("%s: png_create_info_struct failed\n", file_name);
	goto error;
    }

    if (setjmp(png_jmpbuf(png_ptr))) {
	ERRFMT("%s: Error during init_io\n",  file_name);
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

    pic = EPixmapCreate(width, height, pixel_type);

    /* read file */
    if (setjmp(png_jmpbuf(png_ptr))) {
	ERRFMT("%s: Error during read_image\n", file_name);
	goto error;
    }

    row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
    if (row_pointers == NULL) {
	ERRFMT("%s: unable to allocate %d bytes\n", 
	       file_name, sizeof(png_bytep)*height);
	goto error;
    }

    memset(row_pointers, 0, sizeof(png_bytep)*height);
    for (y=0; y < height; y++) {
	if ((row_pointers[y] = (png_byte*) malloc(rowbytes)) == NULL) {
	    ERRFMT("%s: unable to allocate %d bytes\n", 
		   file_name, rowbytes);
	    goto error;
	}
    }
    png_read_image(png_ptr, row_pointers);
    fclose(fp);
    fp = NULL;

    // png_destroy_info_struct(png_ptr, info_ptr);

    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);

    
    for (y = 0; y < height; y++) {
	static int c_warn = 1;
	
	switch(color_type) {
	case PNG_COLOR_TYPE_RGBA:
	    EPixmapPutPixels(pic, 0, y, width, 1, 
			     EPIXEL_TYPE_RGBA, 0, row_pointers[y], width*4);
	    break;
	case PNG_COLOR_TYPE_RGB:
	    EPixmapPutPixels(pic, 0, y, width, 1,
			     EPIXEL_TYPE_RGB, 0, row_pointers[y], width*3);
	    break;
	default:
	    if (c_warn) {
		ERRFMT("%s; unknown color type %d\n", file_name, color_type);
		c_warn = 0;
	    }
	    break;
	}
	free(row_pointers[y]);
    }
    return pic;

error:
    if (pic != NULL)
	EPixmapDestroy(pic);
    if (fp != NULL)
	fclose(fp);
    return NULL;
}

/*
 * # is used for comments!
 *  read a pngcfg file
 *  foundry
 *  family
 *  weight
 *  slant
 *  style
 *  spacing
 *  pixel_size
 *  point_size
 *  descent
 *  ascent
 *
 *  code <unicode>    (e.g 'A' = 65,  '1' = 49 )
 *  pixels <file>.png  (png file that contain pixel data)
 *  name 
 *  width
 *  heigh
 *  xoffs
 *  yoffs
 *  dwx
 *  dwy
 *
 */

#define streq(a, b) (strcasecmp((a),(b)) == 0)

inline unsigned char nibble(char x) {
    if ((x >= '0') && (x <= '9'))
	return x-'0';
    else if ((x >= 'A') && (x <= 'F'))
	return (x-'A')+10;
    else if ((x >= 'a') && (x <= 'f'))
	return (x-'a')+10;
    else
	return 0;
}

#define hexbyte(x1,x2) ((nibble(x1) << 4) | nibble(x2))

/* bits to use for default handling of values */
#define GLYPH_NAME   0x80
#define GLYPH_WIDTH  0x01
#define GLYPH_HEIGHT 0x02
#define GLYPH_XOFFS  0x04
#define GLYPH_YOFFS  0x08
#define GLYPH_DWX    0x10
#define GLYPH_DWY    0x20

#define FONT_PIXEL_SIZE   0x01
#define FONT_POINT_SIZE   0x02
#define FONT_RESOLUTION_X 0x04
#define FONT_RESOLUTION_Y 0x08
#define FONT_DESCENT      0x10
#define FONT_ASCENT       0x20


typedef struct _key_value {
    char*     key;
    u_int32_t value;
} KeyValue;

typedef struct _string_table {
    size_t n;       /* number of entries */
    size_t size;    /* size in bytes */
    size_t remain;  /* remain data int free area */
    char*  ptr;     /* pointer to next free data */
    char*  data;
} StringTable;

typedef struct _glyph {
    size_t   size;    /* size=0 means not used */
    EGlyph*  glyph;   /* glyph=NULL also means not used */
} GlyphData;

typedef struct _glyph_table {
    size_t     n;     /* number of glyphs in table (not counting NULL elems) */
    size_t     size;  /* size of glyph_data table */
    GlyphData* glyph_data;
} GlyphTable;

KeyValue kv_weight[] = {
    {  "none",    EFONT_WEIGHT_NONE },
    { "medium", EFONT_WEIGHT_MEDIUM},
    { "bold",   EFONT_WEIGHT_BOLD },
    { "demibold", EFONT_WEIGHT_DEMIBOLD },
    {  "",    EFONT_WEIGHT_NONE },
    { NULL, 0} 
};

KeyValue kv_slant[] = {
    { "none", EFONT_SLANT_NONE },
    { "r", EFONT_SLANT_ROMAN },
    { "i", EFONT_SLANT_ITALIC },
    { "o", EFONT_SLANT_OBLIQUE},
    { "ri", EFONT_SLANT_REVERSE_ITALIC},
    { "ro", EFONT_SLANT_REVERSE_OBLIQUE},
    { "ot", EFONT_SLANT_OTHER },
    { "roman", EFONT_SLANT_ROMAN },
    { "italic", EFONT_SLANT_ITALIC },
    { "bold italic", EFONT_SLANT_ITALIC }, // MacOS Arial Bold italic.
    { "oblique", EFONT_SLANT_OBLIQUE},
    { "reverse italic", EFONT_SLANT_REVERSE_ITALIC},
    { "reverse oblique", EFONT_SLANT_REVERSE_OBLIQUE},
    { "other", EFONT_SLANT_OTHER },
    { "", EFONT_SLANT_NONE },
    { NULL, 0} 
};
    
KeyValue kv_width[] = {
    { "none", EFONT_WIDTH_NONE},
    { "normal", EFONT_WIDTH_NORMAL},
    { "condensed", EFONT_WIDTH_CONDENSED},
    { "narrow", EFONT_WIDTH_NARROW},
    { "double wide", EFONT_WIDTH_DOUBLE_WIDE},
    { "", EFONT_WIDTH_NONE},
    { NULL, 0} 
};

KeyValue kv_style[] = {
    { "none",        EFONT_STYLE_NONE},
    { "serif",       EFONT_STYLE_SERIF},
    { "sans serfi",  EFONT_STYLE_SANS_SERIF},
    { "informal",    EFONT_STYLE_INFORMAL},
    { "decorated",   EFONT_STYLE_DECORATED},
    { "",        EFONT_STYLE_NONE},
    { NULL, 0} 
};

KeyValue kv_spacing[] = {
    { "none",       EFONT_SPACING_NONE},
    { "p",   EFONT_SPACING_PROPORTIONAL},
    { "m",   EFONT_SPACING_MONOSPACED},
    { "c",   EFONT_SPACING_CHAR_CELL},
    { "proportional", EFONT_SPACING_PROPORTIONAL},
    { "monospaced",   EFONT_SPACING_MONOSPACED},
    { "charcell",     EFONT_SPACING_CHAR_CELL},
    { "",         EFONT_SPACING_NONE},
    { NULL, 0} 
};

u_int32_t kv_lookup(KeyValue* table, char* key, int* found)
{
    int i = 0;
    *found = 0;
    while(table[i].key != NULL) {
	if (streq(table[i].key, key)) {
	    *found = 1;
	    return table[i].value;
	}
	i++;
    }
    return 0;
}

int value_u16(char* value, u_int16_t* vp, char* file, int line)
{
    unsigned long v;
    char* ep = NULL;
    v = strtoul(value, &ep, 0);
    if (*ep != '\0') goto error;
    if ((v > 0xffff)) goto out_of_range;
    *vp = v;
    return 1;
error:
    fprintf(stderr, "%s:%d: integer value expected\n", file, line);
    return 0;
out_of_range:
    fprintf(stderr, "%s:%d: value out of range\n", file, line);
    return 0;
}

int value_i16(char* value, int16_t* vp, char* file, int line)
{
    long v;
    char* ep = NULL;
    v = strtol(value, &ep, 0);
    if (*ep != '\0') goto error;
    if ((v < -0x8000) || (v > 0x7fff)) goto out_of_range;
    *vp = v;
    return 1;
error:
    fprintf(stderr, "%s:%d: integer value expected\n", file, line);
    return 0;
out_of_range:
    fprintf(stderr, "%s:%d: value out of range\n", file, line);
    return 0;
}

int value_u32(char* value, u_int32_t* vp, char* file, int line)
{
    unsigned long v;
    char* ep = NULL;
    v = strtoul(value, &ep, 0);
    if (*ep != '\0') goto error;
    *vp = v;
    return 1;
error:
    fprintf(stderr, "%s:%d: integer value expected\n", file, line);
    return 0;
}


StringTable* new_string_table(void)
{
    return (StringTable*) calloc(1, sizeof(StringTable));
}

/* add a string and return "byte" offset 
 * the string is maxed to 255 characters and
 * has a leading length byte and a terminating zero char
 */
u_int32_t add_string(StringTable* table, char* string)
{
    int sz = strlen(string);
    u_int32_t offset = table->size;

    if (table->remain < (sz+2)) {
	table->data = realloc(table->data, table->size+(sz+2)+1024);
	table->ptr  = table->data + table->size;
	table->remain = (sz+2)+1024;
    }
    table->ptr[0] = sz;
    memcpy(table->ptr+1, string, sz);
    table->ptr[sz+1] = '\0';
    table->ptr    += (sz+2);
    table->size   += (sz+2);
    table->remain -= (sz+2);
    table->n++;
    return offset;
}

GlyphTable* new_glyph_table(void)
{
    return (GlyphTable*) calloc(1, sizeof(GlyphTable));
}

int add_glyph(GlyphTable* table, int index, int vmask, int pixelType,
	      EGlyph* glyph, EPixmap* pix)
{
    size_t glyph_size;

    if ((pix == NULL) || (glyph == NULL))
	return -1;

    if (index >= table->size) {
	int i;
	int new_size = index + 128;
	table->glyph_data = realloc(table->glyph_data,
				    sizeof(GlyphData)*new_size);
	for (i = table->size; i < new_size; i++) {
	    table->glyph_data[i].size = 0;
	    table->glyph_data[i].glyph = NULL;
	}
	table->size = new_size;
    }

    /* convert the pixmap to the destination format */
    if (pixelType != pix->pixelType) {
	EPixmap* npix = EPixmapCreate(pix->width, pix->height, pixelType);
	EPixmapCopy(pix, npix);
	EPixmapDestroy(pix);
	pix = npix;
    }

    /* updated default values where needed */
    if (!(vmask & GLYPH_WIDTH))    glyph->width = pix->width;
    if (!(vmask & GLYPH_HEIGHT))   glyph->height = pix->height;
    if (!(vmask & GLYPH_XOFFS))    glyph->xoffs = 0;
    if (!(vmask & GLYPH_YOFFS))    glyph->yoffs = 0;
    if (!(vmask & GLYPH_DWX))      glyph->dwx = glyph->width;
    if (!(vmask & GLYPH_DWY))      glyph->dwy = 0;
    
    glyph_size = sizeof(EGlyph) + pix->sz;
    table->glyph_data[index].size = glyph_size;
    table->glyph_data[index].glyph = malloc(glyph_size);
    memcpy(table->glyph_data[index].glyph, glyph, sizeof(EGlyph));
    memcpy(((char*)table->glyph_data[index].glyph)+sizeof(EGlyph),
	   pix->data, pix->sz);
    EPixmapDestroy(pix);
    table->n++;
    return 0;
}

/* load efnt description file (edf) 
 * return -1 on error or FONT_x value mask on success
 */

int load_edf(char* file, EFontFile* efnt,
	      StringTable* string_table,
	      GlyphTable* glyph_table)
{
    FILE* f;
    int line = 1;
    char buf[1024];
    int in_font = 0;
    int in_glyph = 0;
    int curr_glyph = -1;
    int nerror = 0;
    EGlyph glyph;
    EPixmap* gpix;
    int glyph_vmask;
    int font_vmask;
    char* imagedir = "";
    char* imagefmt = "%s";

    if ((f = fopen(file, "r")) == NULL) {
	ERRFMT("unable to open file %s\n");
	return -1;
    }

    memset(&glyph, 0, sizeof(EGlyph));
    gpix = NULL;
    glyph_vmask = 0;
    font_vmask = 0;

    while(fgets(buf, 1024, f) != NULL) {
	char ibuf[1024];
	char* key;
	char* value;
	char* ptr;
	int found = 0;

	/* split each line into key/value */
	strcpy(ibuf, buf);  // save for error output
	if ((ptr = strchr(buf, '#')) != NULL) *ptr = '\0';
	ptr = buf;
	while(isspace(*ptr)) ptr++;
	if (!*ptr) {  // blank line
	    line++;
	    continue;
	}
	key = ptr;  // save start of key
	while(*ptr && !isspace(*ptr)) ptr++;
	*ptr++ = '\0'; // terminate key 
	while(isspace(*ptr)) ptr++;
	value = ptr;
	ptr = ptr + strlen(ptr);
	while(isspace(*(ptr-1))) ptr--;
	*ptr='\0';

	// printf("key='%s', value='%s'\n", key, value);

	if (streq(key, "begin") && streq(value, "font")) {
	    in_font = 1;
	    continue;
	}
	else if (streq(key, "begin") && streq(value, "glyph")) {
	    memset(&glyph, 0, sizeof(EGlyph));
	    in_glyph = 1;
	    continue;
	}
	else if (streq(key, "imagedir")) {
	    imagedir = strdup(value);
	    continue;
	}
	else if (streq(key, "imagefmt")) {
	    imagefmt = strdup(value);
	    continue;
	}
	else if (streq(key, "end")) {
	    if (in_glyph) {
		if (add_glyph(glyph_table, curr_glyph, glyph_vmask,
			      efnt->font_info.pixel_type,
			      &glyph, gpix) < 0) {
		    fprintf(stderr, "%s:%d: could not add glyph %d\n",
			    file, line, curr_glyph);
		    nerror++;
		}
		memset(&glyph, 0, sizeof(EGlyph));
		curr_glyph = -1;
		glyph_vmask = 0;
		gpix = NULL;
	    }
	    in_glyph = 0;
	    in_font  = 0;
	    continue;
	}

	if (in_font && in_glyph) {
	    fprintf(stderr, "%s:%d: missing end\n", file, line);
	    in_font = 0;
	    nerror++;
	}

	if (in_font) {
	    /* load font info */
	    if (streq(key, "foundry"))
		efnt->foundry_offset = add_string(string_table, value);
	    else if (streq(key, "family"))
		efnt->family_offset = add_string(string_table, value);
	    else if (streq(key, "weight")) {
		efnt->font_info.weight = kv_lookup(kv_weight, value, &found);
		if (!found) {
		    fprintf(stderr, "%s:%d: unknown weight '%s'\n",
			    file, line, value);
		    nerror++;
		}
	    }
	    else if (streq(key, "slant")) {
		efnt->font_info.slant = kv_lookup(kv_slant, value, &found);
		if (!found) {
		    fprintf(stderr, "%s:%d: unknown slant '%s'\n",
			    file, line, value);
		    nerror++;
		}
	    }
	    else if (streq(key, "width")) {
		efnt->font_info.width = kv_lookup(kv_width, value, &found);
		if (!found) {
		    fprintf(stderr, "%s:%d: unknown width '%s'\n",
			    file, line, value);
		    nerror++;
		}
	    }
	    else if (streq(key, "style")) {
		efnt->font_info.style = kv_lookup(kv_style, value, &found);
		if (!found) {
		    fprintf(stderr, "%s:%d: unknown style '%s'\n",
			    file, line, value);
		    nerror++;
		}
	    }
	    else if (streq(key, "spacing")) {
		efnt->font_info.style = kv_lookup(kv_spacing, value, &found);
		if (!found) {
		    fprintf(stderr, "%s:%d: unknown spacing '%s'\n",
			    file, line, value);
		    nerror++;
		}
	    }
	    else if (streq(key, "pixel_type")) {
		int pixelType = EPixelTypeFromName(value);
		if (pixelType < 0) {
		    fprintf(stderr, "%s:%d: unknown pixel type\n",
			    file, line);
		    nerror++;
		}
		else
		    efnt->font_info.pixel_type = pixelType;
	    }
	    else if (streq(key, "pixel_size")) {
		if (value_u32(value,&efnt->font_info.pixel_size,file,line))
		    font_vmask |= FONT_PIXEL_SIZE;
		else
		    nerror++;
	    }
	    else if (streq(key, "point_size")) {
		if (value_u32(value, &efnt->font_info.point_size,file,line))
		    font_vmask |= FONT_POINT_SIZE;
		else
		    nerror++;
	    }
	    else if (streq(key, "resolution_x")) {
		if (value_u32(value, &efnt->font_info.resolution_x,file,line))
		    font_vmask |= FONT_RESOLUTION_X;
		else
		    nerror++;
	    }
	    else if (streq(key, "resolution_y")) {
		if (value_u32(value, &efnt->font_info.resolution_y,file,line))
		    font_vmask |= FONT_RESOLUTION_Y;
		else
		    nerror++;
	    }
	    else if (streq(key, "descent")) {
		if (value_u32(value, &efnt->font_info.descent,file,line))
		    font_vmask |= FONT_DESCENT;
		else
		    nerror++;
	    }
	    else if (streq(key, "ascent")) {
		if (value_u32(value, &efnt->font_info.ascent,file,line))
		    font_vmask |= FONT_ASCENT;
		else
		    nerror++;
	    }
	    else {
		fprintf(stderr, "%s:%d: unknown config '%s'\n",
			file, line, key);
		nerror++;
	    }
	}
	else if (in_glyph) {
	    if (streq(key, "code")) {
		curr_glyph = atoi(value);
	    }
	    else if (streq(key, "image")) {
		char name[1024];
		char filename[1024];

		sprintf(name, imagefmt, value); /* generate name */
		if (strlen(imagedir) == 0) 
		    strcpy(filename, name);
		else if (name[strlen(name)-1] == '/')
		    sprintf(filename, "%s%s", imagedir, name);
		else
		    sprintf(filename, "%s/%s", imagedir, name);
		fprintf(stderr, "loading: %s\n", filename);
		if ((gpix=load_image(filename,efnt->font_info.pixel_type)) == NULL)
		    nerror++;
	    }
	    else if (streq(key, "name")) {
		glyph.name_offset = add_string(string_table, value);
		glyph_vmask |= GLYPH_NAME;
	    }
	    else if (streq(key, "width")) {
		if (value_u16(value, &glyph.width, file, line))
		    glyph_vmask |= GLYPH_WIDTH;
		else
		    nerror++;
	    }
	    else if (streq(key, "height")) {
		if (value_u16(value, &glyph.height, file, line))
		    glyph_vmask |= GLYPH_HEIGHT;
		else
		    nerror++;
	    }
	    else if (streq(key, "xoffs")) {
		if (value_i16(value, &glyph.xoffs, file, line))
		    glyph_vmask |= GLYPH_XOFFS;
		else
		    nerror++;
	    }
	    else if (streq(key, "yoffs")) {
		if (value_i16(value, &glyph.yoffs, file, line))
		    glyph_vmask |= GLYPH_YOFFS;
		else
		    nerror++;
	    }
	    else if (streq(key, "dwx"))  {
		if (value_i16(value, &glyph.dwx, file, line))
		    glyph_vmask |= GLYPH_DWX;
		else
		    nerror++;
	    }
	    else if (streq(key, "dwy")) {
		if (value_i16(value, &glyph.dwy, file, line))
		    glyph_vmask |= GLYPH_DWY;
		else
		    nerror++;
	    }
	    else {
		fprintf(stderr, "%s:%d: unknown glyph config '%s'\n",
			file, line, key);
		nerror++;
	    }
	}
	line++;
    }
    fclose(f);

    if (nerror)
	return -1;
    return font_vmask;
}

/*
 *  Read a token from bdf file. 
 *    Special treatment of double (single?) quoted strings.
 *    Skipping COMMENT's
 */
char* bdf_token(FILE* f, int* line, int line_token, char* buf, int len)
{
    int c;
    char* ptr = buf;
    char* ptr_end = buf + len - 1;

restart:
    c = fgetc(f);

    /* skip space */
    while (isspace(c)) {
	if (c == '\n') (*line)++;
	c=fgetc(f);
    }

    /* handle double quoted string */
    if (c == '"') {
	ptr = buf;
	c = fgetc(f);
	while((c != EOF) && (c != '"') && (ptr < ptr_end)) {
	    *ptr++ = c;
	    if (c == '\n') (*line)++;
	    c = fgetc(f);
	}
	if (c == '"') {
	    *ptr = '\0';
	    return buf;
	}
	if (ptr == ptr_end) goto overflow;
	return NULL;
    }
    /* handle token */
    else if (c != EOF) {
	ptr = buf;
	*ptr++ = c;
	c = fgetc(f);
	while((c != EOF) && ((line_token && (c != '\n')) ||
			     (!line_token && !isspace(c)))
	      && (ptr < ptr_end)) {
	    *ptr++ = c;
	    c = fgetc(f);
	}
	if (c == '\n') (*line)++;
	if (ptr == ptr_end) goto overflow;
	if (line_token) {
	    while((ptr > buf) && isspace(ptr[-1]))
		ptr--;
	}
	*ptr = '\0';
	if (streq(buf, "COMMENT")) {
	    /* skip to end of line and restart */
	    c = fgetc(f);
	    while((c != '\n') && (c != EOF))
		c = fgetc(f);
	    if (c == EOF)
		return NULL;
	    (*line)++;
	    goto restart;
	}
	return buf;
    }
    return NULL;
overflow:
    fprintf(stderr, "error: buffer to small\n");
    return NULL;
}


enum {
    BDF_NONE = 0,
    BDF_COPYRIGHT,
    BDF_NOTICE,
    BDF_ADOBE_POSTSCRIPT_FONTNAME,
    BDF_DEC_DEVICE_FONTNAMES,
    BDF_STARTFONT,
    BDF_ENDFONT,
    BDF_FONT,
    BDF_SIZE,
    BDF_SUBSCRIPT_SIZE,
    BDF_SUBSCRIPT_X,
    BDF_SUBSCRIPT_Y,
    BDF_SUPERSCRIPT_SIZE,
    BDF_SUPERSCRIPT_X,
    BDF_SUPERSCRIPT_Y,
    BDF_UNDERLINE_THICKNESS,
    BDF_UNDERLINE_POSITION,
    BDF_ITALIC_ANGLE,
    BDF_FONT_TYPE,
    BDF_RASTERIZER_NAME,
    BDF_BITS_PER_PIXEL,
    BDF_FONTBOUNDINGBOX,
    BDF_STARTPROPERTIES,
    BDF_SWIDTH,
    BDF_DWIDTH,
    BDF_SWIDTH1,
    BDF_DWIDTH1,
    BDF_VVECTOR,
    BDF_CHARS,
    BDF_METRICSET,
    BDF_ENCODING,
    BDF_BBX,
    BDF_ENDCHAR,
    BDF_BITMAP,
    BDF_ATTRIBUTES,
    BDF_STARTCHAR,
    BDF_FOUNDRY,
    BDF_FAMILY_NAME,
    BDF_WEIGHT_NAME,
    BDF_WEIGHT,
    BDF_SLANT,
    BDF_SETWIDTH_NAME,
    BDF_ADD_STYLE_NAME,
    BDF_PIXEL_SIZE,
    BDF_RAW_PIXEL_SIZE,
    BDF_POINT_SIZE,
    BDF_RAW_POINT_SIZE,
    BDF_RESOLUTION_X,
    BDF_RESOLUTION_Y,
    BDF_SPACING,
    BDF_AVERAGE_WIDTH,
    BDF_RAW_AVERAGE_WIDTH,
    BDF_CHARSET_REGISTRY,
    BDF_CHARSET_ENCODING,
    BDF_CAP_HEIGHT,
    BDF_RAW_CAP_HEIGHT,
    BDF_X_HEIGHT,
    BDF_RAW_X_HEIGHT,
    BDF_FONT_ASCENT,
    BDF_RAW_ASCENT,
    BDF_FONT_DESCENT,
    BDF_RAW_DESCENT,
    BDF_FACE_NAME,
    BDF_DEFAULT_CHAR,
    BDF_RELATIVE_SETWIDTH,
    BDF_RELATIVE_WEIGHT,
    BDF_CHARSET_COLLECTIONS,
    BDF_FULL_NAME,
    BDF_ENDPROPERTIES,
    BDF_QUAD_WIDTH,
    BDF_RAW_QUAD_WIDTH,
    BDF_RESOLUTION
};

struct {
    char* key;
    int   val;
    char* args;
} bdf_opts[] = {
    {"NONE", BDF_NONE, ""},  /* dummy */
    {"STARTFONT", BDF_STARTFONT, "s" },
    {"COPYRIGHT", BDF_COPYRIGHT, "s" },
    {"NOTICE", BDF_NOTICE, "s" },
    {"_ADOBE_POSTSCRIPT_FONTNAME", BDF_ADOBE_POSTSCRIPT_FONTNAME, "s"},
    {"_DEC_DEVICE_FONTNAMES", BDF_DEC_DEVICE_FONTNAMES, "s"},
    {"SUBSCRIPT_SIZE", BDF_SUBSCRIPT_SIZE, "i"},
    {"SUBSCRIPT_X", BDF_SUBSCRIPT_X, "i"},
    {"SUBSCRIPT_Y", BDF_SUBSCRIPT_Y, "i"},
    {"SUPERSCRIPT_SIZE", BDF_SUPERSCRIPT_SIZE, "i"},
    {"SUPERSCRIPT_X", BDF_SUPERSCRIPT_X, "i"},
    {"SUPERSCRIPT_Y", BDF_SUPERSCRIPT_Y, "i"},
    {"UNDERLINE_THICKNESS", BDF_UNDERLINE_THICKNESS, "i"},
    {"UNDERLINE_POSITION", BDF_UNDERLINE_POSITION, "i"},
    {"ITALIC_ANGLE", BDF_ITALIC_ANGLE, "i"},
    {"FONT_TYPE", BDF_FONT_TYPE, "s"},
    {"RASTERIZER_NAME", BDF_RASTERIZER_NAME, "s"},
    {"FONT", BDF_FONT, "s" },
    {"SIZE", BDF_SIZE, "iii" },
    {"BITS_PER_PIXEL", BDF_BITS_PER_PIXEL, "i" },
    {"FONTBOUNDINGBOX", BDF_FONTBOUNDINGBOX, "iiii" },
    {"STARTPROPERTIES", BDF_STARTPROPERTIES, "i" },
    {"SWIDTH", BDF_SWIDTH, "ii" },
    {"DWIDTH", BDF_DWIDTH, "ii" },
    {"SWIDTH1", BDF_SWIDTH1, "ii" },
    {"DWIDTH1", BDF_DWIDTH1, "ii" },
    {"VVECTOR", BDF_VVECTOR, "ii" },
    {"CHARS", BDF_CHARS, "i" },
    {"METRICSET", BDF_METRICSET, "i" },
    {"ENCODING", BDF_ENCODING, "i" },
    {"BBX", BDF_BBX, "iiii" },
    {"ENDCHAR", BDF_ENDCHAR, "" },
    {"BITMAP", BDF_BITMAP, "" },
    {"ATTRIBUTES", BDF_ATTRIBUTES, "x" },
    {"STARTCHAR", BDF_STARTCHAR, "s" },
    {"ENDFONT", BDF_ENDFONT, "" },
    {"FOUNDRY", BDF_FOUNDRY, "s" },
    {"FAMILY_NAME", BDF_FAMILY_NAME, "s" },
    {"WEIGHT_NAME", BDF_WEIGHT_NAME, "s" },
    {"WEIGHT", BDF_WEIGHT, "i" },
    {"SLANT", BDF_SLANT, "s" },
    {"SETWIDTH_NAME", BDF_SETWIDTH_NAME, "s" },
    {"ADD_STYLE_NAME", BDF_ADD_STYLE_NAME, "s" },
    {"PIXEL_SIZE", BDF_PIXEL_SIZE, "i" },
    {"RAW_PIXEL_SIZE", BDF_RAW_PIXEL_SIZE, "i" },
    {"POINT_SIZE", BDF_POINT_SIZE, "i" },
    {"RAW_POINT_SIZE", BDF_RAW_POINT_SIZE, "i" },
    {"RESOLUTION_X", BDF_RESOLUTION_X, "i" },
    {"RESOLUTION_Y", BDF_RESOLUTION_Y, "i" },
    {"SPACING", BDF_SPACING, "s" },
    {"AVERAGE_WIDTH", BDF_AVERAGE_WIDTH, "i" },
    {"RAW_AVERAGE_WIDTH", BDF_RAW_AVERAGE_WIDTH, "i" },
    {"CHARSET_REGISTRY", BDF_CHARSET_REGISTRY, "s" },
    {"CHARSET_ENCODING", BDF_CHARSET_ENCODING, "s" },
    {"CAP_HEIGHT", BDF_CAP_HEIGHT, "i" },
    {"RAW_CAP_HEIGHT", BDF_RAW_CAP_HEIGHT, "i" },
    {"X_HEIGHT", BDF_X_HEIGHT, "i" },
    {"RAW_X_HEIGHT", BDF_RAW_X_HEIGHT, "i" },
    {"FONT_ASCENT", BDF_FONT_ASCENT, "i" },
    {"RAW_ASCENT", BDF_RAW_ASCENT, "i" },
    {"FONT_DESCENT", BDF_FONT_DESCENT, "i" },
    {"RAW_DESCENT", BDF_RAW_DESCENT, "i" },
    {"FACE_NAME", BDF_FACE_NAME, "s" },
    {"DEFAULT_CHAR", BDF_DEFAULT_CHAR, "i" },
    {"RELATIVE_SETWIDTH", BDF_RELATIVE_SETWIDTH, "i" },
    {"RELATIVE_WEIGHT", BDF_RELATIVE_WEIGHT, "i" },
    {"CHARSET_COLLECTIONS", BDF_CHARSET_COLLECTIONS, "i" },
    {"FULL_NAME", BDF_FULL_NAME, "s" },
    {"ENDPROPERTIES", BDF_ENDPROPERTIES, ""},
    {"QUAD_WIDTH", BDF_QUAD_WIDTH, "i"},
    {"RAW_QUAD_WIDTH", BDF_RAW_QUAD_WIDTH, "i"},
    {"RESOLUTION", BDF_RESOLUTION, "i"},
    {NULL, 0, NULL}
};

typedef union {
    int i;
    unsigned int x;
    char* s;
} bdf_arg_t;


int bdf_opt(char* name)
{
    int i = 0;

    while((bdf_opts[i].key != NULL) &&
	  !streq(bdf_opts[i].key, name))
	i++;
    if (bdf_opts[i].key == NULL)
	return -1;
    return i;
}

int bdf_parse(FILE* f, char* file, int* line, bdf_arg_t* argv, int* nargs)
{
    int opt;
    int ai;
    char* ap;
    char buf[1024];

    if (bdf_token(f, line, 0, buf, sizeof(buf)) == NULL)
	return 0;

    if ((opt = bdf_opt(buf)) < 0) {
	fprintf(stderr, "%s:%d: Unknown BDF keyword: %s\n", file, *line, buf);
	return -1;
    }
    printf("%d: KEY=%s ", *line, bdf_opts[opt].key);
    ap = bdf_opts[opt].args;
    ai = 0;
    /* special (bugs?) */
    if (bdf_opts[opt].val == BDF_FONT) {
	if (bdf_token(f, line, 1, buf, sizeof(buf)) == NULL)
	    goto error;
	argv[ai].s = strdup(buf);
	printf("STR[%d]='%s' ", ai, argv[ai].s);
	ai = 1;
    }
    else {
	/* keyword arguments */
	while(*ap) {
	    if (bdf_token(f, line, 0, buf, sizeof(buf)) == NULL)
		goto error;
	    if (ai >= *nargs)
		goto error;
	    switch(*ap++) {
	    case 'i':
		argv[ai].i = strtol(buf, NULL, 10);
		printf("INT[%d]=%d ", ai, argv[ai].i);
		ai++;
		break;
	    case 's':
		argv[ai].s = strdup(buf);
		printf("STR[%d]='%s' ", ai, argv[ai].s);
		ai++;
		break;
	    case 'x':
		argv[ai].x = strtol(buf, NULL, 16);
		printf("HEX[%d]=%x ", ai, argv[ai].x);
		ai++;
		break;
	    default:
		goto error;
	    }
	}
    }
    printf("\n");
    *nargs = ai;
    return opt;
error:
    fprintf(stderr, "%s:%d: argument error\n", file, *line);
    return -1;
}
    
/* load bdf description file
 * return -1 on error or FONT_x value mask on success
 */

int load_bdf(char* file, EFontFile* efnt,
	     StringTable* string_table,
	     GlyphTable* glyph_table)
{
    FILE* f;
    int line = 1;
    int srcPixelType = EPIXEL_TYPE_A1;
    int bitsPerPixel = EPIXEL_BIT_SIZE(srcPixelType);
    char buf[1024];
    int in_font = 0;
    int in_glyph = 0;
    int done = 0;
    int curr_glyph = -1;
    int nerror = 0;
    EGlyph glyph;
    EPixmap* gpix;
    int glyph_vmask;
    int font_vmask;

    if ((f = fopen(file, "r")) == NULL) {
	ERRFMT("unable to open file %s\n");
	return -1;
    }

    memset(&glyph, 0, sizeof(EGlyph));
    gpix = NULL;
    glyph_vmask = 0;
    font_vmask = 0;

    efnt->font_info.pixel_type = EPIXEL_TYPE_A8;

    /* global properties */
    while(!done) {
	int opt;
	int found;
	int nargs = 10;
	bdf_arg_t args[10];

	if ((opt = bdf_parse(f, file, &line, args, &nargs)) < 0) {
	    fprintf(stderr, "%s:%d: error\n", file, line);
	    return -1;
	}
	switch(bdf_opts[opt].val) {
	case BDF_NONE:
	    fprintf(stderr, "%s:%d: premature end of file\n", file, line);
	    return -1;
	default:
	    break;
	case BDF_STARTFONT:
	    in_font = 1;
	    break;
	case BDF_ENDFONT:
	    in_font = 0;
	    done = 1;
	    break;
	case BDF_FOUNDRY:
	    efnt->foundry_offset = add_string(string_table, args[0].s);
	    break;
	case BDF_FAMILY_NAME:
	    efnt->family_offset = add_string(string_table, args[0].s);
	    break;
	case BDF_WEIGHT_NAME:
	    efnt->font_info.weight = kv_lookup(kv_weight, args[0].s, &found);
	    if (!found) {
		fprintf(stderr, "%s:%d: unknown WEIGHT_NAME '%s'\n",
			file, line, args[0].s);
		nerror++;
	    }
	    break;
	case BDF_SLANT:
	    efnt->font_info.slant = kv_lookup(kv_slant, args[0].s, &found);
	    if (!found) {
		fprintf(stderr, "%s:%d: unknown SLANT '%s'\n",
			file, line, args[0].s);
		nerror++;
	    }
	    break;
	case BDF_SETWIDTH_NAME:
	    efnt->font_info.width = kv_lookup(kv_width, args[0].s, &found);
	    if (!found) {
		fprintf(stderr, "%s:%d: unknown SETWIDTH_NAME '%s'\n",
			file, line, args[0].s);
		nerror++;
	    }
	    break;
	case BDF_SPACING:
	    kv_lookup(kv_spacing, args[0].s, &found);
	    if (!found) {
		fprintf(stderr, "%s:%d: unknown SPACING '%s'\n",
			file, line, args[0].s);
		nerror++;
	    }
	    break;

	case BDF_PIXEL_SIZE:
	    efnt->font_info.pixel_size = args[0].i;
	    font_vmask |= FONT_PIXEL_SIZE;
	    break;

	case BDF_SIZE:
	    efnt->font_info.point_size = args[0].i;
	    font_vmask |= FONT_POINT_SIZE;
	    efnt->font_info.resolution_x = args[1].i;
	    font_vmask |= FONT_RESOLUTION_X;
	    efnt->font_info.resolution_y = args[2].i;
	    font_vmask |= FONT_RESOLUTION_Y;
	    if (nargs > 3) {
		switch(args[3].i) {
		case 1: srcPixelType = EPIXEL_TYPE_A1; break;
		case 2: srcPixelType = EPIXEL_TYPE_A2; break;
		case 4: srcPixelType = EPIXEL_TYPE_A4; break;
		case 8: srcPixelType = EPIXEL_TYPE_A8; break;
		default:
		    fprintf(stderr, "%s:%d: bad SIZE\n", file, line);
		    nerror++;
		}
		bitsPerPixel = EPIXEL_BIT_SIZE(srcPixelType);
	    }
	    break;
	case BDF_POINT_SIZE:
	    efnt->font_info.point_size = args[0].i;
	    font_vmask |= FONT_POINT_SIZE;
	    break;
	case BDF_BITS_PER_PIXEL:
	    switch(args[0].i) {
	    case 1: srcPixelType = EPIXEL_TYPE_A1; break;
	    case 2: srcPixelType = EPIXEL_TYPE_A2; break;
	    case 4: srcPixelType = EPIXEL_TYPE_A4; break;
	    case 8: srcPixelType = EPIXEL_TYPE_A8; break;
	    default:
		fprintf(stderr, "%s:%d: bad BITS_PER_PIXEL\n", file, line);
		nerror++;
	    }
	    bitsPerPixel = EPIXEL_BIT_SIZE(srcPixelType);
	    break;
	case BDF_RESOLUTION_X:
	    efnt->font_info.resolution_x = args[0].i;
	    font_vmask |= FONT_RESOLUTION_X;
	    break;
	case BDF_RESOLUTION_Y:
	    efnt->font_info.resolution_y = args[0].i;
	    font_vmask |= FONT_RESOLUTION_Y;
	    break;
	case BDF_FONT_DESCENT:
	    efnt->font_info.descent = args[0].i;
	    font_vmask |= FONT_DESCENT;
	    break;
	case BDF_FONT_ASCENT:
	    efnt->font_info.ascent = args[0].i;
	    font_vmask |= FONT_ASCENT;
	    break;
	case BDF_DEFAULT_CHAR:
	    efnt->encoding_default = args[0].i;
	    break;

	case BDF_STARTCHAR:
	    memset(&glyph, 0, sizeof(EGlyph));
	    in_glyph = 1;
	    curr_glyph = -1;
	    glyph.name_offset = add_string(string_table, args[0].s);
	    while(in_glyph) {
		nargs = 10;
		if ((opt = bdf_parse(f, file, &line, args, &nargs)) < 0) {
		    fprintf(stderr, "%s:%d: error\n", file, line);
		    return -1;
		}

		switch(bdf_opts[opt].val) {
		case BDF_NONE:
		    fprintf(stderr, "%s:%d: premature end of file\n", 
			    file, line);
		    return -1;
		default:
		    break;
		case BDF_ENCODING:
		    curr_glyph = args[0].i;
		    break;
		case BDF_SWIDTH:
		    break;
		case BDF_DWIDTH:
		    glyph.dwx = args[0].i;
		    glyph.dwy = args[1].i;
		    glyph_vmask |= (GLYPH_DWX|GLYPH_DWY);
		    break;
		case BDF_BBX:
		    glyph.width = args[0].i;
		    glyph.height = args[1].i;
		    glyph.xoffs = args[2].i;
		    glyph.yoffs = args[3].i;
		    glyph_vmask |= (GLYPH_WIDTH|GLYPH_HEIGHT|
				    GLYPH_XOFFS|GLYPH_YOFFS);
		    break;
		case BDF_ATTRIBUTES:
		    break;
		case BDF_BITMAP: {
		    int y = 0;
		    int lshift  = (8 - bitsPerPixel);
		    int mask   = (1 << bitsPerPixel)-1;
		    int pixelsPerByte = (8 / bitsPerPixel);
		    EPixel_t p;

		    p.r = p.g = p.b = 255;
		    printf("GLYPH=%d, width=%d, height=%d\n", 
			   curr_glyph, glyph.width, glyph.height);
		    printf("bitPerPixel=%d, lshift=%d, mask=%d\n",
			   bitsPerPixel, lshift, mask);

		    gpix = EPixmapCreate(glyph.width, glyph.height, EPIXEL_TYPE_A8);
		    while(bdf_token(f, &line, 1, buf, sizeof(buf)) != NULL) {
			int x = 0;
			unsigned char* ptr = (unsigned char*) buf;

			if (streq(buf, "ENDCHAR"))
			    break;
			while((x < glyph.width) && *ptr) {
			    unsigned char bits;
			    unsigned char alpha;
			    int i;

			    bits = hexbyte(ptr[0],ptr[1]);
			    ptr += 2;
			    printf("-%02X-", bits);

			    for (i = 0; i < pixelsPerByte; i++) {
				alpha = bits >> lshift;  // get pixel in low
				bits <<= bitsPerPixel;   // advance to next
				// scale in range [0-255]
				p.a = alpha * (255 / mask);
				// plot
				if (p.a == 0)
				    printf(" ");
				else if (p.a == 255)
				    printf("#");
				else
				    printf(".");
				EPixmapPutPixel(gpix, x, y, EFLAG_NONE, p);
				x++;
			    }
			}
			printf("\n");
			y++;
		    }
		}
		    /* Fall through */
		case BDF_ENDCHAR:
		    if (add_glyph(glyph_table, curr_glyph, glyph_vmask,
				  efnt->font_info.pixel_type,
				  &glyph, gpix) < 0) {
			fprintf(stderr, "%s:%d: could not add glyph %d\n",
				file, line, curr_glyph);
			nerror++;
		    }
		    memset(&glyph, 0, sizeof(EGlyph));
		    curr_glyph = -1;
		    glyph_vmask = 0;
		    gpix = NULL;			    
		    in_glyph = 0;
		    break;

		}
	    }
	    break;
	}
    }
    return 0;
}

/* FREETYPE 2 Font rendering to efnt */

#ifdef HAVE_FREETYPE_2

int load_ft(char* fontname, int fontsize, int xres, int yres,
	    EFontFile* efnt, StringTable* string_table, GlyphTable* glyph_table)
{
    FT_Library  library;
    FT_Face     face;
    FT_GlyphSlot slot;
    FT_UInt      gindex;
    FT_ULong     charcode;
    int          error;
    EGlyph       glyph;
    EPixmap*     gpix;
    EPixel_t     p;
    int glyph_vmask;
    int font_vmask = 0;
    int nerror = 0;
    int found;
    unsigned char* gptr;
    int i;
    int x;
    int y;
    int iso_latin_only = 1;  // FIXME (FreeType is crashing)

    error = FT_Init_FreeType(&library);
    if (error) {
	fprintf(stderr, "could not initialized free type library (%d)\n",error);
	return -1;
    }

    error = FT_New_Face(library, fontname, 0, &face);
    if (error) {
	if (error == FT_Err_Unknown_File_Format)
	    fprintf(stderr, "unsupported font format\n");
	else 
	    fprintf(stderr, "could not open fontfile\n");
	return -1;
    }
    slot = face->glyph;
    // Set char hight to 16pt in 100x100dpi
    // FIXME: retrieve the correct display size!!!
    FT_Set_Char_Size(face, 0, fontsize*64, xres, yres);
    
    // FT_Set_Pixel_Sizes(face, 0, fontsize);

    printf("style_name: %s\n", face->style_name);

    efnt->font_info.weight = EFONT_WEIGHT_MEDIUM;

    efnt->font_info.slant  = kv_lookup(kv_slant, face->style_name, &found);
    if (!found) {
	fprintf(stderr, "could not map style_name '%s'\n", 
		face->style_name);
	efnt->font_info.slant = EFONT_SLANT_ROMAN;
    }
    efnt->font_info.spacing = EFONT_SPACING_PROPORTIONAL;

    efnt->font_info.point_size = fontsize*10;
    font_vmask |= FONT_POINT_SIZE;

    efnt->font_info.pixel_size = face->size->metrics.y_ppem;
    font_vmask |= FONT_PIXEL_SIZE;

    efnt->font_info.descent = -face->size->metrics.descender / 64;
    font_vmask |= FONT_DESCENT;
    
    efnt->font_info.ascent = face->size->metrics.ascender / 64;
    font_vmask |= FONT_ASCENT;

    efnt->font_info.resolution_x = xres;
    font_vmask |= FONT_RESOLUTION_X;

    efnt->font_info.resolution_y = yres;
    font_vmask |= FONT_RESOLUTION_Y;

    efnt->encoding_default = 0;

    printf("Font PointSize=%.2f, PixelSize=%d at resolution=%dx%d\n",
	   efnt->font_info.point_size/10.0,
	   efnt->font_info.pixel_size,
	   efnt->font_info.resolution_x,
	   efnt->font_info.resolution_y);

    memset(&glyph, 0, sizeof(EGlyph));
    gpix = NULL;
    glyph_vmask = 0;
    efnt->font_info.pixel_type = EPIXEL_TYPE_A8;
    nerror = 0;

    printf("family_name: %s\n", face->family_name);
    efnt->family_offset = add_string(string_table, face->family_name);



    charcode = FT_Get_First_Char(face, &gindex);
    while(gindex != 0) {
	char gname[1024];

	if (iso_latin_only && (charcode > 255))
	    goto next;

	error = FT_Load_Glyph(face, gindex, FT_LOAD_NO_BITMAP);
	if (error) {
	    fprintf(stderr, "unable to load char %d, glyph %d\n",i,gindex);
	    goto next;
	}
	// This will render an antialised char
	error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
	if (error) {
	    fprintf(stderr, "unable to render char %d, glyph %d\n",i,gindex);
	    goto next;
	}
	error = FT_Get_Glyph_Name(face, gindex, gname, sizeof(gname));
	if (error) {
	    fprintf(stderr, "unabled to get glyph name for glyph %d\n",gindex);
	    glyph.name_offset = 0;
	    gname[0]='\0';
	}
	else {
	    glyph.name_offset = add_string(string_table, gname);
	    glyph_vmask |= GLYPH_NAME;
	}

	// Now we have the bitmap
	glyph.width  = slot->metrics.width  / 64;
	glyph.height = slot->metrics.height / 64;
	glyph.xoffs  = slot->bitmap_left;
	glyph.yoffs  = -(slot->bitmap.rows - slot->bitmap_top);
	// glyph.yoffs  = slot->metrics.height / 64 - slot->bitmap_top;
	glyph.dwx    = slot->advance.x / 64;
	glyph.dwy    = slot->advance.y / 64;

	glyph_vmask |= (GLYPH_WIDTH|GLYPH_HEIGHT|
			GLYPH_XOFFS|GLYPH_YOFFS|
			GLYPH_DWX|GLYPH_DWY);

	gpix = EPixmapCreate(slot->bitmap.width, slot->bitmap.rows, EPIXEL_TYPE_A8);
	gptr = slot->bitmap.buffer;
	p.r = p.g = p.b = 255;

	if (slot->bitmap.num_grays != 256) {
	    fprintf(stderr, "num_grays != 256 not supported\n");
	    exit(1);
	}

	for (y = 0; y < slot->bitmap.rows; y++) {
	    unsigned char* gptr1 = gptr;
	    for (x = 0; x < slot->bitmap.width; x++) {
		p.a = *gptr1++;
		EPixmapPutPixel(gpix, x, y, EFLAG_NONE, p);
	    }
	    gptr += slot->bitmap.pitch;
	}
	
	if (add_glyph(glyph_table, charcode, glyph_vmask,
		      efnt->font_info.pixel_type,
		      &glyph, gpix) < 0) {
	    fprintf(stderr, "could not add glyph %s[%d]\n", gname,gindex);
	    nerror++;
	}
	memset(&glyph, 0, sizeof(EGlyph));
	glyph_vmask = 0;
	gpix = NULL;

	DBGFMT("%ld: '%s' glyph_index=%d\n", charcode, gname, gindex);
    next:
	charcode = FT_Get_Next_Char(face, charcode, &gindex);
    }
    return font_vmask;
}

#endif


int nint(double x)
{
    int i;
    i = x;
    return i;
}

unsigned long int fontprop(XFontStruct *font,Atom name,char *desc,unsigned long int def)
{
    int i;

    for (i=font->n_properties-1;i>=0;i--) {
	if (font->properties[i].name == name) {
	    return(font->properties[i].card32);
	}
    }
    fprintf(stderr,"no %s property found on assuming %lu\n",
	    desc,def);
    return def;
}

void print_bdf_string(char *s)
{
    putchar('"');
    for (;*s;s++) {
	if (*s == '"') putchar('"');
	putchar(*s);
    }
    putchar('"');
}

static struct {
    char *name;
    Atom format;
    Atom atom;
} proptbl[] = 
{
    { "ADD_STYLE_NAME", XA_ATOM },
    { "CHARSET_COLLECTIONS", XA_ATOM },
    { "CHARSET_ENCODING", XA_ATOM },
    { "CHARSET_REGISTRY", XA_ATOM },
    { "COPYRIGHT", XA_ATOM },
    { "DEVICE_FONT_NAME", XA_ATOM },
    { "FACE_NAME", XA_ATOM },
    { "FAMILY_NAME", XA_ATOM },
    { "FONT", XA_ATOM },
    { "FONTNAME_REGISTRY", XA_ATOM },
    { "FOUNDRY", XA_ATOM },
    { "FULL_NAME", XA_ATOM },
    { "NOTICE", XA_ATOM },
    { "SETWIDTH_NAME", XA_ATOM },
    { "SLANT", XA_ATOM },
    { "SPACING", XA_ATOM },
    { "WEIGHT_NAME", XA_ATOM },
    { "AVERAGE_WIDTH", XA_CARDINAL },
    { "CAP_HEIGHT", XA_CARDINAL },
    { "DESTINATION", XA_CARDINAL },
    { "END_SPACE", XA_CARDINAL },
    { "MAX_SPACE", XA_CARDINAL },
    { "MIN_SPACE", XA_CARDINAL },
    { "NORM_SPACE", XA_CARDINAL },
    { "PIXEL_SIZE", XA_CARDINAL },
    { "POINT_SIZE", XA_CARDINAL },
    { "RELATIVE_SETWIDTH", XA_CARDINAL },
    { "RELATIVE_WEIGHT", XA_CARDINAL },
    { "RESOLUTION", XA_CARDINAL },
    { "RESOLUTION_X", XA_CARDINAL },
    { "RESOLUTION_Y", XA_CARDINAL },
    { "SMALL_CAP_SIZE", XA_CARDINAL },
    { "SUBSCRIPT_SIZE", XA_CARDINAL },
    { "SUPERSCRIPT_SIZE", XA_CARDINAL },
    { "UNDERLINE_THICKNESS", XA_CARDINAL },
    { "WEIGHT", XA_CARDINAL },
    { "X_HEIGHT", XA_CARDINAL },
    { "AVG_CAPITAL_WIDTH", XA_INTEGER },
    { "AVG_LOWERCASE_WIDTH", XA_INTEGER },
    { "FIGURE_WIDTH", XA_INTEGER },
    { "ITALIC_ANGLE", XA_INTEGER },
    { "QUAD_WIDTH", XA_INTEGER },
    { "STRIKEOUT_ASCENT", XA_INTEGER },
    { "STRIKEOUT_DESCENT", XA_INTEGER },
    { "SUBSCRIPT_X", XA_INTEGER },
    { "SUBSCRIPT_Y", XA_INTEGER },
    { "SUPERSCRIPT_X", XA_INTEGER },
    { "SUPERSCRIPT_Y", XA_INTEGER },
    { "UNDERLINE_POSITION", XA_INTEGER },
    { 0 } 
};


int load_xfs(char* fontname, char* displayname,
	     EFontFile* efnt, StringTable* string_table, 
	     GlyphTable* glyph_table)
{
    Display *disp;
    int scrno;
    Screen *scr;
    Window rootwin;
    XFontStruct *font;
    int i;
    int j;
    int b1;
    int b2;
    int b;
    int ptsize;
    int xdpi;
    int ydpi;
    int pmw;
    int pmh;
    XImage *img;
    Pixmap buf;
    GC cleargc;
    GC drawgc;
    int nchars;
    EGlyph glyph;
    EPixmap* gpix;
    int glyph_vmask;
    int font_vmask;
    int found;
    int nerror;
    int srcPixelType = EPIXEL_TYPE_A1;
    int bitsPerPixel = EPIXEL_BIT_SIZE(srcPixelType);
    Atom xa_POINT_SIZE;
    Atom xa_RESOLUTION_X;
    Atom xa_RESOLUTION_Y;
    Atom xa_FOUNDRY;
    Atom xa_FAMILY_NAME;
    Atom xa_WEIGHT_NAME;
    Atom xa_SLANT;
    Atom xa_SETWIDTH_NAME;
    Atom xa_SPACING;
    Atom xa_PIXEL_SIZE;
    Atom xa_DESCENT;
    Atom xa_ASCENT;
    Atom xa_BITS_PER_PIXEL;

    if ((disp = XOpenDisplay(displayname)) == NULL) {
	fprintf(stderr,"can't open display %s\n",XDisplayName(displayname));
	return -1;
    }
    scr = XDefaultScreenOfDisplay(disp);
    scrno = XScreenNumberOfScreen(scr);
    rootwin = XRootWindowOfScreen(scr);

    if ((font = XLoadQueryFont(disp,fontname)) == NULL) {
	fprintf(stderr,"can't load font %s\n",fontname);
	return -1;
    }

    xa_POINT_SIZE     = XInternAtom(disp,"POINT_SIZE",0);
    xa_RESOLUTION_X   = XInternAtom(disp,"RESOLUTION_X",0);
    xa_RESOLUTION_Y   = XInternAtom(disp,"RESOLUTION_Y",0);
    xa_FOUNDRY        = XInternAtom(disp,"FOUNDRY",0);
    xa_FAMILY_NAME    = XInternAtom(disp,"FAMILY_NAME",0);
    xa_WEIGHT_NAME    = XInternAtom(disp,"WEIGHT_NAME",0);
    xa_SLANT          = XInternAtom(disp,"SLANT",0);
    xa_SETWIDTH_NAME  = XInternAtom(disp,"SETWIDTH_NAME",0);
    xa_SPACING        = XInternAtom(disp,"SPACING",0);
    xa_PIXEL_SIZE     = XInternAtom(disp,"PIXEL_SIZE",0);
    xa_DESCENT        = XInternAtom(disp,"DESCENT",0);
    xa_ASCENT         = XInternAtom(disp,"ASCENT",0);
    xa_BITS_PER_PIXEL = XInternAtom(disp,"BITS_PER_PIXEL",0);

    for (j=0;proptbl[j].name;j++)
	proptbl[j].atom = XInternAtom(disp,proptbl[j].name,0);


    memset(&glyph, 0, sizeof(EGlyph));
    gpix = NULL;
    glyph_vmask = 0;
    font_vmask = 0;
    efnt->font_info.pixel_type = EPIXEL_TYPE_A8;
    nerror = 0;

    DBGFMT("STARTFONT 2.1\n");
    DBGFMT("COMMENT Generated from X font `%s' by efnttool\n",fontname);
    DBGFMT("FONT \"%s\"\n",fontname);
    ptsize = nint(fontprop(font,xa_POINT_SIZE,"point size",(unsigned long int)100)/10.0);
    xdpi = fontprop(font,xa_RESOLUTION_X,"X resolution",(unsigned long int)nint(XDisplayWidth(disp,scrno)*25.4/XDisplayWidthMM(disp,scrno)));
    ydpi = fontprop(font,xa_RESOLUTION_Y,"Y resolution",(unsigned long int)nint(XDisplayHeight(disp,scrno)*25.4/XDisplayHeightMM(disp,scrno)));
    DBGFMT("SIZE %d %d %d\n",ptsize,xdpi,ydpi);
    DBGFMT("FONTBOUNDINGBOX %d %d %d %d\n",
	   font->max_bounds.rbearing - font->min_bounds.lbearing,
	   font->max_bounds.ascent + font->max_bounds.descent,
	   font->min_bounds.lbearing,
	   -font->max_bounds.descent );
    DBGFMT("STARTPROPERTIES %d\n",font->n_properties+3);

    DBGFMT("FONT_ASCENT %d\nFONT_DESCENT %d\nDEFAULT_CHAR %u\n",
	   font->ascent,font->descent,font->default_char);

    efnt->font_info.descent = font->descent;
    font_vmask |= FONT_DESCENT;
    
    efnt->font_info.ascent = font->ascent;
    font_vmask |= FONT_ASCENT;

    efnt->encoding_default = font->default_char;


    for (i=0;i<font->n_properties;i++) {
	char *s;
	char *svalue;
	unsigned long uvalue;
	long int      ivalue;
	Atom format;
	Atom prop = font->properties[i].name;

	s = XGetAtomName(disp,prop);
	format = XA_CARDINAL;
	for (j=0;proptbl[j].name;j++) {
	    if (prop == proptbl[j].atom) {
		format = proptbl[j].format;
		break;
	    }
	}
	switch (format) {
	case XA_ATOM:
	    svalue = XGetAtomName(disp,(Atom)font->properties[i].card32);
	    DBGFMT("%s ",s);
	    if (debug)
		print_bdf_string(svalue);
	    DBGFMT("\n");
	    break;
	case XA_CARDINAL:
	    uvalue = font->properties[i].card32;
	    DBGFMT("%s %lu\n",s, uvalue);
	    break;
	case XA_INTEGER:
	    ivalue = (long int)font->properties[i].card32;
	    DBGFMT("%s %ld\n",s, ivalue);
	    break;
	}
	if (prop == xa_FOUNDRY) {
	    efnt->foundry_offset = add_string(string_table, svalue);
	}
	else if (prop == xa_FAMILY_NAME) {
	    efnt->family_offset = add_string(string_table, svalue);
	}
	else if (prop == xa_WEIGHT_NAME) {
	    efnt->font_info.weight = kv_lookup(kv_weight, svalue, &found);
	    if (!found) {
		fprintf(stderr, "unknown WEIGHT_NAME '%s'\n", svalue);
		nerror++;
	    }
	}
	else if (prop == xa_SLANT) {
	    efnt->font_info.slant = kv_lookup(kv_slant, svalue, &found);
	    if (!found) {
		fprintf(stderr, "unknown SLANT '%s'\n", svalue);
		nerror++;
	    }
	}
	else if (prop == xa_SETWIDTH_NAME) {
	    efnt->font_info.width = kv_lookup(kv_width, svalue, &found);
	    if (!found) {
		fprintf(stderr, "unknown SETWIDTH_NAME '%s'\n", svalue);
		nerror++;
	    }	
	}
	else if (prop == xa_SPACING) {
	    kv_lookup(kv_spacing, svalue, &found);
	    if (!found) {
		fprintf(stderr, "unknown SPACING '%s'\n", svalue);
		nerror++;
	    }
	}
	else if (prop == xa_PIXEL_SIZE) {
	    efnt->font_info.pixel_size = uvalue;
	    font_vmask |= FONT_PIXEL_SIZE;
	}
	else if (prop == xa_POINT_SIZE) {
	    efnt->font_info.point_size = uvalue;
	    font_vmask |= FONT_POINT_SIZE;
	}
	else if (prop == xa_BITS_PER_PIXEL) {
	    switch(uvalue) {
	    case 1: srcPixelType = EPIXEL_TYPE_A1; break;
	    case 2: srcPixelType = EPIXEL_TYPE_A2; break;
	    case 4: srcPixelType = EPIXEL_TYPE_A4; break;
	    case 8: srcPixelType = EPIXEL_TYPE_A8; break;
	    default:
		fprintf(stderr, "bad BITS_PER_PIXEL\n");
		nerror++;
	    }
	    bitsPerPixel = EPIXEL_BIT_SIZE(srcPixelType);
	}
	else if (prop == xa_RESOLUTION_X) {
	    efnt->font_info.resolution_x = uvalue;
	    font_vmask |= FONT_RESOLUTION_X;
	}
	else if (prop == xa_RESOLUTION_Y) {
	    efnt->font_info.resolution_y = uvalue;
	    font_vmask |= FONT_RESOLUTION_Y;
	}
    }
    DBGFMT("ENDPROPERTIES\n");
    if (font->per_char) { 
	nchars = 0;
	for (b1=font->min_byte1;b1<=font->max_byte1;b1++) {
	    b = (b1-font->min_byte1) * (font->max_char_or_byte2+1-font->min_char_or_byte2);
	    for (b2=font->min_char_or_byte2;b2<=font->max_char_or_byte2;b2++) {
		XCharStruct *chr;
		chr = font->per_char + b;
		if (chr->lbearing || chr->rbearing || chr->ascent || chr->descent || chr->width) nchars ++;
		b ++;
	    }
	}
    }
    else {
	nchars = (font->max_byte1+1-font->min_byte1) * 
	    (font->max_char_or_byte2+1-font->min_char_or_byte2);
    }
    DBGFMT("CHARS %d\n",nchars);
    pmw = ((font->max_bounds.rbearing - font->min_bounds.lbearing) + 7) & ~7;
    pmh = font->max_bounds.ascent + font->max_bounds.descent;
    buf = XCreatePixmap(disp,rootwin,pmw,pmh,1);
    cleargc = XCreateGC(disp,(Drawable)buf,0L,(XGCValues *)0);
    drawgc = XCreateGC(disp,(Drawable)buf,0L,(XGCValues *)0);
    XSetForeground(disp,cleargc,0L);
    XSetForeground(disp,drawgc,1L);
    XSetBackground(disp,drawgc,0L);
    XSetFont(disp,drawgc,font->fid);
    for (b1=font->min_byte1;b1<=font->max_byte1;b1++) {
	b = (b1-font->min_byte1) * 
	    (font->max_char_or_byte2+1-font->min_char_or_byte2);
	for (b2=font->min_char_or_byte2;b2<=font->max_char_or_byte2;b2++) {
	    XCharStruct *chr;
	    XTextItem16 ti;
	    XChar2b c2b;
	    EPixel_t p;
	    int cw;
	    int ch;
	    int x;
	    int y;
	    int h;
	    int curr_glyph;

	    p.r = p.g = p.b = 255;
	    chr = font->per_char ? (font->per_char + b) : &font->min_bounds;
	    if (chr->lbearing || 
		chr->rbearing || 
		chr->ascent ||
		chr->descent || 
		chr->width) { 
		
		cw = chr->rbearing - chr->lbearing;
		ch = chr->ascent + chr->descent;

		curr_glyph = (b1<<8)+b2;
		// FIXME: get the glyph name!
		DBGFMT("STARTCHAR %d\n",b);
		DBGFMT("ENCODING %d\n", curr_glyph);
		DBGFMT("SWIDTH %d 0\n",nint((chr->width*72000.0)/(ptsize*xdpi)));
		DBGFMT("DWIDTH %d 0\n",chr->width);
		DBGFMT("BBX %d %d %d %d\n",cw,ch,chr->lbearing,-chr->descent);
		DBGFMT("BITMAP\n");

		glyph.width = cw;
		glyph_vmask |= GLYPH_WIDTH;
		glyph.height = ch;
		glyph_vmask |= GLYPH_HEIGHT;
		glyph.xoffs = chr->lbearing;
		glyph_vmask |= GLYPH_XOFFS;
		glyph.yoffs = -chr->descent;
		glyph_vmask |= GLYPH_YOFFS;

		glyph.dwx = chr->width;
		glyph.dwy = 0;
		glyph_vmask |= (GLYPH_DWX|GLYPH_DWY);

		gpix = EPixmapCreate(glyph.width, glyph.height, EPIXEL_TYPE_A8);

		XFillRectangle(disp,(Drawable)buf,cleargc,0,0,pmw,pmh);
		c2b.byte1 = (b1 == 0) ? (b2 >> 8) : b1;
		c2b.byte2 = b2;
		ti.chars = &c2b;
		ti.nchars = 1;
		ti.delta = 0;
		ti.font = None;
		XDrawText16(disp,(Drawable)buf,drawgc,-chr->lbearing,chr->ascent,&ti,1);
		img = XGetImage(disp,(Drawable)buf,0,0,pmw,pmh,1L,XYPixmap);
		for (y=0;y<ch;y++) {
		    for (x=0;x<cw;) {
			h = 0;
			for (i=0;i<8;i++) {
			    h <<= 1;
			    if (XGetPixel(img,x,y)) {
				p.a = 255;
				h |= 1;
			    }
			    else
				p.a = 0;
			    EPixmapPutPixel(gpix, x, y, EFLAG_NONE, p);
			    x++;
			}
			DBGFMT("%02x",h);
		    }
		   DBGFMT("\n");
		}
		DBGFMT("ENDCHAR\n");
		XDestroyImage(img);
		if (add_glyph(glyph_table, curr_glyph, glyph_vmask,
			      efnt->font_info.pixel_type,
			      &glyph, gpix) < 0) {
		    fprintf(stderr, "could not add glyph %d\n", curr_glyph);
		    nerror++;
		}
		memset(&glyph, 0, sizeof(EGlyph));
		glyph_vmask = 0;
		gpix = NULL;
	    }
	    b++;
	}
    }
    DBGFMT("ENDFONT\n");
    return 0;
}


/* calculate padding size (e.g to reach alignment) */
#define ALIGN_SIZE(n,a) ((a - ((n) % (a))) % (a))

int write_efnt(char* file, EFontFile* efnt, int vmask,
	       StringTable* string_table,
	       GlyphTable*  glyph_table)
{
    FILE* f = NULL;
    int i = 0;
    int start = -1;
    int stop  = -1;
    int max_glyph_width = 0;
    int min_glyph_width = 0x10000;
    int max_glyph_height = 0;
    int min_glyph_height = 0x10000;
    size_t glyph_table_size = 0;
    size_t table_sz;
    u_int32_t offset;
    int n;
    char pad[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

    if (file == NULL)
	f = stdout;
    else if ((f = fopen(file, "w")) == NULL) {
	fprintf(stderr, "efnttool: could not open %s for writing : %s\n",
		file, strerror(errno));
	return -1;
    }

    /* scan trough the glyph_table and calculate offset start & stop 
     * also calculate max/min width/height
     */
    i = 0;
    while(i < glyph_table->size) {
	EGlyph* g = glyph_table->glyph_data[i].glyph;
	size_t sz = glyph_table->glyph_data[i].size;
	
	glyph_table_size += sz;
	if (sz) {
	    stop = i;
	    if (start < 0)
		start = i;
	    if (g->width > max_glyph_width) max_glyph_width=g->width;
	    if (g->width < min_glyph_width) min_glyph_width=g->width;
	    if (g->height > max_glyph_height) max_glyph_height=g->height;
	    if (g->height < min_glyph_height) min_glyph_height=g->height;
	}
	i++;
    }

    if (start < 0) {
	fprintf(stderr, "efnttool: could not find any glyphs\n");
	goto error;
    }

    if (!(vmask & FONT_RESOLUTION_X)) efnt->font_info.resolution_x = 75;
    if (!(vmask & FONT_RESOLUTION_Y)) efnt->font_info.resolution_y = 75;
    if (!(vmask & FONT_DESCENT)) efnt->font_info.descent = 0;
    if (!(vmask & FONT_ASCENT))  efnt->font_info.descent = max_glyph_height;
    

    efnt->encoding_start = start;
    efnt->encoding_stop  = stop;
    efnt->encoding_default = '?';

    DBGFMT("#glyphs : %lu\n", glyph_table->n);
    DBGFMT("glyph start=%d\n", start);
    DBGFMT("glyph stop=%d\n", stop);

    table_sz = string_table->size;
    efnt->string_table_start  = 0;
    efnt->string_table_length = table_sz + ALIGN_SIZE(table_sz, 16);

    table_sz = ((stop - start) + 1)*sizeof(u_int32_t);
    efnt->offset_table_start  = efnt->string_table_start + 
	efnt->string_table_length;
    efnt->offset_table_length = table_sz + ALIGN_SIZE(table_sz, 16);

    efnt->glyph_table_start  = 
	efnt->offset_table_start + efnt->offset_table_length;
    efnt->glyph_table_length  = glyph_table_size;

    /* Write EFNT header and FontInfo */
    fwrite(efnt, sizeof(EFontFile), 1, f);

    /* Write string table */
    fwrite(string_table->data, sizeof(char), string_table->size, f);
    // Pad to 16-byte alignment */
    n = efnt->string_table_length - string_table->size;
    fwrite(pad, sizeof(char), n, f);

    /* Write offset table */
    offset = efnt->glyph_table_start;
    for (i = start; i <= stop; i++) {
	size_t sz = glyph_table->glyph_data[i].size;
	if (sz) {
	    DBGFMT("glyph: %d at offset = %u", i, offset);
	    fwrite(&offset, sizeof(u_int32_t), 1, f);
	    offset += sz;
	}
	else {
	    u_int32_t offset0 = 0;
	    DBGFMT("glyph: %d at offset = %u", i, offset0);
	    fwrite(&offset0, sizeof(u_int32_t), 1, f);
	}
    }
    table_sz = ((stop - start) + 1)*sizeof(u_int32_t);
    n = efnt->offset_table_length - table_sz;
    fwrite(pad, sizeof(char), n, f);

    /* write glyph table */
    for (i = start; i <= stop; i++) {
	size_t sz = glyph_table->glyph_data[i].size;
	if (sz)
	    fwrite(glyph_table->glyph_data[i].glyph, sizeof(char), sz, f);
    }
    if (f && (f != stdout))
	fclose(f);
    return 0;
error:
    if (f && (f != stdout))
	fclose(f);
    return -1;
}

// Find the current screen resolution (dpi)
int x11_resolution(Display *dpy, int* x_res, int* y_res)
{
    int scr;
    int pixelWidth;
    int pixelHeight;
    int mmWidth;
    int mmHeight;
    extern double round(double); // math.h Doesn't get it for some reason.
    scr = XDefaultScreen(dpy);

    pixelWidth = DisplayWidth(dpy, scr);
    pixelHeight = DisplayHeight(dpy, scr);
    mmWidth = DisplayWidthMM(dpy, scr);
    mmHeight = DisplayHeightMM(dpy, scr);
    printf("Screen size %.2f x %.2f cm\n", mmWidth/10.0, mmHeight/10.0);
    printf("Screen dim  %dx%d\n", pixelWidth, pixelHeight);
    *x_res = round(pixelWidth / (mmWidth*MM_INCH));
    *y_res = round(pixelHeight / (mmHeight*MM_INCH));
    printf("x_res = %d\n", *x_res);
    printf("y_res = %d\n", *y_res);
    return 0;
}

/*
 *  usage: efnttool options
 *
 * options:
 *       -v                  verbose output
 *       -edf <file>         input file in EDF format (PNG image glyphs)
 *       -bdf <file>         input file is in BDF format
 *       -xfs <name> <size>  rip X font directly 
 *       -ft  <name> <size>  rip font with FreeType2 directly
 *       -o <file>           output file name
 *       -autores            try to determine the screen resolution
 *       -xres <dpi>         target screen resolution in dpi
 *       -yres <dpi>         target screen resolution in dpi
 *       -d <display>        display name (default ":0")
 */
int main(int argc, char** argv)
{
    int i = 1;
    char* outfile = NULL;
    char* filename = NULL;
    char* fontname = NULL;
    int   is_edf = 0;
    int   is_bdf = 0;
    int   is_xfs = 0;
    int   is_ft  = 0;
    int   fontsize = 0;
    int font_vmask;
    int   xres = 0;
    int   yres = 0;
    int   do_screenres = 0;
    EFontFile efnt;
    char *displayname = ":0";
    StringTable* string_table = new_string_table();
    GlyphTable* glyph_table = new_glyph_table();

    debug = 0;
    // setup efnt default values.
    memset(&efnt, 0, sizeof(EFontFile));
    memcpy(efnt.magic, "EFNT", 4);
    efnt.font_info.pixel_type = EPIXEL_TYPE_BGRA;

    /* add a dummy string, this make sure that 
     * offsets do not start at 0
     * any zero offset will lead to the string "?"
     */
    add_string(string_table, "?");

    while(i < argc) {
	if (strcmp(argv[i], "-v") == 0) {
	    debug = 1;
	    i++;
	}
	else if (strcmp(argv[i], "-d") == 0) {
	    displayname = argv[i+1];
	    i += 2;
	}
	else if (strcmp(argv[i], "-xres") == 0) {
	    xres = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-yres") == 0) {
	    yres = atoi(argv[i+1]);
	    i += 2;
	}
	else if (strcmp(argv[i], "-autores") == 0) {
	    do_screenres = 1;
	    i += 1;
	}
	else if (strcmp(argv[i], "-edf") == 0) {
	    filename = argv[i+1];
	    is_edf = 1;
	    i += 2;
	}
	else if (strcmp(argv[i], "-bdf") == 0) {
	    filename = argv[i+1];
	    is_bdf = 1;
	    i += 2;
	}
	else if (strcmp(argv[i], "-xfs") == 0) {
	    is_xfs = 1;
	    fontname = argv[i+1];
	    i += 2;
	}
	else if (strcmp(argv[i], "-ft") == 0) {
#ifndef HAVE_FREETYPE_2
	    fprintf(stderr, "FreeType2 library not supported\n");
	    exit(1);
#endif
	    is_ft = 1;
	    if (!argv[i+1] || !argv[i+2]) {
		fprintf(stderr, "-ft expect font and size arguments\n");
		exit(1);
	    }
	    fontname = argv[i+1];
	    fontsize = atoi(argv[i+2]);
	    i += 3;
	}
	else if (strcmp(argv[i], "-o") == 0) {
	    outfile = argv[i+1];
	    i += 2;
	}
	else
	    break;
    }

    if ((xres ==  0) && (yres == 0)) {
	xres = DEFAULT_XRES;
	yres = DEFAULT_YRES;
	if (do_screenres) {
	    Display* disp;
	    if ((disp = XOpenDisplay(displayname)) == NULL)
		fprintf(stderr,"can't open display %s\n",
			XDisplayName(displayname));
	    else {
		if (x11_resolution(disp, &xres, &yres) < 0)
		    fprintf(stderr, "can't determine screen resolution\n");
		XCloseDisplay(disp);
	    }
	}
    }
    else if (xres == 0) xres = yres;
    else if (yres == 0) yres = xres;

    if ((argv[i] != NULL) && !is_bdf && !is_edf && !is_xfs && !is_ft) {
	int len = strlen(argv[i]);
	filename = argv[i];
	if (streq(filename+len-4, ".edf")) 
	    is_edf = 1;
	else if (streq(filename+len-4, ".bdf"))
	    is_bdf = 1;
	else
	    usage();
	if (outfile == NULL) {
	    outfile = malloc(len + 2);
	    strcpy(outfile, filename);
	    strcpy(outfile+len-3, "efnt");
	}
    }

    if (is_bdf)
	DBGFMT("processing BDF file %s output to %s", filename, outfile);
    else if (is_edf)
	DBGFMT("processing EDF file %s output to %s", filename, outfile);
    else if (is_xfs)
	DBGFMT("processing X11 font %s output to %s", fontname,outfile);	
    else if (is_ft)
	DBGFMT("processing FT font %s:%d output to %s",fontname,fontsize,outfile);	

    if (is_edf)
	font_vmask=load_edf(filename, &efnt,string_table, glyph_table);
    else if (is_bdf)
	font_vmask=load_bdf(filename, &efnt,string_table, glyph_table);
#ifdef HAVE_FREETYPE_2
    else if (is_ft)
	font_vmask=load_ft(fontname, fontsize, xres, yres, 
			   &efnt,string_table, glyph_table);
#endif
    else if (is_xfs)
	font_vmask=load_xfs(fontname, displayname,
			    &efnt,string_table, glyph_table);
    if (font_vmask < 0)
	exit(1);
    if (write_efnt(outfile, &efnt, font_vmask,
		   string_table, glyph_table) < 0)
	exit(1);
    exit(0);    
}



