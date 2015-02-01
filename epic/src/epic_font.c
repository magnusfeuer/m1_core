/*
 *  EPIC Font functions
 */
#include <sys/mman.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include "epic.h"
#include <wchar.h>
#include <errno.h>

void EFontInit(EFont* font)
{
    EOBJECT_INIT(font, EFONT_TYPE);

    font->file_name = NULL;
    font->file_size = 0;
    font->foundry_name = NULL;
    font->family_name = NULL;
    memset(&font->font_info, 0, sizeof(EFontInfo));
    font->font_file = NULL;
    font->font_data = NULL;
    font->font_map = NULL;
}

EFont* EFontCreate()
{
    EFont* font;

    if ((font = (EFont*) malloc(sizeof(EFont))) == NULL)
	return NULL;
    EFontInit(font);
    font->on_heap = 1;
    font->refc = 1;
    return font;
}

void EFONT_TYPE_RELEASE(void* arg)
{
    EFont* font = (EFont*) arg;
    
    EDBGFMT_MEM("EFONT_TYPE_RELEASE: %p", arg);

    if (font->file_name) free(font->file_name);
    if (font->foundry_name) free(font->foundry_name);
    if (font->family_name) free(font->family_name);
    if (font->font_map != NULL)
	munmap(font->font_map, font->file_size);
    else if (font->font_data != NULL)
	free(font->font_data);
    if (font->on_heap)
	free(font);
}

char* EFontFileString(EFontFile* ff, u_int32_t offset)
{
    char* table = (char*)ff->data + U32LE(ff->string_table_start);
    // get string in string table (skip length indication)
    return table + offset + 1;
}

EGlyph* EFontFileGlyph(EFontFile* ff, u_int32_t encoding)
{
    u_int32_t start = U32LE(ff->encoding_start);
    u_int32_t stop  = U32LE(ff->encoding_stop);
    u_int32_t* offset_table = 
	(u_int32_t*) (((char*)ff->data) + U32LE(ff->offset_table_start));
    int offset;

    if ((encoding < start) || (encoding > stop)) {
	encoding = U32LE(ff->encoding_default);
	if ((encoding < start) || (encoding > stop))
	    return NULL;
    }
    offset = U32LE(offset_table[encoding - start]);
    return (EGlyph*) (((char*)ff->data) + offset);
}

/* Create the font structure and load the font info
 * but leave it unmapped and unloaded
 */
EFont* EFontOpen(char* file)
{
    FILE* f;
    char magic[4];
    EFontFile font_file;
    char* string_table = NULL;
    int string_table_length = 0;
    EFont* font;
    size_t file_size;

    if ((f = fopen(file, "r")) == NULL) {
	fprintf(stderr, "epic_font: file %s, open error: %s\n", 
		file, strerror(errno));
	return NULL;
    }

    if (fread(magic, sizeof(char), 4, f) < 4) {
	fprintf(stderr, "epic_font: file %s, format error\n", file);
	goto error;
    }
    if (memcmp(magic, "EFNT", 4) != 0) {
	fprintf(stderr, "epic_font: file %s, format error\n", file);
	goto error;
    }
    fseek(f, 0L, SEEK_END);
    file_size = ftell(f);
    fseek(f, 0L, SEEK_SET);

    if (file_size < sizeof(EFontFile)) {
	fprintf(stderr, "epic_font: file %s, size error\n", file);
	goto error;
    }

    /* read the FontFile header structure */
    if (fread((void*)&font_file, sizeof(EFontFile), 1, f) < 1) {
	fprintf(stderr, "epic_font: file %s, read error: %s\n", file,
		strerror(errno));
	goto error;
    }
    /* load the string table (ASSUMED to be located after header!) 
     * this must be changed if we get string_table_start != 0
     */
    string_table_length = U32LE(font_file.string_table_length);
    if ((string_table = (char*) malloc(string_table_length)) == NULL) {
	fprintf(stderr, "epic_font: file %s could not load string table : %s\n",
		file, strerror(errno));
	goto error;
    }
		
    if (fread(string_table, sizeof(char), string_table_length, f) < 
	(size_t) string_table_length) {
	fprintf(stderr, "epic_font: file %s could not load string table : %s\n",
		file, strerror(errno));
	goto error;
    }
    

    font = EFontCreate();
    font->file_name = strdup(file);
    font->file_size = file_size;
    font->foundry_name = strdup(string_table+U32LE(font_file.foundry_offset)+1);
    font->family_name = strdup(string_table+U32LE(font_file.family_offset)+1);
    // Copy font info, convert to host endian
    font->font_info.weight = U32LE(font_file.font_info.weight);
    font->font_info.slant  = U32LE(font_file.font_info.slant);
    font->font_info.width  = U32LE(font_file.font_info.width);
    font->font_info.style  = U32LE(font_file.font_info.style);
    font->font_info.spacing = U32LE(font_file.font_info.spacing);
    font->font_info.pixel_type = U32LE(font_file.font_info.pixel_type);
    font->font_info.pixel_size = U32LE(font_file.font_info.pixel_size);
    font->font_info.point_size = U32LE(font_file.font_info.point_size);
    font->font_info.resolution_x = U32LE(font_file.font_info.resolution_x);
    font->font_info.resolution_y = U32LE(font_file.font_info.resolution_y);
    font->font_info.descent = U32LE(font_file.font_info.descent);
    font->font_info.ascent = U32LE(font_file.font_info.ascent);

    font->font_file = NULL;
    font->font_map = NULL;

    fclose(f);
    free(string_table);
    return font;

error:
    if (string_table)
	free(string_table);
    fclose(f);
    return NULL;
}

/*
 *   Load font into memory (unmapped)
 */
int EFontLoad(EFont* font)
{
    EFontFile* font_data;
    FILE* f;

    if (EFontIsLoaded(font) || EFontIsMapped(font)) {
	fprintf(stderr, "epic_font: file %s, already loaded or mapped\n",
		font->file_name);
	return -1;
    }
    
    if ((f = fopen(font->file_name, "r")) == NULL) {
	fprintf(stderr, "epic_font: file %s, file error: %s\n",
		font->file_name, strerror(errno));
	return -1;
    }

    font_data = (EFontFile*) malloc(font->file_size);
    if (fread((void*)font_data, sizeof(char), font->file_size, f) < 
	font->file_size) {
	fprintf(stderr, "epic_font: file %s size error\n", font->file_name);
	fclose(f);
	free(font_data);
	return -1;
    }

    font->font_data = font_data;
    font->font_file = font_data;
    return 0;
}

int EFontUnload(EFont* font)
{
    if (!EFontIsLoaded(font))
	return 0;
    free(font->font_data);
    font->font_data = NULL;
    font->font_file = NULL;
    return 0;
}

int EFontMap(EFont* font)
{
    int fd;

    if (EFontIsLoaded(font) || EFontIsMapped(font)) {
	fprintf(stderr, "epic_font: file %s, already loaded or mapped\n",
		font->file_name);
	return -1;
    }

    if ((fd = open(font->file_name, O_RDONLY)) < 0) {
	fprintf(stderr, "epic_font: file %s, open error : %s\n",
		font->file_name, strerror(errno));
	return -1;
    }

    font->font_map = (EFontFile*) mmap(0,
				       font->file_size,
				       PROT_READ, MAP_SHARED, 
				       fd, 0);
    close(fd);
    if ((unsigned char*) font->font_map == (unsigned char *) 0xFFFFFFFF) {
	fprintf(stderr, "epic_font: file %s, map error : %s\n",
		font->file_name, strerror(errno));
	font->font_map = NULL;
	return -1;
    }
    font->font_file = font->font_map;
    return 0;
}

int EFontUnmap(EFont* font)
{
    if (!EFontIsMapped(font))
	return 0;
    if (munmap(font->font_map, font->file_size) < 0) {
	fprintf(stderr, "epic_font: file %s, munmap error : %s\n",
		font->file_name, strerror(errno));
	return -1;
    }
    font->font_map  = NULL;
    font->font_file = NULL;
    return 0;
}

/* Draw one char and return the new x position */
void EFontDrawGlyph(EGc*gc, EPixmap* dst, int* x, int* y, int c)
{
    EGlyph* glyph;
    EFont*  font = gc->font;
    int     xoffs;
    int     dwx;
    unsigned int gwidth;
    unsigned int fwidth;
    unsigned int width;
    (void) gc;

    if ((font == NULL) || (font->font_file == NULL))
	return;
    if ((glyph = EFontFileGlyph(font->font_file, c)) == NULL)
	return;

    gwidth = width = U16LE(glyph->width);

    if ((fwidth = gc->glyph_fixed_width)) {
	if (fwidth > gwidth)
	    xoffs = (fwidth - gwidth) / 2;
	else {
	    xoffs = 0;
	    width = fwidth;
	}
	dwx = fwidth;
    }
    else {
	xoffs = I16LE(glyph->xoffs);
	dwx = I16LE(glyph->dwx);
    }

    if ((c == '.') && gc->glyph_dot_kern)
	dwx += gc->glyph_dot_kern;

    /* Set dst = NULL to determine change in x and y */
    if (dst != NULL) {
	int pixelType = font->font_info.pixel_type;
	int psz = EPIXEL_SIZE(pixelType);
	unsigned int bytesPerRow;
	EPixmap gmap;

	// NOTE: each pixel data row must be 16 byte aligned!
	gmap.width     = width;
	gmap.height    = U16LE(glyph->height);
	gmap.pixelType = pixelType;
	gmap.bytesPerPixel = psz;
	gmap.bitsPerPixel = psz*8;
	bytesPerRow = psz*gwidth;
	gmap.bytesPerRow = bytesPerRow + EPIC_ALIGN_OFFS(bytesPerRow,16);
	gmap.sz = gmap.bytesPerRow*gmap.height;
	gmap.data = (void*) glyph->data;
	ERectSet(&gmap.clip, 0, 0, gmap.width, gmap.height);

	if (pixelType == EPIXEL_TYPE_A8)
	    EPixmapAddColorArea(&gmap, dst, 
				gc->fader_value,
				gc->foreground_color,
				//I16LE(glyph->xoffs), 0,
				0, 0,
				*x + xoffs,
				*y - I16LE(glyph->yoffs) - gmap.height,
				gmap.width,
				gmap.height,
				EFLAG_BLEND);
	else
	    EPixmapFadeArea(&gmap, dst, 
			    gc->fader_value,
			    // I16LE(glyph->xoffs), 0,
			    0, 0,
			    *x + xoffs,
			    *y - I16LE(glyph->yoffs) - gmap.height,
			    gmap.width,
			    gmap.height);
    }
    *x += dwx + gc->glyph_delta_x;
    *y += I16LE(glyph->dwy) + gc->glyph_delta_y;
}


void EFontDrawString(EGc* gc, EPixmap* dst, int* x, int* y, char* string)
{
    int c;
    int n = 0;

    while((c = *string++) != 0) {
	if (n && (c == '.') && gc->glyph_dot_kern && *string)
	    *x += gc->glyph_dot_kern;
	EFontDrawGlyph(gc, dst, x, y, c);
	n++;
    }
    if (n)
	*x -= gc->glyph_delta_x;
}

void EFontDrawWideString(EGc* gc, EPixmap* dst, int* x, int* y, wchar_t* string)
{
    int c;
    int n = 0;

    while((c = *string++) != 0) {
	if (n && (c == '.') && gc->glyph_dot_kern && *string)
	    *x += gc->glyph_dot_kern;
	EFontDrawGlyph(gc, dst, x, y, c);
	n++;
    }
    if (n)
	*x -= gc->glyph_delta_x;
}


#define UNI_REPLACEMENT_CHAR (u_int32_t)0x0000FFFD
#define UNI_MAX_UTF32        (u_int32_t)0x7FFFFFFF
#define UNI_MAX_LEGAL_UTF32  (u_int32_t)0x0010FFFF
#define UNI_SUR_HIGH_START   (u_int32_t)0xD800
#define UNI_SUR_HIGH_END     (u_int32_t)0xDBFF
#define UNI_SUR_LOW_START    (u_int32_t)0xDC00
#define UNI_SUR_LOW_END      (u_int32_t)0xDFFF

static int isLegalUTF8(u_int8_t* source, int length)
{
    u_int8_t a;
    const u_int8_t *srcptr = source+length;
    switch (length) {
    default: return 0;
    case 4: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return 0;
    case 3: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return 0;
    case 2: if ((a = (*--srcptr)) > 0xBF) return 0;
	switch (*source) {
	    /* no fall-through in this inner switch */
	    case 0xE0: if (a < 0xA0) return 0;
	    case 0xED: if (a > 0x9F) return 0;
	    case 0xF0: if (a < 0x90) return 0;
	    case 0xF4: if (a > 0x8F) return 0;
	    default:   if (a < 0x80) return 0;
	}
    case 1: if (*source >= 0x80 && *source < 0xC2) return 0;
    }
    if (*source > 0xF4) return 0;
    return 1;
}

void EFontDrawUTF8(EGc* gc, EPixmap* dst, int* x, int* y, char* string)
{
    u_int8_t* source = (u_int8_t*) string;
    u_int8_t* sourceEnd = source + strlen(string);
    int n = 0;

    while(source < sourceEnd) {
	int c;
	u_int8_t bits = *source;

	if ((bits & 0x80) == 0x00)
	    c = *source++;
	else {
	    u_int32_t ch = 0;
	    int n;

	    if ((bits & 0xE0) == 0xC0) n = 1;
	    else if ((bits & 0xF0) == 0xE0) n = 2;
	    else if ((bits & 0xF8) == 0xF0) n = 3;
	    else if ((bits & 0xFC) == 0xF8) n = 4;
	    else n = 5;

	    c = UNI_REPLACEMENT_CHAR;
	    if (!isLegalUTF8(source, n+1))
		break;
	    switch (n) {
	    case 5:  ch += *source++; ch <<= 6;
	    case 4:  ch += *source++; ch <<= 6;
	    case 3:  ch += *source++; ch <<= 6;
	    case 2:  ch += *source++; ch <<= 6;
	    case 1:  ch += *source++; ch <<= 6;
	    default: ch += *source++;
	    }

	    switch(n) {
	    default: break;
	    case 1: ch -= 0x00003080UL; break;
	    case 2: ch -= 0x000E2080UL; break;
	    case 3: ch -= 0x03C82080UL; break;
	    case 4: ch -= 0xFA082080UL; break;
	    case 5: ch -= 0x82082080UL; break;
	    }

	    if ((ch <= UNI_MAX_LEGAL_UTF32) &&
		((ch < UNI_SUR_HIGH_START) || (ch > UNI_SUR_LOW_END)))
		c = ch;
	}
	if (n && (c == '.') && gc->glyph_dot_kern && (source < sourceEnd))
	    *x += gc->glyph_dot_kern;
	EFontDrawGlyph(gc, dst, x, y, c);
	n++;
    }
    if (n)
	*x -= gc->glyph_delta_x;
}
