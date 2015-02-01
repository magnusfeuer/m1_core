/*
 * EPIC GC functions
 */

#include "epic.h"

void EGcInit(EGc* gc)
{
    EOBJECT_INIT(gc, EGC_TYPE);

    gc->fill_style        = EPIC_FILL_STYLE_SOLID;
    gc->fill_color        = epixel_red;
    gc->fill_texture      = NULL;

    gc->line_style        = EPIC_LINE_STYLE_SOLID;
    gc->line_join_style   = EPIC_JOIN_STYLE_MITER;
    gc->line_cap_style    = EPIC_CAP_STYLE_NONE;
    gc->line_width        = 1;
    gc->line_texture      = NULL;

    gc->border_style      = EPIC_BORDER_STYLE_SOLID;
    gc->border_join_style = EPIC_JOIN_STYLE_MITER;
    gc->border_cap_style  = EPIC_CAP_STYLE_NONE;
    gc->border_color      = epixel_black;
    gc->border_width      = 0;
    gc->border_texture    = NULL;

    gc->foreground_color = epixel_blue;
    gc->background_color = epixel_green;

    gc->fader_value      = 255;

    gc->font = NULL;

    gc->glyph_delta_x     = 0;
    gc->glyph_delta_y     = 0;
    gc->glyph_fixed_width = 0;
    gc->glyph_dot_kern    = 0;
}

EGc* EGcCreate()
{
    EGc* gc;

    if ((gc = (EGc*) malloc(sizeof(EGc))) == NULL) 
	return NULL;
    EGcInit(gc);
    gc->on_heap = 1;
    gc->refc = 1;
    return gc;
}

void EGC_TYPE_RELEASE(void* arg)
{
    EGc* gc = (EGc*) arg;

    EDBGFMT_MEM("EGC_TYPE_RELEASE: %p", arg);
    EObjectUnRef(gc->fill_texture);
    EObjectUnRef(gc->line_texture);
    EObjectUnRef(gc->border_texture);
    EObjectUnRef(gc->font);
    if (gc->on_heap)
	free(gc);
}

EGc* EGcCopy(EGc* gc)
{
    EGc* copy;

    if ((copy = (EGc*) malloc(sizeof(EGc))) == NULL)
	return NULL;
    /* reference textures etc */
    *copy = *gc;
    EObjectRef(copy->fill_texture);
    EObjectRef(copy->line_texture);
    EObjectRef(copy->border_texture);
    EObjectRef(copy->font);
    copy->next = NULL;
    copy->refc = 1;
    copy->on_heap = 1;
    return copy;
}
