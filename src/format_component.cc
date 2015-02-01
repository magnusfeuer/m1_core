//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//

#include <math.h> // for nearbyintf() rounding function. Link with -m.
#include "epic.h"

#include "m1.hh"
#include "format_component.hh"
#include "screen_component.hh"

XOBJECT_TYPE_BOOTSTRAP(CFormatComponent);
XOBJECT_TYPE_BOOTSTRAP(CBoolComponent);
XOBJECT_TYPE_BOOTSTRAP(CCharComponent);
XOBJECT_TYPE_BOOTSTRAP(CByteComponent);
XOBJECT_TYPE_BOOTSTRAP(CSignedComponent);
XOBJECT_TYPE_BOOTSTRAP(CUnsignedComponent);
XOBJECT_TYPE_BOOTSTRAP(CFloatComponent);
XOBJECT_TYPE_BOOTSTRAP(CStringComponent);
XOBJECT_TYPE_BOOTSTRAP(CTextComponent);

static int resolution(CScreenComponent* aScreen)
{
    if (aScreen)
	return aScreen->resolution_y();
    else
	return 75;
}

CFormatComponent::CFormatComponent(CExecutor* aExec,CBaseType *aType):
    CLayerComponent(aExec, aType),
    mFont(NULL),
    mFormat(this),
    mFontName(this),
    mFontSize(this),
    mFontWeight(this),
    mFontSlant(this),
    mFontColor(this),
    mGlyphDeltaX(this),
    mGlyphDeltaY(this),
    mGlyphDotKerning(this),
    mGlyphFixedWidth(this)
{
    mFontName.putValue(aExec, "Helvetica");
    mFontSize.putValue(aExec, 12);
    mFontWeight.putValue(aExec, EFONT_WEIGHT_BOLD);
    mFontSlant.putValue(aExec, EFONT_SLANT_ROMAN);
    mFormat.putValue(aExec, "");
    mGlyphDeltaX.putValue(aExec,  0);
    mGlyphDeltaY.putValue(aExec,  0);
    mGlyphDotKerning.putValue(aExec, 0);
    mGlyphFixedWidth.putValue(aExec, 0);
    mFontChanged   = false;

    eventPut(aExec, XINDEX(CFormatComponent,format),     &mFormat);
    eventPut(aExec, XINDEX(CFormatComponent,fontName),   &mFontName);
    eventPut(aExec, XINDEX(CFormatComponent,fontSize),   &mFontSize);
    eventPut(aExec, XINDEX(CFormatComponent,fontWeight), &mFontWeight);
    eventPut(aExec, XINDEX(CFormatComponent,fontSlant),  &mFontSlant);
    eventPut(aExec, XINDEX(CFormatComponent,fontColor),  &mFontColor);

    eventPut(aExec, XINDEX(CFormatComponent,glyphDeltaX), &mGlyphDeltaX);
    eventPut(aExec, XINDEX(CFormatComponent,glyphDeltaY), &mGlyphDeltaY);
    eventPut(aExec, XINDEX(CFormatComponent,glyphDotKerning),&mGlyphDotKerning);
    eventPut(aExec, XINDEX(CFormatComponent,glyphFixedWidth),&mGlyphFixedWidth);

    loadFont(resolution(m1_default_screen()));
}


CFormatComponent::~CFormatComponent(void)
{
    /* Nothing yet */
}

void CFormatComponent::loadFont(int aResolution)
{
    string name;
    CFont* font;
    
    name = mFontName.value();
    if (!(font = m1_fonts().match(aResolution,name,
				  mFontSize.value(),
				  mFontWeight.value(),
				  mFontSlant.value()))) {
	printf("WARNING: Could not load font [%s] Size[%d]\n", 
	       name.c_str(), mFontSize.value());
    }
    m1SetRetain(CFont, &mFont, font);
}

void CFormatComponent::update(CExecutor* aExec, bool aStart)
{
    mFontChanged = false;

    if (mFontName.assigned()) {
	if (aStart) mFontName.cancel(aExec);
	mFontChanged = true;
    }
    if (mFontSlant.assigned()) {
	if (aStart) mFontSlant.cancel(aExec);
	mFontChanged = true;
    }
    if (mFontWeight.assigned()) {
	if (aStart) mFontWeight.cancel(aExec);
	mFontChanged = true;
    }
    if (mFontSize.assigned()) {
	if (aStart) mFontSize.cancel(aExec);
	mFontChanged = true;
    }
    if (mFontChanged)
	loadFont(resolution(m1_default_screen()));
}

void CFormatComponent::calcText(CExecutor* aExec)
{
    if (mFontChanged || styleChanged() || 
	mFormat.assigned() || valueEvent()->assigned()) {
	CStyle* style;
	int x = 0;
	int y = 0;
	int height = 0;
	char buf[1024];

	valueFormat(buf, sizeof(buf));

	if ((style = mStyleLink.style()) != NULL) {
	    EGc* gc;
	    CFont* fnt;
	    
	    gc = style->epicContext();
	    if ((fnt = style->font()) != NULL) {
		fnt->use(aExec->cycleTime());
		EFontDrawString(gc, NULL, &x, &y, buf);
		height = fnt->epicFont()->font_info.ascent + 
		    fnt->epicFont()->font_info.descent;
	    }
	}
	else if (mFont) {
	    EGc gc;
	    EFont* efnt;
	
	    mFont->use(aExec->cycleTime());
	    if ((efnt = mFont->epicFont()) != NULL) {
		gc.font = efnt;
		EGcSetGlyphDelta(&gc, mGlyphDeltaX.value(), mGlyphDeltaY.value());
		EGcSetGlyphDotKern(&gc, mGlyphDotKerning.value());
		EGcSetGlyphWidth(&gc, mGlyphFixedWidth.value());
		EFontDrawString(&gc, NULL, &x, &y, buf);
		height = efnt->font_info.ascent + efnt->font_info.descent;
	    }
	}
	mContentWidth.putValue(aExec, (float) x);
	mContentHeight.putValue(aExec, (float) height);
    }
}

void CFormatComponent::start(CExecutor* aExec)
{
    CLayerComponent::start(aExec);
    update(aExec, true);
    calcText(aExec);
    valueEvent()->cancel(aExec);
    mFormat.cancel(aExec);
}

void CFormatComponent::execute(CExecutor* aExec)
{
    update(aExec, false);
}

void CFormatComponent::post_execute(CExecutor* aExec)
{
    calcText(aExec);
}

void CFormatComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    EGc*  gc;
    u_int8_t fader;

    if (!aContext || !aContext->mPixmap) {
	printf("CFormatComponent::redrawText(): No context or pixmap provided.\n");
	return;
    }

    gc = aContext->mGc;       // get redrawContext gc
    if ((fader = gc->fader_value) != ALPHA_FACTOR_0) {
	CStyle* style;
	int x, y;
	char buf[1024];

	valueFormat(buf, sizeof(buf));

	if ((style = mStyleLink.style()) != NULL) {
	    CFont* fnt;

	    gc = style->epicContext();
	    if ((fnt = style->font()) != NULL) {
		EPixel_t save_color;

		fnt->use(aSys->cycleTime());
		// Since we are modifying color alpha we need to save/restore
		// unil we fixed EFontDrawString
		save_color = gc->foreground_color;
		if (fader != ALPHA_FACTOR_1)
		    gc->foreground_color.a = (gc->foreground_color.a * fader) >> 8;
		x = int(aContext->lLeft);
		y = int(aContext->lTop) + fnt->epicFont()->font_info.ascent;
		EFontDrawString(gc, aContext->mPixmap, &x, &y, buf);
		gc->foreground_color = save_color;
	    }
	}
	else if (mFont) {
	    EFont* savefnt = gc->font;
	    EPixel_t color;

	    gc->font = mFont->epicFont();
	    color.px = mFontColor.value();
	    // color.a  = 0;

	    if (fader != ALPHA_FACTOR_1)
		color.a = (color.a * fader) >> 8;
	    EGcSetForegroundColor(gc, color);

	    mFont->use(aSys->cycleTime());
	
	    EGcSetGlyphDelta(gc, mGlyphDeltaX.value(), mGlyphDeltaY.value());
	    EGcSetGlyphDotKern(gc, mGlyphDotKerning.value());
	    EGcSetGlyphWidth(gc, mGlyphFixedWidth.value());
	    
	    x = int(aContext->lLeft);
	    y = int(aContext->lTop) + mFont->epicFont()->font_info.ascent;
	    EFontDrawString(gc, aContext->mPixmap, &x, &y, buf);
	    gc->font = savefnt;
	}
    }
}

// Fixme check for exactly one % sign
static int sprintf_value(char* buf, size_t size, UData format, UData value)
{
    string ostr;
    ostringstream oStream;
    UData va[2];
    char* ptr;
    size_t len;

    va[0] = format;
    va[1] = value;

    (void) m1_format_args(&oStream, va);
    ostr = oStream.str();
    ptr = (char*) ostr.c_str();
    if ((len = ostr.length()) >= size)
	len = size-1;
    memcpy(buf, ptr, len);
    buf[len] = '\0';
    return len;
}


////////////////////////////////////////////////////////////////////////////
//  Bool
////////////////////////////////////////////////////////////////////////////

CBoolComponent::CBoolComponent(CExecutor* aExec, CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, ' ');
    mFormat.putValue(aExec, "%d");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CBoolComponent,value), &mValue);
}

int CBoolComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}

////////////////////////////////////////////////////////////////////////////
//  Char
////////////////////////////////////////////////////////////////////////////

CCharComponent::CCharComponent(CExecutor* aExec,CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, ' ');
    mFormat.putValue(aExec, "%c");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CCharComponent,value), &mValue);
}

int CCharComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}

////////////////////////////////////////////////////////////////////////////
//  Byte
////////////////////////////////////////////////////////////////////////////

CByteComponent::CByteComponent(CExecutor* aExec, CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, ' ');
    mFormat.putValue(aExec, "%u");

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CByteComponent,value), &mValue);
}

int CByteComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}


////////////////////////////////////////////////////////////////////////////
//  Signed
////////////////////////////////////////////////////////////////////////////
CSignedComponent::CSignedComponent(CExecutor* aExec,CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, 0);
    mFormat.putValue(aExec, "%d");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CSignedComponent,value), &mValue);
}

int CSignedComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}

////////////////////////////////////////////////////////////////////////////
//  Unsigned
////////////////////////////////////////////////////////////////////////////

CUnsignedComponent::CUnsignedComponent(CExecutor* aExec, CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, 0);
    mFormat.putValue(aExec, "%u");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CUnsignedComponent,value), &mValue);
}

int CUnsignedComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}

////////////////////////////////////////////////////////////////////////////
//  Float
////////////////////////////////////////////////////////////////////////////
CFloatComponent::CFloatComponent(CExecutor* aExec, CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, 0);
    mFormat.putValue(aExec, "%f");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CFloatComponent,value), &mValue);
}

int CFloatComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}

////////////////////////////////////////////////////////////////////////////
//  String
////////////////////////////////////////////////////////////////////////////


CStringComponent::CStringComponent(CExecutor* aExec, CBaseType *aType):
    CFormatComponent(aExec, aType),
    mValue(this)
{
    mValue.putValue(aExec, "");
    mFormat.putValue(aExec, "%s");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CStringComponent,value), &mValue);
}

int CStringComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mValue.uvalue());
}

////////////////////////////////////////////////////////////////////////////
//  Text
// This will be changed to render wrapped text in UTF-8
//
////////////////////////////////////////////////////////////////////////////

CTextComponent::CTextComponent(CExecutor* aExec, CBaseType *aType):
    CFormatComponent(aExec, aType),
    mText(this)
{
    mText.putValue(aExec, "");
    mFormat.putValue(aExec, "%s");  // default format

    mFormat.cancel(aExec);
    eventPut(aExec, XINDEX(CTextComponent,text), &mText);
}

int CTextComponent::valueFormat(char* buf, size_t size)
{
    return sprintf_value(buf, size, mFormat.uvalue(), mText.uvalue());
}

