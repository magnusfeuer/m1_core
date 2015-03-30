//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#ifndef __STYLE_HH__
#define __STYLE_HH__

#include "epx.h"
#include "m1.hh"
#include "font_cache.hh"

typedef enum {
    VALIGN_NONE   = 'n',
    VALIGN_TOP    = 't',
    VALIGN_BOTTOM = 'b',
    VALIGN_CENTER = 'c',
    VALIGN_SCALE  = 's'
} VAlign;

typedef enum {
    HALIGN_NONE   = 'n',
    HALIGN_LEFT   = 'l',
    HALIGN_RIGHT  = 'r',
    HALIGN_CENTER = 'c',
    HALIGN_SCALE  = 's'
} HAlign;

ENUM_TYPE(CVAlign, "VAlign",
	  ENUMERATION(none,    VALIGN_NONE),
	  ENUMERATION(top,     VALIGN_TOP),
	  ENUMERATION(bottom,  VALIGN_BOTTOM),
	  ENUMERATION(center,  VALIGN_CENTER),
	  ENUMERATION(scale,   VALIGN_SCALE));

ENUM_TYPE(CHAlign, "HAlign",
	  ENUMERATION(none,    HALIGN_NONE),
	  ENUMERATION(left,    HALIGN_LEFT),
	  ENUMERATION(right,   HALIGN_RIGHT),
	  ENUMERATION(center,  HALIGN_CENTER),
	  ENUMERATION(scale,   HALIGN_SCALE));

ENUM_TYPE(CFontWeight, "FontWeight",
	  ENUMERATION(none, EPX_FONT_WEIGHT_NONE),
	  ENUMERATION(medium, EPX_FONT_WEIGHT_MEDIUM),
	  ENUMERATION(bold, EPX_FONT_WEIGHT_BOLD),
	  ENUMERATION(demiBold, EPX_FONT_WEIGHT_DEMIBOLD));

ENUM_TYPE(CFontSlant, "FontSlant",
	  ENUMERATION(none, EPX_FONT_SLANT_NONE),
	  ENUMERATION(roman, EPX_FONT_SLANT_ROMAN),
	  ENUMERATION(italic, EPX_FONT_SLANT_ITALIC),
	  ENUMERATION(oblique, EPX_FONT_SLANT_OBLIQUE));

ENUM_TYPE(CFontWidth, "FontWidth",
	  ENUMERATION(none, EPX_FONT_WIDTH_NONE),
	  ENUMERATION(normal, EPX_FONT_WIDTH_NORMAL),
	  ENUMERATION(condensed, EPX_FONT_WIDTH_CONDENSED),
	  ENUMERATION(narrow, EPX_FONT_WIDTH_NARROW),
	  ENUMERATION(doubleWide, EPX_FONT_WIDTH_DOUBLE_WIDE));

ENUM_TYPE(CFontStyle, "FontStyle",
	  ENUMERATION(none, EPX_FONT_STYLE_NONE),
	  ENUMERATION(serif, EPX_FONT_STYLE_SERIF),
	  ENUMERATION(sansSerif, EPX_FONT_STYLE_SANS_SERIF),
	  ENUMERATION(informal, EPX_FONT_STYLE_INFORMAL),
	  ENUMERATION(decorated, EPX_FONT_STYLE_DECORATED));

ENUM_TYPE(CFontSpacing, "FontSpacing",
	  ENUMERATION(none, EPX_FONT_SPACING_NONE),
	  ENUMERATION(proportional, EPX_FONT_SPACING_PROPORTIONAL),
	  ENUMERATION(monoSpaced, EPX_FONT_SPACING_MONOSPACED),
	  ENUMERATION(charCell, EPX_FONT_SPACING_MONOSPACED));

///////////////////////////////////////////////////////////////////////////////
//
// CSyleLink
//        used as a combo event and handles attributes 
//        class=  & style=
//
///////////////////////////////////////////////////////////////////////////////

class CStyle;


class CStyleLink {
public:
    CStyleLink(CExecutable* aOwner, CExecutor* aExec);
    ~CStyleLink();

    bool     update(CExecutor* aExec, bool aStart);

    string   styleClass(void) { return mClass.value(); }
    CStyle*  style(void) { return mStyle.value(); }
    bool     styleUpdated(void) { return mStyleUpdated.updated(); }
private:
    EventObject<CStyle*> mStyle;         // handles the style= attribute
    EventBool            mStyleUpdated;  // handles style updates
    EventString          mClass;         // handles on class name updates
};


///////////////////////////////////////////////////////////////////////////////
//
// The style
//
///////////////////////////////////////////////////////////////////////////////
class CStyle : public CExecutable {
public:
    XOBJECT_TYPE(CStyle, "Style", 
		 "Style template",
		 (CStyle_name, 
		  CStyle_foregroundColor,
		  CStyle_backgroundColor,
		  CStyle_borderWidth,
		  CStyle_borderColor,
		  CStyle_fill,
		  CStyle_fillColor,
		  CStyle_fontName,
		  CStyle_fontSize,
		  CStyle_fontWeight,
		  CStyle_fontSlant,
		  CStyle_fontColor,
		  CStyle_glyphDeltaX,
		  CStyle_glyphDeltaY,
		  CStyle_glyphDotKerning,
		  CStyle_glyphFixedWidth),
		 XFIELD(CStyle,Q_PUBLIC,name, 
			input_string_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,foregroundColor,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,backgroundColor,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,borderWidth,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,borderColor,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fill,
			input_bool_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fillColor,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fontName,
			input_string_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fontSize,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fontWeight,
			EVENT_TYPE(CFontWeightType, E_INPUT),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fontSlant,
			EVENT_TYPE(CFontSlantType, E_INPUT),
			""),
		 XFIELD(CStyle,Q_PUBLIC,fontColor,
			input_unsigned_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,glyphDeltaX,
			input_signed_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,glyphDeltaY,
			input_signed_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,glyphDotKerning,
			input_signed_type(),
			""),
		 XFIELD(CStyle,Q_PUBLIC,glyphFixedWidth, 
			input_unsigned_type(), 
			"")
	);
public:
    CStyle(CExecutor* aExec, CBaseType* aType = CStyleType::singleton());
    ~CStyle(void);
    
    int mark(Mark_t aMark);

    unsigned int backgroundColor(void) { return mBackgroundColor.value(); }
    unsigned int foregroundColor(void) { return mForegroundColor.value(); }
    string name(void) { return mName.value(); }
    CFont* font(void) { return mFont; }

    epx_gc_t*   epxContext(void)     { return &mGC; }
    epx_font_t* epxFont(void)        { return mFont ? mFont->epxFont() : NULL; }

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);

private:
    void loadFont(int aResolution);
    bool update(CExecutor* aExec, bool aStart);

    EventString    mName;   // style name
    EventUnsigned  mForegroundColor;
    EventUnsigned  mBackgroundColor;
    EventUnsigned  mBorderColor;
    EventUnsigned  mFillColor;
    EventBool      mFill;
    EventUnsigned  mBorderWidth;
    EventString    mFontName;
    EventUnsigned  mFontSize;
    EventEnum<epx_font_weight_t> mFontWeight;
    EventEnum<epx_font_slant_t> mFontSlant;
    EventUnsigned mFontColor;
    EventSigned   mGlyphDeltaX;      // fixed kerning
    EventSigned   mGlyphDeltaY;      // fixed kerning
    EventSigned   mGlyphDotKerning;  // fixed kerning for .
    EventUnsigned mGlyphFixedWidth;  // fixe glyph width

    CFont* mFont;
    epx_gc_t    mGC;
};



//
//  Style manager list item
//

class CStyleItem {
public:
    CStyleItem(string aName, CStyle* aStyle);
    CStyleItem(const CStyleItem &aItem);
    ~CStyleItem(void);

    int mark(Mark_t aMark);

    string  name(void)       { return mName; }
    CStyle* style(void)      { return mStyle.value(); }
    EventObject<CStyle*>*    styleEvent(void) { return &mStyle; }
    void  setStyle(CExecutor* aExec, CStyle* aStyle);
private:
    string mName;
    EventObject<CStyle*> mStyle;
};

typedef list<CStyleItem> CStyleList;

//
//  Style manager handle all registered styles
//

class CStyleManager : public CRuntime {
public:
    CStyleManager() {}
    ~CStyleManager() {}

    int mark(Mark_t aMark);

    CStyle* lookupStyle(string name);
    EventObject<CStyle*>* createStyle(CExecutor* aExec, string name);
    bool    unregisterStyle(CExecutor* aExec, string name);
    bool    unregisterStyle(CExecutor* aExec, CStyle* aStyle);
    bool    registerStyle(CExecutor* aExec, CStyle* aStyle);
    bool    member(CStyle* aStyle);
private:
    CStyleList mStyleList;

    CStyleList::iterator findStyle(string name);
    CStyleList::iterator findStyle(CStyle* style);
};

// return/create the global style manager
CStyleManager& m1_styles(void);
CStyle* m1_default_style();

#endif

