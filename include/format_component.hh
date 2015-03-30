//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007
//

#ifndef __FORMAT_COMPONENT_HH__
#define __FORMAT_COMPONENT_HH__

#include "component.hh"
#include "font_cache.hh"
#include "style.hh"

//
// A Format component.
//
class CFormatComponent: public CLayerComponent {
public:
    XDERIVED_OBJECT_TYPE(CFormatComponent, CLayerComponent,
			 "Format",
			 "Formatting component",
			 (CFormatComponent_format,
			  CFormatComponent_fontName,
			  CFormatComponent_fontSize,
			  CFormatComponent_fontWeight,
			  CFormatComponent_fontSlant,
			  CFormatComponent_fontColor,
			  CFormatComponent_glyphDeltaX,
			  CFormatComponent_glyphDeltaY,
			  CFormatComponent_glyphDotKerning,
			  CFormatComponent_glyphFixedWidth),
			 
			 XFIELD(CFormatComponent,Q_PUBLIC,format,
				input_string_type(),
				""),

			// All below is deprecated (will go soon)

			 XFIELD(CFormatComponent,Q_PUBLIC,fontName,
				input_string_type(),
				""),
			 XFIELD(CFormatComponent,Q_PUBLIC,fontSize,
				input_unsigned_type(),
				""),
			 XFIELD(CFormatComponent,Q_PUBLIC,fontWeight,
				CEventType::create(CFontWeightType::singleton(), E_INPUT),
				""),
			 XFIELD(CFormatComponent,Q_PUBLIC,fontSlant,
				CEventType::create(CFontSlantType::singleton(), E_INPUT),
				""),
			 XFIELD(CFormatComponent,Q_PUBLIC,fontColor,
				input_unsigned_type(),
				""),

			 XFIELD(CFormatComponent,Q_PUBLIC,glyphDeltaX,
				input_signed_type(),
				""),
			 XFIELD(CFormatComponent,Q_PUBLIC,glyphDeltaY,
				input_signed_type(),
				""),
			 
			 XFIELD(CFormatComponent,Q_PUBLIC,glyphDotKerning,
				input_signed_type(),
				""),
			 XFIELD(CFormatComponent,Q_PUBLIC,glyphFixedWidth,
				input_unsigned_type(),
				"")
	);
public:
    CFormatComponent(CExecutor* aExec,
		     CBaseType *aType = CFormatComponentType::singleton());
    ~CFormatComponent(void);

    void execute(CExecutor* aExec);
    void post_execute(CExecutor* aExec);
    void start(CExecutor* aExec);

    void redraw(CSystem* aSys, CRedrawContext *aContext);

    bool fontChanged(void) { return mFontChanged; }

    virtual int valueFormat(char* buf, size_t size) { return 0; }
    virtual CEvent* valueEvent(void) { return NULL; }

protected:
    void update(CExecutor* aExec, bool aStart);
    void calcText(CExecutor* aExec);

    CFont* mFont;
    EventString mFormat;
    // This may go since we have styles
    EventString mFontName;
    EventUnsigned mFontSize;
    EventEnum<epx_font_weight_t> mFontWeight;
    EventEnum<epx_font_slant_t> mFontSlant;
    EventUnsigned mFontColor;
    EventSigned   mGlyphDeltaX;      // fixed kerning
    EventSigned   mGlyphDeltaY;      // fixed kerning
    EventSigned   mGlyphDotKerning;  // fixed kerning for .
    EventUnsigned mGlyphFixedWidth;  // fixe glyph width
    bool mFontChanged;             // true iff anything changed
private:
    void loadFont(int aResolution);
};




class CBoolComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CBoolComponent, 
			 CFormatComponent,
			 "Bool",
			 "Boolean format component",
			 (CBoolComponent_value),
			 XFIELD(CBoolComponent,Q_PUBLIC,value,
				input_bool_type(),
				"")
	);
public:
    CBoolComponent(CExecutor* aExec,
		   CBaseType *aType = CBoolComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventBool mValue;
};


class CByteComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CByteComponent, 
			 CFormatComponent,
			 "Byte",
			 "Byte format component",
			 (CByteComponent_value),
			 XFIELD(CByteComponent,Q_PUBLIC,value,
				input_byte_type(),
				"")
	);
public:
    CByteComponent(CExecutor* aExec,
		   CBaseType *aType = CByteComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventByte mValue;
};


class CCharComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CCharComponent, 
			 CFormatComponent,
			 "Char",
			 "Character format component",
			 (CCharComponent_value),
			 XFIELD(CCharComponent,Q_PUBLIC,value,
				input_char_type(),
				"")
	);
public:
    CCharComponent(CExecutor* aExec,
		   CBaseType *aType = CCharComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventChar mValue;
};

class CSignedComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CSignedComponent, 
			 CFormatComponent,
			 "Signed",
			 "Signed integer format component",
			 (CSignedComponent_value),
			 XFIELD(CSignedComponent,Q_PUBLIC,value,
				input_signed_type(),
				"")
	);
public:
    CSignedComponent(CExecutor* aExec,
		     CBaseType *aType = CSignedComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventSigned mValue;
};


class CUnsignedComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CUnsignedComponent, 
			 CFormatComponent,
			 "Unsigned",
			 "Unsigned integer format component",
			 (CUnsignedComponent_value),
			 XFIELD(CUnsignedComponent,Q_PUBLIC,value,
				input_unsigned_type(),
				"")
	);
public:
    CUnsignedComponent(CExecutor* aExec,
		       CBaseType *aType = CUnsignedComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventUnsigned mValue;
};

class CFloatComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CFloatComponent, 
			 CFormatComponent,
			 "Float",
			 "Floating point format component",
			 (CFloatComponent_value),
			 XFIELD(CFloatComponent,Q_PUBLIC,value,
				input_float_type(),
				"")
	);
public:
    CFloatComponent(CExecutor* aExec, 
		    CBaseType *aType = CFloatComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventFloat mValue;
};


class CStringComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CStringComponent, 
			 CFormatComponent,
			 "String",
			 "String format component",
			 (CStringComponent_value),
			 XFIELD(CStringComponent,Q_PUBLIC,value,
				input_string_type(),
				"")
	);
public:
    CStringComponent(CExecutor* aExec,
		     CBaseType *aType = CStringComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mValue; }
private:
    EventString mValue;
};

class CTextComponent: public CFormatComponent {
public:
    XDERIVED_OBJECT_TYPE(CTextComponent, 
			 CFormatComponent,
			 "Text",
			 "Text format component",
			 (CTextComponent_text),
			 XFIELD(CTextComponent,Q_PUBLIC,text,
				input_string_type(),
				"")
	);
public:
    CTextComponent(CExecutor* aExec,
		   CBaseType *aType = CTextComponentType::singleton());
    int     valueFormat(char* buf, size_t size);
    CEvent* valueEvent(void) { return &mText; }
private:
    EventString mText;
};



#endif // __FORMAT_COMPONENT_H__
