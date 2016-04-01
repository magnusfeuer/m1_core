//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#ifndef __COMPONENT_HH__
#define __COMPONENT_HH__

#include <stdint.h>
#include <sys/types.h>

#include "epx.h"
#include "m1.hh"
#include "style.hh"
#include "message.hh"

#ifdef USE_FFMPEG
#include <ffmpeg/avcodec.h>
#include <ffmpeg/swscale.h>
#else
typedef void* SwsContext;
#endif

//
// All displayable components inherits from this one.
//
class CLayerComponent: public CExecutable { 
public:
    XOBJECT_TYPE(CLayerComponent, "Layer", 
		 "Displayable base component.",
		 (CLayerComponent_children, 
		  CLayerComponent_wantFocus, 
		  CLayerComponent_exclusiveFocus, 
		  CLayerComponent_messageMask,
		  CLayerComponent_message,
		  CLayerComponent_top,
		  CLayerComponent_left,
		  CLayerComponent_height,
		  CLayerComponent_width,
		  CLayerComponent_contentWidth,
		  CLayerComponent_contentHeight,
		  CLayerComponent_active, 
		  CLayerComponent_enabled, 
		  CLayerComponent_transparency,
		  CLayerComponent_clip, 
		  CLayerComponent_halign,
		  CLayerComponent_valign,
		  CLayerComponent_hscale, 
		  CLayerComponent_vscale, 
		  CLayerComponent_background, 
		  CLayerComponent_backgroundColor, 
		  CLayerComponent_childrenFirst, 
		  CLayerComponent_style, 
		  CLayerComponent_class),
		 XFIELD(CLayerComponent,Q_PUBLIC, children, 
			CArrayType::create(
			  (this->name() == "Layer") ? 
			  this :
			  CLayerComponent::CLayerComponentType::singleton(),0),
			"Sub layer list"),
		 //! Checked by CInputStrategyBase.
		 XFIELD(CLayerComponent,Q_PUBLIC, wantFocus, bool_type(),
			"Signal if layer need input."),
		 XFIELD(CLayerComponent,Q_PUBLIC, exclusiveFocus, bool_type(),
			"Do not send input events to any layer "
			"above this one."),

		 XFIELD(CLayerComponent,Q_PUBLIC, messageMask, event_unsigned_type(),
			"Bit mask for selecting message types"),
		 XFIELD(CLayerComponent,Q_PUBLIC, message, 
			EVENT_QUEUE_TYPE(CMessage::CMessageType, E_INOUT),
			"The input event message"),
		
		 XFIELD(CLayerComponent,Q_PUBLIC, top, event_float_type(),
			"Top position of layer."),
		 XFIELD(CLayerComponent,Q_PUBLIC, left, event_float_type(),
			"Left position of layer."),
		 XFIELD(CLayerComponent,Q_PUBLIC, height, event_float_type(),
			"Height of layer."),
		 XFIELD(CLayerComponent,Q_PUBLIC, width,  event_float_type(),
			"Width of layer."),
		 XFIELD(CLayerComponent,Q_PUBLIC, contentWidth, event_float_type(),
			"Internal width of layer."),
		 XFIELD(CLayerComponent,Q_PUBLIC, contentHeight, event_float_type(),
			"Internal height of layer."),
		 // Not used yet.
		 XFIELD(CLayerComponent,Q_PUBLIC, active, output_bool_type(),
			"Layer is active."),

		// Not used yet. We should/could use it with focus
		 XFIELD(CLayerComponent,Q_PUBLIC, enabled, event_bool_type(),
			"Layer is enabled."),
		 XFIELD(CLayerComponent,Q_PUBLIC, transparency, event_float_type(),
			"Transparancy level (0,1)"),
		// clip rendering, yes or no
		 XFIELD(CLayerComponent,Q_PUBLIC, clip, event_bool_type(),
			"Clip rendering of layer to it's bounds."),
		 XFIELD(CLayerComponent,Q_PUBLIC, halign, 
			CEventType::create(CHAlignType::singleton()),
			"Horizontal alignment type"),
		 XFIELD(CLayerComponent,Q_PUBLIC, valign, 
			CEventType::create(CVAlignType::singleton()),
			"Vertival aligment type"),
		 XFIELD(CLayerComponent,Q_PUBLIC, hscale, event_float_type(),
			"Horizontal scale factor."),
		 XFIELD(CLayerComponent,Q_PUBLIC, vscale, event_float_type(),
			"Vertical scale factor."),
		 XFIELD(CLayerComponent,Q_PUBLIC, background,  event_bool_type(),
			"Draw background."),
		 XFIELD(CLayerComponent,Q_PUBLIC, backgroundColor, event_unsigned_type(),
			"Background color."),
		 XFIELD(CLayerComponent,Q_PUBLIC, childrenFirst, event_bool_type(),
			"Redraw children before self."),
		 XFIELD(CLayerComponent,Q_PUBLIC, style,
			EVENT_TYPE(CStyle::CStyleType,E_INPUT),
			"Direct style"),
		 XFIELD(CLayerComponent,Q_PUBLIC, class,   input_string_type(),
			"Named style used")
	);
public:
    //
    // Redraw context. 
    // The initial context is setup by redraw and is passed
    // to the top level internalRedraw.
    // Each level can copy the redraw contexxt, modify the local copy
    // and pass it on to its children.
    //
    struct CRedrawContext {
	epx_pixmap_t* mPixmap; // Pixmap to draw in.
	epx_gc_t* mGc;         // Graphic context
	float lTop;            // scaled layer top
	float lLeft;           // scaled layer left
	float lWidth;          // scaled layer width
	float lHeight;         // scaled layer height
	float cWidth;          // scaled content width
	float cHeight;         // scaled content height
	float mTransparency;   // Transparency value to use when drawing.
	float mVscale;         // Scale vertical (1.0 = normal scale).
	float mHscale;         // Scale horizontal (1.0 = normal scale).
	epx_rect_t mClipRect;  // Current clipping rectangle
    };


    CLayerComponent(CExecutor* aExec,
		    CBaseType* aType = CLayerComponentType::singleton());
    virtual ~CLayerComponent(void);

    virtual float height(void) { return mHeight.value(); }
    virtual float width(void) { return mWidth.value(); }
    virtual float top(void) { return mTop.value(); }
    virtual float left(void) { return mLeft.value(); }

    // Content width/height is the "physical size" of the content in the layer
    virtual float contentWidth(void) { return mContentWidth.value(); }
    virtual float contentHeight(void) { return mContentHeight.value(); }

    virtual float scaleHorizontal(void) { return mHscale.value(); }
    virtual float scaleVertical(void) { return mVscale.value(); }

    virtual bool clip(void) { return mClip.value(); }

    virtual VAlign valign(void);
    virtual HAlign halign(void);

    virtual float transparency(void) { return mTransparency.value(); }
    virtual bool enabled(void) { return mEnabled.value(); }
    virtual bool visible(void) { return (mTransparency.value() < 1.0); }

    // Redraw the component contents
    virtual void redraw(CSystem* aSys, CRedrawContext *aContext);  

    virtual bool background(void) { return mBackground.value(); }
    virtual unsigned int backgroundColor(void) { return mBackgroundColor.value(); }

    bool canScale(CRedrawContext* aContext);
    bool needScale(CRedrawContext* aContext);
    void scaleImage(epx_pixmap_t* aSrcImage, epx_pixmap_t* aDstImage, bool aScaleAlpha);
    void redrawScaled(CRedrawContext *aContext, epx_pixmap_t* aDstPixmap);

    // Redraw the children list
    void redrawChildren(CSystem* aSys, CRedrawContext *aContext);
    // Redraw the component only
    void redrawComponent(CSystem* aSys, CRedrawContext *aContext);
    // Redraw layer background and general things
    void redrawLayer(CSystem* aSys, CRedrawContext *aContext);


    void start(CExecutor* aExec);
    void execute(CExecutor* aExec);
    void post_execute(CExecutor* aExec);

    SwsContext* cachedSwsContext(CRedrawContext* aContext,int sfmt,int dfmt,
				 bool aAlways);
    int         pixelType2PixelFormat(int aPixelType);

    CLayerComponent *child(unsigned int aIndex) { 
	CArray* children = at(XINDEX(CLayerComponent, children)).arr;
	if (aIndex >= children->size())
	    return 0;
	return dynamic_cast<CLayerComponent *>(children->at(aIndex).o);
    }

    unsigned int childCount(void) { 
	return at(XINDEX(CLayerComponent, children)).arr->size(); }

    bool wantFocus(void) { 
	return at(XINDEX(CLayerComponent, wantFocus)).t; 
    }

    bool exclusiveFocus(void) { 
	return at(XINDEX(CLayerComponent, exclusiveFocus)).t; 
    }

    unsigned int messageMask(void) { return mMessageMask.value(); }
    void         postMessage(CMessage* aMessage) { mMessage.putValue(NULL,aMessage); }

    void         buttonSet(int i) { button_mask |= (1 << i); }
    void         buttonClr(int i) { button_mask &= ~(1 << i); }
    bool         buttonTest(int i) { return (button_mask & (1 << i)) != 0; }
    unsigned int buttonMask(void) { return button_mask; }
    
protected:
    void intersectClip(CRedrawContext &aContext);
    void initContext(CRedrawContext &aContext, epx_gc_t* aGc);
    void modifyContext(CRedrawContext &aContext);

    bool styleChanged(void);

protected:
    EventFloat mTop;
    EventFloat mLeft;
    EventFloat mHeight;
    EventFloat mWidth;
    EventFloat mContentHeight;
    EventFloat mContentWidth;
    EventBool  mActive;
    EventBool  mEnabled;
    EventFloat mTransparency;

    EventUnsigned mMessageMask;            // Message mask 
    EventQueueObject<CMessage*> mMessage;  // Message input
    EventBool mClip;
    EventEnum<VAlign> mValign;
    EventEnum<HAlign> mHalign;
    EventFloat mVscale;
    EventFloat mHscale;
    // Background is deprecated since we have it in style
    EventBool mBackground;
    EventUnsigned mBackgroundColor;
    EventBool mChildrenFirst;  // Redraw children last
    CStyleLink mStyleLink;

    unsigned int button_mask; // temp hack to keep track on buttons
    SwsContext *mSwsContext;  // cached sws context
    SwsContext *mSwsContext2; // cached sws context (alpha channel)

};

typedef list<CLayerComponent *> CLayerComponentList;
typedef list<CLayerComponent *>::iterator CLayerComponentListIterator;

#endif // __COMPONENT_HH__
