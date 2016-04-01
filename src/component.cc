//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA,  2006.
//
#include "m1.hh"
#include "component.hh"

// Not needed since display component is virtual and cannot be instantiated.
// BUT We need to inhertit the LayerComponent to build gui widget
// Now the LayerComponent do render all it's kids.
XOBJECT_TYPE_BOOTSTRAP(CLayerComponent); 


CLayerComponent::CLayerComponent(CExecutor* aExec,CBaseType* aType) :
    CExecutable(aExec,aType),
    mTop(this),
    mLeft(this),
    mHeight(this),
    mWidth(this),
    mContentHeight(this),
    mContentWidth(this),
    mActive(this),
    mEnabled(this),
    mTransparency(this),
    mMessageMask(this),
    mMessage(this),
    mClip(this),
    mValign(this),
    mHalign(this),
    mVscale(this),
    mHscale(this),
    mBackground(this),
    mBackgroundColor(this),
    mChildrenFirst(this),
    mStyleLink(this, aExec)
{
    // Setup and install a child array.
    CArrayType* t  = CArrayType::create(CLayerComponentType::singleton(), 0);
    CArray *a = new CArray(aExec, t, sizeof(CLayerComponent *), 0);

    put(aExec, "children", UArray(a));

    put(aExec, XINDEX(CLayerComponent, wantFocus), UFalse());
    put(aExec, XINDEX(CLayerComponent, exclusiveFocus), UFalse());
    
    mTop.putValue(aExec, 0.0);
    mLeft.putValue(aExec, 0.0);
    mHeight.putValue(aExec,-1.0);
    mWidth.putValue(aExec,-1.0);
    mContentHeight.putValue(aExec,-1.0);
    mContentWidth.putValue(aExec,-1.0);
    mActive.putValue(aExec, false);
    mEnabled.putValue(aExec,false);
    mTransparency.putValue(aExec, 0.0);
    mClip.putValue(aExec, false);
    mHalign.putValue(aExec,HALIGN_LEFT);
    mValign.putValue(aExec,VALIGN_TOP);
    mVscale.putValue(aExec, 1.0);
    mHscale.putValue(aExec, 1.0);
    mBackground.putValue(aExec, 0);
    mChildrenFirst.putValue(aExec, false);
    mBackgroundColor.putValue(aExec, 0);
    mMessageMask.putValue(aExec, 0);     // no messages received

    button_mask = 0;
    mSwsContext = NULL;
    mSwsContext2 = NULL;

    eventPut(aExec, XINDEX(CLayerComponent,top), &mTop);
    eventPut(aExec, XINDEX(CLayerComponent,left), &mLeft);
    eventPut(aExec, XINDEX(CLayerComponent,height), &mHeight);
    eventPut(aExec, XINDEX(CLayerComponent,width), &mWidth);
    eventPut(aExec, XINDEX(CLayerComponent,contentHeight), &mContentHeight);
    eventPut(aExec, XINDEX(CLayerComponent,contentWidth), &mContentWidth);
    eventPut(aExec, XINDEX(CLayerComponent,active), &mActive);
    eventPut(aExec, XINDEX(CLayerComponent,enabled), &mEnabled);
    eventPut(aExec, XINDEX(CLayerComponent,transparency), &mTransparency);
    eventPut(aExec, XINDEX(CLayerComponent,clip), &mClip);
    eventPut(aExec, XINDEX(CLayerComponent,hscale), &mHscale);
    eventPut(aExec, XINDEX(CLayerComponent,vscale), &mVscale);
    eventPut(aExec, XINDEX(CLayerComponent,valign),     &mValign);
    eventPut(aExec, XINDEX(CLayerComponent,halign),     &mHalign);
    eventPut(aExec, XINDEX(CLayerComponent,background), &mBackground);
    eventPut(aExec, XINDEX(CLayerComponent,backgroundColor), &mBackgroundColor);
    eventPut(aExec, XINDEX(CLayerComponent,childrenFirst), &mChildrenFirst);

    eventPut(aExec, XINDEX(CLayerComponent,messageMask), &mMessageMask);
    eventPut(aExec, XINDEX(CLayerComponent,message), &mMessage);
}

CLayerComponent::~CLayerComponent(void)
{
#ifdef USE_FFMPEG
    if (mSwsContext)
	sws_freeContext(mSwsContext);
    if (mSwsContext2)
	sws_freeContext(mSwsContext2);
#endif
    m1ReleaseArray(at(XINDEX(CLayerComponent, children)).arr);
}

//
// Check if scaling is needed
//
bool CLayerComponent::needScale(CRedrawContext* aContext)
{
    int iw = int(mContentWidth.value());
    int ih = int(mContentHeight.value());
    int ow = int(aContext->cWidth);
    int oh = int(aContext->cHeight);

    return ((iw != ow) || (ih != oh));
}

bool CLayerComponent::canScale(CRedrawContext* aContext)
{
#ifdef USE_FFMPEG
    return true;
#else
    return false;
#endif
}

// Return a SwsContext needed by sws_scale 
// if Always is true the always return a context even if the
// source and destination have the same area, this may be used to
// convert image formats.
SwsContext* CLayerComponent::cachedSwsContext(CRedrawContext* aContext,
					      int sfmt, int dfmt, bool aAlways)
{
#ifdef USE_FFMPEG
    int iw = int(mContentWidth.value());
    int ih = int(mContentHeight.value());
    int ow = int(aContext->cWidth);
    int oh = int(aContext->cHeight);

    if (!aAlways && (iw == ow) && (ih == oh))
	return NULL;
    mSwsContext = sws_getCachedContext(mSwsContext,
				       iw, ih, sfmt,
				       ow, oh, dfmt,
				       SWS_BICUBIC, NULL, NULL, NULL);
    return mSwsContext;
#else
    return NULL;
#endif
}

//
// IMPORTANT NOTE!
// Only the PIX_FMT_RGB32 is working for 32 bit mode with ffmpeg
// when building default (e.g without libswscale)
// this corresponds to PIX_FMT_BGRA on little endian and
//                     PIX_FMT_ARGB on big endian
// img_convert contains a bug that will let it loop until memory is
// full when other 32 bit formats are given!
//
int CLayerComponent::pixelType2PixelFormat(int aPixelType)
{
#ifdef USE_FFMPEG
    switch(aPixelType) {
    case EPX_FORMAT_RGB:      return PIX_FMT_RGB24;
    case EPX_FORMAT_BGR:      return PIX_FMT_BGR24;
    case EPX_FORMAT_RGBA:     return PIX_FMT_RGBA;
    case EPX_FORMAT_ARGB:     return PIX_FMT_ARGB;
    case EPX_FORMAT_BGRA:     return PIX_FMT_BGRA;
    case EPX_FORMAT_ABGR:     return PIX_FMT_ABGR;
    case EPX_FORMAT_565_LE:
#if BYTE_ORDER == BIG_ENDIAN
	return PIX_FMT_BGR565;
#else
	return PIX_FMT_RGB565;
#endif
    case EPX_FORMAT_565_BE:
#if BYTE_ORDER == BIG_ENDIAN
	return PIX_FMT_RGB565;
#else
	return PIX_FMT_BGR565;
#endif
#warning "TONY FIXME: EPX_FORMAT_A1R5G5B5 and EPX_FORMAT_X1R5G5B5 not declared. Wiring this as little-endian version. Please validate"
    case EPX_FORMAT_A1R5G5B5_LE:
    case EPX_FORMAT_X1R5G5B5_LE:
#if BYTE_ORDER == BIG_ENDIAN
	return PIX_FMT_RGB555;
#else
	return PIX_FMT_BGR555;
#endif
	/* no R5G5B5A1 version in ffmpeg */
    default: return PIX_FMT_NONE;
    }
#else
    return -1;
#endif
}

//
// Helper method to scale images
// if ScaleAlpha is true then alpha channel needs scaling 
// even if the sws_scaler does not support it
//
void CLayerComponent::scaleImage(epx_pixmap_t* aSrcImage, epx_pixmap_t* aDstImage, bool aScaleAlpha)
{
#ifdef USE_FFMPEG
    int       srcFmt = pixelType2PixelFormat(aSrcImage->pixel_format);
    int       dstFmt = pixelType2PixelFormat(aDstImage->pixel_format);
    u_int8_t* srcData[4];
    int       srcLineSize[4];
    u_int8_t* dstData[4];
    int       dstLineSize[4];

    // scale srcImage into dstImage
    mSwsContext = sws_getCachedContext(mSwsContext,
				       aSrcImage->width,
				       aSrcImage->height,
				       srcFmt,
				       aDstImage->width,
				       aDstImage->height,
				       dstFmt,
				       SWS_BICUBIC, NULL, NULL, NULL);
    srcData[0]     = aSrcImage->data;
    srcLineSize[0] = aSrcImage->bytes_per_row;

    dstData[0]     = aDstImage->data;
    dstLineSize[0] = aDstImage->bytes_per_row;

    sws_scale(mSwsContext, srcData, srcLineSize, 0,
	      aSrcImage->height, dstData, dstLineSize);
    
    if (aScaleAlpha) {
	epx_pixmap_t*  srcAlphaImage;
	epx_pixmap_t*  dstAlphaImage;
	// If alpha scaling is needed then produce an alpha image
	mSwsContext2 = sws_getCachedContext(mSwsContext2,
					    aSrcImage->width,
					    aSrcImage->height,
					    PIX_FMT_GRAY8,
					    aDstImage->width,
					    aDstImage->height,
					    PIX_FMT_GRAY8,
					    SWS_BICUBIC, NULL, NULL, NULL);

	if ((srcAlphaImage = epx_pixmap_create(aSrcImage->width,
					       aSrcImage->height,
					       EPX_FORMAT_ALPHA)) == 0)
	    return;
	if ((dstAlphaImage = epx_pixmap_create(aDstImage->width,
					       aDstImage->height,
					       EPX_FORMAT_ALPHA)) == 0) {
	    epx_pixmap_destroy(srcAlphaImage);	    
	    return;
	}
	// copy the alpha channel (treat it as PIX_FMT_GRAY8)
	epx_pixmap_copy_to(aSrcImage, srcAlphaImage);
	srcData[0]     = srcAlphaImage->data;
	srcLineSize[0] = srcAlphaImage->bytes_per_row;
	dstData[0]     = dstAlphaImage->data;
	dstLineSize[0] = dstAlphaImage->bytes_per_row;

	sws_scale(mSwsContext2, srcData, srcLineSize, 0,
		  srcAlphaImage->height, dstData, dstLineSize);

	// add the alpha channel to dstImage!
	epx_pixmap_copy_area(dstAlphaImage, aDstImage,
			     0, 0, 0, 0,
			     dstAlphaImage->width,
			     dstAlphaImage->height,
			     EPX_FLAG_SUM);
	epx_pixmap_destroy(srcAlphaImage);
	epx_pixmap_destroy(dstAlphaImage);
    }
#endif
}

//
// Check if the style structure has been updated 
//
bool CLayerComponent::styleChanged(void)
{
    return mStyleLink.styleUpdated();
}

void CLayerComponent::start(CExecutor* aExec)
{
    mStyleLink.update(aExec, true);
}

void CLayerComponent::execute(CExecutor* aExec)
{
    mStyleLink.update(aExec, false);
}

void CLayerComponent::post_execute(CExecutor* aExec)
{
}

//
// Init aContext to values of self. No clipping.
//
void CLayerComponent::initContext(CRedrawContext &aContext, epx_gc_t* aGc)
{
    float sv = scaleVertical();
    float sh = scaleHorizontal();

    aContext.mPixmap  = 0;
    aContext.mGc      = aGc;
    aContext.lTop     = top()    * sv;
    aContext.lLeft    = left()   * sh;
    aContext.lHeight  = height() * sv;
    aContext.lWidth   = width()  * sh;
    aContext.cHeight  = contentHeight() * sv;
    aContext.cWidth   = contentWidth()  * sh;
    aContext.mTransparency = transparency();
    aContext.mVscale  = sv;
    aContext.mHscale  = sh;
    epx_gc_set_fader_float(aGc, (1-aContext.mTransparency));

    epx_rect_set(&aContext.mClipRect, 
		 int(aContext.lLeft),
		 int(aContext.lTop),
		 int(aContext.lWidth),
		 int(aContext.lHeight));
}

//
// Intersect the clip of self with the clip of aContext
//
void CLayerComponent::intersectClip(CRedrawContext &aContext) 
{
    if (clip()) {
	// Note mLeftOffset and mTopOffset IS already updated at this point!
	epx_rect_t rect;
	epx_rect_set(&rect, 
		     int(aContext.lLeft), 
		     int(aContext.lTop),
		     int(aContext.lWidth),
		     int(aContext.lHeight));
	epx_rect_intersect(&aContext.mClipRect, &rect, &aContext.mClipRect);
    }
}

//
// Modify aContext with values of self.
//
void CLayerComponent::modifyContext(CRedrawContext &aContext)
{
    float sv = aContext.mVscale * scaleVertical();
    float sh = aContext.mHscale * scaleHorizontal();
    float t  = transparency();

    aContext.lTop    += top()*sv;
    aContext.lLeft   += left()*sh;
    aContext.lHeight = height()*sv;
    aContext.lWidth  = width()*sh;
    aContext.cHeight = contentHeight()*sv;
    aContext.cWidth  = contentWidth()*sh;

    if (t <= 0.0) t = 0.0;
    if (t >= 1.0) t = 1.0;

    //
    //  calulation: 1 - alpha1 * alpha2  =
    //  1-(1-t1)*(1-t2) = 1-(1-t1-t2+t1*t2) = t1+t2-t1*t2
    // 
    aContext.mTransparency = t+aContext.mTransparency-t*aContext.mTransparency;
    epx_gc_set_fader_float(aContext.mGc, (1-aContext.mTransparency));
    aContext.mHscale = sh;
    aContext.mVscale = sv;

    intersectClip(aContext);
}

// Draw a scaled image into aImage
void CLayerComponent::redrawScaled(CRedrawContext* aContext, epx_pixmap_t* aImage)
{
    //...
}

// The general layer has nothing to draw for it self
void CLayerComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    // noop
}


// Draw children list
void CLayerComponent::redrawChildren(CSystem* aSys, CRedrawContext *aContext)
{
    CArray* children = at(XINDEX(CLayerComponent, children)).arr;
    unsigned int vec_size = children->size();
    unsigned int i;

    for (i = 0; i < vec_size; ++i) {
	CObject* obj = children->at(i).o;
	CLayerComponent *comp = dynamic_cast<CLayerComponent *>(obj);
	if (!comp)
	    continue;
	comp->redrawComponent(aSys, aContext);
    }
}

// Redraw layer background if needed
void CLayerComponent::redrawLayer(CSystem* aSys, CRedrawContext *aContext)
{
    epx_gc_t* gc;
    u_int8_t fader;

    if (!aContext || !aContext->mPixmap)
	return;
    gc = aContext->mGc;
    fader = gc->fader_value;

    if (mBackground.value()) {
	epx_pixel_t color;
	float fWidth  = (aContext->lWidth <= 0.0)  ?
	    aContext->cWidth : aContext->lWidth;
	float fHeight = (aContext->lHeight <= 0.0) ? 
	    aContext->cHeight : aContext->lHeight;
	CStyle* style = mStyleLink.style();

	if (style)
	    color.px = style->backgroundColor();
	else
	    color.px = mBackgroundColor.value();
	color.a  = 255;

	if (fader != ALPHA_FACTOR_1)
	    color.a = (color.a * fader) >> 8;

	epx_gc_set_fill_color(gc, color);

	epx_gc_set_fill_style(gc, EPX_FILL_STYLE_BLEND);
	epx_pixmap_draw_rectangle(aContext->mPixmap, gc,
				  int(aContext->lLeft),
				  int(aContext->lTop),
				  int(fWidth),
				  int(fHeight));
    }
}



void CLayerComponent::redrawComponent(CSystem* aSys, CRedrawContext *aContext)
{
    CRedrawContext ctx;
    u_int8_t fader;
    epx_gc_t gc;
    float hoffs   = 0.0;
    float voffs   = 0.0;
    float hscale  = 1.0;
    float vscale  = 1.0;
    bool  need_clip;
    epx_rect_t save_clip;

    // Setup layer scaling, transparency and clipping
    if (!aContext)
	initContext(ctx, &gc);
    else {
	ctx = *aContext;
	modifyContext(ctx);
    }

    // If layer is transparent ignore (including children !)
    if ((fader = ctx.mGc->fader_value) == ALPHA_FACTOR_0)
	return;

    // Calculate the the alignment offsets
    switch(valign()) {
    case VALIGN_NONE: break;
    case VALIGN_TOP:  break;
    case VALIGN_BOTTOM:
	if (ctx.cHeight <= 0)
	    voffs = 0;
	else if (ctx.lHeight < 0)
	    voffs = ctx.cHeight;
	else
	    voffs = (ctx.lHeight - ctx.cHeight);
	break;
    case VALIGN_CENTER:
	if (ctx.cHeight <= 0)
	    voffs = 0;
	else if (ctx.lHeight < 0)
	    voffs = ctx.cHeight/2;
	else
	    voffs = (ctx.lHeight - ctx.cHeight)/2;
	break;
    case VALIGN_SCALE:
	vscale = (ctx.cHeight <= 0.0) ? 1.0 : ctx.lHeight / ctx.cHeight;
	break;
    }
    
    switch(halign()) {
    case HALIGN_NONE: break;
    case HALIGN_LEFT: break;
    case HALIGN_RIGHT: 
	if (ctx.cWidth <= 0)
	    hoffs = 0.0;
	else if (ctx.lWidth < 0)
	    hoffs = ctx.cWidth; 
	else
	    hoffs = (ctx.lWidth - ctx.cWidth);
	break;
    case HALIGN_CENTER:
	if (ctx.cWidth <= 0)
	    hoffs = 0;
	else if (ctx.lWidth < 0)
	    hoffs = (ctx.cWidth/2);
	else
	    hoffs = (ctx.lWidth - ctx.cWidth)/2;
	break;
    case HALIGN_SCALE:
	hscale = (ctx.cWidth <= 0.0) ? 1.0 : ctx.lWidth / ctx.cWidth;
	break;
    }

    // Update with align offsets
    ctx.lTop  += voffs;
    ctx.lLeft += hoffs;
    ctx.cWidth *= hscale;
    ctx.cHeight *= vscale;

    need_clip = clip();
    if (need_clip) {
	save_clip = ctx.mPixmap->clip;
	epx_pixmap_set_clip(ctx.mPixmap, &ctx.mClipRect);
    }

    if (mChildrenFirst.value()) {
	redrawChildren(aSys, &ctx);
	redrawLayer(aSys, &ctx);
	redraw(aSys, &ctx);
    }
    else {
	redrawLayer(aSys, &ctx);
	redraw(aSys, &ctx);
	redrawChildren(aSys, &ctx);
    }

    // Restore clip if needed
    if (need_clip)
	epx_pixmap_set_clip(ctx.mPixmap, &save_clip);
}




VAlign CLayerComponent::valign(void) 
{ 
    return mValign.value();
}

HAlign CLayerComponent::halign(void) 
{ 
    return mHalign.value();
}
