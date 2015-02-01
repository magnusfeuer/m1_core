//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#include "m1.hh"
#include "shape_component.hh"


#include "epic.h"
#include <math.h> // for nearbyintf() rounding function. Link with -m.

XOBJECT_TYPE_BOOTSTRAP(CShapeComponent);


CShapeComponent::CShapeComponent(CExecutor* aExec, CBaseType *aType):
    CLayerComponent(aExec, aType),
    mShape(this),
    mFill(this),
    mFillColor(this),
    mBorderWidth(this),
    mBorderColor(this)
{
    eventPut(aExec,XINDEX(CShapeComponent, shape),       &mShape);
    eventPut(aExec,XINDEX(CShapeComponent, fill),        &mFill);
    eventPut(aExec,XINDEX(CShapeComponent, fillColor),   &mFillColor);
    eventPut(aExec,XINDEX(CShapeComponent, borderWidth), &mBorderWidth);
    eventPut(aExec,XINDEX(CShapeComponent, borderColor), &mBorderColor);
}

CShapeComponent::~CShapeComponent(void)
{
}

void CShapeComponent::execute(CExecutor* aExec)
{
}


void CShapeComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    u_int8_t fader;
    EPixel_t bcolor;
    EPixel_t fcolor;
    EGc* gc;

    if (!aContext || !aContext->mPixmap) {
	printf("CPlotComponent::redraw(): No context or pixmap provided.\n");
	return;
    }
    gc = aContext->mGc;
    fader = gc->fader_value;

    if (mFill.value())
	EGcSetFillStyle(gc, EPIC_FILL_STYLE_BLEND|EPIC_FILL_STYLE_AALIAS);
    else
	EGcSetFillStyle(gc, EPIC_FILL_STYLE_NONE);

    EGcSetBorderWidth(gc, mBorderWidth.value());
    EGcSetBorderStyle(gc, EPIC_FILL_STYLE_BLEND|EPIC_BORDER_STYLE_AALIAS);

    bcolor.px = mBorderColor.value();
    bcolor.a  = 255;  // FIXME;

    fcolor.px = mFillColor.value();
    fcolor.a  = 255;  // FIXME

    if (fader != ALPHA_FACTOR_1) {
	bcolor.a = (bcolor.a * fader) >> 8;
	fcolor.a   = (fcolor.a * fader) >> 8;
    }
    EGcSetFillColor(gc, fcolor);
    EGcSetBorderColor(gc, bcolor);
    switch(mShape.value()) {
    case SHAPE_NONE:
	break;
    case SHAPE_RECTANGLE:
	EPixmapDrawRectangle(aContext->mPixmap, gc,
			     int(aContext->lLeft), 
			     int(aContext->lTop),
			     int(aContext->cWidth),
			     int(aContext->cHeight));
	break;
    case SHAPE_ELLIPSE: {
	float w = aContext->cWidth;
	float h = aContext->cHeight;
	float x = aContext->lLeft;
	float y = aContext->lTop;
	EPixmapDrawEllipse(aContext->mPixmap, gc, (int)x,(int)y,
			   (int)w,(int)h);
	break;
    }

    case SHAPE_TRIANGLE: {
	float w = aContext->cWidth;
	float h = aContext->cHeight;
	float l = aContext->lLeft;
	float y0 = aContext->lTop;
	float y1 = y0 + h- 1;
	float y2 = y1;
	float x0 = l + (w/2);
	float x1 = l;
	float x2 = l + w -1;
	EPixmapDrawTriangle(aContext->mPixmap, 
			    gc,
			    (int)x0, (int)y0, 
			    (int)x1, (int)y1,
			    (int)x2, (int)y2);
	break;
    }

    }
}


