//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//

#include "m1.hh"
#include "dds_component.hh"


#include "epx.h"
#include <math.h> // for nearbyintf() rounding function. Link with -m.

XOBJECT_TYPE_BOOTSTRAP(CDDSComponent);

CDDSComponent::CDDSComponent(CExecutor* aExec,CBaseType *aType):
    CLayerComponent(aExec,aType),
    mDDS(0),
    mDDSFile(this),
    mCurrentFrame(0),
    mValue(this),
    mFrameStart(this),
    mFrameStop(this)
{
    mFrameStart.putValue(aExec, 0.0);
    mFrameStop.putValue(aExec, 1.0);
    eventPut(aExec, XINDEX(CDDSComponent,ddsFile), &mDDSFile);
    eventPut(aExec, XINDEX(CDDSComponent,value), &mValue);
    eventPut(aExec, XINDEX(CDDSComponent,frameStart), &mFrameStart);
    eventPut(aExec, XINDEX(CDDSComponent,frameStop), &mFrameStop);
}

CDDSComponent::~CDDSComponent(void)
{
    DBGFMT("CDDSComponent::~CDDSComponent(): Called");
}

void CDDSComponent::loadDDS(CExecutor* aExec, bool aStart) 
{
    if (mDDSFile.assigned() && mDDSFile.value() != "") {
// 	if (mDDS) // Deref old file, if present.
// 	    CDDSFileFactory::unload(mDDS);
	
	if (!(mDDS = CDDSFileFactory::load(mDDSFile.value().c_str()))) {
	    DBGFMT("CDDSComponent::loadDDS(): Could not load DDS file [%s]",  mDDSFile.value().c_str());
	    return;
	}

	if (mValue.value() < 0.0)
	    mValue.putValue(aExec, 0);
	if (mValue.value() > 1.0)
	    mValue.putValue(aExec, 1.0);

	mCurrentFrame = static_cast<int> (nearbyintf(((mFrameStop.value() - mFrameStart.value()) * mValue.value() + mFrameStart.value()) * (mDDS->frameCount() - 1)));
//	printf("CDDSComponent::loadDDS() ddsFile[%s] loaded. Frame[%d]\n", mDDSFile.value().c_str(), mCurrentFrame);

	mContentHeight.putValue(aExec, mDDS->height());
	mContentWidth.putValue(aExec, mDDS->width());

	if (aStart) mDDSFile.cancel(aExec);
    }
}


void CDDSComponent::start(CExecutor* aExec)
{
    loadDDS(aExec, true);
}


void CDDSComponent::execute(CExecutor* aExec) 
{
    loadDDS(aExec, false);

    // check assigned, to allow derived components to display directly
    if (mDDS && mValue.assigned()) { 
	float v = mValue.value();

	if (v < 0.0) 
	    v = 0.0;

	if (v > 1.0) 
	    v = 1.0;

	mCurrentFrame = int(nearbyintf(((mFrameStop.value() - mFrameStart.value()) * v +  mFrameStart.value()) * (mDDS->frameCount() - 1)));
//   	printf("(Stop[%f] - Start[%f]) * v[%f] + Start[%f] * FrameCount[%d] - CurrentFrame[%d]\n", 
//  	       mFrameStop.value(),
//  	       mFrameStart.value(),
//   	       v,
//  	       mFrameStart.value(),
//  	       mDDS->frameCount(),
//   	       mCurrentFrame
//   	       );
    }
}

void CDDSComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    // CLayerComponent::redraw(aSys, aContext);
    if (!mDDS)
	return;

    if (!aContext || !aContext->mPixmap) {
	printf("CDDSComponent::redraw(): No context or pixmap provided.\n");
	return;
    }

    if (needScale(aContext) && canScale(aContext)) {
	epx_pixmap_t*  sImage;    // source image
	epx_pixmap_t*  dImage;    // destination image

	// Render dds in temoprary image
	if ((sImage = epx_pixmap_create(mDDS->width(),mDDS->height(),
					EPX_FORMAT_RGBA)) == NULL)
	    goto unscaled;
	    
	// Draw DDS into new pixmap
	epx_pixmap_fill(sImage, epx_pixel_transparent);

	epx_anim_copy_frame(sImage, aContext->mGc, 
			    0, 0,
			    mDDS->width(), mDDS->height(),
			    mDDS->pixelType(),
			    (epx_anim_pixels_t*)(mDDS->pixmap(0)),
			    (epx_anim_pixels_t*)(mDDS->pixmap(mCurrentFrame)));

	// Create the destination image
	if ((dImage = epx_pixmap_create(int(aContext->cWidth),
					int(aContext->cHeight),
					aContext->mPixmap->pixel_format)) == NULL) {
	    epx_pixmap_destroy(sImage);
	    goto unscaled;
	}

	// Scale the image & alpha channel
	scaleImage(sImage, dImage, true);

	epx_pixmap_fade_area(dImage, aContext->mPixmap,
			     aContext->mGc->fader_value,
			     0, 0,
			     int(aContext->lLeft),
			     int(aContext->lTop),
			     dImage->width, dImage->height);
	epx_pixmap_destroy(sImage);
	epx_pixmap_destroy(dImage);
	return;
    }

unscaled:
    epx_anim_draw_frame(aContext->mPixmap, aContext->mGc, 
			int(aContext->lLeft), int(aContext->lTop),
			mDDS->width(), mDDS->height(),
			mDDS->pixelType(),
			(epx_anim_pixels_t*)(mDDS->pixmap(0)),
			(epx_anim_pixels_t*)(mDDS->pixmap(mCurrentFrame)));
}


