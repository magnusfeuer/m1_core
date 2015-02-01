//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __SCREEN_COMPONENT_H__
#define __SCREEN_COMPONENT_H__

#include "epic.h"
#include "epic_thread.hh"
#include "component.hh"
#include "video_component.hh"

#ifndef DARWIN
#include <linux/fb.h>
#endif

#ifdef USE_ETHREAD
#define EPIC_BUFFERS 2
#else
#define EPIC_BUFFERS 1
#endif


class CScreenComponent;

extern CScreenComponent* m1_default_screen();
extern unsigned long m1_mark_screens(unsigned int aMark);

class CScreenComponent: public CLayerComponent
{
public:
    XDERIVED_OBJECT_TYPE(CScreenComponent,CLayerComponent,"Screen",
			 "Screen component",
			 (CScreenComponent_pixelType,
			  CScreenComponent_backendType,
			  CScreenComponent_framebufferDevice,
			  CScreenComponent_activeDevice,
			  CScreenComponent_doubleBuffer,
			  CScreenComponent_threaded,
			  CScreenComponent_frameRate,
			  CScreenComponent_pixClock,
			  CScreenComponent_leftMargin,
			  CScreenComponent_rightMargin,
			  CScreenComponent_upperMargin,
			  CScreenComponent_lowerMargin,
			  CScreenComponent_hsyncLen,
			  CScreenComponent_vsyncLen,
			  CScreenComponent_sync,
			  CScreenComponent_recorder,
			  
			  // Viafb specific things
			  CScreenComponent_tvSystem,
			  CScreenComponent_tvOutputSignal,
			  CScreenComponent_tvScan,
			  CScreenComponent_tvDeDotCrawl,
			  CScreenComponent_samm,
			  CScreenComponent_setFFilter,
			  CScreenComponent_setAdaptiveFFilter,
			  CScreenComponent_tuneFFilter,
			  CScreenComponent_tuneAdaptiveFFilter,
			  CScreenComponent_lcdScaling,
			  CScreenComponent_lcdMode,
			  CScreenComponent_lcdPanelID,
			  CScreenComponent_tvBrightness,
			  CScreenComponent_tvContrast,
			  CScreenComponent_tvSaturation,
			  CScreenComponent_tvTint,
			  CScreenComponent_tvHeight,
			  CScreenComponent_tvWidth,
			  CScreenComponent_tvTop,
			  CScreenComponent_tvLeft,
			  CScreenComponent_height2,
			  CScreenComponent_width2,
			  CScreenComponent_refresh,
			  CScreenComponent_refresh2,
			  CScreenComponent_pixelType2),
			 XFIELD(CScreenComponent,Q_PUBLIC,pixelType,
				input_string_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,backendType,
				input_string_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,framebufferDevice,
				input_string_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,activeDevice,
				input_string_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,doubleBuffer,
				input_bool_type(),
				"Use off screen buffer"),
			 XFIELD(CScreenComponent,Q_PUBLIC,threaded,
				input_bool_type(),
				"Use threaded transfere of off screen buffer"),
			 XFIELD(CScreenComponent,Q_PUBLIC,frameRate,
				input_float_type(),
				"Set wanted frame rate"),
			 XFIELD(CScreenComponent,Q_PUBLIC,pixClock,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,leftMargin,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,rightMargin,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,upperMargin,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,lowerMargin,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,hsyncLen,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,vsyncLen,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,sync,
				input_signed_type(),
				""),
			 XFIELD(CScreenComponent,Q_PUBLIC,recorder, 
				CEventType::create(CVideoComponent::CVideoComponentType::singleton(),E_INPUT),
				""),
			  // Viafb specific settings
			 XFIELD(CScreenComponent,Q_PUBLIC,tvSystem,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvOutputSignal,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvScan,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvDeDotCrawl,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,samm,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,setFFilter,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,setAdaptiveFFilter,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tuneFFilter, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tuneAdaptiveFFilter, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,lcdScaling,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,lcdMode,input_string_type(),""),
			 XFIELD(CScreenComponent,Q_PUBLIC,lcdPanelID,  input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvBrightness, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvContrast, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvSaturation, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvTint, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvHeight, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvWidth, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvTop, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,tvLeft, input_signed_type(), ""),

			 XFIELD(CScreenComponent,Q_PUBLIC,height2, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,width2, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,refresh, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,refresh2, input_signed_type(), ""),
			 XFIELD(CScreenComponent,Q_PUBLIC,pixelType2,input_string_type(),"")
    );

public:
    CScreenComponent(CExecutor* aExec,
		     CBaseType *aType = CScreenComponentType::singleton());
    ~CScreenComponent(void); 

    void start(CExecutor* aExec);
    void stop(CExecutor* aExec);

    void execute(CExecutor* aExec);
    void redraw(CSystem* aSys, CRedrawContext *aContext = 0);
    void leaveGraphicsMode(CExecutor* aExec);
    void enterGraphicsMode(CExecutor* aExec);
    void changeResolution(void);

    EBackend* backend(void) { return mBackend; }
//    EPixmap *pixmap(void) { return mBuffer; }

    // Return screen resolution in DPI
    int resolution_x(void);
    int resolution_y(void);
private:
    void setupEpicParam(EDict *aParam);
    void setup(CExecutor* aExec, bool aStart);
    int curWidth;
    int curHeight;
    int curBuf;
    bool mInGraphicsMode;
    EThread*  mThread;
    EBackend* mBackend;
    EPixmap*  mBuffer[EPIC_BUFFERS];
    EWindow*  mWindow;
    
    EventString mBackendType;
    int mPixelType;
    EventString mPixelTypeString; 
    int mBytesPerPixel;
    int mBytesPerLine;
    TimeStamp mLastTime;
    unsigned long mRedrawCount;
    TimeStamp     mRedrawTime;
    TimeStamp     mStatTime;
    //
    // Framebuffer specific stuff!
    //
    int mMemorySize;
    EventSigned mPixClock;
    EventSigned mLeftMargin;
    EventSigned mRightMargin;
    EventSigned mUpperMargin;
    EventSigned mLowerMargin;
    EventSigned mHsyncLen;
    EventSigned mVsyncLen;
    EventUnsigned mSync;
    EventUnsigned mVmode;
    EventBool mDoubleBuffer; 
    EventBool mThreaded;
    EventFloat mFrameRate;
    EventString mFrameBufferDevice; // FB specific
    EventString mActiveDevice;      // FB specific

    // Viafb specific settings
    EventString mTvSystem;
    EventString mTvOutputSignal;
    EventString mTvScan;
    EventString mTvDeDotCrawl;
    EventString mSamm;
    EventString mSetFFilter;
    EventString mSetAdaptiveFFilter;
    EventSigned mTuneFFilter;
    EventSigned mTuneAdaptiveFFilter;
    EventString mLcdScaling;
    EventString mLcdMode;
    EventSigned mLcdPanelID;
    EventSigned mTvBrightness;
    EventSigned mTvContrast;
    EventSigned mTvSaturation;
    EventSigned mTvTint;
    EventSigned mTvHeight;
    EventSigned mTvWidth;
    EventSigned mTvTop;
    EventSigned mTvLeft;
    EventSigned mHeight2;
    EventSigned mWidth2;
    EventSigned mRefresh;
    EventSigned mRefresh2;
    int mPixelType2;
    EventString mPixelTypeString2;

    EventObject<CVideoComponent *> mRecorder; // Recorder to use
};

typedef list<CScreenComponent*> CScreenList;

#endif // __SCREEN_COMPONENT__
