//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006.

//
// Epic screen component
//
#include "screen_component.hh"
#include "component.hh"
#include "m1vm.hh"

#define DEFAULT_SCREEN_RESOLUTION_X 75
#define DEFAULT_SCREEN_RESOLUTION_Y 75

XOBJECT_TYPE_BOOTSTRAP(CScreenComponent);

CScreenList m1_screenList;

// Return the default screen (now it's only the first screen)
// We could possible mark one screen as the default?
CScreenComponent* m1_default_screen()
{
    if (m1_screenList.empty())
	return NULL;
    return m1_screenList.front();
}

int m1_mark_screens(Mark_t aMark)
{
    CScreenList::iterator sp = m1_screenList.begin();
    int marked = 0;

    while(sp != m1_screenList.end()) {
	marked += (*sp)->mark(aMark);
	sp++;
    }    
    return marked;
}

CScreenComponent::CScreenComponent(CExecutor* aExec, CBaseType *aType) :
    CLayerComponent(aExec, aType),
    curWidth(0),
    curHeight(0),
    curBuf(0),
    mInGraphicsMode(false),
    mThread(0),
    mBackend(0),
    mWindow(0),
    mBackendType(this),
    mPixelType(EPX_FORMAT_RGBA),
    mPixelTypeString(this),
    mBytesPerPixel(0),
    mBytesPerLine(0),
    mMemorySize(0),
    mPixClock(this),
    mLeftMargin(this),
    mRightMargin(this),
    mUpperMargin(this),
    mLowerMargin(this),
    mHsyncLen(this),
    mVsyncLen(this),
    mSync(this),
    mVmode(this),
    mDoubleBuffer(this),
    mThreaded(this),
    mFrameRate(this),
    mFrameBufferDevice(this),
    mActiveDevice(this),

    mTvSystem(this),
    mTvOutputSignal(this),
    mTvScan(this),
    mTvDeDotCrawl(this),
    mSamm(this),
    mSetFFilter(this),
    mSetAdaptiveFFilter(this),
    mTuneFFilter(this),
    mTuneAdaptiveFFilter(this),
    mLcdScaling(this),
    mLcdMode(this),
    mLcdPanelID(this),
    mTvBrightness(this),
    mTvContrast(this),
    mTvSaturation(this),
    mTvTint(this),
    mTvHeight(this),
    mTvWidth(this),
    mTvTop(this),
    mTvLeft(this),
    mHeight2(this),
    mWidth2(this),
    mRefresh(this),
    mRefresh2(this),
    mPixelType2(EPX_FORMAT_RGBA),
    mPixelTypeString2(this),
    mRecorder(this)
{
    int i;
    mActiveDevice.putValue(aExec, "CRT+TV");
    mPixelTypeString.putValue(aExec, "argb");
    mBackendType.putValue(aExec, "auto");
    mFrameRate.putValue(aExec, 30.0);    // Default to 30 fps 
    mLastTime  = aExec->timeStamp();
    mDoubleBuffer.putValue(aExec,  true);
    mThreaded.putValue(aExec, false);
    for (i = 0; i < EPX_BUFFERS; i++)
	mBuffer[i] = NULL;
    mRedrawCount = 0UL;
    mRedrawTime  = 0ULL;
    mStatTime    = 0ULL;

    mPixClock.putValue(aExec, -1);
    mLeftMargin.putValue(aExec, -1);
    mRightMargin.putValue(aExec, -1);
    mUpperMargin.putValue(aExec, -1);
    mLowerMargin.putValue(aExec, -1);
    mHsyncLen.putValue(aExec, -1);
    mVsyncLen.putValue(aExec, -1);
    mSync.putValue(aExec, 0xFFFFFFFF);
    mVmode.putValue(aExec, 0xFFFFFFFF);

    eventPut(aExec,XINDEX(CScreenComponent, pixelType), &mPixelTypeString);
    eventPut(aExec,XINDEX(CScreenComponent, backendType), &mBackendType);
    eventPut(aExec,XINDEX(CScreenComponent, framebufferDevice), &mFrameBufferDevice);
    eventPut(aExec,XINDEX(CScreenComponent, activeDevice), &mActiveDevice);
    eventPut(aExec,XINDEX(CScreenComponent, doubleBuffer), &mDoubleBuffer);
    eventPut(aExec,XINDEX(CScreenComponent, threaded), &mThreaded);
    eventPut(aExec,XINDEX(CScreenComponent, frameRate), &mFrameRate);

    eventPut(aExec,XINDEX(CScreenComponent, pixClock), &mPixClock);
    eventPut(aExec,XINDEX(CScreenComponent, leftMargin), &mLeftMargin);
    eventPut(aExec,XINDEX(CScreenComponent, rightMargin), &mRightMargin);
    eventPut(aExec,XINDEX(CScreenComponent, upperMargin), &mUpperMargin);
    eventPut(aExec,XINDEX(CScreenComponent, lowerMargin), &mLowerMargin);
    eventPut(aExec,XINDEX(CScreenComponent, hsyncLen), &mHsyncLen);
    eventPut(aExec,XINDEX(CScreenComponent, vsyncLen),  &mVsyncLen);
    eventPut(aExec,XINDEX(CScreenComponent, sync),&mSync);

    eventPut(aExec,XINDEX(CScreenComponent, tvSystem), &mTvSystem);
    eventPut(aExec,XINDEX(CScreenComponent, tvOutputSignal), &mTvOutputSignal);
    eventPut(aExec,XINDEX(CScreenComponent, tvScan), &mTvScan);
    eventPut(aExec,XINDEX(CScreenComponent, tvDeDotCrawl), &mTvDeDotCrawl);
    eventPut(aExec,XINDEX(CScreenComponent, samm), &mSamm);
    eventPut(aExec,XINDEX(CScreenComponent, setFFilter), &mSetFFilter);
    eventPut(aExec,XINDEX(CScreenComponent, setAdaptiveFFilter), &mSetAdaptiveFFilter);
    eventPut(aExec,XINDEX(CScreenComponent, tuneFFilter), &mTuneFFilter);
    eventPut(aExec,XINDEX(CScreenComponent, tuneAdaptiveFFilter), &mTuneAdaptiveFFilter);
    eventPut(aExec,XINDEX(CScreenComponent, lcdScaling), &mLcdScaling);
    eventPut(aExec,XINDEX(CScreenComponent, lcdMode), &mLcdMode);
    eventPut(aExec,XINDEX(CScreenComponent, lcdPanelID), &mLcdPanelID);
    eventPut(aExec,XINDEX(CScreenComponent, tvBrightness), &mTvBrightness);
    eventPut(aExec,XINDEX(CScreenComponent, tvContrast), &mTvContrast);
    eventPut(aExec,XINDEX(CScreenComponent, tvSaturation), &mTvSaturation);
    eventPut(aExec,XINDEX(CScreenComponent, tvTint), &mTvTint);

    eventPut(aExec,XINDEX(CScreenComponent, tvHeight), &mTvHeight);
    eventPut(aExec,XINDEX(CScreenComponent, tvWidth), &mTvWidth);
    eventPut(aExec,XINDEX(CScreenComponent, tvTop), &mTvTop);
    eventPut(aExec,XINDEX(CScreenComponent, tvLeft), &mTvLeft);
    eventPut(aExec,XINDEX(CScreenComponent, height2), &mHeight2);

    eventPut(aExec,XINDEX(CScreenComponent, width2), &mWidth2);
    eventPut(aExec,XINDEX(CScreenComponent, refresh), &mRefresh);
    eventPut(aExec,XINDEX(CScreenComponent, refresh2), &mRefresh2);
    eventPut(aExec,XINDEX(CScreenComponent, pixelType2), &mPixelTypeString2);

    //    eventPut(aExec,XINDEX(CScreenComponent, vmode), &mVmode));
    eventPut(aExec,XINDEX(CScreenComponent, recorder), &mRecorder);


    // screenList will do an implicit retain on this!
    m1_screenList.push_back(this);
    retainThis();
}

CScreenComponent::~CScreenComponent()
{
    m1_screenList.remove(this);
}

// Try implement this in a nice way
// We may add a call to the EBackend to retreive this info
// plus have a manual value as override
int CScreenComponent::resolution_x(void)
{
    return DEFAULT_SCREEN_RESOLUTION_Y;
}

// Try implement this in a nice way
// We may add a call to the EBackend to retreive this info
// plus have a manual value as override
int CScreenComponent::resolution_y(void)
{
    return DEFAULT_SCREEN_RESOLUTION_X;
}

//
// Redraw state machine:
//
//  a) wait for old transfere to complete
//  b) wait for frame rate time
//  c) flip page 
//  d) redraw screen into buffer
//  e) start transfere buffer into off-screen 
//  f) goto a
//
void CScreenComponent::redraw(CSystem* aSys, CRedrawContext *aContext)
{
    CRedrawContext context;
    epx_gc_t gc;
    TimeStamp thisTime;

    // If not in graphics mode then skip
    if (!mInGraphicsMode)
	return;
    
    // If no pixmap can be found either in self or in context then ignore
    if ((!aContext || !aContext->mPixmap) && !mBuffer[0])
	return;

    // Wait for offscreen transfere to complete
    // We could try generate redraw EPixmap here
    if (mThread && (EThreadDrawCount(mThread) > 0)) {
	return;
    }

    thisTime = aSys->timeStamp();
    if ((thisTime - mLastTime) < (STAMP_SEC/mFrameRate.value()))
	return;

    // Flip page  (try do this in this thread?)
    if (mThread)
	EThreadSwap(mThread, mWindow);
    else
	epx_window_swap(mWindow);

    mLastTime = thisTime;
    if (!mStatTime) mStatTime = thisTime;
    mRedrawCount++;

    //
    // If a context was provided. Copy and modify it.
    //
    if (aContext) {
	context = *aContext;
	modifyContext(context); // Will intersect clip also.
    }
    else {
	epx_gc_init(&gc);
	initContext(context, &gc);
	context.mPixmap = mBuffer[curBuf];
	intersectClip(context);
    }

    if (background()) {
	epx_pixel_t color;
	color.px = mBackgroundColor.value();
	color.a = 255;  // FIXME?
	epx_pixmap_fill(context.mPixmap, color);
    }

    CLayerComponent::redrawChildren(aSys, &context);

    thisTime = aSys->timeStamp();
    mRedrawTime += (thisTime - mLastTime);
    if ((thisTime - mStatTime) > 500000) {
	float fps = (double(mRedrawCount)/
		     double(thisTime-mStatTime))*double(STAMP_SEC);
	float avg = (double(mRedrawTime)/
		     double(mRedrawCount))/double(STAMP_SEC);
	m1_main().m1FPS.putValue(NULL, fps);
	m1_main().m1AvgRedrawTime.putValue(NULL, avg);

	// fprintf(stderr, "FPS: %f,  AVG: %f\n", fps, avg);

	mRedrawCount = 0;
	mRedrawTime  = 0ULL;
	mStatTime    = thisTime;
    }

    {
	CVideoComponent* recorder = mRecorder.value();
	if (recorder != NULL)
	    recorder->writeFrame(context.mPixmap);
    }
    
    if (mThread) {
	EThreadPixmapDraw(mThread, context.mPixmap, mWindow, true,
			  int(context.lLeft), int(context.lTop),
			  0, 0, 
			  context.mPixmap->width, context.mPixmap->height);
	// Assume double (tripple) buffer
	// curBuf = (curBuf + 1) % EPIC_BUFFERS;
    }
    else
	epx_backend_pixmap_draw(context.mPixmap->backend, 
				context.mPixmap, 
				mWindow, 
#warning "TONY FIXME: OffScreen flag did not carry over from EBacmendPixmapDraw to epx_backend_pixmap_draw"
//				true,
				int(context.lLeft), int(context.lTop),
				0, 0, 
				context.mPixmap->width, context.mPixmap->height);
}

void CScreenComponent::setupEpxParam(epx_dict_t *aParam)
{
    epx_dict_set_string(aParam,(char*) "framebuffer_device", (char*) mFrameBufferDevice.value().c_str());
    epx_dict_set_string(aParam, (char*) "active_device", (char*) mActiveDevice.value().c_str());
    epx_dict_set_integer(aParam, (char*)"height", curHeight);
    epx_dict_set_integer(aParam, (char*)"width",  curWidth);
    epx_dict_set_integer(aParam, (char*)"pixclock", mPixClock.value());
    epx_dict_set_integer(aParam, (char*)"left_margin", mLeftMargin.value());
    epx_dict_set_integer(aParam, (char*)"right_margin", mRightMargin.value());
    epx_dict_set_integer(aParam, (char*)"upper_margin", mUpperMargin.value());
    epx_dict_set_integer(aParam, (char*)"lower_margin", mLowerMargin.value());
    epx_dict_set_integer(aParam, (char*)"hsync_len", mHsyncLen.value());
    epx_dict_set_integer(aParam, (char*)"vsync_len",  mVsyncLen.value());
    epx_dict_set_integer(aParam, (char*)"sync", mSync.value());
    epx_dict_set_integer(aParam, (char*)"vmode", mVmode.value());
    epx_dict_set_integer(aParam, (char*)"double_buffer", mDoubleBuffer.value());
    epx_dict_set_integer(aParam, (char*) "pixel_type", mPixelType);

    if (mTvSystem.assigned())
	epx_dict_set_string(aParam, (char*) "tv_system", (char *) mTvSystem.value().c_str());
    if (mTvOutputSignal.assigned())
	epx_dict_set_string(aParam, (char*) "tv_output_signal", (char *) mTvOutputSignal.value().c_str());
    if (mTvScan.assigned())
	epx_dict_set_string(aParam, (char*) "tv_scan", (char *) mTvScan.value().c_str());
    if (mTvDeDotCrawl.assigned())
	epx_dict_set_string(aParam, (char*) "tv_dedotcrawl", (char *) mTvDeDotCrawl.value().c_str());
    if (mSamm.assigned())
	epx_dict_set_string(aParam, (char*) "samm", (char *) mSamm.value().c_str());
    if (mSetFFilter.assigned())
	epx_dict_set_string(aParam, (char*) "tv_set_ff", (char *) mSetFFilter.value().c_str());
    if (mSetAdaptiveFFilter.assigned())
	epx_dict_set_string(aParam, (char*) "tv_set_adaptive_ffilter", (char *) mSetAdaptiveFFilter.value().c_str());
    if (mTuneFFilter.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_tune_ffilter",  mTuneFFilter.value());
    if (mTuneAdaptiveFFilter.assigned()) 
	epx_dict_set_integer(aParam, (char*) "tv_tune_adaptive_ffilter",  mTuneAdaptiveFFilter.value());
    if (mLcdScaling.assigned())
	epx_dict_set_string(aParam, (char*) "lcd_scaling", (char *) mLcdScaling.value().c_str());
    if (mLcdMode.assigned())
	epx_dict_set_string(aParam, (char*) "lcd_mode", (char *) mLcdMode.value().c_str());
    if (mLcdPanelID.assigned())
	epx_dict_set_integer(aParam, (char*) "lcd_panel_id",  mLcdPanelID.value());
    if (mTvBrightness.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_brightness",  mTvBrightness.value());
    if (mTvContrast.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_contrast",  mTvContrast.value());
    if (mTvSaturation.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_saturation",  mTvSaturation.value());
    if (mTvTint.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_tint",  mTvTint.value());
    if (mTvHeight.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_size_y",  mTvHeight.value());
    if (mTvWidth.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_size_x",  mTvWidth.value());
    if (mTvTop.assigned())
	epx_dict_set_integer(aParam, (char*)  "tv_position_y",  mTvTop.value());
    if (mTvLeft.assigned())
	epx_dict_set_integer(aParam, (char*) "tv_position_x",  mTvLeft.value());
    if (mHeight2.assigned())
	epx_dict_set_integer(aParam, (char*) "height2",  mHeight2.value());
    if (mWidth2.assigned())
	epx_dict_set_integer(aParam, (char*) "width2",  mWidth2.value());
    if (mRefresh.assigned())
	epx_dict_set_integer(aParam, (char*) "refresh",  mRefresh.value());
    if (mRefresh2.assigned())
	epx_dict_set_integer(aParam, (char*) "refresh2",  mRefresh2.value());
    if (mPixelTypeString2.assigned())
	epx_dict_set_integer(aParam, (char*) "pixel_type2", mPixelType2);
}


void CScreenComponent::enterGraphicsMode(CExecutor* aExec)
{
    int i;
    char* name;
    epx_dict_t* param;

    //     fprintf(stderr, "ENTER GRAPHIC MODE Width[%f], Height[%f]\n", 
    // 	    mWidth.value(), mHeight.value());

    curWidth  = (int) mWidth.value();
    curHeight = (int) mHeight.value();
    // Load backend parameters
    param = epx_dict_create();
    setupEpxParam(param);

    if ((mBackendType.value()) == "auto") {
	int i = 0;
	while((name = epx_backend_name(i)) != NULL) {
	    if ((mBackend = epx_backend_create((char *)name, param)) != NULL)
		break;
	    i++;
	}
    }
    else {
	name = (char*) mBackendType.value().c_str();
	mBackend = epx_backend_create((char *) name, param);
    }

    if (mBackend == NULL) {
	puts("EpxScreenComponent: no working backend found");
	if (mBackendType.value() == "x11")
	    puts("EpxScreenComponent: How about make WITH_X11=1");
	puts("Failed to setup epx backend.");
	exit(0);
    }

    epx_dict_destroy(param);

    //     if (mEvent != NULL)
    // 	mEvent->SetBackend(mBackend);

    mWindow = epx_window_create(50,50, curWidth, curHeight);
    mWindow->mask = EPX_EVENT_BUTTON_PRESS|EPX_EVENT_BUTTON_RELEASE|
	EPX_EVENT_POINTER_MOTION|EPX_EVENT_KEY_PRESS|EPX_EVENT_KEY_RELEASE;
    epx_backend_window_attach(mBackend, mWindow);

    for (i = 0; i < EPX_BUFFERS; i++) {
	mBuffer[i] = epx_pixmap_create(curWidth, curHeight, mPixelType);
	epx_backend_pixmap_attach(mBackend, mBuffer[i]);
    }
    curBuf = 0;

    // re-read mPixelType, if epixmap_attach has some other hard-coded idea.
    mPixelType     = mBuffer[0]->pixel_format;  
    mBytesPerLine = (int) mWidth.value() * EPX_PIXEL_BYTE_SIZE(mPixelType);
#ifdef USE_ETHREAD
    if (mThreaded.value())
	mThread = EThreadCreate(mBackend);
#endif

    mInGraphicsMode = true;
    mActive.putValue(aExec, true);
}

void CScreenComponent::leaveGraphicsMode(CExecutor* aExec)
{
    int i;

    if (!mInGraphicsMode)
	return;

    if (mThread) {
	EThreadDestroy(mThread);
	mThread = NULL;
    }

    for (i = 0; i < EPX_BUFFERS; i++) 
	epx_pixmap_detach(mBuffer[i]);
    epx_window_detach(mWindow);

    for (i = 0; i < EPX_BUFFERS; i++) {
	epx_pixmap_destroy(mBuffer[i]);
	mBuffer[i] = NULL;
    }

    epx_window_destroy(mWindow);
    mWindow = NULL;

    epx_backend_destroy(mBackend);
    mBackend = NULL;
    mInGraphicsMode = false;
    mActive.putValue(aExec, false);
}

void CScreenComponent::changeResolution(void)
{
    int i;
    curWidth  = (int) mWidth.value();
    curHeight = (int) mHeight.value();

    if (!mInGraphicsMode)
	return;

    for (i = 0; i < EPX_BUFFERS; i++)
	epx_pixmap_detach(mBuffer[i]);

    epx_window_detach(mWindow);

    for (i = 0; i < EPX_BUFFERS; i++) {
	epx_pixmap_destroy(mBuffer[i]);
	mBuffer[i] = NULL;
    }

    epx_window_destroy(mWindow);

    mWindow = epx_window_create(50,50, curWidth, curHeight);
    mWindow->mask = EPX_EVENT_BUTTON_PRESS|EPX_EVENT_BUTTON_RELEASE|
	EPX_EVENT_POINTER_MOTION|EPX_EVENT_KEY_PRESS|EPX_EVENT_KEY_RELEASE;


    epx_backend_window_attach(mBackend, mWindow);
    for (i = 0; i < EPX_BUFFERS; i++) {
	mBuffer[i] = epx_pixmap_create(curWidth, curHeight, mPixelType);
	epx_backend_pixmap_attach(mBackend, mBuffer[i]);
    }
    // re-read mPixelType, if epixmap_attach has some other hard-coded idea.
    mPixelType     = mBuffer[0]->pixel_format;
    mBytesPerLine = (int) mWidth.value() * EPX_PIXEL_BYTE_SIZE(mPixelType);
}


void CScreenComponent::setup(CExecutor* aExec, bool aStart)
{
    bool needReset = false;
    bool resolutionChanged = false;

    if (mPixelTypeString.assigned()) {
	string typeString = mPixelTypeString.value();
	int res;

	if ((res = epx_pixel_format_from_name((char*)typeString.c_str())) == -1)
	    DBGFMT("CScreenComponent::execute(): pixel_type [%s] not supported",
		   typeString.c_str());
	else if (mPixelType != res) {
	    mPixelType = res;
	    needReset = true;
	}
	if (aStart) 
	    mPixelTypeString.cancel(aExec);
	DBGFMT("CScreenComponent::execute(): PixelType[%x]", mPixelType);
    }

    if (mPixelTypeString2.assigned()) {
	int res;

	if ((res = epx_pixel_format_from_name((char*)mPixelTypeString2.value().c_str())) == -1)
	    DBGFMT("CScreenComponent::execute(): pixel_type2 [%s] not supported",  mPixelTypeString2.value().c_str());
	else if (mPixelType2 != res) {
	    mPixelType2 = res;
	    needReset = true;
	}

	if (aStart) {
	    mPixelTypeString.cancel(aExec);
	    mPixelTypeString2.cancel(aExec);
	}
	DBGFMT("CScreenComponent::execute(): PixelType[%x]", mPixelType);
    }

    if (mThreaded.assigned() || mDoubleBuffer.assigned())
	needReset = true;

    //
    // Check if any of the timing parameters are updated
    // FIXME: maybe checck the needReset and skip?
    //
    if (mBackend != NULL &&  
	(mActiveDevice.assigned() ||
	 mPixelTypeString.assigned() || 
	 mPixClock.assigned() || 
	 mLeftMargin.assigned() ||
	 mRightMargin.assigned() ||
	 mUpperMargin.assigned() ||
	 mLowerMargin.assigned() ||
	 mHsyncLen.assigned() ||
	 mVsyncLen.assigned() ||
	 mSync.assigned() ||
	 mVmode.assigned() ||
	 mTvSystem.assigned() ||
	 mTvOutputSignal.assigned() ||
	 mTvScan.assigned() ||
	 mTvDeDotCrawl.assigned() ||
	 mSamm.assigned() ||
	 mSetFFilter.assigned() ||
	 mSetAdaptiveFFilter.assigned() ||
	 mTuneFFilter.assigned() ||
	 mTuneAdaptiveFFilter.assigned() ||
	 mLcdScaling.assigned() ||
	 mLcdMode.assigned() ||
	 mLcdPanelID.assigned() ||
	 mTvBrightness.assigned() ||
	 mTvContrast.assigned() ||
	 mTvSaturation.assigned() ||
	 mTvTint.assigned() ||
	 mTvTop.assigned() ||
	 mTvLeft.assigned() ||
	 mTvHeight.assigned() ||
	 mTvWidth.assigned() ||
	 mHeight2.assigned() ||
	 mWidth2.assigned() ||
	 mRefresh.assigned() ||
	 mRefresh2.assigned() ||
	 mPixelTypeString2.assigned())) {

	epx_dict_t *param = epx_dict_create();
	setupEpxParam(param);
	epx_backend_adjust(mBackend, param);
	epx_dict_destroy(param);
    }

    if (mHeight.assigned() || mWidth.assigned()) {
	if ((curWidth != (int) mWidth.value()) ||
	    (curHeight != (int) mHeight.value())) 
	    resolutionChanged = true;

	if (aStart) {
	    mHeight.cancel(aExec);
	    mWidth.cancel(aExec);
	}
    }

    if (mInGraphicsMode) {
	if (needReset)
	    leaveGraphicsMode(aExec);
	else if (resolutionChanged)
	    changeResolution();
    }
    if (!mInGraphicsMode)
	enterGraphicsMode(aExec);
}

void CScreenComponent::stop(CExecutor* aExec)
{
    // fprintf(stderr, "SCREEN STOP\n");
    if (mInGraphicsMode)
	leaveGraphicsMode(aExec);
    CLayerComponent::stop(aExec);
}

void CScreenComponent::start(CExecutor* aExec)
{
    // fprintf(stderr, "SCREEN START\n");
    setup(aExec, true);
}

void CScreenComponent::execute(CExecutor* aExec) 
{
    // fprintf(stderr, "SCREEN EXECUTE\n");
    setup(aExec, false);
}
