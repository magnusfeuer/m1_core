/*
 *  Mac Os X display driver
 *
 * Where:
 *  /System/Library/Frameworks/ApplicationServices.framework/Frameworks/
 *    CoreGraphics.framework/Headers
 */
#include <Carbon/Carbon.h>
#include <machine/endian.h>
#include <pthread.h>
#include <unistd.h>
#include <assert.h>

#include "epic.h"


#define kEpicEventNew 1
#define kEpicEventDel 2

typedef struct {
    EBackend b;
    int     ofd;                /* pipe output signal */
    MenuRef menu;
    unsigned short modstate;    /* modifier state */
    unsigned short grab_key;    /* grab key */
} CarbonBackend;

typedef struct {
    Rect             winBounds;
    WindowClass      winClass;
    WindowAttributes winAttrs;
    WindowRef        winRef;
    GWorldPtr        macPort;
} CarbonWindow;

typedef struct {
    CGBitmapInfo bitmapinfo;
    CGColorSpaceRef colorspace;
    CGDataProviderRef provider;
} CarbonPixels;


EBackend* carbon_init(EDict* param);

static int carbon_finish(EBackend*);
static int carbon_pic_attach(EBackend*, EPixmap* pic);
static int carbon_pic_detach(EBackend*, EPixmap* pic);
static int carbon_pic_draw(EBackend*, EPixmap* pic, EWindow* win,
			   int off_screen,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height);
static int carbon_win_attach(EBackend*, EWindow* win);
static int carbon_win_detach(EBackend*, EWindow* win);
static int carbon_win_swap(EBackend*, EWindow* win);
static EHANDLE_T carbon_evt_attach(EBackend*);
static int carbon_evt_detach(EBackend*);
static int carbon_evt_read(EBackend*, EEvent*,u_int16_t mask);
static int carbon_adjust(EBackend *backend, EDict* param);

static EPicCallbacks carbon_callbacks =
{
    carbon_finish,
    carbon_pic_attach,
    carbon_pic_detach,
    carbon_pic_draw,
    carbon_win_attach,
    carbon_win_detach,
    carbon_evt_attach,
    carbon_evt_detach,
    carbon_evt_read,
    carbon_adjust,
    carbon_win_swap
};


/* util */

#define kGenericRGBProfilePathStr       "/System/Library/ColorSync/Profiles/Generic RGB Profile.icc"

static pthread_t carbon_thr;
static pthread_mutex_t carbon_lock;

static pascal OSStatus EPicAppEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData);

static pascal OSStatus EPicWindowEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData);
//----------------------------------------------------------------------------
DEFINE_ONE_SHOT_HANDLER_GETTER( EPicWindowEventHandler )

DEFINE_ONE_SHOT_HANDLER_GETTER( EPicAppEventHandler )

//----------------------------------------------------------------------------

#ifdef debug
char* id_to_string(UInt32 w, char* str, char** end_ptr)
{
    int i = sizeof(w);

    str[i] = '\0';
    while(i--) {
	str[i] = w;
	w >>= 8;
    }
    if (end_ptr != NULL)
	*end_ptr = str+sizeof(w)-1;
    return str;
}

void dbg_print_event(char* label, EventRef theEvent)
{
    UInt32	 eventClass;
    UInt32	 eventKind;		
    char e_class[5];

    eventClass = GetEventClass(theEvent);
    eventKind  = GetEventKind(theEvent);
    id_to_string(eventClass, e_class, NULL);

    EDBGFMT("%s: class='%s', kind=%lu", label, e_class, eventKind);
}
#else
#define dbg_print_event(label,inEvent) do {} while(0)
#endif

static SInt32 gNumberOfRunningThreads = 0;
// This variable must be maintained by your thread scheduling
// code to accurately reflect the number of threads that are
// ready and need time for computation.

static EventHandlerUPP gQuitEventHandlerUPP;   // -> QuitEventHandler

static pascal OSStatus QuitEventHandler(EventHandlerCallRef inHandlerCallRef,
                                        EventRef inEvent, void *inUserData)
// This event handler is used to override the kEventClassApplication/
// kEventAppQuit event while inside our event loop (EventLoopEventHandler).
// It simply calls through to the next handler and, if that handler returns
// noErr (indicating that the application is doing to quit), it sets
// a Boolean to tell our event loop to quit as well.
{
    OSStatus err;

    err = CallNextEventHandler(inHandlerCallRef, inEvent);
    if (err == noErr) {
        *((Boolean *) inUserData) = true;
    }
    return err;
}


static EventHandlerUPP gEventLoopEventHandlerUPP;   // -> EventLoopEventHandler

static pascal OSStatus EventLoopEventHandler(EventHandlerCallRef inHandlerCallRef,
                                             EventRef inEvent, void *inUserData)
{
#pragma unused(inEvent)
    OSStatus        err;
    OSStatus        junk;
    EventHandlerRef installedHandler;
    EventTargetRef  theTarget;
    EventRef        theEvent;
    EventTimeout    timeToWaitForEvent;
    Boolean         quitNow;
    static const EventTypeSpec eventSpec = {kEventClassApplication, kEventAppQuit};
    (void) inHandlerCallRef;
    (void) inUserData;
    quitNow = false;

    EDBGFMT("EventLoopEventHandler: started");
    dbg_print_event("EventLoopEventHandler", inEvent);

    err = InstallEventHandler(GetApplicationEventTarget(), gQuitEventHandlerUPP,
                              1, &eventSpec, &quitNow, &installedHandler);
    if (err == noErr) {
        theTarget = GetEventDispatcherTarget();
        do {
            if (gNumberOfRunningThreads == 0) {
                timeToWaitForEvent = kEventDurationForever;
            } else {
                timeToWaitForEvent = kEventDurationNoWait;
            }
            err = ReceiveNextEvent(0, NULL, timeToWaitForEvent,
                                   true, &theEvent);
            if (err == noErr) {
		dbg_print_event("EventLoopEventHandler", theEvent);

		pthread_mutex_lock(&carbon_lock);
                (void) SendEventToEventTarget(theEvent, theTarget);
		pthread_mutex_unlock(&carbon_lock);

                ReleaseEvent(theEvent);
            }
	    else {
		EDBGFMT("EventLoopEventHandler: ReceiveNextEvent err=%d\n",
		    (int)err);
	    }
            if (gNumberOfRunningThreads > 0) {
                (void) YieldToAnyThread();
            }
        } while ( ! quitNow );
        junk = RemoveEventHandler(installedHandler);
    }
    SysBeep(10);
    EDBGFMT("EventLoopEventHandler: stopped err=%d", (int)err);
    return err;
}

static void RunApplicationEventLoopWithCooperativeThreadSupport(void)
// A reimplementation of RunApplicationEventLoop that supports
// yielding time to cooperative threads.  It relies on the
// rest of your application to maintain a global variable,
// gNumberOfRunningThreads, that reflects the number of threads
// that are ready to run.
{
    static const EventTypeSpec eventSpec = {'KWIN', 'KWIN' };
    OSStatus        err;
    OSStatus        junk;
    // EventTargetRef  appTarget;
    EventHandlerRef installedHandler;
    EventRef        dummyEvent;

    dummyEvent = nil;

    // Create a UPP for EventLoopEventHandler and QuitEventHandler
    // (if we haven't already done so).

    err = noErr;
    if (gEventLoopEventHandlerUPP == nil) {
        gEventLoopEventHandlerUPP = NewEventHandlerUPP(EventLoopEventHandler);
    }
    if (gQuitEventHandlerUPP == nil) {
        gQuitEventHandlerUPP = NewEventHandlerUPP(QuitEventHandler);
    }
    if (gEventLoopEventHandlerUPP == nil || gQuitEventHandlerUPP == nil) {
        err = memFullErr;
    }

    if (err == noErr) {
        err = InstallEventHandler(GetApplicationEventTarget(), 
				  gEventLoopEventHandlerUPP,
                                  1, &eventSpec, nil, &installedHandler);
        if (err == noErr) {
            err = MacCreateEvent(nil, 'KWIN', 'KWIN', GetCurrentEventTime(),
				 kEventAttributeNone, &dummyEvent);
            if (err == noErr) {
                err = PostEventToQueue(GetMainEventQueue(), dummyEvent,
				       kEventPriorityHigh);
            }
            if (err == noErr) {
		EDBGFMT("RunApplicationEventLoopWithCooperativeThreadSupport: post KWIN");
                RunApplicationEventLoop();
            }
            junk = RemoveEventHandler(installedHandler);
        }
    }
    EDBGFMT("RunApplicationEventLoopWithCooperativeThreadSupport: err=%d", 
	   (int) err);
    if (dummyEvent != nil) {
        ReleaseEvent(dummyEvent);
    }
}


void* carbon_event_thread(void* arg)
{
    CarbonBackend* be = (CarbonBackend*) arg;
    OSStatus err;
    EventTypeSpec appEventList[] = { { 'EPIC',  kEpicEventNew},
				     { 'EPIC',  kEpicEventDel},
				     { kEventClassCommand, kEventProcessCommand } };
    
    EDBGFMT("carbon_event_thread: running ofd=%d", be->ofd);

    RegisterAppearanceClient();
    CreateStandardWindowMenu(0, &be->menu);
    InsertMenu(be->menu, 0);
    DrawMenuBar();

    err = InstallApplicationEventHandler( GetEPicAppEventHandlerUPP(),
					  GetEventTypeCount( appEventList ), 
					  appEventList, be, NULL );

    pthread_mutex_unlock(&carbon_lock);

    RunApplicationEventLoopWithCooperativeThreadSupport();
    
    close(be->ofd);  // signal termination!
    return NULL;
}

EBackend* carbon_init(EDict* param)
{
#pragma unused(param)
    CarbonBackend* be;
    static int need_init = 1;

    if ((be = (CarbonBackend*) malloc(sizeof(CarbonBackend))) == NULL)
	return NULL;
    EOBJECT_INIT((EBackend*)be, EBACKEND_TYPE);

    be->b.cb = &carbon_callbacks;
    be->b.pending = 0;
    be->b.pixmap_list = NULL;
    be->b.window_list = NULL;
    be->b.event = INVALID_HANDLE;
    
    if (need_init) {
	static int carbon_pipe[2];
	need_init = 0;  /* FIXME: make fail safe */
	
	pipe(carbon_pipe);
	be->b.event = (EHANDLE_T) carbon_pipe[0];
	be->ofd     = carbon_pipe[1];
	pthread_mutex_init(&carbon_lock, NULL);
	pthread_mutex_lock(&carbon_lock);  // Lock until thread is initialised

	pthread_create(&carbon_thr, NULL, carbon_event_thread, 
		       (void*) be);
	pthread_mutex_lock(&carbon_lock);    // Wait for init to complete
	pthread_mutex_unlock(&carbon_lock);  // Wait for init to complete
    }
    return (EBackend*) &(be->b);
}

/* return the backend event handle */
static EHANDLE_T carbon_evt_attach(EBackend* backend)
{
    (void) backend;
    return (EHANDLE_T) -1;
}

static int carbon_evt_detach(EBackend* backend)
{
    (void) backend;
    return 0;
}

static int carbon_evt_read(EBackend* backend, EEvent* e,u_int16_t mask)
{
    (void) backend;
    (void) e;
    (void) mask;
    return 0;
}

static int carbon_finish(EBackend* backend)
{
    CarbonBackend* be = (CarbonBackend*) backend;

    free(be);
    return 0;
}

static void pixel_ReleaseInfo(void *info)
{
    (void) info;
    EDBGFMT_MEM("ReleaseInfo");
    /* NOOP */
}

static const void* pixel_GetBytePointer(void* info)
{
    EPixmap* pic = (EPixmap*) info;

    EDBGFMT_MEM("GetBytesPointer");
    return (void*) pic->data;
}

static size_t pixel_GetBytesAtOffset(void *info, void *buffer, 
				     size_t offset, size_t count)
{
    EPixmap* pic = (EPixmap*) info;

    EDBGFMT_MEM("GetBytesAtOffset");
    memcpy(buffer, pic->data+offset, count);
    return count;
}

static void pixel_ReleaseBytePointer(void *info, const void *pointer)
{
    (void) info;
    (void) pointer;
    EDBGFMT_MEM("ReleaseBytePointer");
    /* Noop */
}

static CGDataProviderDirectAccessCallbacks dacallbacks =
{
    pixel_GetBytePointer,
    pixel_ReleaseBytePointer,
    pixel_GetBytesAtOffset,
    pixel_ReleaseInfo
};

static int carbon_pic_attach(EBackend* backend, EPixmap* pixmap)
{
    CarbonBackend* be = (CarbonBackend*) backend;
    CarbonPixels* pe;

    if (pixmap->opaque != NULL)
	return -1;
    if ((pe = (CarbonPixels*) malloc(sizeof(CarbonPixels))) == NULL)
	return -1;

    pe->colorspace = CGColorSpaceCreateWithName(kCGColorSpaceGenericRGB);
    pe->bitmapinfo = 0;

    switch(pixmap->pixelType) {
    case EPIXEL_TYPE_RGB:
	pe->bitmapinfo |= kCGImageAlphaNone;         /* RGB format */
	break;
    case EPIXEL_TYPE_RGBA:
	pe->bitmapinfo |= kCGBitmapByteOrder32Big;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast; /* RGBA format */
	break;
    case EPIXEL_TYPE_ARGB:
	pe->bitmapinfo |= kCGBitmapByteOrder32Big;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst; /* ARGB format */
	break;
    case EPIXEL_TYPE_BGR: /* ??? */
	pe->bitmapinfo |= kCGImageAlphaNone;         /* BGR format */
	break;
    case EPIXEL_TYPE_BGRA: /* ARGB */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipFirst;
	break;
    case EPIXEL_TYPE_ABGR: /* RGBA */
	pe->bitmapinfo |= kCGBitmapByteOrder32Little;
	pe->bitmapinfo |= kCGImageAlphaNoneSkipLast;
	break;
    default:
	printf("macos: unhandled pixelType = %d\n", pixmap->pixelType);
	free(pe);
	return -1;
    }

    pe->provider = CGDataProviderCreateDirectAccess(
	(void*) pixmap, pixmap->sz, &dacallbacks);
    EObjectLink(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) pe;
    pixmap->backend = (EBackend*) be;
    return 0;
}

						
static int carbon_pic_detach(EBackend* backend, EPixmap* pixmap)
{
    CarbonPixels* pe = (CarbonPixels*) pixmap->opaque;

    if (pe != NULL) {
	CGColorSpaceRelease(pe->colorspace);
	CGDataProviderRelease(pe->provider);
	EObjectUnlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

/* There must be a better way */
static void EPicDrawImage(CGContextRef ctx, CGRect imgRect, EPixmap* pic)
{
    CarbonPixels* pe = (CarbonPixels*) pic->opaque;
    CGImageRef img;
    unsigned int bytesPerRow   = pic->bytesPerRow;
    unsigned int bitsPerPixel  = pic->bitsPerPixel;

    img = CGImageCreate(pic->width,               /* width in pixels */
			pic->height,              /* height in pixels */
			8,                        /* bitsPerComponent */
			bitsPerPixel,             /* bitsPerPixel */
			bytesPerRow,              /* bytesPerRow */
			pe->colorspace,           /* colorspace */
			pe->bitmapinfo,           /* bitmapinfo, */
			pe->provider,
			NULL,
			false,
			kCGRenderingIntentSaturation);
    if (img == NULL) {
	fprintf(stderr, "epic_macos: error CGImageCreate\n");
    }
    if (img != NULL) {
	CGContextDrawImage(ctx, imgRect, img);
	CGImageRelease(img);
    }
}


static int carbon_pic_draw(EBackend* backend, EPixmap* pic, EWindow* ewin,
			   int off_screen,
			   int src_x, int src_y, int dst_x, int dst_y,
			   unsigned int width,
			   unsigned int height)

{
    CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;
    WindowRef wref;
    CGContextRef ctx;
    CGRect cgWinRect;
    CGRect imgRect;
    CGrafPtr port;
    (void) backend;
    (void) off_screen;

    // Rect bound;
    // RgnHandle       contentRgn;
    // CGRect clipRect;

    if ((cwin == NULL) || ((wref = cwin->winRef) == 0))
	return -1;

    // contentRgn = NewRgn();

    cgWinRect = CGRectMake(0, 0, pic->width, pic->height);
    imgRect = CGRectMake((float)src_x, (float)src_y, 
			 (float)width, (float)height);
    port = GetWindowPort(wref);

    pthread_mutex_lock(&carbon_lock);

    QDBeginCGContext(port, &ctx);

    CGContextSaveGState(ctx);

    // GetWindowPortBounds(wref, &bound);
    // GetWindowRegion(wref, kWindowContentRgn, contentRgn);
    // QDGlobalToLocalRegion(port, contentRgn);
    // ClipCGContextToRegion(ctx, &bound, contentRgn);
    // clipRect = CGContextGetClipBoundingBox(ctx);

    CGContextTranslateCTM(ctx, (float)dst_x, (float)dst_y);
    EPicDrawImage(ctx, imgRect, pic);

    CGContextFlush(ctx);
    /* CGContextSynchronize(Ctx); */
    CGContextRestoreGState(ctx);
    QDEndCGContext(port, &ctx);

    pthread_mutex_unlock(&carbon_lock);

    // DisposeRgn(contentRgn);
    return 0;
}

static int carbon_win_swap(EBackend* backend, EWindow* ewin)
{
    (void) backend;
    (void) ewin;
    return 0;
}

static int carbon_win_attach(EBackend* backend, EWindow* ewin)
{
    CarbonBackend* be = (CarbonBackend*) backend;
    CarbonWindow* cwin;
    EventRef      newEvent;

    if (ewin->opaque != NULL)
	return -1;

    if ((cwin = (CarbonWindow*) malloc(sizeof(CarbonWindow))) == NULL) 
	return -1;
    memset(cwin, 0, sizeof(CarbonWindow));
    ewin->opaque = (void*) cwin;
    EObjectLink(&backend->window_list, ewin);
    ewin->backend = (EBackend*) be;
    cwin->winClass         = kDocumentWindowClass;
    cwin->winAttrs         = kWindowStandardHandlerAttribute | 
	                     kWindowStandardDocumentAttributes |
	                     kWindowInWindowMenuAttribute;
    cwin->winBounds.left   = ewin->x;
    cwin->winBounds.top    = ewin->y;
    cwin->winBounds.bottom = ewin->y+ewin->height-1;
    cwin->winBounds.right  = ewin->x+ewin->width-1;
    EDBGFMT("carbon_win_attach: cwin=%p", cwin);
    MacCreateEvent(nil, 'EPIC',  kEpicEventNew, GetCurrentEventTime(),
		   kEventAttributeNone, &newEvent);
    SetEventParameter(newEvent, 'EPIC', 'CWIN', sizeof(CarbonWindow*), &cwin);
    PostEventToQueue(GetMainEventQueue(), newEvent, kEventPriorityHigh);
    ReleaseEvent(newEvent);
    EDBGFMT("carbon_win_attach: post done");

    /* wait until cwin->winRef != NULL */
    while(cwin->winRef == NULL)
	usleep(1000);
    
    return 0;
}

static int carbon_win_detach(EBackend* backend, EWindow* ewin)
{
    CarbonWindow* cwin = (CarbonWindow*) ewin->opaque;

    if (cwin != NULL) {
	EventRef  delEvent;

	EObjectUnlink(&backend->window_list, ewin);
	ewin->opaque = NULL;
	ewin->backend = NULL;

	MacCreateEvent(nil, 'EPIC',  kEpicEventDel, GetCurrentEventTime(),
		       kEventAttributeNone, &delEvent);
	SetEventParameter(delEvent, 'EPIC', 'CWIN', 
			  sizeof(CarbonWindow*), &cwin);
	PostEventToQueue(GetMainEventQueue(), delEvent, kEventPriorityHigh);
	ReleaseEvent(delEvent);
    }
    return 0;
}


static pascal OSStatus EPicAppEventHandler(
    EventHandlerCallRef	inHandlerCallRef,
    EventRef		inEvent,
    void*		inUserData )
{
#pragma unused(inHandlerCallRef, inUserData)
    OSStatus	 err = eventNotHandledErr;
    UInt32	 eventClass = GetEventClass(inEvent);
    UInt32	 eventKind = GetEventKind(inEvent);
    // CarbonBackend* be = (CarbonBackend*) inUserData;

    dbg_print_event("EPicAppEventHandler", inEvent);

    if (eventClass == 'EPIC') {
	CarbonWindow* cwin;

	GetEventParameter( inEvent, 'EPIC', 'CWIN', 
			   NULL, sizeof(CarbonWindow*), NULL, &cwin);
	EDBGFMT("EPicAppEventHandler:cwin = %p", cwin);
	if (cwin == NULL)
	    return eventNotHandledErr;
	if (eventKind == kEpicEventNew) {
	    static const EventTypeSpec    kWindowEvents[] =
		{
  		    { kEventClassCommand,  kEventCommandProcess },
  		    { kEventClassCommand,  kEventProcessCommand },
		    { kEventClassCommand,  kEventCommandUpdateStatus },
		    { kEventClassKeyboard, kEventRawKeyDown },
		    { kEventClassKeyboard, kEventRawKeyUp },
		    { kEventClassMouse,    kEventMouseDown },
		    { kEventClassMouse,    kEventMouseUp },
		    { kEventClassWindow,   kEventWindowClickContentRgn },
		    { kEventClassWindow,   kEventWindowDrawContent },
		    { kEventClassWindow,   kEventWindowBoundsChanged },
		    { kEventClassWindow,   kEventWindowClose } 
		};
	    WindowRef        winRef;

	    if ((err = CreateNewWindow(cwin->winClass,
				       cwin->winAttrs,
				       &(cwin->winBounds),
				       &winRef)) != noErr) {
		EDBGFMT("EPicAppEventHandler: could not create window %d\n",
		    (int) err);
		return err;
	    }

	    SetWindowTitleWithCFString (winRef, CFSTR("EPic"));
	    SetWRefCon(winRef, (long) cwin); // link to CarbonWindow
	    cwin->macPort = GetWindowPort(winRef);
	    
	    err = InstallWindowEventHandler(winRef,
					    GetEPicWindowEventHandlerUPP(),
					    GetEventTypeCount( kWindowEvents ), 
					    kWindowEvents,
					    cwin, NULL );
	    ShowWindow(winRef);
	    SelectWindow(winRef);
	    BringToFront(winRef);
	    cwin->winRef = winRef;
	    err = noErr;
	}
	else if (eventKind ==  kEpicEventDel) {
	    DisposeWindow(cwin->winRef);
	    free(cwin);
	    err = noErr;
	}
    }
    return err;
}


static pascal OSStatus EPicWindowEventHandler( EventHandlerCallRef inCaller, 
					       EventRef inEvent, 
					       void* inRefcon )
{
#pragma unused(inCaller)
    OSStatus    err = eventNotHandledErr;
    UInt32	eventClass = GetEventClass(inEvent);
    UInt32	eventKind = GetEventKind(inEvent);
    CarbonWindow* cwin = (CarbonWindow*) inRefcon;
    WindowRef window = (cwin != NULL) ? cwin->winRef : NULL;

    dbg_print_event("EPicWindowEventHandler", inEvent);

    switch (eventClass) {
    case kEventClassKeyboard:
	if (eventKind == kEventRawKeyDown) {
            char ch;
            err = GetEventParameter( inEvent, 
				     kEventParamKeyMacCharCodes, 
				     typeChar,
				     NULL,
				     sizeof( char ), 
				     NULL, 
				     &ch );
	    printf("Got key=%c\n", ch);
	}
	break;

    case kEventClassWindow:
        if ( eventKind == kEventWindowDrawContent ) {
	    printf("WindowDrawContent\n");
        }
        else if ( eventKind == kEventWindowClose ) {
	    printf("WindowClose\n");
        }
        else if ( eventKind == kEventWindowClickContentRgn ) {
            Point   where;
            UInt32  modifiers;
            
            GetEventParameter(inEvent, kEventParamMouseLocation, 
			      typeQDPoint, NULL, sizeof(Point), 
			      NULL, &where);
            GetEventParameter(inEvent, kEventParamKeyModifiers, 
			      typeUInt32, NULL, sizeof(UInt32), 
			      NULL, &modifiers);
	    // What should we use?
            QDGlobalToLocalPoint(GetWindowPort(window), &where);

	    printf("WindowClickContentRgn x=%d,y=%d,mod=%lx\n",
		   where.h, where.v, modifiers);
        }
        else if ( eventKind == kEventWindowBoundsChanged ) {
            Rect r;
            GetEventParameter( inEvent, kEventParamCurrentBounds, 
			       typeQDRectangle, NULL, sizeof(Rect),
			       NULL, &r);
            // We get here also when the window gets moved and doesn't 
	    // need to be redrawn.
	    printf("WindowBoundsChanged %d,%d,%d,%d\n",
		   r.top, r.left, r.bottom, r.right);
        }
        break;
	

    case kEventClassCommand: {
	HICommandExtended cmd;
            
	if (eventKind == kEventCommandProcess) {
	    GetEventParameter( inEvent, kEventParamDirectObject, typeHICommand,
			       NULL, sizeof( cmd ), NULL, &cmd);
	    switch ( cmd.commandID ) {
		// Add your own command-handling cases here
	    default:
		break;
	    }
	    break;
	}
	break;
    }

	
    default:
	break;
    }
    return err;
}

int carbon_adjust(EBackend *backend, EDict* param)
{
    (void) backend;
    (void) param;
    return 1;
}

