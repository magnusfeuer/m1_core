/*
 *  Mac Os X / GL display driver
 *
 * Where:
 *  /System/Library/Frameworks/ApplicationServices.framework/Frameworks/
 *    CoreGraphics.framework/Headers
 */
#include <Carbon/Carbon.h>
#include <machine/endian.h>
#include <AGL/agl.h>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>


#include "epic.h"


typedef struct {
    EBackend b;
    unsigned short modstate;    /* modifier state */
    unsigned short grab_key;    /* grab key */
} CarbonGLBackend;

typedef struct 
{
    Boolean fAcceleratedMust;
    GLint aglAttributes[64];
    SInt32 VRAM;
    SInt32 textureRAM;
    AGLPixelFormat fmt;
    Boolean fDraggable;
} GLWindowInfo;

typedef struct {
    AGLContext    aglContext;
    GLWindowInfo  glInfo;
    GLuint textureName;
    WindowRef winRef;
    GWorldPtr macPort;
} CarbonGLWindow;

EBackend* carbon_gl_init(EDict* param);

static int carbon_gl_finish(EBackend*);
static int carbon_gl_pic_attach(EBackend*, EPixmap* pic);
static int carbon_gl_pic_detach(EBackend*, EPixmap* pic);
static int carbon_gl_pic_draw(EBackend*, EPixmap* pic, EWindow* win,
			      int off_screen,
			      int src_x, int src_y, int dst_x, int dst_y,
			      unsigned int width,
			      unsigned int height);
static int carbon_gl_win_attach(EBackend*, EWindow* win);
static int carbon_gl_win_detach(EBackend*, EWindow* win);
static int carbon_gl_win_swap(EBackend*, EWindow* win);
static EHANDLE_T carbon_gl_evt_attach(EBackend*);
static int carbon_gl_evt_detach(EBackend*);
static int carbon_gl_evt_read(EBackend*, EEvent*,u_int16_t mask);
static int carbon_gl_adjust(EBackend *backend, EDict* param);


static EPicCallbacks carbon_gl_callbacks =
{
    carbon_gl_finish,
    carbon_gl_pic_attach,
    carbon_gl_pic_detach,
    carbon_gl_pic_draw,
    carbon_gl_win_attach,
    carbon_gl_win_detach,
    carbon_gl_evt_attach,
    carbon_gl_evt_detach,
    carbon_gl_evt_read,
    carbon_gl_adjust,
    carbon_gl_win_swap
};

// This code is used from example but...
void ReportError (char * strError)
{
    char errMsgCStr [256];

    sprintf (errMsgCStr, "%s\n", strError);

    // out as debug string
#ifdef kVerboseErrors
    // ensure we are faded in
    if (gDSpStarted)
	DSpContext_CustomFadeGammaIn (NULL, NULL, 0);
    CStrToPStr (strErr, errMsgCStr);
    DebugStr (strErr);
#endif // kVerboseErrors
}

OSStatus glReportError (void)
{
    GLenum err = glGetError();
    switch (err) {
    case GL_NO_ERROR:
	break;
    case GL_INVALID_ENUM:
	ReportError ("GL Error: Invalid enumeration");
	break;
    case GL_INVALID_VALUE:
	ReportError ("GL Error: Invalid value");
	break;
    case GL_INVALID_OPERATION:
	ReportError ("GL Error: Invalid operation");
	break;
    case GL_STACK_OVERFLOW:
	ReportError ("GL Error: Stack overflow");
	break;
    case GL_STACK_UNDERFLOW:
	ReportError ("GL Error: Stack underflow");
	break;
    case GL_OUT_OF_MEMORY:
	ReportError ("GL Error: Out of memory");
	break;
    default:
	ReportError ("GL Error: Unknown error");
	break;
    }
    // ensure we are returning an OSStatus noErr if no error condition
    if (err == GL_NO_ERROR)
	return noErr;
    else {
	printf("GL error at %s:%d: %s\n",__FILE__,__LINE__,
	       (char*)gluErrorString(err));
	return (OSStatus) err;
    }
}


OSStatus aglReportError (void)
{
    GLenum err = aglGetError();
    if (AGL_NO_ERROR != err)
	ReportError ((char *)aglErrorString(err));
    // ensure we are returning an OSStatus noErr if no error condition
    if (err == AGL_NO_ERROR)
	return noErr;
    else
	return (OSStatus) err;
}

short FindGDHandleFromWindow (WindowPtr pWindow, GDHandle * phgdOnThisDevice)
{
    GrafPtr pgpSave;
    Rect rectWind, rectSect;
    long greatestArea, sectArea;
    short numDevices = 0;
    GDHandle hgdNthDevice;
	
    if (!pWindow || !phgdOnThisDevice)
	return 0;
    
    *phgdOnThisDevice = NULL;
	
    GetPort (&pgpSave);
    SetPortWindowPort (pWindow);
	

#if TARGET_API_MAC_CARBON
    GetWindowPortBounds (pWindow, &rectWind);
#else
    rectWind = pWindow->portRect;
#endif // TARGET_API_MAC_CARBON
    LocalToGlobal ((Point*)& rectWind.top);	// convert to global coordinates
    LocalToGlobal ((Point*)& rectWind.bottom);
    hgdNthDevice = GetDeviceList ();
    greatestArea = 0;
    // check window against all gdRects in gDevice list and remember 
    //  which gdRect contains largest area of window}
    while (hgdNthDevice) {
	if (TestDeviceAttribute (hgdNthDevice, screenDevice))
	    if (TestDeviceAttribute (hgdNthDevice, screenActive))
	    {
		// The SectRect routine calculates the intersection 
		//  of the window rectangle and this gDevice 
		//  rectangle and returns TRUE if the rectangles intersect, 
		//  FALSE if they don't.
		SectRect (&rectWind, &(**hgdNthDevice).gdRect, &rectSect);
		// determine which screen holds greatest window area
		//  first, calculate area of rectangle on current device
		sectArea = (long) (rectSect.right - rectSect.left) * (rectSect.bottom - rectSect.top);
		if (sectArea > 0)
		    numDevices++;
		if (sectArea > greatestArea)
		{
		    greatestArea = sectArea; // set greatest area so far
		    *phgdOnThisDevice = hgdNthDevice; // set zoom device
		}
		hgdNthDevice = GetNextDevice(hgdNthDevice);
	    }
    }
    
    SetPort (pgpSave);
    return numDevices;
}


static Boolean CheckRenderer (GDHandle hGD, long* pVRAM, long* pTextureRAM, 
			      GLint* pDepthSizeSupport, Boolean fAccelMust)
{
    AGLRendererInfo info, head_info;
    GLint inum;
    GLint dAccel = 0;
    GLint dVRAM = 0, dMaxVRAM = 0;
    Boolean canAccel = false, found = false;
    
    head_info = aglQueryRendererInfo(&hGD, 1);
    aglReportError ();
    if(!head_info) {
	ReportError ("aglQueryRendererInfo error");
	return false;
    }
    else {
	info = head_info;
	inum = 0;
	// see if we have an accelerated renderer, if so ignore non-accelerated ones
	// this prevents returning info on software renderer when actually we'll get the hardware one
	while (info) {	
	    aglDescribeRenderer(info, AGL_ACCELERATED, &dAccel);
	    aglReportError ();
	    if (dAccel)
		canAccel = true;
	    info = aglNextRendererInfo(info);
	    aglReportError ();
	    inum++;
	}
	
	info = head_info;
	inum = 0;
	while (info) {
	    aglDescribeRenderer (info, AGL_ACCELERATED, &dAccel);
	    aglReportError ();
	    // if we can accel then we will choose the accelerated renderer 
	    // how about compliant renderers???
	    if ((canAccel && dAccel) || (!canAccel && (!fAccelMust || dAccel)))
	    {
		aglDescribeRenderer (info, AGL_VIDEO_MEMORY, &dVRAM);	// we assume that VRAM returned is total thus add texture and VRAM required
		aglReportError ();
		if (dVRAM >= (*pVRAM + *pTextureRAM))
		{
		    if (dVRAM >= dMaxVRAM) // find card with max VRAM
		    {
			aglDescribeRenderer (info, AGL_DEPTH_MODES, pDepthSizeSupport);	// which depth buffer modes are supported
			aglReportError ();
			dMaxVRAM = dVRAM; // store max
			found = true;
		    }
		}
	    }
	    info = aglNextRendererInfo(info);
	    aglReportError ();
	    inum++;
	}
    }
    aglDestroyRendererInfo(head_info);
    if (found) // if we found a card that has enough VRAM and meets the accel criteria
    {
	*pVRAM = dMaxVRAM; // return VRAM
	return true;
    }
    // VRAM will remain to same as it did when sent in
    return false;
}

static Boolean CheckAllDeviceRenderers (long* pVRAM, long* pTextureRAM, 
					GLint* pDepthSizeSupport, 
					Boolean fAccelMust)
{
    AGLRendererInfo info, head_info;
    GLint inum;
    GLint dAccel = 0;
    GLint dVRAM = 0, dMaxVRAM = 0;
    Boolean canAccel = false, found = false, goodCheck = true; // can the renderer accelerate, did we find a valid renderer for the device, are we still successfully on all the devices looked at
    long MinVRAM = 0x8FFFFFFF; // max long

    GDHandle hGD = GetDeviceList (); // get the first screen

    while (hGD && goodCheck) {
	head_info = aglQueryRendererInfo(&hGD, 1);
	aglReportError ();
	if(!head_info)	{
	    ReportError ("aglQueryRendererInfo error");
	    return false;
	}
	else {
	    info = head_info;
	    inum = 0;
	    // see if we have an accelerated renderer, if so ignore non-accelerated ones
	    // this prevents returning info on software renderer when actually we'll get the hardware one
	    while (info) {
                aglDescribeRenderer(info, AGL_ACCELERATED, &dAccel);
		aglReportError ();
		if (dAccel)
		    canAccel = true;
		info = aglNextRendererInfo(info);
		aglReportError ();
		inum++;
	    }
	    
	    info = head_info;
	    inum = 0;
	    while (info) {	
		aglDescribeRenderer(info, AGL_ACCELERATED, &dAccel);
		aglReportError ();
		// if we can accel then we will choose the accelerated renderer 
		// how about compliant renderers???
		if ((canAccel && dAccel) || (!canAccel && (!fAccelMust || dAccel)))
		{
                    aglDescribeRenderer(info, AGL_VIDEO_MEMORY, &dVRAM);	// we assume that VRAM returned is total thus add texture and VRAM required
		    aglReportError ();
		    if (dVRAM >= (*pVRAM + *pTextureRAM))
		    {
			if (dVRAM >= dMaxVRAM) // find card with max VRAM
			{
			    aglDescribeRenderer(info, AGL_DEPTH_MODES, pDepthSizeSupport);	// which depth buffer modes are supported
			    aglReportError ();
			    dMaxVRAM = dVRAM; // store max
			    found = true;
			}
		    }
		}
		info = aglNextRendererInfo(info);
		aglReportError ();
		inum++;
	    }
	}
	aglDestroyRendererInfo(head_info);
	if (found) // if we found a card that has enough VRAM and meets the accel criteria
	{
	    if (MinVRAM > dMaxVRAM)
		MinVRAM = dMaxVRAM; // return VRAM
	    
	}
	else
	    goodCheck = false; // one device failed thus entire requirement fails
	hGD = GetNextDevice (hGD); // get next device
    } // while
    if (goodCheck) { // we check all devices and each was good
	*pVRAM = MinVRAM; // return VRAM
	return true;
    }
    return false; //at least one device failed to have mins
}


static OSStatus BuildGLonWindow (WindowPtr pWindow, AGLContext* paglContext, 
				 GLWindowInfo* pcontextInfo, 
				 AGLContext aglShareContext)
{
    GDHandle hGD = NULL;
    GrafPtr cgrafSave = NULL;
    short numDevices;
    GLint depthSizeSupport;
    OSStatus err = noErr;
	
    if (!pWindow || !pcontextInfo) {
	ReportError ("NULL parameter passed to BuildGLonDrawable.");
	return paramErr;
    }
	
    GetPort (&cgrafSave);
    SetPortWindowPort(pWindow);

    // check renderere VRAM and acceleration
    numDevices = FindGDHandleFromWindow (pWindow, &hGD);
    if (!pcontextInfo->fDraggable) {	// if numDevices > 1 then we will only be using the software renderer otherwise check only window device
	if ((numDevices > 1) || (numDevices == 0)) // this window spans mulitple devices thus will be software only
	{
	    // software renderer
	    // infinite VRAM, infinite textureRAM, not accelerated
	    if (pcontextInfo->fAcceleratedMust)
	    {
		ReportError ("Unable to accelerate window that spans multiple devices");
		return err;
	    }
	}
	else // not draggable on single device
	{
	    if (!CheckRenderer (hGD, &(pcontextInfo->VRAM), &(pcontextInfo->textureRAM), &depthSizeSupport, pcontextInfo->fAcceleratedMust))
	    {
		ReportError ("Renderer check failed");
		return err;
	    }
	}
    }
    // else draggable so must check all for support (each device should have at least one renderer that meets the requirements)
    else if (!CheckAllDeviceRenderers (&(pcontextInfo->VRAM), &(pcontextInfo->textureRAM), &depthSizeSupport, pcontextInfo->fAcceleratedMust))
    {
	ReportError ("Renderer check failed");
	return err;
    }
	
    // do agl
    if ((Ptr) kUnresolvedCFragSymbolAddress == (Ptr) aglChoosePixelFormat) // check for existance of OpenGL
    {
	ReportError ("OpenGL not installed");
	return noErr;
    }	
    // we successfully passed the renderer check

    if ((!pcontextInfo->fDraggable && (numDevices == 1)))  // not draggable on a single device
	pcontextInfo->fmt = aglChoosePixelFormat (&hGD, 1, pcontextInfo->aglAttributes); // get an appropriate pixel format
    else
	pcontextInfo->fmt = aglChoosePixelFormat (NULL, 0, pcontextInfo->aglAttributes); // get an appropriate pixel format
    aglReportError ();
    if (NULL == pcontextInfo->fmt) 
    {
	ReportError("Could not find valid pixel format");
	return noErr;
    }
    
    *paglContext = aglCreateContext (pcontextInfo->fmt, aglShareContext); // Create an AGL context
    if (AGL_BAD_MATCH == aglGetError())
	*paglContext = aglCreateContext (pcontextInfo->fmt, 0); // unable to sahre context, create without sharing
    aglReportError ();
    if (NULL == *paglContext) 
    {
	ReportError ("Could not create context");
	return noErr;
    }
	
    if (!aglSetDrawable (*paglContext, GetWindowPort (pWindow))) // attach the CGrafPtr to the context
	return aglReportError ();
    
    if(!aglSetCurrentContext (*paglContext)) // make the context the current context
	return aglReportError ();
    
    SetPort (cgrafSave);
    
    return err;
}

// if fail to allocate: paglContext will be NULL
// if error: will return error and paglContext will be NULL

OSStatus BuildGLFromWindow (WindowPtr pWindow, AGLContext* paglContext, 
			    GLWindowInfo* contextInfo, 
			    AGLContext aglShareContext)
{
    if (!pWindow)
	return paramErr;
    return BuildGLonWindow (pWindow, paglContext, contextInfo, aglShareContext);
}

OSStatus DestroyGLFromWindow (AGLContext* paglContext, 
			      GLWindowInfo* pcontextInfo)
{
    OSStatus err;
	
    if ((!paglContext) || (!*paglContext))
	return paramErr; // not a valid context
    glFinish ();
    aglSetCurrentContext (NULL);
    err = aglReportError ();
    aglSetDrawable (*paglContext, NULL);
    err = aglReportError ();
    aglDestroyContext (*paglContext);
    err = aglReportError ();
    *paglContext = NULL;
    
    if (pcontextInfo->fmt)  {
	aglDestroyPixelFormat (pcontextInfo->fmt); // pixel format is no longer valid
	err = aglReportError ();
    }
    pcontextInfo->fmt = 0;
	
    return err;
}



static OSStatus carbon_gl_window_rect(EWindow* ewin)
{
    CarbonGLWindow* cwin = (CarbonGLWindow*) ewin->opaque;
    Rect rectPort;
    long width  = ewin->width;
    long height = ewin->height;
    GDHandle device;
    Rect deviceRect, availRect, rect;

    // find device the window is mostly on
    GetWindowGreatestAreaDevice(cwin->winRef, kWindowContentRgn, 
				&device, &deviceRect);
    //  get the geretest available area for te windoew 
    //  (minus doc and menu if applicable)
    GetAvailableWindowPositioningBounds (device, &availRect);

    if (width > (availRect.right - availRect.left)) 
	width = (availRect.right - availRect.left);
    if (height > (availRect.bottom - availRect.top))
	height = (availRect.bottom - availRect.top);
    
    SizeWindow (cwin->winRef, (short) width, (short) height, true);
    
    ConstrainWindowToScreen(cwin->winRef, kWindowStructureRgn,
			    kWindowConstrainMayResize, NULL, &rect);
    GetWindowPortBounds (cwin->winRef, &rectPort);
    
    glViewport (0, 0, rectPort.right - rectPort.left, 
		rectPort.bottom - rectPort.top);
    return noErr;
}

/* Set up GL context if needed */
static OSStatus carbon_gl_setup(EWindow* ewin)
{
    CarbonGLWindow* cwin = (CarbonGLWindow*) ewin->opaque;

    if (!cwin->aglContext) {
	OSStatus err;
	GrafPtr portSave = NULL;
	GLint swap = 0;  // =1 Looks a lot nicer but slow things up a bot
	int i;

        GetPort (&portSave);
        SetPort ((GrafPtr) GetWindowPort (cwin->winRef));

        cwin->glInfo.fAcceleratedMust = false;
        cwin->glInfo.VRAM = 0 * 1048576;
        cwin->glInfo.textureRAM = 0 * 1048576;
	cwin->glInfo.fDraggable = true;
        cwin->glInfo.fmt = 0;

        i = 0; // first attribute in array
	cwin->glInfo.aglAttributes [i++] = AGL_RGBA;
	cwin->glInfo.aglAttributes [i++] = AGL_DOUBLEBUFFER;
	cwin->glInfo.aglAttributes [i++] = AGL_ACCELERATED;
	cwin->glInfo.aglAttributes [i++] = AGL_NO_RECOVERY;
	cwin->glInfo.aglAttributes [i++] = AGL_NONE;
        err = BuildGLFromWindow (cwin->winRef, &cwin->aglContext, 
				 &cwin->glInfo, NULL); 
        if (!cwin->aglContext) {
	    SetPort (portSave);
            DestroyGLFromWindow (&cwin->aglContext, &cwin->glInfo);
	    return err;
	}
	aglSetCurrentContext (cwin->aglContext);
	aglUpdateContext (cwin->aglContext);
	aglSetInteger (cwin->aglContext, AGL_SWAP_INTERVAL, &swap); 
	SetPort (portSave); //reset port
    }
    return noErr; // we done
}

static OSStatus load_texture(EPixmap* pic, EWindow* ewin,
			     int src_x, int src_y, int dst_x, int dst_y,
			     unsigned int width,
			     unsigned int height)
{
    CarbonGLWindow* cwin = (CarbonGLWindow*) ewin->opaque;
    GLint  tx_iformat = GL_RGBA8;
    GLenum tx_format = GL_BGRA_EXT;
    GLenum tx_type = GL_UNSIGNED_INT_8_8_8_8_REV;
    (void) dst_x;
    (void) dst_y;

    switch(pic->pixelType) {
    case EPIXEL_TYPE_R8G8B8:
	tx_iformat = GL_RGB8;
	tx_format  = GL_RGB;
	tx_type    = GL_UNSIGNED_BYTE;
	break;
    case EPIXEL_TYPE_565_BE:
	tx_iformat = GL_RGB5;
	tx_format  = GL_RGB;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_SHORT_5_6_5;
#else
	tx_type    = GL_UNSIGNED_SHORT_5_6_5_REV;
#endif
	break;
    case EPIXEL_TYPE_565_LE:
	tx_iformat = GL_RGB5;
	tx_format  = GL_RGB;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_SHORT_5_6_5_REV;
#else
	tx_type    = GL_UNSIGNED_SHORT_5_6_5;
#endif
	break;
    case EPIXEL_TYPE_B8G8R8:
	tx_iformat = GL_RGB8;
	tx_format  = GL_BGR;
	tx_type    = GL_UNSIGNED_BYTE;
	break;
    case EPIXEL_TYPE_A8R8G8B8:
	tx_iformat = GL_RGBA8;
	tx_format  = GL_BGRA_EXT;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_INT_8_8_8_8_REV;
#else
	tx_type    = GL_UNSIGNED_INT_8_8_8_8;
#endif
	break;
    case EPIXEL_TYPE_R8G8B8A8:
	tx_iformat = GL_RGBA8;
	tx_format  = GL_RGBA;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_INT_8_8_8_8;
#else
	tx_type    = GL_UNSIGNED_INT_8_8_8_8_REV;
#endif
	break;
    case EPIXEL_TYPE_B8G8R8A8:
	tx_iformat = GL_RGBA8;
	tx_format  = GL_BGRA;
#if BYTE_ORDER == BIG_ENDIAN
	tx_type    = GL_UNSIGNED_INT_8_8_8_8;
#else
	tx_type    = GL_UNSIGNED_INT_8_8_8_8_REV;
#endif
	break;
    default:
	return eventNotHandledErr; // Better error code?
    }

    if (cwin->textureName == 0)
	glGenTextures(1, &cwin->textureName);
    glBindTexture (GL_TEXTURE_RECTANGLE_ARB, cwin->textureName);

    glPixelStorei (GL_UNPACK_ROW_LENGTH,    pic->width);
    glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
#ifdef GL_UNPACK_CLIENT_STORAGE_APPLE
    // This speeds processing up from 40-50 => 50-60  fps :-)
    glPixelStorei (GL_UNPACK_CLIENT_STORAGE_APPLE, GL_TRUE);
#endif
    glTexParameteri(GL_TEXTURE_RECTANGLE_ARB,GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    /* GL_TEXTURE_RECTANGLE_ARB */
    glTexImage2D (GL_TEXTURE_RECTANGLE_ARB, //GL_TEXTURE_2D, 
		  0, 
		  tx_iformat, 
		  width, height,
		  0, 
		  tx_format, 
		  tx_type,
		  EPIXEL_ADDR(pic, src_x, src_y));
    return noErr;
}

static OSStatus GLWindowEventHandler( EventHandlerCallRef inCaller, 
				    EventRef inEvent, 
				    void* inRefcon );

/* util */

#define kGenericRGBProfilePathStr       "/System/Library/ColorSync/Profiles/Generic RGB Profile.icc"

//----------------------------------------------------------------------------
DEFINE_ONE_SHOT_HANDLER_GETTER( GLWindowEventHandler )
//----------------------------------------------------------------------------

EBackend* carbon_gl_init(EDict* param)
{
#pragma unused(param)
    CarbonGLBackend* be;
    static int need_init = 1;

    if ((be = (CarbonGLBackend*) malloc(sizeof(CarbonGLBackend))) == NULL)
	return NULL;
    EOBJECT_INIT((EBackend*)be, EBACKEND_TYPE);

    be->b.cb = &carbon_gl_callbacks;
    be->b.pending = 0;
    be->b.pixmap_list = NULL;
    be->b.window_list = NULL;
    be->b.event = INVALID_HANDLE;
    
    if (need_init) {
	MenuRef windMenuRef;

	need_init = 0;  /* FIXME mutex */
	RegisterAppearanceClient();
	CreateStandardWindowMenu(0, &windMenuRef);
	InsertMenu(windMenuRef, 0);
	DrawMenuBar();
    }
    return (EBackend*) &(be->b);
}

/* return the backend event handle */
static EHANDLE_T carbon_gl_evt_attach(EBackend* backend)
{
#pragma unused(backend)
    return (EHANDLE_T) -1;
}

static int carbon_gl_evt_detach(EBackend* backend)
{
#pragma unused(backend)
    return 0;
}

static int carbon_gl_evt_read(EBackend* backend, EEvent* e,u_int16_t mask)
{
#pragma unused(backend,e,mask)
    return 0;
}

static int carbon_gl_finish(EBackend* backend)
{
#pragma unused(backend)
    CarbonGLBackend* be = (CarbonGLBackend*) backend;

    free(be);
    return 0;
}

static int carbon_gl_pic_attach(EBackend* backend, EPixmap* pixmap)
{
    CarbonGLBackend* be = (CarbonGLBackend*) backend;

    if (pixmap->opaque != NULL)
	return -1;
    EObjectLink(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) 1;
    pixmap->backend = (EBackend*) be;
    return 0;
}

						
static int carbon_gl_pic_detach(EBackend* backend, EPixmap* pixmap)
{
    if (pixmap->opaque != NULL) {
	EObjectUnlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}


static int carbon_gl_pic_draw(EBackend* backend, EPixmap* pic, EWindow* ewin,
			      int off_screen,
			      int src_x, int src_y, int dst_x, int dst_y,
			      unsigned int width,
			      unsigned int height)
{
    Rect rectPort;
    GrafPtr portSave = NULL;
    CarbonGLWindow* cwin = (CarbonGLWindow*) ewin->opaque;
    (void) backend;

    if ((cwin == NULL) || (cwin->winRef == NULL))
	return -1;
    if (cwin->aglContext == NULL)
	carbon_gl_setup(ewin);
    if (cwin->aglContext == NULL)
	return -1;

    GetPort (&portSave);
    SetPort ((GrafPtr) GetWindowPort (cwin->winRef));

    aglSetCurrentContext(cwin->aglContext);
    aglUpdateContext (cwin->aglContext);

    GetWindowPortBounds(cwin->winRef, &rectPort);

    width = rectPort.right - rectPort.left; // find width
    height = rectPort.bottom - rectPort.top; // and height

    glViewport (0, 0, width, height);
    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();
    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity ();
    // glReportError ();
    // scale to port per pixel scale
    // glScalef (2.0f/width, -2.0f/height, 1.0f);
    // glReportError ();

    load_texture(pic, ewin, src_x, src_y, dst_x, dst_y, width, height);

    glEnable(GL_TEXTURE_RECTANGLE_EXT); // enable texturing
    glBindTexture (GL_TEXTURE_RECTANGLE_ARB, cwin->textureName);
    glBegin(GL_QUADS); {
	glTexCoord2f(0.0f, 0.0f);
	glVertex2f(-1.0f, 1.0f);
				
	glTexCoord2f(0.0f, (float)(height-1));
	glVertex2f(-1.0f, -1.0f);
				
	glTexCoord2f((float)(width-1), (float)(height-1));
	glVertex2f(1.0f, -1.0f);
				
	glTexCoord2f((float)(width-1), 0.0f);
	glVertex2f(1.0f, 1.0f);
	glEnd();
    }
    glDisable(GL_TEXTURE_RECTANGLE_EXT);
    glFlush();

    if (!off_screen)
	aglSwapBuffers (cwin->aglContext);
    SetPort (portSave); //reset port
    return 0;
}

static int carbon_gl_win_swap(EBackend* backend, EWindow* ewin)
{
    CarbonGLWindow* cwin = (CarbonGLWindow*) ewin->opaque;
    GrafPtr portSave = NULL;
    (void) backend;

    if ((cwin == NULL) || (cwin->winRef == NULL) || (cwin->aglContext == NULL))
	return -1;
    GetPort (&portSave);
    SetPort ((GrafPtr) GetWindowPort (cwin->winRef));
    aglSetCurrentContext(cwin->aglContext);
    aglUpdateContext (cwin->aglContext);

    aglSwapBuffers (cwin->aglContext);

    SetPort (portSave); //reset port
    return 0;
}


static int carbon_gl_win_attach(EBackend* backend, EWindow* ewin)
{
    CarbonGLBackend* be = (CarbonGLBackend*) backend;
    CarbonGLWindow* cwin;
    OSStatus err;
    Rect bounds;
    int winType;
    WindowAttributes winAttrs;
    WindowRef winRef;
/*
    static const EventTypeSpec    kWindowEvents[] =
    {
        { kEventClassCommand, kEventCommandProcess }
    };
*/

    if (ewin->opaque != NULL)
	return -1;

    if ((cwin = (CarbonGLWindow*) malloc(sizeof(CarbonGLWindow))) == NULL) 
	return -1;
    memset(cwin, 0, sizeof(CarbonGLWindow));

    bounds.left   = ewin->x;
    bounds.top    = ewin->y;
    bounds.bottom = ewin->y+ewin->height-1;
    bounds.right  = ewin->x+ewin->width-1;

    winType = kPlainWindowClass;
    winAttrs   = kWindowNoAttributes | kWindowStandardHandlerAttribute;
    /* modal window:
     * winType = kMovableModalWindowClass
     * attrs   = kWindowStandardHandlerAttribute
     */
    if ((err = CreateNewWindow(winType, winAttrs,
			       &bounds,
			       &winRef)) != noErr) {
	fprintf(stderr, "could not create window\n");
	return -1;
    }
    SetWRefCon(winRef, (long) ewin); // link to EWindow!

    cwin->winRef  = winRef;
    cwin->macPort = GetWindowPort(winRef);

/*    
    err = InstallWindowEventHandler( we->winRef, GetWindowEventHandlerUPP(),
				     GetEventTypeCount( kWindowEvents ), 
				     kWindowEvents,
				     we, NULL );
*/
    ShowWindow(cwin->winRef);
    SelectWindow(cwin->winRef);
    EObjectLink(&backend->window_list, ewin);
    ewin->opaque = (void*) cwin;
    ewin->backend = (EBackend*) be;

    carbon_gl_setup(ewin);

    return 0;
}

static int carbon_gl_win_detach(EBackend* backend, EWindow* ewin)
{
    CarbonGLWindow* cwin = (CarbonGLWindow*) ewin->opaque;

    if (cwin != NULL) {
	DisposeWindow(cwin->winRef);
	EObjectUnlink(&backend->window_list, ewin);
	ewin->opaque = NULL;
	ewin->backend = NULL;
    }
    return 0;
}

static OSStatus GLWindowEventHandler( EventHandlerCallRef inCaller, 
				    EventRef inEvent, void* inRefcon )
{
    OSStatus    err = eventNotHandledErr;
    /* WindowRef   window = (WindowRef) inRefcon; */
    (void) inCaller;
    (void) inRefcon;
    
    fprintf(stderr, "epic_macos_gl: WindowEventHandle\r\n");
	
    switch (GetEventClass(inEvent))   {
    case kEventClassCommand: {
	HICommandExtended cmd;
	verify_noerr( GetEventParameter( inEvent, kEventParamDirectObject, typeHICommand, NULL, sizeof( cmd ), NULL, &cmd ) );
            
	switch (GetEventKind( inEvent )) {
	case kEventCommandProcess:
	    switch ( cmd.commandID ) {
		// Add your own command-handling cases here
	    default:
		break;
	    }
	    break;
        default:
            break;
	}
	break;
    }

	
    default:
	break;
    }
    return err;
}

int carbon_gl_adjust(EBackend *backend, EDict* param)
{
    (void) backend;
    (void) param;

    return 1;
}

