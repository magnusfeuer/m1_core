/*
 *  X display driver 
 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/XKBlib.h>
#ifdef MAC_OS_X
#include <machine/endian.h>
#else
#include <endian.h>
#endif

#define EHANDLT_T int
#include "epic.h"

/* This structure is stored in EWindow opaque field */
typedef struct {
    Window window;
    Pixmap pm;     /* off-screen pixmap */
    GC gc;
    int height;
    int width;
    int grabbed;
    int accel_num;
    int accel_den;
    int thres;
} X11Window;

typedef struct {
    EBackend b;                /* DO NOT MOVE !!! */
    Display* display;
    Visual*  visual;
    Atom     wm_delete_window; /* atom WM_DELETE_WINDOW */
    unsigned short modstate;   /* modifier state */
    unsigned short grab_key;   /* grab key */
} X11Backend;


EBackend* x11_init(EDict* param);

static int x11_finish(EBackend*);
static int x11_pic_attach(EBackend*, EPixmap*);
static int x11_pic_detach(EBackend*, EPixmap*);
static int x11_pic_draw(EBackend*, EPixmap*, EWindow*,int off_screen,
			int src_x, int src_y, int dst_x, int dst_y,
			unsigned int width,
			unsigned int height);
static int x11_win_attach(EBackend*, EWindow*);
static int x11_win_detach(EBackend*, EWindow*);
static int x11_win_swap(EBackend*, EWindow*);
static EHANDLE_T x11_evt_attach(EBackend*);
static int x11_evt_detach(EBackend*);
static int x11_evt_read(EBackend*, EEvent*,u_int16_t mask);
static int x11_adjust(EBackend *backend, EDict* param);

/* util */
static void x11_grab(EBackend* backend, EWindow* window, int toggle);
static int  x11_error(Display * dpy, XErrorEvent * ev);


static EPicCallbacks x11_callbacks =
{
    x11_finish,
    x11_pic_attach,
    x11_pic_detach,
    x11_pic_draw,
    x11_win_attach,
    x11_win_detach,
    x11_evt_attach,
    x11_evt_detach,
    x11_evt_read,
    x11_adjust,
    x11_win_swap
};

#define NUM_LOCK_MASK    0x00000002
#define CAPS_LOCK_MASK   0x00000001
#define SCROLL_LOCK_MASK 0x00000004


static EWindow* find_event_window(X11Backend* x11, Window w)
{
    EWindow* window = x11->b.window_list;  /* list of attached windows */

    while(window) {
	X11Window* w11 = (X11Window*) window->opaque;
	if (w11->window == w)
	    return window;
	window = window->next;
    }
    return NULL;
}

#if 0 
// Not used right now but it's a fixme

/* scan trough a screen and locate a "nice" visual  */
static Visual* find_visual_of_screen(Screen* screen)
{
    int i;

    for (i = 0; i < screen->ndepths; i++) {
	int j;
	Depth* d = &screen->depths[i];

	for (j = 0; j < d->nvisuals; j++) {
	    Visual* v = &d->visuals[j];

	    (void) v; // Inhibits warning when DBG is not active.

	    EDBG("Visual: screen=%d, visual=%d", i, j);
	    EDBG("          depth=%d", d->depth);
	    EDBG("             id=%X", (unsigned int)v->visualid);
	    EDBG("       red_mask=%lX", v->red_mask);
	    EDBG("     green_mask=%lX", v->green_mask);
	    EDBG("      blue_mask=%lX", v->blue_mask);
	    EDBG("   bits_per_rgb=%d", v->bits_per_rgb);
	    EDBG("%s", "");
	}
    }
    return NULL;
}
#endif



EBackend* x11_init(EDict* param)
{
    X11Backend* x11;
    unsigned int state;
    Screen* screen;

    (void)  param;
    if ((x11 = (X11Backend*) malloc(sizeof(X11Backend))) == NULL)
	return NULL;
    EOBJECT_INIT((EBackend*)x11, EBACKEND_TYPE);
    if ((x11->display = XOpenDisplay(NULL)) == NULL) {
	free(x11);
	return NULL;
    }

    XSetErrorHandler(x11_error);
    screen = ScreenOfDisplay(x11->display, 0);

    x11->wm_delete_window = XInternAtom(x11->display,"WM_DELETE_WINDOW",False);

    /* Fixme locate a proper visual */
    /* x11->visual = find_visual_of_screen(screen); */
    x11->visual = XDefaultVisual(x11->display, 0);
    x11->modstate = 0;
    x11->grab_key = 0;

    x11->b.pending = 0;
    x11->b.cb = &x11_callbacks;
    x11->b.pixmap_list = NULL;
    x11->b.window_list = NULL;
    x11->b.event = INVALID_HANDLE;

    if(XkbGetIndicatorState (x11->display,XkbUseCoreKbd,&state) == Success) {
	EDBGFMT("initial modstate=%x", state);
	if (state & NUM_LOCK_MASK)
	    x11->modstate |= EKBD_MOD_NUM;
	if (state & CAPS_LOCK_MASK)
	    x11->modstate |= EKBD_MOD_CAPS;
	if (state & SCROLL_LOCK_MASK)
	    x11->modstate |= EKBD_MOD_SCR;
	EDBGFMT("x11 modstate=%x\n", x11->modstate);
    }
    return (EBackend*) &(x11->b);
}

/* return the backend event handle */
static EHANDLE_T x11_evt_attach(EBackend* backend)
{
    X11Backend* x11 = (X11Backend*) backend;
    int fd = ConnectionNumber(x11->display);

    return (EHANDLE_T) fd;
}

static int x11_evt_detach(EBackend* backend)
{
    (void) backend;
    return 0;
}

static int x11_finish(EBackend* backend)
{
    X11Backend* x11 = (X11Backend*) backend;

    XCloseDisplay(x11->display);
    free(x11);
    return 0;
}

static int x11_pic_attach(EBackend* backend, EPixmap* pixmap)
{
    X11Backend* x11 = (X11Backend*) backend;
    XImage* ximg;
    unsigned int bytesPerRow   = pixmap->bytesPerRow;
    unsigned int bitsPerPixel  = pixmap->bitsPerPixel;

    if (pixmap->opaque != NULL)
	return -1;

    if ((ximg = (XImage*) calloc(1,sizeof(XImage))) == NULL)
	return -1;

    ximg->width  = pixmap->width;
    ximg->height = pixmap->height;
    ximg->xoffset = 0;
    ximg->format = ZPixmap;
    ximg->data   = (char*) pixmap->data;
    ximg->bitmap_bit_order = MSBFirst;
    switch(pixmap->pixelType) {
    case EPIXEL_TYPE_RGB:
	ximg->byte_order  = MSBFirst;
	ximg->bitmap_unit = 8;
	ximg->bitmap_pad  = 8;
	break;
    case EPIXEL_TYPE_BGR:
	ximg->byte_order  = LSBFirst;
	ximg->bitmap_unit = 8;
	ximg->bitmap_pad  = 8;
	break;
    case EPIXEL_TYPE_RGBA:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = LSBFirst;
#else
	ximg->byte_order  = MSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    case EPIXEL_TYPE_ARGB:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = LSBFirst;
#else
	ximg->byte_order  = MSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    case EPIXEL_TYPE_BGRA:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = MSBFirst;
#else
	ximg->byte_order  = LSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    case EPIXEL_TYPE_ABGR:
#if BYTE_ORDER == BIG_ENDIAN
	ximg->byte_order  = LSBFirst;
#else
	ximg->byte_order  = MSBFirst;
#endif
	ximg->bitmap_unit = 32;
	ximg->bitmap_pad  = 32;
	break;
    default:
	break;
    }
    ximg->depth = 24;                     /* ...*/
    ximg->bytes_per_line = bytesPerRow;
    ximg->bits_per_pixel = bitsPerPixel;
    ximg->red_mask   = 0xFF << 16;
    ximg->green_mask = 0xFF <<  8;
    ximg->blue_mask  = 0xFF <<  0;

    XInitImage(ximg);

    EDBGFMT( "        width=%d",   ximg->width);
    EDBGFMT( "       height=%d",  ximg->height);
    EDBGFMT( "      xoffset=%d", ximg->xoffset);
    EDBGFMT( "       format=%s", 
	    ((ximg->format==XYBitmap)?"XYBitmap":
	     ((ximg->format==XYPixmap)?"XYPixmap":
	      ((ximg->format==ZPixmap)?"ZPixmap":"?"))));
    EDBGFMT(      "byte_order=%s", 
	    ( (ximg->byte_order==LSBFirst) ? "LSBFirst" :
	      ((ximg->byte_order==MSBFirst) ?  "MSBFirst" : "?")));
    EDBGFMT( "     bitmap_unit=%d", ximg->bitmap_unit);
    EDBGFMT( "bitmap_bit_order=%s", 
	    ( (ximg->bitmap_bit_order==LSBFirst) ? "LSBFirst" :
	      ((ximg->bitmap_bit_order==MSBFirst) ? "MSBFirst" : "?")));
    EDBGFMT( "     bitmap_pad=%d", ximg->bitmap_pad);
    EDBGFMT( "          depth=%d", ximg->depth);
    EDBGFMT( " bytes_per_line=%d", ximg->bytes_per_line);
    EDBGFMT( " bits_per_pixel=%d", ximg->bits_per_pixel);
    EDBGFMT( "       red_mask=%lX", ximg->red_mask);
    EDBGFMT( "     green_mask=%lX", ximg->green_mask);
    EDBGFMT( "      blue_mask=%lX", ximg->blue_mask);
    EDBGFMT( "         obdata=%p", ximg->obdata);

    EObjectLink(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) ximg;
    pixmap->backend = (EBackend*) x11;
    return 0;
}

static int x11_pic_detach(EBackend* backend, EPixmap* pixmap)
{
    XImage* ximg = (XImage*) pixmap->opaque;

    if (ximg != NULL) {
	free(ximg);
	EObjectUnlink(&backend->pixmap_list, pixmap);
	pixmap->opaque = NULL;
	pixmap->backend = NULL;
    }
    return 0;
}

static int x11_pic_draw(EBackend* backend, EPixmap* pixmap, EWindow* window,
			int off_screen,
			int src_x, int src_y, int dst_x, int dst_y,
			unsigned int width,
			unsigned int height)
{
    X11Backend* x11 = (X11Backend*) backend;
    X11Window*  w11 = (X11Window*) window->opaque;
    XImage* ximg = (XImage*) pixmap->opaque;

    if (w11 == NULL)
	return -1;
    else {
	Drawable d;
	d = (off_screen && w11->pm) ? w11->pm : w11->window;
	XPutImage(x11->display, d,
		  w11->gc, ximg, src_x, src_y, dst_x, dst_y,
		  width, height);
	if (!(off_screen && w11->pm)) {
	    XSync(x11->display, False);
	    backend->pending = XEventsQueued(x11->display, QueuedAlready);
	}
	return 0;
    }
}

static int x11_win_swap(EBackend* backend, EWindow* window)
{
    X11Backend* x11 = (X11Backend*) backend;
    X11Window*  w11 = (X11Window*) window->opaque;
    
    if (w11 && w11->pm) {
	XCopyArea(x11->display, w11->pm, w11->window, w11->gc,
		  0, 0, w11->width, w11->height, 0, 0);
	XSync(x11->display, False);
	backend->pending = XEventsQueued(x11->display, QueuedAlready);
    }
    return 0;
}

static int x11_win_attach(EBackend* backend, EWindow* window)
{
    X11Backend* x11 = (X11Backend*) backend;
    X11Window* w11;
    Window win;
    XSetWindowAttributes attr;
    unsigned long valuemask;
    unsigned int event_mask = StructureNotifyMask | FocusChangeMask;

    if (window->opaque != NULL)
	return -1;
    if ((w11 = (X11Window*) malloc(sizeof(X11Window))) == NULL)
	return -1;

    /* Handle? ExposureMask,ColormapChangeMask, FocusChangeMask | */
    if (window->mask & EEVENT_KEY_PRESS) event_mask |= KeyPressMask;
    if (window->mask & EEVENT_KEY_RELEASE) event_mask |= KeyReleaseMask;
    if (window->mask & EEVENT_POINTER_MOTION) event_mask |= PointerMotionMask;
    if (window->mask & EEVENT_BUTTON_PRESS) event_mask |= ButtonPressMask;
    if (window->mask & EEVENT_BUTTON_RELEASE) event_mask |= ButtonReleaseMask;
    if (window->mask & EEVENT_FOCUS_IN) event_mask |= FocusChangeMask;
    if (window->mask & EEVENT_FOCUS_OUT) event_mask |= FocusChangeMask;

    attr.backing_store = Always;        /* auto expose */
    attr.save_under = True;		/* popups ... */
    attr.event_mask = event_mask;

    valuemask = CWSaveUnder | CWEventMask | CWBackingStore;

    win = XCreateWindow(x11->display, 
			XDefaultRootWindow(x11->display), 
			window->x, /* x */
			window->y,	/* y */
			window->width,	/* width */
			window->height,	/* height */
			2,		/* border */
			CopyFromParent,	/* depth */
			InputOutput,	/* class */
			x11->visual,	/* Visual */
			valuemask,	/* valuemask */
			&attr		/* attributes */
	);

    if (win != 0) {
	w11->grabbed = 0;
	w11->accel_num = 0;
	w11->accel_den = 0;
	w11->thres = 0;
	w11->window = win;
	w11->width = window->width;
	w11->height = window->height;
	/* FIXME: allocate this ? */
	w11->gc = XDefaultGC(x11->display, 0);
	/* FIXME: configurable */
	w11->pm = XCreatePixmap(x11->display,
				win,  /* used to specify screen */
				window->width,
				window->height,
				DefaultDepth(x11->display, 0));
	if (!w11->pm) 
	    fprintf(stderr, "epic_x11: Failed create of Pixmap\n");

	XSetWMProtocols(x11->display, win, &x11->wm_delete_window, 1);

	XMapWindow(x11->display, win);
	XSync(x11->display, False);
	EObjectLink(&backend->window_list, window);
	window->opaque  = (void*) w11;
	window->backend = (EBackend*) x11;
	return 0;
    }
    free(w11);
    return -1;
}

static int x11_win_detach(EBackend* backend, EWindow* window)
{
    X11Backend* x11 = (X11Backend*) backend;
    X11Window*  w11 = (X11Window*) window->opaque;
    
    if ((w11 != NULL) && (w11->window != 0)) {
	EDBGFMT("XUnmapWindow");
	XUnmapWindow(x11->display, w11->window);
	EDBGFMT("XDestroyWindow");
	if (w11->pm)
	    XFreePixmap(x11->display, w11->pm);
	XDestroyWindow(x11->display, w11->window);
	XFlush(x11->display);
	/* Ungrabb? */
	free(w11);
	EObjectUnlink(&backend->window_list, window);
	window->opaque  = NULL;
	window->backend = NULL;
	return 0;
    }
    return -1;
}

/* Process mouse and keybord events, called from driver_select. 
 * return -1: error in event processing
 *         0: no event returned  & no pending
 *         1: one event returned & no pending
 *         2: one event returned & 1 pending
 *         and so on
 */

static int x11_evt_read(EBackend* backend, EEvent* e,u_int16_t mask)
{
    X11Backend* x11 = (X11Backend*) backend;
    EWindow* win;
    XEvent ev;
    unsigned int event_mask = 0;

    if (mask & EEVENT_KEY_PRESS) event_mask |= KeyPressMask;
    if (mask & EEVENT_KEY_RELEASE) event_mask |= KeyReleaseMask;
    if (mask & EEVENT_POINTER_MOTION) event_mask |= PointerMotionMask;
    if (mask & EEVENT_BUTTON_PRESS) event_mask |= ButtonPressMask;
    if (mask & EEVENT_BUTTON_RELEASE) event_mask |= ButtonReleaseMask;
    if (mask & EEVENT_FOCUS_IN) event_mask |= FocusChangeMask;
    if (mask & EEVENT_FOCUS_OUT) event_mask |= FocusChangeMask;

    backend->pending = 0;

next:
    /* if (XNextEvent(x11->display, &ev) < 0) */
    if (!XCheckMaskEvent(x11->display, event_mask, &ev))
	return -1;
    switch (ev.type) {
    case DestroyNotify:
	EDBGFMT("Event: DestroyNotify");
	if ((win = find_event_window(x11, ev.xdestroywindow.window)) != NULL) {
	    e->type = EEVENT_DESTROYED;
	    e->window = win;
	    goto got_event;
	}
	break;

    case ClientMessage:
	EDBGFMT("Event: ClientMessage");
	if ((ev.xclient.format == 32) &&
	    (ev.xclient.data.l[0] == (int) x11->wm_delete_window)) {
	    if ((win = find_event_window(x11, ev.xclient.window)) != NULL) {
		EDBGFMT("Event: CLOSE");
		e->type = EEVENT_CLOSE;
		e->window = win;
		goto got_event;
	    }
	}
	break;

    case FocusIn:
    case FocusOut:
	if ((win = find_event_window(x11, ev.xfocus.window)) != NULL) {
	    if ((ev.type == FocusIn) && (win->mask & EEVENT_FOCUS_IN)) {
		e->window = win;
		e->type = EEVENT_FOCUS_IN;
		goto got_event;
	    }
	    else if ((ev.type == FocusOut) && (win->mask & EEVENT_FOCUS_OUT)) {
		e->window = win;
		e->type = EEVENT_FOCUS_OUT;
		goto got_event;
	    }
	}
	break;

    case MotionNotify:
	if ((win = find_event_window(x11, ev.xmotion.window)) != NULL) {
	    int button = 0;
	    if (ev.xmotion.state & Button1Mask) button |= EBUT_LEFT;
	    if (ev.xmotion.state & Button2Mask) button |= EBUT_MIDDLE;
	    if (ev.xmotion.state & Button3Mask) button |= EBUT_RIGHT;
	    e->type = EEVENT_POINTER_MOTION;
	    e->window = win;
	    e->pointer.button = button;
	    e->pointer.x = ev.xmotion.x;
	    e->pointer.y = ev.xmotion.y;
	    e->pointer.z = 0;
	    goto got_event;
	}
	break;

    case ButtonPress:	
    case ButtonRelease:
	if ((win = find_event_window(x11, ev.xbutton.window)) != NULL) {
	    int button   = 0;
	    int held = 0;
	    switch(ev.xbutton.button) {
	    case 1: button = EBUT_LEFT;   break;
	    case 2: button = EBUT_MIDDLE; break;
	    case 3: button = EBUT_RIGHT;  break;
	    default: break;
	    }
	    /* Get any other buttons that might be already held */
	    if (ev.xbutton.state & Button1Mask)   held |= EBUT_LEFT;
	    if (ev.xbutton.state & Button2Mask)   held |= EBUT_MIDDLE;
	    if (ev.xbutton.state & Button3Mask)   held |= EBUT_RIGHT;

	    if (ev.type == ButtonPress) {
		e->pointer.button =  button;
		e->type = EEVENT_BUTTON_PRESS;
	    }
	    else {
		e->pointer.button = button;
		e->type = EEVENT_BUTTON_RELEASE;
	    }
	    e->window = win;
	    e->pointer.x = ev.xbutton.x;
	    e->pointer.y = ev.xbutton.y;
	    e->pointer.z = 0;
	    goto got_event;
	}
	break;

    case KeyPress:
    case KeyRelease: {
	char ignored_char;
	KeySym sym = XKeycodeToKeysym(x11->display, ev.xkey.keycode, 0);
	
	if (sym == NoSymbol) {
	    EDBGFMT("event sym=NoSymbol");
	    goto ignore_event;
	}
	if ((win = find_event_window(x11, ev.xkey.window)) == NULL) {
	    EDBGFMT("KPress/KeyRelase: event window not found %x", 
		   (int)ev.xkey.window);
	    goto ignore_event;
	}
	if (ev.type == KeyPress)
	    e->type = EEVENT_KEY_PRESS;
	else
	    e->type = EEVENT_KEY_RELEASE;
	e->window = win;

	/* calculate kbd modifiers*/
	if (x11->modstate) {
	    EDBGFMT("1:modstate=%x", x11->modstate);
	}
	x11->modstate &= (EKBD_MOD_NUM|EKBD_MOD_CAPS|EKBD_MOD_SCR);
	if (ev.xkey.state) {
	    EDBGFMT("key.state=%x", ev.xkey.state);
	}
	if (ev.xkey.state & ShiftMask) {
	    EDBGFMT("shiftmask");
	    x11->modstate |= EKBD_MOD_SHIFT;
	}
	if (ev.xkey.state & LockMask) {
	    EDBGFMT("lockmask (caps)");
	    x11->modstate |= EKBD_MOD_CAPS;
	}
	if (ev.xkey.state & ControlMask) {
	    EDBGFMT("controlmask");
	    x11->modstate |= EKBD_MOD_CTRL;
	}
	if (ev.xkey.state & Mod1Mask) {
	    EDBGFMT("mod1mask (alt)");
	    x11->modstate |= EKBD_MOD_ALT;
	}
	if (ev.xkey.state & Mod2Mask) {
	    EDBGFMT("mod2mask (num)");
	    x11->modstate |= EKBD_MOD_NUM;
	}
	if (ev.xkey.state & Mod3Mask) {
	    EDBGFMT("mod3mask");
	}
	if (ev.xkey.state & Mod4Mask) {
	    EDBGFMT("mod4mask");
	}
	if (ev.xkey.state & Mod5Mask) {
	    EDBGFMT("mod5mask (scr)");
	    x11->modstate |= EKBD_MOD_SCR;
	}
	if (x11->modstate) {
	    EDBGFMT("2:modstate=%x", x11->modstate);
	}
	switch (sym) {
	case XK_Escape: e->key.sym = EKBD_KEY_ESCAPE; break;
	case XK_Delete: e->key.sym = EKBD_KEY_DELETE; break;
	case XK_Home:   e->key.sym = EKBD_KEY_HOME;   break;
	case XK_Left:   e->key.sym = EKBD_KEY_LEFT;   break;
	case XK_Up:     e->key.sym = EKBD_KEY_UP;     break;
	case XK_Right:  e->key.sym = EKBD_KEY_RIGHT;  break;
	case XK_Down:   e->key.sym = EKBD_KEY_DOWN;   break;
	case XK_Page_Up: e->key.sym = EKBD_KEY_PAGEUP; break;
	case XK_Page_Down: e->key.sym = EKBD_KEY_PAGEDOWN; break;
	case XK_End: e->key.sym = EKBD_KEY_END;  break;
	case XK_Insert: e->key.sym = EKBD_KEY_INSERT; break;
	case XK_Pause:
	case XK_Break: e->key.sym = EKBD_KEY_QUIT; break;
	case XK_Print:
	case XK_Sys_Req: e->key.sym = EKBD_KEY_PRINT; break;
	case XK_Menu: e->key.sym = EKBD_KEY_MENU; break;
	case XK_Cancel: e->key.sym = EKBD_KEY_CANCEL; break;
	case XK_KP_Enter: e->key.sym = EKBD_KEY_KP_ENTER; break;
	case XK_KP_Home: e->key.sym = EKBD_KEY_KP7; break;
	case XK_KP_Left: e->key.sym = EKBD_KEY_KP4; break;
	case XK_KP_Up:   e->key.sym = EKBD_KEY_KP8; break;
	case XK_KP_Right: e->key.sym = EKBD_KEY_KP6; break;
	case XK_KP_Down:  e->key.sym = EKBD_KEY_KP2; break;
	case XK_KP_Page_Up: e->key.sym = EKBD_KEY_KP9; break;
	case XK_KP_Page_Down: e->key.sym = EKBD_KEY_KP3; break;
	case XK_KP_End: e->key.sym = EKBD_KEY_KP1; break;
	case XK_KP_Insert: e->key.sym = EKBD_KEY_KP0; break;
	case XK_KP_Delete: e->key.sym = EKBD_KEY_KP_PERIOD; break;
	case XK_KP_Equal:  e->key.sym = EKBD_KEY_KP_EQUALS; break;
	case XK_KP_Multiply: e->key.sym = EKBD_KEY_KP_MULTIPLY; break;
	case XK_KP_Add: e->key.sym = EKBD_KEY_KP_PLUS; break;
	case XK_KP_Subtract: e->key.sym = EKBD_KEY_KP_MINUS; break;
	case XK_KP_Decimal: e->key.sym = EKBD_KEY_KP_PERIOD; break;
	case XK_KP_Divide: e->key.sym = EKBD_KEY_KP_DIVIDE; break;
	case XK_KP_5:
	case XK_KP_Begin: e->key.sym = EKBD_KEY_KP5; break;
	case XK_F1:  e->key.sym = EKBD_KEY_F1;  break;
	case XK_F2:  e->key.sym = EKBD_KEY_F2;  break;
	case XK_F3:  e->key.sym = EKBD_KEY_F3;  break;
	case XK_F4:  e->key.sym = EKBD_KEY_F4;  break;
	case XK_F5:  e->key.sym = EKBD_KEY_F5;  break;
	case XK_F6:  e->key.sym = EKBD_KEY_F6;  break;
	case XK_F7:  e->key.sym = EKBD_KEY_F7;  break;
	case XK_F8:  e->key.sym = EKBD_KEY_F8;  break;
	case XK_F9:  e->key.sym = EKBD_KEY_F9;  break;
	case XK_F10: e->key.sym = EKBD_KEY_F10; break;
	case XK_F11: e->key.sym = EKBD_KEY_F11; break;
	case XK_F12: e->key.sym = EKBD_KEY_F12;  break;
	    /* state modifiers*/
	case XK_Num_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		x11->modstate ^= EKBD_MOD_NUM;
	    goto no_key;
	case XK_Shift_Lock:
	case XK_Caps_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		x11->modstate ^= EKBD_MOD_CAPS;
	    goto no_key;
	case XK_Scroll_Lock:
	    /* not sent, used only for state*/
	    if (ev.xkey.type == KeyRelease)
		x11->modstate ^= EKBD_MOD_SCR;
	    goto no_key;
	case XK_Shift_L:
	    EDBGFMT("shift left");
	    e->key.sym = EKBD_KEY_LSHIFT; 
	    break;
	case XK_Shift_R: 
	    EDBGFMT("shift right");
	    e->key.sym = EKBD_KEY_RSHIFT; 
	    break;
	case XK_Control_L: 
	    EDBGFMT("control left");
	    e->key.sym = EKBD_KEY_LCTRL; 
	    break;
	case XK_Control_R: 
	    EDBGFMT("control right");
	    e->key.sym = EKBD_KEY_RCTRL;
	    break;
	case XK_Alt_L: e->key.sym = EKBD_KEY_LALT; break;
	case XK_Alt_R: e->key.sym = EKBD_KEY_RALT; break;
	case XK_Meta_L:
	case XK_Super_L:
	case XK_Hyper_L: e->key.sym = EKBD_KEY_LMETA; break;
	case XK_Meta_R:
	case XK_Super_R:
	case XK_Hyper_R: e->key.sym = EKBD_KEY_RMETA; break;
	default:
	    switch (sym) {
	    case XK_BackSpace:
	    case XK_Tab:
	    case XK_Return:
		break;
	    default:
		if (sym & 0xFF00) {
		    EDBGFMT("Unhandled X11 keysym: %04x\n", (int)sym);
		}
	    }
	    XLookupString(&ev.xkey, &ignored_char, 1, &sym, NULL );
	    if (x11->modstate & EKBD_MOD_CTRL)
		e->key.sym = sym & 0x1f;	/* Control code */ 
	    else
		e->key.sym = sym & 0xff;	/* ASCII*/
	    break;
	}

	if (x11->modstate & EKBD_MOD_NUM) {
	    switch (e->key.sym) {
	    case EKBD_KEY_KP0:
	    case EKBD_KEY_KP1:
	    case EKBD_KEY_KP2:
	    case EKBD_KEY_KP3:
	    case EKBD_KEY_KP4:
	    case EKBD_KEY_KP5:
	    case EKBD_KEY_KP6:
	    case EKBD_KEY_KP7:
	    case EKBD_KEY_KP8:
	    case EKBD_KEY_KP9:
		e->key.sym = (e->key.sym - EKBD_KEY_KP0) + '0';
		break;
	    case EKBD_KEY_KP_PERIOD: e->key.sym = '.'; break;
	    case EKBD_KEY_KP_DIVIDE: e->key.sym = '/'; break;
	    case EKBD_KEY_KP_MULTIPLY: e->key.sym = '*'; break;
	    case EKBD_KEY_KP_MINUS: e->key.sym = '-'; break;
	    case EKBD_KEY_KP_PLUS:  e->key.sym = '+'; break;
	    case EKBD_KEY_KP_ENTER:
		e->key.sym = EKBD_KEY_ENTER;
		break;
	    case EKBD_KEY_KP_EQUALS: e->key.sym = '-'; break;
		default: break;

	    }
	}
	if (x11->grab_key == e->key.sym)
	    x11_grab(backend, NULL, (ev.xkey.state & ControlMask));
	e->key.code = ev.xkey.keycode;
	e->key.mod  = x11->modstate;
	goto got_event;
    no_key:
	EDBGFMT("3:modstate=%x", x11->modstate);
	goto ignore_event;
    }

    default:
	goto ignore_event;
    }

ignore_event:
    if (XEventsQueued(x11->display, QueuedAlready))
	goto next;
    return 0;

got_event:
    backend->pending = XEventsQueued(x11->display, QueuedAlready);
    return 1+backend->pending;
}


static void x11_grab(EBackend* backend, EWindow* wptr, int toggle)
{
    X11Backend* b = (X11Backend*) backend;
    X11Window* w = (X11Window*) wptr;

    if (toggle) {
	/* toggle grab control */
	if (w->grabbed) {
	    XUngrabPointer(b->display, CurrentTime);
	    XUngrabKeyboard(b->display, CurrentTime);
	    XChangePointerControl(b->display,
				  True, False,
				  w->accel_num, 
				  w->accel_den, 0);
	    w->grabbed = 0;
	}
	else {
	    /* save pointer config */
	    XGetPointerControl(b->display, 
			       &w->accel_num, 
			       &w->accel_den,
			       &w->thres);
	    XChangePointerControl(b->display, True, False, 1, 1, 0);
	    XGrabKeyboard(b->display, w->window,
			  True,  /* only to this window */
			  GrabModeAsync, GrabModeAsync, CurrentTime);
	    XGrabPointer(b->display, w->window, False,
			 PointerMotionMask | ButtonPressMask,
			 GrabModeAsync, GrabModeAsync, None, None,
			 CurrentTime);
	    w->grabbed = 1;
	}
    }
    else if (w->grabbed)
	XChangePointerControl(b->display, True, False,
			      w->accel_num, 
			      w->accel_den, 0);
}

static int x11_error(Display * dpy, XErrorEvent * ev)
{
    char err_string[256];

    XGetErrorText(dpy, ev->error_code, err_string, 256);
    fprintf(stderr, "X11 error: %s\r\n", err_string);
    return 0;
}


int x11_adjust(EBackend* backend, EDict* param)
{
    (void) backend;
    (void) param;
    return 1;
}
