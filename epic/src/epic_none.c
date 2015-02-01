/*
 *  Template display driver 
 */

#include "epic.h"

/* This structure is stored in EWindow opaque field */
typedef struct {
    int wstate;
    unsigned long dcount;
} NoneWindow;

typedef struct {
    EBackend b;                /* DO NOT MOVE !!! */
} NoneBackend;


EBackend* none_init(EDict* param);

static int none_finish(EBackend*);
static int none_pic_attach(EBackend*, EPixmap*);
static int none_pic_detach(EBackend*, EPixmap*);
static int none_pic_draw(EBackend*, EPixmap*, EWindow*, int off_screen,
			int src_x, int src_y, int dst_x, int dst_y,
			unsigned int width,
			unsigned int height);
static int none_win_attach(EBackend*, EWindow*);
static int none_win_detach(EBackend*, EWindow*);
static int none_win_swap(EBackend*, EWindow*);
static EHANDLE_T none_evt_attach(EBackend*);
static int none_evt_detach(EBackend*);
static int none_evt_read(EBackend*, EEvent*, u_int16_t mask);
static int none_adjust(EBackend *backend, EDict* param);


static EPicCallbacks none_callbacks =
{
    none_finish,
    none_pic_attach,
    none_pic_detach,
    none_pic_draw,
    none_win_attach,
    none_win_detach,
    none_evt_attach,
    none_evt_detach,
    none_evt_read,
    none_adjust,
    none_win_swap
};


EBackend* none_init(EDict* param)
{
    NoneBackend* none;

    (void) param;

    if ((none = (NoneBackend*) malloc(sizeof(NoneBackend))) == NULL)
	return NULL;
    EOBJECT_INIT((EBackend*)none, EBACKEND_TYPE);

    none->b.pending = 0;
    none->b.cb = &none_callbacks;
    none->b.pixmap_list = NULL;
    none->b.window_list = NULL;
    none->b.event = INVALID_HANDLE;

    return (EBackend*) &(none->b);
}

/* return the backend event handle */
static EHANDLE_T none_evt_attach(EBackend* backend)
{
    (void) backend;
    return INVALID_HANDLE;
}

static int none_evt_detach(EBackend* backend)
{
    (void) backend;
    return 0;
}

static int none_finish(EBackend* backend)
{
    NoneBackend* none = (NoneBackend*) backend;

    free(none);
    return 0;
}

static int none_pic_attach(EBackend* backend, EPixmap* pixmap)
{
    NoneBackend* none = (NoneBackend*) backend;

    if (pixmap->opaque != NULL)
	return -1;
    EObjectLink(&backend->pixmap_list, pixmap);
    pixmap->opaque = (void*) 1;
    pixmap->backend = (EBackend*) none;
    return 0;
}

static int none_pic_detach(EBackend* backend, EPixmap* pixmap)
{
    EObjectUnlink(&backend->pixmap_list, pixmap);
    pixmap->opaque = NULL;
    pixmap->backend = NULL;
    return 0;
}

static int none_pic_draw(EBackend* backend, EPixmap* pixmap, EWindow* ewin,
			 int off_screen,
			 int src_x, int src_y, int dst_x, int dst_y,
			 unsigned int width,
			 unsigned int height)
{
    NoneWindow*  nwin = (NoneWindow*) ewin->opaque;
    (void) backend;
    (void) pixmap;
    (void) off_screen;
    (void) src_x;
    (void) src_y;
    (void) dst_x;
    (void) dst_y;
    (void) width;
    (void) height;

    if (nwin == NULL)
	return -1;
    nwin->dcount++;
    return 0;
}

static int none_win_swap(EBackend* backend, EWindow* window)
{
    (void) backend;
    (void) window;
    return 0;
}

static int none_win_attach(EBackend* backend, EWindow* ewin)
{
    NoneBackend* none = (NoneBackend*) backend;
    NoneWindow*  nwin;

    if (ewin->opaque != NULL)
	return -1;
    if ((nwin = (NoneWindow*) malloc(sizeof(NoneWindow))) == NULL)
	return -1;
    nwin->wstate = 1;
    nwin->dcount = 0;
    EObjectLink(&backend->window_list, ewin);
    ewin->opaque  = (void*) nwin;
    ewin->backend = (EBackend*) none;
    return 0;
}

static int none_win_detach(EBackend* backend, EWindow* ewin)
{
    NoneBackend* none = (NoneBackend*) backend;
    NoneWindow*  win  = (NoneWindow*)  ewin->opaque;
    
    if ((none != NULL) && (win->wstate != 0)) {
	free(win);
	EObjectUnlink(&backend->window_list, ewin);
	ewin->opaque  = NULL;
	ewin->backend = NULL;
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

static int none_evt_read(EBackend* backend, EEvent* e, u_int16_t mask)
{
    (void) backend;
    (void) e;
    (void) mask;

    return -1;
}

int none_adjust(EBackend *be, EDict*dict)
{
    (void) be;
    (void) dict;
    return 1;
}
