/*
 * EPIC Erlang PICtures
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <memory.h>
#include <math.h>
#include <errno.h>

#ifdef debug
// #define hard_debug
#endif

#include "erl_driver.h"

/* Must be define before include epic.h  */
#define EHANDLE_T ErlDrvEvent
#include "epic.h"
#include "Data.h"

#define EPIC_NOOP               0x0001

#define EBACKEND_CREATE         0x1000
#define EBACKEND_DESTROY        0x1001
#define EBACKEND_LIST           0x1011

#define EWINDOW_CREATE          0x2000
#define EWINDOW_DESTROY         0x2001
#define EWINDOW_ATTACH          0x2002
#define EWINDOW_DETACH          0x2003


#define EPIXMAP_CREATE          0x3000
#define EPIXMAP_DESTROY         0x3001
#define EPIXMAP_ATTACH          0x3002
#define EPIXMAP_DETACH          0x3003
#define EPIXMAP_FILL            0x3011
#define EPIXMAP_COPY            0x3012
#define EPIXMAP_DRAW            0x3013
#define EPIXMAP_PUT_PIXEL       0x3014
#define EPIXMAP_GET_PIXEL       0x3015
#define EPIXMAP_COPY_AREA       0x3016
#define EPIXMAP_DRAW_RECTANGLE  0x3017
#define EPIXMAP_DRAW_LINE       0x3019
#define EPIXMAP_DRAW_ELLIPSE    0x301C
#define EPIXMAP_PUT_PIXELS      0x301E
#define EPIXMAP_SCROLL          0x301F
#define EPIXMAP_TEX_LINE        0x3020
#define EPIXMAP_DRAW_TRIANGLE   0x3022
#define EPIXMAP_DRAW_POINT      0x3025

#define EBITMAP_CREATE          0x4000
#define EBITMAP_DESTROY         0x4001
#define EBITMAP_FILL            0x4011
#define EBITMAP_COPY            0x4012
#define EBITMAP_DRAW            0x4013
#define EBITMAP_PUT_BIT         0x4014
#define EBITMAP_GET_BIT         0x4015
#define EBITMAP_COPY_AREA       0x4016
#define EBITMAP_DRAW_RECTANGLE  0x4017
#define EBITMAP_DRAW_LINE       0x4019
#define EBITMAP_DRAW_CIRCLE     0x401A
#define EBITMAP_DRAW_ELLIPSE    0x401C
#define EBITMAP_PUT_BITS        0x401E
#define EBITMAP_SCROLL          0x401F

#define EGC_CREATE              0x5000
#define EGC_DESTROY             0x5001
#define EGC_COPY                0x5002

#define EGC_SET                 0x5012
#define EGC_GET                 0x5013
// GC SET/GET Selector

#define EGC_FOREGROUND_COLOR    1
#define EGC_BACKGROUND_COLOR    2

#define EGC_FILL_STYLE          10
#define EGC_FILL_COLOR          11
#define EGC_FILL_TEXTURE        12

#define EGC_LINE_STYLE          20
#define EGC_LINE_JOIN_STYLE     21
#define EGC_LINE_CAP_STYLE      22
#define EGC_LINE_WIDTH          23
#define EGC_LINE_TEXTURE        24

#define EGC_BORDER_STYLE        30
#define EGC_BORDER_JOIN_STYLE   31
#define EGC_BORDER_CAP_STYLE    32
#define EGC_BORDER_WIDTH        33
#define EGC_BORDER_TEXTURE      34
#define EGC_BORDER_COLOR        35




#define EDICT_CREATE              0x6000
#define EDICT_DESTROY             0x6001
#define EDICT_COPY                0x6002

#define EDICT_SET                 0x6010
#define EDICT_UNSET               0x6011
#define EDICT_GET                 0x6012


static ErlDrvEntry  epic_drv;

#define REPLY_OK     1
#define REPLY_ERROR  2
#define REPLY_EVENT  3

/* environment */
typedef struct epic_env {
    ErlDrvPort port;          /* port reference */    
    EBackend*  backend_list;  /* list of backends */
    EHash      objref;       /* Object references */
    /* Display Lists */
} EPicEnv;



static int        epic_drv_init(void);
static void       epic_drv_finish(void);
static void       epic_drv_stop(ErlDrvData);
static void       epic_drv_command(ErlDrvData, char*, int);
static void       epic_drv_commandv(ErlDrvData, ErlIOVec*);
static void       epic_drv_input(ErlDrvData, ErlDrvEvent);
static void       epic_drv_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData epic_drv_start(ErlDrvPort, char* command);
static int        epic_drv_ctl(ErlDrvData, unsigned int, char*, int, char**, int);
static void       epic_drv_timeout(ErlDrvData);

/* convert object to handle */
#define EOBJ_HANDLE(ptr) (((u_int32_t)(ptr))>>2)

/* lookup or install address and return a handle */
static u_int32_t epic_handle(EPicEnv* env, EObject* obj)
{
    u_int32_t handle = EOBJ_HANDLE(obj);
    if (handle != 0)
	EHashInsert(&env->objref, (void*)handle, (void*)obj);
    return handle;
}

static inline EObject* epic_object(EPicEnv* env, u_int32_t handle, 
				   EObjectType_t type)
{
    EObject* obj = (EObject*) EHashLookup(&env->objref, (void*) handle);
    if (obj && (obj->type == type))
	return obj;
    return NULL;
}

static inline EPixmap* epixmap_object(EPicEnv* env, u_int32_t handle)
{
    return (EPixmap*) epic_object(env, handle, EPIXMAP_TYPE);
}

static inline EBitmap* ebitmap_object(EPicEnv* env, u_int32_t handle)
{
    return (EBitmap*) epic_object(env, handle, EBITMAP_TYPE);
}

static inline EWindow* ewindow_object(EPicEnv* env, u_int32_t handle)
{
    return (EWindow*) epic_object(env, handle, EWINDOW_TYPE);
}

static inline EBackend* ebackend_object(EPicEnv* env, u_int32_t handle)
{
    return (EBackend*) epic_object(env, handle, EBACKEND_TYPE);
}

static inline EGc* egc_object(EPicEnv* env, u_int32_t handle)
{
    return (EGc*) epic_object(env, handle, EGC_TYPE);
}

static inline EDict* edict_object(EPicEnv* env, u_int32_t handle)
{
    return (EDict*) epic_object(env, handle, EDICT_TYPE);
}

static EBackend* find_event_backend(EPicEnv* env, ErlDrvEvent e)
{
    EBackend* bp = env->backend_list;

    while((bp != NULL) && (bp->event != e))
	bp = bp->next;
    return bp;
}

static hash_value_t objref_hash(void* key)
{
    return (hash_value_t) key;
}

static int objref_cmp(void* key, void* data)
{
    if ((void*)(((u_int32_t)key)<<2) == data)
	return 0;
    return 1;
}

static void objref_release(void *data)
{
    EDBGFMT("objref_release: ptr=%p, type=%d", data, ((EObject*) data)->type);
    EObjectUnRef(data);
}

/* pixel in argb format */
int get_pixel(Data* arg, EPixel_t* color)
{
    return get_uint32(arg, &color->px);
}

int get_dict_data(Data* arg, EDictData* d, char* buf)
{
    u_int8_t tag;

    if (!get_uint8(arg, &tag))
	return 0;

    switch(tag) {
    case STRING1: {
	u_int8_t len;
	if (!get_uint8(arg,&len)) 
	    return 0;
	d->type = EDICT_STRING;
	d->len  = len;
	d->ptr  = arg->ptr;
	arg->ptr += len;
	return 1;
    }
    case STRING4: {
	u_int32_t len;
	if (!get_uint32(arg,&len)) 
	    return 0;
	d->type = EDICT_STRING;
	d->len  = len;
	d->ptr  = arg->ptr;
	arg->ptr += len;
	return 1;
    }

    case BINARY: {
	u_int32_t len;
	if (!get_uint32(arg,&len)) 
	    return 0;
	d->type = EDICT_BINARY;
	d->len  = len;
	d->ptr  = arg->ptr;
	arg->ptr += len;
	return 1;
    }

    case FLOAT64: {
	double f;
	if (!get_float64(arg, &f))
	    return 0;
	memcpy(buf, (void*)&f, sizeof(f));
	d->type = EDICT_FLOAT;
	d->len  = sizeof(f);
	d->ptr  = buf;
	return 1;
    }

    case FLOAT32: {
	float f0;
	double f;
	if (!get_float32(arg, &f0))
	    return 0;
	f = f0;
	memcpy(buf, (void*)&f, sizeof(f));
	d->type = EDICT_FLOAT;
	d->len  = sizeof(f);
	d->ptr  = buf;
	return 1;
    }

    case INT32: {
	int32_t i;

	if (!get_uint32(arg, (u_int32_t*)&i))
	    return 0;
	memcpy(buf, (void*)&i, sizeof(i));
	d->type = EDICT_INTEGER;
	d->len  = sizeof(i);
	d->ptr  = buf;
	return 1;	    
    }

    case UINT32: {
	u_int32_t u;

	if (!get_uint32(arg, &u))
	    return 0;
	memcpy(buf, (void*)&u, sizeof(u));
	d->type = EDICT_INTEGER;
	d->len  = sizeof(u);
	d->ptr  = buf;
	return 1;	    
    }
    default:
	return 0;
    }
}

void put_dict_data(Data* out, EDictData* d)
{
    switch(d->type) {
    case EDICT_ANY: {
	put_atom(out, "any");
	break;
    }
    case EDICT_INTEGER: {
	u_int32_t v = *((u_int32_t*) d->ptr);
	put_int32(out, (int32_t)v);
	break;
    }
    case EDICT_FLOAT: {	
	double v = *((double*) d->ptr);
	put_float64(out, v);
	break;
    }
    case EDICT_STRING: {
	put_string(out, d->ptr, d->len);
	break;
    }
    case EDICT_BINARY: {
	put_binary(out, d->ptr, d->len);
	break;	
    }
    default:
	break;
    }
}

/* format event 
 *
 * {eevent, Window, {key_press,  Sym, Mod, Code}}
 * {eevent, Window, {key_release, Sym, Mod, Code}}
 *
 * {eevent, Window, {button_press,   Button, Where={X, Y, Z}}
 * {eevent, Window, {button_release, Button, Where={X, Y, Z}}}
 * {eevent, Window, {motion,         Button, Where={X, Y, Z}}}
 *
 * {eevent, Window, destroy }
 *
 *   Button = [left,middle,right]
 *   Mod = [shift-left,shift-right,  (+ shift)
 *          ctrl-left, ctrl-right,   (+ ctrl) 
 *          alt-left, alt-right,     (+ alt)
 *          meta-left, meta-right    (+ meta)
 *          num, caps, altgr, scr]
 * APPLE key?
 *
 *   Sym = UNICODE-KEY | symbol name (atom)
 *   Code = integer
 */

/* translate sym speical into atoms */
void make_key_sym(Data* out, unsigned short sym)
{
    switch(sym) {
    case EKBD_KEY_LEFT: put_atom(out, "left"); break;
    case EKBD_KEY_RIGHT: put_atom(out, "right"); break;
    case EKBD_KEY_UP: put_atom(out, "up"); break;
    case EKBD_KEY_DOWN: put_atom(out, "down"); break;
    case EKBD_KEY_INSERT: put_atom(out, "insert"); break;
    case EKBD_KEY_DELETE: put_atom(out, "delete"); break;
    case EKBD_KEY_HOME: put_atom(out, "home"); break;
    case EKBD_KEY_END: put_atom(out, "end"); break;
    case EKBD_KEY_PAGEUP: put_atom(out, "pageup"); break;
    case EKBD_KEY_PAGEDOWN: put_atom(out, "pagedown"); break;
    case EKBD_KEY_F1: put_atom(out, "f1"); break;
    case EKBD_KEY_F2: put_atom(out, "f2"); break;
    case EKBD_KEY_F3: put_atom(out, "f3"); break;
    case EKBD_KEY_F4: put_atom(out, "f4"); break;
    case EKBD_KEY_F5: put_atom(out, "f5"); break;
    case EKBD_KEY_F6: put_atom(out, "f6"); break;
    case EKBD_KEY_F7: put_atom(out, "f7"); break;
    case EKBD_KEY_F8: put_atom(out, "f8"); break;
    case EKBD_KEY_F9: put_atom(out, "f9"); break;
    case EKBD_KEY_F10: put_atom(out, "f10"); break;
    case EKBD_KEY_F11: put_atom(out, "f11"); break;
    case EKBD_KEY_F12: put_atom(out, "f12"); break;
    case EKBD_KEY_PRINT: put_atom(out, "print"); break;
    case EKBD_KEY_SYSREQ: put_atom(out, "sysreq"); break;
    case EKBD_KEY_PAUSE: put_atom(out, "pause"); break;
    case EKBD_KEY_BREAK: put_atom(out, "break"); break;
    case EKBD_KEY_QUIT: put_atom(out, "quit"); break;
    case EKBD_KEY_MENU: put_atom(out, "menu"); break;
    case EKBD_KEY_REDRAW: put_atom(out, "redraw"); break;
    default: put_uint16(out, sym); break;
    }
}

void make_key_event(Data* out, char* type, EEvent* e)
{
    put_tag(out, TUPLE);
    put_atom(out, type);
    make_key_sym(out, e->key.sym);
    put_tag(out, LIST);
    if (e->key.mod) {
	if (e->key.mod & EKBD_MOD_CTRL) {
	    put_atom(out, "ctrl");
	    if (e->key.mod & EKBD_MOD_LCTRL)
		put_atom(out, "ctrl-left");
	    if (e->key.mod & EKBD_MOD_RCTRL)
		put_atom(out, "ctrl-right");
	}
	if (e->key.mod & EKBD_MOD_SHIFT) {
	    put_atom(out, "shift");
	    if (e->key.mod & EKBD_MOD_LSHIFT)
		put_atom(out, "shift-left");
	    if (e->key.mod & EKBD_MOD_RCTRL)
		put_atom(out, "shift-right");
	}
	if (e->key.mod & EKBD_MOD_ALT) {
	    put_atom(out, "alt");
	    if (e->key.mod & EKBD_MOD_LALT)
		put_atom(out, "alt-left");
	    if (e->key.mod & EKBD_MOD_RALT)
		put_atom(out, "alt-right");
	}
	if (e->key.mod & EKBD_MOD_META) {
	    put_atom(out, "meta");
	    if (e->key.mod & EKBD_MOD_LMETA)
		put_atom(out, "meta-left");
	    if (e->key.mod & EKBD_MOD_RMETA)
		put_atom(out, "meta-right");
	}
	if (e->key.mod & EKBD_MOD_NUM)
	    put_atom(out, "num");	    
	if (e->key.mod & EKBD_MOD_CAPS)
	    put_atom(out, "caps");
	if (e->key.mod & EKBD_MOD_ALTGR)
	    put_atom(out, "altgr");
	if (e->key.mod & EKBD_MOD_SCR)
	    put_atom(out, "scr");
    }
    put_tag(out, LIST_END);
    put_uint16(out, e->key.code);
    put_tag(out, TUPLE_END);
}

void make_pointer_event(Data* out, char* type, EEvent* e)
{
    put_tag(out, TUPLE);
    put_atom(out, type);
    put_tag(out, LIST);
    if (e->pointer.button & EBUT_LEFT)
	put_atom(out, "left");
    if (e->pointer.button & EBUT_RIGHT)
	put_atom(out, "right");
    if (e->pointer.button & EBUT_MIDDLE)
	put_atom(out, "middle");
    put_tag(out, LIST_END);
	
    put_tag(out, TUPLE);
    put_int32(out, e->pointer.x);
    put_int32(out, e->pointer.y);
    put_int32(out, e->pointer.z);
    put_tag(out, TUPLE_END);    

    put_tag(out, TUPLE_END);
}


static void process_events(EPicEnv* env, EBackend* backend)
{
    EEvent e;
    Data edata;
    unsigned char buf[256];
    unsigned int len;
    u_int32_t whandle;
    int n;

next:
    n = EBackendEventRead(backend, &e, EEVENT_ALL);
    EDBGFMT("epic_input called n=%d", n);
    if (n <= 0) return;
    n--;

    whandle = EOBJ_HANDLE(e.window);
    if (ewindow_object(env, whandle) == NULL) {
	fprintf(stderr, "Window object not found %x\r\n", whandle);
	if (n > 0) goto next;
	return;
    }

    data_init(&edata, buf, sizeof(buf), 0, 0);

    put_tag(&edata, REPLY_EVENT);
    put_UINT32(&edata, whandle);   /* dispatch info */
    put_tag(&edata, TUPLE);
    put_atom(&edata, "eevent");
    put_uint32(&edata, whandle);   /* also included in event */
    
    switch(e.type) {
    case EEVENT_KEY_PRESS:
	make_key_event(&edata, "key_press", &e); break;
    case EEVENT_KEY_RELEASE:
	make_key_event(&edata, "key_release", &e); break;
    case EEVENT_POINTER_MOTION:
	make_pointer_event(&edata, "motion", &e); break;
    case EEVENT_BUTTON_PRESS:
	make_pointer_event(&edata, "button_press", &e); break;
    case EEVENT_BUTTON_RELEASE:
	make_pointer_event(&edata, "button_release", &e); break;
    case EEVENT_CLOSE:
	put_atom(&edata, "close");   break;
    case EEVENT_DESTROYED:
	put_atom(&edata, "destroyed");   break;
    case EEVENT_FOCUS_IN:
	put_atom(&edata, "focus");     break;
    case EEVENT_FOCUS_OUT:
	put_atom(&edata, "focus_out"); break;
    default:
	data_final(&edata);
	fprintf(stderr, "epic_input: event type %d not known\r\n",
		e.type);
	if (n > 0) goto next;
	return;
    }
    put_tag(&edata, TUPLE_END);
	
    if ((len = (edata.ptr - edata.base)) > 0)
	driver_output(env->port, (char*)edata.base, len);
    data_final(&edata);

    if (n>0) goto next;
}



    
DRIVER_INIT(epic)
{
    epic_drv.handle = NULL;
    epic_drv.driver_name = "epic";
    epic_drv.finish = epic_drv_finish;
    epic_drv.init   = epic_drv_init;
    epic_drv.start  = epic_drv_start;
    epic_drv.stop   = epic_drv_stop;
    epic_drv.output = epic_drv_command;
    epic_drv.outputv = epic_drv_commandv;
    epic_drv.ready_input = epic_drv_input;
    epic_drv.ready_output = epic_drv_output;
    epic_drv.control = epic_drv_ctl;
    epic_drv.timeout = epic_drv_timeout;
    EDBG("DRIVER_INIT called");
    return &epic_drv;
}

/* setup global object area */
static int epic_drv_init(void)
{
    EDBG("epic_drv_init called");
    ESimdInit(EPIC_SIMD_AUTO);
    return 0;
}

/* cleanup global object area, and terminate resource */
static void epic_drv_finish(void)
{
    EDBG("epic_drv_finish called");
}

/* Create a EpicEnv environment */
static ErlDrvData epic_drv_start(ErlDrvPort port, char* command)
{
#pragma unused(command)
    EPicEnv* env;

    EDBG("epic_drv_start called");

    if ((env = (EPicEnv*) malloc(sizeof(EPicEnv))) != NULL) {
	EHashInit(&env->objref, "objref", 2,
		  objref_hash, objref_cmp, objref_release, NULL);
	env->port = port;
	env->backend_list = NULL;
	return (ErlDrvData) env;
    }
    return ERL_DRV_ERROR_ERRNO;
}


static void epic_drv_stop(ErlDrvData d)
{
    EPicEnv* env = (EPicEnv*) d;

    EDBG("epic_drv_stop called");

    EHashDelete(&env->objref);

    free(env);
}

static void epic_drv_command(ErlDrvData d, char* buf, int len)
{
#pragma unused(d,buf,len)
    EDBG("epic_drv_command called");
}

static void epic_drv_commandv(ErlDrvData d, ErlIOVec* ev)
{
#pragma unused(d,ev)
    EDBG("epic_drv_commandv called");
}

static void epic_drv_input(ErlDrvData d, ErlDrvEvent handle)
{
    EPicEnv* env = (EPicEnv*) d;
    EBackend* backend;

    /* locate the backend and call the event function */
    if ((backend = find_event_backend(env, handle)) == NULL) {
	fprintf(stderr, "Backend not found handle=%p\r\n", handle);
	return;
    }
    process_events(env, backend);
}


static void epic_drv_output(ErlDrvData d, ErlDrvEvent e)
{
#pragma unused(d,e)
    EDBG("epic_drv_ouput called");
}

static int epic_drv_ctl(ErlDrvData d, 
			unsigned int cmd, char* buf, int len,
			char** rbuf, int rsize)
{
    EPicEnv* env = (EPicEnv*) d;
    Data arg;
    u_int32_t r32;
    char* ptr = *rbuf;
    
    EDBG("epic_drv_ctl called");

    data_init(&arg, (unsigned char*) buf, len, 0, 0);

    switch(cmd) {
    case EPIC_NOOP: {  /* used for benchmarks etc */
	if (data_avail(&arg) == 0)
	    goto ok;
	goto error;
    }

   /* -------------------------------------------------------------------
      
   BACKEND

   ------------------------------------------------------------------- */
	
    case EBACKEND_CREATE: {
	/* <<Dict:32, Name/binary>> */
	EDBG("EBACKEND_CREATE");
	u_int32_t handle;
	EBackend* backend;
	char name_buf[128];
	char* name_ptr = NULL;
	int name_len;
	ErlDrvEvent event;

	if (get_uint32(&arg, &handle) && 
	    (name_len = data_avail(&arg)) < 128) {
	    EDict* dict = NULL;
	    if (handle != 0) {
		if ((dict = edict_object(env,handle)) == NULL)
		    goto error;
	    }
	    if (name_len > 0) {
		memcpy(name_buf, arg.ptr, name_len);
		name_buf[name_len] = '\0';
		name_ptr = name_buf;
	    }
	    if ((backend = EBackendCreate(name_ptr, dict)) == NULL)
		goto error;
	    /* link in to environment */
	    EObjectLink(&env->backend_list, backend); 
	    r32 = epic_handle(env, (EObject*) backend);
	    if ((event = EBackendEventAttach(backend)) != INVALID_HANDLE) {
		backend->event = event;
		/* we should set it none blocking */
		EDBGFMT("driver select %p", event);
		driver_select(env->port, event, DO_READ, 1);
	    }
	    goto ok_r32;
	}
	goto error;
    }

    case EBACKEND_DESTROY: {
	/* <<Backend:32>> */
	EDBG("EBACKEND_DESTROY");
	u_int32_t handle;
	EBackend* backend;

	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((backend = ebackend_object(env,handle)) != NULL)) {
	    EDBGFMT("driver deselect %p", backend->event);
	    driver_select(env->port, backend->event, DO_READ, 0);
	    EObjectUnlink(&env->backend_list, backend);
	    EBackendEventDetach(backend);
	    EHashErase(&env->objref, (void*)handle);
	    goto ok;
	}
	goto error;
    }

    case EBACKEND_LIST: {
	/* <<>> */
	Data list;
	char* name;
	int i = 0;

	EDBG("EBACKEND_LIST");

	data_init(&list, NULL, 0, 0, 1);
	put_UINT8(&list, REPLY_OK);
	put_tag(&list, LIST);
	while((name = EBackendName(i)) != NULL) {
	    put_string(&list, name, strlen(name));
	    i++;
	}
	put_tag(&list, LIST_END);
	*rbuf = (char*) list.base;     /* ErlangVM will deallocate */
	return (list.ptr - list.base);
    }

   /* -------------------------------------------------------------------
      
   GC

   ------------------------------------------------------------------- */

    case EGC_CREATE: {
	/* <<>> */
	EDBG("EGC_CREATE");
	if(data_avail(&arg) == 0) {
	    EGc* gc = EGcCreate();
	    r32 = epic_handle(env, (EObject*) gc);
	    goto ok_r32;
	}
	goto error;
    }

    case EGC_COPY: {
	/* <<Gc:32>> */
	EDBG("EGC_COPY");
	u_int32_t handle;
	EGc* gc;
	EGc* copy;
	
	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((gc = egc_object(env,handle)) != NULL)) {
	    if ((copy = EGcCopy(gc)) != NULL) {
		r32 = epic_handle(env, (EObject*) copy);
		goto ok_r32;
	    }
	}
	goto error;
    }

    case EGC_DESTROY: {
	/* <<Gc:32>> */
	u_int32_t handle;
	EGc* gc;

	EDBG("EGC_DESTROY");
	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((gc = egc_object(env,handle)) != NULL)) {
	    EHashErase(&env->objref, (void*)handle);
	    goto ok;
	}
	goto error;
    }

    case EGC_SET: {
	u_int32_t handle;
	u_int8_t  selector;
	u_int32_t value;
	EPixel_t color;
	EPixmap* texture;
	EGc* gc;
	
	if (get_uint32(&arg, &handle) && 
	    get_uint8(&arg, &selector) &&
	    get_uint32(&arg, &value) &&
	    (data_avail(&arg) == 0) &&
	    ((gc = egc_object(env,handle)) != NULL)) {
	    switch(selector) {
	    case EGC_FILL_STYLE:
		EGcSetFillStyle(gc, value);
		break;
	    case EGC_FILL_COLOR:
		color.px = value;
		EGcSetFillColor(gc, color);
		break;
	    case EGC_FILL_TEXTURE:
		if (value == 0) 
		    texture = NULL;
		else {
		    if ((texture = epixmap_object(env,value)) == NULL)
			goto error;
		}
		EGcSetFillTexture(gc, texture);
		break;
	    case EGC_LINE_STYLE:
		EGcSetLineStyle(gc, value);
		break;
	    case EGC_LINE_WIDTH:
		EGcSetLineWidth(gc, value);
		break;
	    case EGC_LINE_JOIN_STYLE:
		EGcSetLineJoinStyle(gc, value);
		break;
	    case EGC_LINE_CAP_STYLE:
		EGcSetLineCapStyle(gc, value);
		break;
	    case EGC_LINE_TEXTURE:
		if (value == 0) 
		    texture = NULL;
		else {
		    if ((texture = epixmap_object(env,value)) == NULL)
			goto error;
		}
		EGcSetLineTexture(gc, texture);
		break;
	    case EGC_BORDER_STYLE:
		EGcSetBorderStyle(gc, value);
		break;
	    case EGC_BORDER_JOIN_STYLE:
		EGcSetBorderJoinStyle(gc, value);
		break;
	    case EGC_BORDER_COLOR:
		color.px = value;
		EGcSetBorderColor(gc, color);
		break;
	    case EGC_BORDER_CAP_STYLE:
		EGcSetBorderCapStyle(gc, value);
		break;
	    case EGC_BORDER_WIDTH:
		EGcSetBorderWidth(gc, value);
		break;
	    case EGC_BORDER_TEXTURE:
		if (value == 0) 
		    texture = NULL;
		else {
		    if ((texture = epixmap_object(env,value)) == NULL)
			goto error;
		}
		EGcSetBorderTexture(gc, texture);
		break;
	    case EGC_BACKGROUND_COLOR:
		color.px = value;
		EGcSetBackgroundColor(gc, color);
		break;
	    case EGC_FOREGROUND_COLOR:
		color.px = value;
		EGcSetForegroundColor(gc, color);
		break;
	    default:
		goto error;
	    }
	    goto ok;
	}
	goto error;
    }

    case EGC_GET: {
	u_int32_t handle;
	u_int8_t  selector;
	EGc* gc;

	if (get_uint32(&arg, &handle) && 
	    get_uint8(&arg, &selector) &&
	    (data_avail(&arg) == 0) &&
	    ((gc = egc_object(env,handle)) != NULL)) {
	    switch(selector) {	
	    case EGC_FILL_STYLE:
		r32 = gc->fill_style;
		break;
	    case EGC_FILL_COLOR:
		r32 = gc->fill_color.px;
		break;
	    case EGC_FILL_TEXTURE:
		r32 = epic_handle(env, (EObject*) gc->fill_texture);
		break;
	    case EGC_LINE_STYLE:
		r32 = gc->line_style;
		break;
	    case EGC_LINE_WIDTH:
		r32 = gc->line_width;
		break;
	    case EGC_LINE_JOIN_STYLE:
		r32 = gc->line_join_style;
		break;
	    case EGC_LINE_CAP_STYLE:
		r32 = gc->line_cap_style;
		break;
	    case EGC_LINE_TEXTURE:
		r32 = epic_handle(env, (EObject*) gc->line_texture);
		break;
	    case EGC_BORDER_STYLE:
		r32 = gc->border_style;
		break;
	    case EGC_BORDER_JOIN_STYLE:
		r32 = gc->border_join_style;
		break;
	    case EGC_BORDER_COLOR:
		r32 = gc->border_color.px; 
		break;
	    case EGC_BORDER_CAP_STYLE:
		r32 = gc->border_cap_style;
		break;
	    case EGC_BORDER_WIDTH:
		r32 = gc->border_width;
		break;
	    case EGC_BORDER_TEXTURE:
		r32 = epic_handle(env, (EObject*) gc->border_texture);
		break;
	    case EGC_BACKGROUND_COLOR:
		r32 = gc->background_color.px; 
		break;
	    case EGC_FOREGROUND_COLOR:
		r32 = gc->foreground_color.px; 
		break;

	    default:
		goto error;
	    }
	    goto ok_r32;
	}
	goto error;
    }

   /* -------------------------------------------------------------------
      
      DICT

   ------------------------------------------------------------------- */

    case EDICT_CREATE: {
	/* <<>> */
	EDBG("EDICT_CREATE");
	if(data_avail(&arg) == 0) {
	    EDict* dict = EDictCreate();
	    r32 = epic_handle(env, (EObject*) dict);
	    goto ok_r32;
	}
	goto error;
    }

    case EDICT_COPY: {
	/* <<Dict:32>> */
	u_int32_t handle;
	EDict* dict;
	EDict* copy;

	EDBG("EDICT_COPY");
	if (get_uint32(&arg, &handle) && 
	    ((dict = edict_object(env,handle)) != NULL) &&
	    (data_avail(&arg) == 0)) {
	    if ((copy = EDictCopy(dict)) != NULL) {
		r32 = epic_handle(env, (EObject*) copy);
		goto ok_r32;
	    }
	}
	goto error;
    }

    case EDICT_DESTROY: {
	/* <<Dict:32>> */
	u_int32_t handle;
	EDict* dict;

	EDBG("EDICT_DESTROY");
	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((dict = edict_object(env,handle)) != NULL)) {
	    EHashErase(&env->objref, (void*)handle);
	    goto ok;
	}
	goto error;
    }

    case EDICT_SET: {
	/* <<Dict:32, Key, Value>> */
	u_int32_t handle;
	char kbuf[8], vbuf[8];
	EDictData key, value;
	EDict* dict;

	if (get_uint32(&arg, &handle) && 
	    ((dict = edict_object(env,handle)) != NULL) &&
	    get_dict_data(&arg, &key, kbuf) &&
	    get_dict_data(&arg, &value, vbuf)) {
	    EDictSetEnt(dict, &key, &value);
	    goto ok;
	}
	goto error;
    }

    case EDICT_GET: {
	/* <<Dict:32, Key>> */
	u_int32_t handle;
	char kbuf[8];
	EDictData key;
	EDict* dict;
	EDictEntry* ent;
	Data out;

	if (get_uint32(&arg, &handle) && 
	    ((dict = edict_object(env,handle)) != NULL) &&
	    get_dict_data(&arg, &key, kbuf)) {
	    if ((ent=EDictLookupEnt(dict, &key, EDICT_ANY)) == NULL)
		goto enoent;
	    data_init(&out, (u_int8_t*) *rbuf, rsize, 0, 0);
	    put_tag(&out, REPLY_OK);
	    put_tag(&out, TUPLE);

	    put_dict_data(&out, &ent->key);
	    put_dict_data(&out, &ent->data);
	    put_tag(&out, TUPLE_END);

	    *rbuf = (char*) out.base;
	    return out.ptr - out.base;
	}
	goto error;
    }

    case EDICT_UNSET: {
	/* <<Dict:32, Key>> */
	u_int32_t handle;
	char kbuf[8];
	EDictData key;
	EDict* dict;

	if (get_uint32(&arg, &handle) && 
	    ((dict = edict_object(env,handle)) != NULL) &&
	    get_dict_data(&arg, &key, kbuf)) {
	    EDictUnsetEnt(dict, &key, EDICT_ANY);
	    goto ok;
	}
	goto error;
    }

   /* -------------------------------------------------------------------
      
   BITMAP

   ------------------------------------------------------------------- */

    case EBITMAP_CREATE: {
	/* <<Width:32, Height:32>> */
	u_int32_t width, height;

	EDBG("EBITMAP_CREATE");
	if(get_uint32(&arg, &width) &&
	   get_uint32(&arg, &height) &&
	   (data_avail(&arg) == 0)) {
	    EBitmap* pic = EBitmapCreate(width, height);
	    r32 = epic_handle(env, (EObject*) pic);
	    goto ok_r32;
	}
	goto error;
    }

    case EBITMAP_DESTROY: {
	/* <<Bmp:32>> */
	u_int32_t handle;
	EBitmap* bmp;

	EDBG("EBITMAP_DESTROY");
	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((bmp = ebitmap_object(env,handle)) != NULL)) {
	    EHashErase(&env->objref, (void*)handle);
	    goto ok;
	}
	goto error;
    }

    case EBITMAP_FILL: {
	/* <<Bitmap:32, Pat:8>> */
	u_int32_t handle;
	EBitmap* bmp;
	u_int8_t pat;

	EDBG("EBITMAP_FILL");

	if (get_uint32(&arg, &handle) && 
	    get_uint8(&arg, &pat) &&
	    (data_avail(&arg) == 0) &&
	    ((bmp = ebitmap_object(env,handle)) != NULL)) {
	    EBitmapFill(bmp, pat);
	    goto ok;
	}
	goto error;
    }


    case EBITMAP_PUT_BIT: {
	/* <<Pic:32, X:32, Y:32, Bit:8>> */
	u_int32_t handle;
	u_int32_t x, y;
	u_int8_t bit;
	EBitmap* bmp;

	EDBG("EBITMAP_PUT_BIT");
	if (get_uint32(&arg, &handle) && 
	    ((bmp = ebitmap_object(env,handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    get_uint8(&arg, &bit) &&
	    (data_avail(&arg) == 0)) {
	    EBitmapPutBit(bmp, x, y, bit);
	    goto ok;
	}
	goto error;
    }

    case EBITMAP_PUT_BITS: {
	/* <<Pic:32, X:32, Y:32, Width:32, Height:32, Bits:N>> */
	u_int32_t handle;
	u_int32_t x, y, width, height;
	EBitmap* bmp;

	EDBG("EBITMAP_PUT_BITS");
	if (get_uint32(&arg, &handle) && 
	    ((bmp = ebitmap_object(env,handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    get_uint32(&arg, &width) &&
	    get_uint32(&arg, &height)) {
	    EBitmapPutBits(bmp, x, y, width, height, 
			   arg.ptr, (arg.ptr_end-arg.ptr));
	    goto ok;
	}
	goto error;
    }

    case EBITMAP_GET_BIT: {
	/* <<Bmp:32, X:32, Y:32>> */
	u_int32_t handle;
	u_int32_t x, y;
	EBitmap* bmp;

	EDBG("EBITMAP_GET_BIT");
	if (get_uint32(&arg, &handle) && 
	    ((bmp = ebitmap_object(env,handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    (data_avail(&arg) == 0)) {
	    r32 = (u_int32_t) EBitmapGetBit(bmp, x, y);
	    goto ok_r32;
	}
	goto error;
    }

    case EBITMAP_DRAW: {
	/* <<SrcBmp:32, DstPix:32,
	   XSrc:32, YSrc:32, XDst:32, YDst:32, 
	   Width:32, Height:32, (pixel(Fg)):32, (pixel(Bg)):32 >> */
	u_int32_t pix_handle;
	u_int32_t bmp_handle;
	u_int32_t x_src, y_src, x_dst, y_dst;
	u_int32_t width, height;
	EPixel_t fg, bg;
	EPixmap* pix;
	EBitmap* bmp;

	EDBG("EBITMAP_DRAW");
	if (get_uint32(&arg, &bmp_handle) && 
	    get_uint32(&arg, &pix_handle) && 
	    ((bmp = ebitmap_object(env,bmp_handle)) != NULL) &&
	    ((pix = epixmap_object(env,pix_handle)) != NULL) &&
	    get_uint32(&arg, &x_src) &&
	    get_uint32(&arg, &y_src) &&
	    get_uint32(&arg, &x_dst) &&
	    get_uint32(&arg, &y_dst) &&
	    get_uint32(&arg, &width) &&
	    get_uint32(&arg, &height) &&
	    get_pixel(&arg, &fg) &&
	    get_pixel(&arg, &bg) &&
	    (data_avail(&arg) == 0)) {
	    EBitmapDraw(bmp, pix,
			(int)x_src, (int)y_src, 
			(int) x_dst, (int) y_dst,
			width, height,
			fg, bg);
	    goto ok;
	}
	goto error;
    }


   /* -------------------------------------------------------------------
      
   PIXMAP

   ------------------------------------------------------------------- */
	
    case EPIXMAP_CREATE: {
	/* <<Width:32, Height:32, PixelType:32>> */
	u_int32_t width, height;
	u_int32_t pixelType;

	EDBG("EPIXMAP_CREATE");
	if(get_uint32(&arg, &width) &&
	   get_uint32(&arg, &height) &&
	   get_uint32(&arg, &pixelType) &&
	   (data_avail(&arg) == 0)) {
	    EPixmap* pic = EPixmapCreate(width, height, pixelType);
	    r32 = epic_handle(env, (EObject*) pic);
	    goto ok_r32;
	}
	goto error;
    }

    case EPIXMAP_DESTROY: {
	/* <<Pic:32>> */
	u_int32_t handle;
	EPixmap* pic;

	EDBG("EPIXMAP_DESTROY");
	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((pic = epixmap_object(env,handle)) != NULL)) {
	    EHashErase(&env->objref, (void*)handle);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_ATTACH: {
	/* <<Pic:32, Backend:32>> */
	u_int32_t pic_handle;
	u_int32_t be_handle;
	EPixmap* pic;
	EBackend* backend;

	EDBG("EPIXMAP_ATTACH");
	if (get_uint32(&arg, &pic_handle) && 
	    get_uint32(&arg, &be_handle) && 
	    (data_avail(&arg) == 0) &&
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    ((backend = ebackend_object(env,be_handle)) != NULL)
	    ) {
	    EPixmapAttach(pic, backend);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_DETACH: {
	/* <<Pic:32>> */
	u_int32_t pic_handle;
	EPixmap* pic;

	EDBG("EPIXMAP_DETACH");
	if (get_uint32(&arg, &pic_handle) && 
	    (data_avail(&arg) == 0) &&
	    ((pic = epixmap_object(env,pic_handle)) != NULL)
	    ) {
	    EPixmapDetach(pic);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_FILL: {
	/* <<Pic:32, Pixel:32>> */
	u_int32_t pic_handle;
	EPixmap* pic; 
	EPixel_t pixel;

	EDBG("EPIXMAP_FILL");

	if (get_uint32(&arg, &pic_handle) && 
	    get_pixel(&arg, &pixel) &&
	    (data_avail(&arg) == 0) &&
	    ((pic = epixmap_object(env,pic_handle)) != NULL)) {
	    EPixmapFill(pic, pixel);
	    goto ok;
	}
	goto error;
    }
	
    case EPIXMAP_COPY: {
	/* <<Src:32, Dst:32>> */
	u_int32_t src_handle, dst_handle;
	EPixmap* src;
	EPixmap* dst;

	EDBG("EPIXMAP_COPY");

	if (get_uint32(&arg, &src_handle) && 
	    get_uint32(&arg, &dst_handle) &&
	    (data_avail(&arg) == 0) &&
	    ((src = epixmap_object(env,src_handle)) != NULL) &&
	    ((dst = epixmap_object(env,dst_handle)) != NULL)
	    ) {
	    EPixmapCopy(src, dst);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_SCROLL: {
	/*  <<SrcPic:32, DstPic:32, 
	    Horizontal:32, Vertical:32, Rotate:32, Pixel:32>> */
	u_int32_t src_handle, dst_handle;
	u_int32_t horizontal, vertical, rotate;
	EPixel_t pixel;
	EPixmap* src;
	EPixmap* dst;

	EDBG("EPIXMAP_SCROLL");
	if (get_uint32(&arg, &src_handle) && 
	    get_uint32(&arg, &dst_handle) &&
	    ((src = epixmap_object(env,src_handle)) != NULL) &&
	    ((dst = epixmap_object(env,dst_handle)) != NULL) &&	
	    get_uint32(&arg, &horizontal) &&
	    get_uint32(&arg, &vertical) &&
	    get_uint32(&arg, &rotate) &&
	    get_pixel(&arg, &pixel) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapScroll(src, dst,
			  (int) horizontal, (int) vertical,
			  (int) rotate, pixel);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_DRAW: {
	/*  <<Pic:32, Win:32,
	    XSrc:32, YSrc:32, XDst:32, YDst:32, 
	    Width:32, Height:32>> */
	u_int32_t pic_handle, win_handle;
	u_int32_t x_src, y_src, x_dst, y_dst;
	u_int32_t width, height;
	EPixmap* pic;
	EWindow* win;

	EDBG("EPIXMAP_DRAW");
	if (get_uint32(&arg, &pic_handle) && 
	    get_uint32(&arg, &win_handle) &&
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    ((win = ewindow_object(env,win_handle)) != NULL) &&
	    get_uint32(&arg, &x_src) &&
	    get_uint32(&arg, &y_src) &&
	    get_uint32(&arg, &x_dst) &&
	    get_uint32(&arg, &y_dst) &&
	    get_uint32(&arg, &width) &&
	    get_uint32(&arg, &height) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapDrawWindow(pic, win, 
			      (int)x_src, (int)y_src, 
			      (int) x_dst, (int) y_dst,
			      width, height);
	    if ((win->backend != NULL) && (win->backend->pending))
		process_events(env, win->backend);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_PUT_PIXEL: {
	/* <<Pic:32, X:32, Y:32,Pixel:32>> */
	u_int32_t pic_handle;
	u_int32_t x, y;
	EPixel_t pixel;
	EPixmap* pic;

	EDBG("EPIXMAP_PUT_PIXEL");
	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    get_pixel(&arg, &pixel) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapPutPixel(pic, x, y, EFLAG_NONE, pixel);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_DRAW_POINT: {
	/* <<Pic:32, Gc:32, X:32, Y:322>> */
	u_int32_t pic_handle;
	u_int32_t gc_handle;
	u_int32_t x, y;
	EPixmap* pic;
	EGc* gc;

	EDBG("EPIXMAP_DRAW_POINT");
	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &gc_handle) && 
	    ((gc = egc_object(env,gc_handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapDrawPoint(pic, gc, x, y);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_PUT_PIXELS: {
	/* <<Pic:32, X:32, Y:32, Width:32, Height:32,Pixels:N*32>> */
	u_int32_t pic_handle;
	u_int32_t x, y, width, height;
	EPixmap* pic;

	EDBG("EPIXMAP_PUT_PIXELS");
	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    get_uint32(&arg, &width) &&
	    get_uint32(&arg, &height) &&
	    ((data_avail(&arg) % 4) == 0)) {
	    EPixmapPutPixels(pic, x, y, width, height, 
			     EPIXEL_TYPE_ARGB, EFLAG_NONE,
			     arg.ptr, (arg.ptr_end-arg.ptr));
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_GET_PIXEL: {
	/* <<Pic:32, X:32, Y:32>> */
	u_int32_t pic_handle;
	u_int32_t x, y;
	EPixel_t pixel;
	EPixmap* pic;

	EDBG("EPIXMAP_GET_PIXEL");
	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    (data_avail(&arg) == 0)) {
	    pixel = EPixmapGetPixel(pic, x, y);
	    r32 = pixel.px;
	    goto ok_r32;
	}
	goto error;
    }

    case EPIXMAP_COPY_AREA: {
	/* <<SrcPic:32, DstPic:32,
	   XSrc:32, YSrc:32, XDst:32, YDst:32, 
	   Width:32, Height:32>> */
	u_int32_t src_handle, dst_handle;
	u_int32_t x_src, y_src, x_dst, y_dst;
	u_int32_t width, height;
	EPixmap* src;
	EPixmap* dst;

	EDBG("EPIXMAP_COPY_AREA");
	if (get_uint32(&arg, &src_handle) && 
	    get_uint32(&arg, &dst_handle) &&
	    ((src = epixmap_object(env,src_handle)) != NULL) &&
	    ((dst = epixmap_object(env,dst_handle)) != NULL) &&
	    get_uint32(&arg, &x_src) &&
	    get_uint32(&arg, &y_src) &&
	    get_uint32(&arg, &x_dst) &&
	    get_uint32(&arg, &y_dst) &&
	    get_uint32(&arg, &width) &&
	    get_uint32(&arg, &height) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapCopyArea(src, dst,
			    (int)x_src, (int)y_src, 
			    (int) x_dst, (int) y_dst,
			    width, height, EFLAG_NONE);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_DRAW_RECTANGLE: {
	/* <<Pic:32,Gc:32, X:32, Y:32, Width:32, Height:32>> */
	u_int32_t gc_handle;
	u_int32_t pic_handle;
	u_int32_t x, y;
	u_int32_t width, height;
	EPixmap* pic;
	EGc* gc;

	EDBG("EPIXMAP_DRAW_RECTANGLE");

	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &gc_handle) && 
	    ((gc = egc_object(env,gc_handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    get_uint32(&arg, &width) &&
	    get_uint32(&arg, &height) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapDrawRectangle(pic, gc,
				 (int)x, (int)y, 
				 width, height);
	    goto ok;
	}
	goto error;
    }


    case EPIXMAP_DRAW_TRIANGLE: {
	/* <<Pic:32,Gc:32,X0:32,Y0:32,X1:32,Y1:32,X2:32,Y2:32>> */
	u_int32_t gc_handle;
	u_int32_t pic_handle;
	u_int32_t x0, y0;
	u_int32_t x1, y1;
	u_int32_t x2, y2;
	EPixmap* pic;
	EGc* gc;

	EDBG("EPIXMAP_FILL_RECTANGLE");

	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &gc_handle) && 
	    ((gc = egc_object(env,gc_handle)) != NULL) &&
	    get_uint32(&arg, &x0) && get_uint32(&arg, &y0) &&
	    get_uint32(&arg, &x1) && get_uint32(&arg, &y1) &&
	    get_uint32(&arg, &x2) && get_uint32(&arg, &y2) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapDrawTriangle(pic, gc,
				(int)x0, (int)y0,
				(int)x1, (int)y1,
				(int)x2, (int)y2);
	    goto ok;
	}
	goto error;
    }

	
    case EPIXMAP_DRAW_LINE: {
	/* <<Pic:32, Gc:32, X0:32, Y0:32, X1:32, Y1:32>> */
	u_int32_t pic_handle;
	u_int32_t gc_handle;
	u_int32_t x1, y1;
	u_int32_t x2, y2;
	EPixmap* pic;
	EGc* gc;

	EDBG("EPIXMAP_DRAW_LINE");

	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &gc_handle) && 
	    ((gc = egc_object(env,gc_handle)) != NULL) &&
	    get_uint32(&arg, &x1) &&
	    get_uint32(&arg, &y1) &&
	    get_uint32(&arg, &x2) &&
	    get_uint32(&arg, &y2) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapDrawLine(pic, gc,
			    (int)x1, (int)y1, 
			    (int)x2, (int)y2);
	    goto ok;
	}
	goto error;
    }

    case EPIXMAP_TEX_LINE: {
	/* <<Pic:32, Gc:32, X0:32, Y0:32, X1:32, Y1:32, 
	   Tex:32, TX0:32, TX1:32, Ty:32/float>> */
	u_int32_t pic_handle;
	u_int32_t gc_handle;
	u_int32_t x1, y1;
	u_int32_t x2, y2;
	u_int32_t tex_handle;
	u_int32_t tx1, tx2;
	float ty;
	EPixmap* pic;
	EPixmap* tex;
	EGc* gc;

	EDBG("EPIXMAP_TEX_LINE");

	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &gc_handle) && 
	    ((gc = egc_object(env,gc_handle)) != NULL) &&
	    get_uint32(&arg, &x1) &&
	    get_uint32(&arg, &y1) &&
	    get_uint32(&arg, &x2) &&
	    get_uint32(&arg, &y2) &&
	    get_uint32(&arg, &tex_handle) && 
	    ((tex = epixmap_object(env,tex_handle)) != NULL) &&
	    get_uint32(&arg, &tx1) &&
	    get_uint32(&arg, &tx2) && 
	    get_float32(&arg, &ty) && 
	    (data_avail(&arg) == 0)) {

	    EPixmapTexLine(pic, gc,
			   (int)x1, (int)y1, 
			   (int)x2, (int)y2, 
			   tex,
			   (int)tx1, (int)tx2,
			   ty);
	    goto ok;
	}
	goto error;
    }


    case EPIXMAP_DRAW_ELLIPSE: {
	/* <<Pic:32,Gc:32, X:32, Y:32, A:32, B:32>> */
	u_int32_t pic_handle;
	u_int32_t gc_handle;
	u_int32_t x, y, a, b;
	EPixmap* pic;
	EGc* gc;

	EDBG("EPIXMAP_DRAW_ELLIPSE");

	if (get_uint32(&arg, &pic_handle) && 
	    ((pic = epixmap_object(env,pic_handle)) != NULL) &&
	    get_uint32(&arg, &gc_handle) && 
	    ((gc = egc_object(env,gc_handle)) != NULL) &&
	    get_uint32(&arg, &x) &&
	    get_uint32(&arg, &y) &&
	    get_uint32(&arg, &a) &&
	    get_uint32(&arg, &b) &&
	    (data_avail(&arg) == 0)) {
	    EPixmapDrawEllipse(pic, gc,
			       (int)x, (int)y, a, b);
	    goto ok;
	}
	goto error;
    }


   /* -------------------------------------------------------------------
      
   WINDOW

   ------------------------------------------------------------------- */


    case EWINDOW_CREATE: {
	/* <<X:32, Y:32, Width:32, Height:32, Mask:16>> */
	uint32_t x, y;
	u_int32_t width, height;
	u_int16_t mask;

	EDBG("EWINDOW_CREATE");

	if(get_uint32(&arg, &x) &&
	   get_uint32(&arg, &y) &&
	   get_uint32(&arg, &width) &&
	   get_uint32(&arg, &height) &&
	   get_uint16(&arg, &mask) && 
	   (data_avail(&arg) == 0)) {
	    EWindow* win = EWindowCreate(x, y, width, height);
	    win->mask = mask;
	    r32 = epic_handle(env, (EObject*) win);
	    goto ok_r32;
	}
	goto error;
    }

    case EWINDOW_DESTROY: {
	/* <<Win:32>> */
	/* <<Pic:32>> */
	u_int32_t handle;
	EWindow* win;

	EDBG("EWINDOW_DESTROY");

	if (get_uint32(&arg, &handle) && 
	    (data_avail(&arg) == 0) &&
	    ((win = ewindow_object(env,handle)) != NULL)) {
	    EHashErase(&env->objref, (void*)handle);
	    goto ok;
	}
	goto error;
    }

    case EWINDOW_ATTACH: {
	/* <<Win:32, Backend:32, Mask:16>> */
	u_int32_t win_handle;
	u_int32_t be_handle;
	EWindow* win;
	EBackend* backend;

	EDBG("EWINDOW_ATTACH");

	if (get_uint32(&arg, &win_handle) && 
	    get_uint32(&arg, &be_handle) && 
	    ((win = ewindow_object(env,win_handle)) != NULL) &&
	    ((backend = ebackend_object(env,be_handle)) != NULL) &&
	    (data_avail(&arg) == 0)) {
	    EWindowAttach(win, backend);
	    goto ok;
	}
	goto error;
    }

	/* EWINDOW_CONFIGURE */

    case EWINDOW_DETACH: {
	/* <<Win:32>> */
	u_int32_t win_handle;
	EWindow* win;

	EDBG("EWINDOW_DETACH");

	if (get_uint32(&arg, &win_handle) &&
	    ((win = ewindow_object(env,win_handle)) != NULL) &&
	    (data_avail(&arg) == 0)) {
	    EWindowDetach(win);
	    goto ok;
	}
	goto error;
    }

    default:
	goto error;
    }

enoent:
    *ptr++ = REPLY_ERROR;
    *ptr++ = ATOM;
    *ptr++ = 6;
    memcpy(ptr, "enoent", 6);
    return 9;
error:
    *ptr++ = REPLY_ERROR;
    *ptr++ = ATOM;
    *ptr++ = 6;
    memcpy(ptr, "einval", 6);
    return 9;

ok:
    *ptr = REPLY_OK;
    return 1;

ok_r32:
    *ptr++ = REPLY_OK;
    *ptr++ = UINT32;
    PUT_UINT32(ptr, r32);
    return 6;
}

static void epic_drv_timeout(ErlDrvData d)
{
#pragma unused(d)
    EDBG("epic_drv_timeout called");
}






