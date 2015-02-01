/*
 * Backend managment
 *
 */

#include "epic.h"

typedef EBackend* (*backend_init_t)(EDict* param);

extern EBackend* none_init(EDict* param);

extern EBackend* fb_init(EDict* param);

#ifdef MAC_OS_X
extern EBackend* carbon_init(EDict* param);
#endif

#ifdef MAC_OS_GL
extern EBackend* carbon_gl_init(EDict* param);
#endif

#ifdef WIN32
extern EBackend* win32_init(EDict* param);
#endif

#ifdef X11
extern EBackend* x11_init(EDict* param);
#endif

static struct _backend_item {
    char* name;
    backend_init_t init;
} backend_item [] =
{
#ifdef MAC_OS_X
    {"macos", (backend_init_t) carbon_init},
#endif

#ifdef MAC_OS_GL
    {"macos_gl", (backend_init_t) carbon_gl_init},
#endif

#ifdef WIN32
    {"win32", (backend_init_t) win32_init},
#endif

#ifdef X11
    {"x11", (backend_init_t) x11_init},
#endif

#ifdef FB
    {"fb", (backend_init_t) fb_init},
#endif
    {"none", (backend_init_t) none_init},
    {NULL, NULL}
};

char* EBackendName(int i)
{
    int sz = sizeof(backend_item)/(sizeof(struct _backend_item));

    if (i >= sz)
	return NULL;
    return backend_item[i].name;
}

void EBACKEND_TYPE_RELEASE(void* arg)
{
    EBackend* be = (EBackend*) arg;
    EDBGFMT_MEM("EBACKEND_TYPE_RELEASE: %p", arg);    
    EBackendFinish(be);
}

EBackend* EBackendCreate(char* name, EDict* param)
{
    if (name == NULL) {
	if (backend_item[0].init != NULL) 
	    return (backend_item[0].init)(param);
	return NULL;
    }
    else {
	int i = 0;
	while(backend_item[i].name != NULL) {
	    if (strcmp(backend_item[i].name, name) == 0)
		return (backend_item[i].init)(param);
	    i++;
	}
	return NULL;
    }
}

int EBackendAdjust(EBackend *be, EDict *param)
{
    return be->cb->adjust(be, param);
}
