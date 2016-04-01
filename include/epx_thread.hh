//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __EPX_THREAD_H__
#define __EPX_THREAD_H__

#include <pthread.h>

#include "epx.h"

typedef struct {
    pthread_t tid;      // The thread id (used with join)
    int       ctl;      // The fd needed for communication
    pthread_mutex_t dlock; // protect dcount
    int       dcount;   // Number of draw operations active
    epx_backend_t * backend;  // Used backend
} EThread;

extern EThread* EThreadCreate(epx_backend_t* backend);
extern int EThreadDestroy(EThread* thr);

extern int EThreadPixmapDraw(EThread* thr, epx_pixmap_t* pixmap, epx_window_t* window,
			     int offscreen,
			     int src_x, int src_y, int dst_x, int dst_y,
			     unsigned int width, unsigned int height);
extern int EThreadDrawCount(EThread* thr);
extern int EThreadSwap(EThread* thr, epx_window_t* window);

extern int EThreadEventRead(EThread* thr, epx_event_t* e, u_int16_t mask);

#endif
