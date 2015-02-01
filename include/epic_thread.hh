//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __EPIC_THREAD_H__
#define __EPIC_THREAD_H__

#include <pthread.h>

#include "epic.h"

typedef struct {
    pthread_t tid;      // The thread id (used with join)
    int       ctl;      // The fd needed for communication
    pthread_mutex_t dlock; // protect dcount
    int       dcount;   // Number of draw operations active
    EBackend* backend;  // Used backend
} EThread;

extern EThread* EThreadCreate(EBackend* backend);
extern int EThreadDestroy(EThread* thr);

extern int EThreadPixmapDraw(EThread* thr, EPixmap* pixmap, EWindow* window,
			     int offscreen,
			     int src_x, int src_y, int dst_x, int dst_y,
			     unsigned int width, unsigned int height);
extern int EThreadDrawCount(EThread* thr);
extern int EThreadSwap(EThread* thr, EWindow* window);

extern int EThreadEventRead(EThread* thr, EEvent* e, u_int16_t mask);

#endif
