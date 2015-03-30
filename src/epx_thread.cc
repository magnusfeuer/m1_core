/*
 * Start a backend thread (optional) to handle event processing
 * and pixmap blit as a thread
 * It will create a pipe to the main process to handle communication
 * the input will be commands (draw) and output will be Events.
 * 
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/poll.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>

#include "m1.hh"
#include "epx.h"
#include "epx_thread.hh"

typedef struct {
    EThread*  thr;     /* The thread structure */
    int ctl_fd;        /* thread - side */
} EThreadArgs_t;

typedef enum {
    ETHREAD_REQUEST_SYNC,
    ETHREAD_REQUEST_SWAP,
    ETHREAD_REQUEST_DRAW,
    ETHREAD_REQUEST_EVENT_READ
} EThreadRequestType_t;

typedef struct {
    EThreadRequestType_t type;
    union {
	struct {
	    u_int16_t mask;
	} rq_event_read;

	struct {
	    epx_window_t* window;
	} rq_swap;

	struct {
	    epx_pixmap_t* pixmap;
	    epx_window_t* window;
	    int off_screen;
	    int src_x;
	    int src_y;
	    int dst_x;
	    int dst_y;
	    unsigned int width;
	    unsigned int height;
	} rq_draw;
    } u;
} EThreadRequest_t;

typedef enum {
    ETHREAD_REPLY_SYNC,
    ETHREAD_REPLY_SWAP,
    ETHREAD_REPLY_DRAW,
    ETHREAD_REPLY_EVENT_READ
} EThreadReplyType_t;

typedef struct {
    EThreadReplyType_t type;
    int status;
    union {
	epx_event_t e;
    } u;
} EThreadReply_t;
    

static void* EThreadMain(void* arg);


EThread* EThreadCreate(epx_backend_t* backend)
{
    int ret;
    int arg;
    int terr;
    int sv[2];
    pthread_attr_t attr;
    pthread_t tid;
    EThreadArgs_t* args = NULL;
    EThread* thr = NULL;
    sigset_t sigBlockSet,sigOrigSet;

    sv[0] = -1;
    sv[1] = -1;

    DBGFMT("socketpair called");
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0) {
	perror("socketpair");
	return NULL;
    }

    arg = 1;
    setsockopt(sv[0], IPPROTO_TCP, TCP_NODELAY, &arg, sizeof(arg));
    DBGFMT("socketpair created %d %d", sv[0], sv[1]);
    pthread_attr_init(&attr);
    pthread_attr_setscope(&attr,PTHREAD_SCOPE_SYSTEM);

    if ((thr = (EThread*) malloc(sizeof(EThread))) == NULL) {
	terr = errno;
	goto error;
    }

    pthread_mutex_init(&thr->dlock, NULL);
    thr->backend = backend;
    thr->dcount  = 0;
    thr->ctl     = sv[0];

    if ((args = (EThreadArgs_t*) malloc(sizeof(EThreadArgs_t))) == NULL) {
	terr = errno;
	goto error;
    }
    args->thr = thr;
    args->ctl_fd = sv[1];
    DBGFMT("starting thread\n");
    sigemptyset(&sigBlockSet);
    sigaddset(&sigBlockSet,SIGINT);
    sigaddset(&sigBlockSet,SIGCHLD);
    sigaddset(&sigBlockSet,SIGUSR1);
    sigfillset(&sigBlockSet);
  
    pthread_sigmask(SIG_BLOCK,&sigBlockSet,&sigOrigSet);
 
    if ((ret=pthread_create(&tid, &attr, EThreadMain,(void *)args))!= 0) {
	terr = errno;
	pthread_attr_destroy(&attr);
	pthread_sigmask(SIG_SETMASK, &sigOrigSet, NULL);
	goto error;
    }

    pthread_attr_destroy(&attr);
    pthread_sigmask(SIG_SETMASK, &sigOrigSet, NULL);
    DBGFMT("thread create tid=%d\n", tid);

    thr->tid = tid;

    return thr;

error:
    if (sv[0] >= 0) close(sv[0]);
    if (sv[1] >= 0) close(sv[1]);
    if (args != NULL) free(args);
    if (thr != NULL) free(thr);
    errno = terr;
    return NULL;
}

int EThreadDestroy(EThread* thr)
{
    void* status;
    int res;
    if (thr->ctl >= 0) close(thr->ctl);
    if ((res=pthread_join(thr->tid, &status)) >= 0)
	res = (int) status;
    free(thr);
    return res;
}


// Swap buffers to draw in (pixmap will be a new pixmap to use)
int EThreadPixmapDraw(EThread* thr, epx_pixmap_t* pixmap, epx_window_t* window,
		      int off_screen,
		      int src_x, int src_y, int dst_x, int dst_y,
		      unsigned int width,
		      unsigned int height)
{
    EThreadRequest_t req;
    // EThreadReply_t   rep;

    req.type = ETHREAD_REQUEST_DRAW;
    req.u.rq_draw.pixmap = pixmap;
    req.u.rq_draw.window = window;
    req.u.rq_draw.off_screen = off_screen;
    req.u.rq_draw.src_x = src_x;
    req.u.rq_draw.src_y = src_y;
    req.u.rq_draw.dst_x = dst_x;
    req.u.rq_draw.dst_y = dst_y;
    req.u.rq_draw.width = width;
    req.u.rq_draw.height = height;

    pthread_mutex_lock(&thr->dlock);
    thr->dcount++;
    pthread_mutex_unlock(&thr->dlock);

    write(thr->ctl, (void*) &req, sizeof(req));

    // read(thr->ctl, (void*) &rep, sizeof(rep));
    return 0;
}

int EThreadDrawCount(EThread* thr)
{
    int count;
    pthread_mutex_lock(&thr->dlock);
    count = thr->dcount;
    pthread_mutex_unlock(&thr->dlock);
    return count;
}

int EThreadSwap(EThread* thr, epx_window_t* window)
{
    EThreadRequest_t req;
    req.type = ETHREAD_REQUEST_SWAP;
    req.u.rq_swap.window = window;
    write(thr->ctl, (void*) &req, sizeof(req));
    return 0;
}

// Sync thread loop (before chaning screen size etc)
int EThreadSync(EThread* thr)
{
    EThreadRequest_t req;
    EThreadReply_t   rep;

    req.type = ETHREAD_REQUEST_SYNC;
    write(thr->ctl, (void*) &req, sizeof(req));
    read(thr->ctl, (void*) &rep, sizeof(rep)); 
    return 0;
}

int EThreadEventRead(EThread* thr, epx_event_t* e, u_int16_t mask)
{
    EThreadRequest_t req;
    EThreadReply_t   rep;
    
    req.type = ETHREAD_REQUEST_EVENT_READ;
    req.u.rq_event_read.mask = mask;
    write(thr->ctl, (void*) &req, sizeof(req));
    read(thr->ctl, (void*) &rep, sizeof(rep));
    *e = rep.u.e;
    return rep.status;
}

static void* EThreadMain(void* arg)
{
    sigset_t newset;
    EThreadArgs_t* args = (EThreadArgs_t*) arg;
    int nfd;
    EThread* thr = args->thr;
    int ctl_fd  = args->ctl_fd;
    struct pollfd pfd[2];

    /* we are not interested in signals */
    sigemptyset(&newset);
    sigaddset(&newset, SIGINT);   /* block interrupt */
    sigaddset(&newset, SIGCHLD);  /* block child signals */
    sigaddset(&newset, SIGUSR1);  /* block user defined signal */
    pthread_sigmask(SIG_BLOCK, &newset, NULL);

    free(arg);

    DBGFMT("thread started=%d\n", pthread_self());

    // Poll on control channel
    pfd[0].fd = ctl_fd;
    pfd[0].events = POLLIN;
    nfd = 1;

    // Poll for events (if possible)    
    if ((pfd[1].fd = (int) epx_backend_event_attach(thr->backend)) >= 0) {
#ifdef EPIC_THREAD_FIONBIO
	int one = 1;
	ioctl(pfd[1].fd, FIONBIO, &one);
#else
	fcntl(pfd[1].fd, F_SETFL, fcntl(pfd[1].fd, F_GETFL, 0) | O_NONBLOCK);
#endif
	pfd[1].events = POLLIN;
	nfd++;
    }

    while(1) {
	int k;
	int timeout = -1;

	if ((k = m1Poll(pfd, nfd, timeout)) <= 0) {
	    if (k == 0)
		continue;
	    else if (errno == EINTR) 
		continue;
	    else if (errno == EBADF)
		return 0; // closed or something bad 
	    else {
		perror("poll"); // Fatal error
		exit(1);
	    }
	}

	if (pfd[0].revents & POLLIN) {
	    EThreadRequest_t req;
	    EThreadReply_t rep;
	    int n;

	    if ((n = read(pfd[0].fd, &req, sizeof(req))) != sizeof(req)) {
	      if (n == 0) {
		close(ctl_fd);
		pthread_exit((void*) 0);
	      }
	      else {
		perror("read");
		close(ctl_fd);
		pthread_exit((void*) -1);
		return 0;
	      }
	    }
	    switch(req.type) {
	    case ETHREAD_REQUEST_SYNC: {
		rep.type = ETHREAD_REPLY_SYNC;
		rep.status = 0;
		write(ctl_fd, &rep, sizeof(rep));
		break;
	    }

	    case ETHREAD_REQUEST_SWAP: {

		epx_backend_window_swap(thr->backend, req.u.rq_swap.window);
		break;
	    }

	    case ETHREAD_REQUEST_DRAW: {
		// Here we start with the reply beacuse PixmapDraw will block

		// rep.type = ETHREAD_REPLY_DRAW;
		// rep.status = 0;
		// write(ctl_fd, &rep, sizeof(rep));
		// Now draw the pixmap
		// FIXME: we must lock the pixmap/window lists!

		
		epx_backend_pixmap_draw(thr->backend,
					req.u.rq_draw.pixmap,
					req.u.rq_draw.window,
#warning "TONY FIXME: Off_screen flag not accepted by epx"
//					req.u.rq_draw.off_screen,
					req.u.rq_draw.src_x,req.u.rq_draw.src_y,
					req.u.rq_draw.dst_x,req.u.rq_draw.dst_y,
					req.u.rq_draw.width,req.u.rq_draw.height);
		pthread_mutex_lock(&thr->dlock);
		thr->dcount--;
		pthread_mutex_unlock(&thr->dlock);
		break;
	    }

	    case ETHREAD_REQUEST_EVENT_READ: {
		int p;

#warning "TONY FIXME: How do we filter on req.u.rq_event_read.mask"		
		p = epx_backend_event_read(thr->backend, &rep.u.e
//					   ,req.u.rq_event_read.mask
		    );
		rep.type = ETHREAD_REPLY_EVENT_READ;
		rep.status = p;
		write(ctl_fd, &rep, sizeof(rep));

		// Store mask for next read and deliver result.
		break;
	    }

	    default:
		break;
	    }
	}
	
//	if ((nfd>1) && (pfd[1].revents & POLLIN)) {
//	    epx_event_t e;
//	    // We may have something on the event queue
//	    EBackendEventRead(backend,&e,next_mask);
//	}
    }
}

