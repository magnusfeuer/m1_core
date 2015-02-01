//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.

//
// Runtime support
//
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "m1rt.hh"
#include <stdarg.h>

// FIXME configure
// Darwin have poll that only works on sockets & pipes (BAD karma)
#ifdef DARWIN
#include <sys/select.h>
#define HAVE_SELECT
#define HAVE_BROKEN_POLL
#else
#define HAVE_POLL
#endif

// FIXME: make debug flag capable of debug only certain objects/types?
// Fixed by mechanism below
int m1_debug_mask = 0;

// 
// String map to selectively turn debugging on and off for specific classes.
//
// #ifdef DEBUG
CDebugModules _dbgcls_map;
// #endif
/* statistics */
unsigned long m1_stat_created       = 0;
unsigned long m1_stat_deleted       = 0;
unsigned long m1_stat_retain        = 0;
unsigned long m1_stat_release       = 0;
unsigned long m1_stat_live          = 0;


// Output helpers
void m1EmitError(char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stderr, "%s:%d: ", file, line); 
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
}

void m1Emit(char* file, int line, ...)
{
    va_list ap;
    char* fmt;

    va_start(ap, line);
    fmt = va_arg(ap, char*);

    fprintf(stdout, "%s:%d: ", file, line); 
    vfprintf(stdout, fmt, ap);
    fprintf(stdout, "\r\n");
    va_end(ap);
}

//
// zero_pool: A temporary storage for objects with refCount == 0
//            This pool will be purge every cycle, we may how ever
//            put a limit on how many objects that are destroyed per
//            cylcle and adjust it if needed.
//
#define MAX_ZERO_POOL_SIZE 300000
static int zero_pool_size = 0;
static CRuntime* zero_pool[MAX_ZERO_POOL_SIZE];

int m1ZeroObjects(void) { return zero_pool_size; }

//
// active_pool: A pool that references all active objects
//
#ifdef DEBUG
#define MAX_ACTIVE_POOL_SIZE 500000
static int active_pool_size = 0;
static CRuntime* active_pool[MAX_ACTIVE_POOL_SIZE];

int m1ActiveObjects(void) { return active_pool_size; }

void m1LiveCheck(CRuntime* obj)
{
    int i;
    for (i = 0; i < zero_pool_size; i++) {
	if (obj == zero_pool[i])
	    return;
    }
    for (i = 0; i< active_pool_size; i++) {
	if (obj == active_pool[i])
	    return;
    }
    m1BreakHere(__FILE__, __LINE__, "object is deleted");
}

#else
int m1ActiveObjects(void) 
{
    return 0; 
}

void m1LiveCheck(CRuntime* obj)
{
}

#endif



#ifdef DEBUG
// Add object to the active object reference pool
void m1AddActiveObject(CRuntime* obj)
{
    if (active_pool_size == MAX_ACTIVE_POOL_SIZE) {
	fprintf(stderr, "active pool full\n");
	return;
    }
    active_pool[active_pool_size] = obj;
    obj->mPoolIndex = active_pool_size;
    DBGFMT_MEM("ACTIVE THIS: 0x%08lx: %lu @active_pool[%d]", 
	       (unsigned long) obj, obj->refCount(), obj->mPoolIndex);
    active_pool_size++;
}

// Remove object to the active reference pool 
// When mRefCount == 0 and the object is transfered to zero_pool
void m1RemoveActiveObject(CRuntime* obj)
{
    int i;

    if (((i = obj->mPoolIndex) < 0) || (i >= active_pool_size)) {
	m1BreakHere(__FILE__, __LINE__, "object has bad pool index");
	return;
    }
    DBGFMT_MEM("NOT ACTIVE THIS: 0x%08lx: %lu @active_pool[%d]", 
	       (unsigned long) obj, obj->refCount(), obj->mPoolIndex);
    obj->mPoolIndex = -1;
    // move the last object to the gap created, if obj was not the last one
    active_pool_size--;
    if (i < active_pool_size) {
	active_pool[i] = active_pool[active_pool_size];
	active_pool[i]->mPoolIndex = i;
    }
}


#endif

/*
 *  Mark a va_list ended with special MARK_VA_END ((CRuntime*) 1)
 *  Note that NULL may be present in input but bever a pointer to 1
 */
int m1MarkObjects(Mark_t aMark, ...)
{
    va_list ap;
    int marked = 0;
    CRuntime* obj;

    va_start(ap, aMark);
    while((obj = va_arg(ap, CRuntime*)) != MARK_VA_END) {
	if (obj && (obj->mMark != aMark)) 
	    marked += obj->mark(aMark);
    }
    va_end(ap);
    return marked;
}

int m1MarkVector(Mark_t aMark, CRuntime** aVec, size_t n)
{
    int marked = 0;

    while(n--) {
	CRuntime* obj = *aVec++;
	if (obj && (obj->mMark != aMark))
	    marked += obj->mark(aMark);
    }
    return marked;
}

// Add object to the zero reference pool
// Either object is created or object was released (to refCount === 0)
static void m1AddZeroObject(CRuntime* obj)
{
    if (zero_pool_size == MAX_ZERO_POOL_SIZE) {
	fprintf(stderr, "zero pool full\n");
	// FIXME: deallocate or add segments
	return;
    }
#ifdef DEBUG
    if (obj->mPoolIndex != -1)
	m1RemoveActiveObject(obj);
#endif
    zero_pool[zero_pool_size] = obj;
    obj->mPoolIndex = zero_pool_size;
    DBGFMT_MEM("ZERO THIS: 0x%08lx: %lu @zero_pool[%d]", 
	       (unsigned long) obj, obj->refCount(), obj->mPoolIndex);
    zero_pool_size++;
}


// Remove object to the zero reference pool 
// Either the object as mRefCount > 0 or object is beeing deleted
static void m1RemoveZeroObject(CRuntime* obj)
{
    int i;

    if (((i = obj->mPoolIndex) < 0) || (i >= zero_pool_size)) {
	m1BreakHere(__FILE__, __LINE__, "object has bad pool index");
	return;
    }
#ifdef DEBUG
    if (obj->refCount() > 0)
	m1AddActiveObject(obj);
    else
	obj->mPoolIndex = -1;
#else
    obj->mPoolIndex = -1;
#endif
    DBGFMT_MEM("UNZERO THIS: 0x%08lx: %lu @zero_pool[%d]", 
	       (unsigned long) obj, obj->refCount(), obj->mPoolIndex);
    // move the last object to the gap created, if obj was not the last one
    zero_pool_size--;
    if (i < zero_pool_size) {
	zero_pool[i] = zero_pool[zero_pool_size];
	zero_pool[i]->mPoolIndex = i;
    }
}


//
//  Runtime Executor 
//
CRtExecutor::CRtExecutor(void)
{
}

CRtExecutor::~CRtExecutor(void)
{
}

void* CRuntime::operator new(size_t size) 
{
    CRuntime* p = (CRuntime*) malloc(size);
    p->mPoolIndex = -1;
    m1AddZeroObject(p);
    return (void*) p;
}

void CRuntime::operator delete(void* ptr)
{
    ((CRuntime*) ptr)->mPoolIndex = -1;
    ((CRuntime*) ptr)->mRefCount  = 0;
    free(ptr);
}

CRuntime::CRuntime(void)
{
    mRefCount = 0;
    mMark = MARK_INIT;
    STAT_INC(m1_stat_live);
    STAT_INC(m1_stat_created);
}
//
// Retain the object - (keep it) may move object from zero pool
//
CRuntime* CRuntime::retainThis(void) 
{
    DBGFMT_MEM("RETAIN THIS: 0x%08lx: %lu  %s", 
	       (unsigned long) this, mRefCount, debugName().c_str());
    mRefCount++;
    STAT_INC(m1_stat_retain);
    if (mRefCount == 1)
	m1RemoveZeroObject(this);
    return this;
}

//
// Release the object - may end up on zero pool
//
CRuntime* CRuntime::releaseThis(void)
{
    STAT_INC(m1_stat_release);
    switch(mRefCount) {
    case 0:
	m1BreakHere(__FILE__, __LINE__, "releaseThis on zero object");
	return this;
    case 1:
	DBGFMT_MEM("RELEASE THIS: 0x%08lx: %lu  %s", 
		   (unsigned long) this, mRefCount, debugName().c_str());
	mRefCount = 0;
	m1AddZeroObject(this);
	return this;
    default:
	DBGFMT_MEM("RELEASE THIS: 0x%08lx: %lu  %s", 
		   (unsigned long) this, mRefCount, debugName().c_str());
	mRefCount--;
	return this;
    }
}

void CRuntime::stop(CRtExecutor* aExec)
{
    DBGFMT_MEM("STOP THIS: 0x%08lx: %lu  %s", 
	       (unsigned long) this, mRefCount, debugName().c_str());
    STAT_INC(m1_stat_deleted);
    if (mRefCount != 0) {
	m1BreakHere(__FILE__, __LINE__, "stop on live object");
    }
    else {
	m1RemoveZeroObject(this);
	delete this;
    }
}

string CRuntime::debugName(void) 
{
    char ndata[256];
    sprintf(ndata, "Runtime:<destroyed> #%lu 0x%lx", 
	    refCount(), (unsigned long) this);
    return string(ndata);
}

CRuntime::~CRuntime()
{
    STAT_DEC(m1_stat_live);
}


void m1BreakHere(const char* file, int line, char* message)
{
    fprintf(stderr, "%s:%d: %s\n", file, line, message);
#ifdef DEBUG
    exit(1);
#endif
}


// Take object from last poition if not empty
static CRuntime* lastZeroObject(void)
{
    if (zero_pool_size == 0)
	return NULL;
    return zero_pool[zero_pool_size-1];
}

// Take object from first pool position if not empty
// Not used right now
#if 0
static CRuntime* firstZeroObject(void)
{
    if (zero_pool_size == 0)
	return NULL;
    return zero_pool[0];
}
#endif

int m1DeleteZeroObjects(CRtExecutor* aExec)
{
    CRuntime* obj;
    int objectsDeleted = 0;
#ifdef DEBUG_MEM
    static unsigned count = 0;
#endif
    while((obj = lastZeroObject()) != NULL) {
#ifdef DEBUG
	int zSize = zero_pool_size;
	string debug = obj->debugName();
#ifdef DEBUG_MEM
	if ((count & 15) == 15)
	    fprintf(stderr, "Delete: %s\n", debug.c_str());
#endif
#endif
	DBGFMT_MEM("m1DeleteZeroObjects: Delete: %s", debug.c_str());
	obj->stop(aExec);
	DBGFMT_MEM("m1DeleteZeroObjects: propagated %d objects to pool",
		   zero_pool_size - (zSize-1));
	objectsDeleted++;
    }
#ifdef DEBUG_MEM
    count++;
#endif
    return objectsDeleted;
}

int m1DeletedUnMarkedObjects(CRtExecutor* aExec, unsigned int aMark)
{
    int objectsDeleted = 0;
#ifdef DEBUG
    int i = 0;
    while(i < active_pool_size) {
	CRuntime* obj = active_pool[i];
	if (obj && (obj->mMark != MARK_NO_SWEEP) && (obj->mMark != aMark)) {
	    string debug = obj->debugName();

	    fprintf(stderr,"m1DeleteUnMarkedObjects: Delete: %s\n", 
		    debug.c_str());
#if 0  // Does not really work (since we have loops)
	    m1RemoveActiveObject(obj);
	    obj->clearRefCount();
	    m1AddZeroObject(obj);
	    obj->stop(aExec);
#endif
	    objectsDeleted++;
	    // Do not updated i since m1RemoveActiveObject swap in last object
	    i++; // TESTING 
	}
	else {
	    i++;
	}
    }
#endif
    return objectsDeleted++;
}

/* compatiblilty problems on mac os x :-(  */
#if defined(HAVE_POLL)
int m1Poll(struct pollfd* fds, int nfds, int timeout)
{
    return poll(fds, nfds, timeout);
}
#elif defined(HAVE_SELECT) || defined(HAVE_BROKEN_POLL)
#include <sys/select.h>
int m1Poll(struct pollfd* fds, int nfds, int timeout)
{
    fd_set rfds;
    fd_set wfds;
    fd_set xfds;
    struct timeval tm;
    struct timeval* tp;
    int maxfd = 0;
    int i;
    int r;

    FD_ZERO(&rfds);
    FD_ZERO(&wfds);
    FD_ZERO(&xfds);

    if (timeout < 0)
	tp = NULL;
    else {
	tm.tv_sec = timeout / 1000;
	tm.tv_usec = (timeout % 1000)*1000;
	tp = &tm;
    }

    for (i = 0; i< nfds; i++) {
	if (fds[i].fd < 0)
	    fds[i].revents = POLLNVAL;
	else {
	    fds[i].revents = 0;

	    if (fds[i].events & POLLIN) {
		FD_SET(fds[i].fd, &rfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#ifdef POLLRDNORM
	    if (fds[i].events & POLLRDNORM) {
		FD_SET(fds[i].fd, &rfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#endif
#ifdef POLLRDBAND
	    if (fds[i].events & POLLRDBAND) {
		FD_SET(fds[i].fd, &xfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#endif
	    if (fds[i].events & POLLOUT) {
		FD_SET(fds[i].fd, &wfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#ifdef 	POLLWRNORM
	    if (fds[i].events & POLLWRNORM) {
		FD_SET(fds[i].fd, &wfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#endif

#ifdef 	POLLWRBAND
	    if (fds[i].events & POLLWRBAND) {
		FD_SET(fds[i].fd, &xfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#endif
	    
#ifdef POLLPRI
	    if (fds[i].events & POLLPRI) {
		FD_SET(fds[i].fd, &xfds);
		if (fds[i].fd > maxfd) maxfd = fds[i].fd;
	    }
#endif
	}
    }
    
again:
    if ((r = select(maxfd+1, &rfds, &wfds, &xfds, tp)) <= 0) {
	/* probably an interrupt or poll with no input */
	if ((r == -1) && (errno == EBADF)) {
	    int fixed = 0;
	    /* check which fd that is bad */
	    for (i = 0; i <= nfds; i++) {
		if ((fds[i].fd >= 0) && FD_ISSET(fds[i].fd, &rfds)) {
		    fd_set efds;
		    tm.tv_sec  = 0;
		    tm.tv_usec = 0;
		    tp = &tm;
		    FD_ZERO(&efds);
		    FD_SET(fds[i].fd, &efds);
		    if (select(fds[i].fd+1, &efds, NULL, NULL, tp) == -1) {
			FD_CLR(fds[i].fd, &rfds);
			fds[i].revents |= POLLNVAL;
			fixed++;
		    }
		}
		if ((fds[i].fd >= 0) && FD_ISSET(fds[i].fd, &wfds)) {
		    fd_set efds;
		    tm.tv_sec  = 0;
		    tm.tv_usec = 0;
		    tp = &tm;
		    FD_ZERO(&efds);
		    FD_SET(fds[i].fd, &efds);
		    if (select(fds[i].fd+1, NULL, &efds, NULL, tp)  == -1) {
			FD_CLR(fds[i].fd, &wfds);
			fds[i].revents |= POLLNVAL;
			fixed++;
		    }
		}
	    }
	    if (fixed)
		goto again;
	}
	return r;
    }

    for (i = 0; i < nfds; i++) {
	/* FIXME: Handle HUP and pri data */
	if (FD_ISSET(fds[i].fd, &rfds))
	    fds[i].revents |= POLLIN;
	if (FD_ISSET(fds[i].fd, &wfds))
	    fds[i].revents |= POLLOUT;
    }
    return r;
}

#else
#error "no poll method found"
#endif

    


