//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __M1RT_HH__
#define __M1RT_HH__

#include <stdlib.h>
#include <sys/types.h>
#include <poll.h>

#include <string>
using namespace std;

extern int m1_debug_mask;

#define M1DBG_ALL    0xFFFF
#define M1DBG_INFO   0x0001
#define M1DBG_WARN   0x0010  // emit warnings
#define M1DBG_DDS    0x0020  // dds rendering debugging
#define M1DBG_TYPE   0x0040  // type system debugging
#define M1DBG_SCHD   0x0080  // schedule debugging
#define M1DBG_LINT   0x0100  // lint phase debugging
#define M1DBG_EVAL   0x0200  // eval phase debugging
#define M1DBG_COMP   0x0400  // compile phase debugging
#define M1DBG_EXEC   0x0800  // execute phase debugging
#define M1DBG_PRNT   0x2000  // print data / types 
#define M1DBG_MEM    0x4000  // detailed memory debug
#define M1DBG_MNFO   0x8000  // overall memory info

//#ifdef DEBUG
#include <list>
typedef list<string> CDebugModules;
typedef list<string>::iterator CDebugModulesIterator;

#ifdef DEBUG

#define M1DBG_IS_SET(flags) ((m1_debug_mask & (flags)) == (flags))
#define DBGFMT_mask(mask,...)			\
    do {								\
	if (M1DBG_IS_SET((mask)))					\
	    m1EmitError((char*) __FILE__, __LINE__, (char*) __VA_ARGS__); \
    } while(0)


#define DBGFMT_ADDCLS(classname) {		     \
         extern CDebugModules _dbgcls_map;	     \
         _dbgcls_map.push_back(classname);  \
}

#define DBGFMT_CLS(...) {					                        \
	extern CDebugModules _dbgcls_map;				                \
	CDebugModulesIterator _dbgcls_iter;                                             \
	for( _dbgcls_iter = _dbgcls_map.begin();                                        \
	     _dbgcls_iter != _dbgcls_map.end() && *_dbgcls_iter != type()->typeName();	\
            ++_dbgcls_iter);							\
        if (_dbgcls_iter != _dbgcls_map.end())				                \
	    m1EmitError((char*) __FILE__, __LINE__, __VA_ARGS__);	\
}

#else
#define DBGFMT_CLS(...)
#define DBGFMT_ADDCLS(...)

#define M1DBG_IS_SET(flags) 0
#define DBGFMT_mask(mask,...)
#endif

#define ERRFMT(...)      m1EmitError((char*) __FILE__, __LINE__, __VA_ARGS__)
#define WARNFMT(...)						\
    do {							\
	if (m1_debug_mask & M1DBG_WARN)				\
	    m1EmitError((char*) __FILE__, __LINE__, __VA_ARGS__);	\
    } while(0)

#define DBGFMT(...)      DBGFMT_mask(M1DBG_INFO,__VA_ARGS__)
#define DBGFMT_MEM(...)  DBGFMT_mask(M1DBG_MEM,__VA_ARGS__)
#define DBGFMT_MNFO(...) DBGFMT_mask(M1DBG_MNFO,__VA_ARGS__)
#define DBGFMT_LINT(...) DBGFMT_mask(M1DBG_LINT,__VA_ARGS__)
#define DBGFMT_TYPE(...) DBGFMT_mask(M1DBG_TYPE,__VA_ARGS__)
#define DBGFMT_EVAL(...) DBGFMT_mask(M1DBG_EVAL,__VA_ARGS__)
#define DBGFMT_COMP(...) DBGFMT_mask(M1DBG_COMP,__VA_ARGS__)
#define DBGFMT_WARN(...) DBGFMT_mask(M1DBG_WARN,__VA_ARGS__)
#define DBGFMT_DDS(...)  DBGFMT_mask(M1DBG_DDS,__VA_ARGS__)
#define DBGFMT_SCHD(...) DBGFMT_mask(M1DBG_SCHD,__VA_ARGS__)

#ifdef DEBUG
#define STAT_INC(stat)  (stat)++
#define STAT_DEC(stat)  (stat)--
#else
#define STAT_INC(stat)
#define STAT_DEC(stat)
#endif

#define MARK_INIT      0xffffffff
#define MARK_NO_SWEEP  0xfffffffe
#define MARK_VA_END    ((CRuntime*) 1)


extern unsigned long m1_stat_created;       // total objects created
extern unsigned long m1_stat_deleted;       // total objects deleted
extern unsigned long m1_stat_live;          // total runtime objects
extern unsigned long m1_stat_retain;        // total number of retains
extern unsigned long m1_stat_release;       // total number of releases

typedef unsigned long      RefCount;        // reference counter type
typedef unsigned long      Mark_t;          // type used for mark/sweep

class CRtExecutor {
public:
    CRtExecutor();
    ~CRtExecutor(void);

};

//////////////////////////////////////////////////////////////////////////////
//
// CRuntime - Class for all runtime objects with zero pool handling
//
//////////////////////////////////////////////////////////////////////////////

class CRuntime {
public:
    CRuntime(void);

    static void* operator new(size_t size);

    static void  operator delete (void* ptr);

    virtual void stop(CRtExecutor* aExec);

    CRuntime* retainThis(void);

    CRuntime* releaseThis(void);

    RefCount refCount(void) { return mRefCount; }

    virtual string debugName(void);

    // Used by mark/sweep to delete non accessible (lost) objects
    void clearRefCount(void) { mRefCount = 0; }
    // mark an object return 0 if already marked 1 otherwise
    virtual int mark(Mark_t aMark) {
	int marked = (aMark != mMark);
	mMark = aMark;
	return marked;
    }
public:
    int mPoolIndex;      // must be accessible from m1rt (DONT TOUCH)
    unsigned long mMark; // mark to detect unaccessible data
protected:
    virtual ~CRuntime();
private:
    RefCount mRefCount;
};

extern void m1EmitError(char* file, int line, ...);
extern void m1Emit(char* file, int line, ...);
extern int  m1ActiveObjects(void);
extern int  m1ZeroObjects(void);
extern void m1LiveCheck(CRuntime* obj);

extern int  m1MarkObjects(Mark_t aMark, ...);
extern int  m1MarkVector(Mark_t aMark, CRuntime** aVec, size_t n);

// Please use m1Poll instead of poll, since mac is a bit broken
extern int  m1Poll(struct pollfd* fds, int nfds, int timeout);

extern int  m1DeleteZeroObjects(CRtExecutor* aExec);
extern int m1DeletedUnMarkedObjects(CRtExecutor* aExec, unsigned int aMark);

extern void m1BreakHere(const char* file, int line, char* message);

// m1Mark will mark the object if non NULL
#define m1Mark(ptr,m) ((ptr) ? (ptr)->mark(m) : 0)
// m1MarkArgs will mark a list of CRuntime pointers until MARK_VA_END
#define m1MarkArgs(m, ...) m1MarkObjects((m), __VA_ARGS__, MARK_VA_END)
// m1MarkAndCheck is assumed to run in a RunTime object (checking for mark)
// calling also parent class mark when object is not already marked
#define m1MarkAndCheck(m, pClass,  ...)				\
    ((mMark == (m)) ? 0 : (pClass :: mark((m))) +		\
     m1MarkObjects((m), __VA_ARGS__, MARK_VA_END))

#define m1Retain(type,ptr)  ((ptr) ? ((type *) (ptr)->retainThis()) : NULL)
#define m1Release(type,ptr) ((ptr) ? ((type *) (ptr)->releaseThis()) : NULL)
#define m1New(type,...)	new type(__VA_ARGS__)
#define m1Set(type,loc,ptr) do	{		\
	type* _nPtr = (ptr);			\
	m1Release(type,*(loc));			\
	*((type **)(loc)) = (type *) (_nPtr);	\
    } while(0)
#define m1SetRetain(type,loc,ptr) do {		\
	type* _nPtr = (ptr);			\
	m1Retain(type,_nPtr);			\
	m1Release(type,*(loc));			\
	*((type **)(loc)) = (type *) (_nPtr);	\
    } while(0)
// if *loc is different from ptr then
//   if *loc==NULL then do m1Set else do m1SetRetain
#define m1CondSetRetain(type,loc,ptr) do {			\
	type* _nPtr = (ptr);					\
	if (*(loc) != _nPtr) {					\
	    if (*(loc) != NULL) {				\
		m1Retain(type,_nPtr);				\
		m1Release(type,*(loc));				\
	    }							\
	    *((type **)(loc)) = (type *) (_nPtr);		\
	}							\
    } while(0)


#endif

