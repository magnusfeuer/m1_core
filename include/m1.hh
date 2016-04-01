//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __M1_HH__
#define __M1_HH__

#include "config.h"

#include <list>
#include <queue>
#include <iostream>
#include <sstream>
#include <vector>
#include <poll.h>
#include <stdio.h>
using namespace std;

#include "m1rt.hh"

#define MAX_VALUE_STACK_SIZE  10000
#define MAX_RETURN_STACK_SIZE 10000

//
//
//
#define MAX_TYPE_DEPTH 64
//
// Maximume scope depth when executing objects
//
#define ACTIVATION_LEVELS  4

typedef enum {
    SCOPE_NONE     = -1,  // Not represented
    SCOPE_UNIVERSE =  0,  // universe levels for library access
    SCOPE_GLOBAL   =  1,  // access to current library
    SCOPE_OBJECT   =  2,  // access to this
    SCOPE_STACK    =  3   // the current stack object
} ActivationType_t;


#define RETURN_BLOCK_SIZE  4
#define RETURN_SAVE_SIZE   ACTIVATION_LEVELS

//
// A timestamp measuring millisec since epoch.
//
typedef unsigned long      Time;       // millisecond time stamp (fits in udata)
typedef unsigned long long TimeStamp;  // micro second stamp type
typedef unsigned long      CycleCount; // cycle counter type


#define TIME_SEC   1000      // Seconds per Time step (milli seconds)
#define STAMP_SEC  1000000   // Seconds per TimeStamp step (micro seconds)
#define STAMP_TIME 1000     // STAMP_SEC / TIME_SEC

inline double m1_TimeStampToSec(TimeStamp aTimeStamp) {
    return double(aTimeStamp / STAMP_SEC) + 
	((aTimeStamp % STAMP_SEC) / double(STAMP_SEC));
}

inline double m1_TimeToSec(Time aTimeStamp) {
    return double(aTimeStamp / TIME_SEC) + 
	((aTimeStamp % TIME_SEC) / double(TIME_SEC));
}

inline Time m1_TimeStampToTime(TimeStamp aTimeStamp) {
    return Time(aTimeStamp / STAMP_TIME);
}

extern TimeStamp m1_timeStamp(void);

class CType;
class CBaseType;
class CStringType;
class CObject;
class CString;
class CArray;
class CBaseObject;
class CExecutable;
class CEvent;
class CSystem;
class CExecutor;
class CFileSource;

typedef void*  Instruction;

union UData {
    unsigned char      b;   // byte
    char               c;   // char
    bool               t;   // bool - t for truth value
    signed int         i;
    unsigned int       u;
    float              f;
    CRuntime*          r;
    CObject*           o;    // Component object (string/array)
    CString*          str;   // String object
    CArray*           arr;   // Array object
    CEvent*           evt;   // event CHAR/.../FLOAT/...
    Time               tm;   // millisecond Time stamp
    Instruction*     addr;   // Instruction pointer
    void*              p;    // Any kind of thing 
    UData*            ud;    // pointer to UData
};

typedef enum {
    TRIGGER_AUTO      = 0,   // default rules apply
    TRIGGER_YES       = 1,   // force trigger
    TRIGGER_NO        = 2,   // force trigger suppression
    TRIGGER_PROPAGATE = 3    // default rules, but from propagate
} Trig_t;

typedef enum {
    T_TYPE,
    T_LIBRARY,
    T_APPLICATION,
    T_TOPLEVEL
} Definition_t;

typedef struct {
    CObject* obj;
    int      index;
    Trig_t   trig;
} UAddress;


typedef std::vector<UData> UDataVector;

extern UData nil;

enum {
    Q_NONE       = 0x00,
    Q_PUBLIC     = 0x01,
    Q_PRIVATE    = 0x02,
    Q_EXTERN     = 0x04,
    Q_CONST      = 0x08,
    Q_PERSISTENT = 0x10,
    Q_PROTECTED  = 0x20
};

enum {
    E_INPUT      = 0x01,
    E_OUTPUT     = 0x02,
    E_INOUT      = 0x03
};


/* type codes:
 *  family:2  NONE, INTEGER, FLOAT, POINTER
 *  size:2    OCTET,SINGLE,WORD,DOUBLE
 *  class:3   NUMERIC,STRING,ARRAY,
 *  sign:1    UNSIGNED, SIGNED
 */
#define M1T_TAG_MASK  0x03
#define M1T_NONE      0x00
#define M1T_INTEGER   0x01
#define M1T_FLOAT     0x02
#define M1T_POINTER   0x03

#define M1T_SIZE_MASK 0x0C
#define M1T_OCTET     0x00
#define M1T_WORD      0x04
#define M1T_SINGLE    0x08
#define M1T_DOUBLE    0x0C  /* future use ! */

#define M1T_CLASS_MASK 0x70
#define M1T_NUMERIC  0x00  /* numeric types (float/integer) */
#define M1T_BOOLEAN  0x10  /* boolean (integer) */
#define M1T_STRING   0x20  /* strings (CString) */
#define M1T_ARRAY    0x30  /* array objects (CArray) */
#define M1T_OBJECT   0x40  /* c++ objects (CBaseObject) */
#define M1T_EVENT    0x50  /* event object */
#define M1T_VOID     0x60  /* nothing */
#define M1T_ERROR    0x70  /* error */

#define M1T_SIGN_MASK 0x80
#define M1T_UNSIGNED 0x00
#define M1T_SIGNED   0x80


typedef enum {
    M1TYPE_NONE     = (M1T_NONE),
    M1TYPE_ERROR    = (M1T_UNSIGNED | M1T_ERROR  | M1T_SINGLE | M1T_NONE),
    M1TYPE_VOID     = (M1T_UNSIGNED | M1T_VOID   | M1T_SINGLE | M1T_NONE),
    M1TYPE_ANY      = (M1T_UNSIGNED | M1T_VOID   | M1T_WORD   | M1T_NONE),

    M1TYPE_NUMERIC  = (M1T_UNSIGNED | M1T_NUMERIC| M1T_SINGLE | M1T_NONE),
    M1TYPE_INTEGER  = (M1T_UNSIGNED | M1T_NUMERIC| M1T_WORD   | M1T_INTEGER),

    M1TYPE_BYTE     = (M1T_UNSIGNED | M1T_NUMERIC | M1T_OCTET | M1T_INTEGER),
    M1TYPE_CHAR     = (M1T_SIGNED   | M1T_NUMERIC | M1T_OCTET | M1T_INTEGER),
    M1TYPE_BOOL     = (M1T_UNSIGNED | M1T_BOOLEAN | M1T_OCTET | M1T_INTEGER),

    M1TYPE_SIGNED   = (M1T_SIGNED   | M1T_NUMERIC | M1T_SINGLE | M1T_INTEGER),
    M1TYPE_UNSIGNED = (M1T_UNSIGNED | M1T_NUMERIC | M1T_SINGLE | M1T_INTEGER),

    M1TYPE_FLOAT    = (M1T_SIGNED   | M1T_NUMERIC | M1T_SINGLE | M1T_FLOAT),

    M1TYPE_STRING   = (M1T_STRING   | M1T_WORD   | M1T_POINTER),
    M1TYPE_ARRAY    = (M1T_ARRAY    | M1T_WORD   | M1T_POINTER),
    M1TYPE_OBJECT   = (M1T_OBJECT   | M1T_WORD   | M1T_POINTER),
    M1TYPE_EVENT    = (M1T_EVENT    | M1T_WORD   | M1T_POINTER),

    BUILTIN_ARG_END  = (M1T_SIGNED | M1T_VOID | M1T_POINTER),
    BUILTIN_ARG_VARG = (M1T_UNSIGNED | M1T_VOID | M1T_POINTER)
} M1TypeTag;

typedef UData (*UFunction)(CExecutor* aExec, UData*);

#define MAX_BUILTIN_ARGS 128

typedef struct {
    M1TypeTag rtype;
    char* name;
    bool  pure;  // return value depend purly on input 
    UFunction func;
    M1TypeTag atype[MAX_BUILTIN_ARGS];
} UBuiltin;


enum EFlags {
    // Executable in sweeper queue
    ExecutableInQueue     = 0x01,
    // Executable in sweeper next queue (rescheduled)
    ExecutableInNextQueue = 0x02,
    // Call execute() every sweep.
    ExecuteEverySweep     = 0x04,
    // Call when one or more subscribed event are updated.
    ExecuteOnEventUpdate = 0x08
};

typedef unsigned int UpdateStatus;
enum {
    UpdateStatusNo        = 0x00,
    UpdateStatusYes       = 0x01,     // is updated
    UpdateStatusLater     = 0x02,     // keep beeing updated
    UpdateStatusMark      = 0x04,     // set when in Executable update list
    UpdateStatusLocal     = 0x10,     // Local update of event variable
    UpdateStatusRemote    = 0x20,     // Remote updated of event variable
    UpdateStatusPropagate = 0x40,     // Trigg update of event variable
    UpdateStatusAssigned  = 0x70      // Local|Remote|Propagate
};

extern CType*    error_type(void);
extern CType*    void_type(void);
extern CType*    any_type(void);
extern CType*    numeric_type(void);
extern CType*    integer_type(void);
extern CType*    byte_type(void);
extern CType*    char_type(void);
extern CType*    bool_type(void);
extern CType*    signed_type(void);
extern CType*    unsigned_type(void);
extern CType*    float_type(void);
extern CType*    string_type(void);
extern CType*    time_type(void);

/* predefined event types */
extern CType*    event_byte_type(void);
extern CType*    event_char_type(void);
extern CType*    event_bool_type(void);
extern CType*    event_signed_type(void);
extern CType*    event_unsigned_type(void);
extern CType*    event_float_type(void);
extern CType*    event_string_type(void);
extern CType*    event_time_type(void);

extern CType*    input_byte_type(void);
extern CType*    input_char_type(void);
extern CType*    input_bool_type(void);
extern CType*    input_signed_type(void);
extern CType*    input_unsigned_type(void);
extern CType*    input_float_type(void);
extern CType*    input_string_type(void);
extern CType*    input_time_type(void);

extern CType*    output_byte_type(void);
extern CType*    output_char_type(void);
extern CType*    output_bool_type(void);
extern CType*    output_signed_type(void);
extern CType*    output_unsigned_type(void);
extern CType*    output_float_type(void);
extern CType*    output_string_type(void);
extern CType*    output_time_type(void);


extern CType* event_queue_byte_type(void);
extern CType* event_queue_char_type(void);
extern CType* event_queue_bool_type(void);
extern CType* event_queue_signed_type(void);
extern CType* event_queue_unsigned_type(void);
extern CType* event_queue_float_type(void);
extern CType* event_queue_time_type(void);
extern CType* event_queue_string_type(void);

/* create a CType given the type tag */
extern CType*    tag_type(int tag);


extern UBuiltin* lookupBuiltin(char* name);
extern UBuiltin* matchBuiltin(char* name, CType** types, int nargs,
			      UBuiltin** altBf);
extern string formatTypeTag(M1TypeTag tag);
extern string formatCall(char* fname, CType** types, int nargs);
extern string formatBuiltin(UBuiltin* bf);
extern int    nargsBuiltin(UBuiltin* bf);


extern unsigned long m1_stat_live_types;    // total types
extern unsigned long m1_stat_live_elements; // total parser elements
extern unsigned long m1_stat_live_objects;  // total objects

// Wrapper inlines for creating UData
static inline UData UTrue(void) { UData r; r.t = true; return r; }
static inline UData UFalse(void) { UData r; r.t = false; return r; }
static inline UData UBool(bool t) { UData r; r.t = t; return r; }
static inline UData UByte(unsigned char b) { UData r; r.b = b; return r; }
static inline UData UChar(char c) { UData r; r.c = c; return r; }
static inline UData UUnsigned(unsigned int u) { UData r; r.u = u; return r; }
static inline UData USigned(int i) { UData r; r.i = i; return r; }
static inline UData UFloat(float f) { UData r; r.f = f; return r; }
static inline UData UString(CString* str) { UData r; r.str = str; return r; }
// Also check after CString declaration for alternate UString declaration.
static inline UData UEvent(CEvent *evt) { UData r; r.evt = evt; return r; }
static inline UData UObject(CObject* obj) { UData r; r.o = obj; return r; }
static inline UData UArray(CArray* a) { UData r; r.arr = a; return r; }
static inline UData URuntime(CRuntime* obj) { UData r; r.r = obj; return r; }

static inline UData UTime(Time t) { UData r; r.tm = t; return r; }

extern int m1_format_args(ostream* os,UData* va);

//////////////////////////////////////////////////////////////////////////////
//
// CArgs - Class for constructor arguments handling
//
//////////////////////////////////////////////////////////////////////////////

class CArgs : public CRuntime {
public:
    CArgs(CExecutable* aCreator, CBaseObject* aGlobal) { 
	mCreator = aCreator;
	mGlobal  = aGlobal;
    }

    ~CArgs(void) {}

    int mark(Mark_t aMark);

    virtual void run(CExecutor* aExec, CObject* aObj) {}
    CExecutable* creator(void) { return mCreator; }
    CBaseObject* global(void) { return mGlobal; }
private:
    CExecutable* mCreator;
    CBaseObject* mGlobal;
};

//////////////////////////////////////////////////////////////////////////////
//
// CType - Base for M1 dynamic type system
//
//////////////////////////////////////////////////////////////////////////////

#define m1RetainType(ptr)      m1Retain(CType,ptr)
#define m1ReleaseType(ptr)     m1Release(CType,ptr)
#define m1SetRetainType(loc,ptr)  m1SetRetain(CType,loc,ptr)

class CType : public CRuntime {
public:
    CType() {
	STAT_INC(m1_stat_live_types); 
	mStatObjects = 0;
	mDocumentation = NULL;
	mMark = MARK_INIT;
    }

    CType(string aName) {
	STAT_INC(m1_stat_live_types);
	mStatObjects = 0;
	mDocumentation = NULL;
	mName = aName;
	mMark = MARK_INIT;
    }

    ~CType();

    virtual void setName(string aName) { mName = aName; }
    
    virtual int  typeLevel() { return 0; }
    virtual void setTypeLevel(int aScopeLevel) {}

    virtual string  scopeName() { return mName; }
    virtual void    setScope(CType* aScope) {}
    virtual CType*  scope(void) { return NULL; }

    virtual void   setParent(CType* aParentType) {}
    virtual void   setChild(CType* aChildType) {}
    virtual CType* parentType() { return NULL; }
    virtual bool   isVmType()   { return true; }
    virtual bool   isExecutableType()   { return false; }
    virtual string parentName() { return ""; }

    // Check if this type represent a library
    virtual bool isLibrary(void) { return false; }
    virtual bool isApplication(void) { return false; }

    virtual void init(CExecutor*,CBaseObject*)       { }
    virtual void defaults(CExecutor*,CBaseObject*)   { }
    virtual void construct(CExecutor*, CBaseObject*)  { }
    virtual void destruct(CExecutor*, CBaseObject*)   { }

    virtual UData produce(CExecutor*,CBaseType* aBase=NULL, CArgs* aArgs=NULL) { return nil; }
    virtual UData produceArray(CExecutor*,size_t)  { return nil; }
    virtual UData produceEvent(CExecutor*,bool)    { return nil; }

    virtual UData  retainObject(UData a)      { return a; }
    virtual UData  releaseObject(UData a)     { return a; }
    virtual int    markObject(UData a,Mark_t aMark) { return 0; }

    virtual void   shallowCopy(CExecutor* aExec, UData src, UData* dst)   {  }
    virtual void   deepCopy(CExecutor* aExec, UData src, UData* dst)      {  }

    virtual int     compare(UData a, UData b) { return 0; }
    virtual const size_t sizeOf(void)         { return 0; }
    virtual const M1TypeTag typeTag(void)     { return M1TYPE_ANY; }
    virtual string  typeSignature(void)       { return "("+mName+")"; }
    virtual string  typeName(void)            { return mName; }

    virtual CType* typeAt(int aIndex)          { return NULL; }
    virtual CType* typeAt(string aName)         { return NULL; }
    virtual int    indexAt(string aName)        { return -1; }

    virtual void elementInit(CExecutor* aExec,UDataVector *vec,int index) {
	vec->at(index) = nil;
    }

    virtual UData elementAt(UDataVector *vec,int index) {
	return vec->at(index);
    }

    virtual void elementPut(CExecutor* aExec,UDataVector *vec,int index,
			    UData value, Trig_t trig=TRIGGER_AUTO) {
	vec->at(index) = value;
    }

    virtual void elementCopy(CExecutor* aExec, UDataVector *vec,int index,
			     UData value, Trig_t trig=TRIGGER_AUTO) {
	vec->at(index) = value;
    }

    // Print the type specifier (default is just the name)
    virtual void  print(ostream* os) { *os << name(); }

    // Print the object
    virtual void  print(ostream* os, UData a) {}
    
    string name(void) { return mName; };

    char* cname(void) { return (char*) mName.c_str(); };

    string debugName(void);

    const char*   documentation(void)  { return mDocumentation; }
    void    setDocumentation(const char* aDoc) { mDocumentation = aDoc; }

    // Check if the type is a compund type
    virtual bool isCompound(void) { return false; }
    // Set and get the frame size, used for Compound statements to generate
    // the stack frame size
    virtual unsigned int frameSize(void) { return 0; }
    virtual unsigned int frameSize(unsigned int aSize) { return 0; }
public:
    unsigned long mStatObjects;  // Number of objects of this type (debug)
private:
    string mName;                 // Name of the type
    const char*  mDocumentation;  // Documentation - description of field
};

class CVarType : public CType {
public:
    CVarType(string aVarName) : CType(aVarName) {
	mBound = NULL;
    }

    ~CVarType() { m1ReleaseType(mBound); }
    
    void bind(CType* aType) { m1SetRetainType(&mBound, aType); }
    CType* bound(void)      { return mBound; }

    string typeSignature(void) { return "{"+name()+"}"; }

private:
    CType* mBound;
};

//////////////////////////////////////////////////////////////////////////////
//
// CObject - Base object for all M1 Objects
//
//////////////////////////////////////////////////////////////////////////////

#define m1RetainObject(ptr)      m1Retain(CObject,ptr)
#define m1ReleaseObject(ptr)     m1Release(CObject,ptr)
#define m1SetRetainObject(loc,ptr)  m1SetRetain(CObject,loc,ptr)


class CObject : public CRuntime {
public:
    CObject(void)  { 
	STAT_INC(m1_stat_live_objects); 
    }

    ~CObject(void) { 
	STAT_DEC(m1_stat_live_objects);
    }

    virtual CType* type() = 0;
    virtual CType* typeAt(int index) = 0;
    virtual CType* typeAt(string aName) = 0;
    virtual int    indexAt(string aName) { return type()->indexAt(aName); }

    virtual UData at(int index) = 0;
    virtual void  put(CExecutor* aExec,int index,UData value,Trig_t trig=TRIGGER_AUTO) = 0;
    virtual void  copy(CExecutor* aExec,CType* aType,int index,UData value,Trig_t trig=TRIGGER_AUTO) = 0;

    virtual size_t size(void) { return 0; }
    //
    // Insert a new element before aIndex.
    // If no index is given, add at end of list.
    //
    virtual void insert(UData aValue, int aIndex = -1) {}

    //
    // Erase the element at the given index.
    //
    virtual void erase(int index) {}

    //! Put raw (overwrite with not retain/release)
    virtual void putw(int index,UData value) {  }

    virtual CEvent* eventAt(int index) { return NULL; }
    virtual CEvent* eventAt(string aFieldName) { return NULL; }
    
    virtual int eventPut(CExecutor* aExec, int index, CEvent* aEvt) { return -1; }
    virtual int eventPut(CExecutor* aExec, string aFieldName, CEvent* aEvt) { return -1;}

    string debugName(void);
	
private:
};


//////////////////////////////////////////////////////////////////////////////
//
// CField       -- Members of object type
// CBaseType    -- Structure object type
//
//////////////////////////////////////////////////////////////////////////////

class CField : public CRuntime {
public:
    CField(int aStorage, string aName, CType* aType, 
	   const char* aDocumentation=NULL, 
	   UData aConst=nil, 
	   int* aIndexPtr=NULL);
    CField(const CField &);
    ~CField(void);

    int mark(Mark_t aMark);

    string name()                 { return mName; }
    char*  cname()                { return (char*) mName.c_str(); };

    CType* type(void)             { return mType; }
    CType* setType(CType* aType);

    int    storage(void) { return mStorage; }
    int    setStorage(int aStorageClass);

    UData  constant(void)            { return mConst; }

    void   setConstant(UData value)  { 
	if (mType && value.o) {
	    mType->retainObject(value);
	    if (mConst.o) mType->releaseObject(mConst);
	}
	mConst = value;
    }

    // Tony: These blocks are called comments. They increase the
    //       readability of the code. Hint hint. Nudge nudge.
    //
    // Love: There is a commuication method called email that can
    //       be used for writing lengthy comments about style and 
    //       good coding ethics. There is also a audio communication 
    //       device called phone that can be used to send and
    //       receive voice samples. The voice sample can for example
    //       be your voice emitting sounds like screams etc ...
    //       (I will try to improve my comments, like adding them ;-)
    //       
    // Tony: I shall NOT NOT battle you to the end of the world regarding this
    //       irrelevant subject.
    //
    // Love: Thanx!
    //
    // The index methods gets and sets the index that this fields uses
    // into CBaseType::mFields and CBaseObject::mElements. These two
    // lists share the same index.
    //
    int     index(void)            { return mIndex; };

    void    setIndex(int aNewIndex)   {
	if (mIndexPtr) *mIndexPtr = aNewIndex;
	mIndex = aNewIndex;
    }

    const char*   documentation(void)  { return mDocumentation; }

    int     stackPos(void)       { return mStackPos; }
    int     stackPos(int aPos)   { return mStackPos = aPos; }
private:
    int           mStorage;      // CONST/PUBLIC/PRIVATE/EXTERN
    string        mName;         // name of field
    CType*        mType;         // type of field
    UData         mConst;        // constant value for field (Q_CONST)

    int     mIndex;               // Index into CBaseObject::mElements
    int*    mIndexPtr;            // Pointer to type singleton for field update
    const char*   mDocumentation; // Documentation - description of field
    int     mStackPos;            // Stack position for local variables
    // FIXME - we need a patch list for interface fields.    
};

#define m1RetainBaseType(ptr)      m1Retain(CBaseType,ptr)
#define m1ReleaseBaseType(ptr)     m1Release(CBaseType,ptr)
#define m1SetRetainBaseType(loc,ptr)  m1SetRetain(CBaseType,loc,ptr)

class CBaseType : public CType {
public:
    CBaseType(string aName);
    CBaseType(void);
    ~CBaseType(void);

    int mark(Mark_t aMark);

    void init(CExecutor* aExec,CBaseObject* aObj);
    void defaults(CExecutor* aExec,CBaseObject* aObj);
    void construct(CExecutor* aExec,CBaseObject* aObj);
    void destruct(CExecutor* aExec,CBaseObject* aObj);

    CType*  typeAt(int aIndex)    { return mFields[aIndex]->type(); }
    CType*  typeAt(string aName)  { return fieldType(aName); }
    int     indexAt(string aName);

    // Note do only used field() result temporaily since the
    // array of field may grow and will be reallocate the address
    // may become invalid.
    CField* field(int aFieldIndex)     { return mFields[aFieldIndex]; }
    CType*  fieldType(int aFieldIndex) { return mFields[aFieldIndex]->type(); }

    CField* field(string aFieldName); 
    CType*  fieldType(string aFieldName);

    char *fieldName(int aFieldIndex) { return mFields[aFieldIndex]->cname(); }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    // release/retain objects of type CBaseType
    UData  retainObject(UData a)  { return URuntime(m1Retain(CRuntime,a.o));  }
    UData  releaseObject(UData a) { return URuntime(m1Release(CRuntime,a.o)); }
    int    markObject(UData a, Mark_t aMark) { return m1Mark(a.o, aMark); }

    // subtypes
    CBaseType* subTypes(void) { return mSubTypes; }

    // add aType to subtypes
    void addSubType(int aStorage, string aName, CType* aType);
    void addSubType(CType* aType);

    // update a Type name with a new type description
    void updateSubType(int aStorage, string aName, CType* aType);
    void updateSubType(CType* aType);

    void   shallowCopy(CExecutor* aExec, UData src, UData* dst);
    void   deepCopy(CExecutor* aExec, UData src, UData* dst);

    int compare(UData a, UData b);

    const size_t sizeOf(void) { return sizeof(UData); }
    const M1TypeTag typeTag(void) { return M1TYPE_OBJECT; }

    // Put an object (value.o) of type CBaseType into a vector
    void elementPut(CExecutor* aExec, UDataVector *vec, int index, 
		    UData value, Trig_t trig);

    void elementCopy(CExecutor* aExec, UDataVector *vec, int index, 
		     UData value, Trig_t trig);

    void setParent(CType* aParent) { 
	if (aParent) aParent->setChild(this);
	m1SetRetainType(&mParent,aParent);
    }

    CType* parentType() { return mParent; }

    string    parentName() { 
	return (mParent == NULL) ? "" : mParent->name(); 
    }

    void copyFields(CBaseType* aTarget, CType* (aTranslateType)(CType* aType) = NULL);

    u_int8_t* loadFields(u_int8_t* ptr,u_int8_t* ptr_end);

    void setupFields(CField *aFields, size_t aFieldCount);

    CField* addField(CField* aField);

    CField* updateField(CField* aField);

    CField* addField(int aStorage, string aName, CType* aType) {
	CField field = CField(aStorage, aName, aType);
	return addField(&field);
    }

    CField* updateField(int aStorage, string aName, CType* aType) {
	CField field = CField(aStorage, aName, aType);
	return updateField(&field);
    }

    // Return the number of fields for self.
    size_t fieldCount(void)      { return mFields.size(); };

    // Return the start index
    int fieldTypeOffset(void) { return mFieldOffset; }
    void setFieldTypeOffset(int aFieldOffset) { mFieldOffset = aFieldOffset; }

    // Numer of fields (fieldOffset based) for the type
    size_t fieldTypeCount(void)  { return mFieldCount; }
    void setFieldTypeCount(size_t aFieldCount) { mFieldCount = aFieldCount; }

    CBaseType* globalType(void) { return mGlobalType; }
    void setGlobalType(CBaseType* aType) {
	m1SetRetain(CBaseType, &mGlobalType, aType);
    }

    int typeLevel() { return mTypeLevel; }
    void setTypeLevel(int aTypeLevel) { mTypeLevel = aTypeLevel; }

    CBaseObject* singleton(void) { return mSingleton; }
    void setSingleton(CBaseObject* aObject);

    CBaseObject* global(void) { return mGlobalType->singleton(); }

    void print(ostream* os);
    void print(ostream* os, UData a);
private:
    //
    // Exported fields. 
    // This vector will be populated left to right with field descriptions. 
    // In the case of DERIVED_[VIRTUAL_]OBJECT_TYPE,/ the parent class' 
    // fields will be inserted in the beginning, followed
    // by the fields of the subclasses, in class-descending order.
    // The fields of a sub class will have the same slot id (0-x)
    // as the base class. A base class may specify field "x" to be located
    // in slot 0, while a sub-class may have field "children" with the same
    // slot. 
    //
    // By traversing mFields right-to-left when looking for a field
    // with a given slot, the lowest class-level with the defined slot 
    // will be found first.
    //
    vector<CField*> mFields;
    int    mFieldOffset;     // Start index of local fields
    size_t mFieldCount;      // Number of local fields
    CType* mParent;          // inherited parent
    CBaseType* mSubTypes;    // context childs
    CBaseType* mGlobalType;  // The global context (library/filescope)
    int mTypeLevel;          // Type level in nested type declarations
    CBaseObject* mSingleton; // Singleton (library/filescope) objects
};

//////////////////////////////////////////////////////////////////////////////
//
// CBaseObject  -- Structured object
//
//////////////////////////////////////////////////////////////////////////////

#define m1RetainBaseObject(ptr)      m1Retain(CBaseObject,ptr)
#define m1ReleaseBaseObject(ptr)     m1Release(CBaseObject,ptr)
#define m1SetRetainBaseObect(loc,ptr)  m1SetRetain(CBaseObject,loc,ptr)

class CBaseObject : public CObject {
public:
    CBaseObject(CExecutor* aExec, CBaseType* aType, size_t elemSize, size_t n);
    ~CBaseObject(void);

    // Mark object and all accessible fields
    int mark(Mark_t aMark);

    CType* type()                { return mType; }
    CType* typeAt(int index)     { return mType->typeAt(index); }
    CType* typeAt(string aName)  { return mType->typeAt(aName); }
    int indexAt(string aName)    { return mType->indexAt(aName); }

    UData at(int aFieldIndex) {
	return typeAt(aFieldIndex)->elementAt(base(),aFieldIndex);
    }

    UData at(string aFieldName) {
	return at(indexAt(aFieldName));
    }

    void put(CExecutor* aExec, int aIndex, UData aValue, Trig_t aTrig=TRIGGER_AUTO) {
	typeAt(aIndex)->elementPut(aExec, base(),aIndex,aValue,aTrig);
    }

    void put(CExecutor* aExec, string aFieldName, UData aValue, Trig_t aTrig=TRIGGER_AUTO) {
	put(aExec, indexAt(aFieldName), aValue, aTrig);
    }

    void copy(CExecutor* aExec, CType* aType, int aIndex, UData aValue, Trig_t aTrig=TRIGGER_AUTO) {
	aType->elementCopy(aExec, base(),aIndex,aValue,aTrig);
    }

    void putw(int index,UData value) {
	mElements->at(index) = value;  
    }

    void putw(string aFieldName, UData aValue) {
	putw(indexAt(aFieldName), aValue);
    }

    size_t size() { return mElements->size(); }

    void erase(int aIndex) { mElements->erase(mElements->begin() + aIndex); }

    CEvent* eventAt(int index) { return mElements->at(index).evt; }

    CEvent* eventAt(string aName) { return eventAt(indexAt(aName)); }

    int eventPut(CExecutor* aExec, int index, CEvent* aEvt);

    int eventPut(CExecutor* aExec, string aFieldName, CEvent* aEvt);

    virtual UDataVector *base() { return mElements; }

    virtual void   resize(size_t newSize) { mElements->resize(newSize,nil); }

    CBaseObject* global() { return mType->globalType()->singleton(); }

private:
    // mType is the objects type representation
    CBaseType *mType;
    // Object fields are stored in mElements
    UDataVector *mElements;
};



//////////////////////////////////////////////////////////////////////////////
//
// CEvent
//
//////////////////////////////////////////////////////////////////////////////


typedef std::list<CEvent*> CEventList;

class CEvent {
public:
    CEvent(CExecutable* aOwner) {
	mValue       = nil;
	mOwner       = aOwner;
	mNext        = NULL;
	mSubscribers = NULL;
	mSource      = NULL;
	mUNext       = NULL;
	mUpdate      = UpdateStatusNo;
	mSenderFile  = NULL;
	mSenderLine  = 0;
	mIndex       = -1;
	mTracePropagation = false;
    }

    // Needed to make CEvent polymorphic and allow downcasts
    virtual ~CEvent(void) {
	deleteFromSource();  // disconnect from event source
	disconnect();        // disconnect all subscribers
    }

    // Are we updated ?
    bool updated(void) { 
	return ((mUpdate & UpdateStatusYes) != 0); 
    }
    void setUpdated(CExecutor* aExec);
    virtual void clearUpdated(CExecutor* aExec);

    // Are we assigned ?
    bool assigned(void) { 
	return ((mUpdate & UpdateStatusAssigned) != 0); 
    }

    // Are we assigned by local method
    bool localAssigned(void) { 
	return ((mUpdate & UpdateStatusLocal) != 0); 
    }

    // Are we assigned by other object
    bool remoteAssigned(void) { 
	return ((mUpdate & UpdateStatusRemote) != 0); 
    }

    // Are we assigned by propgation from subscription
    bool propagteAssigned(void) { 
	return ((mUpdate & UpdateStatusPropagate) != 0); 
    }

    // is the event marked (used for in list detection) 
    bool marked(void);
    void setMarked(void);
    void clearMarked(void);

    // is the event kept on update list for next round
    bool later(void);
    void setLater(void);
    void clearLater(void);
    
    // Cancel the update, removed from owner update list etc
    void cancel(CExecutor* aExec);

    /* manage update links */
    CEvent*  getNextUpdated(void)           { return mUNext; }
    CEvent** getNextUpdatedAddr(void)       { return &mUNext; }
    void     setNextUpdated(CEvent* aUNext) { mUNext = aUNext; }

    CExecutable* owner(void) { return mOwner; }

    CEvent* source(void) { return mSource; }

    // Count number of subsribers (debugging only)
    size_t subscriberCount(void);

    void deleteFromSource(void) {
	if (mSource) mSource->deleteSubscriber(this);
    }

    // Propagate value 1-step (to direct subscribers)
    void propagate(CExecutor* aExec);
	    
    // Disconnect all subscribers
    void disconnect(void);

    // Locate and remove a subscriber
    void deleteSubscriber(CEvent* aEvt);

    // Add a new subscriber
    void addSubscriber(CEvent* aEvt);

    void setSender(const char* aFile, int aLine) {
	mSenderFile = aFile;
	mSenderLine = aLine;
    }

    // Debug and trace
    const char* senderFile(void) { return mSenderFile; }
    int   senderLine(void) { return mSenderLine; }
    void  setIndex(int aIndex) { mIndex = aIndex; }

    string debugName(void);
    void   printValue(ostream* os);

    virtual UData assign(CExecutor* aExec, UData aNewValue, 
			 bool aSetSender, Trig_t aTrig);

    // Get raw value
    UData uvalue(void) { return mValue; }
    
    virtual void setValue(UData aValue) { mValue = aValue; }
    virtual void retainValue(UData aValue) {}
    virtual void releaseValue(UData aValue) {}

    // mark event objects and event sources (for garbage collection)
    virtual int mark(Mark_t aMark);


    // Do we want debug output of how propagation is done?

    void tracePropagationOn(void) { mTracePropagation = true; }
    void tracePropagationOff(void) { mTracePropagation = false; }
    bool tracePropagation(void) { return mTracePropagation; }


protected:
    void setSource(CEvent* aSource);

    UData         mValue;        // The generic value
    CExecutable*  mOwner;        // Event owner
    CEvent*       mNext;         // Next in subsriber list
    CEvent*       mSubscribers;  // First in subscriber list
    CEvent*       mSource;       // The events source or NULL
    CEvent*       mUNext;        // Next in mOwner's updated list
private:
    CEvent& operator= (const CEvent& rhs) {
	m1BreakHere(__FILE__, __LINE__, (char*) "over write of event variable ignored");
	return *this;
    }

    UpdateStatus  mUpdate;       // Update flags (None/Now/Later)
    // Event tracing
    const char*         mSenderFile;   // Event sender file name
    int                 mSenderLine;   // Event sender file name
    int                 mIndex;        // Index in owner object
    bool                mTracePropagation; // Flag for debug tracing of propagation
};

///////////////////////////////////////////////////////////////////////////////
//
// Basic Event types
// 
//    EventBool
//    EventChar
//    EventByte
//    EventSigned
//    EventUnsigned
//    EventFloat
//    EventTime
///////////////////////////////////////////////////////////////////////////////

#define BASIC_EVENT_TYPE(ID, selector, T)				\
    class Event ## ID : public CEvent {					\
    public:								\
	    Event ## ID (CExecutable* aOwner) : CEvent(aOwner) {} \
	    T putValue(CExecutor* aExec, const T &aValue, Trig_t trig = TRIGGER_AUTO) { \
		UData v; v.selector = aValue;				\
		assign(aExec,v,false,trig);				\
		return aValue;						\
	    }								\
	    T  value(void)      { return (T) mValue.selector; }		\
    }


BASIC_EVENT_TYPE(Float,    f, float);    
BASIC_EVENT_TYPE(Signed,   i, int);    
BASIC_EVENT_TYPE(Unsigned, u, unsigned int);
BASIC_EVENT_TYPE(Char,     c, char);
BASIC_EVENT_TYPE(Byte,     b, unsigned char);
BASIC_EVENT_TYPE(Bool,     t, bool);
BASIC_EVENT_TYPE(Time,     tm, Time);




//////////////////////////////////////////////////////////////////////////////
//
// CExecutable
//
//////////////////////////////////////////////////////////////////////////////

class CExecutable : public CBaseObject {
public: 
    CExecutable(CExecutor* aExec, CBaseType *aType);
    CExecutable(CExecutor* aExec, CBaseType *aType, size_t elemSize, size_t n);
    ~CExecutable(void);

    CycleCount cycle()                      { return mCycle; }
    void       setCycle(CycleCount aCycle)  { mCycle = aCycle; }

    void setFlags(unsigned int aFlags) { mFlags |= aFlags; }
    void clrFlags(unsigned int aFlags) { mFlags &= ~aFlags; }
    bool tstFlag(unsigned int aFlag)   { return (mFlags & aFlag) != 0; }

    bool isScheduled(void) {   return tstFlag(ExecutableInQueue); }
    void setScheduled()      { setFlags(ExecutableInQueue); }
    void clrScheduled()      { clrFlags(ExecutableInQueue); }

    bool isScheduledLater(void) { return tstFlag(ExecutableInNextQueue); }
    void setScheduledLater()    { setFlags(ExecutableInNextQueue); }
    void clrScheduledLater(void);

    bool executeOnEventUpdate()  { return tstFlag(ExecuteOnEventUpdate); }
    bool executeEverySweep()     { return tstFlag(ExecuteEverySweep); }
    bool executeNever() { 
	return !( tstFlag(ExecuteOnEventUpdate) ||
		  tstFlag(ExecuteEverySweep));
    }


    CExecutable*  qLink(void) { return mQLink; }
    CExecutable** qPtr(void) { return &mQLink; }
    void          setQLink(CExecutable* aQLink) { mQLink=aQLink; }

    bool disconnect(int evtPos);
    bool disconnect(string evtField);
    bool connect(int evtPos,CObject* src,int srcPos);
    bool connect(string evtField, CObject* src, string srcField);
    void disconnectOutput(CExecutor* aExec);

    bool updatedEventEmpty(void) { return mUpdatedEvents==NULL; }
    CEvent* updatedEvents(void)  { return mUpdatedEvents; }

    // Scan trough all updated events and propagate values
    void propagateAllUpdated(CExecutor* aExec);

    // Add the event to the list of updated events
    void addUpdatedEvent(CExecutor* aExec, CEvent *aUpdated);

    // Remove the event from the list of updated events
    void delUpdatedEvent(CExecutor* aExec, CEvent* aUpdated);

    // Remove updated events from update list,
    // but keep events that are (also) scheduled for next round
    void clearAllUpdated(CExecutor* aExec, CEvent* aUntil=NULL);

    // Set all events on the updated list to Updated
    void setAllUpdated(CExecutor* aExec);    

    void useStructureChanged(void);
    void setStructureChanged(CExecutor* aExec);
    void clearStructureChanged(CExecutor* aExec);
    CEvent* structureChangedEvent(void) { return mStructureChanged; }

    //
    // execute is implemented in each class but it's the
    // MyObjectType::execute that is responsible for executing
    // the correct one....

    void execute(CExecutor* aExec);

    // 
    // execute is run from derived class to parent and so 
    // to handle anything that the parent has setup during execute
    // then post_excute is the right method since it's called
    // in the oppsite direction than execute.
    //

    void post_execute(CExecutor* aExec) {}

    //
    //  start is called last in produce for
    //  VmMachineType and VmEvalType to allow the c++
    //  objects to setup and calculate initial values
    //
    virtual void start(CExecutor* aExec) {}

    //
    //  stop is called before object is destroyed
    //  VmMachineType and VmEvalType to allow 
    //  to run type destructors 
    //  NOTE that parent stop must be called manually since it's
    //  the CRuntime stop method that actually remove the object
    //
    void stop(CRtExecutor* aExec);


protected:
    // ExecutableInQueue iff object is in mQueue
    // ExecutableInNextQueue iff object is in mNextQueue
    // ExecuteOnEventsUpdate iff object is to be scheduled on event update
    // ExecuteEverySweep iff object is to be scheduled every time
    unsigned int  mFlags;
    // General disconnect event that will disconnect all outputs
    // For the object
    EventBool mDisconnect;
private:
    // Last execution time
    CycleCount mCycle;
    // List of updated events, linked trough Events ULink field
    CEvent* mUpdatedEvents;
    // A pointer to a "boolean" event field when is defined (!= NULL)
    // this allows code to subscribe on field update on any member
    CEvent* mStructureChanged;
    // Pointer to next element in Schedule Queue ( mQueue or mNextQueue)
    CExecutable* mQLink;
};

//////////////////////////////////////////////////////////////////////////////
//
// CSystem & CExecutor 
// a split of old CSweeper functionality 
// CSystem is the component queue manager and also polls io
// CExecutor execute the components found on the queue,
//
//
// Each sweep, the components are executed in the following order:
// 1) mEverySweepComponents
// 2) mDescriptorArray components (where applicable)
// 3) mPendingComponents.
//
// For each step above, all components triggered by updated
// symbols are executed as well in a recursive fashion, 
// with the exception that no component can be executed more than 
// once during a sweep. (See mPendingComponents description below)
//
//
//////////////////////////////////////////////////////////////////////////////

class CExecutableQueue {
public:
    CExecutableQueue() {
	mHead = NULL;
	mTailPtr = &mHead;
    }
    ~CExecutableQueue() { }

    void reset(void) {
	mHead = NULL;
	mTailPtr = &mHead;
    }

    bool empty(void) { return (mTailPtr == &mHead); }

    CExecutable* front() { return mHead; }
    CExecutable* rear()  { return *mTailPtr; }

    void enqueue(CExecutable* aComponent) {
	*mTailPtr = aComponent;
	mTailPtr  = aComponent->qPtr();
	aComponent->retainThis();
    }

    CExecutable* dequeue(void) {
	CExecutable* elem = mHead;
	if (elem != NULL) {
	    if ((mHead = elem->qLink()) == NULL)
		mTailPtr = &mHead;
	    elem->setQLink(NULL);
	    elem->releaseThis();
	}
	return elem;
    }

    // Enqueue all element in aQ into this
    void merge(CExecutableQueue* aQ)  {
	if (!aQ->empty()) {
	    *mTailPtr = aQ->mHead;
	    mTailPtr  = aQ->mTailPtr;
	    // clear aQ
	    aQ->mHead = NULL;
	    aQ->mTailPtr = &aQ->mHead;
	}
    }


    bool remove(CExecutable* aComponent) {
	CExecutable** pptr = &mHead;
	while(*pptr && (*pptr != aComponent))
	    pptr = (*pptr)->qPtr();
	if (*pptr) {
	    if (aComponent->qPtr() == mTailPtr)
		mTailPtr = pptr;
	    *pptr = aComponent->qLink();
	    aComponent->releaseThis();
	    aComponent->setQLink(NULL);
	    return true;
	}
	return false;
    }

    size_t size() {
	/* only for debugging */
	CExecutable* ptr = mHead;
	size_t n = 0;
	while(ptr) {
	    n++;
	    ptr = ptr->qLink();
	}
	return n;
    }

private:
    CExecutable*  mHead;
    CExecutable** mTailPtr;
};
//
// CSystem is a global container for executors to execute
// the code in.
// (We should try to put EVERY THING global in this environment)
//

class CSystem {
public:
    CSystem(void);

    //
    // Do one sweep.
    //
    void sweep(void);

    //
    // Mark all accessible objects and return number of objects marked
    // 
    int mark(Mark_t aMark);

    //! Returns hires timestamp  (usec, but should be nanosec)
    TimeStamp timeStamp(void);

    // Adjust time 
    void adjTime(time_t aOldTime, time_t aNewTime);

    TimeStamp timeStart(void)     { return mSystemStart; }

    CycleCount cycle(void)     { return mCurrentCycle; }

    // Start time for this cycle
    TimeStamp     cycleTime(void) { return mCycleTime; }

    // The (thread specific) executor
    CExecutor*  executor(void) { return mExecutor; }

    //
    // Schedule a component for execution. 
    // Return the status UpdateStatusNow when schedule in this cycle
    // or UpdateStatusLater when scheduled in next cycle
    // LOCK
    //
    UpdateStatus schedule(CExecutable *aObject);

    // 
    // add Component will check mFlags and may
    // schedule component if needed
    // LOCK
    void addComponent(CExecutable *aComponent);
    
    // Remove component form queues and every sweep.
    // LOCK
    void removeComponent(CExecutable* aComponent);

    //
    // Setup a file descriptor in the poll map.
    //
    void addPollDescriptor(int aDescriptor, short aPollEvents, CFileSource *aComponent);

    // Remove a file descriptor from the poll map.
    bool deletePollDescriptor(int aDescriptor);

private:

    ~CSystem(void) {
	delete[] mPollVector;
    }
	
    // Do an file descriptor poll and execute all triggered components.
    void executePolledComponents(TimeStamp aTimeStamp);

    CExecutor*    mExecutor;      // This will be thread in future!!!
    TimeStamp     mSystemStart;   // System start time
    CycleCount    mCurrentCycle;  // Cycle number 
    TimeStamp     mCycleTime;     // Start time for this cycle

    TimeStamp     mStatStart;     // When we started sampling
    int           mCycleCount;    // Number of sweeps done since sample start.
    int           mTotalCycleTime; // Total time spent in sweeps since start of stats.
    
    // A list of components to be executed at every sweep.
    list<CExecutable*> mEverySweepComponents;

    // All CExecutable's are put on either mQueue or mQueueNext
    // if CExecutable->cycle is the same as mCurrentCycle then
    // its put on the mQueueNext otherwise it is put on the
    // mQueue
    CExecutableQueue mQueue;

    CExecutableQueue mNextQueue;

    //
    // Poll vector. Resized as necessary.
    // Contains all descriptors to be polled. Can be fed directly to poll(2)
    //
    struct pollfd *mPollVector;
    int mPollVectorSize; // Actual size of vector.
    int mPollVectorAllocSize; // Allocated size of vector.

    //
    // Poll descriptor->CFileSource map
    // Indexed by the file descriptor.
    // 
    // mFileSourceDescriptorMap[fd] will point to the CExecutable 
    // that is subscribing to events from that descriptor.
    //
    vector<CFileSource *> mFileSourceDescriptorMap;
};

// A handle reference to the default system
extern CSystem&       m1_system(void);


// Mark all accessible objects
extern int m1_mark(Mark_t aMark);
//
// Instance of CExecutor are able to execute CExecutables
// They are picked up from the CSystem queue. CExecutor will
// typically be run in an os thread.
//

typedef struct {
    CBaseObject* mActivation[ACTIVATION_LEVELS];
} ExecutorState;

//
// Stack is handled as an CObject to simplify use in runtime engine
// FIXME add a special type for CStack ?
//  
class CStack : public CObject {
public:
    CStack();
    ~CStack();

    // Reset the stack to inital state
    void reset(void);

    // Allocate stack frame FramePtr[-1] is the framlink
    void allocFrame(unsigned int n) {
	UData* oldFramePtr = mFramePtr;
	mFramePtr = mValueStackPtr+1;
	mFramePtr[-1].ud = oldFramePtr;
	mValueStackPtr = mFramePtr+n;
    }

    void deallocFrame() {
	mValueStackPtr = mFramePtr-1;  // old stack location
	mFramePtr = mFramePtr[-1].ud;  // unwind frame pointer
    }

    UData* valueStackPtr() { return mValueStackPtr; }
    UData* valueStackEnd() { return mValueStackEnd; }
    UData* valueStackBegin() { return mValueStackBegin; }
    void   setValueStackPtr(UData* ptr) { mValueStackPtr = ptr; }

    UData* framePtr()       { return mFramePtr; }

    UData* returnStackPtr() { return mReturnStackPtr; }
    UData* returnStackEnd() { return mReturnStackEnd; }
    UData* returnStackBegin() { return mReturnStackBegin; }
    void   setReturnStackPtr(UData* ptr) { mReturnStackPtr = ptr; }

    // CObject interface
    CType* type()               { return void_type(); }
    CType* typeAt(int index)    { return void_type(); }
    CType* typeAt(string aName) { return void_type(); }
    int    indexAt(string aName) { return -1; }

    // at/put work on stack frames
    UData at(int index) { return mFramePtr[index]; }
    void  put(CExecutor* aExec, int index, UData value, Trig_t trig) {
	mFramePtr[index] = value;
    }
    void  copy(CExecutor* aExec, CType* aType, int index, UData value, Trig_t trig) {
	aType->shallowCopy(aExec, value, &mFramePtr[index]);
    }

    size_t size(void) { return mValueStackEnd - mValueStackBegin; }
    
private:
    // Value stack
    UData* mValueStackEnd;
    UData* mValueStackBegin;
    UData* mValueStackPtr;

    // Frame 
    UData* mFramePtr;

    // Return stack
    UData* mReturnStackEnd;
    UData* mReturnStackBegin;
    UData* mReturnStackPtr;
};


class CExecutor : public CRtExecutor {
public:
    CExecutor(CSystem* mSystem);

    ~CExecutor(void);

    int mark(Mark_t aMark);
    
    // Return the currently executing CExcutable
    CExecutable* current(void) { return mCurrent; }

    CBaseObject* global(void) { return mState.mActivation[SCOPE_GLOBAL]; }

    TimeStamp timeStamp(void)  { return mSystem->timeStamp(); }

    CycleCount cycle(void)     { return mSystem->cycle(); }

    // Start time for this cycle
    TimeStamp     cycleTime(void) { return mSystem->cycleTime(); }

    void addComponent(CExecutable *aComponent) { 
	mSystem->addComponent(aComponent);
    }
    
    void removeComponent(CExecutable* aComponent) {
	mSystem->removeComponent(aComponent);
    }

    void addPollDescriptor(int aDescriptor, short aPollEvents, CFileSource *aComponent) {
	mSystem->addPollDescriptor(aDescriptor, aPollEvents, aComponent);
    }

    // Remove a file descriptor from the poll map.
    bool deletePollDescriptor(int aDescriptor) {
	return mSystem->deletePollDescriptor(aDescriptor);
    }

    UpdateStatus schedule(CExecutable *aObject) {
	return mSystem->schedule(aObject);
    }

    // Reset execution state and stack
    void reset(void);

    // Execute a component
    void execute(CExecutable* aExec);

    // Save execution state (executable and context)
    void save(ExecutorState* state);

    // Restore execution state (executable and context)
    void restore(ExecutorState* state);

    // Activate global context ( file context and library context )
    void activate_global(CBaseObject* aGlobal, CBaseObject** aSave);

    // Restore global context 
    void restore_global(CBaseObject* aGlobal);

    // Activate execution state
    void activate(CBaseObject* aObject);


    // Deactivate execution state (mearly fo debugging! could/should be removed)
    void deactivate(CBaseObject* aObject);

    // Allocate stack frame
    void allocFrame(unsigned int n) { mStack->allocFrame(n); }

    void deallocFrame() { mStack->deallocFrame(); }

    UData* valueStackPtr() { return mStack->valueStackPtr(); }
    UData* valueStackEnd() { return mStack->valueStackEnd(); }
    UData* valueStackBegin() { return mStack->valueStackBegin(); }
    void   setValueStackPtr(UData* ptr) { mStack->setValueStackPtr(ptr); }

    UData* framePtr()       { return mStack->framePtr(); }

    UData* returnStackPtr() { return mStack->returnStackPtr(); }
    UData* returnStackEnd() { return mStack->returnStackEnd(); }
    UData* returnStackBegin() { return mStack->returnStackBegin(); }
    void   setReturnStackPtr(UData* ptr) { mStack->setReturnStackPtr(ptr); }

    // Get objects from the active object set
    CBaseObject* scope(ActivationType_t aLevel) { 
	return mState.mActivation[aLevel]; 
    }

    void setEventSource(const char* aFile, int aLine) {
	mEventSourceFile = aFile;
	mEventSourceLine = aLine;
    }

    const char* eventSourceFile() { return mEventSourceFile; }

    int   eventSourceLine() { return mEventSourceLine; }

private:
    CSystem* mSystem;          // The system object
    CStack*  mStack;           // Executor stack
    CExecutable* mCurrent;     // Current object
    ExecutorState mState;      // Execution state (scopes)

    // Event debugging
    const char* mEventSourceFile;
    int         mEventSourceLine;
};



//////////////////////////////////////////////////////////////////////////////
//
// CEventQueue
//     declared in m1 as: event queue <type>
//     can be used when one event source is updated multiple times
//     an everything counts. Note that the queued values are sequenced
//     out one per cycle.
//////////////////////////////////////////////////////////////////////////////

enum EventQueuePolicy {
    EventQueuePolicyStore,
    EventQueuePolicyDropFront,
    EventQueuePolicyDropRear,
    EventQueuePolicyBlock    // Hmm, future extension
};


class CEventQueue : public CEvent {
public:
    CEventQueue(CExecutable* aOwner) : CEvent(aOwner) {
	mMaxSize = 0;
	mMaxTime = 0.0;
	mPolicy  = EventQueuePolicyStore;
    }

    CEventQueue(const CEventQueue& aEvent) : CEvent(aEvent) {
	// CHECK ME: I do not queue to be copied only the current value 
	// should be handled
	mMaxSize = aEvent.mMaxSize;
	mMaxTime = aEvent.mMaxTime;
	mPolicy  = aEvent.mPolicy;
    }

    UData assign(CExecutor* aExec, UData aValue, 
		 bool aSetSender, Trig_t aTrig);

    void clearUpdated(CExecutor* aExec);

private:
    typedef struct {
	TimeStamp mTime;          // Time when event entered the queue
	UData     mValue;
	const char*     mSenderFile;   // Event sender file name
	int       mSenderLine;   // Event sender file name
    } QItem;
	
    size_t mMaxSize;          // Maximum size (0 means infinite)
    float  mMaxTime;          // Maximum time frame to buffer
    EventQueuePolicy mPolicy; // What to do when queue is full
    queue<QItem> mQueue;
};


// Represent input/output events (one for each type)
class CEventType : public CType {
public:
    CEventType(CType* aType, bool aQueue, int aDirection=E_INOUT);
    CEventType(string aName, bool aQueue, int aDirection=E_INOUT);
    ~CEventType() {
	m1ReleaseType(mBaseType);
    }

    static CType* create(CType* aType, int aDirection=E_INOUT);
    static CType* createQueue(CType* aType, int aDirection=E_INOUT);

    static CType* createType(CType* aType, bool aQueue, int aDirection);

    UData produce(CExecutor* aExec,CBaseType*, CArgs*) { return produceEvent(aExec, mQueued); }

    UData produceEvent(CExecutor* aExec,bool aQueued) { return mBaseType->produceEvent(aExec,aQueued); }

    bool isQueued(void) { return mQueued; }

    const size_t sizeOf(void) { return sizeof(UData); }
    const M1TypeTag typeTag(void) { return M1TYPE_EVENT; }

    string typeSignature(void);

    string typeName(void);

    void elementInit(CExecutor* aExec,UDataVector *vec, int index);

    UData elementAt(UDataVector *vec, int index);

    void elementPut(CExecutor* aExec, UDataVector *vec,int index,
		    UData value,Trig_t trig);

    void elementCopy(CExecutor* aExec, UDataVector *vec,int index,
		     UData value,Trig_t trig);

    void shallowCopy(CExecutor* aExec, UData src, UData* dst) {
	dst->evt->setValue(src.evt->uvalue());
    }

    void deepCopy(CExecutor* aExec, UData src, UData* dst) {
	dst->evt->setValue(src.evt->uvalue());
    }

    CType* baseType(void) { return mBaseType; }
    int    direction(void) { return mDirection; }
    bool   isInput(void)  { return (mDirection & E_INPUT) != 0; }
    bool   isOutput(void)  { return (mDirection & E_OUTPUT) != 0; }

    void print(ostream* os);
    void print(ostream* os, UData a);
private:
    bool       mQueued;      // queue E_INPUT | E_INOUT
    int        mDirection;  // E_INPUT | E_OUTPUT
    CType*     mBaseType;
};


//////////////////////////////////////////////////////////////////////////////
//
// CExecutableType
//
//////////////////////////////////////////////////////////////////////////////

class CExecutableType: public CBaseType {
public:
    CExecutableType(string aName) : CBaseType(aName) { }
    CExecutableType() : CBaseType() { }
    ~CExecutableType(void) {}

    virtual void execute(CExecutor* aExec,CExecutable *aObject);

    bool   isExecutableType()   { return true; }

    void destruct(CExecutor* aExec, CBaseObject* aObject);

private:
};

//////////////////////////////////////////////////////////////////////////////
//
// CBindingType
//   Used for global / local bindings object
//
//////////////////////////////////////////////////////////////////////////////
class CBindingType : public CExecutableType {
public:
    CBindingType() : CExecutableType() { }
    CBindingType(string aName) : CExecutableType(aName) { }
    ~CBindingType(void) {}

    UData produce(CExecutor* aExec,CBaseType* aBase=NULL, CArgs* aArgs=NULL);
    void init(CExecutor* aExec,CBaseObject* obj);
    void defaults(CExecutor*,CBaseObject* obj);
    void construct(CExecutor* aExec,CBaseObject* obj);
    void destruct(CExecutor* aExec,CBaseObject* obj);
private:
};

class CCompoundType : public CBindingType {
public:
    CCompoundType() : CBindingType(), mFrameSize(0) {}
    ~CCompoundType(void) {}

    bool isCompound(void) { return true; }

    unsigned int frameSize(void) { return mFrameSize; }
    unsigned int frameSize(unsigned int aSize) { return mFrameSize=aSize; }

private:
    unsigned int mFrameSize; // when used for Compund/Switch (stack usage)
};
    
    


//////////////////////////////////////////////////////////////////////////////
//
// CBindingObject  -- Global and local environments object
//
//   This is a dynamic object that will expand with new fields
//   by calling addField
//////////////////////////////////////////////////////////////////////////////

class CBindingObject : public CExecutable {
public:
    CBindingObject(CExecutor* aExec, CBindingType* aType, size_t n) :
	CExecutable(aExec, aType, sizeof(UData), n) {
	type()->init(aExec, this);
    }

    ~CBindingObject() {
    }

    void stop(CRtExecutor* aExec);

    // Add a binding type and intialize to 0/nil, 
    // it's not allowed to define same binding name multiple times
    // it's like multiple defined symbols...
    void addField(CField* aField) {
	string name = aField->name();
	if (typeAt(name) != NULL)
	    fprintf(stderr, "error: binding %s multiply defined\n",
		    aField->cname());
	else {
	    CBaseType* t = (CBaseType*) type();
	    size_t n = t->fieldCount()+1;
	    t->addField(aField);
	    // Resize object to match the updated type
	    if (n > size())
		resize(n);
	    // FIXME: for global symbols we should probably
	    //        put a symbol here.
	    //        for array we should allocate the array etc...
	}
    }

    // Update or Add a binding type and
    void updateField(CField* aField) {
	CBaseType* t = (CBaseType*) type();
	string name = aField->name();

	if (typeAt(name) != NULL) {
	    t->updateField(aField);
	    // maybe clear value?
	}
	else {
	    size_t n = t->fieldCount()+1;
	    t->addField(aField);
	    // Resize object to match the updated type
	    if (n > size())
		resize(n);
	    // FIXME: for global symbols we should probably
	    //        put a symbol here.
	    //        for array we should allocate the array etc...
	}
    }
	
    void addField(int aStorage, string aFieldName, CType* aType) {
	CField field = CField(aStorage, aFieldName, aType);
	addField(&field);
    }

    void updateField(int aStorage, string aFieldName, CType* aType) {
	CField field = CField(aStorage, aFieldName, aType);
	updateField(&field);
    }

private:
};


// A handle to the global type/library scope
extern CBindingObject&  m1_context(void);


extern void m1TypeRegister(int aStorage, string aName, CType* aType);
extern void m1TypeRegister(string aName, CType* aType);
extern void m1TypeRegister(CType* aType);
extern void m1TypeUnRegister(CType* aType);
extern CType* m1TypeLookup(string aName);
extern CType* m1TypeFmtLookup(char* fmt, ...);


class CString : public CObject {
public:
    CString() {}
    CString(char*  aString) : mString(aString) { }
    CString(char* aString,size_t n) : mString(aString, 0, n) {}
    CString(string aString) : mString(aString) { }

    ~CString() { };

    string debugName(void);

    CType* type() { return string_type(); }
    CType* typeAt(int index) { return char_type(); }
    CType* typeAt(string aName) { return char_type(); }
    int    indexAt(string aName) { return -1; }

    UData at(int aIndex) { 
	return UChar(mString[aIndex]); 
    }

    void put(CExecutor* aExec, int aIndex,UData aValue,Trig_t trig) {
	mString[aIndex] = aValue.c;
    }

    void copy(CExecutor* aExec,CType* aType,int aIndex,UData aValue,Trig_t trig) {
	mString[aIndex] = aValue.c;
    }

    void putw(int aIndex,UData aValue) { 
	mString[aIndex] = aValue.c;  
    }

    void insert(UData aValue, int aIndex = -1) {
	if (aIndex == -1) 
	    mString.push_back(aValue.c);
	else
	    mString.insert(mString.begin()+aIndex, 1, aValue.c); 
    }

    void erase(int index) { mString.erase(mString.begin() + index); }
    void erase(UData aString) {
	string str = aString.str->str();
	for (int i=0; i < (int)str.size(); i++)
	    elementErase(UChar(str[i]));
    }
    
    void elementErase(UData aElement) {
	string::size_type loc = mString.find(aElement.c, 0);
	if (loc != string::npos)
	    mString.erase(mString.begin() + loc);
    }

    size_t size() { return   mString.length(); }
    size_t length() { return mString.length()+1; }
    void   resize(size_t newSize) { mString.resize(newSize,0); }

    int compare(CString* aStr) { 
	return mString.compare(aStr->mString); 
    }
    int compare(char* bp, size_t bn) {
	return mString.compare(0, bn, bp); 
    }
    int compare(string aStr) {
	return mString.compare(aStr);
    }

    void   set(string aStr) { mString = aStr; }
    void   stradd(string aStr) { mString += aStr; }
    string strcat(string aStr) { return mString + aStr; }
    string substr(string::size_type index, 
		  string::size_type num = string::npos) {
	return mString.substr(index, num);
    }
    
	
    const char* c_str(void)    { return mString.c_str(); }
    string      str(void)      { return mString; }
    string&     str_ref(void)  { return mString; }
private:
    string mString;
};

#define m1RetainString(ptr)      m1Retain(CString,ptr)
#define m1ReleaseString(ptr)     m1Release(CString,ptr)
#define m1SetRetainString(loc,ptr)  m1SetRetain(CString,loc,ptr)

//////////////////////////////////////////////////////////////////////////////
//
// CStringType  M1 wrapper for string type
//
//
//////////////////////////////////////////////////////////////////////////////
class CStringType : public CType {
public:
    CStringType(void);
    ~CStringType() {}

    UData produce(CExecutor* aExec,CBaseType* aBase, CArgs* aArgs);
    UData produceEvent(CExecutor* aExec,bool aQueued);

    void   shallowCopy(CExecutor* aExec, UData src, UData* dst);
    void   deepCopy(CExecutor* aExec, UData src, UData* dst);

    UData  retainObject(UData a) { return URuntime(m1Retain(CRuntime,a.o));  }
    UData  releaseObject(UData a) { return URuntime(m1Release(CRuntime,a.o)); }
    int    markObject(UData a, Mark_t aMark) { return m1Mark(a.o, aMark); }

    int  compare(UData a, UData b);
    int  compare(UData a, char* bp, size_t bn);
    int  compare(UData a, string str);

    const size_t sizeOf(void)         { return sizeof(UData); }
    const M1TypeTag typeTag(void)     { return M1TYPE_STRING; }
    string typeSignature(void)        { return "s"; }
    string typeName(void)             { return "string"; }
    // Put/Copy a CStringType object (use value.o) into a object/vector

    void elementInit(CExecutor* aExec, UDataVector *vec, int index);

    void elementPut(CExecutor* aExec, UDataVector *vec, int index, 
		    UData value, Trig_t trig);
    void elementCopy(CExecutor* aExec, UDataVector *vec, int index, 
		     UData value, Trig_t trig);
    void print(ostream* os, UData a);
};


// Represent error condition 
class CErrorType : public CType {
public:
    CErrorType(void);
    ~CErrorType(void) {}
    const M1TypeTag typeTag(void) { return M1TYPE_ERROR; }
};

// Represent none type
class CVoidType : public CType {
public:
    CVoidType(void);
    ~CVoidType(void) {}
    const M1TypeTag typeTag(void) { return M1TYPE_VOID; }
    string typeSignature(void)        { return "v"; }
    string typeName(void)             { return "void"; }

};

// Represent the wildcard type
class CAnyType : public CType {
public:
    CAnyType(void);
    ~CAnyType(void) {}
    const M1TypeTag typeTag(void) { return M1TYPE_ANY; }
    string typeSignature(void)        { return "*"; }
    string typeName(void)             { return "any"; }
};

class CNumericType : public CType {
public:
    CNumericType(void) {}
    CNumericType(string name);
    ~CNumericType(void) {}

    const M1TypeTag typeTag(void) { return M1TYPE_NUMERIC; }
};

//////////////////////////////////////////////////////////////////////////////
//  Word - Base class for word size types
//////////////////////////////////////////////////////////////////////////////

class CWordType : public CNumericType {
public:
    CWordType(void) {}
    CWordType(string name) : CNumericType(name) {}
    ~CWordType(void) {}

    void shallowCopy(CExecutor* aExec, UData src, UData* dst) { *dst = src; }
    void deepCopy(CExecutor* aExec, UData src, UData* dst)    { *dst = src; }

    const size_t sizeOf(void) { return sizeof(UData); }
};

class CByteType : public CWordType {
public:
    CByteType() : CWordType("byte") {}
    ~CByteType()  {}

    const size_t sizeOf(void)         { return 1; }
    const M1TypeTag typeTag(void)     { return M1TYPE_BYTE; }
    string typeSignature(void)        { return "b"; }
    string typeName(void)             { return "byte"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	if (a.b == b.b) return 0;
	else if (a.b < b.b) return -1;
	return 1;
    }

    void print(ostream* os, UData a);
};

class CCharType : public CWordType {
public:
    CCharType() : CWordType("char") {}
    ~CCharType()    {}

    const size_t sizeOf(void)         { return 1; }
    const M1TypeTag typeTag(void)     { return M1TYPE_CHAR; }
    string typeSignature(void)        { return "c"; }
    string typeName(void)             { return "char"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	return a.c - b.c;
    }

    void print(ostream* os, UData a);
};

class CBoolType : public CWordType {
public:
    CBoolType() : CWordType("bool") {}
    ~CBoolType() {}

    const size_t sizeOf(void)         { return 1; }
    const M1TypeTag typeTag(void)     { return M1TYPE_BOOL; }
    string typeSignature(void)        { return "t"; }
    string typeName(void)             { return "bool"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	if (a.t == b.t) return 0;
	if (a.t == false) return -1;
	return 1;
    }

    void print(ostream* os, UData a);
};

class CSignedType : public CWordType {
public:
    CSignedType(void) : CWordType("signed") {}
    CSignedType(string aName) : CWordType(aName) {}
    ~CSignedType()      {}

    const M1TypeTag typeTag(void)  { return M1TYPE_SIGNED; }
    string typeSignature(void)     { return "i"; }
    string typeName(void)          { return "int"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	return a.i - b.i;
    }
    void print(ostream* os, UData a);
};

class CUnsignedType : public CWordType {
public:
    CUnsignedType(void) : CWordType("unsigned") {}
    ~CUnsignedType()    {}

    const M1TypeTag typeTag(void)   { return M1TYPE_UNSIGNED; }
    string typeSignature(void)      { return "u"; }
    string typeName(void)           { return "unsigned int"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	if (a.u == b.u) return 0;
	else if (a.u < b.u) return -1;
	return 1;
    }
	
    void print(ostream* os, UData a);
};

class CFloatType : public CWordType {
public:
    CFloatType(void) : CWordType("float") {}
    ~CFloatType()    {}

    const M1TypeTag typeTag(void)     { return M1TYPE_FLOAT; }
    string typeSignature(void)        { return "f"; }
    string typeName(void)             { return "float"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	float f = a.f - b.f;
	if (f < 0) return -1;
	if (f > 0) return 1;
	return 0;
    }
    void print(ostream* os, UData a);
};

class CTimeType : public CWordType {
public:
    CTimeType(void) : CWordType("time") {}
    ~CTimeType()    {}

    const M1TypeTag typeTag(void)     { return M1TYPE_UNSIGNED; }
    string typeSignature(void)        { return "T"; }
    string typeName(void)             { return "time"; }

    UData produceEvent(CExecutor* aExec,bool aQueued);

    int compare(UData a, UData b) {
	if (a.tm < b.tm) return -1;
	if (a.tm > b.tm) return 1;
	return 0;
    }
    void print(ostream* os, UData a);
};

class CEnumeration {
public:
    CEnumeration(string aName, int aValue) { 
	mName = aName; 
	mValue = aValue; 
    }

    CEnumeration(const CEnumeration& aEnum) {
	mName = aEnum.mName; 
	mValue = aEnum.mValue; 
    }

    ~CEnumeration() {}

    string name()  { return mName; }
    char*  cname() { return (char*) mName.c_str(); }
    int    value() { return mValue; }
private:
    string mName;
    int mValue;
};

typedef std::vector<CEnumeration> CEnumerationVector;

// Enumeration type values are signed ints
class CEnumerationType : public CSignedType {
public:
    CEnumerationType(void) : CSignedType() { }
    CEnumerationType(string aName) : CSignedType(aName) {}
    ~CEnumerationType() {}

    void addEnumeration(string aName, int aValue);

    void addEnumerations(CEnumeration* aEnums, size_t aEnumCount);

    bool enumName(int aValue, string& aName);  // value -> name
    bool enumValue(string aName, int& rValue); // name -> value

    void print(ostream* os);
    void print(ostream* os, UData a);

private:
    CEnumerationVector mEnumerations;
};

#define ENUMERATION(name,value) CEnumeration(#name, value)

#define ENUM_TYPE(CID, CNAME, ...)			\
class CID ## Type : public CEnumerationType {	\
public:							\
   static CID ## Type* singleton(void) {		\
      static CID ## Type *result = 0;		\
      if (!result) result = m1New(CID ## Type);		\
      return result;					\
   }							\
   CID ## Type(string aName) : CEnumerationType(aName) {		\
	CEnumeration enum_desc[] = { __VA_ARGS__ };				\
	addEnumerations(enum_desc,sizeof(enum_desc)/sizeof(enum_desc[0])); \
    } \
    CID ## Type() : CEnumerationType(CNAME) { \
	CEnumeration enum_desc[] = { __VA_ARGS__ };				\
	addEnumerations(enum_desc,sizeof(enum_desc)/sizeof(enum_desc[0])); \
    } \
}

///////////////////////////////////////////////////////////////////////////////
//
//  CArrayType
//
///////////////////////////////////////////////////////////////////////////////

class CArrayType : public CBaseType {
public:
    CArrayType(string eTypeName, size_t n);
    CArrayType(CType* eType, size_t n);
    ~CArrayType() {
	m1ReleaseType(mElemType);
    }

    int mark(Mark_t aMark);

    static CArrayType* create(CType* aType, size_t n);

    void init(CExecutor* aExec,CBaseObject* obj);
    void construct(CExecutor* aExec,CBaseObject* obj);
    void destruct(CExecutor* aExec,CBaseObject* obj);

    UData produce(CExecutor* aExec,CBaseType* aBase=NULL, CArgs* aArgs=NULL);
    UData produceArray(CExecutor* aExec,size_t size);
    UData produceEvent(CExecutor* aExec,bool aQueued);

    UData  retainObject(UData a)  { return URuntime(m1Retain(CRuntime,a.o));  }
    UData  releaseObject(UData a) { return URuntime(m1Release(CRuntime,a.o)); }

    void   shallowCopy(CExecutor* aExec, UData src, UData* dst);
    void   deepCopy(CExecutor* aExec, UData src, UData* dst);

    const size_t sizeOf(void)         { return sizeof(UData); }
    const M1TypeTag typeTag(void)     { return M1TYPE_ARRAY; }
    string typeSignature(void) {
	string baseSig = mElemType->typeSignature();
	if (mArraySize == 0) 
	    return baseSig + "[]";
	else {
	    stringstream sig;
	    sig << baseSig << "[" << mArraySize << "]";
	    return sig.str();
	}
    }
    string typeName(void) {
	string baseName = mElemType->typeName();
	if (mArraySize == 0) 
	    return baseName + "[]";
	else {
	    stringstream nm;
	    nm << baseName << "[" << mArraySize << "]";
	    return nm.str();
	}
    }

    int compare(UData a, UData b);

    CType*  typeAt(int index) { return mElemType; }
    CType*  typeAt(string aname) { return mElemType; }
    int     indexAt(string aName) { return -1; }

    CType*  elementType() { return mElemType; }
    size_t  arraySize()   { return mArraySize; }


    // Put an array object (use value.o) of type CBaseType into a vector
    void elementPut(CExecutor* aExec, UDataVector *vec, int index, UData value, Trig_t trig) {
	CObject* t = vec->at(index).o;
	m1RetainObject(value.o);
	vec->at(index) = value;
	m1ReleaseObject(t);
    }

    void elementCopy(CExecutor* aExec, UDataVector *vec, int index, UData value, Trig_t trig) {
	shallowCopy(aExec, value, &vec->at(index));
    }

    void print(ostream* os, UData a);
    void print(ostream* os);

private:
    CType* mElemType;   // Element type
    size_t  mArraySize;  // Array length 0=dynamic by instansiation
};

///////////////////////////////////////////////////////////////////////////////
//
//  CArray
//
///////////////////////////////////////////////////////////////////////////////

#define m1RetainArray(ptr)      m1Retain(CArray,ptr)
#define m1ReleaseArray(ptr)     m1Release(CArray,ptr)
#define m1SetRetainArray(loc,ptr)  m1SetRetain(CArray,loc,ptr)

class CArray : public CBaseObject {
public:
    CArray(CExecutor* aExec, CArrayType* aType,size_t elemSize,size_t n) : 
	CBaseObject(aExec, aType,elemSize,n) {
	type()->init(aExec, this);
    }

    ~CArray() {  }

    UData at(int index) {
	return typeAt(index)->elementAt(base(), index);
    }

    void put(CExecutor* aExec, int index,UData value,Trig_t trig=TRIGGER_AUTO) {
	typeAt(index)->elementPut(aExec, base(), index, value, trig);
    }

    void copy(CExecutor* aExec,CType* aType, int index,UData value,Trig_t trig=TRIGGER_AUTO) {
	typeAt(index)->elementCopy(aExec, base(), index, value, trig);
    }

    // Array operation - called from built-ins
    void rotate(int aShift);
    void reverse();
    void swap(int i, int j);

    // operators
    void append(CExecutor* aExec, UData aArray);          // += array
    void elementAppend(CExecutor* aExec, UData aElem);    // += element
    void elementPrepend(CExecutor* aExec, UData aElem);   // element + array

    int  erase(CExecutor* aExec, UData aElem);           // -= array
    bool elementErase(CExecutor* aExec, UData aElem);    // -= element

    void stop(CRtExecutor* aExec);
protected:
};


#define QUEUE_EVENT_TYPE(ID, selector, T)				\
    class EventQueue ## ID : public CEventQueue {			\
    public:								\
	    EventQueue ## ID (CExecutable* aOwner) : CEventQueue(aOwner) {} \
	    T putValue(CExecutor* aExec, const T &aValue, Trig_t trig = TRIGGER_AUTO) { \
		UData v; v.selector = aValue;				\
		assign(aExec,v,false,trig);				\
		return aValue;						\
	    }								\
	    T  value(void)      { return (T) mValue.selector; }		\
    }

QUEUE_EVENT_TYPE(Float,    f, float);    
QUEUE_EVENT_TYPE(Signed,   i, int);    
QUEUE_EVENT_TYPE(Unsigned, u, unsigned int);
QUEUE_EVENT_TYPE(Char,     c, char);
QUEUE_EVENT_TYPE(Byte,     b, unsigned char);
QUEUE_EVENT_TYPE(Bool,     t, bool);
QUEUE_EVENT_TYPE(Time,     tm, Time);

///////////////////////////////////////////////////////////////////////////////
//
//  Template for Event Enumeration
//
///////////////////////////////////////////////////////////////////////////////

template <class T> class EventEnum : public CEvent {
public:
    EventEnum(CExecutable* aOwner) : CEvent(aOwner) {}

    T putValue(CExecutor* aExec, const T &aValue, Trig_t trig = TRIGGER_AUTO) {
	UData v; v.i = aValue;
	assign(aExec,v,false, trig);
	return aValue;
    }
    T  value(void)   { return (T) mValue.i; }
};


template <class T> class EventQueueEnum : public CEventQueue {
public:
    EventQueueEnum(CExecutable* aOwner) : CEventQueue(aOwner) {}
    T putValue(CExecutor* aExec, const T &aValue, Trig_t trig = TRIGGER_AUTO) {
	UData v; v.i = aValue;
	assign(aExec,v,false, trig);
	return aValue;
    }
    T  value(void)   { return (T) mValue.i; }
};


///////////////////////////////////////////////////////////////////////////////
//
// Template for Event Objects
//
///////////////////////////////////////////////////////////////////////////////

template <class T> class EventObject : public CEvent {
public:
    EventObject(CExecutable *aOwner) : CEvent(aOwner) {
	mValue.o = NULL;
    }

    EventObject(const EventObject<T>& aEvent) : CEvent(aEvent) {
	// parent copy constructor just copy, so retain the value!
	m1RetainObject(mValue.o);
    }

    ~EventObject(void) {
	m1ReleaseObject(mValue.o);
    }

    T putValue(CExecutor* aExec, const T &aValue, Trig_t trig = TRIGGER_AUTO) {
	UData v; v.o = (CObject*) aValue;
	assign(aExec,v,false, trig);
	return aValue;
    }
    T  value(void)   { return (T) mValue.o; }

    void setValue(UData aValue) { m1SetRetainObject(&mValue.o, aValue.o); }
    void retainValue(UData aValue) { m1RetainObject(aValue.o); }
    void releaseValue(UData aValue) { m1ReleaseObject(aValue.o); }


    int mark(Mark_t aMark) {
	return CEvent::mark(aMark) + m1Mark(mValue.o, aMark);
    }
};


template <class T> class EventQueueObject : public CEventQueue {
public:
    EventQueueObject(CExecutable *aOwner) : CEventQueue(aOwner) {
	mValue.o = NULL;
    }

    EventQueueObject(const EventQueueObject<T>& aEvent) {
	// parent copy constructor just copy so retain the value!
	m1RetainObject(mValue.o);
    }


    ~EventQueueObject(void) {
	m1Release(CObject,mValue.o);
    }

    T putValue(CExecutor* aExec, const T &aValue, Trig_t trig = TRIGGER_AUTO) {
	UData v; v.o = (CObject*) aValue;
	assign(aExec,v,false, trig);
	return aValue;
    }
    T  value(void)   { return (T) mValue.o; }

    void setValue(UData aValue) { m1SetRetainObject(&mValue.o, aValue.o); }
    void retainValue(UData aValue) { m1RetainObject(aValue.o); }
    void releaseValue(UData aValue) { m1ReleaseObject(aValue.o); }

    int mark(Mark_t aMark) {
	// FIXME: mark the queue items
	return CEvent::mark(aMark) + m1Mark(mValue.o, aMark);
    }
};

///////////////////////////////////////////////////////////////////////////////
//
//  EventString
//
///////////////////////////////////////////////////////////////////////////////

class EventString : public CEvent {
public:
    EventString(CExecutable *aOwner) : CEvent(aOwner) {
	CString* s = m1New(CString);
	mValue.str = m1Retain(CString, s);
    }

    EventString(const EventString& aEvent) : CEvent(aEvent) {
	m1Retain(CString, mValue.str);
    }
	

    ~EventString(void) {
	m1Release(CString,mValue.str);
    }

    string putValue(CExecutor* aExec, const string& aValue, Trig_t trig = TRIGGER_AUTO) {
	CString s(aValue);
	UData v;
	v.str = &s;
	assign(aExec,v,false, trig); 
	return aValue;
    }
    string  value(void)   { return mValue.str->str(); }

    void setValue(UData aValue) { mValue.str->set(aValue.str->str()); }
    void retainValue(UData aValue) { m1RetainString(aValue.str); }
    void releaseValue(UData aValue) { m1ReleaseString(aValue.str); }

    int mark(Mark_t aMark) {
	return CEvent::mark(aMark) + m1Mark(mValue.str, aMark);
    }
};


class EventQueueString : public CEventQueue {
public:
    EventQueueString(CExecutable *aOwner) : CEventQueue(aOwner) {
	CString* s = m1New(CString);
	mValue.str = m1Retain(CString, s);
    }

    EventQueueString(const EventQueueString& aEvent) : CEventQueue(aEvent) {
	m1Retain(CString, mValue.str);
    }

    ~EventQueueString(void) {
	m1Release(CString,mValue.str);
    }

    string putValue(CExecutor* aExec, const string& aValue, Trig_t trig = TRIGGER_AUTO) {
	CString s(aValue);
	UData v;
	v.str = &s;
	assign(aExec,v,false,trig);
	return aValue;
    }
    string value(void)    { return mValue.str->str(); }

    void setValue(UData aValue)     { mValue.str->set(aValue.str->str()); }
    void retainValue(UData aValue)  { m1RetainString(aValue.str); }
    void releaseValue(UData aValue) { m1ReleaseString(aValue.str); }

    int mark(Mark_t aMark) {
	// FIXME: mark the queue items
	return CEvent::mark(aMark) + m1Mark(mValue.str, aMark);
    }
};


// Macro for subtype checking
#define isAType(type, ptr) \
    ((ptr != NULL) && ((dynamic_cast<type>(ptr)) != NULL))

// Macro to cast (not using dynamic_cast right now)
// dynamic_cast<type>(ptr)
#define Cast(type, ptr) \
    ((type)(ptr))

#define EVENT_TYPE(CID, Direction) \
    CEventType::create(CID::singleton(), (Direction))

#define EVENT_QUEUE_TYPE(CID, Direction) \
    CEventType::createQueue(CID::singleton(), (Direction))

#define ARRAY_TYPE(CID, Size) \
    CArrayType::create(CID::singleton(), (Size))

//
// NEW VERSION  X version 
//
#define UNWRAP(...) __VA_ARGS__

// A Field declaration that goes with new index macro
#define XFIELD(CID, Visibility, Name, Type, Doc)			\
    CField((Visibility), #Name,(Type),(Doc),nil,&mFieldIndex[CID##_##Name])

#define XFIELDC(CID, Visibility, Name, Type, Doc, Const)		\
    CField((Visibility),#Name,(Type),(Doc),(Const),&mFieldIndex[CID##_##Name])

#define XINDEX(CID, Name) \
    CID::CID ## Type::mFieldIndex[CID::CID ## Type::CID##_##Name]

// This is used to define base object types
#define XBASE_TYPE(CID, CNAME, DOC, FIELDS, ...)	\
class CID ## Type : public CBaseType {	\
public: \
    enum CID##Fields {UNWRAP FIELDS, CID##_##NumFields};	\
    static CID ## Type* singleton(void) { \
	static CID ## Type *result = 0; \
	if (!result) result = m1New(CID ## Type);	\
	return result; \
    } \
    \
    UData produce(CExecutor* aExec,CBaseType* aBase, CArgs* args) {	\
	ExecutorState state;						\
	UData r;							\
	aExec->save(&state);				\
	r.o = m1New(CID, aBase?aBase:CID ## Type::singleton());		\
	if (args) args->run(aExec, r.o);				\
	aExec->restore(&state);						\
	return r;							\
    }									\
    CID ## Type(string aName) : CBaseType(aName) {		\
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    } \
    CID ## Type() : CBaseType(CNAME) { \
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    } \
    void init(CExecutor*, CBaseObject*) {}	\
    void defaults(CExecutor*, CBaseObject*) {}  \
    void construct(CExecutor*, CBaseObject*) {} \
    void destruct(CExecutor*, CBaseObject*) {}	\
    static int  mFieldIndex[CID##_##NumFields];				\
}


#define XOBJECT_TYPE(CID, CNAME, DOC, FIELDS, ...)	\
class CID ## Type : public CExecutableType {	\
public: \
    enum CID##Fields {UNWRAP FIELDS, CID##_##NumFields};	\
    static CID ## Type* singleton(void) {	\
	static CID ## Type *result = 0; \
	if (!result) result = m1New(CID ## Type);	\
	return result; \
    } \
    \
    UData produce(CExecutor* aExec,CBaseType* aBase, CArgs* args) {	\
	ExecutorState state;						\
	UData r;							\
	aExec->save(&state);				\
	r.o = m1New(CID, aExec, aBase?aBase:CID ## Type::singleton());	\
	if (args) args->run(aExec,r.o);					\
	aExec->restore(&state);						\
	if (isAType(CExecutable*, r.o))					\
	    aExec->addComponent((CExecutable*)r.o);			\
	return r;							\
    }									\
    CID ## Type(string aName) : CExecutableType(aName) {		\
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    }									\
    CID ## Type() : CExecutableType(CNAME) { \
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    }									\
									\
    void execute(CExecutor* aExec, CExecutable *aObject) {		\
	Cast(CID *,aObject)->execute(aExec);				\
	Cast(CID *,aObject)->post_execute(aExec);			\
    }									\
    void init(CExecutor*,CBaseObject*) {}				\
    void defaults(CExecutor*,CBaseObject*) {}				\
    void construct(CExecutor*,CBaseObject*) {}				\
    void destruct(CExecutor*,CBaseObject*) {}				\
    static int  mFieldIndex[CID##_##NumFields];				\
}

// This is used to define derived object types
#define XDERIVED_OBJECT_TYPE(CID, PID, CNAME, DOC, FIELDS, ...)	\
class CID ## Type : public PID ## Type {	\
public: \
    enum CID##Fields {UNWRAP FIELDS, CID##_##NumFields};	\
    static CID ## Type* singleton(void) { \
	static CID ## Type *result = 0; \
	if (!result) result = m1New(CID ## Type);	\
	return result; \
    } \
	UData produce(CExecutor* aExec,CBaseType* aBase, CArgs* args) {	\
	ExecutorState state;						\
	UData r;							\
	aExec->save(&state);				\
	r.o = m1New(CID,aExec,aBase?aBase:CID ## Type::singleton());	\
	if (args) args->run(aExec,r.o);					\
	aExec->restore(&state);						\
	if (isAType(CExecutable*, r.o))					\
	    aExec->addComponent((CExecutable*)r.o);	\
	return r;							\
    }									\
    CID ## Type(string aName) : PID ## Type (aName) {                    \
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    }                                                                   \
    CID ## Type() : PID ## Type (CNAME) {				\
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    }									\
    void execute(CExecutor* aExec, CExecutable *aObject) {		\
	Cast(CID *,aObject)->execute(aExec);				\
	PID ## Type::execute(aExec, aObject);				\
	Cast(CID *,aObject)->post_execute(aExec);			\
    }									\
    CType* parentType() { return PID ## Type :: singleton(); }	\
    void init(CExecutor*,CBaseObject*) {}				\
    void defaults(CExecutor*,CBaseObject*) {}  \
    void construct(CExecutor*,CBaseObject*) {} \
    void destruct(CExecutor*,CBaseObject*) {} \
    static int  mFieldIndex[CID##_##NumFields];				\
}

// This is used to define abstract base object types
#define XVIRTUAL_OBJECT_TYPE(CID, CNAME, DOC, FIELDS, ...)	\
class CID ## Type : public CExecutableType {	\
public:							\
    enum CID##Fields {UNWRAP FIELDS, CID##_##NumFields};	\
    static CID ## Type* singleton(void) {		\
	static CID ## Type *result = 0;			\
	if (!result) result = m1New(CID ## Type);	\
	return result;					\
    } \
    \
    CID ## Type(string aName) : CExecutableType(aName) { \
	CField field_desc[] = { __VA_ARGS__ };				\
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    } \
    CID ## Type() : CExecutableType(CNAME) { \
	CField field_desc[] = { __VA_ARGS__ }; \
	setDocumentation((DOC));					\
	setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0])); \
    } \
    void execute(CExecutor* aExec, CExecutable *aObject) {  \
	Cast(CID *,aObject)->execute(aExec);				\
	Cast(CID *,aObject)->post_execute(aExec);			\
    }									\
    void init(CExecutor*,CBaseObject*) {}  \
    void defaults(CExecutor*,CBaseObject*) {}  \
    void construct(CExecutor*,CBaseObject*) {} \
    void destruct(CExecutor*,CBaseObject*) {} \
    static int  mFieldIndex[CID##_##NumFields];				\
}

// Creattion of singleton and declaration of accerlated field lookup
#define XOBJECT_TYPE_BOOTSTRAP(CID) \
    static CID :: CID ## Type *global ## CID ## Type= CID :: CID ## Type :: singleton(); \
    int CID :: CID ## Type :: mFieldIndex[CID ## Type :: CID##_##NumFields]

//
// For those type classes that does not have a field.
//
#define NULL_FIELD CField(Q_NONE, "", static_cast<CType*>(0)) 


// CFileSource
//
// This is the connection between file descriptors and the CSweeper class'
// polling functionality.
// To get polling triggers:
// 
// int fd = open("/dev/whatever");
// CFileSource *file_desc = new CFileSource;
//
// file_desc->setDescriptor(fd);
// file_desc->setPollEvents(POLLIN); // Not necessary, POLLIN is default.
// mFd->connect("revents", this, "my_trigger");
//
// my_trigger will be updated and self will be scheduled when data
// is available.
//
class CFileSource: public CExecutable {
public:
    XOBJECT_TYPE(CFileSource, "FileSource",
		 "File source",
		 (CFileSource_revents),
		 XFIELD(CFileSource,Q_PUBLIC,revents,
			output_signed_type(),
			"Data ready events with POLLx flags set")
	);
public:
    //
    // Constructor/destructor
    //
    CFileSource(CExecutor* aExec, 
		CBaseType *aType = CFileSourceType::singleton());
    ~CFileSource(void);

    //
    // Return the descriptor we are using
    //
    void setDescriptor(CExecutor* aExec, int aDescpriptor, short aPollEvents = POLLIN);
    int descriptor(void) { return mDescriptor; }
    short pollEvents(void) { return mEvents; }

    void stop(CRtExecutor* aExec);
    void execute(CExecutor* aExec); 
    void post_execute(CExecutor* aExec); 

    void ready(short aRevents);

private:
    EventSigned mRevents; // poll_fd::revents value
    int mDescriptor;
    short mEvents;
};

#endif

