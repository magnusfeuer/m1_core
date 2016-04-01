//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __M1VM_HH__
#define __M1VM_HH__
#include <stdio.h>
#include <stdarg.h>
#include <list>
#include <iostream>
#include <memory.h>
#include <sstream>

using namespace std;

#include <sys/types.h>

#include "m1.hh"
#include "m1vm_opcodes.hh"

class VmObjectType;

struct InstructionSpec {
    char*         name;       // instruction name
    Instruction   instr;      // instruction address
    int           args;
    int           produce;    // words added to stack
    int           consume;    // words pop from stack
    char*         pre_spec;   // arguments on stack before entry
    char*         post_spec;  // results on stack after entry
    char*         arg_spec;   // type of arguments
};

extern InstructionSpec vm_instruction[256];

extern EventSigned m1Halt;

//
//  A VmInstruction buffer - used for constructing vm code
//

class VmInstructionBuffer {
public:
    VmInstructionBuffer(int S_size=0, int R_size=0) {
	mDynamic = false;
	mBase    = mBuffer;
	mPtr     = mBuffer;
	mPtrEnd  = mBuffer;
	mSsize   = S_size;
	mScur    = S_size;
	mRsize   = R_size;
	mRcur    = R_size;
    }

    VmInstructionBuffer(Instruction* aBuffer, size_t aBufLen, 
			size_t aSkip=0, bool aDynamic=true,
			int S_size=0,  int R_size=0) {
	mDynamic = aDynamic;
	mBase    = aBuffer;
	mPtr     = mBuffer + aSkip;
	mPtrEnd = mBuffer + aBufLen;
	mSsize   = S_size;
	mScur    = S_size;
	mRsize   = R_size;
	mRcur    = R_size;
    }

    ~VmInstructionBuffer() {
	if (mDynamic && mBase)
	    free(mBase);
    }

    // Get the instructions constructed and restart the internal buffer
    Instruction* retrieve(size_t& aSize, int& S_size, int& R_size) {
	Instruction* ptr = mBase;
	if ((aSize = mPtr - mBase) == 0)
	    return NULL;
	S_size   = mSsize;
	R_size   = mRsize;
	mDynamic = false;
	mBase    = mBuffer;
	mPtr     = mBuffer;
	mPtrEnd  = mBuffer;
	return ptr;
    }

    void setStack(int S_size, int R_size) {
	if (S_size > mSsize) mSsize = S_size;
	if (R_size > mRsize) mRsize = R_size;
    }

    // Resize the instruction buffer if needed 
    void need(size_t aNeed) {
	if ((mPtrEnd - mPtr) < (int) aNeed) {
	    Instruction* base  = mBase;
	    size_t old_size = mPtrEnd - mBase;
	    size_t new_size;
	    int offset;
	
	    if (aNeed < 256) aNeed += 256;
	    new_size = old_size + aNeed;
	    if (mDynamic)
		mBase = (Instruction*) realloc(mBase,new_size*sizeof(Instruction));
	    else {		
		mBase = (Instruction*) malloc(new_size*sizeof(Instruction));
		memcpy(mBase, base, old_size*sizeof(Instruction));
	    }
	    offset = mBase - base;
	    mPtr += offset;
	    mPtrEnd += offset;
	    mPtrEnd += aNeed;
	    mDynamic = true;
	}
    }
    
    //! check number of instruction remain available with out need.
    size_t avail(void) { return mPtrEnd - mPtr; }

    //! allocate an instruction area. (advances internal pointer)
    Instruction* alloc(size_t aSize) {
	Instruction* ptr;

	if (avail() < aSize)
	    need(aSize);
	ptr = mPtr;
	mPtr += aSize;
	return ptr;
    }

    void addR(int amount) { 
	mRcur += amount;
	if (mRcur > mRsize) mRsize = mRcur;
    }

    void addS(int amount) { 
	mScur += amount;
	if (mScur > mSsize) mSsize = mScur;
    }
	

    int put(Instruction instr) {
	int loc = mPtr - mBase;
	if (!avail()) need(1);
	*mPtr++ = instr;
	return loc;
    }

    int put(int loc, Instruction instr) {
	size_t sz = capacity();
	if (loc >= (int) sz) need((loc - sz) + 1);
	mBase[loc] = instr;
	return loc;
    }

    int put_op(int op) {
	addS(vm_instruction[op].produce - vm_instruction[op].consume);
	switch(op) {
	case op_BEGIN:    addR(RETURN_BLOCK_SIZE); break;
	case op_END:      addR(-RETURN_BLOCK_SIZE); break;
	case op_SAVE:     addR(RETURN_SAVE_SIZE); break;
	case op_RESTORE:  addR(-RETURN_SAVE_SIZE); break;
	case op_CALL:     // FIXME - consume n
	case op_INEWa:    // FIXME - consume n
	    break;
	}
	return put(vm_instruction[op].instr);  
    }

    int put_unsigned(unsigned u) { return put((Instruction) u);   }
    int put_signed(unsigned i)   { return put((Instruction) i);   }
    int put_data(void* p)        { return put((Instruction) p);   }
    int put_float(float f)       { 
	union { float f; u_int32_t u; } fu;
	fu.f = f;
	return put((Instruction) fu.u); 
    }

    int put_unsigned(int loc, unsigned u) { return put(loc,(Instruction) u);}
    int put_signed(int loc,   unsigned i) { return put(loc,(Instruction) i); }
    int put_data(int loc, void* p)        { return put(loc,(Instruction) p); }
    int put_float(int loc, float f)       { 
	union { float f; u_int32_t u; } fu;
	fu.f = f;
	return put(loc, (Instruction) fu.u);
    }

    Instruction get(void) {
	if (!avail()) return NULL;
	return *mPtr++;
    }

    int position(void) { return mPtr - mBase; }

    size_t capacity(void)  { return mPtrEnd - mBase; }

    // Move current pointer forward (or allocate if needed)
    void forward(size_t aSize) {
	(void) alloc(aSize);
    }

    // Move current pointer forward
    void backward(size_t aSize) {
	Instruction* ptr = mPtr - aSize;
	mPtr = (ptr < mBase) ? mBase : ptr;
    }

private:
    bool         mDynamic;    // malloced when dynamic==true 
    Instruction* mBase;       // base pointer 
    Instruction* mPtr;        // current insertion point
    Instruction* mPtrEnd;     // end of buffer (+1) 
    Instruction  mBuffer[1];  // used in some dynamic cases 
    int          mSsize;      // calculated value stack size
    int          mScur;       // current value stack level
    int          mRcur;       // calculated return stack size
    int          mRsize;      // current retrun stack size
};



extern void m1_init(char **shared_object_directories);
extern void m1_dump_stat(ostream* os);

// Exceptions & result status for eval ...
enum M1Status {
    M1StatusOk    = 0,
    M1StatusError = -1,
    M1StatusBreak = 1,
    M1StatusContinue = 2,
    M1StatusReturn = 3
};


//!
//! The VM main object, hold all global events
//!

class VmMain : public CExecutable {
public:
    XOBJECT_TYPE(VmMain, (char*) "VmMain",
		 (char*) "M1 main program",
		 (VmMain_version,
		  VmMain_serial,
		  VmMain_halt,
		  VmMain_fps,
		  VmMain_averageSweepTime,
		  VmMain_averageRedrawTime,
		  VmMain_argv),
		 XFIELDC(VmMain,Q_PUBLIC,version,
			string_type(),
			 "Version of M1 runtime system",
			 UString(m1New(CString, (char*) VERSION))),
		 XFIELDC(VmMain,Q_PUBLIC,serial,
			 string_type(),
			 "Serial number of m1 device, as read from /m1/serial.txt",
			 UString(m1New(CString,(char*) ""))),
		 XFIELD(VmMain,Q_PUBLIC,halt,
			input_signed_type(),
			"Halt the M1 system after N cycles"),
		 XFIELD(VmMain,Q_PUBLIC,fps,
			output_float_type(),
			"Number of screen redraws per second"),
		 XFIELD(VmMain,Q_PUBLIC,averageSweepTime,
			output_float_type(),
			"Average time to rin one cycle"),
		 XFIELD(VmMain,Q_PUBLIC,averageRedrawTime,
			output_float_type(),
			"Average time to redraw on screen"),
		 XFIELD(VmMain,Q_PUBLIC,argv,
			CArrayType::create(string_type(),0),
			"Command line arguments to m1 executable")
	);

    VmMain(CExecutor* aExec, CBaseType *aType = VmMainType::singleton());
    ~VmMain(void);

    // Implemented in the m1e_main (m1c_main) etc....
    void execute(CExecutor* aExec);
public:
    // GLOBALS
    CString*     m1Version;
    CString*     m1Serial;
    EventSigned  m1Halt;
    EventFloat   m1FPS;
    EventFloat   m1AvgSweepTime;
    EventFloat   m1AvgRedrawTime;
};

// A reference to global main object
extern VmMain& m1_main(void);

//!
//! The VM mother object, all executable object types defined by them VM
//! are created using this type.
//!

class VmObject : public CExecutable {
public:
    VmObject(CExecutor* aExec, CBaseType* aType);
    ~VmObject(void);

    // Do we need this? VmMachineType and VmEvalType execute
    // do the job
    void execute(CExecutor* aExec) { }

    void stop(CRtExecutor* aExec) { 
	// type()->destruct(aExec, this);  
	CExecutable::stop(aExec);
    }

private:
};

extern list<VmObject*> m1_applicationList;

//!
//! VmObjectType is the parent for VmMachineType and VmEvalType
//!
class VmObjectType : public CExecutableType {
public:
    // The singleton is "dummy" parent for VmMachineType and VmEvalType
    static VmObjectType* singleton(void) {
	static VmObjectType *result = 0;
	if (!result) result = m1New(VmObjectType, T_TYPE);
	return result;
    }

    VmObjectType(string aName, Definition_t aDef);
    VmObjectType(Definition_t aDef);
    ~VmObjectType(void);

    int mark(Mark_t aMark);

    UData produce(CExecutor* aExec, CBaseType* aBase, CArgs* args);

    // Scope is the sourrounding scope (m1_context/library/file/type)
    void setScope(CType* aScope);

    CType* scope() { return mScope; }

    bool isLibrary(void);
    bool isApplication(void);

    // The namespace name
    string scopeName();

    void execute(CExecutor* aExec, CExecutable *aObject) {
	Cast(VmObject*,aObject)->execute(aExec);
    }
    unsigned int constructorFrameSize(void) { return mConstructorFrameSize; }
    unsigned int constructorFrameSize(unsigned int aSize) { 
	return mConstructorFrameSize=aSize; }

    unsigned int destructorFrameSize(void) { return mDestructorFrameSize; }
    unsigned int destructorFrameSize(unsigned int aSize) { 
	return mDestructorFrameSize=aSize; }
    Definition_t definitionType(void) { return mDefinitionType; }
protected:
    unsigned int mConstructorFrameSize;
    unsigned int mDestructorFrameSize;
    Definition_t mDefinitionType;
private:
    CType* mScope;  // The type context 
};


//!
//! VmInterfaceType is a place holder type for undefined/forwarded types
//!
class VmInterfaceType : public VmObjectType {
public:
    VmInterfaceType(string aName,Definition_t aDef);
    ~VmInterfaceType();

    void setChild(CType* aChildType);
    
    // Should generate nil if unresolved, or demand load dependices
    UData produce(CExecutor* aExec, CBaseType* aBase, CArgs* args);    

    // Should generate a failure
    void execute(CExecutor* aExec, CExecutable *aObject);

    // Print interface declarations 
    void print(ostream* os);
    void print(ostream* os, UData a);
private:
    // Version info for interface (from library)
    int mVerMajor;
    int mVerMinor;
    int mVerPatch;

    // Implmentation binding
    CType* mImplementation;   // NULL when unresolved

    // Prototype object
    CBaseObject* mPrototype;  // Object prototype

    // list of subtypes directly inherit this interface, and that
    // needs resolving when the implementation is loaded.
    list<CBaseType*> derivedTypes;
};

//!
//! VmMachineType represents the class for virtual machine objects
//!

class VmMachineType : public VmObjectType {
public:
    VmMachineType(string name,Definition_t aDef);
    VmMachineType(Definition_t aDef);
    ~VmMachineType(void);

    void init(CExecutor* aExec, CBaseObject* aObject);
    void defaults(CExecutor* aExec, CBaseObject* aObject);
    void construct(CExecutor* aExec, CBaseObject* aObject);
    void destruct(CExecutor* aExec, CBaseObject* aObject);
    UData produce(CExecutor* aExec, CBaseType* aBase, CArgs* args);
    // Objects of this type is not built in
    bool   isVmType()   { return false; }

    void execute(CExecutor* aExec, CExecutable *aObject);
    
    void format(ostream* os, UData a) { print(os, a); }

    size_t valueStackSize(void)  { return S_size; };
    size_t returnStackSize(void) { return R_size; };

    void setValueStackSize(size_t sz)  { 
	if (sz > S_size) S_size = sz;
    }

    void setReturnStackSize(size_t sz) { 
	if (sz > R_size) R_size = sz;
    }

    u_int8_t* loadPreScript(u_int8_t* ptr,u_int8_t* ptr_end);
    u_int8_t* loadPostScript(u_int8_t* ptr,u_int8_t* ptr_end);
    u_int8_t* loadConstructor(u_int8_t* ptr,u_int8_t* ptr_end);
    u_int8_t* loadDestructor(u_int8_t* ptr,u_int8_t* ptr_end);
    u_int8_t* loadDefaults(u_int8_t* ptr,u_int8_t* ptr_end);

    Instruction* preScript(void) { return mPreScript; }
    Instruction* postScript(void) { return mPostScript; }

    void setPreScript(Instruction* aScript) {
	if (mPreScript) delete [] mPreScript;
	mPreScript = aScript;
    }
    void setPostScript(Instruction* aScript) {
	if (mPostScript) delete [] mPostScript;
	mPostScript = aScript;
    }

    Instruction* constructor(void) { return mConstructor; }

    void setConstructor(Instruction* aConstructor) {
	if (mConstructor) delete [] mConstructor;
	mConstructor = aConstructor;
    }

    Instruction* destructor(void) { return mDestructor; }

    void setDestructor(Instruction* aDestructor) {
	if (mDestructor) delete [] mDestructor;
	mDestructor = aDestructor;
    }

    Instruction* defaults(void) { return mDefaults; }

    void setDefaults(Instruction* aDefaults) {
	if (mDefaults) delete [] mDefaults;
	mDefaults = aDefaults;
    }

    CString* addStringConstant(string s) {
	int sz = cnst.size();
	int i;
	CString* cstr;

	for (i = 0; i < sz; i++) {
	    if (cnst[i].o->type() == string_type()) {
		if (((CStringType*)string_type())->compare(cnst[i],s) == 0)
		    return cnst[i].str;
	    }
	}
	cnst.resize(sz+1);
	cstr = m1New(CString, s);
	cnst[sz].str = m1RetainString(cstr);
	return cstr;
    }

    // Emit the dissassembly on the ostream
    void printCode(ostream* os);

    void writeCode(ostream* os);

    void exec(CExecutor* aExec, Instruction* I, bool init);

private:
    size_t S_size;   // Initial size of value stack
    size_t R_size;   // Initial size of control stack
    size_t N_size;   // Size of constant area

    Instruction*  mPreScript;    // Execute code before parent (may be NULL)
    Instruction*  mPostScript;   // Execute code after parent (may be NULL)
    Instruction*  mConstructor;  // Constructor code (may be NULL)
    Instruction*  mDestructor;   // Destructor code (may be NULL)
    Instruction*  mDefaults;     // Default values (may be NULL)
    
    UDataVector    cnst;           // constants area (strings and ...)

    u_int8_t* loadCode(char* execName,
		       u_int8_t* ptr,u_int8_t* ptr_end,
		       Instruction** code);
};

class VmMachineArgs : public CArgs {
public:
    VmMachineArgs(CExecutable* aCreator, CBaseObject* aGlobal,
		  Instruction* aArgs) :
	CArgs(aCreator, aGlobal) {
	mArgs = aArgs;
    }
    ~VmMachineArgs(void) { }

    void run(CExecutor* aExec, CObject* obj);

private:
    Instruction* mArgs;
};


#endif
