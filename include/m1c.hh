//
// M1C parse & compiler defintions
//
#ifndef __M1C_HH__
#define __M1C_HH__

#include <stdio.h>
#include <list>
#include <iostream>
#include <sstream>

#include "m1vm_opcodes.hh"
#include "m1vm.hh"

using namespace std;

class M1Expr;
class M1Statement;
class M1StatementList;
class M1Specifier;
class M1Identifier;
class M1Declaration;
class M1Script;
class M1ScriptList;
class M1TypeSpecifier;
class VmEvalType;
class M1Constant;



/* From m1c_eval.cc */
extern bool evalDefinitions(VmEvalType* aType);
extern UData m1Convert(UData r, CType* from, CType* to);

/* From m1c_lint.cc */
extern VmEvalType* lintDefinitions(M1TypeSpecifier* file);

/* From m1c_vmload.cc */
extern VmMachineType* loadDefinitions(VmEvalType* aEval);

/* From m1c_print.cc */
extern void printDefinitions(VmEvalType* aEval, ostream* os);


extern long          m1NumericToLong(char* cptr, char** end_ptr);
extern unsigned long m1NumericToULong(char* cptr, char** end_ptr);
extern float         m1NumericToFloat(char* cptr, char** end_ptr);

extern CType* mgNumericType(CType* a, CType* b);
extern CType* mgIntegerType(CType* a, CType* b);

/* From m1c_print.cc */
string formatOperator(int op);

static void inline put_f32(ostream* os, float f)
{
    char buf[4] = { VM_FLOAT32(f) };
    os->write(buf, 4);
}

static void inline put_f64(ostream* os, double f)
{
    char buf[8] = { VM_FLOAT64(f) };
    os->write(buf, 8);
}

static void inline put_u32(ostream* os, unsigned int u)
{
    char buf[4] = { VM_UNSIGNED32(u) };
    os->write(buf, 4);
}

static void inline put_i32(ostream* os, signed int i)
{
    char buf[4] = { VM_SIGNED32(i) };
    os->write(buf, 4);
}

static void inline put_u64(ostream* os, unsigned long long u)
{
    char buf[8] = { VM_UNSIGNED64(u) };
    os->write(buf, 8);
}

static void inline put_i64(ostream* os, long long i)
{
    char buf[8] = { VM_SIGNED64(i) };
    os->write(buf, 8);
}

static inline UAddress nullCheck(UAddress a)
{
    if (a.obj == NULL) {
	m1BreakHere(__FILE__, __LINE__, "nil access exception");
	throw M1StatusError;
    }
    return a;
}

// 3-bit code 
#define M1CVT_byte     1
#define M1CVT_char     2
#define M1CVT_bool     3
#define M1CVT_signed   4
#define M1CVT_unsigned 5
#define M1CVT_float    6

    
#define M1CVT_code(from,to) ((from) | ((to)<<3))
#define M1CVT_from(code) ((code) & 0x7)
#define M1CVT_to(code)   (((code)>>3) & 0x7)

static inline char M1CVT_selector(int code)
{
    switch (code & 0xff) {
    case M1CVT_byte: return 'b';
    case M1CVT_char: return 'c';
    case M1CVT_bool: return 't';
    case M1CVT_signed: return 'i';
    case M1CVT_unsigned: return 'u';
    case M1CVT_float: return 'f';
    default: return 'n';
    }
}


// 6-bit code
typedef enum {
    M1CVT_nn = M1CVT_code(0, 0),  // no conversion

    M1CVT_bb = M1CVT_code(M1CVT_byte,M1CVT_byte),
    M1CVT_bc = M1CVT_code(M1CVT_byte,M1CVT_char),
    M1CVT_bt = M1CVT_code(M1CVT_byte,M1CVT_bool),
    M1CVT_bi = M1CVT_code(M1CVT_byte,M1CVT_signed),
    M1CVT_bu = M1CVT_code(M1CVT_byte,M1CVT_unsigned),
    M1CVT_bf = M1CVT_code(M1CVT_byte,M1CVT_float),

    M1CVT_cb = M1CVT_code(M1CVT_char,M1CVT_byte),
    M1CVT_cc = M1CVT_code(M1CVT_char,M1CVT_char),
    M1CVT_ct = M1CVT_code(M1CVT_char,M1CVT_bool),
    M1CVT_ci = M1CVT_code(M1CVT_char,M1CVT_signed),
    M1CVT_cu = M1CVT_code(M1CVT_char,M1CVT_unsigned),
    M1CVT_cf = M1CVT_code(M1CVT_char,M1CVT_float),

    M1CVT_tb = M1CVT_code(M1CVT_bool,M1CVT_byte),
    M1CVT_tc = M1CVT_code(M1CVT_bool,M1CVT_char),
    M1CVT_tt = M1CVT_code(M1CVT_bool,M1CVT_bool),
    M1CVT_ti = M1CVT_code(M1CVT_bool,M1CVT_signed),
    M1CVT_tu = M1CVT_code(M1CVT_bool,M1CVT_unsigned),
    M1CVT_tf = M1CVT_code(M1CVT_bool,M1CVT_float),

    M1CVT_ib = M1CVT_code(M1CVT_signed,M1CVT_byte),
    M1CVT_ic = M1CVT_code(M1CVT_signed,M1CVT_char),
    M1CVT_it = M1CVT_code(M1CVT_signed,M1CVT_bool),
    M1CVT_ii = M1CVT_code(M1CVT_signed,M1CVT_signed),
    M1CVT_iu = M1CVT_code(M1CVT_signed,M1CVT_unsigned),
    M1CVT_if = M1CVT_code(M1CVT_signed,M1CVT_float),

    M1CVT_ub = M1CVT_code(M1CVT_unsigned,M1CVT_byte),
    M1CVT_uc = M1CVT_code(M1CVT_unsigned,M1CVT_char),
    M1CVT_ut = M1CVT_code(M1CVT_unsigned,M1CVT_bool),
    M1CVT_ui = M1CVT_code(M1CVT_unsigned,M1CVT_signed),
    M1CVT_uu = M1CVT_code(M1CVT_unsigned,M1CVT_unsigned),
    M1CVT_uf = M1CVT_code(M1CVT_unsigned,M1CVT_float),

    M1CVT_fb = M1CVT_code(M1CVT_float,M1CVT_byte),
    M1CVT_fc = M1CVT_code(M1CVT_float,M1CVT_char),
    M1CVT_ft = M1CVT_code(M1CVT_float,M1CVT_bool),
    M1CVT_fi = M1CVT_code(M1CVT_float,M1CVT_signed),
    M1CVT_fu = M1CVT_code(M1CVT_float,M1CVT_unsigned),
    M1CVT_ff = M1CVT_code(M1CVT_float,M1CVT_float),
} M1CvtCode;


#define FBINARY_OP(op,tp,v,lv,rv)					\
    switch((tp)->typeTag()) {						\
    case M1TYPE_FLOAT:    (v).f = (lv).f op (rv).f; break;		\
    case M1TYPE_SIGNED:   (v).i = (lv).i op (rv).i; break;		\
    case M1TYPE_UNSIGNED: (v).u = (lv).u op (rv).u; break;		\
    case M1TYPE_CHAR:     (v).c = (lv).c op (rv).c; break;		\
    case M1TYPE_BYTE:     (v).b = (lv).b op (rv).b; break;		\
    case M1TYPE_BOOL:     (v).t = (lv).t op (rv).t; break;		\
    default: break;							\
    }

#define IBINARY_OP(op,tp,v,lv,rv)					\
    switch((tp)->typeTag()) {						\
    case M1TYPE_SIGNED:   (v).i = (lv).i op (rv).i; break;		\
    case M1TYPE_UNSIGNED: (v).u = (lv).u op (rv).u; break;		\
    case M1TYPE_CHAR:     (v).c = (lv).c op (rv).c; break;		\
    case M1TYPE_BYTE:     (v).b = (lv).b op (rv).b; break;		\
    case M1TYPE_BOOL:     (v).t = (lv).t op (rv).t; break;		\
    default: break;							\
    }

#define SBINARY_OP(op,tp,v,lv,rv)					\
    switch((tp)->typeTag()) {						\
    case M1TYPE_SIGNED:   (v).i = (lv).i op (rv).i; break;		\
    case M1TYPE_UNSIGNED: (v).u = (lv).u op (rv).i; break;		\
    case M1TYPE_CHAR:     (v).c = (lv).c op (rv).i; break;		\
    case M1TYPE_BYTE:     (v).b = (lv).b op (rv).i; break;		\
    case M1TYPE_BOOL:     (v).t = (lv).t op (rv).i; break;		\
    default: break;							\
    }

#define RBINARY_OP(op,tp,v,lv,rv)					\
    switch((tp)->typeTag()) {						\
    case M1TYPE_FLOAT:    (v).i = (lv).f op (rv).f; break;		\
    case M1TYPE_SIGNED:   (v).i = (lv).i op (rv).i; break;		\
    case M1TYPE_UNSIGNED: (v).i = (lv).u op (rv).u; break;		\
    case M1TYPE_CHAR:     (v).i = (lv).c op (rv).c; break;		\
    case M1TYPE_BYTE:     (v).i = (lv).b op (rv).b; break;		\
    case M1TYPE_BOOL:     (v).i = (lv).t op (rv).t; break;		\
    default:              (v).i = (lv).o op (rv).o; break;		\
    }

// Unary operator on floats and integers
#define FUNARY_OP(op,tp,v,mv)					\
    switch((tp)->typeTag()) {					\
    case M1TYPE_FLOAT:    (v).f = op (mv).f; break;		\
    case M1TYPE_SIGNED:   (v).i = op (mv).i; break;		\
    case M1TYPE_UNSIGNED: (v).u = op (mv).u; break;		\
    case M1TYPE_CHAR:     (v).c = op (mv).c; break;		\
    case M1TYPE_BYTE:     (v).b = op (mv).b; break;		\
    case M1TYPE_BOOL:     (v).t = op (mv).t; break;		\
    default: break;						\
    }

// Unary operator on integers
#define IUNARY_OP(op,tp,v,mv)					\
    switch((tp)->typeTag()) {					\
    case M1TYPE_SIGNED:   (v).i = op (mv).i; break;		\
    case M1TYPE_UNSIGNED: (v).u = op (mv).u; break;		\
    case M1TYPE_CHAR:     (v).c = op (mv).c; break;		\
    case M1TYPE_BYTE:     (v).b = op (mv).b; break;		\
    case M1TYPE_BOOL:     (v).t = op (mv).t; break;		\
    default: break;						\
    }

// Unary op converting to integer (!)
#define RUNARY_OP(op,tp,v,mv)					\
    switch((tp)->typeTag()) {					\
    case M1TYPE_FLOAT:    (v).i = op (mv).f; break;		\
    case M1TYPE_SIGNED:   (v).i = op (mv).i; break;		\
    case M1TYPE_UNSIGNED: (v).i = op (mv).u; break;		\
    case M1TYPE_CHAR:     (v).i = op (mv).c; break;		\
    case M1TYPE_BYTE:     (v).i = op (mv).b; break;		\
    case M1TYPE_BOOL:     (v).i = op (mv).t; break;		\
    default: break;						\
    }

// Extra container for lint time information
class CLintInfo {
public:
    CLintInfo() : mLevel(0), mSize(0), mMaxSize(0) 
	{}
    CLintInfo(int aLevel, int aSize, int aMaxSize) :
	mLevel(aLevel), mSize(aSize),  mMaxSize(aMaxSize) 
	{}

    unsigned int expandFrame(int mExpandWith=1) { 
	mSize += mExpandWith;
	if (mSize > mMaxSize)
	    mMaxSize = mSize;
	return mSize;
    }
    unsigned int shrinkFrame(int mShrinkWith=1) {
	mSize -= mShrinkWith;
	return mSize;
    }
    int  nextFramePos(void) { 
	return (int) mSize; 
    }
    unsigned int  maxFrameSize(void) { return mMaxSize; }
    int  downLevel(void)    { mLevel++; return mSize; }
    void upLevel(unsigned int aSize) { mLevel--; mSize = aSize; }
    int  level(void)     { return mLevel; }
    
private:
    int mLevel;             // current compound level
    unsigned int mSize;     // current frame base offset
    unsigned int mMaxSize;  // max frame size
};


class M1Element : public CRuntime {
public:
    M1Element(void) {
	STAT_INC(m1_stat_live_elements);
	mLine = 0;
	mFile = "???";
	mMark = MARK_INIT;
    }

    ~M1Element(void) {
	STAT_DEC(m1_stat_live_elements);
    }

    int          line(void) { return mLine; }
    const char*  file(void) { return mFile; }

    void        setLine(int aLine) { mLine = aLine; }
    void        setFile(const char* aFile) { mFile = aFile; }
    

    virtual void print(ostream* os, int indent=0) {};

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Element: file=%s, line=%d, #%lu 0x%lx:",
		mFile, mLine, refCount(), (unsigned long) this);
	return string(ndata);
    }

private:
    const char* mFile;
    int mLine;
};


class M1ExprIterator {
public:
    M1ExprIterator() {}
    virtual ~M1ExprIterator();

    // before is called before sub-expression are iterated
    // return this to continue to elements
    //        NULL to keep expression as is a skip after
    //        new-expr to replace current expression and skip after!
    virtual M1Expr* before(M1Expr* aElement);

    // element is called after sub-expression are iterated
    virtual M1Expr* after(M1Expr* aElement);
};


class M1Expr : public M1Element {
public:
    M1Expr(int opID, CType* aType) {
	mOperator=opID;
	mType=m1RetainType(aType);
	mStorage = Q_NONE;
	mIsLinted = false;
    }

    ~M1Expr(void) {
	m1ReleaseType(mType);
    }

    int mark(Mark_t aMark) { 
	return m1MarkAndCheck(aMark,M1Element,mType); 
    }

    int            op(void) { return mOperator; }

    string         operatorName(void) { return formatOperator(op()); }

    CType*         type(void) { return mType; }

    CType* setType(CType* aType) {
	m1SetRetainType(&mType,aType); 
	return aType;  
    }

    int            storage(void) { return mStorage; }
    void           setStorage(int aStorageClass) { mStorage = aStorageClass; }

    virtual void    bytecode(ostream* os) {}

    virtual void load(VmMachineType* aMachine,VmInstructionBuffer* vmb) {}
    virtual void load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb) {}
    virtual void load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb) {}

    virtual void    iterate(M1Expr** aExpr, M1ExprIterator* iter);

    bool            isLinted(void) { return mIsLinted; }
    void            setIsLinted(bool aIsLinted) { mIsLinted = aIsLinted; }
    virtual CType*  lint(M1Expr** aExpr, CType* restrict);
    virtual CType*  lint_ref(M1Expr** aExpr, CType* restrict);
    virtual CType*  lint_trg(M1Expr** aExpr);

    virtual UData    eval(CExecutor* aExec);
    virtual UAddress eval_ref(CExecutor* aExec);
    virtual bool     eval_trg(CExecutor* aExec);

    virtual UData   value(void) { return nil; }
    virtual void    setValue(UData aValue) {  }

    // Type conversion code
    virtual void    setCvt(M1CvtCode aCvt) { }
    virtual M1CvtCode cvt(void) { return M1CVT_nn; }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Expr: file=%s, line=%d, operator=%d, #%lu 0x%lx:",
		file(), line(), mOperator, refCount(), (unsigned long) this);
	return string(ndata);
    }
private:
    CType*  mType;        // Node type (when typed and ready)
    int     mStorage;     // Nodes storage class (lhs expressions)
    int     mOperator;    // Parse tree operator id
    bool    mIsLinted;    // expression already linted
};

#define m1RetainExpr(ptr)         m1Retain(M1Expr,ptr)
#define m1ReleaseExpr(ptr)        m1Release(M1Expr,ptr)
#define m1SetExpr(loc,ptr)        m1Set(M1Expr,loc,ptr)
#define m1SetRetainExpr(loc,ptr)  m1SetRetain(M1Expr,loc,ptr)
#define m1CondSetRetainExpr(loc,ptr)  m1CondSetRetain(M1Expr,loc,ptr)

class M1ExprList : public M1Element {
public:
    M1ExprList(void) { }

    ~M1ExprList(void) {
	list<M1Expr*>::iterator iter = mList.begin();
	while(iter != mList.end()) {
	    m1ReleaseExpr(*iter);
	    iter++;
	}
    }

    int mark(Mark_t aMark) {
	int marked = 0;
	if (mMark != aMark) {
	    list<M1Expr*>::iterator iter;
	    marked += M1Element::mark(aMark);
	    iter = mList.begin();
	    while(iter != mList.end()) {
		marked += m1Mark((*iter), aMark);
		iter++;
	    }
	}
	return marked;
    }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "ExprList: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }


    void appendElement(M1Expr* expr) {
	mList.push_back(m1RetainExpr(expr));
    }

    void prependElement(M1Expr* expr) {
	mList.push_front(m1RetainExpr(expr));
    }

    M1Expr* popElement(void) {
	M1Expr* expr;
	if (mList.empty())
	    return NULL;
	expr = mList.front();
	mList.pop_front();
	m1ReleaseExpr(expr);
	return expr;
    }
	
    bool empty(void) { return mList.empty(); }

    size_t size(void) { return mList.size(); }

    list<M1Expr*>::iterator erase(list<M1Expr*>::iterator loc) {
	// NOTE: m1Release must be called by caller
	return mList.erase(loc);
    }
    list<M1Expr*>::iterator insert(list<M1Expr*>::iterator loc,M1Expr* expr) {
	return mList.insert(loc,m1RetainExpr(expr)); 
    }

    // Ugly stuff?
    list<M1Expr*>::iterator begin() { return mList.begin(); }
    list<M1Expr*>::iterator end()   { return mList.end(); }

    void  print(ostream* os, int indent);
    void  iterate(M1ExprIterator* iter);

private:
    list<M1Expr*> mList;
};

#define m1RetainExprList(ptr)         m1Retain(M1ExprList,ptr)
#define m1ReleaseExprList(ptr)        m1Release(M1ExprList,ptr)
#define m1SetExprList(loc,ptr)        m1Set(M1ExprList,loc,ptr)

class M1Enum : public M1Element {
public:
    M1Enum(string aId, M1Expr* aExpr) { 
	mId = aId;
	mExpr = m1RetainExpr(aExpr);
    }

    ~M1Enum(void) {
	m1ReleaseExpr(mExpr);
    }

    int mark(Mark_t aMark) { 
	return m1MarkAndCheck(aMark,M1Element,mExpr);
    }

    string   id(void)     { return mId;     }
    M1Expr*  value(void)  { return mExpr;   }
    M1Expr** valuep(void) { return &mExpr;  }

    void print(ostream* os, int indent=0);
    
private:
    string  mId;      // fixme always M1Identifier* 
    M1Expr* mExpr;
};

#define m1RetainEnum(ptr)         m1Retain(M1Enum,ptr)
#define m1ReleaseEnum(ptr)        m1Release(M1Enum,ptr)
#define m1SetRetainEnum(loc,ptr)  m1SetRetain(M1Enum,loc,ptr)


class M1EnumList : public M1Element {
public:
    M1EnumList(void) { }

    ~M1EnumList(void) {
	list<M1Enum*>::iterator iter = mList.begin();
	while(iter != mList.end()) {
	    m1ReleaseEnum(*iter);
	    iter++;
	}
    }

    int mark(Mark_t aMark) {
	int marked = 0;
	if (mMark != aMark) {
	    list<M1Enum*>::iterator iter;
	    marked += M1Element::mark(aMark);
	    iter = mList.begin();
	    while(iter != mList.end()) {
		marked += m1Mark((*iter), aMark);
		iter++;
	    }
	}
	return marked;
    }

    void appendElement(M1Enum* expr) {
	mList.push_back(m1RetainEnum(expr));
    }

    void print(ostream* os, int indent);

    // Ugly stuff?
    list<M1Enum*>::iterator begin() { return mList.begin(); }
    list<M1Enum*>::iterator end()   { return mList.end(); }
private:
    list<M1Enum*> mList;
};

#define m1RetainEnumList(ptr)         m1Retain(M1EnumList,ptr)
#define m1ReleaseEnumList(ptr)        m1Release(M1EnumList,ptr)

class M1Version : public M1Element {
public:
    M1Version(int aMajor, int aMinor, int aPatch) {
	mMajor = aMajor;
	mMinor = aMinor;
	mPatch = aPatch;
    }
    ~M1Version(void) {}

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Version: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int vsnMajor() { return mMajor; }
    int vsnMinor() { return mMinor; }
    int vsnPatch() { return mPatch; }

    void print(ostream* os, int indent=0);
private:
    unsigned mMajor;
    unsigned mMinor;
    unsigned mPatch;
};

class M1Specifier : public M1Element {
public:
    M1Specifier(void) {}
    ~M1Specifier(void) {}
    virtual string name(void) { return ""; }
    virtual CType* lint(int aStorage) = 0;
private:
};

class M1TypePrimitive : public M1Specifier {
public:
    M1TypePrimitive(CType* aType) { mType=m1RetainType(aType); }
    ~M1TypePrimitive(void) { m1ReleaseType(mType); }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "TypePrimitive: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Specifier, mType);
    }    

    CType* lint(int aStorage);
    void print(ostream* os, int indent=0);
    string name(void) { return mType->name(); }
private:
    CType* mType;
};

class M1TypeIdentifier : public M1Specifier {
public:
    M1TypeIdentifier(string aTypeId) { mTypeId = aTypeId; }
    ~M1TypeIdentifier(void) { }

    CType* lint(int aStorage);
    void   print(ostream* os, int indent=0);
    string name(void)    { return mTypeId; }
    char*  cname(void)   { return (char*) mTypeId.c_str(); }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "TypeIdentifier: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }
private:
    string mTypeId;
};

class M1EventTypeIdentifier : public M1Specifier {
public:
    M1EventTypeIdentifier(string aTypeId,bool aQueued,int aDirection) { 
	mTypeId = aTypeId;
	mQueued    = aQueued;
	mDirection = aDirection;
    }
    ~M1EventTypeIdentifier(void) { }

    CType* lint(int aStorage);
    void   print(ostream* os, int indent=0);
    string name(void)    {
	switch(mDirection & E_INOUT) {
	case E_INPUT:
	    if (mQueued)
		return "input event queue "+ mTypeId; 
	    else
		return "input event "+ mTypeId; 
	case E_OUTPUT: return "output event "+ mTypeId; 
	case E_INOUT: 
	    if (mQueued)
		return "event queue "+ mTypeId; 
	    else
		return "event "+ mTypeId; 
	default: return mTypeId;
	}
    }

    char*  cname(void)   { return (char*) name().c_str(); }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "EventTypeIdentifier: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

private:
    int mDirection;
    bool mQueued;
    string mTypeId;
};

// enum [<typeid>] {  a = 1, b = 2 } 
class M1EnumSpecifier : public M1Specifier {
public:
    M1EnumSpecifier(M1TypeIdentifier* aType, M1EnumList* aEnumList) {
	mType = m1Retain(M1TypeIdentifier,aType);
	mEnumList = m1RetainEnumList(aEnumList);
    }

    ~M1EnumSpecifier(void) {
	m1Release(M1TypeIdentifier, mType);
	m1ReleaseEnumList(mEnumList);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Specifier, mType, mEnumList);
    }

    CType* lint(int aStorage);
    void print(ostream* os, int indent=0);

private:
    M1TypeIdentifier* mType;
    M1EnumList* mEnumList;
};

// type_declarator '[' expr ']'
class M1ArrayTypeDeclarator : public M1Specifier {
public:
    M1ArrayTypeDeclarator(M1Specifier* aBaseType, M1Expr* aExpr) {
	mBaseType = m1Retain(M1Specifier,aBaseType);
	mSize = m1RetainExpr(aExpr);
    }
    ~M1ArrayTypeDeclarator(void) {
	m1Release(M1Specifier,mBaseType);
	m1ReleaseExpr(mSize);
    }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "ArrayTypeDeclarator: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Specifier, mBaseType, mSize);
    }

    CType* lint(int aStorage);
    void print(ostream* os, int indent=0);
private:
    M1Specifier*  mBaseType;
    M1Expr* mSize;   // NULL for dynamic array []
};


// Field identifier - example  x 
class M1Identifier : public M1Expr {
public:
    M1Identifier(string aID) : M1Expr(op_ID, NULL) { 
	mID = aID; 
	mIndex = -1;
	mLevel = SCOPE_NONE;
    }
    ~M1Identifier(void) {  }

    void setIndex(int aIndex) { mIndex = aIndex; }
    int  index(void) { return mIndex; }

    ActivationType_t activationType(void) { return mLevel; }

    void print(ostream* os, int indent=0);

    string name(void) { return mID; }
    char*  cname(void) { return (char*) mID.c_str(); }
    string id(void)  { return mID; }

    void   load(VmMachineType* aMachine,VmInstructionBuffer* vmb);    
    void   load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void   load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);
    CType* lint_trg(M1Expr** aExpr);

    UData eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
    bool     eval_trg(CExecutor* aExec);
protected:
    string mID;
    int mIndex;              // field index in activation object
    ActivationType_t mLevel; // index to scope object (stack/object/library...)
};


// Base identifier @Type[:Type..].field for access to parent type
// example:  @Layer.width
class M1BasedIdentifier : public M1Expr {
public:
    M1BasedIdentifier(M1TypeIdentifier* aTypeId, string aId) : 
	M1Expr(op_BID, NULL) { 
	mTypeId = m1Retain(M1TypeIdentifier, aTypeId);
	mId = aId; 
	mIndex = -1;
    }
    ~M1BasedIdentifier(void) {  
	m1Release(M1TypeIdentifier, mTypeId);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mTypeId);
    }

    void setIndex(int aIndex) { mIndex = aIndex; }
    int  index(void) { return mIndex; }

    void print(ostream* os, int indent=0);

    string name(void) { return mId; }
    char*  cname(void) { return (char*) mId.c_str(); }
    string id(void)  { return mId; }
    M1TypeIdentifier* typeId(void) { return mTypeId; }

    void   load(VmMachineType* aMachine,VmInstructionBuffer* vmb);    
    void   load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);    

    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);

    UData    eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);

protected:
    M1TypeIdentifier* mTypeId;  // The base type | NULL
    string mId;
    int mIndex;                // field index in scope
};

//
// Field index expression, return index to the field
// example
//       &witdh
//       &Layer:width
//
class M1Index : public M1Expr {
public:
    M1Index(M1TypeIdentifier* aTypeId, string aId) : M1Expr(op_IX, NULL) { 
	mTypeId = m1Retain(M1TypeIdentifier, aTypeId);
	mId = aId; 
    }
    ~M1Index(void) {  
	m1Release(M1TypeIdentifier, mTypeId);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mTypeId);
    }

    void print(ostream* os, int indent=0);

    string name(void) { return mId; }
    char*  cname(void) { return (char*) mId.c_str(); }
    string id(void)  { return mId; }
    M1TypeIdentifier* typeId(void) { return mTypeId; }

    void   load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    UData    eval(CExecutor* aExec);
protected:
    M1TypeIdentifier* mTypeId;  // The base type | NULL
    string mId;
    int mIndex;                // field index in scope
};

// Field access   object . field 
// Binary expressin  a.b is replaced with this one!!!
class M1Field : public M1Expr {
public:
    M1Field(M1Expr* aObject, string aId, int aIndex) : M1Expr(op_FLD, NULL) { 
	mId = aId;
	mObject = m1RetainExpr(aObject);
	mIndex = aIndex;
    }
    ~M1Field(void) {
	m1ReleaseExpr(mObject);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mObject);
    }

    void  iterate(M1Expr** aExpr, M1ExprIterator* iter);

    void   print(ostream* os, int indent=0);

    string name(void) { return mId; }
    char*  cname(void) { return (char*) mId.c_str(); }
    string id(void)  { return mId; }

    int    index(void) { return mIndex; }

    M1Expr* object(void) { return mObject; }

    void   load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void   load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void   load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_trg(M1Expr** aExpr);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);

    UData    eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
    bool     eval_trg(CExecutor* aExec);
protected:
    M1Expr* mObject;  // Object scope expression
    string mId;       // The field id
    int mIndex;       // field index in scope
};

//
// Local variable access
//
class M1Local : public M1Expr {
public:
    M1Local(string aId, int aIndex) : M1Expr(op_VAR, NULL) { 
	mId = aId;
	mIndex = aIndex;
    }
    ~M1Local(void) {  }

    void  iterate(M1Expr** aExpr, M1ExprIterator* iter);

    void   print(ostream* os, int indent=0);

    string name(void) { return mId; }
    char*  cname(void) { return (char*) mId.c_str(); }
    string id(void)  { return mId; }

    int    index(void) { return mIndex; }

    void   load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void   load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);

    UData    eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
protected:
    string mId;       // The variable id
    int mIndex;       // index in stack frame
};



class M1Declarator : public M1Element {
public:
    M1Declarator(void) {}
    ~M1Declarator(void) {}

    virtual string  name(void) = 0;
    virtual M1Expr* declExpression(void) = 0;
    virtual CField* lint(CType* aType, CLintInfo* aInfo, int aStorage) = 0;
private:
};

#define m1RetainDeclarator(ptr)      m1Retain(M1Declarator,ptr)
#define m1ReleaseDeclarator(ptr)     m1Release(M1Declarator,ptr)

// field id
class M1IdDeclarator : public M1Declarator {
public:
    M1IdDeclarator(string aID)   { mId = aID; }
    ~M1IdDeclarator(void)        { }

    string name(void)            { return mId; }
    char*  cname(void)           { return (char*) mId.c_str(); }

    M1Expr* declExpression(void) { return m1New(M1Identifier, mId); }

    CField* lint(CType* aType, CLintInfo* aInfo, int aStorage);
    void print(ostream* os, int indent=0);
private:
    string mId;
};

// reference to base type field id (within declaration)
class M1BaseIdDeclarator : public M1Declarator {
public:
    M1BaseIdDeclarator(M1TypeIdentifier* aTypeId, string aID) { 
	mTypeId = m1Retain(M1TypeIdentifier, aTypeId);
	mId   = aID; 
    }
    ~M1BaseIdDeclarator(void) { 
	m1Release(M1TypeIdentifier, mTypeId);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Declarator, mTypeId);
    }

    M1TypeIdentifier* typeId(void) { return mTypeId; }
    string name(void)  { return mId; }
    char*  cname(void) { return (char*) mId.c_str(); }

    M1Expr* declExpression(void) { 
	return m1New(M1BasedIdentifier, mTypeId, mId); 
    }
    
    CField* lint(CType* aType, CLintInfo* aInfo, int aStorage);
    void print(ostream* os, int indent=0);
private:
    M1TypeIdentifier* mTypeId;
    string mId;
};

// <decl> ( '[' [<constant_expr>] ']' ) +
class M1ArrayDeclarator : public M1Declarator {
public:
    M1ArrayDeclarator(M1Declarator* aDecl, M1ExprList* aSizeList) {
	mDecl = m1Retain(M1Declarator,aDecl);
	mSizeList = m1RetainExprList(aSizeList);
    }
    ~M1ArrayDeclarator(void) {
	m1Release(M1Declarator,mDecl);
	m1ReleaseExprList(mSizeList);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Declarator, mDecl, mSizeList);
    }

    string name(void) { return mDecl->name(); }

    M1Expr* declExpression(void) { return mDecl->declExpression(); }

    void addDimension(M1Expr* aDimension) { 
	mSizeList->prependElement(aDimension);
    }

    CField* lint(CType* aType, CLintInfo* aInfo, int aStorage);
    void print(ostream* os, int indent=0);
private:
    M1Declarator* mDecl;    // Base type declaration
    M1ExprList* mSizeList;  // List of array size expressions
};

class M1Declaration : public M1Element {
public:
    M1Declaration(int aStorage) { 
	// Default variable field to public
	if ((aStorage & (Q_PUBLIC|Q_PRIVATE|Q_EXTERN)) == 0)
	    aStorage |= Q_PUBLIC;
	mStorage = aStorage;
    }

    ~M1Declaration(void) {  }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Declaration: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int storage(void) { return mStorage; }
    void setStorage(int aStorageClass) { mStorage = aStorageClass; }

    virtual void lint(CLintInfo* aInfo) {}
private:
    int mStorage;
};


#define m1RetainDeclaration(ptr)      m1Retain(M1Declaration,ptr)
#define m1ReleaseDeclaration(ptr)     m1Release(M1Declaration,ptr)
#define m1SetRetainDeclaration(loc,ptr)  m1SetRetain(M1Declaration,loc,ptr)


class M1DeclarationList : public M1Element {
public:
    M1DeclarationList(void) { }

    ~M1DeclarationList(void) {
	list<M1Declaration*>::iterator iter = mList.begin();
	while(iter != mList.end()) {
	    m1ReleaseDeclaration(*iter);
	    iter++;
	}
    }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "DeclarationList: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }


    int mark(Mark_t aMark) {
	int marked = 0;
	if (mMark != aMark) {
	    list<M1Declaration*>::iterator iter;
	    marked += M1Element::mark(aMark);
	    iter = mList.begin();
	    while(iter != mList.end()) {
		marked += m1Mark((*iter), aMark);
		iter++;
	    }
	}
	return marked;
    }


    void appendElement(M1Declaration* expr) {
	mList.push_back(m1RetainDeclaration(expr));
    }

    void prependElement(M1Declaration* expr) {
	mList.push_front(m1RetainDeclaration(expr));
    }

    void print(ostream* os, int indent);

    // Lint the declarations and create initaialization code
    void lint(M1StatementList* aInitList, CLintInfo* aInfo);

    size_t size(void) { return mList.size(); }

    // Ugly stuff?
    list<M1Declaration*>::iterator begin() { return mList.begin(); }
    list<M1Declaration*>::iterator end()   { return mList.end(); }
    
private:
    list<M1Declaration*> mList;
};

#define m1RetainDeclarationList(ptr)      m1Retain(M1DeclarationList,ptr)
#define m1ReleaseDeclarationList(ptr)     m1Release(M1DeclarationList,ptr)

class M1FieldDeclaration : public M1Declaration {
public:
    M1FieldDeclaration(int aStorage, 
		       M1Specifier* aTypeSpec, 
		       M1Declarator* aDecl,
		       int aInitOp,
		       M1Expr* aExpr) : M1Declaration(aStorage) {
	mTypeSpec    = m1Retain(M1Specifier,aTypeSpec);
	mDecl     = m1RetainDeclarator(aDecl);
	mInitOp   = aInitOp,
	mInit     = m1RetainExpr(aExpr);
    }
    ~M1FieldDeclaration(void) {
	m1Release(M1Specifier,mTypeSpec);
	m1ReleaseDeclarator(mDecl);
	m1ReleaseExpr(mInit);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Declaration, mTypeSpec, mDecl, mInit);
    }


    M1Specifier*      typeSpecifier(void)  { return mTypeSpec; }
    M1Declarator*     typeDeclarator(void) { return mDecl; }
    M1Expr*           initExpression(void) { return mInit; }
    M1Expr*           declExpression(void) { return mDecl->declExpression(); }
    int               initOperator(void)   { return mInitOp; }
    
    void              setInitExpression(M1Expr* aInit) {
	m1SetRetainExpr(&mInit, aInit);
    }

    void lint(CLintInfo* aInfo);
    void print(ostream* os, int indent=0);
private:
    M1Specifier* mTypeSpec;
    M1Declarator* mDecl;
    M1Expr* mInit;
    int mInitOp;
};


// Example:
//   type FooBar { signed x; }
//   type FooBar { signed a = 1; private signed b; }
//   type MyArray byte[4];
//
class M1DeclSpecifier : public M1Declaration {
public:
    M1DeclSpecifier(int aStorage, M1Specifier* aSpec) :
	M1Declaration(aStorage) {
	mSpec = m1Retain(M1Specifier,aSpec);
    }
    ~M1DeclSpecifier(void) {
	m1Release(M1Specifier, mSpec);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Declaration, mSpec);
    }

    void lint(CLintInfo* aInfo);
    void print(ostream* os, int indent=0);
private:
    M1Specifier* mSpec;
};


class M1InterfaceSpecifier : public M1Specifier {
public:
    M1InterfaceSpecifier(M1TypeIdentifier* aType,
			 M1TypeIdentifier* aParentType, 
			 M1DeclarationList* aDeclarations,
			 M1Version* aVersion,
			 Definition_t aDefinitionType=T_TYPE) {
	mType = m1Retain(M1TypeIdentifier,aType);
	mParentType = m1Retain(M1TypeIdentifier,aParentType);
	mDeclarations = m1RetainDeclarationList(aDeclarations);
	mVersion = m1Retain(M1Version, aVersion);
	mDefinitionType = aDefinitionType;
	    
    }
    ~M1InterfaceSpecifier(void) {
	m1Release(M1TypeIdentifier, mType);
	m1Release(M1TypeIdentifier, mParentType);
	m1ReleaseDeclarationList(mDeclarations);
	m1Release(M1Version, mVersion);

    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Specifier, mVersion, mType,
			      mParentType, mDeclarations);
    }

    CType* lint(int aStorage);
    void print(ostream* os, int indent=0);
private:
    Definition_t mDefinitionType;
    M1Version* mVersion;
    M1TypeIdentifier* mType;
    M1TypeIdentifier* mParentType;
    M1DeclarationList* mDeclarations;
};


class M1Statement : public M1Element {
public:
    M1Statement(void)  {}
    ~M1Statement(void) {}

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Statement: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    virtual void lint(CLintInfo* aInfo) = 0;
    virtual M1Status eval(CExecutor* aExec) = 0;
    virtual void load(VmMachineType* aMachine,VmInstructionBuffer* vmb) = 0;
};


#define m1RetainStatement(ptr)         m1Retain(M1Statement,ptr)
#define m1ReleaseStatement(ptr)        m1Release(M1Statement,ptr)
#define m1SetRetainStatement(loc,ptr)  m1SetRetain(M1Statement,loc,ptr)

class M1StatementList : public M1Element {
public:
    M1StatementList(void) { }

    ~M1StatementList(void) {
	list<M1Statement*>::iterator iter = mList.begin();
	while(iter != mList.end()) {
	    m1ReleaseStatement(*iter);
	    iter++;
	}
    }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "StatementList: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int mark(Mark_t aMark) {
	int marked = 0;
	if (mMark != aMark) {
	    list<M1Statement*>::iterator iter;
	    marked += M1Element::mark(aMark);
	    iter = mList.begin();
	    while(iter != mList.end()) {
		marked += m1Mark((*iter), aMark);
		iter++;
	    }
	}
	return marked;
    }


    void appendElement(M1Statement* aStatement) {
	mList.push_back(m1RetainStatement(aStatement));
    }

    void prependElement(M1Statement* aStatement) {
	mList.push_front(m1RetainStatement(aStatement));
    }

    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    size_t size(void) { return mList.size(); }
    // Ugly stuff?
    list<M1Statement*>::iterator begin() { return mList.begin(); }
    list<M1Statement*>::iterator end()   { return mList.end(); }

private:
    list<M1Statement*> mList;
};

#define m1RetainStatementList(ptr)         m1Retain(M1StatementList,ptr)
#define m1ReleaseStatementList(ptr)        m1Release(M1StatementList,ptr)
#define m1SetStatementList(loc,ptr)        m1Set(M1StatementList,loc,ptr)
#define m1SetRetainStatementList(loc,ptr)  m1SetRetain(M1StatementList,loc,ptr)

class M1IfStatement : public M1Statement {
public:
    M1IfStatement(M1Expr* aCondition, M1Statement* aThen, M1Statement* aElse) {
	mCondition = m1RetainExpr(aCondition);
	mThen = m1RetainStatement(aThen);
	mElse = m1RetainStatement(aElse);
    }
    ~M1IfStatement(void) { 
	m1ReleaseExpr(mCondition);
	m1ReleaseStatement(mThen);
	m1ReleaseStatement(mElse);
    }
    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Statement, mCondition,mThen,mElse);
    }
    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    M1Expr* mCondition;
    M1Statement* mThen;
    M1Statement* mElse;  // optional
};


class M1CaseStatement : public M1Statement {
public:
    M1CaseStatement(M1Expr* aExpr, M1Statement* aStat) {
	mExpr = m1RetainExpr(aExpr);
	mStatement = m1RetainStatement(aStat);
    }
    ~M1CaseStatement(void) {
	m1ReleaseExpr(mExpr);
	m1ReleaseStatement(mStatement);	
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mExpr,mStatement);
    }

    void lint(CLintInfo* aInfo);


    M1Expr**     exprp(void) { return &mExpr; }
    M1Statement* statement(void) { return mStatement; }

    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    M1Expr* mExpr;  // MUST be constant (or evaluate to a constant)
    M1Statement* mStatement;
};

class M1DefaultStatement : public M1Statement {
public:
    M1DefaultStatement(M1Statement* aStat)  {
	mStatement = m1RetainStatement(aStat);
    }
    ~M1DefaultStatement(void) {
	m1ReleaseStatement(mStatement);	
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mStatement);
    }

    M1Statement* statement(void) { return mStatement; }

    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    M1Statement* mStatement;
};

class M1JumpStatement : public M1Statement {
public:
    M1JumpStatement(int aWhere, M1Expr* aExpr=NULL)  {
	mWhere = aWhere;
	mExpr  = m1RetainExpr(aExpr);
    }
    ~M1JumpStatement(void) {
	m1ReleaseExpr(mExpr);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mExpr);
    }

    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    int mWhere;
    M1Expr* mExpr;
};



class CSelect {
public:
    CSelect(int aValue, int aPosition) {
	mValue    = aValue;
	mPosition = aPosition;
    }

    ~CSelect(void) {}

    int mValue;     // case value
    int mPosition;  // statement index (-1 signal not code)
private:
};

typedef vector<CSelect>      CSelectVector;
typedef vector<M1Statement*> M1StatementVector;

class M1SwitchStatement : public M1Statement {
public:
    M1SwitchStatement(M1Expr* aValue,
		      M1DeclarationList* aDeclarations,
		      M1StatementList* aStatements) {
	mValue        = m1RetainExpr(aValue);
	mDeclarations = m1RetainDeclarationList(aDeclarations);
	mStatements   = m1RetainStatementList(aStatements);
	mDefaultPos   = -1;
	mInitStart    = -1;
	mInitStop     = -1;
    }

    ~M1SwitchStatement(void) {
	m1ReleaseExpr(mValue);
	m1ReleaseDeclarationList(mDeclarations);
	m1ReleaseStatementList(mStatements);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mValue,mDeclarations,
			      mStatements);
    }

    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    M1Expr* mValue;
    M1DeclarationList* mDeclarations;
    M1StatementList*   mStatements;

    CSelectVector     mSelect;      // Selection vector create by lint
    int               mInitStart;   // Index to constructor statements
    int               mInitStop;     // Index to constructor statements
    int               mDefaultPos;  // Index to default statements
    M1StatementVector mSelectCode;  // Statements index by pos
    int mLocalStackPos;             // start position in stack
    unsigned int mLocalSize;        // size of local area
};

class M1ForEachStatement : public M1Statement {
public:
    M1ForEachStatement(M1Expr* aVar, M1Expr* aRange, M1Statement* aStatement) {
	mVar   = m1RetainExpr(aVar);
	mRange = m1RetainExpr(aRange);
	mStatement = m1RetainStatement(aStatement);
    }
    ~M1ForEachStatement(void) {
	m1ReleaseExpr(mVar);
	m1ReleaseExpr(mRange);
	m1ReleaseStatement(mStatement);
    }
    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mVar,mRange,mStatement);
    }

    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

private:
    M1Expr* mVar;
    M1Expr* mRange;
    M1Statement* mStatement;
};


class M1ExprStatement : public M1Statement {
public:
    M1ExprStatement(M1Expr* aExpr) { mExpr = m1RetainExpr(aExpr); }
    ~M1ExprStatement(void) { m1ReleaseExpr(mExpr); }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mExpr);
    }
    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    M1Expr* mExpr;
};



// Abstract class for all (numeric) types
class M1Constant : public M1Expr {
public:
    M1Constant(int aOp, CType* aType) :
	M1Expr(aOp, aType) {}
    ~M1Constant(void) {}

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Constant: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    UData value(void)           { return mValue; }
    void setValue(UData aValue) { mValue = aValue; }
protected:
    UData mValue;
};


// Byte constant (replaces Numeric after lint phase)
class M1ByteConstant : public M1Constant {
public:
    M1ByteConstant(unsigned char aValue) : M1Constant(op_CNSTb,byte_type()) {
	mValue.b = aValue;
    }
    M1ByteConstant(char* aString) : M1Constant(op_CNSTb,byte_type()) {
	mValue.b = m1NumericToULong(aString, NULL);
    }
    ~M1ByteConstant(void) { }
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void   load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
private:
};

// Char constant (replaces Numeric after lint phase)
class M1CharConstant : public M1Constant {
public:
    M1CharConstant(char aValue) : M1Constant(op_CNSTc,char_type()) {
	mValue.c = aValue;
    }
    M1CharConstant(char* aString) : M1Constant(op_CNSTc,char_type()) {
	mValue.c = m1NumericToLong(aString, NULL);
    }
    ~M1CharConstant(void) { }
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
private:
};


class M1BoolConstant : public M1Constant {
public:
    M1BoolConstant(bool aValue) : M1Constant(op_CNSTt,bool_type()) {
	mValue.t = aValue;
    }
    M1BoolConstant(char* aString) : M1Constant(op_CNSTt,bool_type()) {
	if (strcasecmp(aString, "true") == 0)
	    mValue.t = true;
	else if (strcasecmp(aString, "false") == 0)
	    mValue.t = false;
	else
	    mValue.t = m1NumericToULong(aString, NULL);
    }
    ~M1BoolConstant(void) {}
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_trg(M1Expr** aExpr);
    UData eval(CExecutor* aExec);
    bool  eval_trg(CExecutor* aExec);
private:
};

// Signed constant (replaces Numeric after lint phase)
class M1SignedConstant : public M1Constant {
public:
    M1SignedConstant(signed int aValue) : M1Constant(op_CNSTi,signed_type()) {
	mValue.i = aValue;
    }
    M1SignedConstant(char* aNumeric) : M1Constant(op_CNSTi,signed_type()) {
	mValue.i = m1NumericToLong(aNumeric, NULL);
    }
    ~M1SignedConstant(void) { }
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
private:
};

// Unsigned constant (replaces Numeric after lint phase)
class M1UnsignedConstant : public M1Constant {
public:
    M1UnsignedConstant(unsigned int aValue) : 
	M1Constant(op_CNSTu,unsigned_type()) {
	mValue.u = aValue;
    }

    M1UnsignedConstant(char* aNumeric) : 
	M1Constant(op_CNSTu,unsigned_type()) {
	mValue.u = m1NumericToULong(aNumeric, NULL);
    }

    ~M1UnsignedConstant(void) { }
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
private:
};

// Float constant (replaces Numeric after lint phase)
class M1FloatConstant : public M1Constant {
public:
    M1FloatConstant(float aValue) : M1Constant(op_CNSTf,float_type()) {
	mValue.f = aValue;
    }
    M1FloatConstant(char* aNumeric) : M1Constant(op_CNSTf,float_type()) {
	mValue.f = m1NumericToFloat(aNumeric, NULL);
    }
    ~M1FloatConstant(void) { }
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
private:
};

class M1StringConstant : public M1Constant {
public:
    M1StringConstant(string s) : M1Constant(op_CNSTs,string_type()) {
	CString* lString = m1New(CString, s);
	mValue = URuntime(lString->retainThis());
    }

    M1StringConstant(CString* cs) : M1Constant(op_CNSTs,string_type()) {
	mValue = URuntime(cs->retainThis());
    }

    ~M1StringConstant(void) { 
	mValue.str->releaseThis(); 
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Constant, mValue.str);
    }

    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    UData value(void) { return mValue; }
    
    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
private:
};



class M1Nil : public M1Constant {
public:
    M1Nil(void) : M1Constant(op_CNSTn, any_type()) {
	mValue.o = NULL;
    }
    ~M1Nil(void) {}

    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
private:
};


class M1This : public M1Expr {
public:
    M1This(void) : M1Expr(op_THIS, any_type()) { }
    ~M1This(void) {}

    void print(ostream* os, int indent=0);

    void    bytecode(ostream* os);
    void    load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    CType*  lint(M1Expr** aExpr, CType* restrict);
    UData    eval(CExecutor* aExec);
private:
};


//
// conditional:  cond ? expr1 : expr2
//
class M1TrinaryExpr : public M1Expr {
public:
    M1TrinaryExpr(int opID, M1Expr* aFst, M1Expr* aSnd, M1Expr* aThrd) :
	M1Expr(opID,NULL) {
	mFst = m1RetainExpr(aFst); 
	mSnd = m1RetainExpr(aSnd);
	mThrd = m1RetainExpr(aThrd);
    }
    ~M1TrinaryExpr(void) {
	m1ReleaseExpr(mFst); 
	m1ReleaseExpr(mSnd);
	m1ReleaseExpr(mThrd);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mFst, mSnd, mThrd);
    }

    void     iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void     print(ostream* os, int indent=0);
    M1Expr*  first(void)  { return mFst; }
    M1Expr*  second(void) { return mSnd; }
    M1Expr*  third(void)  { return  mThrd; }

    void  load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void  load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType*   lint(M1Expr** aExpr, CType* restrict);
    CType*   lint_ref(M1Expr** aExpr, CType* restrict);
    UData    eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
private:
    M1Expr* mFst;   // Left tree
    M1Expr* mSnd;   // Right tree
    M1Expr* mThrd;  // Right tree
};

// operators
// multiplicative: '*','/','%'
//        additive: '+','-'
//           shift: LEFT_OP,RIGHT_OP
//      relational: '<','>',LE_OP,GE_OP
//        equality: EQ_OP,NE_OP
//         bitwise: '&', '^', '|', AND_OP, OR_OP
//      assignment: '=', MUL_ASSIGNE, DIV_ASSIGN,MOD_ASSIGN,ADD_ASSIGN,
//                  SUB_ASSIGN,LEFT_ASSIGN,RIGHT_ASSIGN,AND_ASSIGN,XOR_ASSIGN,
//                  OR_ASSIGN
//           expr:  ,
//
class M1BinaryExpr : public M1Expr {
public:
    M1BinaryExpr(int opID, M1Expr* aLeft, M1Expr* aRight) : M1Expr(opID,NULL) {
	mLeft  = m1RetainExpr(aLeft); 
	mRight = m1RetainExpr(aRight);
    }
    ~M1BinaryExpr(void) { 
	m1ReleaseExpr(mLeft);
	m1ReleaseExpr(mRight); 
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mLeft, mRight);
    }

    void  iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void  print(ostream* os, int indent=0);
    void  bytecode(ostream* os);
    void  load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void  load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void  load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    M1Expr* left(void)   { return mLeft; }
    M1Expr* right(void)  { return mRight; }

    M1Expr** leftp(void)   { return &mLeft; }
    M1Expr** rightp(void)  { return &mRight; }


    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);
    CType* lint_trg(M1Expr** aExpr);

    UData eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
    bool     eval_trg(CExecutor* aExec);

private:
    M1Expr* mLeft;
    M1Expr* mRight;
};

// operators:
//    '+', '-', '~', '!'
//
class M1UnaryExpr : public M1Expr {
public:
    M1UnaryExpr(int opID, M1Expr* aUnary) : M1Expr(opID, NULL) {
	mExpr = m1RetainExpr(aUnary);
    }
    ~M1UnaryExpr(void) { m1ReleaseExpr(mExpr); }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mExpr);
    }


    M1Expr* expr(void) { return mExpr; }
    M1Expr** exprp(void) { return &mExpr; }

    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);
    CType* lint_trg(M1Expr** aExpr);

    UData  eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
    bool   eval_trg(CExecutor* aExec);

private:
    M1Expr* mExpr;
};

// Implicit conversion operator, convert between numberic types
class M1CvtExpr : public M1Expr {
public:
    M1CvtExpr(M1CvtCode aCvt, M1Expr* aExpr) : M1Expr(op_CVT,NULL) {
	mCvt    = aCvt;
	mExpr   = m1RetainExpr(aExpr); 
    }

    ~M1CvtExpr(void) {
	m1ReleaseExpr(mExpr);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mExpr);
    }

    M1Expr* expr(void)   { return mExpr; }

    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    UData  eval(CExecutor* aExec);

    void    setCvt(M1CvtCode aCvt) { mCvt = aCvt; }
    M1CvtCode cvt(void)            { return mCvt; }
private:
    M1CvtCode mCvt;
    M1Expr* mExpr;
};



// [:]Type[:Type]( expr )
class M1CastExpr : public M1Expr {
public:
    M1CastExpr(M1TypeIdentifier* aTypeId, M1Expr* aExpr):M1Expr(op_CAST,NULL) {
	mTypeId = m1Retain(M1TypeIdentifier, aTypeId);
	mExpr   = m1RetainExpr(aExpr); 
    }
    ~M1CastExpr(void) { 
	m1Release(M1TypeIdentifier, mTypeId);
	m1ReleaseExpr(mExpr); 
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mTypeId, mExpr);
    }

    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb);
    void load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    M1TypeIdentifier* typeId(void) { return mTypeId; }
    M1Expr* expr(void)   { return mExpr; }
    
    M1Expr** exprp(void)   { return &mExpr; }



    CType* lint(M1Expr** aExpr, CType* restrict);
    CType* lint_ref(M1Expr** aExpr, CType* restrict);
    CType* lint_trg(M1Expr** aExpr);

    UData eval(CExecutor* aExec);
    UAddress eval_ref(CExecutor* aExec);
    bool     eval_trg(CExecutor* aExec);

private:
    M1TypeIdentifier* mTypeId;
    M1Expr* mExpr;
};


class M1CallExpr : public M1Expr {
public:
    M1CallExpr(M1Expr* aFunc, M1ExprList* aArgList) : M1Expr(op_CALL,NULL) {
	mFunc = m1RetainExpr(aFunc);
	mArgs = m1RetainExprList(aArgList);
    }

    ~M1CallExpr(void) {
	m1ReleaseExpr(mFunc);
	m1ReleaseExprList(mArgs);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mFunc, mArgs);
    }


    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);

private:
    M1Expr* mFunc;
    M1ExprList* mArgs;
};

class M1BuiltinExpr : public M1Expr {
public:
    M1BuiltinExpr(UBuiltin* aBf, M1ExprList* aArgList) : M1Expr(op_CALL,NULL) {
	mBf = aBf;
	mArgs = m1RetainExprList(aArgList);
    }
    ~M1BuiltinExpr(void) {
	m1ReleaseExprList(mArgs);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mArgs);
    }

    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);

private:
    UBuiltin* mBf;
    M1ExprList* mArgs;
};

class M1ObjectConstant : public M1Expr {
public:
    M1ObjectConstant(M1TypeIdentifier* aTypeId, M1ExprList* aInitList) :
	M1Expr(op_OBJ, NULL) {
	mTypeId   = m1Retain(M1TypeIdentifier,aTypeId);
	mInitList = m1RetainExprList(aInitList);
    }
    ~M1ObjectConstant(void) { 
	m1Release(M1TypeIdentifier,mTypeId);
	m1ReleaseExprList(mInitList);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mTypeId, mInitList);
    }

    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType*   lint(M1Expr** aExpr, CType* restrict);
    UData    eval(CExecutor* aExec);

private:
    M1TypeIdentifier* mTypeId;
    M1ExprList* mInitList;
};

class M1ArrayConstant : public M1Expr {
public:
    M1ArrayConstant(M1ExprList* aInitList) :
	M1Expr(op_ARR, NULL) {
	mInitList = m1RetainExprList(aInitList);
    }

    ~M1ArrayConstant(void) { 
	m1ReleaseExprList(mInitList);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Expr, mInitList);
    }

    void iterate(M1Expr** aExpr, M1ExprIterator* iter);
    void print(ostream* os, int indent=0);
    void bytecode(ostream* os);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    CType* lint(M1Expr** aExpr, CType* restrict);
    UData eval(CExecutor* aExec);

private:
    M1ExprList* mInitList;
};


class M1Script : public M1Element {
public:

    M1Script(string aId, M1Expr* aTrigger, M1Expr* aWhen, M1Statement* aBody) {
	mId = aId;
	mTrigger = m1RetainExpr(aTrigger);
	mWhen = m1RetainExpr(aWhen);
	mBody = m1RetainStatement(aBody);
	mFrameSize = 0;
    }

    ~M1Script(void) {
	m1ReleaseExpr(mTrigger);
	m1ReleaseExpr(mWhen);
	m1ReleaseStatement(mBody);
    }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "Script: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark, M1Element, mTrigger, mWhen, mBody);
    }


    string name(void) { return mId; }
    M1Statement* body(void) { return mBody; }

    void lint(void);
    void eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);

    unsigned int frameSize(void) { return mFrameSize; }
    unsigned int frameSize(unsigned int aSize) { return mFrameSize=aSize; }

private:
    // mId is either "script" for normal scripts
    // or TypeName for constructor
    // or ~TypeName for destructor
    string  mId;
    M1Expr* mTrigger;
    M1Expr* mWhen;
    M1Statement* mBody;
    unsigned int mFrameSize;
};

#define m1RetainScript(ptr)      m1Retain(M1Script,ptr)
#define m1ReleaseScript(ptr)     m1Release(M1Script,ptr)
#define m1SetRetainScript(loc,ptr)  m1SetRetain(M1Script,loc,ptr)

class M1ScriptList : public M1Element {
public:
    M1ScriptList(void) { }

    ~M1ScriptList(void) {
	list<M1Script*>::iterator iter = mList.begin();
	while(iter != mList.end()) {
	    m1ReleaseScript(*iter);
	    iter++;
	}
    }

    string debugName(void) { 
	char ndata[512];
	sprintf(ndata, "ScriptList: file=%s, line=%d, #%lu 0x%lx:",
		file(), line(), refCount(), (unsigned long) this);
	return string(ndata);
    }

    int mark(Mark_t aMark) {
	int marked = 0;
	if (mMark != aMark) {
	    list<M1Script*>::iterator iter;
	    marked += M1Element::mark(aMark);
	    iter = mList.begin();
	    while(iter != mList.end()) {
		marked += m1Mark((*iter), aMark);
		iter++;
	    }
	}
	return marked;
    }

    void appendElement(M1Script* scr) {
	mList.push_back(m1RetainScript(scr));
    }

    void lint();
    void print(ostream* os, int indent);

    // Ugly stuff?
    list<M1Script*>::iterator begin() { return mList.begin(); }
    list<M1Script*>::iterator end()   { return mList.end(); }

    list<M1Script*>::iterator erase(list<M1Script*>::iterator loc) {
	return mList.erase(loc);
    }
    list<M1Script*>::iterator insert(list<M1Script*>::iterator loc,
				     M1Script* scr) {
	return mList.insert(loc,m1RetainScript(scr)); 
    }
private:
    list<M1Script*> mList;
};

#define m1RetainScriptList(ptr)      m1Retain(M1ScriptList,ptr)
#define m1ReleaseScriptList(ptr)     m1Release(M1ScriptList,ptr)
#define m1SetScriptList(loc,ptr)        m1Set(M1ScriptList,loc,ptr)
#define m1SetRetainScriptList(loc,ptr)  m1SetRetain(M1ScriptList,loc,ptr)

// '{' <declarations> <statements> '}'
class M1CompoundStatement : public M1Statement {
public:
    M1CompoundStatement(M1DeclarationList* aDeclarations,
			M1StatementList* aStatements) {
	mDeclarations = m1RetainDeclarationList(aDeclarations);
	mStatements   = m1RetainStatementList(aStatements);
    }
    ~M1CompoundStatement(void) {
	m1ReleaseDeclarationList(mDeclarations);
	m1ReleaseStatementList(mStatements);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Statement,mDeclarations,
			      mStatements);
    }

    void lint(CLintInfo* aInfo);
    M1Status eval(CExecutor* aExec);
    void print(ostream* os, int indent=0);
    void load(VmMachineType* aMachine,VmInstructionBuffer* vmb);
private:
    M1DeclarationList* mDeclarations;
    M1StatementList* mStatements;
    int mLocalStackPos;          // start position in stack
    unsigned int mLocalSize;     // size of local area
};

//
// type [ <id> ]  [':' <parent-type> ]
//     '{' <declarations> <scripts> <start-statments> '}'
//
class M1TypeSpecifier : public M1Specifier {
public:
    M1TypeSpecifier(M1TypeIdentifier* aType,
		    M1TypeIdentifier* aParentType, 
		    M1DeclarationList* aDeclarations,
		    M1ScriptList* aScriptList,
		    M1StatementList* aStatementList,
		    M1Version* aVersion,
		    Definition_t aDefinitionType = T_TYPE) {
	mType = m1Retain(M1TypeIdentifier,aType);
	mParentType = m1Retain(M1TypeIdentifier,aParentType);
	mDeclarations = m1RetainDeclarationList(aDeclarations);
	mScriptList = m1RetainScriptList(aScriptList);
	mStatementList = m1RetainStatementList(aStatementList);
	mVersion = m1Retain(M1Version, aVersion);
	mDefinitionType = aDefinitionType;
    }
    ~M1TypeSpecifier(void) {
	m1Release(M1TypeIdentifier, mType);
	m1Release(M1TypeIdentifier, mParentType);
	m1ReleaseDeclarationList(mDeclarations);
	m1ReleaseScriptList(mScriptList);
	m1ReleaseStatementList(mStatementList);
	m1Release(M1Version, mVersion);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Specifier,mVersion,mType,
			      mParentType,mDeclarations,mScriptList,
			      mStatementList);
    }

    CType* lint(int aStorage);

    void print(ostream* os, int indent=0);
private:
    Definition_t mDefinitionType;
    M1Version* mVersion;
    M1TypeIdentifier* mType;
    M1TypeIdentifier* mParentType;
    M1DeclarationList* mDeclarations;
    M1ScriptList* mScriptList;
    M1StatementList* mStatementList; // Start code
};

//
// typedef <type-decl> <id> 
//
class M1TypeDefSpecifier : public M1Specifier {
public:
    M1TypeDefSpecifier(M1TypeIdentifier* aType,
		       M1Specifier* aSpec) {
	mType = m1Retain(M1TypeIdentifier,aType);
	mSpec = m1Retain(M1TypeSpecifier,aSpec);
    }
    ~M1TypeDefSpecifier(void) {
	m1Release(M1TypeIdentifier, mType);
	m1Release(M1TypeSpecifier,  mSpec);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,M1Specifier,mType,mSpec);
    }

    CType* lint(int aStorage);

    void print(ostream* os, int indent=0);
private:
    M1TypeIdentifier* mType;
    M1Specifier* mSpec;
};

class VmEvalArgs : public CArgs {
public:
    VmEvalArgs(CExecutable* aCreator, CBaseObject* aGlobal,
	       M1ExprList* aInitList) :
	CArgs(aCreator, aGlobal) {
	mInitList = m1RetainExprList(aInitList);
    }

    ~VmEvalArgs(void) {
	m1ReleaseExprList(mInitList);
    }

    int mark(Mark_t aMark) {
	return m1MarkAndCheck(aMark,CArgs,mInitList);
    }

    void run(CExecutor* aExec, CObject* obj);

private:
    M1ExprList* mInitList;
};


// Interpreted version of VmObjectType
class VmEvalType : public VmObjectType {
public:
    VmEvalType(string aName,Definition_t aDef) : VmObjectType(aName,aDef) {
	mPreScript = NULL; 
	mPostScript = NULL; 
	mDefaults = NULL;
	mConstructor = NULL;
	mDestructor = NULL;
    }

    VmEvalType(Definition_t aDef) : VmObjectType(aDef) {
	mPreScript = NULL; 
	mPostScript = NULL; 
	mDefaults = NULL;
	mConstructor = NULL;
	mDestructor = NULL;
    }

    ~VmEvalType(void) {
	m1ReleaseScriptList(mPreScript);
	m1ReleaseScriptList(mPostScript);
	m1ReleaseStatementList(mDefaults);
	m1ReleaseStatementList(mConstructor);
	m1ReleaseStatementList(mDestructor);
    }

    int mark(Mark_t aMark);

    void init(CExecutor* aExec,CBaseObject* aObject);
    void defaults(CExecutor* aExec,CBaseObject* aObject);
    void construct(CExecutor* aExec, CBaseObject* aObject);
    void destruct(CExecutor* aExec, CBaseObject* aObject);
    UData produce(CExecutor* aExec, CBaseType* aBase, CArgs* args);
    // Objects of this type is not built in
    bool   isVmType()   { return false; }

    void execute(CExecutor* aExec, CExecutable* aObject);


    void setPreScript(M1ScriptList* aScriptList) {
	m1SetRetainScriptList(&mPreScript, aScriptList);
    }

    void setPostScript(M1ScriptList* aScriptList) {
	m1SetRetainScriptList(&mPostScript, aScriptList);
    }

    M1ScriptList* getPreScript(void) { return mPreScript; }

    M1ScriptList* getPostScript(void) { return mPostScript; }


    void setConstructor(M1StatementList* aConstructor) {
	m1SetRetainStatementList(&mConstructor, aConstructor);
    }

    M1StatementList* getConstructor(void) { return mConstructor; }

    void setDestructor(M1StatementList* aDestructor) {
	m1SetRetainStatementList(&mDestructor, aDestructor);
    }

    M1StatementList* getDestructor(void) { return mDestructor; }

    void setDefaults(M1StatementList* aDefaults) {
	m1SetRetainStatementList(&mDefaults, aDefaults);
    }

    M1StatementList* getDefaults(void) { return mDefaults; }

private:

    M1ScriptList* mPreScript;
    M1ScriptList* mPostScript;
    M1StatementList* mDefaults;
    M1StatementList* mConstructor;
    M1StatementList* mDestructor;
};


#endif
