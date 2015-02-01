//
// M1 Evaluator
//

#include <stdlib.h>
#include <ctype.h>
#include <iostream>
#include "m1c.hh"
#include "m1_parse.hh"

using namespace std;

UAddress null = { NULL, 0, TRIGGER_AUTO};

static inline CType* unfoldType(CType* a)
{
    if ((a!=NULL) && isAType(CEventType*, a))
	return ((CEventType*)a)->baseType();
    return a;
}

/* Convert number types */
UData m1ReadConvert(UData r, CType* from)
{
    if (isAType(CEventType*, from))
	from = ((CEventType*)from)->baseType();
    switch(from->typeTag()) {
    case M1TYPE_BOOL:  r.i = r.t; break;
    case M1TYPE_CHAR:  r.i = r.c; break;
    case M1TYPE_BYTE:  r.u = r.b; break;
    default: break;
    }
    return r;
}

//
// Convert using conversion code
//
UData m1Cvt(M1CvtCode code, UData v)
{
    UData r;

    switch(code) {
    case M1CVT_bc: r.c = v.b; break;
    case M1CVT_bt: r.t = v.b; break;
    case M1CVT_bi: r.u = v.b; break;
    case M1CVT_bu: r.u = v.b; break;
    case M1CVT_bf: r.f = (float) v.b; break;

    case M1CVT_cb: r.b = v.c; break;
    case M1CVT_ct: r.t = v.c; break;
    case M1CVT_ci: r.i = v.c; break;
    case M1CVT_cu: r.u = v.c; break;
    case M1CVT_cf: r.f = (float) v.c; break;

    case M1CVT_tb: r.b = v.t; break;
    case M1CVT_tc: r.c = v.t; break;
    case M1CVT_ti: r.i = v.t; break;
    case M1CVT_tu: r.u = v.t; break;
    case M1CVT_tf: r.f = (float) v.t; break;
	
    case M1CVT_ib: r.b = v.i; break;
    case M1CVT_ic: r.c = v.i; break;
    case M1CVT_it: r.t = v.i; break;
    case M1CVT_iu: r.u = v.i; break;
    case M1CVT_if: r.f = (float) v.i; break;

    case M1CVT_ub: r.b = v.u; break;
    case M1CVT_uc: r.c = v.u; break;
    case M1CVT_ut: r.t = v.u; break;
    case M1CVT_ui: r.i = v.u; break;
    case M1CVT_uf: r.f = (float) v.u; break;

    case M1CVT_fb: r.b = (unsigned char) v.f; break;
    case M1CVT_fc: r.c = (char) v.f; break;
    case M1CVT_ft: r.t = (bool) v.f; break;
    case M1CVT_fi: r.i = (int) v.f; break;
    case M1CVT_fu: r.u = (unsigned int) v.f; break;
    default: r = v; break;
    }
    return r;
}

static inline UData addressAt(UAddress a)
{
    return a.obj->at(a.index);
}

static inline void addressPut(CExecutor* aExec, M1Element* elem, 
			      UAddress a, UData v)
{
#ifdef DEBUG
    aExec->setEventSource(elem->file(), elem->line());
#endif
    a.obj->put(aExec, a.index, v, a.trig);
}

static inline void addressCopy(CExecutor* aExec, M1Expr* elem,
			       UAddress a, UData v)
{
    CType* dstType = elem->type();
#ifdef DEBUG
    aExec->setEventSource(elem->file(), elem->line());
#endif
    a.obj->copy(aExec, dstType, a.index, v, a.trig);
}

//
// VmEvalType. 
//
int VmEvalType::mark(Mark_t aMark)
{
    return m1MarkAndCheck(aMark,VmObjectType,
			  mPreScript, mPostScript, mDefaults,
			  mConstructor, mDestructor);
}

void VmEvalType::init(CExecutor* aExec,CBaseObject* obj)
{
    DBGFMT_EVAL("VmEvalType::%s::init(%p)",cname(),obj);

    VmObjectType::init(aExec,obj);
}


void VmEvalType::defaults(CExecutor* aExec,CBaseObject* obj)
{
    DBGFMT_EVAL("VmEvalType::%s::defaults(%p)",cname(),obj);

    VmObjectType::defaults(aExec, obj);

    // run script contructor code
    if (mDefaults) {
	CBaseObject* savedGlobal;
	aExec->activate_global(global(),&savedGlobal);
	mDefaults->eval(aExec);
	aExec->restore_global(savedGlobal);
    }
}

void VmEvalType::construct(CExecutor* aExec,CBaseObject* obj)
{
    DBGFMT_EVAL("VmEvalType::%s::construct(%p)",cname(),obj);

    VmObjectType::construct(aExec, obj);

    // run script contructor code
    if (mConstructor) {
	CBaseObject* savedGlobal;
	aExec->activate_global(global(),&savedGlobal);
	DBGFMT_EVAL("VmEvalType::construct() alloc frameSize=%d", mConstructorFrameSize);
	aExec->allocFrame(mConstructorFrameSize);
	mConstructor->eval(aExec);
	aExec->deallocFrame();
	DBGFMT_EVAL("VmEvalType::construct() dealloc frameSize=%d", mConstructorFrameSize);
	aExec->restore_global(savedGlobal); 
    }
    aExec->addComponent((CExecutable*)obj);
}

void VmEvalType::destruct(CExecutor* aExec,CBaseObject* obj)
{
    ExecutorState state;

    DBGFMT_EVAL("VmEvalType::%s::destruct(%p)",cname(),obj);

    aExec->save(&state);
    aExec->activate((CExecutable*) obj);
    if (mDestructor) {
	CBaseObject* savedGlobal;
	aExec->activate_global(global(),&savedGlobal);
	DBGFMT_EVAL("VmEvalType::destruct() alloc frameSize=%d", mDestructorFrameSize);
	aExec->allocFrame(mDestructorFrameSize);
	mDestructor->eval(aExec);
	aExec->deallocFrame();
	DBGFMT_EVAL("VmEvalType::destruct() dealloc frameSize=%d", mDestructorFrameSize);
    }
    VmObjectType::destruct(aExec, obj);
    aExec->restore(&state);
}


UData VmEvalType::produce(CExecutor* aExec,CBaseType* aBase, CArgs* args)
{
    ExecutorState state;
    CBaseType* produceType = this;
    CBaseType* initType = aBase ? aBase : this;
    UData r;
    CEvent* clearUntil;

    aExec->save(&state);

    // Find the base type to use when producing the object 
    while(!produceType->isVmType())
	produceType = (CBaseType*) produceType->parentType();

    r = produceType->produce(aExec, initType, NULL);
    aExec->activate((CExecutable*) r.o);
    initType->init(aExec, (CBaseObject*) r.o);
    clearUntil =  ((CExecutable*)r.o)->updatedEvents();
    initType->defaults(aExec, (CBaseObject*) r.o);
    ((CExecutable*) r.o)->clearAllUpdated(aExec, clearUntil);
    if (args) args->run(aExec, r.o); // run constructor args if present
    initType->construct(aExec, (CBaseObject*) r.o);
    if (mDefinitionType == T_APPLICATION) {
	m1_applicationList.push_back((VmObject*) r.o);
	r.o->retainThis();
    }
    aExec->restore(&state);
    return r;
}

void VmEvalType::execute(CExecutor* aExec, CExecutable* aObject)
{
    CBaseObject* savedGlobal;
    CType* pt;
    DBGFMT_EVAL("VmEvalType::execute() %s", cname());

    aExec->activate_global(global(), &savedGlobal);

    if (mPreScript)  {
	list<M1Script*>::iterator i;

	for (i = mPreScript->begin(); i != mPreScript->end(); i++)
	    (*i)->eval(aExec);
    }

    if ((pt = parentType())) {
	if (pt->isExecutableType())
	    ((CExecutableType*) pt)->execute(aExec, aObject);
    }

    if (mPostScript)  {
	list<M1Script*>::iterator i;

	for (i = mPostScript->begin(); i != mPostScript->end(); i++) 
	    (*i)->eval(aExec);
    }

    aExec->restore_global(savedGlobal);
}


// M1Expr:
//    handle the default case and print error
//
UData M1Expr::eval(CExecutor* aExec) 
{
    ostringstream oStream;

    print(&oStream, 0);
    ERRFMT("M1Expr::eval: error expression not evaluated %s",
	   oStream.str().c_str());
    return nil; 
}

bool M1Expr::eval_trg(CExecutor* aExec) 
{
    ostringstream oStream;

    print(&oStream, 0);
    ERRFMT("M1Expr::eval_trg: error expression not evaluated %s",
	   oStream.str().c_str());
    return false;
}

UAddress M1Expr::eval_ref(CExecutor* aExec)
{
    ostringstream oStream;

    print(&oStream, 0);
    ERRFMT("M1Expr::eval_ref: error expression [%s] not evaluated",
	   oStream.str().c_str());
    throw M1StatusError;
    return null;
}

//
// M1Identifier
//

UData M1Identifier::eval(CExecutor* aExec)
{
    UData value;
    CBaseObject* obj;

    DBGFMT_EVAL("M1Identifier::eval() %s", cname());

    if ((obj = aExec->scope(mLevel)) == NULL) {
	m1BreakHere(file(), line(), "nil scope exception");
	throw M1StatusError;
    }
    value = obj->at(mIndex);

    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf(" [");
	type()->print(&cout, value);
	printf("]\n");
    }
    return m1ReadConvert(value, type());
}

bool M1Identifier::eval_trg(CExecutor* aExec)
{
    CEvent* evt;
    CBaseObject* obj;

    DBGFMT_EVAL("M1Identifier::eval_trg() %s", cname());
    
    // NOTE: trigger expression can not contain local varaibles
    if ((obj = aExec->scope(mLevel)) == NULL) {
	m1BreakHere(file(), line(), "nil scope exception");
	throw M1StatusError;
    }

    if ((evt = obj->eventAt(mIndex)) == NULL) {
	m1BreakHere(file(), line(), "nil event exception");
	throw M1StatusError;
    }
    DBGFMT_EVAL("M1Identifier::eval_trg() %s[%d:%d] = %s", cname(),
		mLevel, mIndex,
		evt->updated() ? "UPDATED" : "NOT UPDATED");
    return evt->updated();
}

UAddress M1Identifier::eval_ref(CExecutor* aExec)
{
    UAddress a;

    DBGFMT_EVAL("M1Identifier::eval_ref() %s", cname());

    if ((a.obj = aExec->scope(mLevel)) == NULL) {
	m1BreakHere(file(), line(), "nil scope exception");
	throw M1StatusError;
    }
    a.index = mIndex;
    a.trig  = TRIGGER_AUTO;
    return a;
}

UData M1Index::eval(CExecutor* aExec)
{
    return USigned(mIndex);
}

UData M1BasedIdentifier::eval(CExecutor* aExec)
{
    return (aExec->current())->at(mIndex);
}

UAddress M1BasedIdentifier::eval_ref(CExecutor* aExec)
{
    UAddress a;

    a.obj   = aExec->current();
    a.index = mIndex;
    a.trig  = TRIGGER_AUTO;
    return a;
}

UData M1Field::eval(CExecutor* aExec)
{
    UData object = mObject->eval(aExec);
    UData value;

    if (object.o == NULL) {
	m1BreakHere(file(), line(), "nil field access exception");
	throw M1StatusError;
    }
    value = object.o->at(mIndex);
    DBGFMT_EVAL("M1Field:eval: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	print(&cout, 0);
	printf("= ");
	type()->print(&cout, value);
	printf("\n");
    }
    return m1ReadConvert(value, type());
}

UAddress M1Field::eval_ref(CExecutor* aExec)
{
    UAddress a;
    UData    object = mObject->eval(aExec);

    if (object.o == NULL) {
	m1BreakHere(file(), line(), "nil access exception");
	throw M1StatusError;
    }
    a.obj   = object.o;
    a.index = mIndex;
    a.trig  = TRIGGER_AUTO;
    return a;
}

bool M1Field::eval_trg(CExecutor* aExec)
{
    CEvent* evt;
    UData object = mObject->eval(aExec);

    if (object.o == NULL) {
	m1BreakHere(file(), line(), "nil access exception");
	throw M1StatusError;
    }
    if ((evt = object.o->eventAt(mIndex)) == NULL) {
	m1BreakHere(file(), line(), "nil event exception");
	throw M1StatusError;
    }
    return evt->updated();    
}

//
// Local variables
//
UData M1Local::eval(CExecutor* aExec)
{
    UData* frame = aExec->framePtr();
    return frame[mIndex];
}

UAddress M1Local::eval_ref(CExecutor* aExec)
{
    UAddress a;
    // FIXME
    return a;
}

//
// type of numeric constant MUST be known at this point
// either signed|unsigned|byte|char|float|double|quad|uquad
// signed|unsigned|char may have 'a' notation '\x1234' 'abcd' '\n'
//

UData M1BoolConstant::eval(CExecutor* aExec)
{
    return USigned(mValue.t);
}

bool M1BoolConstant::eval_trg(CExecutor* aExec)
{
    return mValue.t;
}

UData M1SignedConstant::eval(CExecutor* aExec)
{
    return mValue;
}

UData M1UnsignedConstant::eval(CExecutor* aExec)
{
    return mValue;
}

UData M1CharConstant::eval(CExecutor* aExec)
{
    return USigned(mValue.c);
}

UData M1ByteConstant::eval(CExecutor* aExec)
{
    return UUnsigned(mValue.b);
}

UData M1FloatConstant::eval(CExecutor* aExec)
{
    return mValue;
}

UData M1StringConstant::eval(CExecutor* aExec)
{
    return mValue;
}

UData M1Nil::eval(CExecutor* aExec)
{
    return nil;
}

UAddress M1Nil::eval_ref(CExecutor* aExec)
{
    return null;
}

UData M1This::eval(CExecutor* aExec)
{
    return UObject(aExec->current());
}

UData M1TrinaryExpr::eval(CExecutor* aExec)
{
    UData v;

    switch(op()) {
    case op_COND:
	v = mFst->eval(aExec);
	if (v.b)
	    return mSnd->eval(aExec);
	else
	    return mThrd->eval(aExec);
	break;

    case op_PROG2:
	if (mFst)  mFst->eval(aExec);
	v = mSnd->eval(aExec);
	if (mThrd) mThrd->eval(aExec);
	return v;
	
    default:
	ERRFMT("M1TrinayExpr::eval operator %s not handled",
	       formatOperator(op()).c_str());
	return nil;
    }
}

UAddress M1TrinaryExpr::eval_ref(CExecutor* aExec)
{
    UData v;
    switch(op()) {
    case op_COND:
	v = mFst->eval(aExec);
	if (v.b)
	    return mSnd->eval_ref(aExec);
	else
	    return mThrd->eval_ref(aExec);
	break;
    default:
	ERRFMT("M1TrinayExpr::eval_ref operator %s not handled",
	       formatOperator(op()).c_str());
	return null;
    }
}

#define EVAL_BINARY(op,r,ln,rn,sel) do {	 \
	UData lv = (ln)->eval(aExec);		 \
	UData rv = (rn)->eval(aExec);		 \
	r.sel = lv.sel op rv.sel;		 \
    } while(0)

#define EVAL_UPDATE(op,r,ln,rn,sel) do {			\
	UAddress a = nullCheck((ln)->eval_ref(aExec));		\
	UData lv   = addressAt(a);				\
	r = (rn)->eval(aExec);					\
	lv.sel op r.sel;					\
	addressPut(aExec,(ln), a, lv);				\
	r = lv;							\
    } while(0)


UData M1BinaryExpr::eval(CExecutor* aExec)
{
    UData r;

    DBGFMT_EVAL("M1BinaryExpr:eval: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	print(&cout, 0);
	printf("\n");
    }
    switch(op()) {
    case op_PUTs:
    case op_PUTo:
    case op_PUTf:
    case op_PUTi:
    case op_PUTu: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
        // No eval type conversion needed!
	r = mRight->eval(aExec);  
	addressPut(aExec, mLeft, a, r);
	break;
    }

    case op_PUTc: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	r = mRight->eval(aExec);
	r.c = r.i;
	addressPut(aExec, mLeft, a, r);
	break;
    }

    case op_PUTt: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	r = mRight->eval(aExec);
	r.t = r.i;
	addressPut(aExec, mLeft, a, r);
	break;
    }

    case op_PUTb: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	r = mRight->eval(aExec);
	r.b = r.u;
	addressPut(aExec, mLeft, a, r);
	break;
    }

    case op_PUT: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	r = mRight->eval(aExec);
	addressPut(aExec, mLeft, a, r);
	break;
    }
    case op_COPY: {  /* :=  copy object */
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	r = mRight->eval(aExec);
	addressCopy(aExec, mLeft, a, r);
	break;
    }

    case op_MULPUTi: EVAL_UPDATE(*=,r,mLeft,mRight,i); break;
    case op_MULPUTu: EVAL_UPDATE(*=,r,mLeft,mRight,u); break;
    case op_MULPUTb: EVAL_UPDATE(*=,r,mLeft,mRight,b); break;
    case op_MULPUTc: EVAL_UPDATE(*=,r,mLeft,mRight,c); break;
    case op_MULPUTt: EVAL_UPDATE(*=,r,mLeft,mRight,t); break;
    case op_MULPUTf: EVAL_UPDATE(*=,r,mLeft,mRight,f); break;

    case op_DIVPUTi: EVAL_UPDATE(/=,r,mLeft,mRight,i); break;
    case op_DIVPUTu: EVAL_UPDATE(/=,r,mLeft,mRight,u); break;
    case op_DIVPUTb: EVAL_UPDATE(/=,r,mLeft,mRight,b); break;
    case op_DIVPUTc: EVAL_UPDATE(/=,r,mLeft,mRight,c); break;
    case op_DIVPUTt: EVAL_UPDATE(/=,r,mLeft,mRight,t); break;
    case op_DIVPUTf: EVAL_UPDATE(/=,r,mLeft,mRight,f); break;

    case op_ADDPUTi: EVAL_UPDATE(+=,r,mLeft,mRight,i); break;
    case op_ADDPUTu: EVAL_UPDATE(+=,r,mLeft,mRight,u); break;
    case op_ADDPUTb: EVAL_UPDATE(+=,r,mLeft,mRight,b); break;
    case op_ADDPUTc: EVAL_UPDATE(+=,r,mLeft,mRight,c); break;
    case op_ADDPUTt: EVAL_UPDATE(+=,r,mLeft,mRight,t); break;
    case op_ADDPUTf: EVAL_UPDATE(+=,r,mLeft,mRight,f); break;
    case op_ADDPUTs: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.str->set(lv.str->str() + r.str->str());
	r = lv;
	break;
    }
    case op_ADDPUTsc: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.str->set(lv.str->str() + r.c);
	r = lv;
	break;
    }

    case op_ADDPUTa: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.arr->append(aExec, r);
	r = lv;
	break;
    }
    case op_ADDPUTae: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.arr->elementAppend(aExec, r);
	r = lv;
	break;
    }

    case op_SUBPUTi: EVAL_UPDATE(-=,r,mLeft,mRight,i); break;
    case op_SUBPUTu: EVAL_UPDATE(-=,r,mLeft,mRight,u); break;
    case op_SUBPUTb: EVAL_UPDATE(-=,r,mLeft,mRight,b); break;
    case op_SUBPUTc: EVAL_UPDATE(-=,r,mLeft,mRight,c); break;
    case op_SUBPUTt: EVAL_UPDATE(-=,r,mLeft,mRight,t); break;
    case op_SUBPUTf: EVAL_UPDATE(-=,r,mLeft,mRight,f); break;
    case op_SUBPUTs: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.str->erase(r);
	r = lv;
	break;
    }
    case op_SUBPUTsc: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.str->elementErase(r);
	r = lv;
	break;
    }

    case op_SUBPUTa: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.arr->erase(aExec, r);
	r = lv;
	break;
    }

    case op_SUBPUTae: {
	UAddress a = nullCheck(mLeft->eval_ref(aExec));
	UData lv   = addressAt(a);
	r = mRight->eval(aExec);
	lv.arr->elementErase(aExec, r);
	r = lv;
	break;
    }

    case op_REMPUTi: EVAL_UPDATE(%=,r,mLeft,mRight,i); break;
    case op_REMPUTu: EVAL_UPDATE(%=,r,mLeft,mRight,u); break;
    case op_REMPUTb: EVAL_UPDATE(%=,r,mLeft,mRight,b); break;
    case op_REMPUTc: EVAL_UPDATE(%=,r,mLeft,mRight,c); break;
    case op_REMPUTt: EVAL_UPDATE(%=,r,mLeft,mRight,t); break;

    case op_BANDPUTi: EVAL_UPDATE(&=,r,mLeft,mRight,i); break;
    case op_BANDPUTu: EVAL_UPDATE(&=,r,mLeft,mRight,u); break;
    case op_BANDPUTb: EVAL_UPDATE(&=,r,mLeft,mRight,b); break;
    case op_BANDPUTc: EVAL_UPDATE(&=,r,mLeft,mRight,c); break;
    case op_BANDPUTt: EVAL_UPDATE(&=,r,mLeft,mRight,t); break;

    case op_BORPUTi: EVAL_UPDATE(|=,r,mLeft,mRight,i); break;
    case op_BORPUTu: EVAL_UPDATE(|=,r,mLeft,mRight,u); break;
    case op_BORPUTb: EVAL_UPDATE(|=,r,mLeft,mRight,b); break;
    case op_BORPUTc: EVAL_UPDATE(|=,r,mLeft,mRight,c); break;
    case op_BORPUTt: EVAL_UPDATE(|=,r,mLeft,mRight,t); break;

    case op_BXORPUTi: EVAL_UPDATE(^=,r,mLeft,mRight,i); break;
    case op_BXORPUTu: EVAL_UPDATE(^=,r,mLeft,mRight,u); break;
    case op_BXORPUTb: EVAL_UPDATE(^=,r,mLeft,mRight,b); break;
    case op_BXORPUTc: EVAL_UPDATE(^=,r,mLeft,mRight,c); break;
    case op_BXORPUTt: EVAL_UPDATE(^=,r,mLeft,mRight,t); break;

    case op_BSLPUTi: EVAL_UPDATE(<<=,r,mLeft,mRight,i); break;
    case op_BSLPUTu: EVAL_UPDATE(<<=,r,mLeft,mRight,u); break;
    case op_BSLPUTb: EVAL_UPDATE(<<=,r,mLeft,mRight,b); break;
    case op_BSLPUTc: EVAL_UPDATE(<<=,r,mLeft,mRight,c); break;
    case op_BSLPUTt: EVAL_UPDATE(<<=,r,mLeft,mRight,t); break;


    case op_BSRPUTi: EVAL_UPDATE(>>=,r,mLeft,mRight,i); break;
    case op_BSRPUTu: EVAL_UPDATE(>>=,r,mLeft,mRight,u); break;
    case op_BSRPUTb: EVAL_UPDATE(>>=,r,mLeft,mRight,b); break;
    case op_BSRPUTc: EVAL_UPDATE(>>=,r,mLeft,mRight,c); break;
    case op_BSRPUTt: EVAL_UPDATE(>>=,r,mLeft,mRight,t); break;

    case op_CNCT: {
	bool res;
	UAddress al = nullCheck(mLeft->eval_ref(aExec));
	UAddress ar = mRight->eval_ref(aExec); // nil == disconnect!!!
	res = ((CExecutable*) al.obj)->connect(al.index, ar.obj, ar.index);
	if ((ar.obj == NULL) && (ar.index == 0)) {
	    DBGFMT_EVAL("expr: disconnected (%d) %s:%s",
		   res,
		   ((al.obj)->type())->cname(),
		   ((CBaseType*)(al.obj)->type())->fieldName(al.index));
	}
	else {
	    DBGFMT_EVAL("expr: connected (%d) %s:%s with %s:%s",
		   res,
		   ((al.obj)->type())->cname(),
		   ((CBaseType*)(al.obj)->type())->fieldName(al.index),
		   ((ar.obj)->type())->cname(),
		   ((CBaseType*)(ar.obj->type()))->fieldName(ar.index));
	}
	r = nil;
	break;
    }

    case op_ADDi: EVAL_BINARY(+,r,mLeft,mRight,i); break;
    case op_ADDu: EVAL_BINARY(+,r,mLeft,mRight,u); break;
    case op_ADDf: EVAL_BINARY(+,r,mLeft,mRight,f); break;
    case op_ADDs: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	r.str = m1New(CString, lv.str->str() + rv.str->str());
	break;
    }
    case op_ADDsc: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	r.str = m1New(CString, lv.str->str() + rv.c);
	break;
    }
    case op_ADDcs: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	r.str = m1New(CString, lv.c + rv.str->str());
	break;
    }

    case op_ADDa: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	CType* t = lv.arr->type();
	r = t->produce(aExec);
	t->shallowCopy(aExec, lv, &r);
	r.arr->append(aExec, rv);
	break;
    }
    case op_ADDae: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	CType* t = lv.arr->type();
	r = t->produce(aExec);
	t->shallowCopy(aExec, lv, &r);
	r.arr->elementAppend(aExec, rv);
	break;
    }
    case op_ADDea: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	CType* t = rv.arr->type();
	r = t->produce(aExec);
	t->shallowCopy(aExec, rv, &r);
	r.arr->elementPrepend(aExec, lv);
	break;
    }


    case op_SUBi: EVAL_BINARY(-,r,mLeft,mRight,i); break;
    case op_SUBu: EVAL_BINARY(-,r,mLeft,mRight,u); break;
    case op_SUBf: EVAL_BINARY(-,r,mLeft,mRight,f); break;
    case op_SUBs: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	r.str = m1New(CString, lv.str->str());
	r.str->erase(rv);
	break;
    }
    case op_SUBsc: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	r.str = m1New(CString, lv.str->str());
	r.str->elementErase(rv);
	break;
    }
    case op_SUBa: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	CType* t = lv.arr->type();
	r = t->produce(aExec);
	t->shallowCopy(aExec, lv, &r);
	r.arr->erase(aExec, rv);
	break;
    }
    case op_SUBae: {
	UData lv = mLeft->eval(aExec);
	UData rv = mRight->eval(aExec);
	CType* t = lv.arr->type();
	r = t->produce(aExec);
	t->shallowCopy(aExec, lv, &r);
	r.arr->elementErase(aExec, rv);
	break;
    }


    case op_MULi: EVAL_BINARY(*,r,mLeft,mRight,i); break;
    case op_MULu: EVAL_BINARY(*,r,mLeft,mRight,u); break;
    case op_MULf: EVAL_BINARY(*,r,mLeft,mRight,f); break;

    case op_DIVi: EVAL_BINARY(/,r,mLeft,mRight,i); break;
    case op_DIVu: EVAL_BINARY(/,r,mLeft,mRight,u); break;
    case op_DIVf: EVAL_BINARY(/,r,mLeft,mRight,f); break;

    case op_REMi: EVAL_BINARY(%,r,mLeft,mRight,i); break;
    case op_REMu: EVAL_BINARY(%,r,mLeft,mRight,u); break;

    case op_BSLu: EVAL_BINARY(<<,r,mLeft,mRight,u); break;
    case op_BSLi: EVAL_BINARY(<<,r,mLeft,mRight,i); break;

    case op_BSRu: EVAL_BINARY(>>,r,mLeft,mRight,u); break;
    case op_BSRi: EVAL_BINARY(>>,r,mLeft,mRight,i); break;

    case op_BANDu: EVAL_BINARY(&,r,mLeft,mRight,u); break;

    case op_BORu: EVAL_BINARY(|,r,mLeft,mRight,u); break;

    case op_BXORu: EVAL_BINARY(^,r,mLeft,mRight,u); break;

    case op_ORi:
	r = mLeft->eval(aExec);
	if (!r.i) r = mRight->eval(aExec);
	if (r.i) r.i = 1;
	break;

    case op_ANDi:
	r = mLeft->eval(aExec);
	if (r.i) r = mRight->eval(aExec);
	if (r.i) r.i = 1;
	break;

    case op_CMPu: {
	UData a = mLeft->eval(aExec);
	UData b = mRight->eval(aExec);
	r.i = unsigned_type()->compare(a, b);
	break;
    }
    case op_CMPi: {
	UData a = mLeft->eval(aExec);
	UData b = mRight->eval(aExec);
	r.i = signed_type()->compare(a, b);
	break;
    }

    case op_CMPf: {
	UData a = mLeft->eval(aExec);
	UData b = mRight->eval(aExec);
	r.i = float_type()->compare(a, b);
	break;
    }

    case op_CMPo: {
	UData a = mLeft->eval(aExec);
	UData b = mRight->eval(aExec);

	/* FIXME: handle subtype compare */
	a.o = dynamic_cast<CObject*>(a.o);
	b.o = dynamic_cast<CObject*>(b.o);

	if (a.o == b.o) r.i = 0;
	else if (b.o == NULL) r.i = -1;
	else if (a.o == NULL) r.i = 1;
	else r.i = a.o->type()->compare(a, b);
	break;
    }

    case op_ELEM: {
	CType* lt;
	UData lv = mLeft->eval(aExec);
	UData iv = mRight->eval(aExec);

	if (lv.o == NULL) {
	    m1BreakHere(file(), line(), "nil array access exception");
	    throw M1StatusError;
	}
	if ((iv.i < 0) || (iv.i >= (int) lv.o->size())) {
	    m1BreakHere(file(), line(), "array index exception");
	    throw M1StatusError;
	}
	r = lv.o->at(iv.i);
	if ((lt = unfoldType(mLeft->type())) == string_type())
	    r = m1ReadConvert(r, char_type());
	else
	    r = m1ReadConvert(r, ((CArrayType*)lt)->elementType());
	break;
    }

    case op_ELEMS: {
	M1Expr* xstart = ((M1TrinaryExpr*)mRight)->first();
	M1Expr* xstop  = ((M1TrinaryExpr*)mRight)->second();
	M1Expr* xstep  = ((M1TrinaryExpr*)mRight)->third();
	UData lv       = mLeft->eval(aExec);
	UData start;
	UData stop;
	UData step;

	start = xstart->eval(aExec);
	if ((xstop==NULL) && (xstep == NULL)) // v[start]
	    stop = start;
	else if (xstop == NULL)               // v[start::step]
	    stop.i = lv.arr->size()-1;
	else
	    stop = xstop->eval(aExec);        // v[start:stop]
	if (xstep == NULL) {                  // v[start:stop]
	    if (start.i <= stop.i)
		step = USigned(1);
	    else
		step = USigned(-1);
	}
	else
	    step = xstep->eval(aExec);        // v[start:stop:step]
	if (step.i == 0)
	    r = nil;
	else {
	    // calculate the result array size
	    int sz = (stop.i - start.i + step.i) / step.i;
	    CArrayType* t = (CArrayType*) mLeft->type();
	    int i;
	    int j = 0;

	    r = t->produceArray(aExec, sz);
	    if (step.i > 0) {
		for (i = start.i, j=0; i <= stop.i; i += step.i, j++)
		    r.arr->put(aExec, j, lv.arr->at(i));
	    }
	    else {
		for (i = start.i, j=0; i >= stop.i; i += step.i, j++) 
		    r.arr->put(aExec, j, lv.arr->at(i));
	    }
	}
	break;
    }

    case op_SEQ:
	mLeft->eval(aExec);
	r = mRight->eval(aExec);
	break;

    default:
	ERRFMT("M1BinaryExpr::eval operator %s not handled",
	       formatOperator(op()).c_str());
	return nil;
    }
    DBGFMT_EVAL("value %d", r.i);
    return r;
}


bool M1BinaryExpr::eval_trg(CExecutor* aExec)
{
    DBGFMT_EVAL("M1BinaryExpr:eval_trg: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("[");
	print(&cout, 0);
	printf("]\n");
    }
    switch(op()) {
    case op_ORi:
	return (mLeft->eval_trg(aExec) || mRight->eval_trg(aExec));
    case op_ANDi:
	return (mLeft->eval_trg(aExec) && mRight->eval_trg(aExec));
    case op_EQ:
	return (mLeft->eval_trg(aExec) == mRight->eval_trg(aExec));
    case op_NEQ:
	return (mLeft->eval_trg(aExec) != mRight->eval_trg(aExec));
    case op_ELEM: {
	CEvent* evt;
	UData lv = mLeft->eval(aExec);
	UData iv = mRight->eval(aExec);

	if (lv.o == NULL) {
	    m1BreakHere(file(), line(), "nil array access exception");
	    throw M1StatusError;
	}
	if ((iv.i < 0) || (iv.i >= (int) lv.o->size())) {
	    m1BreakHere(file(), line(), "array index exception");
	    throw M1StatusError;
	}
	if ((evt = lv.o->eventAt(iv.i)) == NULL) {
	    m1BreakHere(file(), line(), "nil event exception");
	    throw M1StatusError;
	}
	return evt->updated();
    }

    default:
	ERRFMT("M1BinaryExpr::eval_trg operator %d - %s not handled",
	       op(), formatOperator(op()).c_str());
	return false;
    }
}

UAddress M1BinaryExpr::eval_ref(CExecutor* aExec)
{
    UAddress a;

    DBGFMT_EVAL("M1BinaryExpr:eval_ref: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("[");
	this->print(&cout, 0);
	printf("]\n");
    }
    switch(op()) {
    case op_ELEM: {
	UData lv = mLeft->eval(aExec);
	UData iv = mRight->eval(aExec);

	if (lv.o == NULL) {
	    m1BreakHere(file(), line(), "nil array access exception");
	    throw M1StatusError;
	}
	else if (iv.i < 0)  {
	    m1BreakHere(file(), line(), "array index exception");
	    throw M1StatusError;
	}
	if (iv.i >= (int) lv.o->size()) {
	    CType* t = unfoldType(lv.o->type());
	    CArrayType* at;
	    if (t == string_type()) {
		lv.str->resize(iv.i+1);
	    }
	    else if ((at = dynamic_cast<CArrayType*> (t)) != NULL) {
		if (at->arraySize() == 0) {
		    CType* bt = at->elementType();
		    if (int(lv.arr->size()) <= iv.i) {
			unsigned i = lv.arr->size();
			lv.arr->resize(iv.i + 1);
			while(i < lv.arr->size()) {
			    bt->elementInit(aExec, lv.arr->base(), i);
			    i++;
			}
		    }
		}
		else {
		    m1BreakHere(file(), line(), "array index exception");
		    throw M1StatusError;
		}
	    }
	    else {
		m1BreakHere(file(), line(), "array index exception");
		throw M1StatusError;
	    }		
	}
	a.obj   = lv.o;
	a.index = iv.i;
	a.trig  = TRIGGER_AUTO;
	break;
    }
    default:
	ERRFMT("M1BinaryExpr::eval_ref %c not handled",
	       formatOperator(op()).c_str());
	throw M1StatusError;
	return null;
    }
    DBGFMT_EVAL("address %p[%d]", a.obj, a.index); 
    return a;
}

#define EVAL_UNARY(op,r,n,sel) do {	 \
	UData nv = (n)->eval(aExec);		 \
	r.sel = op nv.sel;			 \
    } while(0)

#define EVAL_UP(r,n,val,sel) do {		\
	UAddress a = nullCheck((n)->eval_ref(aExec));	\
	UData lv   = addressAt(a);			\
	r.sel = lv.sel + (val);				\
	addressPut(aExec, (n), a, r);			\
    } while(0)
	

UData M1UnaryExpr::eval(CExecutor* aExec)
{
    UData r;

    DBGFMT_EVAL("M1UnaryExpr::eval: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("[");
	this->print(&cout, 0);
	printf("]\n");
    }
    switch(op()) {
    case op_NEGi: EVAL_UNARY(-,r,mExpr,i); break;
    case op_NEGf: EVAL_UNARY(-,r,mExpr,f); break;

    case op_BNOTu: EVAL_UNARY(~,r,mExpr,u); break;

    case op_NOTi: EVAL_UNARY(!,r,mExpr,i); break;

    case op_INCi: EVAL_UP(r,mExpr,1,i); break;
    case op_INCu: EVAL_UP(r,mExpr,1,u); break;
    case op_INCb: EVAL_UP(r,mExpr,1,b); break;
    case op_INCc: EVAL_UP(r,mExpr,1,c); break;
    case op_INCt: EVAL_UP(r,mExpr,1,t); break;
    case op_INCf: EVAL_UP(r,mExpr,1.0,f); break;

    case op_DECi: EVAL_UP(r,mExpr,-1,i); break;
    case op_DECu: EVAL_UP(r,mExpr,-1,u); break;
    case op_DECb: EVAL_UP(r,mExpr,-1,b); break;
    case op_DECc: EVAL_UP(r,mExpr,-1,c); break;
    case op_DECt: EVAL_UP(r,mExpr,-1,t); break;
    case op_DECf: EVAL_UP(r,mExpr,-1.0,f); break;

    case op_LTz: r.i = (mExpr->eval(aExec).i < 0); break;
    case op_LTEz: r.i = (mExpr->eval(aExec).i <= 0); break;
    case op_GTz: r.i = (mExpr->eval(aExec).i > 0); break;
    case op_GTEz: r.i = (mExpr->eval(aExec).i >= 0); break;
    case op_EQz: r.i = (mExpr->eval(aExec).i == 0); break;
    case op_NEQz: r.i = (mExpr->eval(aExec).i != 0); break;
	
    default:
	ERRFMT("M1Unary::eval operator %s not handled",
	       formatOperator(op()).c_str());
	return nil;
    }
    return r;
}

UAddress M1UnaryExpr::eval_ref(CExecutor* aExec)
{
    UAddress a;

    DBGFMT_EVAL("M1UnaryExpr::eval_ref: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("[");
	this->print(&cout, 0);
	printf("]\n");
    }
    switch(op()) {
    case op_TRIGGER:
	a = mExpr->eval_ref(aExec);
	a.trig = TRIGGER_YES;
	return a;
    case op_SUPRESS:
	a = mExpr->eval_ref(aExec);
	a.trig = TRIGGER_NO;
	return a;
    default:
	ERRFMT("M1Unary::eval_ref operator %s not handled",
	       formatOperator(op()).c_str());
	return null;	
    }
}


bool M1UnaryExpr::eval_trg(CExecutor* aExec)
{
    DBGFMT_EVAL("M1UnaryExpr::eval_trg: ");
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("[");
	this->print(&cout, 0);
	printf("]\n");
    }

    switch(op()) {
    case op_NOTi:
	return !mExpr->eval_trg(aExec);
    default:
	ERRFMT("M1Unary::eval_trg() operator %s not handled",
	       formatOperator(op()).c_str());
	return false;
    }
}

UData M1CvtExpr::eval(CExecutor* aExec)
{
    UData r = mExpr->eval(aExec);
    return m1Cvt(mCvt, r);
}


UData M1CastExpr::eval(CExecutor* aExec)
{
    UData value = mExpr->eval(aExec);
    CObject* obj;

    if (value.o == NULL)  // nil is ok
	return value;
    if ((obj = dynamic_cast<CObject*>(value.o)) == NULL) {
	WARNFMT("warning: value %u can not be casted to %s",
		value.u, type()->cname());
	return nil;
    }
    else {
	CType* objType = obj->type();  // This is the real type
	CType* t = objType;
	// type() must be a found among t's base types
	while(t && t != type())
	    t = t->parentType();
	if (t == NULL) {
	    WARNFMT("warning: %s can not be casted to %s",
		    objType->cname(), type()->cname());
	    return nil;
	}
    }
    return value;
}

UAddress M1CastExpr::eval_ref(CExecutor* aExec)
{
    // FIXME: need proper cast !
    return mExpr->eval_ref(aExec);
}

bool M1CastExpr::eval_trg(CExecutor* aExec)
{
    // FIXME: remove this
    return mExpr->eval_trg(aExec);
}

// This node is not used, replaced in lint by M1BuiltinExpr
UData M1CallExpr::eval(CExecutor* aExec)
{
    ERRFMT("M1CallExpr::eval() called");
    return nil;
}

UData M1BuiltinExpr::eval(CExecutor* aExec)
{
    int ai = 0;
    bool vargs = false;
    UData args[MAX_BUILTIN_ARGS];
    list<M1Expr*>::iterator iter = mArgs->begin();

    while(iter != mArgs->end()) {
	if (!vargs) {
	    if (mBf->atype[ai] == M1TYPE_EVENT) {
		UAddress a = (*iter)->eval_ref(aExec);
		args[ai++].evt = a.obj->eventAt(a.index);
	    }
	    else {
		args[ai++] = (*iter)->eval(aExec);		
	    }
	}
	else {
	    if (mBf->atype[ai] == BUILTIN_ARG_VARG)
		vargs = true;
	    args[ai++] = (*iter)->eval(aExec);
	}
	iter++;
    }
    return (*mBf->func)(aExec, args);
}

// Evaluate the field id, return the index and object the object in case
// of field array
static int evalFid(CExecutor* aExec, M1Expr* expr, CObject* &object)
{
    if (isAType(M1BinaryExpr*, expr)) {
	M1BinaryExpr* elemExpr = (M1BinaryExpr*)expr;
	UData ix = elemExpr->right()->eval(aExec);
	int fix  = evalFid(aExec, elemExpr->left(), object);
	UData elem = object->at(fix);
	object = elem.o;
	return ix.i;
    }
    else if (isAType(M1Identifier*, expr))
	return ((M1Identifier*) expr)->index();
    else if (isAType(M1BasedIdentifier*, expr))
	return ((M1BasedIdentifier*) expr)->index();
    return -1;
}

void VmEvalArgs::run(CExecutor* aExec, CObject* obj)
{
    list<M1Expr*>::iterator iter = mInitList->begin();
    ExecutorState state;
    CBaseObject* dummy;

    DBGFMT_EVAL("VmEvalArgs::run(%p)", obj);

    aExec->save(&state);

    // Switch to creator to get context right!!!
    aExec->activate(creator());
    aExec->activate_global(global(), &dummy);

    while(iter != mInitList->end()) {
	CObject* object = obj;  // get a copy, since evalFid may update it!
	M1BinaryExpr* field = (M1BinaryExpr*) *iter;
	CType* ftype;
	int index;

	index = evalFid(aExec, field->left(), object); // may update object!!!
	ftype = object->typeAt(index);

	switch(field->op()) {
	case op_CNCT: {
	    UAddress a = field->right()->eval_ref(aExec);
	    ((CExecutable*)object)->connect(index,(CExecutable*)a.obj,a.index);
	    break;
	}
	case op_PUTs:
	case op_PUTo:
	case op_PUTf:
	case op_PUTi:
	case op_PUTu: {
	    UData value = field->right()->eval(aExec);
	    object->put(aExec, index, value);
	    break;
	}
	case op_PUTc: {
	    UData value = field->right()->eval(aExec);
	    value.c = value.i;
	    object->put(aExec, index, value);
	    break;
	}
	case op_PUTt: {
	    UData value = field->right()->eval(aExec);
	    value.t = value.i;
	    object->put(aExec, index, value);
	    break;
	}
	case op_PUTb: {
	    UData value = field->right()->eval(aExec);
	    value.b = value.u;
	    object->put(aExec, index, value);
	    break;
	}

	default:
	    break;
	}
	iter++;
    }
    aExec->restore(&state);
}

UData M1ObjectConstant::eval(CExecutor* aExec)
{
    VmEvalArgs* args = NULL;
    CBaseType* t = (CBaseType*)type();
    UData object;

    DBGFMT_EVAL("M1ObjectConstant::eval() type:%s", t->cname());

    if ((mInitList != NULL) && (mInitList->size() > 0))
	args = new VmEvalArgs(aExec->current(), aExec->global(), mInitList);
    object = t->produce(aExec, t, args);
    if (isAType(CExecutable*, object.o))
	((CExecutable*)object.o)->start(aExec);

    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("M1ObjectConstant: ");
	if (t == NULL)
	    printf("nil");
	else
	    t->print(&cout, object);
	printf("\n");
    }
    return object;
}

UData M1ArrayConstant::eval(CExecutor* aExec)
{
    list<M1Expr*>::iterator iter = mInitList->begin();
    CArrayType* t = (CArrayType*)type();
    UData array;
    int index = 0;

    DBGFMT_EVAL("M1ArrayConstant::eval() type:%s", t->cname());

    array = t->produceArray(aExec, mInitList->size());
    while(iter != mInitList->end()) {
	UData value = (*iter)->eval(aExec);
	array.arr->put(aExec, index, value);
	index++;
	iter++;
    }
    if (M1DBG_IS_SET(M1DBG_EVAL|M1DBG_PRNT)) {
	printf("M1ArrayConstant: ");
	if (t == NULL)
	    printf("nil");
	else
	    t->print(&cout, array);
	printf("\n");
    }
    return array;
}

// Check if statement 
M1Status M1IfStatement::eval(CExecutor* aExec)
{
    UData cond = mCondition->eval(aExec);

    DBGFMT_EVAL("M1IfStatement::eval() cond=%d", cond.t);

    if (cond.i)
	return mThen->eval(aExec);
    else if (mElse != NULL)
	return mElse->eval(aExec);
    else
	return M1StatusOk;
}

M1Status M1SwitchStatement::eval(CExecutor* aExec)
{
    M1Status res = M1StatusOk;
    UData    v;
    int      pos = mDefaultPos;
    int      i, n;

    DBGFMT_EVAL("M1SwitchStatement::eval()");

    v = mValue->eval(aExec);

    // Find the statement(s) to execute
    n = (int) mSelect.size();
    for (i = 0; i < n; i++) {
	if (mSelect[i].mValue == v.i) {
	    pos = mSelect[i].mPosition;
	    break;
	}
    }
    if (pos < 0)
	return res;  // Nothing to execute continue

    if (mLocalSize) {
	// reset local variables
	unsigned int n = mLocalSize;
	UData* ptr = aExec->framePtr() + mLocalStackPos;
	
	while(n--)
	    *ptr++ = nil;
    }
    // run init code 
    for (i = mInitStart; i < mInitStop; i++)  {
	M1Statement* stmt = mSelectCode.at(i);
	res = stmt->eval(aExec);
    }

    n = (int) mSelectCode.size();
    while ((res == M1StatusOk) && (pos < n)) {
	M1Statement* stmt = mSelectCode.at(pos);
	
	res = stmt->eval(aExec);
	pos++;
    }
    return (res == M1StatusBreak) ? M1StatusOk : res;
}

M1Status M1CaseStatement::eval(CExecutor* aExec)
{
    // Should not appear during eval
    ERRFMT("runtime error: case label in bad context");
    return M1StatusError;
}

M1Status M1DefaultStatement::eval(CExecutor* aExec)
{
    // Should not appear during eval
    ERRFMT("runtime error: default label in bad context");
    return M1StatusError;
}

M1Status M1JumpStatement::eval(CExecutor* aExec)
{
    DBGFMT_EVAL("M1JumpStatement::eval()");
    switch(mWhere) {
    case m1::m1Parser::token::BREAK:    return M1StatusBreak;
    case m1::m1Parser::token::CONTINUE: return M1StatusContinue;
    case m1::m1Parser::token::RETURN:   return M1StatusReturn;
    default:       return M1StatusError;
    }
}

M1Status M1CompoundStatement::eval(CExecutor* aExec)
{
    M1Status res = M1StatusOk;

    DBGFMT_EVAL("M1CompundStatement::eval()");
    if (mLocalSize) {
	// reset local variables
	unsigned int n = mLocalSize;
	UData* ptr = aExec->framePtr() + mLocalStackPos;
	
	while(n--)
	    *ptr++ = nil;
    }
    if (mStatements)
	res = mStatements->eval(aExec);
    return res;
}
//
// foreach x in [1:10]   { x; }
// foreach x in [1:10:3] { x; }
// foreach x in [10:1:-3] { x; }
// foreach x in array { }  // x[0], x[2], .. x[N-1]
//
M1Status M1ForEachStatement::eval(CExecutor* aExec)
{
    DBGFMT_EVAL("M1EachStatement::eval()");
    CType* loopType = mVar->type();
    M1Status res = M1StatusOk;

    if (mRange->op() == op_RANGE) {
	UAddress a  = mVar->eval_ref(aExec);
	UData    cur  = ((M1TrinaryExpr*)mRange)->first()->eval(aExec);
	UData    end  = ((M1TrinaryExpr*)mRange)->second()->eval(aExec);
	M1Expr*  sx   = ((M1TrinaryExpr*)mRange)->third();
	UData    step;

	switch(loopType->typeTag()) {
	case M1TYPE_BOOL:
	case M1TYPE_CHAR:
	case M1TYPE_SIGNED:
	    step = sx ? sx->eval(aExec) : USigned(1);
	    if (step.i > 0) {
		if (cur.i <= end.i) {
		    while ((res != M1StatusBreak) && (cur.i < end.i)) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
			cur.i += step.i;
		    }
		    if (res != M1StatusBreak) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
		    }
		}
	    }
	    else if (step.i < 0) {
		if (cur.i >= end.i) {
		    while ((res != M1StatusBreak) && (cur.i > end.i)) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
			cur.i += step.i;
		    }
		    if (res != M1StatusBreak) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
		    }
		}
	    }
	    break;
	case M1TYPE_BYTE:
	case M1TYPE_UNSIGNED:
	    step = sx ? sx->eval(aExec) : USigned(1);
	    if (step.i > 0) {
		if (cur.u <= end.u) {
		    while ((res != M1StatusBreak) && (cur.u < end.u)) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
			cur.u += step.i;
		    }
		    if (res != M1StatusBreak) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
		    }
		}
	    }
	    else if (step.i < 0) {
		if (cur.u >= end.u) {
		    while ((res != M1StatusBreak) && (cur.u > end.u)) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
			cur.u += step.i;
		    }
		    if (res != M1StatusBreak) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
		    }
		}
	    }
	    break;
	case M1TYPE_FLOAT:
	    step = sx ? sx->eval(aExec) : UFloat(1.0);
	    if (step.f > 0.0) {
		if (cur.f <= end.f) {
		    while ((res != M1StatusBreak) && (cur.f < end.f)) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
			cur.f += step.f;
		    }
		    if (res != M1StatusBreak) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
		    }
		}
	    }
	    else if (step.f < 0.0) {
		if (cur.f >= end.f) {
		    while ((res != M1StatusBreak) && (cur.f > end.f)) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
			cur.f += step.f;
		    }
		    if (res != M1StatusBreak) {
			addressPut(aExec, mVar, a, cur);
			res = mStatement->eval(aExec);
		    }
		}
	    }
	    break;
	default:
	    throw M1StatusError;
	}
    }
    else {
	UAddress a  = mVar->eval_ref(aExec);
	UData object = mRange->eval(aExec);
	int i = 0;
	size_t n = object.arr->size();
	UData step = USigned(1);

	while((res != M1StatusBreak) && (i < (int) n)) {
	    UData v = object.arr->at(i);
	    addressPut(aExec, mVar, a, v);
	    res = mStatement->eval(aExec);
	    i += step.i;
	}
    }
    return (res != M1StatusError) ? M1StatusOk: res;
}

M1Status M1ExprStatement::eval(CExecutor* aExec)
{
    DBGFMT_EVAL("M1ExprStatement::eval()");
    if (mExpr) {
	try {
	    mExpr->eval(aExec);
	}
	catch( M1Status status) {
	    return status;
	}
    }
    return M1StatusOk;
}

M1Status M1StatementList::eval(CExecutor* aExec)
{
    M1Status res = M1StatusOk;
    list<M1Statement*>::iterator iter = mList.begin();
    while((res == M1StatusOk) && (iter!=mList.end())) {
	res = (*iter)->eval(aExec);
	iter++;
    }
    return res;
}

void M1Script::eval(CExecutor* aExec)
{
    list<M1Expr*>::iterator iter;
    UData doBody;

    DBGFMT_EVAL("M1Script::eval()");

    doBody.t = true;
    if (mTrigger != NULL)
	doBody.t = mTrigger->eval_trg(aExec);

    if (doBody.t && (mWhen != NULL))
	doBody = mWhen->eval(aExec);
    if (doBody.t) {
	DBGFMT_EVAL("M1Scrip::eval() alloc frameSize=%d", mFrameSize);
	aExec->allocFrame(mFrameSize);
	mBody->eval(aExec);
	aExec->deallocFrame();
	DBGFMT_EVAL("M1Scrip::eval() dealloc frameSize=%d", mFrameSize);
    }
}

