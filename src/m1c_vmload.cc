//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// M1 vm code loader
//

#include "m1vm.hh"
#include "m1c.hh"
#include "m1_parse.hh"


#define MAX_TYPE_STACK 64

static inline CType* unfoldType(CType* a)
{
    if ((a!=NULL) && isAType(CEventType*, a))
	return ((CEventType*)a)->baseType();
    return a;
}

static inline int getOP(CType* t, VmInstructionBuffer* vmb)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_FLOAT:    return vmb->put_op(op_GETf);
    case M1TYPE_SIGNED:   return vmb->put_op(op_GETi);
    case M1TYPE_UNSIGNED: return vmb->put_op(op_GETu);
    case M1TYPE_BOOL:     return vmb->put_op(op_GETt);
    case M1TYPE_CHAR:     return vmb->put_op(op_GETc);
    case M1TYPE_BYTE:     return vmb->put_op(op_GETb);
    case M1TYPE_STRING:   return vmb->put_op(op_GETs);
    case M1TYPE_ARRAY:    return vmb->put_op(op_GETo);
    case M1TYPE_OBJECT:   return vmb->put_op(op_GETo);
    default: return 0;
    }
}

static inline int putOP(CType* t, VmInstructionBuffer* vmb)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_FLOAT:    return vmb->put_op(op_PUTf);
    case M1TYPE_SIGNED:   return vmb->put_op(op_PUTi);
    case M1TYPE_UNSIGNED: return vmb->put_op(op_PUTu);
    case M1TYPE_BOOL:     return vmb->put_op(op_PUTt);
    case M1TYPE_CHAR:     return vmb->put_op(op_PUTc);
    case M1TYPE_BYTE:     return vmb->put_op(op_PUTb);
    case M1TYPE_STRING:   return vmb->put_op(op_PUTs);
    case M1TYPE_ARRAY:    return vmb->put_op(op_PUTo);
    case M1TYPE_OBJECT:   return vmb->put_op(op_PUTo);
    default: return 0;
    }
}

static inline int fgetOP(CType* t, VmInstructionBuffer* vmb)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_FLOAT:    return vmb->put_op(op_FGETf);
    case M1TYPE_SIGNED:   return vmb->put_op(op_FGETi);
    case M1TYPE_UNSIGNED: return vmb->put_op(op_FGETu);
    case M1TYPE_BOOL:     return vmb->put_op(op_FGETt);
    case M1TYPE_CHAR:     return vmb->put_op(op_FGETc);
    case M1TYPE_BYTE:     return vmb->put_op(op_FGETb);
    case M1TYPE_STRING:   return vmb->put_op(op_FGETs);
    case M1TYPE_ARRAY:    return vmb->put_op(op_FGETo);
    case M1TYPE_OBJECT:   return vmb->put_op(op_FGETo);
    default: return 0;
    }
}

static inline int fputOP(CType* t, VmInstructionBuffer* vmb)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_FLOAT:    return vmb->put_op(op_FPUTf);
    case M1TYPE_SIGNED:   return vmb->put_op(op_FPUTi);
    case M1TYPE_UNSIGNED: return vmb->put_op(op_FPUTu);
    case M1TYPE_BOOL:     return vmb->put_op(op_FPUTt);
    case M1TYPE_CHAR:     return vmb->put_op(op_FPUTc);
    case M1TYPE_BYTE:     return vmb->put_op(op_FPUTb);
    case M1TYPE_STRING:   return vmb->put_op(op_FPUTs);
    case M1TYPE_ARRAY:    return vmb->put_op(op_FPUTo);
    case M1TYPE_OBJECT:   return vmb->put_op(op_FPUTo);
    default: return 0;
    }
}

static inline int add1(CType* t, VmInstructionBuffer* vmb)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_FLOAT:
	vmb->put_op(op_PUSHf);
	vmb->put_float(1.0);
	return vmb->put_op(op_ADDf);
    case M1TYPE_SIGNED:
    case M1TYPE_CHAR:
    case M1TYPE_BYTE:
	vmb->put_op(op_PUSHi);
	vmb->put_signed(1);
	return vmb->put_op(op_ADDi);
    case M1TYPE_UNSIGNED:
    case M1TYPE_BOOL:
	vmb->put_op(op_PUSHu);
	vmb->put_unsigned(1);
	return vmb->put_op(op_ADDu);
    default: 
	return 0;
    }
}

static inline int cmp(CType* t, VmInstructionBuffer* vmb)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_FLOAT:
	return vmb->put_op(op_CMPf);
    case M1TYPE_SIGNED:
    case M1TYPE_CHAR:
    case M1TYPE_BYTE:
	return vmb->put_op(op_CMPi);
    case M1TYPE_UNSIGNED:
    case M1TYPE_BOOL:
	return vmb->put_op(op_CMPu);
    default: 
	return 0;
    }
}
//
// Type names are given with : as scope separator with 
// an initial : to mark scope 0
//
static CBaseType* typeStack[MAX_TYPE_DEPTH];
static int    typeDepth = -1;
static string libName = "";  // current library being loaded

CType* loadType(string aId)
{
    CType* t = NULL;
    int typeLevel = 0;

    DBGFMT_COMP("vmload: loadType %s\n", aId.c_str());

    if ((aId != "") && (aId[0] == ':')) {
	string sid = aId.substr(1);
	CBaseType* scope;
	CField*    fld;
	string::size_type loc = sid.find(':');
	
	aId = sid.substr(0, loc);
	if (aId == libName) {
	    if (loc == string::npos)
		return typeStack[0];
	    sid = sid.substr(loc+1);
	    loc = sid.find(':');
	    scope = typeStack[0]->subTypes();
	}
	else
	    scope =  ((CBaseType*)m1_context().type())->subTypes();

	while((loc != string::npos) && (scope != NULL)) {
	    aId = sid.substr(0, loc);
	    sid = sid.substr(loc+1);
	    if ((fld = scope->field(aId)) == NULL)
		goto not_found;
	    loc = sid.find(':');
	    scope = dynamic_cast<CBaseType*>(fld->type());
	    scope = (scope == NULL) ? NULL : scope->subTypes();
	}
	aId = sid;
	if ((scope == NULL) || ((fld = scope->field(aId)) == NULL))
	    goto not_found;
	t = fld->type();
    }
    else {
	typeLevel = typeDepth;

	while(typeLevel >= 0) {
	    CBaseType* scope = typeStack[typeLevel]->subTypes();
	    CField* fld;
	    if ((scope != NULL) && ((fld = scope->field(aId)) != NULL)) {
		t = fld->type();
		goto found;
	    }
	    typeLevel--;
	}
	if ((t = m1TypeLookup(aId)) == NULL)
	    goto not_found;
    }
found:
    if (isAType(VmEvalType*, t)) {
	fprintf(stderr, "vmload: type %s not translated\n", aId.c_str());
	return t;
    }
    else if (isAType(VmMachineType*, t))
	DBGFMT_COMP("vmload: found VmMachineType %s\n", t->cname());
    else
	DBGFMT_COMP("vmload: found Type %s\n", t->cname());
    return t;
not_found:
    fprintf(stderr, "vmload: type %s not found\n", aId.c_str());
    exit(1);
    return NULL;
}


//
// Translate all subtypes 
//
static VmMachineType* loadDef(VmEvalType* aEval);

static void loadSubTypes(CBaseType* aBase, VmMachineType* aMachine)
{
    CBaseType* subt = aBase->subTypes();
    int i;

    if (subt == NULL)
	return;
    
    for (i = 0; i < (int)subt->fieldCount(); i++) {
	CField* fld = subt->field(i);
	CType* fld_type = fld->type();
	if (isAType(VmEvalType*, fld_type)) {
	    VmMachineType* subMachine;
	    subMachine = loadDef(((VmEvalType*)fld_type));
	    aMachine->addSubType(fld->storage(), fld->name(), subMachine);
	}
    }
}

// Callback to translate VmEvalType to VmMachineType
static CType* translateType(CType* aType)
{
    switch(aType->typeTag()) {
    case M1TYPE_EVENT: {
	CEventType* t = (CEventType*) aType;
	CType* bt = t->baseType();
	CType* new_bt;

	if ((new_bt = translateType(bt)) == bt)
	    return aType;
	// fprintf(stderr, "Translated %s:\n", aType->cname());
	return CEventType::create(new_bt, t->direction());
    }

    case M1TYPE_OBJECT: {
	CBaseType* t = (CBaseType*) aType;
	string scopeName = t->scopeName();
	if (scopeName == "") return aType;
	// fprintf(stderr, "Translated %s:\n", scopeName.c_str());
	return loadType(scopeName);
    }

    case M1TYPE_ARRAY: {
	CArrayType* t = (CArrayType*) aType;
	CType* et = t->elementType();
	CType* new_et;

	if ((new_et = translateType(et)) == et)
	    return aType;
	// fprintf(stderr, "Translated %s:\n", aType->cname());
	return CArrayType::create(new_et, t->arraySize());
    }
    default:
	return aType;
    }
}

//
// Translate an eval type into a machine type
//
static VmMachineType* loadDef(VmEvalType* aEval)
{
    VmMachineType* iMachine;
    M1ScriptList* scriptList;
    M1StatementList* statementList;
    string pName = aEval->parentName();
    int S_size = 1;   // at least one word needed
    int R_size = 4;   // default to a top frame

    typeDepth++;

    // fprintf(stderr, "LOAD: %s\n", aEval->cname());
    
    iMachine = m1New(VmMachineType, aEval->definitionType());
    iMachine->setName(aEval->name());

    typeStack[typeDepth] = iMachine;
    loadSubTypes(aEval, iMachine);

    aEval->copyFields(iMachine, translateType);

    iMachine->setFieldTypeOffset(aEval->fieldTypeOffset());
    iMachine->setFieldTypeCount(aEval->fieldTypeCount());
    iMachine->setTypeLevel(aEval->typeLevel());



    if (aEval->parentType() != NULL)
	iMachine->setParent(translateType(aEval->parentType()));

    if ((scriptList = aEval->getPreScript()) != NULL) {
	size_t sz;
	VmInstructionBuffer vmb(S_size, R_size);
	list<M1Script*>::iterator iter  = scriptList->begin();
	while(iter != scriptList->end()) {
	    (*iter)->load(iMachine,&vmb);
	    iter++;
	}
	vmb.put_op(op_RETURN);
	vmb.put_op(op_UNDEF);
	iMachine->setPreScript(vmb.retrieve(sz, S_size, R_size));
    }

    if ((scriptList = aEval->getPostScript()) != NULL) {
	size_t sz;
	VmInstructionBuffer vmb(S_size, R_size);
	list<M1Script*>::iterator iter  = scriptList->begin();
	while(iter != scriptList->end()) {
	    (*iter)->load(iMachine,&vmb);
	    iter++;
	}
	vmb.put_op(op_RETURN);
	vmb.put_op(op_UNDEF);
	iMachine->setPostScript(vmb.retrieve(sz, S_size, R_size));
    }

    if ((statementList = aEval->getDefaults()) != NULL) {
	size_t sz;
	VmInstructionBuffer vmb(S_size, R_size);
	list<M1Statement*>::iterator iter = statementList->begin();
	while(iter != statementList->end()) {
	    (*iter)->load(iMachine,&vmb);
	    iter++;
	}
	vmb.put_op(op_RETURN);
	vmb.put_op(op_UNDEF);
	iMachine->setDefaults(vmb.retrieve(sz, S_size, R_size));
    }

    if ((statementList = aEval->getConstructor()) != NULL) {
	size_t sz;
	VmInstructionBuffer vmb(S_size, R_size);
	list<M1Statement*>::iterator iter = statementList->begin();
	while(iter != statementList->end()) {
	    (*iter)->load(iMachine,&vmb);
	    iter++;
	}
	vmb.put_op(op_RETURN);
	vmb.put_op(op_UNDEF);
	iMachine->setConstructor(vmb.retrieve(sz, S_size, R_size));
    }
    iMachine->setValueStackSize(S_size);
    iMachine->setReturnStackSize(R_size);
    
    typeDepth--;

    return iMachine;
}



VmMachineType* loadDefinitions(VmEvalType* aEval)
{
    typeDepth = -1;
    libName = aEval->name();
    return loadDef(aEval);
}

void M1Identifier::load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_SCOPE);
    vmb->put_signed((int)mLevel);
    vmb->put_op(op_PUSHi);       // push the index
    vmb->put_signed(mIndex);
    vmb->put_op(op_GETe);        // load event
    vmb->put_op(op_UPDATED);     // check if updated
}

void M1Identifier::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_SCOPE);
    vmb->put_signed((int)mLevel);
    vmb->put_op(op_PUSHi);      // push the index
    vmb->put_signed(mIndex);
}

void M1Identifier::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_SCOPE);
    vmb->put_signed((int)mLevel);
    vmb->put_op(op_PUSHi);       // push the index
    vmb->put_signed(mIndex);
    getOP(type(), vmb);          // generate the GETx op 
}

void M1BasedIdentifier::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_THIS);
    vmb->put_op(op_PUSHi);    // push the index
    vmb->put_signed(mIndex);
}

void M1BasedIdentifier::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_THIS);
    vmb->put_op(op_PUSHi);    // push the index
    vmb->put_signed(mIndex);
    getOP(type(), vmb);      // generate the GETx op 
}

void M1Index::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_PUSHi);
    vmb->put_signed(mIndex);
}

void M1Field::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    mObject->load(aMachine,vmb);   // generate fetch of object
    vmb->put_op(op_PUSHi);         // push the index
    vmb->put_signed(mIndex);
}

void M1Field::load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    if (isAType(M1This*, mObject)) {
	vmb->put_op(op_FGETe);
	vmb->put_signed(mIndex);
    }
    else {
	mObject->load(aMachine,vmb);   // generate fetch of object
	vmb->put_op(op_PUSHi);         // push the index
	vmb->put_signed(mIndex);
	vmb->put_op(op_GETe);          // fetch event
    }
    vmb->put_op(op_UPDATED);       // check if it's updated
}

void M1Field::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    if (isAType(M1This*, mObject)) {
	fgetOP(type(), vmb);            // generate FGETx op
	vmb->put_signed(mIndex);
    }
    else {
	mObject->load(aMachine,vmb);    // generate fetch of object
	vmb->put_op(op_PUSHi);          // push the index
	vmb->put_signed(mIndex);
	getOP(type(), vmb);            // generate GETx op 
    }
}


void M1TrinaryExpr::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    switch(op()) {
    case op_COND: {
	// (fst) ? (snd) : (thrd)
	int offset_loc;
	int thrd_loc;

	mFst->load(aMachine, vmb);
	vmb->put_op(op_JUMPeq);
	offset_loc = vmb->put_signed(0); // this is the offset to be patched
	mSnd->load_ref(aMachine, vmb);
	thrd_loc  = vmb->position();
	vmb->put_signed(offset_loc, thrd_loc - offset_loc);
	mThrd->load_ref(aMachine, vmb);
	break;
    }
    default:
	break;
    }
}

void M1TrinaryExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    switch(op()) {
    case op_COND: {
	// (fst) ? (snd) : (thrd)
	int offset_loc;
	int thrd_loc;

	mFst->load(aMachine, vmb);
	vmb->put_op(op_JUMPeq);
	offset_loc = vmb->put_signed(0); // this is the offset to be patched
	mSnd->load(aMachine, vmb);
	thrd_loc  = vmb->position();
	vmb->put_signed(offset_loc, thrd_loc - offset_loc);
	mThrd->load(aMachine, vmb);
	break;
    }

    case op_PROG2:
	if (mFst) {
	    mFst->load(aMachine, vmb);
	    vmb->put_op(op_DROP);
	}
	mSnd->load(aMachine, vmb);
	if (mThrd) {
	    mThrd->load(aMachine, vmb);
	    vmb->put_op(op_DROP);
	}
	break;
    }
}


void M1BinaryExpr::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    switch(op()) {
    case op_ELEM:
	mLeft->load(aMachine, vmb);   // the array
	mRight->load(aMachine, vmb);  // the index
	break;
    default:
	ERRFMT("M1BinaryExpr::load_ref operator %s not handled",
	       formatOperator(op()).c_str());
	break;
    }
}

void M1BinaryExpr::load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    switch(op()) {
    case op_ANDi: {
	int patch;
	int pos;
	
	mLeft->load_trg(aMachine,vmb);
	vmb->put_op(op_DUP);
	vmb->put_op(op_JUMPeq);
	patch = vmb->put_signed(0); // this is the offset to be patched
	vmb->put_op(op_DROP);
	mRight->load_trg(aMachine,vmb);
	pos  = vmb->position();
	vmb->put_signed(patch, pos - patch);
	break;
    }

    case op_ORi: {
	int patch;
	int pos;
	
	mLeft->load_trg(aMachine,vmb);
	vmb->put_op(op_DUP);
	vmb->put_op(op_JUMPne);
	patch = vmb->put_signed(0); // this is the offset to be patched
	vmb->put_op(op_DROP);
	mRight->load_trg(aMachine,vmb);
	pos = vmb->position();
	vmb->put_signed(patch, pos - patch);
	break;
    }

    case op_EQ:
	mLeft->load_trg(aMachine,vmb);
	mRight->load_trg(aMachine,vmb);
	vmb->put_op(op_CMPi);
	vmb->put_op(op_NOTi);
	break;

    case op_NEQ:
	mLeft->load_trg(aMachine,vmb);
	mRight->load_trg(aMachine,vmb);
	vmb->put_op(op_CMPi);
	break;
	
    case op_ELEM:
	mLeft->load(aMachine, vmb);
	mRight->load(aMachine, vmb);
	vmb->put_op(op_GETe);       // get source event
	vmb->put_op(op_UPDATED);    // check if updated
	break;

    default:
	ERRFMT("M1BinaryExpr::load_trg operator %s not handled",
	       formatOperator(op()).c_str());
	break;
    }
}

void M1BinaryExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    switch(op()) {
    case op_PUTs:
    case op_PUTo:
    case op_PUTf:
    case op_PUTi:
    case op_PUTu:
    case op_PUTc:
    case op_PUTt:
    case op_PUTb:
	if (isAType(M1Field*, mLeft) &&
	    isAType(M1This*, ((M1Field*) mLeft)->object())) {
	    mRight->load(aMachine,vmb);
	    fputOP(type(), vmb);
	    vmb->put_signed(((M1Field*) mLeft)->index());
	}
	else {
	    mLeft->load_ref(aMachine,vmb);
	    mRight->load(aMachine,vmb);
	    putOP(type(), vmb);
	}
	break;

    case op_COPY:
	// FIXME: load destination type
	mLeft->load_ref(aMachine,vmb);
	mRight->load(aMachine,vmb);
	vmb->put_op(op_COPY);
	break;

    case op_MULPUTi: case op_MULPUTu:  case op_MULPUTb:
    case op_MULPUTc: case op_MULPUTt:  case op_MULPUTf:
    case op_DIVPUTi: case op_DIVPUTu:  case op_DIVPUTb:
    case op_DIVPUTc: case op_DIVPUTt:  case op_DIVPUTf:
    case op_ADDPUTi: case op_ADDPUTu:  case op_ADDPUTb:
    case op_ADDPUTc: case op_ADDPUTt:  case op_ADDPUTf:
    case op_SUBPUTi: case op_SUBPUTu:  case op_SUBPUTb:
    case op_SUBPUTc: case op_SUBPUTt:  case op_SUBPUTf:
    case op_REMPUTi: case op_REMPUTu:  case op_REMPUTb:
    case op_REMPUTc: case op_REMPUTt:
    case op_BANDPUTi: case op_BANDPUTu:  case op_BANDPUTb:
    case op_BANDPUTc: case op_BANDPUTt:
    case op_BORPUTi: case op_BORPUTu:  case op_BORPUTb:
    case op_BORPUTc: case op_BORPUTt:
    case op_BXORPUTi: case op_BXORPUTu:  case op_BXORPUTb:
    case op_BXORPUTc: case op_BXORPUTt:
    case op_BSLPUTi: case op_BSLPUTu:  case op_BSLPUTb:
    case op_BSLPUTc: case op_BSLPUTt:
    case op_BSRPUTi: case op_BSRPUTu:  case op_BSRPUTb:
    case op_BSRPUTc: case op_BSRPUTt:
    case op_ADDPUTs:
	mLeft->load_ref(aMachine,vmb);  // address from object index
	mRight->load(aMachine,vmb);     // value
	vmb->put_op(op());
	break;

    case op_CNCT:
	// FIXME: optimise for nil!!!
	mLeft->load_ref(aMachine,vmb);  // load desitnation address
	vmb->put_op(op_GETe);           // get target event
	mRight->load_ref(aMachine,vmb); // load source address
	vmb->put_op(op_GETe);           // get source event
	vmb->put_op(op_CNCT);           // connect/disconnect
	break;

    case op_ANDi: {
	int offset_loc;
	int next_loc;
	
	mLeft->load(aMachine,vmb);       // -- v1
	vmb->put_op(op_DUP);             // -- v1 v1
	vmb->put_op(op_JUMPeq);          //
	offset_loc = vmb->put_signed(0); // this is the offset to be patched
	vmb->put_op(op_DROP);            
	mRight->load(aMachine,vmb);
	next_loc  = vmb->position();
	vmb->put_op(op_NEQz);           // normalize value
	vmb->put_signed(offset_loc, next_loc - offset_loc);
	break;
    }
	
    case op_ORi: {
	int offset_loc;
	int next_loc;
	
	mLeft->load(aMachine,vmb);
	vmb->put_op(op_DUP);
	vmb->put_op(op_JUMPne);
	offset_loc = vmb->put_signed(0); // this is the offset to be patched
	vmb->put_op(op_DROP);
	mRight->load(aMachine,vmb);
	next_loc  = vmb->position();
	vmb->put_op(op_NEQz);           // normalize value
	vmb->put_signed(offset_loc, next_loc - offset_loc);
	break;
    }

    case op_SEQ:
	mLeft->load(aMachine, vmb);
	vmb->put_op(op_DROP);         // left value is not used
	mRight->load(aMachine, vmb);
	break;

    case op_ELEM:
	mLeft->load(aMachine, vmb);   // the object
	mRight->load(aMachine, vmb);  // the index
	getOP(type(), vmb);           // get element
	break;
	
    default:
	mLeft->load(aMachine,vmb);
	mRight->load(aMachine,vmb);
	vmb->put_op(op());
	break;
    }
}

void M1UnaryExpr::load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb)  
{
    switch(op()) {
    case op_NOT:
	mExpr->load_trg(aMachine, vmb);
	vmb->put_op(op_NOTi);
	break;
    default:
	ERRFMT("M1UnaryExpr::load_trg operator %s not handled",
	       formatOperator(op()).c_str());
	break;
    }
}

void M1UnaryExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)  
{
    switch(op()) {
    case op_INCi: case op_INCu: case op_INCb: 
    case op_INCc: case op_INCt: case op_INCf:
    case op_DECi: case op_DECu: case op_DECb:
    case op_DECc: case op_DECt: case op_DECf:
	mExpr->load_ref(aMachine,vmb);
	vmb->put_op(op());	
	break;
    default:
	mExpr->load(aMachine,vmb);
	vmb->put_op(op());
	break;
    }
}

void M1CvtExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    mExpr->load(aMachine,vmb);
    switch(cvt()) {
    case M1CVT_iu: vmb->put_op(op_CVTiu); break;
    case M1CVT_if: vmb->put_op(op_CVTif); break;
    case M1CVT_ui: vmb->put_op(op_CVTui); break;
    case M1CVT_uf: vmb->put_op(op_CVTuf); break;
    case M1CVT_fi: vmb->put_op(op_CVTfi); break;
    case M1CVT_fu: vmb->put_op(op_CVTfu); break;
    case M1CVT_uu: break;
    case M1CVT_ii: break;
    case M1CVT_ff: break;
    default:
	// Should not appear during load
	ERRFMT("runtime error: bad conversion ");
    }
}


void M1CastExpr::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    // FIXME: need proper cast !
    mExpr->load_ref(aMachine,vmb);
}

void M1CastExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    // FIXME: need proper cast !
    mExpr->load(aMachine,vmb);
}
    
void M1CastExpr::load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    // FIXME: remove this
    mExpr->load_trg(aMachine,vmb);
}

void M1BuiltinExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    list<M1Expr*>::iterator iter = mArgs->begin();
    int nargs = 0;
    while(iter != mArgs->end()) {
	(*iter)->load(aMachine, vmb);
	iter++;
	nargs++;
    }
    vmb->put_op(op_CALL);
    vmb->put_data(mBf);
    vmb->put_signed(nargs);
}

void M1ByteConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    vmb->put_op(op_PUSHu);
    vmb->put_unsigned(mValue.b);
}

void M1CharConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    vmb->put_op(op_PUSHi);
    vmb->put_signed(mValue.c);
}

void M1SignedConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    vmb->put_op(op_PUSHi);
    vmb->put_signed(mValue.i);
}

void M1UnsignedConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    vmb->put_op(op_PUSHu);
    vmb->put_unsigned(mValue.u);
}

void M1FloatConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb) 
{
    vmb->put_op(op_PUSHf);
    vmb->put_float(mValue.f);
}

void M1Nil::load_ref(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_NIL);
    vmb->put_op(op_PUSHi);  // FIXME: could be op_NIL!!
    vmb->put_signed(0);
}

void M1Nil::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_NIL);
}

void M1This::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_THIS);
}

void M1BoolConstant::load_trg(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_PUSHi);
    vmb->put_signed( (mValue.t == true) ? 1 : 0);
}

void M1BoolConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    vmb->put_op(op_PUSHi);
    vmb->put_signed( (mValue.t == true) ? 1 : 0);
}

void M1StringConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    CString* s = aMachine->addStringConstant(mValue.str->str());
    vmb->put_op(op_PUSHs);
    vmb->put_data((void*)s);
}

//
// NOTE!!!
//  Them Block after NEWo is run form with in the object creation and
//  is after the default values but before the constructor!!!
//
//  NEWo <type> next
//  PUSH <patched-object>
//  BEGIN 0 0
//       ( case  @<type> {  x = 12 } )
//     IGET       ( -- o  )
//     PUSH ix    ( o -- o ix )
//     PUSH 12    ( o ix -- o ix 12 )
//     PUTi       ( o ix 12 -- 12 )
//     DROP
// ...
//       ( case  @<type> {  x[4] = 13 } )
//       
//     IGET       ( -- o )
//     PUSH ix    ( o -- o ix )
//     GETo       ( -- a )
//     PUSH 4     ( a -- a 4 )
//     PUSH 13    ( a 4 -- a 4 13 )
//     PUTi       ( a 4 13  -- 13 )
//     DROP
//
//       ( case @<type> {  x <- y } )
//
//     IGET       ( -- o )
//     PUSH ix    ( o -- o ix )
//     GETe       ( o ix -- x  )
//     THIS       ( x -- x this )
//     PUSH iy    ( x this -- x this iy )
//     GETe       ( x this iy -- x y )
//     CNCT       ( x y -- )
//     DROP
//  END
//  RETURN
// next:
//
//

// Produce code that access the field relative an object on stack
static CType* loadFid(CBaseType* bt, M1Expr* expr, VmMachineType* aMachine,
		      VmInstructionBuffer* vmb)
{
    if (isAType(M1BinaryExpr*, expr)) {
	M1BinaryExpr* elemExpr = (M1BinaryExpr*)expr;
	CType* ft = loadFid(bt, elemExpr->left(), aMachine, vmb);
	vmb->put_op(op_GETo);
	elemExpr->right()->load(aMachine, vmb);
	ft = unfoldType(ft);
	if (ft == string_type()) 
	    return char_type();
	else
	    return ((CArrayType*)ft)->elementType();
    }
    else if (isAType(M1Identifier*, expr)) {
	int index = ((M1Identifier*) expr)->index();
	vmb->put_op(op_PUSHi);
	vmb->put_signed(index);
	return bt->typeAt(index);
    }
    else if (isAType(M1BasedIdentifier*, expr)) {
	int index = ((M1BasedIdentifier*) expr)->index();
	vmb->put_op(op_PUSHi);
	vmb->put_signed(index);
	return bt->typeAt(index);
    }
    ERRFMT("loadFid: unknown fid expression, not handled");
    return NULL;
}

void M1ObjectConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    list<M1Expr*>::iterator iter = mInitList->begin();
    CBaseType* t = (CBaseType*) loadType(mTypeId->name());
    int patch = 0;
    int pos;

    vmb->put_op(op_NEWo);
    vmb->put_data(t);
    patch = vmb->put_signed(0);
    vmb->put_op(op_PUSHu);
    vmb->put_signed(0);    // caller of VmMachineArgs will patch object here
    // Generate the Args BLOCK  the object is on the stack on entry
    vmb->put_op(op_BEGIN);
    vmb->put_signed(0);
    vmb->put_signed(0);

    while(iter != mInitList->end()) {
	M1BinaryExpr* field = (M1BinaryExpr*) *iter;
	CType* ft;

	vmb->put_op(op_IGET);
	ft = loadFid(t, field->left(), aMachine, vmb);

	switch(field->op()) {
	case op_CNCT:
	    vmb->put_op(op_GETe);
	    field->right()->load_ref(aMachine, vmb);
	    vmb->put_op(op_GETe);
	    vmb->put_op(op_CNCT);
	    vmb->put_op(op_DROP);
	    break;
	case op_PUTs:
	case op_PUTo:
	case op_PUTf:
	case op_PUTi:
	case op_PUTu:
	case op_PUTc:
	case op_PUTt:
	case op_PUTb:
	    field->right()->load(aMachine, vmb);
	    putOP(ft, vmb);
	    vmb->put_op(op_DROP);
	    break;

	default:
	    ERRFMT("runtime error: bad Object field op ");
	    break;
	}
	iter++;
    }
    vmb->put_op(op_END);
    vmb->put_op(op_RETURN);
    pos = vmb->position();
    vmb->put_signed(patch, pos - patch);
}

//
// array CODE LAYOUT:
//
//    push v0
//    push v1
//    push vN-1
//    INEWa  <type> <n>
//

void M1ArrayConstant::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    list<M1Expr*>::iterator iter = mInitList->begin();
    size_t n = mInitList->size();
    CArrayType* t = (CArrayType*)translateType(type());

    if (n > 0) {
	// push all values
	while(iter != mInitList->end()) {
	    (*iter)->load(aMachine, vmb);
	    iter++;
	}
	vmb->put_op(op_INEWa);
	vmb->put_data(t);
	vmb->put_unsigned(n);
    }
    else {
	vmb->put_op(op_NEWa);
	vmb->put_data(t);
	vmb->put_unsigned(0);
    }
}

void M1Script::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    int patch1 = 0;
    int patch2 = 0;
    int break_patch = 0;
    int pos;
    int end_pos;

    if (mTrigger) {
	mTrigger->load_trg(aMachine, vmb);
	vmb->put_op(op_JUMPeq);
	patch1 = vmb->put_signed(0);
    }
    if (mWhen) {
	mWhen->load(aMachine, vmb);
	vmb->put_op(op_JUMPeq);
	patch2 = vmb->put_signed(0);
    }

    vmb->put_op(op_NIL);    // block value is not used
    vmb->put_op(op_BEGIN);
    break_patch    = vmb->put_signed(0);
    vmb->put_signed(0);
    mBody->load(aMachine, vmb);
    end_pos = vmb->put_op(op_END);
    pos = vmb->position();
    vmb->put_signed(break_patch, end_pos - break_patch);

    if (patch1)
	vmb->put_signed(patch1, pos - patch1);
    if (patch2)
	vmb->put_signed(patch2, pos - patch2);

}

void M1StatementList::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    list<M1Statement*>::iterator iter = mList.begin();
    while(iter!=mList.end()) {
	(*iter)->load(aMachine, vmb);
	iter++;
    }
}

//
//  layout:
//       condition-code
//       jumpEQ else-offset
//       then-code
// else-offset:
//       else-code
//
//
void M1IfStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    int else_patch;
    int else_pos;
    int end_pos;

    mCondition->load(aMachine, vmb);
    vmb->put_op(op_JUMPeq);
    else_patch = vmb->put_signed(0); // this is the offset to be patched
    mThen->load(aMachine, vmb);
    if (mElse) {
	int end_patch;
	vmb->put_op(op_JUMP);
	end_patch = vmb->put_signed(0); // this is the offset to be patched	
	else_pos  = vmb->position();
	mElse->load(aMachine, vmb);
	end_pos  = vmb->position();
	vmb->put_signed(end_patch, end_pos - end_patch);
    }
    else
	else_pos  = vmb->position();	
    vmb->put_signed(else_patch, else_pos - else_patch);
}

void M1JumpStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    switch(mWhere) {
    case m1::m1Parser::token::BREAK:
	vmb->put_op(op_BREAK);
	break;
    case m1::m1Parser::token::CONTINUE:
	vmb->put_op(op_CONTINUE);
	break;
    case m1::m1Parser::token::RETURN:
	vmb->put_op(op_RETURN);
	break;
    default:
	ERRFMT("runtime error: bad jump statement");
	break;
    }
}
//
// switch CODE LAYOUT:
//
// dense:
//     PUSH 0
//     BEGIN break 0
//     <switch-expr>
//     PUSH low
//     SUB
//     JUMPi L0 L1 L2 .... Ln-1
//     JUMP default|break
// L0:      <code-0>
// L1:      <code-1>
//
// Ln-1:    <code-n-1>
// default: <code-default>
// break:
//      END
//
// sparse:
//     PUSH 0
//     BEGIN break 0
//     <switch-expr>
//     JUMPi v0 v1 v2 ... v-1n L0 L1 L2 .... Ln-1
//     JUMP default|break
// L0:      <code-0>
// L1:      <code-1>
//
// Ln-1:    <code-n-1>
// default: <code-default>
// break:
//      END
//
//
void M1SwitchStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    int      i;
    int      n;
    int      m;
    int      pos;
    bool     dense = false;
    vector<int> loc;
    int      base_pos    = 0; // JUMPv/i instruction offset
    int      end_pos     = 0; // position after switch
    int      offset_pos  = 0; // Offset table position
    int      default_patch = 0; // patch location for default code jump
    int      break_patch   = 0;
    int      end_patch     = 0; // patch location for end jump

    mValue->load(aMachine, vmb);

    // generate init code (may depend on value)
    // FIXME this should go into the Compound constructor....
    // FIXME subtypes!!!
    // FIXME ...
    for (i = mInitStart; i < mInitStop; i++)  {
	M1Statement* stmt = mSelectCode.at(i);
	stmt->load(aMachine, vmb);
    }

    n = m = (int) mSelect.size();
    //
    //  When value are sparse we use JUMPv that implements binary 
    //  search for values
    //  Or we use a JUMPi when values are dense.
    //

    // Calculate potential JUMPi vectors size
    // This can be improved, for example locate dense groups.
    if (n > 0) {
	int v1 = mSelect.at(0).mValue;
	int vn = mSelect.at(n-1).mValue;

	m = (vn - v1) + 1;  // direct table size
	dense = (m <= n*2);

	vmb->put_op(op_NIL);    // block value is not used
	vmb->put_op(op_BEGIN);
	break_patch = vmb->put_signed(0);
	vmb->put_signed(0);

	if (dense) {
	    if (v1 != 0) {
		vmb->put_op(op_PUSHi);
		vmb->put_signed(v1);
		vmb->put_op(op_SUBi);
	    }
	    vmb->put_op(op_JUMPi);
	    vmb->put_unsigned((unsigned int) m);
	    offset_pos = vmb->position();
	    for (i = 0; i < m; i++)
		vmb->put_signed(0);
	    base_pos = vmb->position();
	}
	else {
	    vmb->put_op(op_JUMPv);
	    vmb->put_unsigned((unsigned int) n);
	    for (i = 0; i < n; i++)
		vmb->put_signed(mSelect.at(i).mValue);
	    offset_pos = vmb->position();
	    for (i = 0; i < n; i++)
		vmb->put_signed(0);
	    base_pos = vmb->position();
	}
    }

    if (mDefaultPos >= 0) {
	vmb->put_op(op_JUMP);
	default_patch = vmb->put_signed(0);
    }
    else {
	vmb->put_op(op_JUMP);
	end_patch = vmb->put_signed(0);
    }

    n = (int) mSelectCode.size();
    loc.resize(n);
    pos = (mInitStop > 0) ? mInitStop : 0;

    while (pos < n) {
	M1Statement* stmt = mSelectCode.at(pos);
	loc[pos] = vmb->position();
	stmt->load(aMachine, vmb);
	pos++;
    }
    end_pos = vmb->put_op(op_END);

    if (break_patch)
	vmb->put_signed(break_patch, end_pos - break_patch);
    if (default_patch)
	vmb->put_signed(default_patch, loc[mDefaultPos] - default_patch);
    if (end_patch)
	vmb->put_signed(end_patch, end_pos - end_patch);

    // back patch locations into jump table
    n = (int) mSelect.size();
    if (dense) {
	int j = 0;
	for (i = mSelect.at(0).mValue; i <= mSelect.at(n-1).mValue; i++) {
	    if ((j<n) && (mSelect.at(j).mValue == i)) {
		vmb->put_signed(offset_pos,loc[mSelect.at(j).mPosition]-base_pos);
		j++;
	    }
	    else if (mDefaultPos>=0)
		vmb->put_signed(offset_pos,loc[mDefaultPos]-base_pos);
	    else
		vmb->put_signed(offset_pos,end_pos-base_pos);
	    offset_pos++;
	}
    }
    else {
	for (i = 0; i < n; i++) {
	    vmb->put_signed(offset_pos,loc[mSelect.at(i).mPosition]-base_pos);
	    offset_pos++;
	}
    }
}

//
// foreach CODE LAYOUT:
//
// RANGE
//   <stop>               ( -- n )
//   <start>              ( n -- n i )
//   BEGIN break1 continue1 ( n i -- n )
//   DUP                  ( n -- n n )
//   IGET                 ( n -- n n i )
//   CMPx                 ( n n i -- n cond=compare(n,i) )
//   JUMPlt break2        ( n cond -- n )
//   JUMP   first
// continue1:
// continue2:
//    DUP            ( n -- n n )
//    IGET           ( n -- n n i' )
//    CMPx           ( n n i' -- n cond=compare(n,i') )
//    JUMPle break3  ( n cond -- n )
//    IGET           ( n -- n i  , push loop var onto stack )
//    add1           ( n -- n (i+1) )
//    IPUT           ( n i' -- n  )
// first:
//    mVar-ref       ( n -- n var )
//    IGET           ( n var -- n var i' )
//    PUTx           ( n var i' -- n i' )
//    DROP           ( n i' -- n )
//    <loop-code>
//    JUMP continue1
// break1:
// break2:
// break3:
//    DROP
//    END
//
//
// ARRAY
//   mRange               ( -- array )
//   CALL size/1          ( array -- n )
//   PUSH 0               ( n -- n 0 )
//   BEGIN break1 continue1 ( n 0 -- n )
//   JUMP check           ( check initial value )
// continue1:
// continue2:
//    IGET           ( n -- n i  , push loop var onto stack )
//    PUSHi 1        ( n i -- n i 1  )
//    ADDi           ( n i 1 -- n i+1 , updated loop value )
//    IPUT           ( n i' -- n  )
// check:
//    DUP            ( n -- n n )
//    IGET           ( n -- n n i' )
//    CMPx           ( n n i' -- n cond=compare(n,i') )
//    JUMPle break2   ( n cond -- n )
//    mVar-ref       ( n -- n var )
//    mRange         ( n -- n var obj )
//    IGET           ( n var obj  -- n var obj i' )
//    GETu           ( n var obj i'  -- n var elem )
//    PUTx           ( n var elem -- n elem )
//    DROP           ( n elem -- n )
//    <loop-code>
//    JUMP continue2
// break1:
// break2:
//    DROP
//    END
//
//
//
//
void M1ForEachStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    CType* loopType = mVar->type();
    int check_pos;
    int continue_pos;
    int break_pos;
    int first_pos;
    int break_patch1 = 0;
    int break_patch2 = 0;
    int break_patch3 = 0;
    int continue_patch1 = 0;
    int continue_patch2 = 0;
    int check_patch = 0;
    int first_patch = 0;
    
    if (mRange->op() == op_RANGE) {
	((M1TrinaryExpr*)mRange)->first()->load(aMachine, vmb); // stop code
	((M1TrinaryExpr*)mRange)->second()->load(aMachine, vmb); // start code
	// FIXME: step !
	vmb->put_op(op_BEGIN);
	break_patch1 = vmb->put_signed(0);
	continue_patch1 = vmb->put_signed(0);
	vmb->put_op(op_DUP);
	vmb->put_op(op_IGET);
	cmp(loopType, vmb);
	vmb->put_op(op_JUMPlt);
	break_patch2 = vmb->put_signed(0);
	vmb->put_op(op_JUMP);	
	first_patch  = vmb->put_signed(0);
	
	continue_pos = vmb->position();
	vmb->put_op(op_DUP);
	vmb->put_op(op_IGET);
	cmp(loopType, vmb);
	vmb->put_op(op_JUMPle);
	break_patch3 = vmb->put_signed(0);

	vmb->put_op(op_IGET);
	add1(loopType, vmb);
	vmb->put_op(op_IPUT);

	first_pos = vmb->position();
	mVar->load_ref(aMachine, vmb);
	vmb->put_op(op_IGET);
	putOP(loopType, vmb);	
	vmb->put_op(op_DROP);
	mStatement->load(aMachine, vmb);
	vmb->put_op(op_JUMP);
	continue_patch2 = vmb->put_signed(0);
	break_pos = vmb->put_op(op_DROP);
	vmb->put_op(op_END);
	// patch up locations
	vmb->put_signed(continue_patch1, continue_pos - continue_patch1);
	vmb->put_signed(continue_patch2, continue_pos - continue_patch2);
	vmb->put_signed(break_patch1, break_pos - break_patch1);
	vmb->put_signed(break_patch2, break_pos - break_patch2);
	vmb->put_signed(break_patch3, break_pos - break_patch3);
	vmb->put_signed(first_patch,  first_pos - first_patch);
    }
    else {
	mRange->load(aMachine, vmb);
	vmb->put_op(op_CALL);
	vmb->put_data(lookupBuiltin("size"));  // FIXME!!!
	vmb->put_signed(1);                    // number of args
	vmb->put_op(op_PUSHi);
	vmb->put_signed(0);          // start index
	vmb->put_op(op_BEGIN);
	break_patch1 = vmb->put_signed(0);
	continue_patch1 = vmb->put_signed(0);
	vmb->put_op(op_JUMP);
	check_patch = vmb->put_signed(0);
	continue_pos = vmb->position();
	vmb->put_op(op_IGET);
	vmb->put_op(op_PUSHi);
	vmb->put_signed(1);
	vmb->put_op(op_ADDi);
	vmb->put_op(op_IPUT);
	check_pos = vmb->position();
	vmb->put_op(op_DUP);
	vmb->put_op(op_IGET);
	vmb->put_op(op_CMPi);
	vmb->put_op(op_JUMPle);
	break_patch2 = vmb->put_signed(0);
	mVar->load_ref(aMachine, vmb);
	mRange->load(aMachine, vmb);
	vmb->put_op(op_IGET);
	vmb->put_op(op_GETu);
	putOP(loopType, vmb);
	vmb->put_op(op_DROP);
	mStatement->load(aMachine, vmb);
	vmb->put_op(op_JUMP);
	continue_patch2 = vmb->put_signed(0);
	break_pos = vmb->put_op(op_DROP);
	vmb->put_op(op_END);
	// patch up locations
	vmb->put_signed(break_patch1, break_pos - break_patch1);
	vmb->put_signed(break_patch2, break_pos - break_patch2);
	vmb->put_signed(continue_patch1, continue_pos - continue_patch1);
	vmb->put_signed(continue_patch2, continue_pos - continue_patch2);
	vmb->put_signed(check_patch, check_pos - check_patch);
    }
}

//
// Expressions leave a value so it must be popped (optimise ...)
//   <code>
//   POP
// 
void M1ExprStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    if (mExpr) {
	mExpr->load(aMachine, vmb);
	vmb->put_op(op_DROP);
    }
}

//
// Compound statement
//

void M1CompoundStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    if (mStatements)
	mStatements->load(aMachine, vmb);
}

void M1CallExpr::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    // Should not appear during load
    ERRFMT("runtime error: call instruction in bad context");
}

void M1CaseStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    // Should not appear during load
    ERRFMT("runtime error: case label in bad context");
}

void M1DefaultStatement::load(VmMachineType* aMachine,VmInstructionBuffer* vmb)
{
    // Should not appear during load
    ERRFMT("runtime error: case label in bad context");
}


