//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.

//
// M1 Language type checker
//

#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <iostream>
#include <stack>
#include "m1c.hh"
#include "m1_parse.hh"
#include <algorithm>

using namespace std;

extern string formatDefinitionType(Definition_t def);

// Copy line and file from aCopyElem to a newly created element
// from transformation etc.
static inline M1Element* initElement(M1Element* aElem, M1Element* aCopyElem)
{
    aElem->setLine(aCopyElem->line());
    aElem->setFile(aCopyElem->file());
    return aElem;
}

//
// Wrapper for m1New that copies line and file info to the newly
// create element.
//
#define m1NewElement(copy,type,...)			\
    ((type*) initElement(m1New(type, __VA_ARGS__), (copy)))

#define isConst(expr)	 \
    (((expr)->storage() & Q_CONST) == Q_CONST)
#define isConstant(expr) \
    (isAType(M1Constant*, expr) || isConst((expr)))

//
// Scope checking:
//   global scope (CBindingType)
//         top level object contains all public declarations
//         and also all extern variable declarations
//   local scope (CBindingType)
//         next level top object contains all private declarations
//         on the file scope level.
//   object scope (CEvalType | CMachineType)
//         contains all field declarations for the object beeing
//         defined
//   script scope (CBindingType)
//         contains all local variables in the script
//   compound scope (CBindingType)
//         contains all local compund statement variables
//   ... (repeat)
//
// A variable name is search for from bottom up to find the
// scope level and field declaration.
//
// 

class LintState {
public:
    LintState() {
	mTypeCount = 0; 
	mErrorCount = 0;
	mWarnCount  = 0;
	// mTypeLevel=0 is the global scope (where m1_context() are found)
	mTypeLevel = 0;
	// Note the global fields are used with Type id's starting with .)
	// (e.g .M1  .GUI  .Button ...)
	mTypeStack[0] = (CBaseType*) m1_context().type();
    }

    ~LintState() {
	// FIXME: Check and clear the stack!!!
    }

    void error(M1Element* elem, char* fmt, ...) {
	va_list ap;

	fprintf(stderr, "%s:%d: error: ", elem->file(), elem->line());
	va_start(ap, fmt);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	// FIXME combine error and store in a error list
	mErrorCount++;
    }

    void warning(M1Element* elem, char* fmt, ...) {
	va_list ap;

	fprintf(stderr, "%s:%d: warning: ", elem->file(), elem->line());
	va_start(ap, fmt);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	// FIXME combine error and store in a error list
	mWarnCount++;
    }

    unsigned int nError() { return mErrorCount; }

    void pushScope(CBaseType* aFields) {
	if (mTypeLevel >= MAX_TYPE_DEPTH-1) {
	    fprintf(stderr, "m1c_lint: type stack overflow\n");
	    mErrorCount++;
	}
	else {
	    mTypeLevel++;
	    aFields->setTypeLevel(mTypeLevel);
	    mTypeStack[mTypeLevel] = aFields;
	    // We need to now the "global" context so hook it up here
	    if (mTypeLevel == 1)
		aFields->setGlobalType(mTypeStack[0]);
	    else
		aFields->setGlobalType(mTypeStack[1]);
	}
    }

    CBaseType* popScope(void) {
	if (mTypeLevel < 0) {
	    fprintf(stderr, "m1c_line: scope stack underflow\n");
	    mErrorCount++;
	    return NULL;
	}
	else {
	    CBaseType* aFields = mTypeStack[mTypeLevel];
	    mTypeStack[mTypeLevel] = NULL;
	    mTypeLevel--;
	    return aFields;
	}
    }

    // return current scope level
    int typeLevel(void)  { return mTypeLevel; }

    CField* addDeclaration(CField* aDecl) {
	CBaseType* aFields = mTypeStack[mTypeLevel];
	CField* fld = aFields->addField(aDecl);
	return fld;
    }

    CField* addDeclaration(int aStorage, string aName, CType* aType) {
	CBaseType* aFields = mTypeStack[mTypeLevel];
	CField* fld = aFields->addField(aStorage,aName,aType);
	return fld;
    }

    void addType(int aStorage, string aName, CType* aType) {
	// fprintf(stderr, "addType: %s\n", aName.c_str());
	mTypeStack[mTypeLevel]->addSubType(aStorage,aName,aType);
    }

    void addPostExpression(M1Expr* aExpr) {
	mPostExprList.appendElement(aExpr);
    }

    M1Expr* popPostExpression(void) { return mPostExprList.popElement(); }

    void addPreExpression(M1Expr* aExpr) {
	mPreExprList.appendElement(aExpr);
    }

    M1Expr* popPreExpression(void) { return mPreExprList.popElement();  }

    // Generate a temporary type name
    string tmpTypeName() {
	ostringstream oStream;
	oStream << "%Tx" << mTypeCount;
	mTypeCount++;
	return oStream.str();
    }

    CBaseType* topType() {
	return mTypeStack[mTypeLevel];
    }

    CBaseType* currentType() {
	// Look in the stack for a type beeing defined
	CBaseType* t;
	int i = mTypeLevel;
	while(i >= 0) {
	    t = mTypeStack[i];
	    if (isAType(VmObjectType*, t))
		return t;
	    i--;
	}
	return NULL;
    }

    // Check if a field is declared current type
    bool isOwnMember(CField* fld, CBaseType* defining)
    {
	CBaseType* pType = dynamic_cast<CBaseType*>(defining->parentType());
	if (pType != NULL) {
	    if (fld->index() < (int) pType->fieldCount())
		return false;
	}
	return true;
    }

    // Lookup declaration in current scope only
    CField* findDeclaration(string aId, bool localOnly) {
	CBaseType* scope = mTypeStack[mTypeLevel];
	CField* fld;
	if ((scope != NULL) && ((fld = scope->field(aId)) != NULL)) {
	    if (localOnly && !isOwnMember(fld, scope))
		return NULL;
	    return fld;
	}
	return NULL;
    }

    // When locating an identifier we may locate it in 
    // either CBindingType (local scopes) or in the first VmObjectType
    // (the surrounding type) or in the library or global scope
    CField* lookupDeclaration(string aId,CBaseType* defining,
			      bool &rIsPrivate, int &rScopeLevel) {
	int scopeLevel = 0;
	bool localScope = true;

	if ((aId != "") && (aId[0] == '.'))
	    aId = aId.substr(1);
	else
	    scopeLevel = mTypeLevel;

	// fprintf(stderr, "LOOKUP %s [%d]\n", aId.c_str(), scopeLevel);
	while(scopeLevel >= 0) {
	    CBaseType* scope = mTypeStack[scopeLevel];
	    CField* fld;

	    if (scope) {
		// fprintf(stderr, "  SCOPE %s [%d]\n", scope->name().c_str(), scopeLevel);
		if (localScope || (scopeLevel <= 1)) {
		    if ((fld = scope->field(aId)) != NULL) {
			rIsPrivate = false; // assume not private 
			if ((fld->storage() & Q_PRIVATE) == 0) {
			    rScopeLevel = scopeLevel;
			    return fld;
			}
			else {
			    // mark the last one found as private
			    rIsPrivate = true;
			    if ((scope == defining) && 
				isOwnMember(fld,defining)) {
				rScopeLevel = scopeLevel;
				return fld;
			    }
			}
		    }
		    localScope = isAType(CBindingType*, scope);
		}
	    }
	    scopeLevel--;
	}
	rScopeLevel = -1;
	return NULL;
    }

    // Type names are given with : as scope separator with 
    // an initial : to mark scope 0
    CType* lookupType(string aTypeId) {
	bool libraryType = false;
	string::size_type loc;
	CBaseType* scope = NULL;
	CField*    fld = NULL;
	string     subTypeId;

	if (aTypeId == "")
	    return false;
	if (aTypeId[0] == ':') {
	    aTypeId = aTypeId.substr(1);  // strip the initial :
	    libraryType = true;
	    scope = ((CBaseType*)m1_context().type())->subTypes();
	}

	// Split in Type [ : SubTypes ]
	if ((loc = aTypeId.find(':')) != string::npos) {
	    subTypeId = aTypeId.substr(loc+1);
	    aTypeId   = aTypeId.substr(0, loc);
	}

	// Find a matching field
	if (libraryType) {
	    if (!scope)
		return NULL;
	    fld = scope->field(aTypeId);
	}
	else {
	    int scopeLevel = mTypeLevel;
	    // Find the type in the type scope stack
	    while((scopeLevel >= 0) && !fld) {
		scope = mTypeStack[scopeLevel]->subTypes();
		if (scope != NULL)
		    fld = scope->field(aTypeId);
		scopeLevel--;
	    }
	    if (!fld && (subTypeId == ""))
		return m1TypeLookup(aTypeId);
	}

	while(fld && (subTypeId != "")) {
	    if ((loc = subTypeId.find(':')) != string::npos) {
		aTypeId   = subTypeId.substr(0, loc);
		subTypeId = subTypeId.substr(loc+1);
	    }
	    else {
		aTypeId = subTypeId;
		subTypeId = "";
	    }
	    if ((scope = dynamic_cast<CBaseType*>(fld->type())) == NULL)
		fld = NULL;
	    else {
		scope = scope->subTypes();
		fld = scope->field(aTypeId);
	    }
	}
	if (fld)
	    return fld->type();
	return NULL;
    }

private:
    int mTypeCount;  // generated type index count
    int mTypeLevel;
    CBaseType* mTypeStack[MAX_TYPE_DEPTH];
    int mErrorCount;
    int mWarnCount;
    M1ExprList mPreExprList;  // collect pre expressions like ++x
    M1ExprList mPostExprList;  // collect post expression like x++
};

LintState* gLint;


static bool isInputEvent(CType* t)
{
    return isAType(CEventType*, t) && (((CEventType*)t)->isInput());
}

static bool isOutputEvent(CType* t)
{
    return isAType(CEventType*, t) && (((CEventType*)t)->isOutput());
}

//
// Select most general numeric type of a and b.
// always return signed || unsigned || float || error
// 

CType* mgNumericType(CType* a, CType* b)
{
    M1TypeTag at;
    M1TypeTag bt;

    if ((at = a->typeTag()) == M1TYPE_EVENT) {
	a = ((CEventType*)a)->baseType();
	at = a->typeTag();
    }

    if ((bt = b->typeTag()) == M1TYPE_EVENT) {
	b = ((CEventType*)b)->baseType();
	bt = b->typeTag();
    }

    if ((at & M1T_CLASS_MASK) > M1T_BOOLEAN)
	return error_type();

    if ((bt != M1TYPE_ANY) && ((bt & M1T_CLASS_MASK) > M1T_BOOLEAN))
	return error_type();
    else if (bt == M1TYPE_FLOAT)
	return float_type();

    switch(at) {
    case M1TYPE_FLOAT:
	return float_type();
    case M1TYPE_BYTE:
    case M1TYPE_UNSIGNED:
	if ((bt&M1T_SIGN_MASK) == M1T_UNSIGNED)
	    return unsigned_type();
	return signed_type();
    case M1TYPE_BOOL:
    case M1TYPE_CHAR:
    case M1TYPE_SIGNED:
	return signed_type();
    default:
	return error_type();
    }
}

/* select most general numeric type of a and b (signed||unsigned) */
CType* mgIntegerType(CType* a, CType* b)
{
    M1TypeTag at;
    M1TypeTag bt;

    if ((at = a->typeTag()) == M1TYPE_EVENT) {
	a = ((CEventType*)a)->baseType();
	at = a->typeTag();
    }

    if ((bt = b->typeTag()) == M1TYPE_EVENT) {
	b = ((CEventType*)b)->baseType();
	bt = b->typeTag();
    }

    switch(at) {
    case M1TYPE_BYTE:
    case M1TYPE_UNSIGNED:
	if ((bt&M1T_SIGN_MASK) == M1T_UNSIGNED)
	    return unsigned_type();
	return signed_type();
    case M1TYPE_BOOL:
    case M1TYPE_CHAR:
    case M1TYPE_SIGNED:
	return signed_type();
    default:
	return error_type();
    }
}

// extract event base type if needed
static inline CType* unfoldType(CType* a)
{
    if ((a!=NULL) && isAType(CEventType*, a))
	return ((CEventType*)a)->baseType();
    return a;
}

// check if aType is derived from bType
static bool isDerived(CType* aType, CType* bType)
{
    aType = unfoldType(aType);
    bType = unfoldType(bType);	
    if (aType == bType) // special case for isDerived
	return true;
    if (bType->typeTag() != M1TYPE_OBJECT)
	return false;
    while(aType != NULL) {
	if (aType == bType) 
	    return true;
	aType = unfoldType(aType->parentType());
    }
    return false;
}

static bool isObjectType(CType* aType)
{
    return (unfoldType(aType)->typeTag() == M1TYPE_OBJECT);
}

#if 0
static bool isArrayType(CType* aType)
{
    return (unfoldType(aType)->typeTag() == M1TYPE_ARRAY);
}
#endif

// Check if aType is a subType of bType (derived or castable)
static bool isSubType(CType* aType, CType* bType)
{
    if (isDerived(aType, bType))
	return true;
    if (mgNumericType(aType, bType) != error_type())
	return true;
    return false;
}

// check if two type have same (object type) ancestor
static bool hasCommonBaseType(CType* aType, CType* bType)
{
    aType = unfoldType(aType);
    bType = unfoldType(bType);
    if (aType == bType)   // Special case
	return true;
    if (!isObjectType(bType))
	return false;
    while(aType != NULL) {
	if (isDerived(bType, aType)) {
	    if (aType == VmObjectType::singleton())
		return false;
	    return true;
	}
	aType = unfoldType(aType->parentType());
    }
    return false;
}


// Do type checking
static bool typeCheck(M1Element* aElem, CType* aType, CType* bType)
{
    DBGFMT_TYPE("typeCheck: %s %s", aType->cname(), bType->cname());

    if (aType->typeTag() == M1TYPE_EVENT)
	aType = unfoldType(aType);

    if (bType->typeTag() == M1TYPE_EVENT)
	bType = unfoldType(bType);

    if ((aType == bType) || (aType == any_type()) || (bType == any_type()))
	return true;

    switch(aType->typeTag()) {
    case M1TYPE_ARRAY: {
	CArrayType* at;
	CArrayType* et;
	if (bType->typeTag() != M1TYPE_ARRAY) {
	    gLint->error(aElem, "type mismatch");
	    return false;
	}
	at = (CArrayType*) aType;
	et = (CArrayType*) bType;
	if ((et->arraySize() != 0) && (at->arraySize() != et->arraySize())) {
	    gLint->error(aElem, "array size mismatch");
	    return false;
	}
	return typeCheck(aElem, at->elementType(), et->elementType());
    }
    case M1TYPE_OBJECT: {
	CType* t = aType;
	
	if (bType->typeTag() != M1TYPE_OBJECT) {
	    gLint->error(aElem, "type mismatch");
	    return false; 
	}
	while((t != NULL) && (t != bType)) 
	    t = unfoldType(t->parentType());
	if (t == NULL) {
	    gLint->error(aElem, "'%s' is not derived from '%s'",
			 aType->cname(), bType->cname());
	    return false;
	}
	return true;
    }
    case M1TYPE_BYTE:
    case M1TYPE_CHAR:
    case M1TYPE_BOOL:
    case M1TYPE_SIGNED:
    case M1TYPE_UNSIGNED:
    case M1TYPE_FLOAT:
	switch(bType->typeTag()) {
	case M1TYPE_BYTE:
	case M1TYPE_CHAR:
	case M1TYPE_BOOL:
	case M1TYPE_SIGNED:
	case M1TYPE_UNSIGNED:
	case M1TYPE_FLOAT:
	    return true;
	default:
	    gLint->error(aElem, "type mismatch");
	    return false;
	}
    default:
	gLint->error(aElem, "type mismatch");
	return false;
    }
}


M1ExprIterator::~M1ExprIterator()
{
}

M1Expr* M1ExprIterator::before(M1Expr* aElement)
{
    return aElement;
}

M1Expr* M1ExprIterator::after(M1Expr* aElement)
{
    return aElement;
}


static string formatExpression(M1Expr* expr)
{
    stringstream sexpr;

    expr->print(&sexpr, 0);
    return sexpr.str();
}

//
// M1ExprIterator Class for translating input expression
// retwrite the expression iterated over with new variables
// 
class M1EventRewritor : public M1ExprIterator {
public:
    M1EventRewritor() {
	mICount    = 0; 
	mJCount    = 0; 
	mDecls    = m1New(M1DeclarationList);
	mInitList = m1New(M1ExprList);
    }

    ~M1EventRewritor() {
	/* mDecls/mInitList are garbage collected ! */
    }
    
    bool isOutput(M1Expr* aExpr) {
	return isOutputEvent( aExpr->type() );
    }

    bool isInput(M1Expr* aExpr) {
	return isInputEvent( aExpr->type() );
    }     

    string concat(string prefix, int n) {
	char buf[9];
	sprintf(buf, "%d", n);
	return prefix + string(buf);
    }

    // Handle expressions before going into elements
    M1Expr* before(M1Expr* aExpr) {
	if (isOutput(aExpr)) {
	    // Generate event expression _i<i> = <expr> 
	    string iname;
	    M1Specifier* spec;
	    M1Declarator*    decl;
	    M1FieldDeclaration* field;
	    M1Identifier* id;
	    M1Expr* expr;
	    int storage = aExpr->storage();
            // Internal field name not allowed by parser (avoid name clash)
	    iname = concat("_i", mICount);

	    // generate: input event <type> _i<i>;
	    spec  = m1NewElement(aExpr,M1TypePrimitive,
				 CEventType::create(unfoldType(aExpr->type()),
						    E_INPUT));
	    decl  = m1NewElement(aExpr,M1IdDeclarator, iname);
	    field = m1NewElement(aExpr,M1FieldDeclaration, storage,
				 spec, decl, 0, NULL);
	    mDecls->appendElement(field);

	    // generate: _i<i> <- expr;
	    id   = m1NewElement(aExpr, M1Identifier, iname);
	    expr = m1NewElement(aExpr, M1BinaryExpr, op_CNCT, id, aExpr);
	    mInitList->appendElement(expr);
	    mICount++;
	    // replace the aExpr with the id
	    return id;
	}
	else if (isInput(aExpr)) {
	    gLint->warning(aExpr, "input event used in connect expression");
	}
	else if (isAType(M1Field*, aExpr) || isAType(M1Identifier*, aExpr)) {
		// We may relax this if the object is a library global !!!
		 // && isAType(M1This*, ((M1Field*)aExpr)->object())) {
	    // rewrite this.x into _p<j> = this.x
	    string              iname;
	    M1Specifier*        spec;
	    M1Declarator*       decl;
	    M1FieldDeclaration* field;
	    M1Identifier*       id;
	    M1Expr*             expr;
	    int storage = aExpr->storage();
            // Internal field name not allowed by parser (avoid name clash)
	    iname = concat("_p", mJCount);

	    // generate: <type> _p<j>;
	    spec  = m1NewElement(aExpr,M1TypePrimitive, aExpr->type()),
	    decl  = m1NewElement(aExpr,M1IdDeclarator, iname);
	    field = m1NewElement(aExpr,M1FieldDeclaration, storage,
				 spec, decl, 0, NULL);
	    mDecls->appendElement(field);

	    // generate: _p<j> = expr;
	    id   = m1NewElement(aExpr, M1Identifier, iname);
	    expr = m1NewElement(aExpr, M1BinaryExpr, op_PUT, id, aExpr);
	    mInitList->appendElement(expr);
	    mJCount++;
	    // replace the aExpr with the id
	    return id;
	}
	return aExpr;
    };

    //
    // We could probably generate the internal type directly, but
    // this code build a parse tree possibly usable for compilation...
    //
    M1Expr* after(M1Expr* aExpr) {
	if (isOutput(aExpr)) {
	    // Generate event expression _i<i> = <expr> 
	    string iname;
	    M1Specifier* spec;
	    M1Declarator*    decl;
	    M1FieldDeclaration* field;
	    M1Identifier* id;
	    M1Expr* expr;
	    int storage = aExpr->storage();
            // Internal field name not allowed by parser (avoid name clash)
	    iname = concat("_i", mICount);

	    // generate: input event <type> _i<i>;
	    spec  = m1NewElement(aExpr,M1TypePrimitive,
				 CEventType::create(unfoldType(aExpr->type()),
						    E_INPUT));
	    decl  = m1NewElement(aExpr,M1IdDeclarator, iname);
	    field = m1NewElement(aExpr,M1FieldDeclaration, storage,
				 spec, decl, 0, NULL);
	    mDecls->appendElement(field);

	    // generate: _i<i> <- expr;
	    id   = m1NewElement(aExpr, M1Identifier, iname);
	    expr = m1NewElement(aExpr, M1BinaryExpr, op_CNCT, id, aExpr);
	    mInitList->appendElement(expr);
	    mICount++;
	    // replace the aExpr with the id
	    return id;
	}
	else if (isInput(aExpr)) {
	    gLint->warning(aExpr, "input event used in connect expression");
	}
	return aExpr;
    }

    // Given the rewritten expression generate the VmEval type
    CType* makeType(string aTypeName, M1Expr* aExpr, CType* oType) {
	M1Specifier* spec;
	M1Declarator*    decl;
	M1FieldDeclaration* field;
	M1Identifier* id;
	M1Statement* stat;
	M1Statement* body;
	M1Script* script;
	M1ScriptList* scriptList;
	VmEvalType* t;
	M1StatementList* slist;
	M1StatementList* construct;
	M1StatementList* defaults;
	CLintInfo info;
	int storage = aExpr->storage();

	// add the output declaration
	spec  = m1NewElement(aExpr, M1TypePrimitive, 
			     CEventType::create(unfoldType(oType),E_OUTPUT));
	decl  = m1NewElement(aExpr, M1IdDeclarator, "_out");
	field = m1NewElement(aExpr,M1FieldDeclaration, storage,
			     spec, decl, 0, NULL);
	mDecls->appendElement(field);

	// the script and the script list
	id   = m1NewElement(aExpr,M1Identifier, "_out");
	slist = m1NewElement(aExpr,M1StatementList);
	stat  = m1NewElement(aExpr,M1ExprStatement,
			     m1NewElement(aExpr,M1BinaryExpr,op_PUT,id,aExpr));
	slist->appendElement(stat);
	body   = m1NewElement(aExpr,M1CompoundStatement,
			      m1NewElement(aExpr,M1DeclarationList),
			      slist);
	script = m1NewElement(aExpr,M1Script,"+",NULL,NULL,body);

	scriptList = m1NewElement(aExpr,M1ScriptList);
	scriptList->appendElement(script);

	t = m1New(VmEvalType,T_TYPE);
	t->setParent(VmObjectType::singleton());
	t->setName(aTypeName);
	gLint->addType(Q_PRIVATE, aTypeName, t);
	gLint->pushScope(t);
	defaults  = new M1StatementList;
	mDecls->lint(defaults, &info);
	scriptList->lint();

	// Create a constructor
	construct = new M1StatementList;
	// We MUST make a copy here!
	stat  = m1NewElement(aExpr,M1ExprStatement,
			     m1NewElement(aExpr,M1BinaryExpr,op_PUT,
					  m1NewElement(aExpr,M1UnaryExpr,
						       op_SUPRESS, id),
					  aExpr));
	construct->appendElement(stat);
	construct->lint(&info);
	
	t->setDefaults(defaults);
	t->setConstructor(construct);
	t->constructorFrameSize(info.maxFrameSize());
	t->setPreScript(scriptList);
	return gLint->popScope();
    }
    
    M1DeclarationList* declarations() { return mDecls; }
    M1ExprList* initList()            { return mInitList; }

private:
    int mICount;
    int mJCount;
    M1DeclarationList* mDecls;
    M1ExprList* mInitList;
};

// rewrite the rhs of a connect (e.g  <- x+1)
// lhs MUST be an input event
// rhs can be any expression, if the expression is
// a single output event expression of the same base type
// as output they are simply connected. Otherwise a wrapper
// type is generated to handle the expression.

CType* connectExpression(M1Expr* expr, M1Expr** aExpr, CType* aType)
{
    string typeName = gLint->tmpTypeName();
    CType* rtype;              // new type
    M1EventRewritor* rewrite;  // rewrite event expression
    M1ObjectConstant* cons;    // new object constant expression
    M1Expr* rhs;
    M1Expr* cexpr = NULL;

    DBGFMT_LINT("connectExpression.1 [%s]", 
		formatExpression(expr).c_str());

    // iterate over expression and rewrite the connect expression
    expr->iterate(&cexpr, rewrite = new M1EventRewritor);

    DBGFMT_LINT("connectExpression.2 [%s]", 
		formatExpression(cexpr).c_str());

    rtype = rewrite->makeType(typeName, cexpr, aType); // vm eval type
    // cons = @_Txi { _i0 <- x0,  ... _in <- xn } */
    cons = m1NewElement(expr, M1ObjectConstant, 
			m1NewElement(expr, M1TypeIdentifier, typeName),
			rewrite->initList());
    // rhs = (@_Txi { .. })._out
    rhs = m1NewElement(expr, M1BinaryExpr, op_FLD, cons,
		       m1NewElement(expr, M1Identifier, "_out"));
    // m1CondSetRetainExpr(aExpr, rhs);
    // *aExpr = m1RetainExpr(rhs);

    DBGFMT_LINT("connectExpression.3 [%s]", 
		formatExpression(rhs).c_str());

    delete rewrite;

    return rhs->lint(aExpr, aType);
}




// Return the type code for a type
int typeCode(CType* t)
{
    switch(unfoldType(t)->typeTag()) {
    case M1TYPE_BYTE:   return M1CVT_byte;
    case M1TYPE_CHAR:   return M1CVT_char;
    case M1TYPE_BOOL:   return M1CVT_bool;
    case M1TYPE_SIGNED: return M1CVT_signed;
    case M1TYPE_UNSIGNED: return M1CVT_unsigned;
    case M1TYPE_FLOAT:  return M1CVT_float;
    default: return 0;
    }
}
//
// When a element is read it's upconverted to a
// word type 
//        byte => unsigned
//        char/bool => signed
// This is mapped to GETx operations
//
CType* readType(CType* from)
{
    from = unfoldType(from);
    switch(from->typeTag()) {
    case M1TYPE_BYTE:     return unsigned_type();
    case M1TYPE_CHAR:     return signed_type();
    case M1TYPE_BOOL:     return signed_type();
    case M1TYPE_SIGNED:   return signed_type();
    case M1TYPE_UNSIGNED: return unsigned_type();
    case M1TYPE_FLOAT:    return float_type();
    case M1TYPE_STRING:   return string_type();
    default: return from;
    }
}

//
// Generate type conversion code for member write access
// Accepted from code are 
//
M1CvtCode typeWriteAccessCode(CType* from, CType* to)
{
    switch(unfoldType(from)->typeTag()) {
    case M1TYPE_SIGNED:
	switch(unfoldType(to)->typeTag()) {
	case M1TYPE_BYTE:     return M1CVT_iu;
	case M1TYPE_UNSIGNED: return M1CVT_iu;
	case M1TYPE_FLOAT:    return M1CVT_if;
	default: return M1CVT_nn;
	}

    case M1TYPE_UNSIGNED:
	switch(unfoldType(to)->typeTag()) {
	case M1TYPE_CHAR:   return M1CVT_ui;
	case M1TYPE_BOOL:   return M1CVT_ui;
	case M1TYPE_SIGNED: return M1CVT_ui;
	case M1TYPE_FLOAT:  return M1CVT_uf;
	default: return M1CVT_nn;
	}

    case M1TYPE_FLOAT:
	switch(unfoldType(to)->typeTag()) {
	case M1TYPE_BYTE:   return M1CVT_fu;
	case M1TYPE_CHAR:   return M1CVT_fi;
	case M1TYPE_BOOL:   return M1CVT_fi;
	case M1TYPE_SIGNED: return M1CVT_fi;
	case M1TYPE_UNSIGNED: return M1CVT_fu;
	default: return M1CVT_nn;
	}
    default: 
	return M1CVT_nn;
    }
}

VmEvalType* lintDefinitions(M1TypeSpecifier* spec)
{
    CType* type;

    gLint = new LintState;

    type = spec->lint(Q_PUBLIC);

    if (gLint->nError() == 0) {
	delete gLint;
	return (VmEvalType*) type;
    }
    else {
	delete gLint;
	return NULL;
    }
}



static unsigned long char_constant(char* cptr, char** end_ptr)
{
    unsigned long v = 0;
    size_t n = 0;

    if (end_ptr)
	*end_ptr = cptr;

    if (*cptr != '\'')
	return 0;
    cptr++;
    if (*cptr == '\\') {
	cptr++;
	switch(*cptr) {
	case 'n': v='\n'; break;
	case 't': v='\t'; break;
	case 'v': v='\v'; break;
	case 'b': v='\b'; break;
	case 'r': v='\r'; break;
	case 'f': v='\f'; break;
	case 'a': v='\a'; break;
	case '\\': v='\\'; break;
	case '?': v='\?'; break;
	case '\'': v='\''; break;
	case '\"': v='\"'; break;
	case 'x':
	    cptr++;
	    while(isxdigit(*cptr)) {
		if (*cptr <= '9')
		    v = (v << 4) | (*cptr - '0');
		else if (*cptr <= 'F')
		    v = (v << 4) | (10+(*cptr - 'A'));
		else if (*cptr <= 'f')
		    v = (v << 4) | (10+(*cptr - 'a'));
		cptr++;
		n++;
	    }
	    if (n > 4) {
		if (end_ptr) *end_ptr = cptr;
		return 0;
	    }
	    break;
	    
	case '0': case '1':case '2':case '3':
	case '4': case '5': case '6':case '7': // octal number
	    while((*cptr >= '0') && (*cptr <= '7')) {
		v = (v << 3) | (*cptr - '0');
		cptr++;
		n++;
	    }
	    if (n != 3) {
		if (end_ptr) *end_ptr = cptr;
		return v;
	    }
	    break;
	    
	default:
	    v = *cptr++;
	    break;
	}
    }
    else {
	while(*cptr != '\'') { // allow 32 bit wide char constants
	    v = (v << 8) | *cptr;
	    cptr++;
	    n++;
	}
	if (n > sizeof(int)) {
	    if (end_ptr) *end_ptr = cptr+1;
	    return v;
	}
    }
    if (end_ptr) *end_ptr = cptr+1;
    return v;
}

long m1NumericToLong(char* cptr, char** end_ptr)
{
    if (*cptr == '\'')
	return (long) char_constant(cptr, end_ptr);
    else
	return strtol(cptr, end_ptr, 0);
}

unsigned long m1NumericToULong(char* cptr, char** end_ptr)
{
    if (*cptr == '\'')
	return (unsigned long) char_constant(cptr, end_ptr);
    else
	return strtoul(cptr, end_ptr, 0);
}

float m1NumericToFloat(char* cptr, char** end_ptr)
{
    return strtof(cptr, end_ptr);
}

M1Constant* createConstant(CType* aType, UData v)
{
    switch(aType->typeTag()) {
    case M1TYPE_BYTE:     return m1New(M1ByteConstant, v.b);
    case M1TYPE_CHAR:     return m1New(M1CharConstant, v.c);
    case M1TYPE_BOOL:     return m1New(M1BoolConstant, v.t); 
    case M1TYPE_SIGNED:   return m1New(M1SignedConstant, v.i); 
    case M1TYPE_UNSIGNED: return m1New(M1SignedConstant, v.u); 
    case M1TYPE_FLOAT:    return m1New(M1FloatConstant, v.f); 
    case M1TYPE_STRING:   return m1New(M1StringConstant, v.str);
    default: return NULL;
    }
}

UData m1Convert(UData r, CType* from, CType* to)
{
    if (to == any_type())  // no conversion needed
	return r;
    if (isAType(CEventType*, to))
	to = ((CEventType*)to)->baseType();
    if (isAType(CEventType*, from))
	from = ((CEventType*)from)->baseType();
    if (from == to)
	return r;
    switch(from->typeTag()) {
    case M1TYPE_FLOAT:
	switch(to->typeTag()) {
	case M1TYPE_FLOAT:    r.f = float(r.f); break;
	case M1TYPE_SIGNED:   r.i = int(r.f); break;
	case M1TYPE_UNSIGNED: r.u = (unsigned int) r.f; break;
	case M1TYPE_BOOL:     r.t = r.f; break;
	case M1TYPE_CHAR:     r.c = (signed char) r.f; break;
	case M1TYPE_BYTE:     r.b = (unsigned char) r.f; break;
	default: break;
	}
	break;
    case M1TYPE_SIGNED:
	switch(to->typeTag()) {
	case M1TYPE_FLOAT:    r.f = (float) r.i; break;
	case M1TYPE_SIGNED:   r.i = r.i; break;
	case M1TYPE_UNSIGNED: r.u = r.i; break;
	case M1TYPE_BOOL:     r.t = r.i; break;
	case M1TYPE_CHAR:     r.c = r.i; break;
	case M1TYPE_BYTE:     r.b = r.i; break;
	default: break;
	}
	break;
    case M1TYPE_UNSIGNED:
	switch(to->typeTag()) {
	case M1TYPE_FLOAT:    r.f = (float) r.u; break;
	case M1TYPE_SIGNED:   r.i = r.u; break;
	case M1TYPE_UNSIGNED: r.u = r.u; break;
	case M1TYPE_BOOL:     r.t = r.u; break;
	case M1TYPE_CHAR:     r.c = r.u; break;
	case M1TYPE_BYTE:     r.b = r.u; break;
	default: break;
	}
	break;

    case M1TYPE_BOOL:
	switch(to->typeTag()) {
	case M1TYPE_FLOAT:    r.f = (float) r.t; break;
	case M1TYPE_SIGNED:   r.i = r.t; break;
	case M1TYPE_UNSIGNED: r.u = r.t; break;
	case M1TYPE_BOOL:     r.t = r.t; break;
	case M1TYPE_CHAR:     r.c = r.t; break;
	case M1TYPE_BYTE:     r.b = r.t; break;
	default: break;
	}
	break;

    case M1TYPE_CHAR:
	switch(to->typeTag()) {
	case M1TYPE_FLOAT:    r.f = (float) r.c; break;
	case M1TYPE_SIGNED:   r.i = r.c; break;
	case M1TYPE_UNSIGNED: r.u = r.c; break;
	case M1TYPE_BOOL:     r.t = r.c; break;
	case M1TYPE_CHAR:     r.c = r.c; break;
	case M1TYPE_BYTE:     r.b = r.c; break;
	default: break;
	}
	break;

    case M1TYPE_BYTE:
	switch(to->typeTag()) {
	case M1TYPE_FLOAT:    r.f = (float) r.b; break;
	case M1TYPE_SIGNED:   r.i = r.b; break;
	case M1TYPE_UNSIGNED: r.u = r.b; break;
	case M1TYPE_BOOL:     r.t = r.b; break;
	case M1TYPE_CHAR:     r.c = r.b; break;
	case M1TYPE_BYTE:     r.b = r.b; break;
	default: break;
	}
	break;
    default:
	break;  // No conversion done
    }
    return r;
}

// Convert a constant to the given type
static M1Constant* convertConstant(M1Constant* from, CType* toType)
{
    UData v;

    toType = unfoldType(toType);
    if ((toType == any_type()) || (from->type() == toType))
	return from;
    v = m1Convert(from->value(), from->type(), toType);
    return createConstant(toType, v);
}

// Convert from type to type code
int M1CVT_type(CType* t) 
{
    switch(t->typeTag()) {
    case M1TYPE_BYTE: return M1CVT_byte;
    case M1TYPE_CHAR: return M1CVT_char;
    case M1TYPE_BOOL: return M1CVT_bool;
    case M1TYPE_SIGNED: return M1CVT_signed;
    case M1TYPE_UNSIGNED: return M1CVT_unsigned;
    case M1TYPE_FLOAT: return M1CVT_float;
    default: return 0;
    }
}

// Generate a conversion node
M1Expr* convertExpr(M1Expr* expr, CType* fromType, CType* toType)
{
    if (isAType(M1Constant*, expr))
	return convertConstant((M1Constant*) expr, toType);
    else {
	int fcode = M1CVT_type(fromType);
	int tcode = M1CVT_type(toType);
	M1CvtCode cvt = (M1CvtCode) M1CVT_code(fcode, tcode);

	if ((fcode == tcode) || (fcode == 0) || (tcode == 0))
	    return expr;
	expr = m1NewElement(expr, M1CvtExpr, cvt, expr);
	expr->setType(toType);
	return expr;
    }
}

// Convert operator into specific typed operators
int convertOperator(int op, M1TypeTag tag)
{
    switch(op) {
    case op_ADD:
	switch(tag) {
	case M1TYPE_SIGNED:   return op_ADDi;
	case M1TYPE_UNSIGNED: return op_ADDu;
	case M1TYPE_FLOAT:    return op_ADDf;
	case M1TYPE_STRING:   return op_ADDs;
	case M1TYPE_ARRAY:    return op_ADDa;
	default: break;
	}
	break;
    case op_SUB:
	switch(tag) {
	case M1TYPE_SIGNED:   return op_SUBi;
	case M1TYPE_UNSIGNED: return op_SUBu;
	case M1TYPE_FLOAT:    return op_SUBf;
	case M1TYPE_STRING:   return op_SUBs;
	case M1TYPE_ARRAY:    return op_SUBa;
	default: break;
	}
	break;

    case op_MUL:
	switch(tag) {
	case M1TYPE_SIGNED:   return op_MULi;
	case M1TYPE_UNSIGNED: return op_MULu;
	case M1TYPE_FLOAT:    return op_MULf;
	default: break;
	}
	break;

    case op_DIV:
	switch(tag) {
	case M1TYPE_SIGNED:   return op_DIVi;
	case M1TYPE_UNSIGNED: return op_DIVu;
	case M1TYPE_FLOAT:    return op_DIVf;
	default: break;
	}
	break;

    case op_REM:
	switch(tag) {
	case M1TYPE_SIGNED:   return op_REMi;
	case M1TYPE_UNSIGNED: return op_REMu;
	default: break;
	}
	break;

    case op_BAND:
	switch(tag) {
	case M1TYPE_UNSIGNED: return op_BANDu;
	default: break;
	}
	break;
	
    case op_BOR:
	switch(tag) {
	case M1TYPE_UNSIGNED: return op_BORu;
	default: break;
	}
	break;

    case op_BXOR:
	switch(tag) {
	case M1TYPE_UNSIGNED: return op_BXORu;
	default: break;
	}
	break;

    case op_BSL:
	switch(tag) {
	case M1TYPE_UNSIGNED: return op_BSLu;
	case M1TYPE_SIGNED:   return op_BSLi;
	default: break;
	}
	break;

    case op_BSR:
	switch(tag) {
	case M1TYPE_SIGNED: return op_BSRi;
	case M1TYPE_UNSIGNED: return op_BSRu;
	default: break;
	}
	break;

    case op_PUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_PUTb;
	case M1TYPE_CHAR:     return op_PUTc;
	case M1TYPE_BOOL:     return op_PUTt;
	case M1TYPE_SIGNED:   return op_PUTi;
	case M1TYPE_UNSIGNED: return op_PUTu;
	case M1TYPE_FLOAT:    return op_PUTf;
	case M1TYPE_STRING:   return op_PUTs;
	case M1TYPE_OBJECT:   return op_PUTo;
	case M1TYPE_ARRAY:    return op_PUTo;
	default: break;
	}
	break;	

    case op_COPY:
	switch(tag) {
	case M1TYPE_BYTE:     return op_PUTb;
	case M1TYPE_CHAR:     return op_PUTc;
	case M1TYPE_BOOL:     return op_PUTt;
	case M1TYPE_SIGNED:   return op_PUTi;
	case M1TYPE_UNSIGNED: return op_PUTu;
	case M1TYPE_FLOAT:    return op_PUTf;
	case M1TYPE_STRING:   return op_PUTs;
	case M1TYPE_OBJECT:   return op_COPY;
	case M1TYPE_ARRAY:    return op_COPY;
	default: break;
	}
	break;	


    case op_MULPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_MULPUTb;
	case M1TYPE_CHAR:     return op_MULPUTc;
	case M1TYPE_BOOL:     return op_MULPUTt;
	case M1TYPE_SIGNED:   return op_MULPUTi;
	case M1TYPE_UNSIGNED: return op_MULPUTu;
	case M1TYPE_FLOAT:    return op_MULPUTf;
	default: break;
	}
	break;

    case op_DIVPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_DIVPUTb;
	case M1TYPE_CHAR:     return op_DIVPUTc;
	case M1TYPE_BOOL:     return op_DIVPUTt;
	case M1TYPE_SIGNED:   return op_DIVPUTi;
	case M1TYPE_UNSIGNED: return op_DIVPUTu;
	case M1TYPE_FLOAT:    return op_DIVPUTf;
	default: break;
	}
	break;

    case op_ADDPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_ADDPUTb;
	case M1TYPE_CHAR:     return op_ADDPUTc;
	case M1TYPE_BOOL:     return op_ADDPUTt;
	case M1TYPE_SIGNED:   return op_ADDPUTi;
	case M1TYPE_UNSIGNED: return op_ADDPUTu;
	case M1TYPE_FLOAT:    return op_ADDPUTf;
	case M1TYPE_STRING:   return op_ADDPUTs;
	case M1TYPE_ARRAY:    return op_ADDPUTa;
	default: break;
	}
	break;

    case op_SUBPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_SUBPUTb;
	case M1TYPE_CHAR:     return op_SUBPUTc;
	case M1TYPE_BOOL:     return op_SUBPUTt;
	case M1TYPE_SIGNED:   return op_SUBPUTi;
	case M1TYPE_UNSIGNED: return op_SUBPUTu;
	case M1TYPE_FLOAT:    return op_SUBPUTf;
	case M1TYPE_STRING:   return op_SUBPUTs;
	case M1TYPE_ARRAY:    return op_SUBPUTa;
	default: break;
	}
	break;

    case op_REMPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_REMPUTb;
	case M1TYPE_CHAR:     return op_REMPUTc;
	case M1TYPE_BOOL:     return op_REMPUTt;
	case M1TYPE_SIGNED:   return op_REMPUTi;
	case M1TYPE_UNSIGNED: return op_REMPUTu;
	default: break;
	}
	break;

    case op_BANDPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_BANDPUTb;
	case M1TYPE_CHAR:     return op_BANDPUTc;
	case M1TYPE_BOOL:     return op_BANDPUTt;
	case M1TYPE_SIGNED:   return op_BANDPUTi;
	case M1TYPE_UNSIGNED: return op_BANDPUTu;
	default: break;
	}
	break;


    case op_BXORPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_BXORPUTb;
	case M1TYPE_CHAR:     return op_BXORPUTc;
	case M1TYPE_BOOL:     return op_BXORPUTt;
	case M1TYPE_SIGNED:   return op_BXORPUTi;
	case M1TYPE_UNSIGNED: return op_BXORPUTu;
	default: break;
	}
	break;

    case op_BORPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_BORPUTb;
	case M1TYPE_CHAR:     return op_BORPUTc;
	case M1TYPE_BOOL:     return op_BORPUTt;
	case M1TYPE_SIGNED:   return op_BORPUTi;
	case M1TYPE_UNSIGNED: return op_BORPUTu;
	default: break;
	}
	break;

    case op_BSLPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_BSLPUTb;
	case M1TYPE_CHAR:     return op_BSLPUTc;
	case M1TYPE_BOOL:     return op_BSLPUTt;
	case M1TYPE_SIGNED:   return op_BSLPUTi;
	case M1TYPE_UNSIGNED: return op_BSLPUTu;
	default: break;
	}
	break;

    case op_BSRPUT:
	switch(tag) {
	case M1TYPE_BYTE:     return op_BSRPUTb;
	case M1TYPE_CHAR:     return op_BSRPUTc;
	case M1TYPE_BOOL:     return op_BSRPUTt;
	case M1TYPE_SIGNED:   return op_BSRPUTi;
	case M1TYPE_UNSIGNED: return op_BSRPUTu;
	default: break;
	}
	break;

    case op_NEG:
	switch(tag) {
	case M1TYPE_SIGNED:   return op_NEGi;
	case M1TYPE_UNSIGNED: return op_NEGu;
	case M1TYPE_FLOAT:    return op_NEGf;
	default: break;
	}
	break;

    case op_BNOT:
	switch(tag) {
	case M1TYPE_UNSIGNED: return op_BNOTu;
	default: break;
	}
	break;

    case op_NOT:
	switch(tag) {
	case M1TYPE_SIGNED: return op_NOTi;
	default: break;
	}
	break;

    case op_PSTINC:
    case op_PREINC:
	switch(tag) {
	case M1TYPE_BYTE:     return op_INCb;
	case M1TYPE_CHAR:     return op_INCc;
	case M1TYPE_BOOL:     return op_INCt;
	case M1TYPE_SIGNED:   return op_INCi;
	case M1TYPE_UNSIGNED: return op_INCu;
	case M1TYPE_FLOAT:    return op_INCf;
	default: break;
	}
	break;

    case op_PSTDEC:
    case op_PREDEC:
	switch(tag) {
	case M1TYPE_BYTE:     return op_DECb;
	case M1TYPE_CHAR:     return op_DECc;
	case M1TYPE_BOOL:     return op_DECt;
	case M1TYPE_SIGNED:   return op_DECi;
	case M1TYPE_UNSIGNED: return op_DECu;
	case M1TYPE_FLOAT:    return op_DECf;
	default: break;
	}
	break;

    case op_AND:
	return op_ANDi;

    case op_OR:
	return op_ORi;

    default:
	break;
    }
    return 0;
}


UData uEval(int op, M1Constant* mid, CType* t)
{
    UData mv = m1Convert(mid->value(), mid->type(), t);
    UData v;
    switch(op) {
    case op_BNOT: IUNARY_OP(~,t,v,mv); break;
    case op_NEG:  FUNARY_OP(-,t,v,mv); break;
    case op_NOT:  IUNARY_OP(-,t,v,mv); break;
    }
    return v;
}

M1Expr* unaryEval(int op, M1Constant* mid, CType* t)
{
    UData v = uEval(op, mid, t);
    return createConstant(t, v);
}

//
// Evaluate binary expression with two constant operands
// Both operands are converted to the target type t
// Then the value is calculated.
//
UData bEval(int op, M1Constant* left, M1Constant* right, CType* t)
{
    UData lv = m1Convert(left->value(),  left->type(), t);
    UData rv = m1Convert(right->value(), right->type(), t);
    UData v;

    switch(op) {
    case op_ADD:  FBINARY_OP(+, t, v, lv, rv); break;
    case op_SUB:  FBINARY_OP(-, t, v, lv, rv); break;
    case op_MUL:  FBINARY_OP(*, t, v, lv, rv); break;
    case op_DIV:  FBINARY_OP(/, t, v, lv, rv); break;
    case op_REM:  IBINARY_OP(%, t, v, lv, rv); break;
    case op_OR:   FBINARY_OP(||, t, v, lv, rv); break;
    case op_AND:  FBINARY_OP(&&, t, v, lv, rv); break;
    case op_BAND: IBINARY_OP(&, t, v, lv, rv); break;
    case op_BOR:  IBINARY_OP(|, t, v, lv, rv); break;
    case op_BXOR: IBINARY_OP(&, t, v, lv, rv); break;
    }
    return v;
}

// Evaluate binary releational expression with two constant operands
// Both operands are converted to the target type t
// Then the value is calculated to a signed result.
//
UData rEval(int op, M1Constant* left, M1Constant* right, CType* t)
{
    UData lv = m1Convert(left->value(),  left->type(), t);
    UData rv = m1Convert(right->value(), right->type(), t);
    UData v;

    switch(op) {
    case op_LT:  RBINARY_OP(<,  t, v, lv, rv); break; 
    case op_GT:  RBINARY_OP(>,  t, v, lv, rv); break;
    case op_LTE: RBINARY_OP(<=, t, v, lv, rv); break;
    case op_GTE: RBINARY_OP(>=, t, v, lv, rv); break;
    case op_EQ:  RBINARY_OP(==, t, v, lv, rv); break;
    case op_NEQ: RBINARY_OP(!=, t, v, lv, rv); break;
    case op_EQL: RBINARY_OP(==, t, v, lv, rv); break;
    case op_NQL: RBINARY_OP(!=, t, v, lv, rv); break;
    }
    return v;
}

// Evaluate binary shift expression with two constant operands
// left operands is converted to the target type t and
// right is converted to signed_type
// Then the value is calculated to a target result.
//
UData sEval(int op, M1Constant* left, M1Constant* right, CType* t)
{
    UData lv = m1Convert(left->value(),  left->type(), t);
    UData rv = m1Convert(right->value(), right->type(), signed_type());
    UData v;

    switch(op) {
    case op_BSL: SBINARY_OP(<<, t, v, lv, rv); break;
    case op_BSR: SBINARY_OP(<<, t, v, lv, rv); break;
    }
    return v;
}

M1Expr* binaryEval(int op, M1Constant* left, M1Constant* right, CType* t)
{
    UData v = bEval(op, left, right, t);
    return createConstant(t, v);
}

M1Expr* shiftEval(int op, M1Constant* left, M1Constant* right, CType* t)
{
    UData v = sEval(op, left, right, t);
    return createConstant(t, v);
}


CType* lintResult(M1Expr** aExpr, M1Expr* rExpr, CType* aType)
{
    m1CondSetRetainExpr(aExpr, rExpr);
    rExpr->setIsLinted(true);
    return rExpr->setType(aType);
}

// M1Expr
//   default handling of expression linting
//
CType* M1Expr::lint_ref(M1Expr** aExpr, CType* aType)
{
    gLint->error(this, "reference expression [%s] not allowed (not handled)", 
		 formatExpression(this).c_str());
    // mType = aType;
    return lintResult(aExpr, this, aType);
}

//
void M1Expr::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;

    if ((expr = iter->before(this)) == this)
	expr = iter->after(this);
    m1CondSetRetainExpr(aExpr, expr);
}

CType* M1Expr::lint(M1Expr** aExpr, CType* aType)
{
    gLint->error(this, "expression [%s] not allowed (not handled)",
		 formatExpression(this).c_str());
    return lintResult(aExpr, this, aType);
}

CType* M1Expr::lint_trg(M1Expr** aExpr)
{
    gLint->error(this, "expression [%s] is not a trigger expression",
		 formatExpression(this).c_str());
    return lintResult(aExpr, this, bool_type());
}


void M1ExprList::iterate(M1ExprIterator* iter)
{
    list<M1Expr*>::iterator liter = mList.begin();
    while (liter != mList.end()) {
	(*liter)->iterate(&*liter, iter);
	liter++;
    }
}

//
// identifier must have been declared at this point
// note that an identifier is always a field to something.
//
CType* M1Identifier::lint(M1Expr** aExpr, CType* aType)
{
    CType* type;
    CType* rtype = NULL;
    M1Expr* expr = this;
    CField* decl;
    CBaseType* defining = gLint->currentType();
    int scopeLevel = -1;
    int eValue = 0;
    bool isPrivate = false;

    if (isAType(CEnumerationType*, aType) &&
	((CEnumerationType*)aType)->enumValue(name(), eValue)) {
	DBGFMT_LINT("M1Identifier::lint() %s, enum=%d, type=%s",
	       cname(), eValue, aType->cname());
	expr = m1NewElement(this,M1SignedConstant,eValue);
	rtype = type = signed_type();
    }
    else if (isAType(CEventType*, aType) &&
	     isAType(CEnumerationType*, ((CEventType*)aType)->baseType()) &&
	     ((CEnumerationType*)((CEventType*)aType)->baseType())->enumValue(name(), eValue)) {
	DBGFMT_LINT("M1Identifier::lint() %s, enum=%d, type=%s",
	       cname(), eValue, aType->cname());
	expr = m1NewElement(this,M1SignedConstant,eValue);
	rtype = type = signed_type();
    }
    else if ((decl = gLint->lookupDeclaration(name(),defining,
					      isPrivate, scopeLevel)) == NULL) {
	if (isPrivate)
	    gLint->error(this,"identifier '%s' is private", cname());
	else
	    gLint->error(this," identifier '%s' not declared", cname());
	decl = gLint->addDeclaration(Q_NONE, name(), any_type());
	mLevel = SCOPE_NONE;
	rtype = type = any_type();
	mIndex     = decl->index();
    }
    else {
	type  = decl->type();
	rtype = readType(type);
	typeCheck(this, rtype, aType);
	if (isConst(decl))
	    expr = createConstant(type, decl->constant());
	else {
	    // Convert local type member to  this.id
	    if (defining && (defining->typeLevel()  == scopeLevel))
		expr = m1NewElement(this,M1Field,m1NewElement(this,M1This),
				    name(),decl->index());
	    else {
		if (decl->stackPos() >= 0) {
		    mLevel = SCOPE_STACK;
		    mIndex = decl->stackPos();
		}
		else {
		    switch(scopeLevel) {
		    case 0: mLevel = SCOPE_UNIVERSE; break;
		    case 1: mLevel = SCOPE_GLOBAL; break;
		    default: mLevel = SCOPE_OBJECT; break;
		    }
		    mIndex = decl->index();
		}
	    }
	}
	expr->setStorage(decl->storage());
    }
    DBGFMT_LINT("M1Identifier::lint() %s[%d:%d],type=%s,restr=%s",
		cname(), scopeLevel, mIndex,
		type->cname(), aType->cname());
    // Note the separation between the returned type and the stored type!!!
    lintResult(aExpr, expr, type);
    return rtype;
}

CType* M1Identifier::lint_trg(M1Expr** aExpr)
{
    CField* decl;
    M1Expr* expr = this;
    int scopeLevel;
    CBaseType* defining = gLint->currentType();
    bool isPrivate = false;

    if ((decl = gLint->lookupDeclaration(name(),defining,
					 isPrivate,scopeLevel)) == NULL) {
	if (isPrivate)
	    gLint->error(this,"identifier '%s' is private", cname());
	else
	    gLint->error(this,"identifier '%s' not declared", cname());
	decl = gLint->addDeclaration(Q_PUBLIC, name(), input_bool_type());
	mLevel = SCOPE_NONE;
	mIndex      = decl->index();
    }
    else {
	mIndex     = decl->index();
	if (!isInputEvent(decl->type()))
	    gLint->error(this,"identifier '%s' is not an input event", cname());
	else if ((defining != NULL) && (defining->typeLevel() == scopeLevel))
	    expr = m1NewElement(this,M1Field,m1NewElement(this,M1This),
				name(),decl->index());
	else {
	    gLint->error(this,"identifier '%s' is not a local event", cname());
	    mLevel = SCOPE_NONE;
	    mIndex = decl->index();
	}
    }
    DBGFMT_LINT("M1Identifier::lint_trg() %s[%d:%d]", 
		cname(), scopeLevel, decl->index());
    return lintResult(aExpr, expr, bool_type());
}

CType* M1Identifier::lint_ref(M1Expr** aExpr, CType* aType)
{
    CType* type;
    M1Expr* expr = this;
    CField* decl;
    int scopeLevel;
    CBaseType* defining = gLint->currentType();
    bool isPrivate = false;

    if ((decl = gLint->lookupDeclaration(name(),defining,
					 isPrivate, scopeLevel))==NULL) {
	if (isPrivate)
	    gLint->error(this,"identifier '%s' is private", cname());
	else
	    gLint->error(this," identifier '%s' not declared", cname());
	decl = gLint->addDeclaration(Q_NONE, name(), any_type());
	mLevel = SCOPE_NONE;
	type   = any_type();
	mIndex = decl->index();	
    }
    else {
	typeCheck(this, decl->type(), aType);

	if ((defining!= NULL) && (defining->typeLevel() == scopeLevel))
	    expr = m1NewElement(this,M1Field, m1NewElement(this,M1This),
				name(), decl->index());
	else {
	    if (decl->stackPos() >= 0) {
		mLevel = SCOPE_STACK;
		mIndex = decl->stackPos();
	    }
	    else {
		switch(scopeLevel) {
		case 0: mLevel = SCOPE_UNIVERSE; break;
		case 1: mLevel = SCOPE_GLOBAL; break;
		default: mLevel = SCOPE_OBJECT; break;
		}
		mIndex = decl->index();
	    }
	}
	type = decl->type();
    }
    expr->setStorage(decl->storage());

    DBGFMT_LINT("M1Identifier::lint_ref() %s[%d:%d],type=%s,type=%s",
		cname(), scopeLevel, decl->index(),
		decl->type()->cname(), aType->cname());
    return lintResult(aExpr, expr, type);
}

// Base identifier
CType* M1BasedIdentifier::lint(M1Expr** aExpr, CType* aType)
{
    CType* type;
    M1Expr* expr = this;
    CBaseType* objType;
    
    if (mTypeId) {
	CType* t = mTypeId->lint(Q_NONE);
	if (!isAType(CBaseType*, t)) {
	    gLint->error(this, "type '%s' is not a object type", t->cname());
	    mIndex = -1;
	    return lintResult(aExpr, expr, signed_type());
	}
	objType = (CBaseType*) t;
    }
    else
	objType = gLint->currentType();

    if ((mIndex = objType->indexAt(mId)) < 0) {
	if (objType != error_type())
	    gLint->error(this, "'%s' has no member named '%s'",
			 objType->cname(), mId.c_str());
	type = any_type();
    }
    else {
	CField* decl = objType->field(mIndex);
	type = decl->type();
	typeCheck(this, type, aType);
	if (isConst(decl))
	    expr = createConstant(type, decl->constant());
	expr->setStorage(decl->storage());
    }
    return lintResult(aExpr, expr, type);
}

CType* M1BasedIdentifier::lint_ref(M1Expr** aExpr, CType* aType)
{
    return lint(aExpr, aType);
}


// Field index expression &[Type:].field
CType* M1Index::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    CBaseType* objType;
    
    if (mTypeId) {
	CType* t = mTypeId->lint(Q_NONE);
	if (!isAType(CBaseType*, t)) {
	    gLint->error(this, "type '%s' is not a object type",
			 mTypeId->cname());
	    mIndex = -1;
	    return lintResult(aExpr, expr, signed_type());
	}
	objType = (CBaseType*) t;
    }
    else
	objType = gLint->currentType();

    if ((mIndex = objType->indexAt(mId)) < 0) {
	if (objType != error_type())
	    gLint->error(this, "'%s' has no member named '%s'",
			 objType->cname(), mId.c_str());
    }
    // FIXME: type check signed_type() with aType...
    return lintResult(aExpr, expr, signed_type());
}

void M1Field::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mObject->iterate(&mObject, iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);
}

CType* M1Field::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    M1Expr* obj  = NULL;
    CType*  type;
    CType*  rtype;
    CType*  t;
    CField* decl;

    t = mObject->lint(&obj, any_type());
    if (!isAType(CBaseType*, t)) {
	if (t != error_type())
	    gLint->error(this, "'%s' does not have any members", t->cname());
	return lintResult(aExpr, expr, aType);
    }
    else if ((decl = ((CBaseType*)t)->field(mId)) == NULL) {
	if (t != error_type())
	    gLint->error(this, "'%s' has no member named '%s'",
			 t->cname(), mId.c_str());
	return lintResult(aExpr, expr, aType);
    }
    type  = decl->type();
    rtype = readType(type);
    typeCheck(this, rtype, aType);    
    if (isConst(decl))
	expr = createConstant(type, decl->constant());
    else if (obj != mObject)
	expr = m1NewElement(this, M1Field, obj, mId, mIndex);
    lintResult(aExpr, expr, type);
    return rtype;
}

CType* M1Field::lint_ref(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    M1Expr* obj = NULL;
    CType*  t;
    CField* decl;

    t = mObject->lint(&obj, any_type());
    if (!isAType(CBaseType*, t)) {
	if (t != error_type())
	    gLint->error(this, "'%s' does not have any members", t->cname());
	return lintResult(aExpr, expr, aType);
    }
    else if ((decl = ((CBaseType*)t)->field(mId)) == NULL) {
	if (t != error_type())
	    gLint->error(this, "'%s' has no member named '%s'",
			 t->cname(), mId.c_str());
	return lintResult(aExpr, expr, aType);
    }
    if (obj != mObject)
	expr = m1NewElement(this, M1Field, obj, mId, mIndex);
    typeCheck(this, decl->type(), aType);
    return lintResult(aExpr, expr, type());
}

CType* M1Field::lint_trg(M1Expr** aExpr)
{
    M1Expr* expr = this;
    M1Expr* obj  = NULL;
    CType*  t;
    CField* decl;

    t = mObject->lint(&obj, any_type());

    if (!isAType(CBaseType*, t)) {
	if (t != error_type())
	    gLint->error(this, "'%s' does not have any members", t->cname());
	return lintResult(aExpr, expr, bool_type());
    }
    else if ((decl = ((CBaseType*)t)->field(mId)) == NULL) {
	if (t != error_type())
	    gLint->error(this, "'%s' has no member named '%s'",
			 t->cname(), mId.c_str());
	return lintResult(aExpr, expr, bool_type());
    }
    if (!isInputEvent(decl->type()))
	gLint->error(this,"identifier '%s' is not an input event", mId.c_str());
    if (obj != mObject)
	expr = m1NewElement(this, M1Field, obj, mId, mIndex);
    return lintResult(aExpr, expr, bool_type());
}


CType* M1BoolConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Constant* expr;

    DBGFMT_LINT("M1BoolConstant::lint() called type=%s",aType->cname());
    if ((expr = convertConstant(this, aType)) == this)
	return lintResult(aExpr, expr, bool_type());
    return lintResult(aExpr, expr, unfoldType(aType));
}

CType* M1BoolConstant::lint_trg(M1Expr** aExpr)
{
    M1Constant* expr = this;
    DBGFMT_LINT("M1BoolConstant::lint_trg() called");
    return lintResult(aExpr, expr, bool_type());
}

CType* M1SignedConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Constant* expr;
    DBGFMT_LINT("M1SignedConstant::lint() called type=%s",aType->cname());
    if (!typeCheck(this, signed_type(), aType))
	return aType;
    if ((expr = convertConstant(this, aType)) == this)
	return lintResult(aExpr, expr, signed_type());
    return lintResult(aExpr, expr, unfoldType(aType));
}

CType* M1UnsignedConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Constant* expr;
    DBGFMT_LINT("M1UnsignedConstant::lint() called type=%s",aType->cname());
    if (!typeCheck(this, unsigned_type(), aType))
	return aType;
    if ((expr = convertConstant(this, aType)) == this)
	return lintResult(aExpr, expr, unsigned_type());
    return lintResult(aExpr, expr, unfoldType(aType));
}

CType* M1CharConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Constant* expr;
    DBGFMT_LINT("M1CharConstant::lint() called type=%s",aType->cname());
    if (!typeCheck(this, char_type(), aType))
	return aType;
    if ((expr = convertConstant(this, aType)) == this)
	return lintResult(aExpr, expr, char_type());
    return lintResult(aExpr, expr, unfoldType(aType));
}

CType* M1ByteConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Constant* expr;
    DBGFMT_LINT("M1ByteConstant::lint() called type=%s",aType->cname());
    if (!typeCheck(this, byte_type(), aType))
	return aType;
    if ((expr = convertConstant(this, aType)) == this)
	return lintResult(aExpr, expr, byte_type());
    return lintResult(aExpr, expr, unfoldType(aType));
}

CType* M1FloatConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Constant* expr;
    DBGFMT_LINT("M1FloatConstant::lint() called type=%s",aType->cname());
    if (!typeCheck(this, float_type(), aType))
	return aType;
    if ((expr = convertConstant(this, aType)) == this)
	return lintResult(aExpr, expr, float_type());
    return lintResult(aExpr, expr, unfoldType(aType));
}

CType* M1StringConstant::lint(M1Expr** aExpr, CType* aType)
{
    CType* t = unfoldType(aType);
    DBGFMT_LINT("M1StringConstant::lint() called type=%s", aType->cname());
    if (!typeCheck(this, string_type(), aType))
	return aType;
    if ((t == any_type()) || (t->typeTag() == M1TYPE_STRING))
	return lintResult(aExpr, this, string_type());
    gLint->error(this, "type mismatch, type %s expected", aType->cname());
    return lintResult(aExpr, this, string_type());
}

CType* M1Nil::lint(M1Expr** aExpr, CType* aType)
{
    DBGFMT_LINT("M1Nil::lint() called aType=%s", aType->cname());
    return lintResult(aExpr, this, any_type());
}

CType* M1Nil::lint_ref(M1Expr** aExpr, CType* aType)
{
    DBGFMT_LINT("M1Nil::lint() called aType=%s", aType->cname());
    return lintResult(aExpr, this, any_type());
}

CType* M1This::lint(M1Expr** aExpr, CType* aType)
{
    CType* t = gLint->currentType();
    DBGFMT_LINT("M1This::lint() called type=%s, aType=%s", 
		t->cname(), aType->cname());
    // FIXME: type check with aType ...
    return lintResult(aExpr, this, t);
}

void M1TrinaryExpr::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;

    if ((expr = iter->before(this)) == this) {
	mFst->iterate(&mFst, iter);
	mSnd->iterate(&mSnd, iter);
	mThrd->iterate(&mThrd, iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);
}


CType* M1TrinaryExpr::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* fst = NULL;
    M1Expr* snd = NULL;
    M1Expr* thrd = NULL;
    CType* type;
    CType*  ft;
    CType*  st;
    CType*  tt;
    M1Expr* expr = this;
    DBGFMT_LINT("M1TrinaryExpr::lint() called aType=%s",aType->cname());

    switch(op()) {
    case op_COND:
	mFst->lint(&fst, bool_type());
	type = mSnd->lint(&snd, aType);
	type = mThrd->lint(&thrd, type);
	if ((fst != mFst) || (snd != mSnd) || (thrd != mThrd))
	    expr = m1NewElement(this, M1TrinaryExpr, op(), fst, snd, thrd);
	return lintResult(aExpr, expr, type);

    case op_PROG2:
	if (mFst)
	    mFst->lint(&fst, any_type());
	type = mSnd->lint(&snd, any_type());
	if (mThrd)
	    mThrd->lint(&thrd, any_type());
	if ((fst != mFst) || (snd != mSnd) || (thrd != mThrd))
	    expr = m1NewElement(this, M1TrinaryExpr, op(), fst, snd, thrd);
	return lintResult(aExpr, expr, type);

    case op_RANGE:
	ft = mFst->lint(&fst, aType);
	fst = convertExpr(fst, ft, aType);

	if (mSnd) {
	    st = mSnd->lint(&snd, aType);
	    snd = convertExpr(snd, st, aType);
	}

	if (mThrd) {
	    if ((aType->typeTag() & M1T_TAG_MASK) == M1T_INTEGER)
		tt = mThrd->lint(&thrd, signed_type());
	    else
		tt = mThrd->lint(&thrd, aType);
	}

	if ((fst != mFst) || (snd != mSnd) || (thrd != mThrd))
	    expr = m1NewElement(this, M1TrinaryExpr, op(), fst, snd, thrd);
	return lintResult(aExpr, expr, aType);
    default:
	gLint->error(this, "bad expression");
	return lintResult(aExpr, expr, any_type());
    }
}

CType* M1TrinaryExpr::lint_ref(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;

    DBGFMT_LINT("M1TrinaryExpr::lint() called aType=%s",aType->cname());

    if (op() == op_COND) {
	M1Expr* fst = NULL;
	M1Expr* snd = NULL;
	M1Expr* thrd = NULL;
	CType* type2;
	CType* type3;

	mFst->lint(&fst, bool_type());
	type2 = mSnd->lint_ref(&snd, aType);
	type3 = mThrd->lint(&thrd, type2);
	if ((fst != mFst) || (snd != mSnd) || (thrd != mThrd))
	    expr = m1NewElement(this, M1TrinaryExpr, op(), fst, snd, thrd);
	return lintResult(aExpr, expr, type2);
    }
    else {
	gLint->error(this, "bad expression");
	return lintResult(aExpr, expr, any_type());
    }
}



void M1BinaryExpr::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mLeft->iterate(&mLeft, iter);
	mRight->iterate(&mRight, iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);
}

static CType* lintBinaryPut(M1Expr* expr, M1Expr** loc, 
			    int op,
			    M1Expr* leftExpr, M1Expr* rightExpr,
			    CType* leftType, CType* rightType)
{
    M1Expr* left  = NULL;
    M1Expr* right = NULL;
    CType*  lt = leftExpr->lint_ref(&left, any_type());
    CType*  rt = rightExpr->lint(&right, any_type());

    if ((unfoldType(lt) != leftType) || (unfoldType(rt) != rightType))
	gLint->warning(expr, "type changed for internal operator %s",
		       formatOperator(op).c_str());
    if ((left != leftExpr) || (right != rightExpr))
	expr = m1NewElement(expr, M1BinaryExpr, op, left, right);
    return lintResult(loc, expr, leftType);
}

static CType* lintBinaryExpr(M1Expr* expr, M1Expr** loc, 
			     int op,
			     M1Expr* leftExpr, M1Expr* rightExpr,
			     CType* leftType, CType* rightType,
			     CType* returnType)
{
    M1Expr* left  = NULL;
    M1Expr* right = NULL;
    CType*  lt = leftExpr->lint(&left, any_type());
    CType*  rt = rightExpr->lint(&right, any_type());

    if ((unfoldType(lt) != leftType) || (unfoldType(rt) != rightType))
	gLint->warning(expr, "type changed for internal operator %s",
		       formatOperator(op).c_str());
    if ((left != leftExpr) || (right != rightExpr))
	expr = m1NewElement(expr, M1BinaryExpr, op, left, right);
    return lintResult(loc, expr, returnType);
}

static CType* lintBinaryCmp(M1Expr* expr, M1Expr** loc, 
			    int op,
			    M1Expr* leftExpr, M1Expr* rightExpr,
			    CType* leftType, CType* rightType)
{
    M1Expr* left  = NULL;
    M1Expr* right = NULL;
    CType*  lt = leftExpr->lint(&left, any_type());
    CType*  rt = rightExpr->lint(&right, any_type());

    if ((leftType != any_type()) && (unfoldType(lt) != leftType))
	gLint->warning(expr, "type changed for internal operator %s",
		       formatOperator(op).c_str());
    else if ((rightType != any_type()) && (unfoldType(rt) != rightType))
	gLint->warning(expr, "type changed for internal operator %s",
		       formatOperator(op).c_str());

    if ((left != leftExpr) || (right != rightExpr))
	expr = m1NewElement(expr, M1BinaryExpr, op, left, right);
    return lintResult(loc, expr, signed_type());
}

// Take care of any pre post expression "buffered" in the lint state
CType* lintPrePost(M1Expr* expr, M1Expr** loc, CType* rtype)
{
    M1Expr* x  = NULL;
    M1Expr* pre   = NULL;
    M1Expr* post  = NULL;
    M1Expr* e;
    CType* t;

    t = expr->lint(&x, rtype);
	
    // construct pre1, pre2, .. expr, post1 .. postn
    while((e = gLint->popPreExpression())) {
	if (pre == NULL)
	    pre = e;
	else
	    pre = m1NewElement(expr, M1BinaryExpr, op_SEQ, e, pre);
	pre->setType(any_type());
	pre->setIsLinted(true);
    }
    
    while((e = gLint->popPostExpression())) {
	if (post == NULL)
	    post = e;
	else 
	    post = m1NewElement(expr, M1BinaryExpr, op_SEQ, e, post);
	post->setType(any_type());
	post->setIsLinted(true);
    }

    if ((pre != NULL) || (post != NULL)) {
	x = m1NewElement(expr,M1TrinaryExpr,op_PROG2, pre, x, post);
	x->setType(any_type());
    }

    if (expr != x)
	m1SetRetainExpr(loc, x);
    x->setIsLinted(true);
    return t;
}
	    


CType* M1BinaryExpr::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* left = NULL;
    M1Expr* right = NULL;
    M1Expr* expr  = this;
    CType* lt;
    CType* rt;
    CType* mt;
    CType* rdtype;

    DBGFMT_LINT("M1BinaryExpr:lint: ");
    if (M1DBG_IS_SET(M1DBG_LINT|M1DBG_PRNT)) {
	printf("M1BinaryExpr:lint: ");
	this->print(&cout, 0);
	printf("\n");
    }

    setIsLinted(true);
    
    switch(op()) {
    case op_INIT:
    case op_PUT:
    case op_COPY: {
	int op1 = op();
	lt = mLeft->lint_ref(&left, any_type());
	rt = mRight->lint(&right, lt);
	if ((op() != op_INIT) && isConst(left))
	    gLint->error(this, "assignment of read-only location");
	else 
	    typeCheck(this, lt, aType);
	if (op1 == op_INIT)
	    op1 = op_PUT;
	switch(unfoldType(lt)->typeTag()) {
	case M1TYPE_STRING:
	    /* always copy strings */
	    if (isAType(M1Identifier*, left) &&
		(((M1Identifier*)left)->activationType() == SCOPE_STACK))
		op1 = op_PUTo;
	    else
		op1 = op_COPY; 
	    break;
	case M1TYPE_ARRAY:
	    /* always copy fixed arrays */
	    if ((op1 == op_PUT) && (((CArrayType*) lt)->arraySize() > 0))
		op1 = op_COPY;
	    else if (op1 == op_PUT)
		op1 = op_PUTo;
	    break;
	default: {
	    M1CvtCode cvt;
	    /* do we need write conversion for numeric value ? */
	    if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
		right = m1NewElement(this, M1CvtExpr, cvt, right);
	    // convert both COPY/PUT => PUTx for numerical values
	    op1 = convertOperator(op1, unfoldType(lt)->typeTag());
	    break;
	}
	    
	}
	if ((op() != op1) || (left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	return lintResult(aExpr, expr, lt);
    }

    case op_ADDPUT: {
	CType* ult;
	CType* urt;
	int op1 = op();
	int m1t;
	M1CvtCode cvt;

	lt = mLeft->lint_ref(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	if (isConst(left))
	    gLint->error(this, "assignment of read-only location");
	else if (isAType(CArrayType*, ult) &&
		 isAType(CArrayType*, urt) &&
		 isSubType(((CArrayType*)urt)->elementType(),
			   ((CArrayType*)ult)->elementType())) {
	    if (((CArrayType*)ult)->arraySize() != 0)
		gLint->error(this, "lhs array must be dynamic");
	    op1 = op_ADDPUTa;
	}
	else if (isAType(CArrayType*, ult) &&
		 isSubType(urt, ((CArrayType*)ult)->elementType())) {
	    if (((CArrayType*)ult)->arraySize() != 0)
		gLint->error(this, "lhs array must be dynamic");
	    op1 = op_ADDPUTae;
	}
	else if ((ult == string_type()) && isSubType(urt, char_type())) {
	    if ((cvt = typeWriteAccessCode(rt, char_type())) != M1CVT_nn)
		right = m1NewElement(this, M1CvtExpr, cvt, right);
	    op1 = op_ADDPUTsc;
	}
	else {
	    typeCheck(this, lt, aType);
	    typeCheck(this, rt, lt);
	    m1t = ult->typeTag() & M1T_CLASS_MASK;
	    if ((m1t != M1T_STRING) && (m1t != M1T_NUMERIC) && 
		(m1t != M1T_BOOLEAN)) {
		gLint->error(this, "bad operand types for operator '%s'",
			     formatOperator(op()).c_str());
	    }
	    op1 = convertOperator(op(), ult->typeTag());
	    if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
		right = m1NewElement(this, M1CvtExpr, cvt, right);
	}
	if ((op() != op1) || (left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	return lintResult(aExpr, expr, lt);
    }

    case op_SUBPUT: {
	CType* ult;
	CType* urt;
	int op1 = op();
	int m1t;
	M1CvtCode cvt;

	lt = mLeft->lint_ref(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	if (isConst(left))
	    gLint->error(this, "assignment of read-only location");
	else if (isAType(CArrayType*, ult) &&
		 isAType(CArrayType*, urt) &&
		 isSubType(((CArrayType*)urt)->elementType(),
			   ((CArrayType*)ult)->elementType())) {
	    if (((CArrayType*)ult)->arraySize() != 0)
		gLint->error(this, "lhs array must be dynamic");
	    op1 = op_SUBPUTa;
	}
	else if (isAType(CArrayType*, ult) &&
		 isSubType(urt, ((CArrayType*)ult)->elementType())) {
	    if (((CArrayType*)ult)->arraySize() != 0)
		gLint->error(this, "lhs array must be dynamic");
	    op1 = op_SUBPUTae;
	}
	else if ((ult == string_type()) && isSubType(urt, char_type())) {
	    if ((cvt = typeWriteAccessCode(rt, char_type())) != M1CVT_nn)
		right = m1NewElement(this, M1CvtExpr, cvt, right);
	    op1 = op_SUBPUTsc;
	}
	else {
	    typeCheck(this, lt, aType);
	    typeCheck(this, rt, lt);
	    m1t = ult->typeTag() & M1T_CLASS_MASK;
	    if ((m1t != M1T_STRING) && (m1t != M1T_NUMERIC) &&
		(m1t != M1T_BOOLEAN)) {
		gLint->error(this, "bad operand types for operator '%s'",
			     formatOperator(op()).c_str());
	    }
	    op1 = convertOperator(op(), ult->typeTag());
	    if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
		right = m1NewElement(this, M1CvtExpr, cvt, right);
	}
	if ((op() != op1) || (left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	return lintResult(aExpr, expr, lt);
    }

    case op_MULPUT:
    case op_DIVPUT: {
	int op1 = op();
	int m1t;
	M1CvtCode cvt;

	lt = mLeft->lint_ref(&left, any_type());
	rt = mRight->lint(&right, lt);
	if (isConst(left))
	    gLint->error(this, "assignment of read-only location");
	else
	    typeCheck(this, lt, aType);

	m1t = unfoldType(lt)->typeTag() & M1T_CLASS_MASK;
	if ((m1t != M1T_NUMERIC) || (m1t != M1T_BOOLEAN)) {
	    gLint->error(this, "bad operand types for operator '%s'",
			 formatOperator(op()).c_str());
	}
	op1 = convertOperator(op(), unfoldType(lt)->typeTag());
	if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
	    right = m1NewElement(this, M1CvtExpr, cvt, right);
	if ((op() != op1) || (left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	return lintResult(aExpr, expr, lt);
    }

    case op_REMPUT:
    case op_BANDPUT:
    case op_BXORPUT:
    case op_BORPUT: {
	int op1 = op();
	int m1t;
	M1CvtCode cvt;

	lt = mLeft->lint_ref(&left, aType);
	rt = mRight->lint(&right, lt);
	typeCheck(this, rt, lt);
	m1t = unfoldType(lt)->typeTag() & M1T_TAG_MASK;
	if (m1t != M1T_INTEGER) {
	    gLint->error(this, "bad operand types for operator '%s'",
			 formatOperator(op()).c_str());
	}
	op1 = convertOperator(op(), unfoldType(lt)->typeTag());
	if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
	    right = m1NewElement(this, M1CvtExpr, cvt, right);
	if ((op() != op1) || (left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	return lintResult(aExpr, expr, lt);
    }

    case op_BSLPUT:
    case op_BSRPUT: {
	int op1 = op();
	int m1t;
	M1CvtCode cvt;

	lt = mLeft->lint_ref(&left, aType);
	rt = mRight->lint(&right, signed_type());
	typeCheck(this, rt, lt);

	m1t = unfoldType(lt)->typeTag() & M1T_TAG_MASK;
	if (m1t != M1T_INTEGER) {
	    gLint->error(this, "bad operand types for operator '%s'",
			 formatOperator(op()).c_str());
	}

	if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
	    right = m1NewElement(this, M1CvtExpr, cvt, right);
	op1 = convertOperator(op(), unfoldType(lt)->typeTag());
	if ((op() != op1) || (left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	return lintResult(aExpr, expr, lt);
    }

    case op_CNCT:
	lt = mLeft->lint_ref(&left, aType);
	// FIXME: add special code for single variable | nil 
	mRight->lint(&right, lt);
        // NOTE we are not using the returned type, since it's stripped
	// from information about events and subtypes.
	rt = right->type();

	if (!isInputEvent(lt))
	    gLint->error(this, "bad connect destination");
	else if (!isAType(M1Nil*, right)) {
	    if (!isOutputEvent(rt)) {
		M1Expr* cexpr = NULL;
		rt = connectExpression(right,&cexpr,lt);
		right = cexpr;
	    }
	    else if (((CEventType*) rt)->baseType() !=
		     ((CEventType*) lt)->baseType()) {
		M1Expr* cexpr = NULL;
      
#ifdef DEBUG
		gLint->warning(this, "type conversion in connect expression");
#endif
		// if (right != mRight) m1RetainExpr(right);
		rt = connectExpression(right,&cexpr,lt);
		right = cexpr;
	    }
	}
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, lt);

    case op_FLD: {
	string fname;
	CField* fld;

	lt = mLeft->lint(&left, any_type());  // The object
	fname = ((M1Identifier*) mRight)->name();
	lt = unfoldType(lt);
	if (!isAType(CBaseType*, lt)) {
	    if (lt != error_type())
		gLint->error(this, "%s does not have any members", lt->cname());
	    return lintResult(aExpr, expr, aType);
	}
	else if ((fld = ((CBaseType*)lt)->field(fname)) == NULL) {
	    if (lt != error_type())
		gLint->error(this, "'%s' has no member named '%s'",
			     lt->cname(), fname.c_str());
	    return lintResult(aExpr, expr, aType);
	}
	rdtype = readType(fld->type());
	if (((fld->storage() & Q_PRIVATE) != 0) &&
	    (lt != gLint->currentType())) {
	    // FIXME allow access to library privates from types being defined
	    //  in the library!?
	    gLint->error(this, "'%s' member '%s' is private",
			 lt->cname(), fname.c_str());
	    return lintResult(aExpr, expr, aType);
	}
	if (isConst(fld))
	    expr = createConstant(rdtype, fld->constant());
	else
	    expr = m1NewElement(this, M1Field, left, fname, fld->index());
	setStorage(fld->storage());
	expr->setStorage(fld->storage());
	typeCheck(this, rdtype, aType);
	lintResult(aExpr, expr, fld->type());
	return rdtype;
    }

    case op_ELEM:
	// FIXME: warn for constant index out of range
	lt = mLeft->lint(&left, any_type()); // The array var/expr
	rt = mRight->lint(&right, signed_type()); // The index
	if ((mLeft != left) || (mRight != right))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	lt = unfoldType(lt);
	if (isAType(CArrayType*, lt))
	    mt = ((CArrayType*) lt)->elementType();
	else if (lt == string_type())
	    mt = char_type();
	else {
	    gLint->error(this, "bad array type %s", lt->cname());
	    mt = any_type();
	}
	rdtype = readType(mt);
	typeCheck(this, rdtype, aType);
	lintResult(aExpr, expr, mt);
	return rdtype;

    case op_ELEMS:
	// FIXME: warn for constant index out of range
	lt = mLeft->lint(&left, any_type()); // The array var/expr
	rt = mRight->lint(&right, signed_type()); // The index range
	if ((mLeft != left) || (mRight != right))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	lt = unfoldType(lt);
	if (isAType(CArrayType*, lt))
	    mt = lt;
	else if (lt == string_type())
	    mt = string_type();
	else {
	    gLint->error(this, "bad array type %s", lt->cname());
	    mt = any_type();
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_SUBs:
    case op_ADDs:
	typeCheck(this, string_type(), aType);
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, string_type(), string_type(),
			      string_type());
    case op_SUBsc:
    case op_ADDsc:
	typeCheck(this, string_type(), aType);
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, string_type(), char_type(),
			      string_type());
    case op_ADDcs:
	typeCheck(this, string_type(), aType);
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, char_type(), string_type(),
			      string_type());

    case op_SUBa:
    case op_ADDa: {
	CType* ult;
	CType* urt;
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	mt = lt;
	if (isAType(CArrayType*, ult) && isAType(CArrayType*, urt) &&
		 hasCommonBaseType(((CArrayType*)ult)->elementType(),
				   ((CArrayType*)urt)->elementType())) {
	    // FIXME: return type should []
	    typeCheck(this, mt, aType);
	}
	else {
	    gLint->error(this, "bad operands to binary operator %s",
			 formatOperator(op()).c_str());
	}
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(expr, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, mt);
    }

    case op_SUBae:
    case op_ADDae: {
	CType* ult;
	CType* urt;
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	mt = lt;
	if (isAType(CArrayType*, ult) &&
	    isSubType(urt, ((CArrayType*)ult)->elementType())) {
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	    // FIXME: return type should []
	    typeCheck(this, mt, aType);
	}
	else {
	    gLint->error(this, "bad operands to binary operator %s",
			 formatOperator(op()).c_str());
	}
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(expr, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, mt);
    }	
	
    case op_ADDea: {
	CType* ult;
	CType* urt;
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	mt = lt;
	if (isAType(CArrayType*, urt) &&
	    isSubType(ult, ((CArrayType*)urt)->elementType())) {
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	    // FIXME: return type should []
	    typeCheck(this, mt, aType);
	}
	else {
	    gLint->error(this, "bad operands to binary operator %s",
			 formatOperator(op()).c_str());
	}
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(expr, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, mt);
    }

    case op_ADDPUTa:
    case op_SUBPUTa: {
	CType* ult;
	CType* urt;
	lt  = mLeft->lint_ref(&left, any_type());
	rt  = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	mt = lt;
	if (isAType(CArrayType*, ult) &&
	    isAType(CArrayType*, urt) &&
	    isSubType(((CArrayType*)urt)->elementType(),
		      ((CArrayType*)ult)->elementType())) {
	    if (((CArrayType*)ult)->arraySize() != 0)
		gLint->error(this, "lhs array must be dynamic");
	    typeCheck(this, mt, aType);
	}
	else {
	    gLint->error(this, "bad operands to assignment operator %s",
			 formatOperator(op()).c_str());
	}
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(expr, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, mt);
    }


    case op_ADDPUTae:
    case op_SUBPUTae: {
	CType* ult;
	CType* urt;
	lt  = mLeft->lint_ref(&left, any_type());
	rt  = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);
	mt = lt;
	if (isAType(CArrayType*, ult) &&
	    isSubType(urt, ((CArrayType*)ult)->elementType())) {
	    if (((CArrayType*)ult)->arraySize() != 0)
		gLint->error(this, "lhs array must be dynamic");
	    typeCheck(this, mt, aType);
	}
	else {
	    gLint->error(this, "bad operands to assignment operator %s",
			 formatOperator(op()).c_str());
	}
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(expr, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, mt);
    }
	
    case op_ADDi:
    case op_SUBi:
    case op_MULi:
    case op_DIVi:
    case op_REMi:
    case op_ANDi:
    case op_ORi:
	typeCheck(this, signed_type(), aType);	
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, signed_type(), signed_type(),
			      signed_type());
    case op_ADDu:
    case op_SUBu:
    case op_MULu:
    case op_DIVu:
    case op_REMu:
    case op_BANDu:
    case op_BORu:
    case op_BXORu:
	typeCheck(this, unsigned_type(), aType);	
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, unsigned_type(), unsigned_type(),
			      unsigned_type());
    case op_ADDf:
    case op_SUBf:
    case op_MULf:
    case op_DIVf:
	typeCheck(this, float_type(), aType);
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, float_type(), float_type(),
			      float_type());
    case op_BSLu:
    case op_BSRu:
	typeCheck(this, unsigned_type(), aType);
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, unsigned_type(), signed_type(),
			      unsigned_type());
    case op_BSLi:
    case op_BSRi:
	typeCheck(this, signed_type(), aType);
	return lintBinaryExpr(this, aExpr, op(),
			      mLeft, mRight, signed_type(), signed_type(),
			      signed_type());

    case op_SUBPUTsc:
    case op_ADDPUTsc:
	typeCheck(this, string_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,string_type(),char_type());
    case op_SUBPUTs:
    case op_ADDPUTs:
	typeCheck(this, string_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,string_type(),string_type());

    case op_MULPUTu:
    case op_DIVPUTu:
    case op_ADDPUTu:
    case op_SUBPUTu:
    case op_REMPUTu:
    case op_BANDPUTu:
    case op_BXORPUTu:
    case op_BORPUTu:
	typeCheck(this, unsigned_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,unsigned_type(),unsigned_type());
    case op_MULPUTi:
    case op_DIVPUTi:
    case op_ADDPUTi:
    case op_SUBPUTi:
    case op_REMPUTi:
    case op_BANDPUTi:
    case op_BXORPUTi:
    case op_BORPUTi:
	typeCheck(this, signed_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft, mRight, signed_type(), signed_type());
    case op_MULPUTb:
    case op_DIVPUTb:
    case op_ADDPUTb:
    case op_SUBPUTb:
    case op_REMPUTb:
    case op_BANDPUTb:
    case op_BXORPUTb:
    case op_BORPUTb:
	typeCheck(this, byte_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,byte_type(),byte_type());
    case op_MULPUTc:
    case op_DIVPUTc:
    case op_ADDPUTc:
    case op_SUBPUTc:
    case op_REMPUTc:
    case op_BANDPUTc:
    case op_BXORPUTc:
    case op_BORPUTc:
	typeCheck(this, char_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,char_type(),char_type());
    case op_MULPUTt:
    case op_DIVPUTt:
    case op_ADDPUTt:
    case op_SUBPUTt:
    case op_REMPUTt:
    case op_BANDPUTt:
    case op_BXORPUTt:
    case op_BORPUTt:
	typeCheck(this, bool_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,bool_type(),bool_type());
    case op_MULPUTf:
    case op_DIVPUTf:
    case op_ADDPUTf:
    case op_SUBPUTf:
	typeCheck(this, float_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,float_type(),float_type());
    case op_BSLPUTt:
    case op_BSRPUTt:
	typeCheck(this, bool_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,bool_type(),signed_type());
    case op_BSLPUTc:
    case op_BSRPUTc:
	typeCheck(this, char_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,char_type(),signed_type());
    case op_BSLPUTb:
    case op_BSRPUTb:
	typeCheck(this, byte_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,byte_type(),signed_type());
    case op_BSLPUTu:
    case op_BSRPUTu:
	typeCheck(this, unsigned_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,unsigned_type(),signed_type());
    case op_BSLPUTi:
    case op_BSRPUTi:
	typeCheck(this, signed_type(), aType);
	return lintBinaryPut(this, aExpr, op(),
			     mLeft,mRight,signed_type(),signed_type());

    case op_CMPi:
	typeCheck(this, signed_type(), aType);
	return lintBinaryCmp(this, aExpr, op(),
			     mLeft,mRight,signed_type(),signed_type());
    case op_CMPu:
	typeCheck(this, signed_type(), aType);
	return lintBinaryCmp(this, aExpr, op(),
			     mLeft,mRight,unsigned_type(),unsigned_type());
    case op_CMPf:
	typeCheck(this, signed_type(), aType);
	return lintBinaryCmp(this, aExpr, op(),
			     mLeft,mRight,float_type(),float_type());
    case op_CMPo:
	typeCheck(this, signed_type(), aType);
	return lintBinaryCmp(this, aExpr, op(),
			     mLeft,mRight,any_type(),any_type());
    case op_ADD: {
	CType* ult;
	CType* urt;
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);

	if (isAType(M1StringConstant*, left) &&
	    isAType(M1StringConstant*, right)) {
	    UData lv = left->eval(NULL);
	    UData rv = right->eval(NULL);
	    UData r;
	    r.str = m1New(CString, lv.str->str() + rv.str->str());
	    expr = createConstant(string_type(), r);
	    mt = string_type();
	}
	else if (isAType(M1StringConstant*, left) &&
		 isAType(M1Constant*, right) && 
		 ((rt->typeTag() & M1T_TAG_MASK) == M1T_INTEGER)) {
	    UData lv = left->eval(NULL);
	    UData rv = right->eval(NULL);
	    UData r;
	    rv = m1Convert(rv, rt, char_type());
	    r.str = m1New(CString, lv.str->str() + rv.c);
	    expr = createConstant(string_type(), r);
	    mt = string_type();
	}
	else if (isAType(M1Constant*, left) && 
		 ((lt->typeTag() & M1T_TAG_MASK) == M1T_INTEGER) &&
		 isAType(M1StringConstant*, right)) {
	    UData lv = left->eval(NULL);
	    UData rv = right->eval(NULL);
	    UData r;
	    lv = m1Convert(lv, lt, char_type());
	    r.str = m1New(CString, lv.c + rv.str->str());
	    expr = createConstant(string_type(), r);
	    mt = string_type();
	}
	else if (ult == string_type() && isSubType(urt, char_type())) {
	    right = convertExpr(right, urt, char_type());
	    expr = m1NewElement(this, M1BinaryExpr, op_ADDsc, left, right);
	    mt = string_type();
	}
	else if (isSubType(ult, char_type()) &&	(urt == string_type())) {
	    left = convertExpr(left, ult, char_type());
	    expr = m1NewElement(this, M1BinaryExpr, op_ADDcs, left, right);
	    mt = string_type();
	}
	else if ((ult==string_type()) && (urt==string_type())) {
	    expr = m1NewElement(this, M1BinaryExpr, op_ADDs, left, right);
	    mt = string_type();
	}
	else if (isAType(CArrayType*, ult) && isAType(CArrayType*, urt) &&
		 hasCommonBaseType(((CArrayType*)ult)->elementType(),
				   ((CArrayType*)urt)->elementType())) {
	    expr = m1NewElement(this, M1BinaryExpr, op_ADDa, left, right);
	    // FIXME: type should be [size(a)+size(b)] or []
	    mt = lt;
	}
	else if (isAType(CArrayType*, ult) &&
		 isSubType(urt, ((CArrayType*)ult)->elementType())) {
	    // FIXME: type should be [size(a)+1] or []
	    expr = m1NewElement(this, M1BinaryExpr, op_ADDae, left, right);
	    mt = lt;
	}
	else if (isAType(CArrayType*, urt) &&
		 isSubType(ult, ((CArrayType*)urt)->elementType())) {
	    // FIXME: type should be [size(b)+1] or []
	    expr = m1NewElement(this, M1BinaryExpr, op_ADDea, left, right);
	    mt = rt;
	}
	else {
	    mt = mgNumericType(lt, rt);  // most general numeric type
	    if (isAType(M1Constant*, left) && isAType(M1Constant*, right))
		expr = binaryEval(op_ADD,(M1Constant*)left,(M1Constant*)right,mt);
	    else {
		int op1;
		left  = convertExpr(left, lt, mt);
		right = convertExpr(right, rt, mt);
		if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		    gLint->error(this, "bad operands to binary operator +");
		    op1 = op();
		}
		expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	    }
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);
    }

    case op_SUB: {
	CType* ult;
	CType* urt;	
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	ult = unfoldType(lt);
	urt = unfoldType(rt);

	if (isAType(M1StringConstant*, left) &&
	    isAType(M1StringConstant*, right)) {
	    UData lv = left->eval(NULL);
	    UData rv = right->eval(NULL);
	    UData r;
	    r.str = m1New(CString, lv.str->str());
	    r.str->erase(rv);
	    expr = createConstant(string_type(), r);
	    mt = string_type();
	}
	else if (isAType(M1StringConstant*, left) &&
		 isAType(M1Constant*, right) && 
		 ((rt->typeTag() & M1T_TAG_MASK) == M1T_INTEGER)) {
	    UData lv = left->eval(NULL);
	    UData rv = right->eval(NULL);
	    UData r;
	    rv = m1Convert(rv, rt, char_type());
	    r.str = m1New(CString, lv.str->str());
	    r.str->elementErase(rv);
	    expr = createConstant(string_type(), r);
	    mt = string_type();
	}
	else if ((ult == string_type()) && isSubType(urt, char_type())) {
	    right = convertExpr(right, urt, char_type());
	    expr = m1NewElement(this, M1BinaryExpr, op_SUBsc, left, right);
	    mt = string_type();
	}
	else if ((ult==string_type()) && (urt==string_type())) {
	    expr = m1NewElement(this, M1BinaryExpr, op_SUBs, left, right);
	    mt = string_type();
	}
	else if (isAType(CArrayType*, ult) && isAType(CArrayType*, urt) &&
		 hasCommonBaseType(((CArrayType*)ult)->elementType(),
				   ((CArrayType*)urt)->elementType())) {
	    expr = m1NewElement(this, M1BinaryExpr, op_SUBa, left, right);
	    // FIXME: return type should []
	    mt = lt;
	}
	else if (isAType(CArrayType*, ult) &&
		 isSubType(urt, ((CArrayType*)ult)->elementType())) {
	    // FIXME: return type should []
	    expr = m1NewElement(this, M1BinaryExpr, op_SUBae, left, right);
	    mt = lt;
	}
	else {
	    mt = mgNumericType(lt, rt);  // most general numeric type
	    if (isAType(M1Constant*, left) && isAType(M1Constant*, right))
		expr = binaryEval(op_SUB,(M1Constant*)left,(M1Constant*)right,mt);
	    else {
		int op1;
		left  = convertExpr(left, lt, mt);
		right = convertExpr(right, rt, mt);
		if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		    gLint->error(this, "bad operands to binary operator -");
		    op1 = op();
		}
		expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	    }
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);
    }

    case op_MUL:
    case op_DIV:
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	mt = mgNumericType(lt, rt);  // most general numeric type
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right))
	    expr = binaryEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	else {
	    int op1;
	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt,  mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to binary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_REM:
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	mt = mgIntegerType(lt, rt);  // most general integer type
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right)) 
	    expr = binaryEval(op_REM,(M1Constant*)left,(M1Constant*)right,mt);
	else {
	    int op1;
	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt, mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to binary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_BAND:
    case op_BXOR:
    case op_BOR:
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	mt = unsigned_type();
	if (((lt->typeTag() & M1T_TAG_MASK) != M1T_INTEGER) ||
	    ((rt->typeTag() & M1T_TAG_MASK) != M1T_INTEGER)) {
	    gLint->error(this, "invalid operands to binary %s",
			 operatorName().c_str());
	}
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right))
	    expr = binaryEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	else {
	    int op1;
	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt, mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to binary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_OR:
    case op_AND:
	lt = mLeft->lint(&left,   any_type());
	rt = mRight->lint(&right, any_type());

	DBGFMT_LINT("M1BinaryExpr::lint() + lhs type:%s rhs type:%s",
	       lt->cname(), rt->cname());
	mt = signed_type();
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right)) 
	    expr = binaryEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	else {
	    int op1;
	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt, mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to binary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_BSL:
	lt = mLeft->lint(&left,   any_type());
	rt = mRight->lint(&right, signed_type());
	mt = mgIntegerType(lt, unsigned_type());
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right))
	    expr = shiftEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	else {
	    int op1;
	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt, signed_type());
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to binary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_BSR:
	lt = mLeft->lint(&left,   any_type());
	rt = mRight->lint(&right, signed_type());
	mt = mgIntegerType(lt, unsigned_type());
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right))
	    expr = shiftEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	else {
	    int op1;
	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt, signed_type());
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to binary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	typeCheck(this, mt, aType);
	return lintResult(aExpr, expr, mt);

    case op_LT:   /* < */
    case op_GT:   /* > */
    case op_LTE:  /* <= */
    case op_GTE:  /* >= */
    case op_EQ:   /* == */
    case op_NEQ:  /* != */
    case op_EQL:  /* =:= */
    case op_NQL:  /* =!= */
	lt = mLeft->lint(&left, any_type());
	// FIXME: how can we handle this both ways?
	if (isAType(CEnumerationType*, left->type()))
	    rt = mRight->lint(&right, left->type());
	else
	    rt = mRight->lint(&right, any_type());
	if ((mt = mgNumericType(lt, rt)) == error_type())
	    mt = rt;
	DBGFMT_LINT("M1BinaryExpr::lint() lhs type:%s rhs type:%s",
		    lt->cname(), rt->cname());
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right)) {
	    UData value;

	    if (isAType(M1StringConstant*, left) &&
		isAType(M1StringConstant*, right)) {
		UData a = left->eval(NULL);
		UData b = right->eval(NULL);
		int cmp = a.str->compare(b.str);
		switch(op()) {
		case op_LT: value.i = (cmp < 0); break;
		case op_LTE: value.i = (cmp <= 0); break;
		case op_GT: value.i = (cmp > 0); break;
		case op_GTE: value.i = (cmp >= 0); break;
		case op_EQ: value.i = (cmp == 0); break;
		case op_NEQ: value.i = (cmp != 0); break;
		case op_EQL: value.i = (cmp == 0); break;
		case op_NQL: value.i = (cmp != 0); break;
		}
	    }
	    else if (isAType(M1Nil*, left) && isAType(M1Nil*, right)) {
		if ((op() == op_EQ) || (op() == op_EQL) ||
		    (op() == op_LTE) || (op() == op_GTE))
		    value.i = 1;
		else
		    value.i = 0;
	    }
	    else if (isAType(M1StringConstant*, left) && 
		     isAType(M1Nil*, right)) {
		value.i = 0;
	    }
	    else if (isAType(M1Nil*, left) &&
		     isAType(M1StringConstant*, right))
		value.i = 1;
	    else
		value = rEval(op(),(M1Constant*)left,(M1Constant*)right,rt);
	    expr = createConstant(signed_type(), value);
	}
	else if (typeCheck(this, lt, rt)) {
	    // FIXME: for objected typeCheck will only check that 
	    //        lt is a subtype of rt but we may want to handle that 
	    //        rt is a subtype of lt for compare operations
	    int op1 = 0;

	    left  = convertExpr(left, lt, mt);
	    right = convertExpr(right, rt, mt);

	    switch(unfoldType(mt)->typeTag()) {
	    case M1TYPE_STRING: op1 = op_CMPo; break;
	    case M1TYPE_FLOAT:	op1 = op_CMPf; break;
	    case M1TYPE_SIGNED:	op1 = op_CMPi; break;
	    case M1TYPE_UNSIGNED: op1 = op_CMPu; break;
	    case M1TYPE_ARRAY:
	    case M1TYPE_OBJECT:
	    case M1TYPE_ANY:
		if ((op() == op_NQL) || (op() == op_EQL))
		    op1 = op_CMPo;
		else 
		    op1 = op_CMPu;
		break;
	    default:
		gLint->error(this, "internal error, case %s not handled",
			     mt->cname());
		op1 = op_CMPu;
		break;
	    }

	    // Generate the compare operator
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	    expr->setType(signed_type());
	    expr->setIsLinted(true);
	    switch(op()) {
	    case op_LT:  op1 = op_LTz; break;
	    case op_GT:  op1 = op_GTz; break;
	    case op_LTE: op1 = op_LTEz; break;
	    case op_GTE: op1 = op_GTEz; break;
	    case op_EQ:  op1 = op_EQz; break;
	    case op_NEQ: op1 = op_NEQz; break;
	    case op_EQL: op1 = op_EQz; break;
	    case op_NQL: op1 = op_NEQz; break;
	    }
	    expr = m1NewElement(this, M1UnaryExpr, op1, expr);
	}
	else if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	typeCheck(this, signed_type(), aType);
	return lintResult(aExpr, expr, signed_type());

    case op_SEQ:
	// we should be able to remove some left expressions (like constants)
	// and warn them as useless like  a = 1,2,x;
	lt = mLeft->lint(&left, any_type());
	rt = mRight->lint(&right, any_type());
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	typeCheck(this, rt, aType);
	return lintResult(aExpr, expr, rt);

    default:
	gLint->error(this,
		     "M1BinaryExpr::lint %s not handled",
		     operatorName().c_str());
	return lintResult(aExpr, expr, any_type());
    }
}

CType* M1BinaryExpr::lint_trg(M1Expr** aExpr)
{
    M1Expr* left = NULL;
    M1Expr* right = NULL;
    M1Expr* expr  = this;
    CType* lt;
    CType* rt;
    CType* mt;

    DBGFMT_LINT("M1BinaryExpr:lint_trg: ");
    if (M1DBG_IS_SET(M1DBG_LINT|M1DBG_PRNT)) {
	this->print(&cout, 0);
	printf("\n");
    }

    switch(op()) {
    case op_ELEM:
	lt = mLeft->lint(&left, any_type());      // The array var/expr
	rt = mRight->lint(&right, signed_type()); // The index
	if ((mLeft != left) || (mRight != right))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	if (!isAType(CArrayType*, lt)) {
	    gLint->error(this, "'%s' is not an array type", lt->cname());
	    lt = event_bool_type();
	}
	else {
	    // FIXME check that it's a array of input events!
	    lt = ((CArrayType*) lt)->elementType();
	    if (!isInputEvent(lt)) {
		gLint->error(this, "'%s' is not an input event type", lt->cname());
		lt = event_bool_type();
	    }
	}
	return lintResult(aExpr, expr, lt);

    case op_OR:
    case op_AND:
	lt = mLeft->lint_trg(&left);
	rt = mRight->lint_trg(&right);
	mt = signed_type();
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right)) {
	    UData value = rEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	    expr = createConstant(mt, value);
	}
	else {
	    int op1 = convertOperator(op(), mt->typeTag());
	    expr = m1NewElement(this, M1BinaryExpr, op1, left, right);
	}
	return lintResult(aExpr, expr, mt);
	
    case op_EQ:
    case op_NEQ:
	lt = mLeft->lint_trg(&left);
	rt = mRight->lint_trg(&right);
	mt = signed_type();
	if (isAType(M1Constant*, left) && isAType(M1Constant*, right)) {
	    UData value = rEval(op(),(M1Constant*)left,(M1Constant*)right,mt);
	    expr = createConstant(mt, value);
	}
	else if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	return lintResult(aExpr, expr, mt);

    default:
	gLint->error(this, "boolean trigger expression expected");
	return lintResult(aExpr, expr, any_type());
    }
}

CType* M1BinaryExpr::lint_ref(M1Expr** aExpr, CType* aType)
{
    M1Expr* left = NULL;
    M1Expr* right = NULL;
    M1Expr* expr  = this;
    CType* lt;
    CType* rt;
    CType* mt;

    DBGFMT_LINT("M1BinaryExpr:lint_ref: ");
    if (M1DBG_IS_SET(M1DBG_LINT|M1DBG_PRNT)) {
	this->print(&cout, 0);
	printf("\n");
    }

    setIsLinted(true);

    switch(op()) {
    case op_FLD: {
	string fname;
	CField* fld;

	lt = mLeft->lint_ref(&left, any_type());  // The object
	fname = ((M1Identifier*) mRight)->name();
	lt = unfoldType(lt);
	if (!isAType(CBaseType*, lt)) {
	    if (lt != error_type())
		gLint->error(this, "'%s' does not have any members",
			     lt->cname());
	    return lintResult(aExpr, expr, aType);
	}
	else if ((fld = ((CBaseType*)lt)->field(fname)) == NULL) {
	    if (lt != error_type())
		gLint->error(this, "'%s' has no member named '%s'",
			     lt->cname(), fname.c_str());
	    return lintResult(aExpr, expr, aType);
	}
	expr = m1NewElement(this, M1Field, left, fname, fld->index());
	setStorage(fld->storage());
	expr->setStorage(fld->storage());
	return lintResult(aExpr, expr, fld->type());
    }

    case op_ELEM:
	lt = mLeft->lint_ref(&left, any_type());  // The object
	rt = mRight->lint(&right, signed_type()); // The index
	if ((left != mLeft) || (right != mRight))
	    expr = m1NewElement(this, M1BinaryExpr, op(), left, right);
	lt = unfoldType(lt);
	if (isAType(CArrayType*, lt)) {
	    setStorage(mLeft->storage());
	    mt = ((CArrayType*) lt)->elementType();
	}
	else if (lt == string_type()) {
	    setStorage(mLeft->storage());
	    mt = char_type();
	}
	else {
	    gLint->error(this, "bad array type %s", lt->cname());
	    mt = any_type();
	}
	typeCheck(this, aType, mt);
	return lintResult(aExpr, expr, mt);
    

    default:
	gLint->error(this, "M1BinaryExpr::eval_ref %s not handled",
		     operatorName().c_str());
	return lintResult(aExpr, expr, any_type());
    }
}

void M1UnaryExpr::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mExpr->iterate(&mExpr, iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);
}


CType* M1UnaryExpr::lint(M1Expr** aExpr, CType* aType)
{
    CType* mt;
    CType* t;
    M1Expr* mid = NULL;
    M1Expr* expr = this;

    DBGFMT_LINT("M1UnaryExpr::lint() called type=%s",aType->cname());

    switch(op()) {
    case op_POS:
	mt = mExpr->lint(&mid, aType);
	if (isAType(M1Constant*, mid)) {
	    UData value = (mid)->eval(NULL); 
	    expr = createConstant(mt,value);
	}
	else
	    expr = mid;
	return lintResult(aExpr, expr, mt);

    case op_BNOT:
	t = mExpr->lint(&mid, aType);
	mt = unsigned_type();
	if ((t->typeTag() & M1T_TAG_MASK) != M1T_INTEGER)
	    gLint->error(this, "invalid operands to unary %s",
			 operatorName().c_str());
	if (isAType(M1Constant*, mid)) 
	    expr = unaryEval(op_BNOT, (M1Constant*)mid, mt);
	else {
	    int op1;
	    mid = convertExpr(mid, t, mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to unary operator %s",
			     operatorName().c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1UnaryExpr, op1, mid);
	}
	return lintResult(aExpr, expr, mt);

    case op_NEG:
	t = mExpr->lint(&mid, aType);
	mt = mgNumericType(t, any_type());
	if (isAType(M1Constant*, mid)) 
	    expr = unaryEval(op_NEG, (M1Constant*)mid, mt);
	else {
	    int op1;
	    mid = convertExpr(mid, t, mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to unary operator %s",
			     operatorName().c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1UnaryExpr, op1, mid);
	}
	return lintResult(aExpr, expr, mt);

    case op_NOT:
	t = mExpr->lint(&mid, aType);
	mt = signed_type();
	if (isAType(M1Constant*, mid))
	    expr = unaryEval(op_NOT,(M1Constant*)mid, mt);
	else {
	    int op1;
	    mid = convertExpr(mid, t, mt);
	    if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
		gLint->error(this, "bad operands to unary operator %s",
			     formatOperator(op()).c_str());
		op1 = op();
	    }
	    expr = m1NewElement(this, M1UnaryExpr, op1, mid);
	}
	return lintResult(aExpr, expr, mt);

    case op_TRIGGER:
	gLint->error(this, "operator * not allowed in rhs expression");
	mt = mExpr->lint(&mid, aType);
	return lintResult(aExpr, mid, mt);
    case op_SUPRESS:
	gLint->error(this, "operator ** not allowed in rhs expression");
	mt = mExpr->lint(&mid, aType);
	return lintResult(aExpr, mid, mt);

    case op_LTz:
    case op_LTEz:
    case op_GTz:
    case op_GTEz:
    case op_EQz:
    case op_NEQz:
	mt = mExpr->lint(&mid, signed_type());
	if (unfoldType(mt) != signed_type())
	    gLint->warning(expr, "type changed for internal operator %s",
			   formatOperator(op()).c_str());
	if (mExpr != mid)
	    expr = m1NewElement(this, M1UnaryExpr, op(), mid);
	return lintResult(aExpr, expr, mt);
	

    case op_PSTINC:
    case op_PREINC:
    case op_PSTDEC:
    case op_PREDEC: {
	int op1;

	mt = mExpr->lint_ref(&mid, any_type());
	if ((op1 = convertOperator(op(), mt->typeTag())) == 0) {
	    gLint->error(this, "bad operand to unary operator %s",
			 formatOperator(op()).c_str());
	    op1 = op();
	}

	// FIXME! This side effect must be handled in better way!
	if (!isLinted()) {
	    setIsLinted(true);
	    expr = m1NewElement(this, M1UnaryExpr, op1, mid);
	    expr->setType(mt);
	    expr->setIsLinted(true);

	    switch(op()) {
	    case op_PSTINC:
	    case op_PSTDEC:
		gLint->addPostExpression(expr);
		break;
	    case op_PREINC:
	    case op_PREDEC:
		gLint->addPreExpression(expr);
		break;
	    }
	}
	return lintResult(aExpr, mid, mt);
    }

    default:
	gLint->error(this,
		     "M1UnaryExpr::lint %s not handled",
		     operatorName().c_str());
	return lintResult(aExpr, expr, any_type());
    }
}

CType* M1UnaryExpr::lint_ref(M1Expr** aExpr, CType* aType)
{
    M1Expr* mid = NULL;
    M1Expr* expr = this;
    CType* mt;

    switch(op()) {
    case op_TRIGGER:
	mt = mExpr->lint_ref(&mid, aType);
	if (mt->typeTag() != M1TYPE_EVENT) 
	    gLint->error(this, "trigger expression must be of event type");
	if (mExpr != mid)
	    expr = m1NewElement(this, M1UnaryExpr, op(), mid);
	return lintResult(aExpr, expr, mt);
    case op_SUPRESS:
	mt = mExpr->lint_ref(&mid, aType);
	if (mt->typeTag() != M1TYPE_EVENT) 
	    gLint->error(this, "supress expression must be of event type");
	if (mExpr != mid)
	    expr = m1NewElement(this, M1UnaryExpr, op(), mid);
	return lintResult(aExpr, expr, mt);
    default:
	gLint->error(this, "reference expression [%s] not allowed (not handled)", 
		     formatExpression(this).c_str());
	return lintResult(aExpr, expr, aType);
    }
}

CType* M1UnaryExpr::lint_trg(M1Expr** aExpr)
{
    M1Expr* expr = this;
    M1Expr* mid = NULL;
    CType* mt;

    DBGFMT_LINT("M1UnaryExpr::lint_trg() called");
    switch(op()) {
    case op_NOT:
	mt = mExpr->lint(&mid, signed_type());
	if (isAType(M1Constant*, mid))
	    expr = unaryEval(op_NOT, (M1Constant*)mid, mt);
	else
	    expr = m1NewElement(this, M1UnaryExpr, op_NOTi, mid);
	return lintResult(aExpr, expr, mt);

    default:
	gLint->error(this, "boolean trigger expression expected");
	return lintResult(aExpr, expr, bool_type());
    }
}

void M1CvtExpr::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mExpr->iterate(&mExpr, iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);
}
//
// Normally the lint phase will not be run over cvt, becuase
// The linter will insert cvt's when needed. But for generated
// constructs there may sometimes be needed to lint again so
// we just make sure that the conversion is retained.
//
CType* M1CvtExpr::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    M1Expr* cvtexpr  = NULL;
    CType* t = mExpr->lint(&cvtexpr, any_type());
    M1CvtCode cvt = typeWriteAccessCode(t, aType);

    if (typeCode(t) != M1CVT_from(mCvt))
	gLint->warning(this, "internal error type changed %d -> %d",
		       M1CVT_from(cvt), M1CVT_from(mCvt));
    if (expr != cvtexpr)
	expr = m1NewElement(this, M1CvtExpr, mCvt, cvtexpr);
    return lintResult(aExpr, expr, type());
}

void M1CastExpr::iterate(M1Expr** aExpr, M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {    
	mExpr->iterate(&mExpr, iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);    
}

// [:]Type[:Type] ( expr )
CType* M1CastExpr::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    M1Expr* cast = NULL;
    CType* castType = mTypeId->lint(Q_NONE);
    CType* exprType = mExpr->lint(&cast, any_type());

    if (isObjectType(castType) && isObjectType(exprType)) {
	if (!hasCommonBaseType(castType, exprType)) {
	    gLint->error(this, "type %s and %s do not have common type",
			 castType->cname(), exprType->cname());
	    return castType;
	}
	/* I grew tired of the eternal bitching. /Love F.
#ifdef DEBUG
	else if (!isSubType(exprType, castType)) 
	    gLint->warning(this, "type %s is not derived from %s",
			   exprType->cname(), castType->cname());
#endif
	*/

    }
    else if (!typeCheck(this, castType, exprType))
	return castType;

    if (isAType(M1Constant*, cast))
	expr = convertExpr(cast, exprType, castType);
    else {
	M1TypeTag ct = castType->typeTag();
	if (((ct & M1T_TAG_MASK) == M1T_INTEGER) ||
	    ((ct & M1T_TAG_MASK) == M1T_FLOAT)) 
	    expr = convertExpr(cast, exprType, castType);
	else if (cast != mExpr)
	    expr = m1NewElement(this, M1CastExpr, mTypeId, cast);
    }
    return lintResult(aExpr, expr, castType);
}

CType* M1CastExpr::lint_trg(M1Expr** aExpr)
{
    M1Expr* expr = this;
    M1Expr* cast = NULL;
    CType* castType = mTypeId->lint(Q_NONE);
    CType* exprType = mExpr->lint(&cast, any_type());

    if (!typeCheck(this, castType, exprType))
	return castType;
    if (isAType(M1Constant*, cast))
	expr = createConstant(castType, cast->value());
    else {
	M1TypeTag ct = castType->typeTag();
	if (((ct & M1T_TAG_MASK) == M1T_INTEGER) ||
	    ((ct & M1T_TAG_MASK) == M1T_FLOAT)) 
	    expr = convertExpr(cast, exprType, castType);
	else if (cast != mExpr)
	    expr = m1NewElement(this, M1CastExpr, mTypeId, cast);
    }
    return lintResult(aExpr, expr, castType);
}

CType* M1CastExpr::lint_ref(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    M1Expr* cast = NULL;
    CType* castType = mTypeId->lint(Q_NONE);
    CType* exprType = mExpr->lint(&cast, aType);

    if (!typeCheck(this, castType, exprType))
	return lintResult(aExpr, expr, castType);
    if (isAType(M1Constant*, cast))
	expr = createConstant(castType, cast->value());
    else if (cast != mExpr)
	expr = m1NewElement(this, M1CastExpr, mTypeId, cast);
    return lintResult(aExpr, expr, castType);
}

void M1CallExpr::iterate(M1Expr** aExpr,  M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mArgs->iterate(iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);    
}
//
// Implement built in functions
//  trigonometrics: cos/sin
//  numbers:        abs/round
//  etc:
//


CType* M1CallExpr::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    CType* type;

    DBGFMT_LINT("M1CallExpr::lint() called type=%s", aType->cname());

    if (isAType(M1Identifier*, mFunc)) {
	M1Identifier* func = (M1Identifier*) mFunc;
	list<M1Expr*>::iterator iter = mArgs->begin();
	CType* types[MAX_BUILTIN_ARGS];
	int ai = 0;

	// Extract all argument types
	while((iter != mArgs->end()) && (ai < MAX_BUILTIN_ARGS)) {
	    M1Expr* texpr = NULL;  // a copy will be generated here
	    (*iter)->lint(&texpr, any_type());
	    types[ai++] = texpr->type();
	    iter++;
	}
	if (ai == MAX_BUILTIN_ARGS) {
	    gLint->error(this, 
			 "too many arguments, max %d arguments allowed",
			 MAX_BUILTIN_ARGS);
	    return lintResult(aExpr, expr, any_type());
	}
	else {
	    UBuiltin* bf;
	    UBuiltin* alt_bf;
	    int bi = 0;
	    bool is_constant = true;
	    list<M1Expr*>::iterator iter = mArgs->begin();
	    UData args[MAX_BUILTIN_ARGS];

	    if ((bf = matchBuiltin(func->cname(),types,ai,&alt_bf))==NULL) {
		if (alt_bf != NULL) {
		    int nargs = nargsBuiltin(alt_bf);
		    if (nargs < ai)
			gLint->error(this, 
				     "too few arguments to %s",
				     formatBuiltin(alt_bf).c_str());
		    else if (nargs > ai) {
			gLint->error(this, 
				     "too many arguments to %s",
				     formatBuiltin(alt_bf).c_str());

		    }
		    else {
			gLint->error(this, 
				     "function argument mismatch %s",
				     formatBuiltin(alt_bf).c_str());
		    }
		}
		else {
		    gLint->error(this, "function %s/%d not found", func->cname(),ai);
		}
		return lintResult(aExpr, expr, any_type());
	    }

	    ai = 0;
	    while((iter != mArgs->end()) && 
		  (bf->atype[bi] != BUILTIN_ARG_END)) {
		CType* t;
		if (bf->atype[bi] == BUILTIN_ARG_VARG)
		    t = any_type();
		else if (bf->atype[bi] == M1TYPE_EVENT) { // FIXME
		    t = any_type();
		    bi++;
		}
		else {
		    t = tag_type(bf->atype[bi]);
		    bi++;
		}
		(*iter)->lint(&*iter, t);
		if (isAType(M1Constant*, *iter))
		    args[ai] = (*iter)->value();
		else
		    is_constant = false;
		iter++;
		ai++;
	    }
	    if (iter != mArgs->end()) {
		gLint->error(this, 
			     "too many arguments, %s() expects %d argument%s",
			     func->cname(), ai, (ai==1) ? "" : "s");
	    }
	    else if ((bf->atype[bi] != BUILTIN_ARG_END) &&
		     (bf->atype[bi] != BUILTIN_ARG_VARG)) {
		while(bf->atype[bi] != BUILTIN_ARG_END)
		    bi++;
		gLint->error(this, 
			     "too few arguments, %s() expects %d argument%s",
			     func->cname(), bi, (bi==1) ? "" : "s");
	    }
	    else {
		if ((bf->rtype == M1TYPE_OBJECT) &&
		    ((bi == 1) && (bf->atype[0] == M1TYPE_OBJECT)))
		    /* SUPER SPECIAL CASE: assume input type is same
		       as output type (maybe hard code clone/copy?) */
		    type = unfoldType(types[0]);
		else
		    type = tag_type(bf->rtype);

		if (is_constant && bf->pure) {
		    UData v = (*bf->func)(NULL, args);
		    expr = createConstant(type, v);
		}
		else {
		    expr = m1NewElement(this, M1BuiltinExpr, bf, mArgs);
		}
		typeCheck(this, type, aType);
		return lintResult(aExpr, expr, type);
	    }
	}
	return lintResult(aExpr, expr, any_type());
    }
    else {
	gLint->error(this, "function identifier expected");
	return lintResult(aExpr, expr, any_type());
    }
}

void M1BuiltinExpr::iterate(M1Expr** aExpr,  M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mArgs->iterate(iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);    
}

CType* M1BuiltinExpr::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    list<M1Expr*>::iterator iter = mArgs->begin();
    CType* type;
    CType* types[MAX_BUILTIN_ARGS];
    int ai = 0;
    DBGFMT_LINT("M1BuiltinEpxr::lint() called type=%s", aType->cname());

    while (iter != mArgs->end()) {
	CType* t = (*iter)->lint(&*iter, any_type());
	types[ai++] = unfoldType(t);
	iter++;
    }
    if ((mBf->rtype == M1TYPE_OBJECT) &&
	((ai == 1) && (mBf->atype[0] == M1TYPE_OBJECT)))
	/* SUPER SPECIAL CASE: assume input type is same
	   as output type (maybe hard code clone/copy?) */
	type = types[0];
    else
	type = tag_type(mBf->rtype);
    typeCheck(this, type, aType);
    return lintResult(aExpr, expr, type);
}


CType* M1TypePrimitive::lint(int aStorage)
{
    DBGFMT_LINT("M1TypePrimitive::lint() called type=%s", mType->cname());
    return mType;
}

CType* M1TypeIdentifier::lint(int aStorage)
{
    CType* t;

    DBGFMT_LINT("M1TypeIdentifier::lint() called id=%s", cname());

    if ((t = gLint->lookupType(name())) == NULL) {
	gLint->error(this,"type %s not defined", cname());
	return error_type();
    }
    return t;
}

CType* M1EventTypeIdentifier::lint(int aStorage)
{
    CType* t;

    DBGFMT_LINT("M1EventTypeIdentifier::lint() called id=%s", 
		cname());

    if ((t = gLint->lookupType(mTypeId)) == NULL) {
	gLint->error(this,"type %s not defined", mTypeId.c_str());
	return error_type();
    }
    return CEventType::createType(t,mQueued,mDirection);
}

// Check uniqness of all enumerated values
CType* M1EnumSpecifier::lint(int aStorage)
{
    list<M1Enum*>::iterator iter;
    list<string>::iterator siter;
    list<int> allocList;        // list of used explict values
    list<string> autoList;      // list of auto enumrated
    list<string> idList;        // list of all id's
    CEnumerationType* t;
    int eValue;
    M1TypeIdentifier* tid = dynamic_cast<M1TypeIdentifier*>(mType);

    DBGFMT_LINT("M1EnumSpecifier::lint()");

    t = m1New(CEnumerationType);

    if (tid) {
	string typeName = tid->name();
	t->setName(typeName);
	gLint->addType(aStorage, typeName, t);
    }

    for (iter = mEnumList->begin(); iter != mEnumList->end(); iter++) {
	string eId = (*iter)->id();
	list<string>::iterator siter = idList.begin();
	M1Expr** vp = (*iter)->valuep();
	int found = 0;

	while(!found && (siter != idList.end())) {
	    if (eId == *siter)
		found = 1;
	    else
		siter++;
	}

	if (found)
	    gLint->error(*iter, "enumeration %s already defined", eId.c_str());
	else {
	    idList.push_back(eId);

	    if (*vp == NULL)
		autoList.push_back(eId);
	    else {
		(*vp)->lint(vp, signed_type());
		if (!isAType(M1Constant*, *vp)) {
		    gLint->error(*iter, "enumerated value is not constant");
		    t->addEnumeration(eId, 0);
		}
		else {
		    UData value = (*vp)->value();
		    allocList.push_back(value.i);
		    DBGFMT_LINT("M1EnumSpecifier: %s = %d",
				eId.c_str(), value.i);
		    t->addEnumeration(eId, value.i);
		}
	    }
	}
    }
    // enumerate the auto enumerated values
    eValue = 0;
    for (siter = autoList.begin(); siter != autoList.end(); siter++) {
	string eId = *siter;
	bool nextValue = true;

	// FIXME: sort the allocList 
	while(nextValue) {
	    list<int>::iterator iiter = allocList.begin();
	    bool eFound = false;

	    while(!eFound && (iiter != allocList.end())) {
		if (eValue == *iiter)
		    eFound = true;
		else
		    iiter++;
	    }
	    if (eFound)
		eValue++;
	    else
		nextValue = false;
	}
	DBGFMT_LINT("M1EnumSpecifier: %s = %d",
	       eId.c_str(), eValue);
	t->addEnumeration(eId, eValue);
	eValue++;
    }
    return t;
}

//
//  Lint a array type delcarator base[A][B][C] is parsed as
//    ( ( base [A] ) [B] ) [C]
//  The type should however be linted as
//    (((base[C]) [B]) [A]) to get the correct interpretation of
//
CType* M1ArrayTypeDeclarator::lint(int aStorage)
{
    CType* baseType;

    baseType = mBaseType->lint(aStorage);

    if (mSize == NULL)
	return CArrayType::create(baseType, 0);
    else {
	mSize->lint(&mSize, unsigned_type());
	if (isAType(M1UnsignedConstant*, mSize)) {
	    UData v = ((M1UnsignedConstant*) mSize)->value();
	    return CArrayType::create(baseType, v.u);
	}
	else {
	    gLint->warning(this, "array index not constant");
	    return CArrayType::create(baseType, 0);
	}
    }
}

void M1ObjectConstant::iterate(M1Expr** aExpr,  M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mInitList->iterate(iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);    
}


//
// Lint the field id part id | id[] ...
//
CType* lintFid(M1Expr* expr, CBaseType* aBase, string& fid)
{
    CField* fld = NULL;

    if (isAType(M1BinaryExpr*, expr)) {  // MUST BE fid[]
	CType* t;
	M1BinaryExpr* bexpr = (M1BinaryExpr*) expr;
	M1Expr** rp = bexpr->rightp();
	(*rp)->lint(rp, signed_type());  // lint index expression
        // recurse to find id
	if ((t = lintFid(bexpr->left(), aBase, fid)) == NULL) 
	    return NULL;
	if (isAType(CArrayType*, t))
	    return ((CArrayType*)t)->elementType();
	return t;
    }
    else if (isAType(M1Identifier*, expr)) {
	M1Identifier* id  = (M1Identifier*) expr;
	fid = id->name();
	if ((fld = aBase->field(fid)) != NULL) {
	    id->setIndex(fld->index());
	    return fld->type();
	}
    }
    else if (isAType(M1BasedIdentifier*, expr)) {
	M1BasedIdentifier* id  = (M1BasedIdentifier*) expr;
	M1TypeIdentifier* typeId = id->typeId();
	if (typeId == NULL) {
	    fid = id->name();
	    fld = aBase->field(fid);
	}
	else {
	    CType* bt = typeId->lint(Q_NONE);
	    fid = typeId->name() + "." + id->name();
	    if (bt != NULL) {
		if (isDerived(aBase, bt))
		    fld = ((CBaseType*)bt)->field(id->name());
		else
		    gLint->error(expr, "'%s' is not a member in '%s'",
				 id->cname(), bt->cname());
	    }
	}
	if (fld) {
	    id->setIndex(fld->index());
	    return fld->type();
	}
    }
    return NULL;
}


CType* M1ObjectConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    list<M1Expr*>::iterator iter = mInitList->begin();
    UData object = nil;
    CBaseType* t;
    CType* ct;

    DBGFMT_LINT("M1ObjectConstant::lint() type=%s", aType->cname());

    if ((ct = mTypeId->lint(Q_NONE)) == error_type())
	return error_type();
    t = (CBaseType*) ct;
    DBGFMT_LINT("M1ObjectConstant::lint() OBJECT");
    while(iter != mInitList->end()) {
	string fid;
	M1BinaryExpr* fexpr = (M1BinaryExpr*) *iter;
	M1Expr** rp         = fexpr->rightp();
	CType* lt           = NULL;
	CType* rt;

	if ((lt = lintFid(fexpr->left(), t, fid)) == NULL) {
	    gLint->error(this, "'%s' is not a member in '%s'",
			 fid.c_str(), t->cname());
	}
	else {
	    switch(fexpr->op()) {
	    case op_CNCT:
		(*rp)->lint(rp, lt);
		rt = (*rp)->type();  // Using node type, return type is stripped
		DBGFMT_LINT("M1ObjectConstant::lint() CONNECT member=%s type=%s",
			    fid.c_str(), lt->cname());
		if (!isInputEvent(lt))
		    gLint->error(fexpr, "bad connect destination");
		else if (!isAType(M1Nil*, *rp)) {
		    if (!isOutputEvent(rt))
			rt = connectExpression(*rp, rp, lt);
		    else if (((CEventType*) rt)->baseType() !=
			     ((CEventType*) lt)->baseType()) {
#ifdef DEBUG
			gLint->warning(this, "type conversion in connect expression");
#endif
			rt = connectExpression(*rp, rp, lt);
		    }
		}
		break;
	    case op_PUT: {
		M1CvtCode cvt;
		M1Expr* gexpr;
		M1Expr* rn;
		int op1;

		DBGFMT_LINT("M1ObjectConstant::lint() ASSIGN member=%s type=%s",
			    fid.c_str(), lt->cname());
		rt = (*rp)->lint(rp, lt);
		typeCheck(fexpr, rt, lt);
		if ((cvt = typeWriteAccessCode(rt, lt)) != M1CVT_nn)
		    rn = m1NewElement(*rp, M1CvtExpr, cvt, *rp);
		else
		    rn = *rp;
		op1 = convertOperator(op_PUT, unfoldType(lt)->typeTag());
		gexpr = m1NewElement(fexpr, M1BinaryExpr, op1, fexpr->left(), rn);
		m1ReleaseExpr(fexpr);
		iter = mInitList->erase(iter);
		iter = mInitList->insert(iter, gexpr);
		break;
	    }
	    case op_PUTb:
	    case op_PUTc:
	    case op_PUTt:
	    case op_PUTi:
	    case op_PUTu:
	    case op_PUTf:
	    case op_PUTs:
	    case op_PUTo:
		// Assume already linted!!!
		break;
	    default:
		gLint->error(fexpr, 
			     "operator %s not allowed",
			     fexpr->operatorName().c_str());
	    }
	}
	iter++;
    }
    return lintResult(aExpr, expr, t);
}

void M1ArrayConstant::iterate(M1Expr** aExpr,  M1ExprIterator* iter)
{
    M1Expr* expr;
    if ((expr = iter->before(this)) == this) {
	mInitList->iterate(iter);
	expr = iter->after(this);
    }
    m1CondSetRetainExpr(aExpr, expr);    
}


CType* M1ArrayConstant::lint(M1Expr** aExpr, CType* aType)
{
    M1Expr* expr = this;
    list<M1Expr*>::iterator iter = mInitList->begin();
    DBGFMT_LINT("M1ArrayConstant::lint() restric=%s", aType->cname());

    if (!isAType(CArrayType*, aType))
	gLint->error(this, "type mismatch, constant array not expected");
    else {
	CArrayType* arrType = (CArrayType*) aType;
	CType* baseType = arrType->elementType();
	size_t arrSize;
	if (((arrSize = arrType->arraySize()) > 0) &&
	    (mInitList->size() > arrSize))
	    gLint->error(this, "constant array too big");
	while(iter != mInitList->end()) {
	    (*iter)->lint(&*iter, baseType);
	    iter++;
	}
    }
    return lintResult(aExpr, expr, aType);
}


// Check if statement
void M1IfStatement::lint(CLintInfo* aInfo)
{
    DBGFMT_LINT("M1IfStatement::lint()");
    lintPrePost(mCondition, &mCondition, signed_type());
    mThen->lint(aInfo);
    if (mElse != NULL)
	mElse->lint(aInfo);	
}

static bool sort_select(CSelect a, CSelect b)
{
    return a.mValue < b.mValue;
}


void M1SwitchStatement::lint(CLintInfo* aInfo)
{
    CCompoundType* t;
    CType* vt;

    DBGFMT_LINT("M1SwitchStatement::lint()");

    vt = lintPrePost(mValue, &mValue, signed_type());
    vt = mValue->type(); // need to pickup the real mValue type!

    t = m1New(CCompoundType);
    // Setting type name here will not register it! we do not want to
    t->setName("_Switch"); 
    gLint->pushScope(t);
    mLocalStackPos = aInfo->downLevel();

    if (mDeclarations != NULL) {
	M1StatementList* construct;
	list<M1Statement*>::iterator iter;

	construct = new M1StatementList;
	mDeclarations->lint(construct, aInfo);
	iter = construct->begin();
	while(iter != construct->end()) {
	    M1Statement* stmt = *iter;
	    if (mInitStart == -1)
		mInitStart = mInitStop = 1;
	    else
		mInitStop++;
	    stmt->lint(aInfo);
	    m1RetainStatement(stmt);
	    mSelectCode.push_back(stmt);
	    iter++;
	}
    }
    mLocalSize = aInfo->nextFramePos() - mLocalStackPos;
    //
    // Be more conservative than gcc and require that
    // ALL labeles for switch is within the next compund scope
    // We need to scan the compound statement list fo
    // labeled statements 
    //    CASE expr: statement
    //                 statement may contain CASE and DEFAULT
    //    DEFAULT: statement
    //                 statement may contain CASE and DEFAULT
    //
    if (mStatements != NULL) {
	list<M1Statement*>::iterator iter;

	for(iter=mStatements->begin();iter!=mStatements->end();iter++) {
	    M1Statement* stmt = *iter;

	    while(stmt != NULL) {
		if (isAType(M1CaseStatement*, stmt)) {
		    M1Expr** ep = ((M1CaseStatement*)stmt)->exprp();
		    UData v     = USigned(0);

		    (*ep)->lint(ep, vt);
		    if (isAType(M1SignedConstant*, *ep)) {
			int i;
			v = ((M1SignedConstant*) (*ep))->value();
			for (i = 0; i < (int) mSelect.size(); i++) {
			    if (mSelect.at(i).mValue == v.i) {
				gLint->error(stmt, "multiple case value");
				break;
			    }
			}
			mSelect.push_back(CSelect(v.i, mSelectCode.size()));
		    }
		    else {
			gLint->error(stmt, "case must have a constant value");
			mSelect.push_back(CSelect(v.i, mSelectCode.size()));
		    }
		    // Continue with the labled statment
		    stmt = ((M1CaseStatement*)stmt)->statement();
		}
		else if (isAType(M1DefaultStatement*, stmt)) {
		    if (mDefaultPos >= 0)
			gLint->error(stmt,
				     "multiple default labels in one swicth");
		    mDefaultPos = mSelectCode.size();
		    // Continue with the labled statment
		    stmt = ((M1DefaultStatement*)stmt)->statement();
		}
		else {
		    stmt->lint(aInfo);
		    m1RetainStatement(stmt);
		    mSelectCode.push_back(stmt);
		    stmt = NULL;
		}
	    }
	}
    }
    // sort the mSelect vector on mValue field (MUST!)
    sort(mSelect.begin(), mSelect.end(), sort_select);
    aInfo->upLevel(mLocalStackPos);
    gLint->popScope();
}

void M1CaseStatement::lint(CLintInfo* aInfo)
{
    gLint->error(this, "case not with in switch statement");
}

void M1DefaultStatement::lint(CLintInfo* aInfo)
{
    gLint->error(this, "default statement not with in switch statement");
}

void M1JumpStatement::lint(CLintInfo* aInfo)
{
    DBGFMT_LINT("M1JumpStatement::lint()");
    // continue; break; return;
}

void M1CompoundStatement::lint(CLintInfo* aInfo)
{
    CCompoundType* t;
    list<M1Script*>::iterator scr_iter;

    DBGFMT_LINT("M1CompoundStatement::lint()");

    t = m1New(CCompoundType);
    // Setting type name here will not register it! we do not want to
    t->setName("_Compound");
    gLint->pushScope(t);
    mLocalStackPos = aInfo->downLevel();

    if (mDeclarations && mDeclarations->size())
	mDeclarations->lint(mStatements, aInfo);
    mLocalSize = aInfo->nextFramePos() - mLocalStackPos;

    if (mStatements) 
	mStatements->lint(aInfo);

    aInfo->upLevel(mLocalStackPos);
    gLint->popScope();
}

//
// foreach x in [1:2] { x; }
// foreach x in [1:10:3] {  // 1 4 7 10 
// foreach x in array { x; }
//
// MAYBE?
// foreach x in [N] { x; }  => 0 ... N-1
//    expand [N] => [0:N-1:1]  (x integer only?)
//
void M1ForEachStatement::lint(CLintInfo* aInfo)
{
    CType* loopType;
    CType* rangeType;
    DBGFMT_LINT("M1ForEachStatement::lint()");
    loopType = mVar->lint(&mVar, any_type());
    if (mRange->op() == op_RANGE) {
	rangeType = mRange->lint(&mRange, loopType);
	if (!isAType(CWordType*, loopType))
	    gLint->error(this, "bad loop variable type");
	if ((((M1TrinaryExpr*)mRange)->second() == NULL) &&
	    (((M1TrinaryExpr*)mRange)->third() == NULL))
	    gLint->error(this, "range expression must be bound");
    }
    else {
	mRange->lint(&mRange, CArrayType::create(loopType, 0));
    }
    mStatement->lint(aInfo);
}

void M1ExprStatement::lint(CLintInfo* aInfo)
{
    DBGFMT_LINT("M1ExprStatement::lint()");
    if (mExpr == NULL)  // Empty statement
	;
    else {
	lintPrePost(mExpr, &mExpr, any_type());
    }
}

void M1StatementList::lint(CLintInfo* aInfo)
{
    list<M1Statement*>::iterator iter = mList.begin();
    while(iter != mList.end()) {
	(*iter)->lint(aInfo);
	iter++;
    }
}

//
//  Lint a array type delcarator baseType id[A][B][C] is stored reversed as
//  baseType [C] [B] [A]
//
CField* M1ArrayDeclarator::lint(CType* baseType, CLintInfo* aInfo, int aStorage)
{
    CField* decl;
    CType*  t;
    list<M1Expr*>::iterator iter;
    DBGFMT_LINT("M1ArrayDeclarator::lint() basetype=%s", baseType->cname());

    decl = mDecl->lint(baseType, aInfo, aStorage);
    t = decl->type();  // Get the base type
    iter = mSizeList->begin();
    while(iter != mSizeList->end()) {
	if (isAType(M1Nil*, *iter)) {
	    // dynamic size
	    t = CArrayType::create(t, 0);
	}
	else {
	    // normally a constant expression size
	    (*iter)->lint(&*iter, unsigned_type());
	    if (isAType(M1UnsignedConstant*, *iter)) {
		UData v = (*iter)->value();
		t = CArrayType::create(t, v.u);
	    }
	    else {
		gLint->warning(this, "array index not constant");
		t = CArrayType::create(t, 0);
	    }
	}
	iter++;
    }
    // Assign the new updated type
    decl->setType(t);  
    return decl;
}

// Check if id is already declarared. If so issue an error
CField* M1IdDeclarator::lint(CType* aType, CLintInfo* aInfo, int aStorage)
{
    CField* decl;

    if ((decl = gLint->findDeclaration(name(), true)) != NULL) {
	gLint->error(this, " '%s' is already defined", cname());
	return decl;
    }
    else if ((decl = gLint->findDeclaration(name(), false)) != NULL) {
	gLint->warning(this, " '%s' is already defined in parent type",
		       cname());
    }
    DBGFMT_LINT("M1IdDeclarator::lint() id=%s, type=%s", cname(), aType->cname());
    decl = gLint->addDeclaration(aStorage, mId, aType);

    if ((aInfo->level() > 0) && !(aStorage & Q_CONST)) {
	int pos = aInfo->nextFramePos();
	aInfo->expandFrame();
	decl->stackPos(pos);
    }
    return decl;
}

// Check that id is already declared in the base type.
CField* M1BaseIdDeclarator::lint(CType* aType, CLintInfo* aInfo, int aStorage)
{
    CField* decl;
    CType*  t = mTypeId->lint(Q_NONE);

    // Check that the field is present in type
    if ((t != error_type()) && (t->typeAt(mId) == NULL))
	gLint->error(this, "'%s' is not a member of '%s'",
		     mId.c_str(), t->cname());

    // Check that currentType is a derived type
    typeCheck(this, gLint->currentType(), t);
    
    if ((decl = ((CBaseType*)t)->field(mId)) == NULL) {
	gLint->error(this, "'%s' is not a member of '%s'",
		     mId.c_str(), t->cname());
	return gLint->addDeclaration(Q_NONE, mId, aType);
    }
    DBGFMT_LINT("M1BaseIdDeclarator::lint() id=%s, type=%s", cname(), 
		aType->cname());
    return decl;
}

// type/library interface declarations
// type interface Name [: Parent ] { declarations scripts [statements] }
// library interface Name [: Parent ] { declarations scripts [statements] }
//
CType* M1InterfaceSpecifier::lint(int aVisiblity)
{
    string aTypeName;
    CBaseType* parentType = NULL;
    CType* ct;

    if (mType != NULL)
	aTypeName = mType->name();
    else
	aTypeName = "";
    
    DBGFMT_LINT("M1InterfaceSpecifier::lint() type=%s", aTypeName.c_str());

    if (mParentType != NULL) {
	CType* pt = mParentType->lint(Q_NONE);
	if (pt == NULL)
	    gLint->error(this, "parent type '%s' is not defined",
			 mParentType->cname());
	else if (!isAType(CBaseType*, pt))
	    gLint->error(this, "parent type '%s' can not be derived",
			 pt->cname());
	parentType = dynamic_cast<CBaseType*>(pt);
    }
    else {
	parentType = VmObjectType::singleton();
    }

    if ((aTypeName != "") && (ct = gLint->lookupType(aTypeName)) != NULL) {
	string def = formatDefinitionType(mDefinitionType);
	gLint->error(this, "%s '%s' is already defined",
		     def.c_str(), aTypeName.c_str());
	return (CBaseType*) ct;
    }
    else {
	VmInterfaceType* t;
	list<M1Statement*>::iterator   iter;
	M1StatementList* defaults;  // FIXME no defualt allowed?

	t = m1New(VmInterfaceType, aTypeName, mDefinitionType);

	t->setParent(parentType);

	// create a combined type by copy parent chain Fields 
	/*** MAJOR FIXME
	if (parentType)
	    parentType->copyFields(t);
	****/

	// We must publish the type name at this point, in order
	// for recursive type to work (only through reference)
	if ((aTypeName != "") && (aTypeName[0] != '_')) {
	    // skip types starting with _
	    switch(mDefinitionType) {
	    case T_LIBRARY:
	    case T_APPLICATION:
		// MUST add library/application before subtype !!!!
		m1_context().updateField(Q_PUBLIC, aTypeName, t);
		break;
	    default:
		break;
	    }
	    gLint->addType(aVisiblity, aTypeName, t);
	}

	gLint->pushScope(t);

	// FIXME Dummy (fix parser not to accept default values)
	defaults  = new M1StatementList;
	if (mDeclarations) {
	    CLintInfo info;
	    mDeclarations->lint(defaults, &info);
	}

	return gLint->popScope();
    }
}

// object declarations
// type Name [: Parent ] { declarations scripts [statements] }
// library Name [: Parent ] { declarations scripts [statements] }
//
CType* M1TypeSpecifier::lint(int aVisiblity)
{
    string aTypeName;
    CBaseType* parentType = NULL;
    CType* ct;

    if (mType != NULL)
	aTypeName = mType->name();
    else
	aTypeName = "";
    
    DBGFMT_LINT("TypeSpecifier::lint() type=%s", aTypeName.c_str());

    if (mParentType != NULL) {
	CType* pt = mParentType->lint(Q_NONE);
	if (pt == NULL)
	    gLint->error(this, "parent type '%s' is not defined",
			 mParentType->cname());
	else if (!isAType(CBaseType*, pt))
	    gLint->error(this, "parent type '%s' can not be derived",
			 pt->cname());
	parentType = dynamic_cast<CBaseType*>(pt);
    }
    else {
	parentType = VmObjectType::singleton();
    }

    if ((aTypeName != "") && (ct = gLint->lookupType(aTypeName)) != NULL) {
	string def = formatDefinitionType(mDefinitionType);	
	gLint->error(this, "%s '%s' is already defined",
		     def.c_str(), aTypeName.c_str());
	return (CBaseType*) ct;
    }
    else {
	list<M1Statement*>::iterator   iter;
	list<M1Script*>::iterator scr_iter;
	M1StatementList* construct;
	M1StatementList* destruct;
	M1StatementList* defaults;
	VmEvalType* t;
	M1Script* scr_constructor = NULL;
	M1Script* scr_destructor = NULL;
	M1ScriptList* preScriptList = NULL;
	M1ScriptList* postScriptList = NULL;
	CLintInfo info;
	unsigned int cFrameSize = 0;
	unsigned int dFrameSize = 0;

	t = m1New(VmEvalType,mDefinitionType);
	t->setParent(parentType);
	if (aTypeName != "")
	    t->setName(aTypeName);
	// create a combined type by copy parent chain Fields 
	if (parentType)
	    parentType->copyFields(t);

	// We must publish the type name at this point, in order
	// for recursive type to work (only through reference)
	if (aTypeName[0] != '_') {
	    // skip types starting with _
	    switch(mDefinitionType) {
	    case T_LIBRARY:
	    case T_APPLICATION:
		// MUST add library/application before subtype !!!!
		m1_context().updateField(Q_PUBLIC, aTypeName, t);
		break;
	    default:
		break;
	    }
	    gLint->addType(aVisiblity, aTypeName, t);
	}

	gLint->pushScope(t);

	// Declare all fields
	defaults  = new M1StatementList;
	if (mDeclarations)
	    mDeclarations->lint(defaults, &info);

	// Lint all defaults statements
	for (iter=defaults->begin(); iter != defaults->end(); iter++) {
	    (*iter)->lint(&info);
	}

	scr_iter = mScriptList->begin();
	while(scr_iter != mScriptList->end()) {
	    if ((*scr_iter)->name() == "+") {
		(*scr_iter)->lint();
		if (preScriptList == NULL)
		    preScriptList = m1NewElement(*scr_iter,M1ScriptList);
		preScriptList->appendElement(*scr_iter);
		scr_iter++;
	    }
	    else if ((*scr_iter)->name() == "-") {
		(*scr_iter)->lint();
		if (postScriptList == NULL)
		    postScriptList = m1NewElement(*scr_iter,M1ScriptList);
		postScriptList->appendElement(*scr_iter);
		scr_iter++;
	    }
	    else if ((*scr_iter)->name() == "~"+aTypeName) {
		if (scr_destructor != NULL) {
		    gLint->error(this, "destructor already defined at line %d",
				 scr_destructor->line());
		    scr_iter++;
		}
		else {
		    scr_destructor = *scr_iter;
		    scr_destructor->lint();
		    dFrameSize = scr_destructor->frameSize();
		    scr_iter++;
		}
	    }
	    else if ((*scr_iter)->name() == aTypeName) {
		if (scr_constructor != NULL) {
		    gLint->error(this, "constructor already defined at line %d",
				 scr_constructor->line());
		    scr_iter++;
		}
		else {
		    scr_constructor = *scr_iter;
		    scr_constructor->lint();
		    cFrameSize = scr_constructor->frameSize();
		    scr_iter++;
		}
	    }
	    else {
		if (((*scr_iter)->name()[0]) == '~')
		    gLint->error(this, "bad context for destructor");
		else
		    gLint->error(this, "bad context for constructor");
		scr_iter++;
	    }
	}

	destruct = new M1StatementList;
	if (scr_destructor)
	    destruct->appendElement(scr_destructor->body());
	// Lint all statement and add them to construct code
	construct = new M1StatementList;
	if (scr_constructor)
	    construct->appendElement(scr_constructor->body());

	for (iter=mStatementList->begin();
	     iter != mStatementList->end(); iter++) {
	    (*iter)->lint(&info);
	    construct->appendElement(*iter);
	}

	t->setDefaults(defaults);
	t->setConstructor(construct);
	if (info.maxFrameSize() > cFrameSize)
	    cFrameSize = info.maxFrameSize();
	t->constructorFrameSize(cFrameSize);

	t->setDestructor(destruct);
	t->destructorFrameSize(dFrameSize);

	t->setPreScript(preScriptList);
	t->setPostScript(postScriptList);

	return gLint->popScope();
    }
}

// typedef <type-decl> <type-id>
CType* M1TypeDefSpecifier::lint(int aVisiblity)
{
    string aTypeName;
    CType* t;

    aTypeName = mType->name();

    if ((t = gLint->lookupType(aTypeName)) != NULL) {
	gLint->error(this, "'%s' is already defined",
		     aTypeName.c_str());
	return t;
    }
    t = mSpec->lint(aVisiblity);
    gLint->addType(aVisiblity, aTypeName, t);    
    return t;
}

void M1FieldDeclaration::lint(CLintInfo* aInfo)
{
    CType*  baseType;
    CField* decl;
    int     fStorage = storage();

    baseType = mTypeSpec->lint(Q_NONE);
    if ((fStorage & (Q_PUBLIC|Q_PRIVATE|Q_PROTECTED|Q_EXTERN)) == 0)
	fStorage |= Q_PUBLIC;
    setStorage(fStorage);

    decl = mDecl->lint(baseType, aInfo, fStorage);
    // decl->setStorage(fStorage);

    if ((aInfo->level() > 0) && (baseType->typeTag() == M1TYPE_EVENT)) {
	gLint->error(this, "event declaration not allowed in this context");
    }

    if (mInit != NULL) {
	if (mInitOp == op_CNCT) {
	    CType* lt = decl->type();
	    CType* rt;

	    mInit->lint(&mInit, decl->type());
	    rt = mInit->type();

	    if (!isInputEvent(lt))
		gLint->error(this, "bad connect destination");
	    else if (!isAType(M1Nil*, mInit)) {
		if (!isOutputEvent(rt))
		    rt = connectExpression(mInit, &mInit, lt);
		else if (((CEventType*) rt)->baseType() != 
			 ((CEventType*) lt)->baseType()) {
#ifdef DEBUG
		    gLint->warning(this, "type conversion in connect expression");
#endif
		    rt = connectExpression(mInit, &mInit, lt);
		}
	    }
	}
	else {
	    CType* baseType = decl->type();
	    CType* exprType = mInit->lint(&mInit, baseType);
	    if (fStorage & Q_CONST) {
		if (!isAType(M1Constant*, mInit)) {
                    // FIXME what about const Foo = @Foo {} ???
		    gLint->error(this, "must evaluate to a constant");
		}
		else
		    decl->setConstant(((M1Constant*)mInit)->value());
		// Must nuke mInit here to make sure assignment is generated
		m1ReleaseExpr(mInit);
		mInit = NULL;
	    }
	    typeCheck(this, exprType, baseType);
	}
    }
    else if (fStorage & Q_CONST)
	gLint->error(this, "constant object must be assigned a value");	
    DBGFMT_LINT("M1FieldDeclaration:lint() %s type:%s", 
		decl->cname(), decl->type()->cname());
}

void M1DeclarationList::lint(M1StatementList* aInitList, CLintInfo* aInfo)
{
    list<M1Declaration*>::iterator iter;
    CBaseType* scopeType = gLint->topType(); // Type being defined
    size_t startOffset = scopeType->fieldCount();

    DBGFMT_LINT("M1DeclarationList::lint: BEGIN offset=%d, typeCount=%d",
		scopeType->fieldTypeOffset(),
		(int) scopeType->fieldTypeCount());

    // Two rounds. 1 run forward and declarar all fields
    for (iter = mList.begin(); iter != mList.end(); iter++)
	(*iter)->lint(aInfo);


    // 2. run backwards and prepend statement onto aInitList
    // This is done to keep the initialization order
    iter = mList.end();
    while(iter != mList.begin()) {
	M1Declaration* decl;

	iter--;
	decl = *iter;
	if (isAType(M1FieldDeclaration*, decl)) {
	    M1FieldDeclaration* m1FDecl = (M1FieldDeclaration*) decl;
	    // If no errors then add constuctor
	    if (gLint->nError() == 0) {
		M1Expr*           m1Expr  = m1FDecl->initExpression();
		M1Expr*           m1Decl  = m1FDecl->declExpression();
		int               op1     = m1FDecl->initOperator();

		if (op1 == op_PUT)
		    op1 = op_INIT;
		if (m1Expr != NULL) {
		    M1Expr* expr = m1NewElement(this, M1BinaryExpr,
						op1,
						m1Decl, m1Expr);
		    M1Statement* stmt = m1NewElement(this,M1ExprStatement,expr);
		    aInitList->prependElement(stmt);
		    m1FDecl->setInitExpression(NULL); // was moved!
		}
	    }
	}
    }

    // After increamentally adding fields we must set the
    // correct fieldTypeCount & restore the offset. This is since
    // addDeclaration will call setupField each time and set mFieldCount=1
    // and offset will move.
    DBGFMT_LINT("M1DeclarationList::lint: END, offset=%d, typeCount=%d",
		(int) startOffset,
		(int)scopeType->fieldCount() - (int) startOffset);
    scopeType->setFieldTypeCount(scopeType->fieldCount() - startOffset);
    scopeType->setFieldTypeOffset(startOffset);
}

//
// [storage] specifier 
//  public type foo    { ... }
//  type interface bar { ... }
//
void M1DeclSpecifier::lint(CLintInfo* aInfo)
{
    mSpec->lint(storage());
}

void M1Script::lint(void)
{
    list<M1Expr*>::iterator iter;
    CLintInfo info;

    DBGFMT_LINT("M1Script::lint() called");

    // Trigger Expression must be a boolean expression with 
    // local input fields.
    if (mTrigger != NULL) 
	mTrigger->lint_trg(&mTrigger);
    if (mWhen != NULL)
	mWhen->lint(&mWhen, bool_type());
    mBody->lint(&info);
    // Set the overall frame size need 
    DBGFMT_LINT("M1Script::lint() maxFrameSize = %d", info.maxFrameSize());
    frameSize(info.maxFrameSize());
}

void M1ScriptList::lint(void)
{
    list<M1Script*>::iterator iter = mList.begin();
    while(iter != mList.end()) {
	(*iter)->lint();
	iter++;
    }
}

