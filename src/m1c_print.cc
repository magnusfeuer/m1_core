//
// M1 Language pretty printer
//

#include "m1vm.hh"
#include "m1c.hh"
#include "m1_parse.hh"

// #define PRINT_REF 1

void printIndent(ostream* os, int indent)
{
    while(indent--)
	*os << "  ";
}

void printNewLineIndent(ostream* os, int indent)
{
    *os << "\n";
    printIndent(os, indent);
}

string formatStorageClass(int sclass)
{
    string str = "";
    if (sclass & Q_PRIVATE)
	str += "private ";
    if (sclass & Q_PUBLIC)
	str += "public ";
    if (sclass & Q_EXTERN)
	str += "extern ";
    if (sclass & Q_PERSISTENT)
	str += "persistent ";
    if (sclass & Q_CONST)
	str += "const ";
    return str;
}

string formatDefinitionType(Definition_t def)
{
    switch(def) {
    case T_TYPE: return "type";
    case T_LIBRARY: return "library";
    case T_APPLICATION: return "application";	
    case T_TOPLEVEL: return "";
    default: return "";
    }
}

void printBody(VmEvalType* aEval, ostream* os, int indent, 
	       string aPrefix, int aStorage, string aName);

void printSubTypes(CBaseType* aBase, ostream* os, int indent)
{
    CBaseType* subt = aBase->subTypes();
    int i;

    if (subt == NULL)
	return;

    printNewLineIndent(os, indent+1);
    for (i = 0; i < (int)subt->fieldCount(); i++) {
	CField* fld = subt->field(i);
	CType* fld_type = fld->type();
	if (isAType(VmEvalType*, fld_type)) {
	    printBody((VmEvalType*)fld_type, os, indent+1, "type", 
		      fld->storage(), fld->name());
	}
	printNewLineIndent(os, indent+1);
    }
    printNewLineIndent(os, indent+1);
}


void printBody(VmEvalType* aEval, ostream* os, int indent, 
	       string aPrefix, int aStorage, string aName)
{
    int i;
    M1ScriptList* scriptList;
    M1StatementList* statementList;
    string pName = aEval->parentName();
    
    if (pName == "")
	*os << aPrefix << " " << aName << " {";
    else
	*os << aPrefix << " " << aName << " : " << pName << " {";
    printNewLineIndent(os, indent+1);
    // print types, note the order is not correct since
    // subtype may be used by fields! the order is not
    // relevant after linting....
    printSubTypes(aEval, os, indent+1);

    for (i = 0; i < (int)aEval->fieldCount(); i++) {
	if (i >= aEval->fieldTypeOffset()) {  // only display added members
	    CField* fld = aEval->field(i);
	    *os << formatStorageClass(fld->storage()) << 
		fld->type()->name() << " " << fld->name() << ";";
	    printNewLineIndent(os, indent+1);
	}
    }

    if ((scriptList = aEval->getPreScript()) != NULL) {
	list<M1Script*>::iterator iter  = scriptList->begin();
	printNewLineIndent(os, indent+1);
	while(iter != scriptList->end()) {
	    (*iter)->print(os, indent+1);
	    iter++;
	    printNewLineIndent(os, indent+1);
	}
    }

    if ((scriptList = aEval->getPostScript()) != NULL) {
	list<M1Script*>::iterator iter  = scriptList->begin();
	printNewLineIndent(os, indent+1);
	while(iter != scriptList->end()) {
	    (*iter)->print(os, indent+1);
	    iter++;
	    printNewLineIndent(os, indent+1);
	}
    }

    if ((statementList = aEval->getDefaults()) != NULL) {
	list<M1Statement*>::iterator iter = statementList->begin();
	printNewLineIndent(os, indent+1);
	*os << "script _DEFAULT {";
	while(iter != statementList->end()) {
	    printNewLineIndent(os, indent+2);
	    (*iter)->print(os, indent+2);
	    iter++;
	}
	printNewLineIndent(os, indent+1);
	*os << "}";
    }

    if ((statementList = aEval->getConstructor()) != NULL) {
	list<M1Statement*>::iterator iter = statementList->begin();
	printNewLineIndent(os, indent+1);
	*os << "script " << aName << " /* frameSize=" 
	    << aEval->constructorFrameSize() << "*/" << " {";
	while(iter != statementList->end()) {
	    printNewLineIndent(os, indent+2);
	    (*iter)->print(os, indent+2);
	    iter++;
	}
	printNewLineIndent(os, indent+1);
	*os << "}";
    }

    if ((statementList = aEval->getDestructor()) != NULL) {
	list<M1Statement*>::iterator iter = statementList->begin();
	printNewLineIndent(os, indent+1);
	*os << "script ~" << aName << " /* frameSize=" 
	    << aEval->destructorFrameSize() << "*/" << " {";
	while(iter != statementList->end()) {
	    printNewLineIndent(os, indent+2);
	    (*iter)->print(os, indent+2);
	    iter++;
	}
	printNewLineIndent(os, indent+1);
	*os << "}";
    }
    printNewLineIndent(os, indent);
    *os << "};";
    printNewLineIndent(os, indent);
}

void printDefinitions(VmEvalType* aEval, ostream* os)
{
    if (aEval->name() == "_FileScope")
	printBody(aEval, os, 0, "implicit", Q_PUBLIC,  "_FileScope");
    else 
	printBody(aEval, os, 0, "library", Q_PUBLIC, aEval->name());
}

int operatorPriority(int op)
{
    switch(op) {
    case op_ID: return 0;


    case op_RANGE: return 50;
    case op_SEQ: return 50;

    case op_MULPUT: return 100;
    case op_MULPUTi: return 100;
    case op_MULPUTu: return 100;
    case op_MULPUTb: return 100;
    case op_MULPUTc: return 100;
    case op_MULPUTt: return 100;
    case op_MULPUTf: return 100;

    case op_DIVPUT: return 100;
    case op_DIVPUTi: return 100;
    case op_DIVPUTu: return 100;
    case op_DIVPUTb: return 100;
    case op_DIVPUTc: return 100;
    case op_DIVPUTt: return 100;
    case op_DIVPUTf: return 100;

    case op_REMPUT: return 100;
    case op_REMPUTi: return 100;
    case op_REMPUTu: return 100;
    case op_REMPUTb: return 100;
    case op_REMPUTc: return 100;
    case op_REMPUTt: return 100;

    case op_ADDPUT: return 100;
    case op_ADDPUTi: return 100;
    case op_ADDPUTu: return 100;
    case op_ADDPUTb: return 100;
    case op_ADDPUTc: return 100;
    case op_ADDPUTt: return 100;
    case op_ADDPUTf: return 100;
    case op_ADDPUTs: return 100;
    case op_ADDPUTsc: return 100;
    case op_ADDPUTa: return 100;
    case op_ADDPUTae: return 100;

    case op_SUBPUT: return 100;
    case op_SUBPUTi: return 100;
    case op_SUBPUTu: return 100;
    case op_SUBPUTb: return 100;
    case op_SUBPUTc: return 100;
    case op_SUBPUTt: return 100;
    case op_SUBPUTf: return 100;
    case op_SUBPUTsc: return 100;
    case op_SUBPUTa: return 100;
    case op_SUBPUTae: return 100;

    case op_BSLPUT: return 100;
    case op_BSLPUTu: return 100;
    case op_BSLPUTi: return 100;
    case op_BSLPUTb: return 100;
    case op_BSLPUTc: return 100;
    case op_BSLPUTt: return 100;

    case op_BSRPUT: return 100;
    case op_BSRPUTu: return 100;
    case op_BSRPUTi: return 100;
    case op_BSRPUTb: return 100;
    case op_BSRPUTc: return 100;
    case op_BSRPUTt: return 100;

    case op_BANDPUT: return 100;
    case op_BANDPUTu: return 100;
    case op_BANDPUTi: return 100;
    case op_BANDPUTb: return 100;
    case op_BANDPUTc: return 100;
    case op_BANDPUTt: return 100;

    case op_BXORPUT: return 100;
    case op_BXORPUTu: return 100;
    case op_BXORPUTi: return 100;
    case op_BXORPUTb: return 100;
    case op_BXORPUTc: return 100;
    case op_BXORPUTt: return 100;

    case op_BORPUT: return 100;
    case op_BORPUTu: return 100;
    case op_BORPUTi: return 100;
    case op_BORPUTb: return 100;
    case op_BORPUTc: return 100;
    case op_BORPUTt: return 100;

    case op_PUT: return 100;
    case op_PUTu: return 100;
    case op_PUTi: return 100;
    case op_PUTf: return 100;
    case op_PUTb: return 100;
    case op_PUTc: return 100;
    case op_PUTt: return 100;
    case op_PUTo: return 100;
    case op_PUTs: return 100;

    case op_COPY: return 100;
    case op_CNCT: return 100;
    case op_INIT: return 100;

    case op_LT:    return 300;
    case op_LTE:   return 300;
    case op_GT:    return 300;
    case op_GTE:   return 300;
    case op_EQ:    return 300;
    case op_EQL:   return 300;
    case op_NEQ:   return 300;
    case op_NQL:   return 300;

    case op_ADD:   return 400;
    case op_ADDu:  return 400;
    case op_ADDi:  return 400;
    case op_ADDf:  return 400;
    case op_ADDs:  return 400;
    case op_ADDa:  return 400;
    case op_ADDae: return 400;
    case op_ADDea: return 400;

    case op_SUB:   return 400;
    case op_SUBu:  return 400;
    case op_SUBi:  return 400;
    case op_SUBf:  return 400;
    case op_SUBs:  return 400;
    case op_SUBsc:  return 400;
    case op_SUBa:  return 400;
    case op_SUBae: return 400;

    case op_BSL:   return 400;
    case op_BSLu:  return 400;
    case op_BSLi:  return 400;
    case op_BSR:   return 400;
    case op_BSRu:  return 400;
    case op_BSRi:  return 400;
    case op_BOR:   return 400;  
    case op_BORu:  return 400;  
    case op_BXOR:  return 400; 
    case op_BXORu: return 400; 
    case op_OR:    return 400;
    case op_ORi:   return 400;

    case op_MUL: return 500;
    case op_MULu: return 500;
    case op_MULi: return 500;
    case op_MULf: return 500;

    case op_DIV: return 500;
    case op_DIVu: return 500;
    case op_DIVi: return 500;
    case op_DIVf: return 500;

    case op_REM: return 500;
    case op_REMu: return 500;
    case op_REMi: return 500;

    case op_BAND: return 500;
    case op_BANDu: return 500;
    case op_AND:  return 500;
    case op_ANDi:  return 500;

    case op_PSTINC: return 600;
    case op_PSTDEC: return 600;
    case op_PREINC:return 600;
    case op_PREDEC: return 600;

    case op_INCi: return 600;
    case op_INCu: return 600;
    case op_INCb: return 600;
    case op_INCc: return 600;
    case op_INCt: return 600;
    case op_INCf: return 600;

    case op_DECi: return 600;
    case op_DECu: return 600;
    case op_DECb: return 600;
    case op_DECc: return 600;
    case op_DECt: return 600;
    case op_DECf: return 600;

    case op_POS: return 600;
    case op_NEG: return 600;
    case op_NEGi: return 600;
    case op_NEGu: return 600;
    case op_NEGf: return 600;
    case op_BNOT: return 600;
    case op_BNOTu: return 600;
    case op_NOT: return 600;
    case op_NOTi: return 600;

    case op_ELEM: return 700;
    case op_FLD: return 700;
    default: return 600; 
    }
}

string formatOperator(int op)
{
    switch(op) {
    case op_ELEM: return "[";

    case op_LT:   return "<";
    case op_LTE:  return "<=";
    case op_GT:   return ">";
    case op_GTE:  return ">=";
    case op_EQ:   return "==";
    case op_EQL:  return "=:=";
    case op_NEQ:  return "!=";
    case op_NQL:  return "=!=";

    case op_CNCT: return "<-";
    case op_MULPUT: return "*=";
    case op_MULPUTi: return "*i=";
    case op_MULPUTu: return "*u=";
    case op_MULPUTb: return "*b=";
    case op_MULPUTc: return "*c=";
    case op_MULPUTt: return "*t=";
    case op_MULPUTf: return "*f=";

    case op_DIVPUT: return "/=";
    case op_DIVPUTi: return "/i=";
    case op_DIVPUTu: return "/u=";
    case op_DIVPUTb: return "/b=";
    case op_DIVPUTc: return "/c=";
    case op_DIVPUTt: return "/t=";
    case op_DIVPUTf: return "/f=";

    case op_REMPUT: return "%=";
    case op_REMPUTi: return "%i=";
    case op_REMPUTu: return "%u=";
    case op_REMPUTb: return "%b=";
    case op_REMPUTc: return "%c=";
    case op_REMPUTt: return "%t=";

    case op_ADDPUT: return "+=";
    case op_ADDPUTi: return "+i=";
    case op_ADDPUTu: return "+u=";
    case op_ADDPUTb: return "+b=";
    case op_ADDPUTc: return "+c=";
    case op_ADDPUTt: return "+t=";
    case op_ADDPUTf: return "+f=";
    case op_ADDPUTs: return "+s=";
    case op_ADDPUTsc: return "+sc=";
    case op_ADDPUTa: return "+a=";
    case op_ADDPUTae: return "+ae=";

    case op_SUBPUT: return "-=";
    case op_SUBPUTi: return "-i=";
    case op_SUBPUTu: return "-u=";
    case op_SUBPUTb: return "-b=";
    case op_SUBPUTc: return "-c=";
    case op_SUBPUTt: return "-t=";
    case op_SUBPUTf: return "-f=";
    case op_SUBPUTs: return "-s=";
    case op_SUBPUTsc: return "-sc=";
    case op_SUBPUTa: return "-a=";
    case op_SUBPUTae: return "-ae=";

    case op_BSLPUT: return "<<=";
    case op_BSLPUTu: return "<<u=";
    case op_BSLPUTi: return "<<i=";
    case op_BSLPUTb: return "<<b=";
    case op_BSLPUTc: return "<<c=";
    case op_BSLPUTt: return "<<t=";

    case op_BSRPUT: return ">>=";
    case op_BSRPUTu: return ">>u=";
    case op_BSRPUTi: return ">>i=";
    case op_BSRPUTb: return ">>b=";
    case op_BSRPUTc: return ">>c=";
    case op_BSRPUTt: return ">>t=";

    case op_BANDPUT: return "&=";
    case op_BANDPUTu: return "&u=";
    case op_BANDPUTi: return "&i=";
    case op_BANDPUTb: return "&b=";
    case op_BANDPUTc: return "&c=";
    case op_BANDPUTt: return "&t=";

    case op_BXORPUT: return "^=";
    case op_BXORPUTu: return "^u=";
    case op_BXORPUTi: return "^i=";
    case op_BXORPUTb: return "^b=";
    case op_BXORPUTc: return "^c=";
    case op_BXORPUTt: return "^t=";

    case op_BORPUT: return "|=";
    case op_BORPUTu: return "|u=";
    case op_BORPUTi: return "|i=";
    case op_BORPUTb: return "|b=";
    case op_BORPUTc: return "|c=";
    case op_BORPUTt: return "|t=";

    case op_RANGE: return "..";
    case op_FLD: return ".";
    case op_PUT: return "=";
    case op_INIT: return "=";
    case op_PUTu: return "=u";
    case op_PUTi: return "=i";
    case op_PUTf: return "=f";
    case op_PUTc: return "=c";
    case op_PUTb: return "=b";
    case op_PUTt: return "=t";
    case op_PUTs: return "=s";
    case op_PUTo: return "=o";
    case op_COPY: return ":=";
    case op_SEQ: return ",";
    case op_ADD: return "+";
    case op_ADDu: return "+u";
    case op_ADDi: return "+i";
    case op_ADDf: return "+f";
    case op_ADDs: return "+s";
    case op_ADDsc: return "+sc";
    case op_ADDcs: return "+cs";
    case op_ADDa: return "+a";
    case op_ADDae: return "+ae";
    case op_ADDea: return "+ea";

    case op_SUB: return "-";
    case op_SUBu: return "-u";
    case op_SUBi: return "-i";
    case op_SUBf: return "-f";
    case op_SUBs: return "-s";
    case op_SUBsc: return "-sc";
    case op_SUBa: return "-a";
    case op_SUBae: return "-ae";

    case op_MUL: return "*";
    case op_MULu: return "*u";
    case op_MULi: return "*i";
    case op_MULf: return "*f";

    case op_DIV: return "/";
    case op_DIVu: return "/u";
    case op_DIVi: return "/i";
    case op_DIVf: return "/f";

    case op_REM: return "%";
    case op_REMu: return "%u";
    case op_REMi: return "%i";

    case op_BSL:  return "<<";
    case op_BSLu:  return "<<u";
    case op_BSLi:  return "<<i";

    case op_BSR:  return ">>";
    case op_BSRu:  return ">>u";
    case op_BSRi:  return ">>i";

    case op_BAND: return "&";
    case op_BANDu: return "&u";

    case op_BOR: return "|";  
    case op_BORu: return "|u";  

    case op_BXOR: return "^"; 
    case op_BXORu: return "^u"; 

    case op_AND:  return "&&";
    case op_ANDi:  return "&&i";
    case op_OR:   return "||";
    case op_ORi:   return "||i";

    case op_PSTINC: return "++";
    case op_PSTDEC: return "--";
    case op_PREINC:return "++";
    case op_PREDEC: return "--";

    case op_INCi: return "+i+";
    case op_INCu: return "+u+";
    case op_INCb: return "+b+";
    case op_INCc: return "+c+";
    case op_INCt: return "+t+";
    case op_INCf: return "+f+";

    case op_DECi: return "-i-";
    case op_DECu: return "-u-";
    case op_DECb: return "-b-";
    case op_DECc: return "-c-";
    case op_DECt: return "-t-";
    case op_DECf: return "-f-";

    case op_POS: return "+";
    case op_NEG: return "-";
    case op_NEGi: return "-i";
    case op_NEGu: return "-u";
    case op_NEGf: return "-f";
    case op_BNOT: return "~";
    case op_BNOTu: return "~u";
    case op_NOT: return "!";
    case op_NOTi: return "!i";
    case op_TRIGGER: return "*";
    case op_SUPRESS: return "**";
    default: return "????";
    }
}


void printDefinitions(ostream* os, M1TypeSpecifier* decl)
{
    decl->print(os, 0);
}

void M1Identifier::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << mID;
}

void M1BasedIdentifier::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << "@";
    if (mTypeId)
	mTypeId->print(os, indent);
    *os << "." << mId;
}

void M1Index::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << "&";
    if (mTypeId) {
	mTypeId->print(os, indent);
	*os << ".";
    }
    *os << mId;
}

void M1Field::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    mObject->print(os, indent);
    *os << "." << mId;
}

void M1Nil::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << "nil";
}

void M1This::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << "this";
}

void M1BoolConstant::print(ostream* os, int indent)
{
    bool_type()->print(os, mValue);
}

void M1ByteConstant::print(ostream* os, int indent)
{
    byte_type()->print(os, mValue);
}

void M1CharConstant::print(ostream* os, int indent)
{
    char_type()->print(os, mValue);
}

void M1SignedConstant::print(ostream* os, int indent)
{
    signed_type()->print(os, mValue);
}

void M1UnsignedConstant::print(ostream* os, int indent)
{
    unsigned_type()->print(os, mValue);
}

void M1FloatConstant::print(ostream* os, int indent)
{
    float_type()->print(os,  mValue);
}

void M1StringConstant::print(ostream* os, int indent)
{
    string_type()->print(os, mValue);
}

void M1TrinaryExpr::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    switch(op()) {
    case op_COND:
	mFst->print(os, indent);
	*os << "?";
	mSnd->print(os, indent);
	*os << ":";
	mThrd->print(os, indent);
	break;
    case op_PROG2:
	if (mFst) mFst->print(os, indent);
	*os << ";";
	mSnd->print(os, indent);
	*os << ";";
	if (mThrd) mThrd->print(os, indent);
	break;
    case op_RANGE:
	*os << "[";
	mFst->print(os, indent);
	if (mSnd || mThrd) {
	    *os << ":";
	    if (mSnd)
		mSnd->print(os, indent);
	    if (mThrd) {
		*os << ":";
		mThrd->print(os, indent);
	    }
	}
	*os << "]";
	break;
    }
}

static void expr_print(ostream* os, M1Expr* expr, int indent)
{
    if (expr)
	expr->print(os, indent);
    else
	*os << "nil";
}

// FIXME add parentheses when needed!
void M1BinaryExpr::print(ostream* os, int indent)
{
    int leftPrio;
    int rightPrio;
    int prio;
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    switch(op()) {
    case op_CMPu:
	*os << "_cmp_u(";
	expr_print(os, mLeft, indent);	
	*os << ",";
	expr_print(os, mRight, indent);	
	*os << ")";
	break;
    case op_CMPi:
	*os << "_cmp_i(";
	expr_print(os, mLeft, indent);	
	*os << ",";
	expr_print(os, mRight, indent);	
	*os << ")";
	break;
    case op_CMPf:
	*os << "_cmp_f(";
	expr_print(os, mLeft, indent);	
	*os << ",";
	expr_print(os, mRight, indent);	
	*os << ")";
	break;
    case op_CMPo:
	*os << "_cmp_o(";
	expr_print(os, mLeft, indent);	
	*os << ",";
	expr_print(os, mRight, indent);	
	*os << ")";
	break;
    case op_ELEM:
	expr_print(os, mLeft, indent); 
	*os << "[";
	expr_print(os, mRight, indent); 
	*os << "]";
	break;
    default:
	leftPrio  = operatorPriority(mLeft->op());
	rightPrio = operatorPriority(mRight->op());
	prio = operatorPriority(op());

	if (leftPrio && (leftPrio < prio)) {
	    *os << "("; expr_print(os, mLeft, indent); *os << ")";
	}
	else
	    expr_print(os, mLeft, indent);

	if (op() != op_FLD)
	    *os << " " << formatOperator(op()) << " ";
	else
	    *os << formatOperator(op());

	if (rightPrio && (rightPrio < prio)) {
	    *os << "("; expr_print(os,mRight,indent); *os << ")";
	}
	else
	    expr_print(os,mRight,indent);
	break;
    }	
}
	

void M1UnaryExpr::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    switch(op()) {
    case op_PSTINC:
	expr_print(os,mExpr,indent); *os << "++"; break;
    case op_PSTDEC:
	expr_print(os,mExpr,indent); *os << "--"; break;
    case op_PREINC:
	*os << "++"; expr_print(os,mExpr,indent); break;
    case op_PREDEC:
	*os << "--"; expr_print(os,mExpr,indent); break;
    case op_INCi: 
	*os << "_inc_i("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_INCu: 
	*os << "_inc_u("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_INCb: 
	*os << "_inc_b("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_INCc: 
	*os << "_inc_c("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_INCt: 
	*os << "_inc_t("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_INCf: 
	*os << "_inc_f("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_DECi: 
	*os << "_dec_i("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_DECu: 
	*os << "_dec_u("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_DECb: 
	*os << "_dec_b("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_DECc: 
	*os << "_dec_c("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_DECt: 
	*os << "_dec_t("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_DECf: 
	*os << "_dec_f("; expr_print(os,mExpr,indent); *os<<")"; break;
    case op_LTz:
	expr_print(os,mExpr,indent); *os << "<0"; break;
    case op_LTEz:
	expr_print(os,mExpr,indent); *os << "<=0"; break;
    case op_GTz:
	expr_print(os,mExpr,indent); *os << ">0"; break;
    case op_GTEz:
	expr_print(os,mExpr,indent); *os << ">=0"; break;
    case op_EQz:
	expr_print(os,mExpr,indent); *os << "==0"; break;
    case op_NEQz:
	expr_print(os,mExpr,indent); *os << "!=0"; break;
    default:
	*os << formatOperator(op());
	expr_print(os,mExpr,indent); 
	break;
    }
}

void M1CvtExpr::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << "_cvt_" <<
	M1CVT_selector(M1CVT_from(mCvt)) <<
	M1CVT_selector(M1CVT_to(mCvt)) << "(";
    mExpr->print(os, indent);
    *os << ")";
}

void M1CastExpr::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    *os << "@";
    mTypeId->print(os, indent);
    *os << "(";
    mExpr->print(os, indent);
    *os << ")";
}

void M1ExprList::print(ostream* os, int indent)
{
    list<M1Expr*>::iterator iter = mList.begin();
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    if (iter != mList.end()) {
	(*iter)->print(os, indent);
	iter++;
	while(iter != mList.end()) {
	    *os << ",";
	    (*iter)->print(os, indent);
	    iter++;
	}
    }
}

void M1CallExpr::print(ostream* os, int indent)
{
#if PRINT_REF
    *os << "/* #Ref=" << refCount() << "*/";
#endif
    expr_print(os,mFunc,indent);
    *os << "(";
    if (mArgs)
	mArgs->print(os, indent);
    *os << ")";    
}

void M1BuiltinExpr::print(ostream* os, int indent)
{
    *os << mBf->name << "(";
    if (mArgs)
	mArgs->print(os, indent);
    *os << ")";    
}

void M1TypePrimitive::print(ostream* os, int indent)
{
    *os << mType->name();
}

void M1TypeIdentifier::print(ostream* os, int indent)
{
    *os << name();
}

void M1EventTypeIdentifier::print(ostream* os, int indent)
{
    *os << name();
}


void M1Enum::print(ostream* os, int indent)
{
    *os << mId;
    if (mExpr != NULL) {
	*os << " = ";
	mExpr->print(os, indent);
    }
}

void M1EnumList::print(ostream* os, int indent)
{
    list<M1Enum*>::iterator iter = mList.begin();
    if (iter != mList.end()) {
	(*iter)->print(os, indent);
	iter++;
	while(iter != mList.end()) {
	    *os << ",";
	    (*iter)->print(os, indent);
	    iter++;
	}
    }
}


void M1ArrayTypeDeclarator::print(ostream* os, int indent)
{
    mBaseType->print(os, indent);
    *os << "[";
    if (mSize != NULL)
	mSize->print(os, indent);
    *os << "]";
}


void M1ObjectConstant::print(ostream* os, int indent)
{
    *os << "@";
    mTypeId->print(os);
    *os << "{";
    if (mInitList)
	mInitList->print(os, indent);
    *os << "}";
}

void M1ArrayConstant::print(ostream* os, int indent)
{
    *os << "{";
    if (mInitList)
	mInitList->print(os, indent);
    *os << "}";
}

void M1StatementList::print(ostream* os, int indent)
{
    list<M1Statement*>::iterator iter = mList.begin();

    if (iter != mList.end()) 
	printNewLineIndent(os, indent);
    if (iter != mList.end()) {
	(*iter)->print(os, indent);
	iter++;
	while(iter != mList.end()) {
	    printNewLineIndent(os, indent);
	    (*iter)->print(os, indent);
	    iter++;
	}
    }
}

void M1IfStatement::print(ostream* os, int indent)
{
    *os << "if ("; mCondition->print(os, indent); *os << ") ";
    mThen->print(os, indent);
    if (mElse != NULL) {
	*os << " else ";
	mElse->print(os,indent);
    }
}

void M1SwitchStatement::print(ostream* os, int indent)
{
    *os << "switch ("; mValue->print(os, indent); *os << ") ";
    if ((mDeclarations == NULL) && (mStatements == NULL)) {
	*os << "{";
	indent++;
	indent--;
	printNewLineIndent(os, indent);
	*os << "}";
    }
    else {
	*os << "{";
	indent++;
	if (mDeclarations)
	    mDeclarations->print(os, indent);

	if (mStatements)
	    mStatements->print(os, indent);

	indent--;
	printNewLineIndent(os, indent);	
	*os << "}";
    }
}


void M1CaseStatement::print(ostream* os, int indent)
{
    mExpr->print(os, indent);
    *os << ": ";
    mStatement->print(os, indent+1);
}

void M1DefaultStatement::print(ostream* os, int indent)
{
    *os << "default: ";
    mStatement->print(os, indent+1);
}

void M1JumpStatement::print(ostream* os, int indent)
{
    switch(mWhere) {
    case m1::m1Parser::token::RETURN:
	*os << "return";
	if (mExpr != NULL)
	    mExpr->print(os, indent);
	break;
    case m1::m1Parser::token::CONTINUE:
	*os << "continue";
	break;
    case m1::m1Parser::token::BREAK:
	*os << "break";
	break;
    }
}

void M1CompoundStatement::print(ostream* os, int indent)
{
    if ((mDeclarations == NULL) && (mStatements == NULL)) {
	*os << "{";
	indent++;
	indent--;
	printNewLineIndent(os, indent);
	*os << "}";
    }
    else {
	*os << "{";
	indent++;
	if (mDeclarations) 
	    mDeclarations->print(os,indent);

	if (mStatements) 
	    mStatements->print(os,indent);

	indent--;
	printNewLineIndent(os, indent);	
	*os << "}";
    }
}

void M1ForEachStatement::print(ostream* os, int indent)
{
    *os << "foreach ";
    mVar->print(os, indent);
    *os << " in ";
    mRange->print(os, indent);
    *os << " ";
    mStatement->print(os, indent);
}

void M1ExprStatement::print(ostream* os, int indent)
{
    if (mExpr != NULL) 
	mExpr->print(os, indent);
    *os << ";";
}

void M1DeclarationList::print(ostream* os, int indent)
{
    list<M1Declaration*>::iterator iter = mList.begin();

    while(iter != mList.end()) {
	printNewLineIndent(os, indent);	
	(*iter)->print(os, indent); *os << ";";
	iter++;
    }
}

void M1ArrayDeclarator::print(ostream* os, int indent)
{
    list<M1Expr*>::iterator iter = mSizeList->begin();
    mDecl->print(os, indent);
    while(iter != mSizeList->end()) {
	*os << "[";
	if (!isAType(M1Nil*, *iter))
	    (*iter)->print(os, indent);
	*os << "]";
	iter++;
    }
}

void M1IdDeclarator::print(ostream* os, int indent)
{
    *os << mId;
}

void M1BaseIdDeclarator::print(ostream* os, int indent)
{
    *os << "@";
    mTypeId->print(os, indent);
    *os << "." << mId;
}

// Print version if needed
void M1Version::print(ostream* os, int indent)
{
    if ((mMajor>0) || (mMinor>0)) {
	*os << "(" << mMajor << "," << mMinor;
	if (mPatch > 0)
	    *os << mPatch;
	*os << ") ";
    }
}

void M1TypeSpecifier::print(ostream* os, int indent)
{
    string def = formatDefinitionType(mDefinitionType);
    if (def != "")
	*os << def << " ";
    if (mType) {
	mType->print(os, indent);
	*os << " ";
	if ( ((mDefinitionType==T_LIBRARY) ||
	      (mDefinitionType==T_APPLICATION)) && mVersion)
	    mVersion->print(os, indent);
    }

    if (mParentType != NULL) {
	*os << ": ";
	mParentType->print(os, indent);
	*os << " ";
    }

    *os << "{";
    indent++;
    if (mDeclarations)
	mDeclarations->print(os, indent);

    if (mScriptList)
	mScriptList->print(os,indent);

    if (mStatementList)
	mStatementList->print(os,indent);

    indent--;
    printNewLineIndent(os, indent);
    *os << "}";
}

void M1TypeDefSpecifier::print(ostream* os, int indent)
{
    *os << "typedef ";
    mSpec->print(os, indent);
    *os << " ";
    mType->print(os, indent);
}

void M1InterfaceSpecifier::print(ostream* os, int indent)
{
    string def = formatDefinitionType(mDefinitionType);
    if (def != "")
	*os << "interface " << def << " ";
    if (mType) {
	mType->print(os, indent);
	*os << " ";
	if ( ((mDefinitionType==T_LIBRARY) ||
	      (mDefinitionType==T_APPLICATION)) && mVersion)
	    mVersion->print(os, indent);
    }

    if (mParentType != NULL) {
	*os << ": ";
	mParentType->print(os, indent);
	*os << " ";
    }

    *os << "{";
    indent++;
    if (mDeclarations)
	mDeclarations->print(os, indent);
    indent--;
    printNewLineIndent(os, indent);
    *os << "}";
}

void M1EnumSpecifier::print(ostream* os, int indent)
{
    *os << "enum ";
    if (mType) {
	mType->print(os, indent);
	*os << " ";
    }
    *os << "{";
    mEnumList->print(os, indent);
    *os << "}";
}

void M1DeclSpecifier::print(ostream* os, int indent)
{
    *os << formatStorageClass(storage()); 
    mSpec->print(os, indent);
}


void M1FieldDeclaration::print(ostream* os, int indent)
{
    *os << formatStorageClass(storage()); 
    mTypeSpec->print(os, indent);
    *os << " ";
    mDecl->print(os, indent);
    if (mInit != NULL) {
	if (mInitOp == op_PUT)
	    *os << " = ";
	else if (mInitOp == op_CNCT)
	    *os << " <- ";
	else
	    *os << " ??? ";
	mInit->print(os, indent);
    }
}


void M1Script::print(ostream* os, int indent)
{
    if (mId == "+")
	*os << "+script ";
    else if (mId == "-")
	*os << "+script ";
    else
	*os << "script " << mId << " ";
    if (mTrigger != NULL) {
	mTrigger->print(os, indent);
	*os << " ";
    }
    if (mWhen != NULL) {
	*os << "when ";
	mWhen->print(os, indent);
    }
    *os << "/* frameSize = " << frameSize() << " */";
    mBody->print(os, indent);
}

void M1ScriptList::print(ostream* os, int indent)
{
    list<M1Script*>::iterator iter = mList.begin();
    
    if (iter != mList.end())
	printNewLineIndent(os, indent);
    while(iter != mList.end()) {
	printNewLineIndent(os, indent);
	(*iter)->print(os, indent);
	iter++;
    }
}

