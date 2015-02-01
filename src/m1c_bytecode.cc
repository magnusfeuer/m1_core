//
// M1 Language compiler
//

#include "m1vm.hh"
#include "m1c.hh"
#include "m1_parse.hh"


void M1BinaryExpr::bytecode(ostream* os) 
{
    mLeft->bytecode(os);
    mRight->bytecode(os);
    switch(op()) {
    case op_PUT:
	break;
    case op_ADD:
	os->put(op_ADDi);  // or ADDf, ADDu, ADDb, ADDc
	break;
    case op_SUB:
	os->put(op_SUBi);  // or SUBf, SUBu, SUBb, SUBc
	break;
    case op_MUL:
	os->put(op_MULi);  // or MULf, MULu, MULb, MULc
	break;
    }
}

void M1UnaryExpr::bytecode(ostream* os)  
{
    mExpr->bytecode(os);
    switch(op()) {
    case op_POS:
	break; // nop
    case op_NEG:
	os->put(op_NEGi);  // or NEGf, NEGc
	break;
    case op_NOT:
	os->put(op_NOTi);
	break;
    case op_BNOT:
	os->put(op_BNOTu);
	break;
    }	    
}

void M1CvtExpr::bytecode(ostream* os)
{
    mExpr->bytecode(os);
}


void M1CastExpr::bytecode(ostream* os)
{
    mExpr->bytecode(os);
}
    

void M1CallExpr::bytecode(ostream* os)
{
}

void M1BuiltinExpr::bytecode(ostream* os)
{
}

void M1ByteConstant::bytecode(ostream* os) 
{
    os->put(op_PUSHu);
    put_u32(os, mValue.b);
}

void M1CharConstant::bytecode(ostream* os) 
{
    os->put(op_PUSHi);
    put_i32(os, mValue.c);
}

void M1SignedConstant::bytecode(ostream* os) 
{
    os->put(op_PUSHi);
    put_i32(os, mValue.i);
}

void M1UnsignedConstant::bytecode(ostream* os) 
{
    os->put(op_PUSHu);
    put_u32(os, mValue.u);
}

void M1FloatConstant::bytecode(ostream* os) 
{
    os->put(op_PUSHf);
    put_f32(os, mValue.f);
}


void M1Nil::bytecode(ostream* os)
{
    os->put(op_NIL);
}

void M1This::bytecode(ostream* os)
{
    os->put(op_THIS);
}

void M1BoolConstant::bytecode(ostream* os)
{
    os->put(op_PUSHi);
    put_i32(os, (mValue.t == true) ? 1 : 0);
}

void M1StringConstant::bytecode(ostream* os)
{
    os->put(op_PUSHs);
    os->put(mValue.str->size());
    os->write(mValue.str->c_str(), mValue.str->size());
}

void M1ObjectConstant::bytecode(ostream* os)
{
    
}

void M1ArrayConstant::bytecode(ostream* os)
{
    
}
