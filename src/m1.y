%{
#include "m1vm.hh"
#include "m1vm_opcodes.hh"
#include "m1c.hh"
#include "m1_lex.hh"
#define YYDEBUG 1

%}

%union {
    int                ival;
    CString*           sval;
    CType*             type;
    M1Expr*            expr;
    M1ExprList*        expr_list;
    M1Statement*       stmt;
    M1StatementList*   stmt_list;
    M1Declaration*     decl;
    M1DeclarationList* decl_list;
    M1Enum*            enm;
    M1EnumList*        enm_list;
    M1Script*          scr;
    M1ScriptList*      scr_list;
    M1Declarator*      decr;
    M1Specifier*       spec;
    M1TypeSpecifier*   odecl;
    M1Version*         ver;
    struct {
	M1Expr* when;
	M1Statement* body;
    } scr_body;
}

%skeleton "lalr1pp.cc"
%require "2.3"
%defines
%define "parser_class_name" "m1Parser"
%define "push_parser" "1"
%name-prefix="m1"
%file-prefix="m1_"
%locations
%parse-param { M1Lex* mLex }
%lex-param { M1Lex* mLex }

%token M1BLOCK_COMMENT M1LINE_COMMENT
%token M1IDENTIFIER M1CHAR M1INTEGER M1FLOAT M1STRING
%token SHIFTL_OP SHIFTR_OP LE_OP GE_OP EQ_OP NE_OP EQL_OP NQL_OP
%token INC_OP DEC_OP POST_INC_OP POST_DEC_OP PRE_INC_OP PRE_DEC_OP
%token AND_OP OR_OP COPY_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN SHIFTL_ASSIGN SHIFTR_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN M1TIDENTIFIER
%token CONNECT_OP
%token M1NIL M1THIS M1TRUE M1FALSE
%token TYPE LIBRARY APPLICATION INTERFACE EVENT INPUT OUTPUT QUEUE 
%token PUBLIC PRIVATE PERSISTENT PROTECTED
%token CHAR BYTE INT SIGNED UNSIGNED FLOAT BOOL STRING CONST ENUM TIME
%token WHEN SCRIPT RANGE STEP
%token CASE DEFAULT IF ELSE SWITCH FOREACH IN CONTINUE BREAK RETURN
%token SUPRESS_OP TYPEDEF

%type <type> primitive_type event_type
%type <ival> unary_operator assignment_operator
%type <ival> storage storage_list visibility
%type <ival> integer
%type <sval> type_context_name
%type <expr> fid identifier based_identifier context_identifier 
%type <expr> range_expr 
%type <expr> primary_expr postfix_expr 
%type <expr> unary_expr multiplicative_expr additive_expr
%type <expr> shift_expr relational_expr equality_expr 
%type <expr> and_expr exclusive_or_expr inclusive_or_expr
%type <expr> logical_and_expr logical_or_expr conditional_expr
%type <expr> assignment_expr object_expr expr constant_expr
%type <expr> constructor object_constructor array_constructor
%type <expr> finitializer 
%type <expr_list> initializer_list finitializer_list
%type <expr_list> argument_expr_list
%type <stmt> statement 
%type <stmt_list> statement_list
%type <stmt> labeled_statement compound_statement
%type <stmt> expression_statement selection_statement
%type <stmt> iteration_statement jump_statement

%type <enm> enumerator
%type <enm_list> enumerator_list 
%type <decl_list> declaration_list
%type <decl>     declaration
%type <decr>     declarator
%type <spec>     typeid parentid decl_specifier enum_specifier
%type <spec>     type_specifier type_context type_declarator
%type <odecl>    file library application toplevel
%type <scr>      script
%type <scr_list> script_list
%type <scr_body> script_body
%type <ver>      version

%destructor { $$->releaseThis(); } type_context_name
%destructor { $$->releaseThis(); } fid identifier based_identifier
%destructor { $$->releaseThis(); } context_identifier 
%destructor { $$->releaseThis(); } range_expr 
%destructor { $$->releaseThis(); } primary_expr postfix_expr 
%destructor { $$->releaseThis(); } unary_expr multiplicative_expr additive_expr
%destructor { $$->releaseThis(); } shift_expr relational_expr equality_expr 
%destructor { $$->releaseThis(); } and_expr exclusive_or_expr inclusive_or_expr
%destructor { $$->releaseThis(); } logical_and_expr logical_or_expr 
%destructor { $$->releaseThis(); } conditional_expr
%destructor { $$->releaseThis(); } assignment_expr object_expr expr 
%destructor { $$->releaseThis(); } constant_expr
%destructor { $$->releaseThis(); } constructor object_constructor 
%destructor { $$->releaseThis(); } array_constructor
%destructor { $$->releaseThis(); } finitializer
%destructor { $$->releaseThis(); } initializer_list finitializer_list
%destructor { $$->releaseThis(); } argument_expr_list
%destructor { $$->releaseThis(); } statement
%destructor { $$->releaseThis(); } statement_list
%destructor { $$->releaseThis(); } labeled_statement compound_statement 
%destructor { $$->releaseThis(); } expression_statement selection_statement
%destructor { $$->releaseThis(); } iteration_statement jump_statement
%destructor { $$->releaseThis(); } enumerator
%destructor { $$->releaseThis(); } enumerator_list 
%destructor { $$->releaseThis(); } declaration_list
%destructor { $$->releaseThis(); } declaration
%destructor { $$->releaseThis(); } declarator
%destructor { m1Release(M1Specifier,$$); } typeid parentid decl_specifier
%destructor { m1Release(M1Specifier,$$); } enum_specifier
%destructor { $$->releaseThis(); } type_specifier type_context type_declarator
%destructor { $$->releaseThis(); } file library application toplevel
%destructor { $$->releaseThis(); } script
%destructor { $$->releaseThis(); } script_list
%destructor { $$->releaseThis(); } version

%{
int m1lex(m1::m1Parser::semantic_type* yylval, m1::location* yylloc, M1Lex* aLex);

// m1New macro with location and retain, only useful with M1Element
static M1Element* setElementRetain(M1Element* aElem, m1::location& yylloc)
{
    aElem->setFile(yylloc.begin.filename->c_str());
    aElem->setLine(yylloc.begin.line);
    aElem->retainThis();
    return aElem;
}

// m1New macro with location and retain, only useful with M1Element
static M1Element* setElement(M1Element* aElem, m1::location& yylloc)
{
    aElem->setFile(yylloc.begin.filename->c_str());
    aElem->setLine(yylloc.begin.line);
    return aElem;
}

static CObject* doRetain(CObject* aObj)
{
    aObj->retainThis();
    return aObj;
}

// allocate copy element info and retain		   
#define m1cNew(loc, type, ...)				\
    ((type*) setElementRetain(m1New(type, __VA_ARGS__), (loc)))

// allocate copy element info
#define m1eNew(loc, type, ...)				\
    ((type*) setElement(m1New(type, __VA_ARGS__), (loc)))

// allocate and retain
#define m1rNew(type, ...)			\
    ((type*) doRetain(m1New(type, __VA_ARGS__)))
    

%}

%start file

%%

identifier:
  M1IDENTIFIER     { $$=m1cNew(@$,M1Identifier, mLex->tokenText()); }
;

integer:
  M1INTEGER        { $$=atoi(mLex->tokenText()); }
;

//
// version specifier 
//
version:
/* empty */ {
    $$ = m1cNew(@$,M1Version, 0, 0, 0); 
   }
| '(' integer ')' { 
    $$ = m1cNew(@$,M1Version, $2, 0, 0); 
  }
| '(' integer ',' integer ')' { 
    $$ = m1cNew(@$,M1Version, $2, $4, 0); 
  }
| '(' integer ',' integer ',' integer ')' { 
    $$ = m1cNew(@$,M1Version, $2, $4, $6); 
  }
;

based_identifier:
  identifier  { $$=$1; }
| '@' M1IDENTIFIER { 
    $$=m1cNew(@$,M1BasedIdentifier, NULL, mLex->tokenText()); 
  }
| '@' type_context '.' M1IDENTIFIER { 
    $$=m1cNew(@$,M1BasedIdentifier,(M1TypeIdentifier*)$2, mLex->tokenText());
    $2->releaseThis();
  }
;

context_identifier:
'.' M1TIDENTIFIER { $$=m1cNew(@$,M1Identifier, "."+string(mLex->tokenText())); }
;

primary_expr:
  based_identifier    { $$=$1; }
| context_identifier  { $$=$1; }
| M1CHAR         { $$=m1cNew(@$,M1CharConstant, mLex->tokenText()); }
| M1INTEGER      { $$=m1cNew(@$,M1UnsignedConstant, mLex->tokenText()); }
| M1FLOAT        { $$=m1cNew(@$,M1FloatConstant, mLex->tokenText()); }
| M1TRUE         { $$=m1cNew(@$,M1BoolConstant, true); }
| M1FALSE        { $$=m1cNew(@$,M1BoolConstant, false); }
| M1STRING       { $$=m1cNew(@$,M1StringConstant, mLex->unquote(mLex->tokenText())); }
| M1NIL          { $$=m1cNew(@$,M1Nil); }
| M1THIS         { $$=m1cNew(@$,M1This); }
| '@' object_constructor { $$ = $2; }
| '@' array_constructor  { $$ = $2; }
| '@' type_context '(' expr ')' { 
    $$=m1cNew(@$,M1CastExpr,(M1TypeIdentifier*)$2,$4);
    $2->releaseThis();
    $4->releaseThis();
  }
| '@' primitive_type '(' expr ')' { 
    $$=m1cNew(@$,M1CastExpr,m1eNew(@$,M1TypeIdentifier,$2->name()),$4); 
    $4->releaseThis();
  }
| '&' M1IDENTIFIER { $$=m1cNew(@$,M1Index, NULL, mLex->tokenText()); }
| '&' type_context '.' M1IDENTIFIER { 
    $$=m1cNew(@$,M1Index,(M1TypeIdentifier*)$2, mLex->tokenText()); 
    $2->releaseThis();
  }
| '(' expr ')'   { $$=$2; }
;

postfix_expr:
  primary_expr                           { $$=$1; }
| postfix_expr '[' expr ']'  {
    $$=m1cNew(@$,M1BinaryExpr,op_ELEM,$1,$3);
    $1->releaseThis();
    $3->releaseThis();
  }
| postfix_expr '[' expr ':' expr ']'  {
    $$=m1cNew(@$,M1BinaryExpr,op_ELEMS,$1,
	      m1eNew(@$,M1TrinaryExpr,op_RANGE,$3,$5,NULL)); 
    $1->releaseThis();
    $3->releaseThis();
    $5->releaseThis();
  }
| postfix_expr '[' expr ':' expr ':' expr ']' { 
    $$=m1cNew(@$,M1BinaryExpr,op_ELEMS,$1,
	      m1eNew(@$,M1TrinaryExpr,op_RANGE,$3,$5,$7)); 
    $1->releaseThis();
    $3->releaseThis();
    $5->releaseThis();
    $7->releaseThis();
  }
| postfix_expr '(' ')' { 
    $$=m1cNew(@$,M1CallExpr,$1,m1eNew(@$,M1ExprList));
    $1->releaseThis();
  }
| postfix_expr '(' argument_expr_list ')'{ 
    $$=m1cNew(@$,M1CallExpr,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| postfix_expr '.' identifier { 
    $$=m1cNew(@$,M1BinaryExpr,op_FLD,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| postfix_expr INC_OP { 
    $$=m1cNew(@$,M1UnaryExpr,op_PSTINC,$1); 
    $1->releaseThis();
  }
| postfix_expr DEC_OP {
    $$=m1cNew(@$,M1UnaryExpr,op_PSTDEC,$1); 
    $1->releaseThis();
  }
;

argument_expr_list:
  assignment_expr {
      $$ = m1cNew(@$,M1ExprList); $$->appendElement($1); 
      $1->releaseThis();
  }
| argument_expr_list ',' assignment_expr { 
    $1->appendElement($3); $$=$1;
    $3->releaseThis();
  }
;

unary_expr:
  postfix_expr              { $$ = $1; }
| unary_operator unary_expr { 
    $$ = m1cNew(@$,M1UnaryExpr, $1, $2);
    $2->releaseThis();    
  }
;

unary_operator
: '+'  { $$ = op_POS;     }
| '-'  { $$ = op_NEG;     }
| '~'  { $$ = op_BNOT;    }
| '!'  { $$ = op_NOT;     }
| '*'  { $$ = op_TRIGGER; }
| SUPRESS_OP { $$ = op_SUPRESS; }
| INC_OP { $$ = op_PREINC; }
| DEC_OP { $$ = op_PREDEC; }
;

multiplicative_expr:
  unary_expr                         { $$ = $1; }
| multiplicative_expr '*' unary_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_MUL,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| multiplicative_expr '/' unary_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_DIV,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| multiplicative_expr '%' unary_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_REM,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

additive_expr:
  multiplicative_expr                   { $$ = $1; }
| additive_expr '+' multiplicative_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_ADD,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| additive_expr '-' multiplicative_expr {
    $$=m1cNew(@$,M1BinaryExpr,op_SUB,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }

;

shift_expr:
  additive_expr                     { $$ = $1; }
| shift_expr SHIFTL_OP additive_expr  { 
    $$=m1cNew(@$,M1BinaryExpr,op_BSL,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| shift_expr SHIFTR_OP additive_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_BSR,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }

;

relational_expr:
  shift_expr                       { $$ = $1; }
| relational_expr '<' shift_expr   { 
    $$=m1cNew(@$,M1BinaryExpr,op_LT,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| relational_expr '>' shift_expr   { 
    $$=m1cNew(@$,M1BinaryExpr,op_GT,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| relational_expr LE_OP shift_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_LTE,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| relational_expr GE_OP shift_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_GTE,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

equality_expr:
  relational_expr                     { $$ = $1; }
| equality_expr EQ_OP relational_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_EQ,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| equality_expr NE_OP relational_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_NEQ,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| equality_expr EQL_OP relational_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_EQL,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| equality_expr NQL_OP relational_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_NQL,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

and_expr:
  equality_expr              { $$ = $1; }
| and_expr '&' equality_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_BAND,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

exclusive_or_expr:
  and_expr                       { $$ = $1; }
| exclusive_or_expr '^' and_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_BXOR,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

inclusive_or_expr:
  exclusive_or_expr                       { $$ = $1; }
| inclusive_or_expr '|' exclusive_or_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_BOR,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

logical_and_expr:
  inclusive_or_expr                         { $$ = $1; }
| logical_and_expr AND_OP inclusive_or_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_AND,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

logical_or_expr:
  logical_and_expr                       { $$ = $1; }
| logical_or_expr OR_OP logical_and_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_OR,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

conditional_expr:
  logical_or_expr   { $$ = $1; }
| logical_or_expr '?' logical_or_expr ':' conditional_expr {
    $$ = m1cNew(@$,M1TrinaryExpr,op_COND,$1,$3,$5); 
    $1->releaseThis();
    $3->releaseThis();
    $5->releaseThis();
  }
;

type_context_name:
  M1TIDENTIFIER         { $$=m1rNew(CString, mLex->tokenText()); }
  | ':' M1TIDENTIFIER   { $$=m1rNew(CString, ":"+string(mLex->tokenText())); }
  | type_context_name ':' M1TIDENTIFIER {
      $$ = m1rNew(CString, $1->str() + ":" + string(mLex->tokenText())); 
      $1->releaseThis(); 
    }
;

type_context:
  type_context_name   { 
      $$=m1cNew(@$,M1TypeIdentifier,$1->str()); 
      $1->releaseThis();
  }
;

object_constructor:
   type_context '{' '}'{ 
       $$=m1cNew(@$,M1ObjectConstant, (M1TypeIdentifier*)$1, 
		 m1eNew(@$,M1ExprList)); 
   }
|  type_context '{' finitializer_list '}' {
    $$=m1cNew(@$,M1ObjectConstant, (M1TypeIdentifier*)$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
   }
;

array_constructor:
  '{' '}'  { $$=m1cNew(@$,M1ArrayConstant, m1eNew(@$,M1ExprList)); }
| '{' initializer_list '}'  { 
    $$=m1cNew(@$,M1ArrayConstant, $2); 
    $2->releaseThis();
  }
;

constructor:
  object_constructor { $$ = $1; }
| array_constructor { $$ = $1; }
;

object_expr:
  conditional_expr  { $$ = $1; }
| constructor       { $$ = $1; }
;

assignment_expr:
  conditional_expr  { $$ = $1; }
| unary_expr assignment_operator assignment_expr { 
    $$=m1cNew(@$,M1BinaryExpr,$2,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
| unary_expr assignment_operator constructor {
    $$=m1cNew(@$,M1BinaryExpr,$2,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

assignment_operator:
'='            { $$=op_PUT; }
| COPY_ASSIGN   { $$=op_COPY; }
| MUL_ASSIGN   { $$=op_MULPUT; }
| DIV_ASSIGN   { $$=op_DIVPUT; }
| REM_ASSIGN   { $$=op_REMPUT; }
| ADD_ASSIGN   { $$=op_ADDPUT; }
| SUB_ASSIGN   { $$=op_SUBPUT; }
| SHIFTL_ASSIGN { $$=op_BSLPUT; }
| SHIFTR_ASSIGN { $$=op_BSRPUT; }
| AND_ASSIGN   { $$=op_BANDPUT; }
| XOR_ASSIGN   { $$=op_BXORPUT; }
| OR_ASSIGN    { $$=op_BORPUT; }
| CONNECT_OP   { $$=op_CNCT; }
;

expr:
  assignment_expr          { $$=$1; }
| expr ',' assignment_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_SEQ,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

constant_expr:
  conditional_expr { $$=$1; }
;

visibility:
/* empty */ { $$ = Q_NONE; }
| PUBLIC    { $$ = Q_PUBLIC;  }
| PRIVATE   { $$ = Q_PRIVATE; }
| PROTECTED { $$ = Q_PROTECTED; }
;

storage:
  CONST      { $$ = Q_CONST; }
| PERSISTENT { $$ = Q_PERSISTENT; }
;


storage_list:
/* empty */  { $$ = 0; }
| storage_list storage { $$ = $1 | $2; }
;


typeid:
M1TIDENTIFIER { $$=m1cNew(@$,M1TypeIdentifier,mLex->tokenText()); }
;

parentid:
/* empty */ { $$ = NULL; }
| ':' type_context { $$=$2; }
;


primitive_type:
  SIGNED CHAR   { $$=char_type(); }
| CHAR          { $$=char_type(); }
| UNSIGNED CHAR { $$=byte_type(); }
| BYTE          { $$=byte_type(); }
| INT           { $$=signed_type(); }
| SIGNED        { $$=signed_type(); }
| SIGNED INT    { $$=signed_type(); }
| UNSIGNED INT  { $$=unsigned_type(); }
| UNSIGNED      { $$=unsigned_type(); }
| FLOAT         { $$=float_type(); }
| BOOL          { $$=bool_type(); }
| STRING        { $$=string_type(); }
| TIME          { $$=time_type(); }
;

event_type:
  primitive_type { $$=$1; }
| INPUT EVENT primitive_type       { $$=CEventType::create($3,E_INPUT); }
| OUTPUT EVENT primitive_type      { $$=CEventType::create($3,E_OUTPUT); }
| EVENT primitive_type             { $$=CEventType::create($2,E_INOUT); }
| INPUT EVENT QUEUE primitive_type { $$=CEventType::createQueue($4,E_INPUT); }
| EVENT QUEUE primitive_type       { $$=CEventType::createQueue($3,E_INOUT); }
;

enum_specifier:
  ENUM typeid '{' enumerator_list '}' {
    $$=m1cNew(@$,M1EnumSpecifier,(M1TypeIdentifier*)$2, $4); 
    $2->releaseThis();
    $4->releaseThis();
  }
| ENUM '{' enumerator_list '}' {
      $$=m1cNew(@$,M1EnumSpecifier,NULL, $3);
      $3->releaseThis();
  }
;


enumerator_list:
  enumerator {
      $$=m1cNew(@$,M1EnumList); 
      $$->appendElement($1);
      $1->releaseThis();
  }
| enumerator_list ',' enumerator { 
    $1->appendElement($3); 
    $$=$1; 
    $3->releaseThis();
  }
;

enumerator:
  identifier { 
      $$=m1cNew(@$,M1Enum,((M1Identifier*)$1)->name(), NULL); 
      $1->releaseThis();
  }
| identifier '=' constant_expr {
    $$=m1cNew(@$,M1Enum,((M1Identifier*)$1)->name(),$3);
    $1->releaseThis();
    $3->releaseThis();
  }
;

type_specifier:
  event_type           { $$=m1cNew(@$,M1TypePrimitive,$1); }
| type_context         { $$ = $1; }
| INPUT EVENT type_context_name {
    $$ = m1cNew(@$,M1EventTypeIdentifier,$3->str(),false,E_INPUT);
    $3->releaseThis();
  }
| OUTPUT EVENT type_context_name {
    $$ = m1cNew(@$,M1EventTypeIdentifier,$3->str(),false,E_OUTPUT); 
    $3->releaseThis();
  }
| EVENT type_context_name { 
    $$ = m1cNew(@$,M1EventTypeIdentifier,$2->str(),false,E_INOUT); 
    $2->releaseThis();
  }
| INPUT EVENT QUEUE type_context_name { 
    $$ = m1cNew(@$,M1EventTypeIdentifier,$4->str(),true,E_INPUT);
    $4->releaseThis();
  }
| EVENT QUEUE type_context_name {
    $$ = m1cNew(@$,M1EventTypeIdentifier,$3->str(),true,E_INOUT); 
    $3->releaseThis();
  }
;


decl_specifier:
  TYPE typeid parentid '{' declaration_list script_list statement_list '}' {
      $$=m1cNew(@$,M1TypeSpecifier,(M1TypeIdentifier*)$2,
		(M1TypeIdentifier*)$3,$5,$6,$7,NULL,T_TYPE); 
      $2->releaseThis();
      if ($3) $3->releaseThis();
      $5->releaseThis();
      $6->releaseThis();
      $7->releaseThis();
  }
| TYPE typeid parentid '{' declaration_list script_list  '}' {
    $$=m1cNew(@$,M1TypeSpecifier,(M1TypeIdentifier*)$2,(M1TypeIdentifier*)$3,
	      $5,$6,m1eNew(@$,M1StatementList),NULL,T_TYPE); 
    $2->releaseThis();
    if ($3) $3->releaseThis();
    $5->releaseThis();
    $6->releaseThis();
  }
| TYPEDEF type_declarator typeid {
    $$=m1cNew(@$,M1TypeDefSpecifier,(M1TypeIdentifier*)$3, $2);
    $2->releaseThis();
    $3->releaseThis();
  }
| INTERFACE TYPE typeid parentid '{' declaration_list '}' { 
    $$=m1cNew(@$,M1InterfaceSpecifier,(M1TypeIdentifier*)$3,
	      (M1TypeIdentifier*)$4, $6, NULL, T_TYPE); 
    $3->releaseThis();
    if ($4) $4->releaseThis();
    $6->releaseThis();
  }
| INTERFACE LIBRARY typeid version '{' declaration_list  '}'  {
    $$=m1cNew(@$,M1InterfaceSpecifier,(M1TypeIdentifier*)$3,NULL,$6,$4,
	      T_LIBRARY);
    $3->releaseThis();
    $4->releaseThis();    
    $6->releaseThis();    
  }
| INTERFACE APPLICATION typeid version '{' declaration_list  '}'  {
    $$=m1cNew(@$,M1InterfaceSpecifier,(M1TypeIdentifier*)$3,NULL,$6,$4,
	      T_APPLICATION);
    $3->releaseThis();
    $4->releaseThis();    
    $6->releaseThis();    
  }
;


declaration:
  visibility storage_list type_specifier declarator {
      $$=m1cNew(@$,M1FieldDeclaration,$1|$2,$3,$4,0,NULL); 
      $3->releaseThis();
      $4->releaseThis();
  }
| visibility  storage_list type_specifier declarator '=' object_expr {
    $$=m1cNew(@$,M1FieldDeclaration,$1|$2,$3,$4,op_PUT,$6); 
    $3->releaseThis();
    $4->releaseThis();
    $6->releaseThis();
  }
| visibility  storage_list type_specifier declarator CONNECT_OP object_expr {
    $$=m1cNew(@$,M1FieldDeclaration,$1|$2,$3,$4,op_CNCT,$6); 
    $3->releaseThis();
    $4->releaseThis();
    $6->releaseThis();
  }
| visibility storage_list enum_specifier {
    $$=m1cNew(@$,M1DeclSpecifier,$1,$3); 
    $3->releaseThis();
  }
| visibility storage_list enum_specifier declarator {
      $$=m1cNew(@$,M1FieldDeclaration,$1|$2,$3,$4,0,NULL); 
      $3->releaseThis();
      $4->releaseThis();
  }
| visibility storage_list enum_specifier declarator '=' object_expr {
    $$=m1cNew(@$,M1FieldDeclaration,$1|$2,$3,$4,op_PUT,$6); 
    $3->releaseThis();
    $4->releaseThis();
    $6->releaseThis();
  }
| visibility decl_specifier {
    $$=m1cNew(@$,M1DeclSpecifier,$1,$2); 
    $2->releaseThis();
  }
;

declaration_list:
  { $$ = m1cNew(@$,M1DeclarationList); }
| declaration_list declaration ';' { 
    $1->appendElement($2); $$=$1; 
    $2->releaseThis();
  }
;

type_declarator:
  type_specifier                { $$=$1; }
| '(' type_declarator ')'       { $$=$2; }
| type_declarator '[' ']'       { 
    $$=m1cNew(@$,M1ArrayTypeDeclarator,$1,NULL);
    $1->releaseThis();
  }
| type_declarator '[' constant_expr ']' { 
    $$=m1cNew(@$,M1ArrayTypeDeclarator,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

declarator:
  M1IDENTIFIER { 
      $$=m1cNew(@$,M1IdDeclarator,mLex->tokenText()); 
  }
| '@' type_context '.' M1IDENTIFIER { 
    $$=m1cNew(@$,M1BaseIdDeclarator,(M1TypeIdentifier*)$2,mLex->tokenText()); 
    $2->releaseThis();
  }
| '(' declarator ')'               { $$=$2; }
| declarator '[' ']'               {
    if (isAType(M1ArrayDeclarator*, $1)) {
	((M1ArrayDeclarator*)$1)->addDimension(m1eNew(@$,M1Nil));
	$$ = $1;
    }
    else {
	$$=m1cNew(@$,M1ArrayDeclarator,$1,m1eNew(@$,M1ExprList));
	((M1ArrayDeclarator*)$$)->addDimension(m1eNew(@$,M1Nil));
	$1->releaseThis();
    }
  }
| declarator '[' constant_expr ']' { 
    if (isAType(M1ArrayDeclarator*, $1)) {
	((M1ArrayDeclarator*)$1)->addDimension($3);
	$3->releaseThis();
	$$ = $1;
    }
    else {
	$$=m1cNew(@$,M1ArrayDeclarator,$1,m1eNew(@$,M1ExprList));
	((M1ArrayDeclarator*)$$)->addDimension($3);
	$1->releaseThis();
	$3->releaseThis();
    }
  }
;

initializer_list:
  object_expr { 
      $$=m1cNew(@$,M1ExprList); $$->appendElement($1); 
      $1->releaseThis();      
  }
| initializer_list ',' object_expr { 
    $1->appendElement($3); $$=$1; 
    $3->releaseThis();      
  }
;

fid:
   based_identifier  { $$=$1; }
|  fid '[' expr ']'  { 
    $$=m1cNew(@$,M1BinaryExpr, op_ELEM, $1, $3);
    $1->releaseThis();
    $3->releaseThis();
   }
;

finitializer:
  fid '=' object_expr { 
      $$=m1cNew(@$,M1BinaryExpr,op_PUT, $1, $3); 
      $1->releaseThis();
      $3->releaseThis();
  }
| fid CONNECT_OP conditional_expr { 
    $$=m1cNew(@$,M1BinaryExpr,op_CNCT,$1,$3); 
    $1->releaseThis();
    $3->releaseThis();
  }
;

finitializer_list:
  finitializer { 
      $$ = $$=m1cNew(@$,M1ExprList); 
      $$->appendElement($1); 
      $1->releaseThis();
  }
| finitializer_list ',' finitializer { 
    $1->appendElement($3); $$=$1;  
    $3->releaseThis();
  }
;

range_expr:
  '[' expr ']' { 
    $$ = m1cNew(@$,M1TrinaryExpr,op_RANGE,$2,NULL,NULL); 
    $2->releaseThis();
  }
| '[' expr ':' expr ']' { 
    $$ = m1cNew(@$,M1TrinaryExpr,op_RANGE,$2,$4,NULL); 
    $2->releaseThis();
    $4->releaseThis();
  }
| '[' expr ':' expr ':' expr ']' { 
    $$ = m1cNew(@$,M1TrinaryExpr,op_RANGE,$2,$4,$6); 
    $2->releaseThis();
    $4->releaseThis();
    $6->releaseThis();
  }
;


statement:
labeled_statement      { $$=$1; }
| compound_statement   { $$=$1; }
| expression_statement { $$=$1; }
| selection_statement  { $$=$1; }
| iteration_statement  { $$=$1; }
| jump_statement       { $$=$1; }
;

labeled_statement:
  CASE constant_expr ':' statement  { 
      $$=m1cNew(@$,M1CaseStatement,$2,$4); 
      $2->releaseThis();
      $4->releaseThis();
  }
| DEFAULT ':' statement { 
    $$=m1cNew(@$,M1DefaultStatement,$3); 
    $3->releaseThis();
  }
;

compound_statement
: '{' '}'  { 
    $$=m1cNew(@$,M1CompoundStatement,
	      m1eNew(@$,M1DeclarationList),
	      m1eNew(@$,M1StatementList)); 
  }
| '{' declaration_list statement_list '}' { 
    $$=m1cNew(@$,M1CompoundStatement, $2, $3);
    $2->releaseThis();
    $3->releaseThis();
  }
;

statement_list
: statement { 
    $$=m1cNew(@$,M1StatementList); 
    $$->appendElement($1);
    $1->releaseThis();
  }
| statement_list statement { 
    $1->appendElement($2); $$=$1; 
    $2->releaseThis();
  }
;

expression_statement:
  ';' {
      $$ = m1cNew(@$,M1ExprStatement,NULL); 
  }
| expr ';' { 
    $$ = m1cNew(@$,M1ExprStatement,$1); 
    $1->releaseThis();    
  }
;

selection_statement:
  IF '(' expr ')' statement { 
      $$=m1cNew(@$,M1IfStatement,$3,$5,NULL); 
      $3->releaseThis();
      $5->releaseThis();
  }
| IF '(' expr ')' statement ELSE statement { 
    $$=m1cNew(@$,M1IfStatement,$3,$5,$7); 
    $3->releaseThis();
    $5->releaseThis();
    $7->releaseThis();
  }
| SWITCH '(' expr ')' '{' '}' { 
    $$=m1cNew(@$,M1SwitchStatement,$3,NULL,NULL); 
    $3->releaseThis();
  }
| SWITCH '(' expr ')' '{' declaration_list statement_list '}' { 
    $$=m1cNew(@$,M1SwitchStatement,$3,$6,$7); 
    $3->releaseThis();
    $6->releaseThis();
    $7->releaseThis();
  }
;

iteration_statement:
  FOREACH identifier IN assignment_expr compound_statement { 
      $$=m1cNew(@$,M1ForEachStatement,$2,$4,$5); 
      $2->releaseThis();
      $4->releaseThis();
      $5->releaseThis();
  }
| FOREACH identifier IN range_expr compound_statement { 
    $$=m1cNew(@$,M1ForEachStatement,$2,$4,$5); 
    $2->releaseThis();
    $4->releaseThis();
    $5->releaseThis();
  }
;


jump_statement:
  CONTINUE ';'  {
      $$=m1cNew(@$,M1JumpStatement,m1::m1Parser::token::CONTINUE); 
  }
| BREAK ';' { 
    $$=m1cNew(@$,M1JumpStatement,m1::m1Parser::token::BREAK); 
  }
| RETURN ';' { 
    $$=m1cNew(@$,M1JumpStatement,m1::m1Parser::token::RETURN); 
  }
| RETURN expr ';' { 
    $$=m1cNew(@$,M1JumpStatement,m1::m1Parser::token::RETURN,$2); 
  }
;

script_list:
/* empty */ { 
    $$=m1cNew(@$,M1ScriptList); 
  }
| script_list script { 
    $1->appendElement($2); 
    $$=$1;
    $2->releaseThis();
  }
;

script:
  '+' SCRIPT expr script_body { 
      $$=m1cNew(@$,M1Script, "+", $3, $4.when, $4.body);
      $3->releaseThis();
      if ($4.when) $4.when->releaseThis();
      $4.body->releaseThis();
  }
| '+' SCRIPT script_body { 
    $$=m1cNew(@$,M1Script, "+", NULL, $3.when, $3.body); 
    if ($3.when) $3.when->releaseThis();
    $3.body->releaseThis();
  }
| '-' SCRIPT expr script_body { 
    $$=m1cNew(@$,M1Script, "-", $3, $4.when, $4.body); 
    $3->releaseThis();
    if ($4.when) $4.when->releaseThis();
    $4.body->releaseThis();    
  }
| '-' SCRIPT script_body { 
    $$=m1cNew(@$,M1Script, "-", NULL, $3.when, $3.body); 
    if ($3.when) $3.when->releaseThis();
    $3.body->releaseThis();
  }
| SCRIPT expr script_body { 
    $$=m1cNew(@$,M1Script, "+", $2, $3.when, $3.body); 
    $2->releaseThis();
    if ($3.when) $3.when->releaseThis();
    $3.body->releaseThis();
  }
| SCRIPT script_body { 
    $$=m1cNew(@$,M1Script, "+", NULL, $2.when, $2.body); 
    if ($2.when) $2.when->releaseThis();
    $2.body->releaseThis();    
  }
| SCRIPT typeid compound_statement { 
    $$=m1cNew(@$,M1Script, $2->name(), NULL, NULL, $3); 
    $2->releaseThis();
    $3->releaseThis();
  }
| SCRIPT '~' typeid compound_statement { 
    $$=m1cNew(@$,M1Script, "~"+$3->name(), NULL, NULL, $4); 
    $3->releaseThis();
    $4->releaseThis();
  }
;

script_body:
  WHEN expr compound_statement { 
      $$.when = $2; 
      $$.body = $3; 
  }
| compound_statement { 
    $$.when = NULL; 
    $$.body = $1; 
  }
;


library:
  LIBRARY typeid version '{' declaration_list script_list '}'  { 
      $$=m1cNew(@$,M1TypeSpecifier,(M1TypeIdentifier*)$2,NULL,$5,$6,
		m1eNew(@$,M1StatementList),$3,T_LIBRARY); 
      $2->releaseThis();
      $3->releaseThis();
      $5->releaseThis();
      $6->releaseThis();
  }
| LIBRARY typeid version '{' declaration_list script_list statement_list '}' {
    $$=m1cNew(@$,M1TypeSpecifier,(M1TypeIdentifier*)$2,NULL,$5,$6,$7,$3,
	      T_LIBRARY);
    $2->releaseThis();
    $3->releaseThis();
    $5->releaseThis();
    $6->releaseThis();
    $7->releaseThis();
  }
;

application:
  APPLICATION typeid version '{' declaration_list script_list '}'  { 
      $$=m1cNew(@$,M1TypeSpecifier,(M1TypeIdentifier*)$2,NULL,$5,$6,
		m1eNew(@$,M1StatementList),$3,T_APPLICATION); 
      $2->releaseThis();
      $3->releaseThis();
      $5->releaseThis();
      $6->releaseThis();
  }
| APPLICATION typeid version '{' declaration_list script_list statement_list '}' {
    $$=m1cNew(@$,M1TypeSpecifier,(M1TypeIdentifier*)$2,NULL,$5,$6,$7,$3,
	      T_APPLICATION);
    $2->releaseThis();
    $3->releaseThis();
    $5->releaseThis();
    $6->releaseThis();
    $7->releaseThis();
  }
;

toplevel:
   declaration_list script_list { 
       $$ = m1cNew(@$,M1TypeSpecifier,m1eNew(@$,M1TypeIdentifier,"_FileScope"),
		   NULL,$1,$2,m1eNew(@$,M1StatementList),NULL,T_TOPLEVEL); 
       $1->releaseThis();
       $2->releaseThis();
   }
 | declaration_list script_list statement_list { 
     $$ = m1cNew(@$,M1TypeSpecifier,m1eNew(@$,M1TypeIdentifier,"_FileScope"),
		 NULL,$1, $2, $3, NULL, T_TOPLEVEL); 
     $1->releaseThis();
     $2->releaseThis();
     $3->releaseThis();
   }
;

/*****  TOP LEVEL WITH ONLY COMPOUND STATEMENT
toplevel:
 declaration_list statement_list {
     M1CompoundStatement* stmt = m1eNew(@$,M1CompoundStatement, $1, $2);
     M1StatementList* slist    = m1eNew(@$,M1StatementList);
     $1->releaseThis();
     $2->releaseThis();
     slist->appendElement(stmt);
     $$ = m1cNew(@$,M1TypeSpecifier,
		 m1eNew(@$,M1TypeIdentifier,"_FileScope"),
		 NULL, m1eNew(@$,M1DeclarationList), m1eNew(@$,M1ScriptList),
		 slist, NULL, T_TOPLEVEL); 
   }
*****/

file:
   library { $$ = $1; }
|  application { $$ = $1; }
|  toplevel { $$ = $1; }
;

%%

//
// Error printing
//
void m1::m1Parser::error(const m1::m1Parser::location_type& l, const std::string& m)
{
    std::cerr << l << ": " << m << std::endl;
}

//
// For none push parser we use this one
//
int m1lex(m1::m1Parser::semantic_type* yylval,m1::location* yylloc,M1Lex* aLex)
{
    int token;

    switch((token = aLex->getToken())) {
    case LEX_EOF: return 0;
    case LEX_MORE: return -2;
    case LEX_ERROR: return -1;
    default:
	yylval->ival = token;
	return token;
    }
}
