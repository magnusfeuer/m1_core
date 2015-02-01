//
// Implementation class of m1_PushParser
//

#include <stdio.h>

#include "m1_parser.hh"

M1Parser::M1Parser(M1Lex* aLex) : m1::m1Parser(aLex)
{
    value = NULL;
}

void M1Parser::yyerror(const char* s)
{
    std::cerr << mLex->location() << ": " << s << std::endl;
}

void M1Parser::accept()
{
    // std::cout << "\taccept!" << std::endl;
    value = top().odecl;
}

void M1Parser::error()
{
    // std::cout << "\terror!" << std::endl;
    value = NULL;
}

void M1Parser::error(const location_type& loc, const std::string& msg)
{
    std::cerr << loc << ":" << msg << std::endl;
}
// 
// Parse
//  return 0 and spec set if ok
//         1 parsing failed
//        -2 when more input is wanted (poll lex descriptor)
//        -1 io/lex error
//
int M1Parser::parse(M1TypeSpecifier** spec)
{
    M1Parser::semantic_type val;

    while(1) {
	int token = mLex->getToken();
	int res;

	if (token == LEX_EOF) {
	    val.ival = EOF;
	    res = parse_token(0, val, mLex->location());
	    *spec = value;
	    return res;
	}
	if (token == LEX_MORE)
	    return -2;
	if (token == LEX_ERROR)
	    return -1;
	val.ival = token;
	if ((res = parse_token(token, val, mLex->location())) != 3)
	    return res;
    }
}
