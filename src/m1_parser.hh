//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#ifndef __M1_PARSER_HH__
#define __M1_PARSER_HH__

#include "m1_lex.hh"
#include "m1_parse.hh"

class M1Parser : public m1::m1Parser
{
public:
    M1Parser(M1Lex* aLex);

    void yyerror(const char* s);
    void accept();
    void error();
    void error (const location_type& loc, const std::string& msg);

    int parse(M1TypeSpecifier** spec);
private:
    M1TypeSpecifier* value;
};

#endif
