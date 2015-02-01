m4_divert(-1)
# C++ push skeleton version 0.1.0 for LALR(1) parsing with Bison
# Copyright (C) 2002 Free Software Foundation, Inc.
# Copyright (C) 2004 Brueckner & Jarosch Ing. GmbH
# Copyright (C) 2004 Sven Schaepe <schaepebj-ig.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
# 02111-1307  USA
m4_include(b4_pkgdatadir/[c++.m4])

# The header is mandatory.
b4_defines_if([],
              [m4_fatal(b4_skeleton[: using %defines is mandatory])])

# Backward compatibility.
m4_define([b4_location_constructors])
m4_include(b4_pkgdatadir/[location.cc])


m4_define([b4_bjcopyright],
[/* A Bison parser, made by GNU Bison b4_version.  */

/* C++ push skeleton version 0.1.0 for LALR(1) parsing with Bison

   Copyright (C) $1 Brueckner & Jarosch Ing. GmbH
   Copyright (C) $1 Sven Schaepe <schaepe@@bj-ig.de>

   $2

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  
*/])


## ---------------- ##
## Default values.  ##
## ---------------- ##

# Stack parameters.
m4_define_default([b4_stack_depth_init],  [200])

# Default Parser class name.
m4_define([b4_parser_class_name],[m4_if(b4_prefix[],[],[yyPushParser],b4_prefix[]PushParser)])

## ----------------- ##
## Semantic Values.  ##
## ----------------- ##


# b4_lhs_value([TYPE])
# --------------------
# Expansion of $<TYPE>$.
m4_define([b4_lhs_value],
[yyval[]m4_ifval([$1], [.$1])])


# b4_rhs_value(RULE-LENGTH, NUM, [TYPE])
# --------------------------------------
# Expansion of $<TYPE>NUM, where the current rule has RULE-LENGTH
# symbols on RHS.
m4_define([b4_rhs_value],
[semantic_stack_@{m4_eval([$1 - $2])@}m4_ifval([$3], [.$3])])

m4_define_default([b4_location_type], [Location])

# b4_lhs_location()
# -----------------
# Expansion of @$.
m4_define([b4_lhs_location],
[yyloc])


# b4_rhs_location(RULE-LENGTH, NUM)
# ---------------------------------
# Expansion of @NUM, where the current rule has RULE-LENGTH symbols
# on RHS.
m4_define([b4_rhs_location],
[location_stack_@{m4_eval([$1 - $2])@}])


m4_define([b4_inherit],
          [m4_ifdef([b4_root],
                    [: public b4_root
],
                    [])])

m4_define([b4_param],
          [m4_ifdef([b4_root],
                    [,
            const Param& param],
                    [])])

m4_define([b4_constructor],
          [m4_ifdef([b4_root],
                    [b4_root (param),
      ],
                    [])])


# We do want M4 expansion after # for CPP macros.
m4_changecom()
m4_divert(0)
dnl
@output pushParser.hh
b4_bjcopyright([2004])
#ifndef _BISON_PUSH_PARSER_INTERFACE_H_
#define _BISON_PUSH_PARSER_INTERFACE_H_
#include <iostream>
template<typename ST,typename LT>
class PushParser
{
    public:
    enum ParserState{Unknown=-1,Accept,Abort,Wait};
    virtual ~PushParser() {}
    virtual ParserState parseToken(int token,ST yylval,LT yylloc)=0;

    virtual std::ostream& debug_stream () = 0;
    /// Set the current debugging stream.
    virtual void set_debug_stream (std::ostream &) = 0;

    virtual int debug_level(void) = 0;
    virtual void set_debug_level(int l) = 0;

    virtual void accept()=0;
    virtual void error()=0;
};

#endif

dnl
@output @output_header_name@
b4_bjcopyright([2004],[This Code based on GNU Bison 1.875 lalr1.cc C++ Skeleton.])
[
/* FIXME: This is wrong, we want computed header guards.
   I don't know why the macros are missing now. :( */
#ifndef PUSH_PARSER_HEADER_H
#define PUSH_PARSER_HEADER_H

#include "pushParser.hh"
#include "location.hh"
#include "stack.hh"

#include <string>
#include <iostream>

/* Using locations.  */
#define YYLSP_NEEDED ]b4_locations_flag[

]b4_token_defines(b4_tokens)[

/* Copy the first part of user declarations.  */
]b4_pre_prologue[

]/* Line __line__ of lalr1.cc.  */
b4_syncline([@oline@], [@ofile@])[

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG ]b4_debug[
#endif

/* Enabling verbose error message.  */
#ifndef YYERROR_VERBOSE
# define YYERROR_VERBOSE ]b4_error_verbose[
#endif

#ifndef YYSTYPE
]m4_ifdef([b4_stype],
[typedef union 
b4_stype 
yystype;
/* Line __line__ of lalr1.cc.  */
b4_syncline([@oline@], [@ofile@])
    ],
[typedef int yystype;])[
#else
#define yystype YYSTYPE
#endif

#define yyltype yy::Location

/* Copy the second part of user declarations.  */
]b4_post_prologue[

]/* Line __line__ of lalr1.cc.  */
b4_syncline([@oline@], [@ofile@])[
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N) \
   Current.last.line = Rhs[N].last.line; \
   Current.last.column = Rhs[N].last.column;
#endif


#if YYLSP_NEEDED
class ]b4_parser_class_name[:public PushParser<yystype,yyltype>
#else
class ]b4_parser_class_name[:public PushParser<yystype,char*>
#endif
  {
  public:

    typedef ]b4_int_type_for([b4_translate])[ TokenNumberType;
    typedef ]b4_int_type_for([b4_rhs])[       RhsNumberType;
    typedef int      StateType;
    typedef yystype  SemanticType;
#if YYLSP_NEEDED
    typedef yyltype LocationType;
#endif


    typedef yy::Stack< StateType >    StateStack;
    typedef yy::Stack< SemanticType > SemanticStack;
#if YYLSP_NEEDED
    typedef yy::Stack< LocationType > LocationStack;
#endif


  public:
    virtual ~]b4_parser_class_name[();
#if YYLSP_NEEDED
    virtual ParserState parseToken(int token,SemanticType yylval,LocationType yylloc);
#else
    virtual ParserState parseToken(int token,SemanticType yylval,char* dummy);
#endif
    virtual std::ostream& debug_stream ();
    /// Set the current debugging stream.
    virtual void set_debug_stream (std::ostream &);

    virtual int debug_level(void);
    virtual void set_debug_level(int l);

    virtual void accept();
    virtual void error();
    virtual void YYACCEPT();
    virtual void YYABORT();
    virtual void YYERROR();
    virtual void yyerrok();
    virtual void yyerror(const char* errorString) =0;

  protected:
    ]b4_parser_class_name[(]b4_parse_param_decl[);

    void debugOutput(const int aInt);
    void debugOutput(const std::string aString);

    ParserState doDefault();
    void doReduce();
    ParserState doErrlab();
    ParserState doErrlab1();
    ParserState doAbortlab();
    ParserState doAcceptlab();

    /* Debugging. */
    int yydebug_;
    std::ostream* yycdebug_;
]b4_parse_param_vars[

    /* interna */
    int looka_;
    SemanticType value_;
#if YYLSP_NEEDED
    LocationType location_;
#endif

    /* State. */
    int n_;
    int state_;
    int nerrs_; 
    int errstatus_;
    
    /* Stacks. */
    StateStack    state_stack_;
    SemanticStack semantic_stack_;
#if YYLSP_NEEDED
    LocationStack location_stack_;
#endif

    /* Tables.  */
    static const ]b4_int_type_for([b4_pact])[ pact_[];
    static const ]b4_int_type(b4_pact_ninf, b4_pact_ninf)[ pact_ninf_;
    static const ]b4_int_type_for([b4_defact])[ defact_[];
    static const ]b4_int_type_for([b4_pgoto])[ pgoto_[];
    static const ]b4_int_type_for([b4_defgoto])[ defgoto_[];
    static const ]b4_int_type_for([b4_table])[ table_[];
    static const ]b4_int_type(b4_table_ninf, b4_table_ninf)[ table_ninf_;
    static const ]b4_int_type_for([b4_check])[ check_[];
    static const ]b4_int_type_for([b4_r1])[ r1_[];
    static const ]b4_int_type_for([b4_r2])[ r2_[];

#if YYDEBUG || YYERROR_VERBOSE
    static const char* const name_[];
#endif

    /* More tables, for debugging.  */
#if YYDEBUG
    static const RhsNumberType rhs_[];
    static const ]b4_int_type_for([b4_prhs])[ prhs_[];
    static const ]b4_int_type_for([b4_rline])[ rline_[];
    static const ]b4_int_type_for([b4_stos])[ stos_[];
    static const ]b4_int_type_for([b4_toknum])[ token_number_[];
#endif

    /* Even more tables.  */
    static inline TokenNumberType translate_ (int token);

    /* Constants.  */
    static const int eof_;
    /* LAST_ -- Last index in TABLE_.  */
    static const int last_;
    static const int nnts_;
    static const int empty_;
    static const int final_;
    static const int terror_;
    static const int errcode_;
    static const int ntokens_;
    static const int initdepth_;
    static const unsigned user_token_number_max_;
    static const TokenNumberType undef_token_;
};


#endif /* ! defined PUSH_PARSER_HEADER_H */]
dnl
@output @output_parser_name@
b4_bjcopyright([2004],[This Code based on GNU Bison 1.875 lalr1.cc C++ Skeleton.])
[
#include @output_header_name@

]b4_parser_class_name[::]b4_parser_class_name[(]b4_parse_param_decl[)
  : yydebug_ (false),
    yycdebug_ (&std::cerr)]b4_parse_param_cons[
{
    n_=0;
    state_=0;
    nerrs_=0;
    errstatus_=0;
    state_stack_=StateStack (0);
    semantic_stack_=SemanticStack (1);
#if YYLSP_NEEDED
    location_stack_=LocationStack (1);
#endif  
}
]b4_parser_class_name[::~]b4_parser_class_name[() {}
#if YYLSP_NEEDED
]b4_parser_class_name[::ParserState
]b4_parser_class_name[::parseToken(int token,SemanticType yylval,LocationType yylloc)
#else
]b4_parser_class_name[::ParserState
]b4_parser_class_name[::parseToken(int token,SemanticType yylval,char* dummy=NULL)
#endif
{
    static bool firstTime=true;
    ParserState returnValue=Unknown;
    if(firstTime) {
	firstTime=false;
	state_stack_.push (state_);
#if YYDEBUG
	debugOutput("Entering state ");
	debugOutput(state_);
	debugOutput("\n");
#endif
	while(returnValue==Unknown) {
	    n_ = pact_[state_];
	    if (n_ == pact_ninf_) {
		returnValue=doDefault();
		state_stack_.push (state_);
#if YYDEBUG
		debugOutput("Entering state ");
		debugOutput(state_);
		debugOutput("\n");
#endif
	    }  else {
		break;
	    }
	}
    }
    looka_=token;
    value_=yylval;
#if YYLSP_NEEDED
    location_=yylloc;
#endif

#if YYDEBUG
    debugOutput("Starting parse token: ");
    debugOutput(looka_);
    debugOutput(" (");
    debugOutput(name_[translate_(looka_)]);
    debugOutput(")\n");
#endif

/* New state.  */
    while(returnValue==Unknown) {
	    /* Read a lookahead token.  */
	    if(looka_==empty_) {
		return Wait;
	    }
	    /* Convert token to internal form.  */
	    if (looka_ <= 0)
	    {
		looka_ = eof_;
#if YYDEBUG
		debugOutput("Now at end of input.\n");
#endif
	    } else {
#if YYDEBUG
		debugOutput("Next token is ");
		debugOutput(looka_);
		debugOutput(" (");
		debugOutput(name_[translate_(looka_)]);
		debugOutput(")\n");
#endif
	    }
	    n_ += translate_(looka_);
	    if (n_ < 0 || last_ < n_ || check_[n_] != translate_(looka_)) {
		returnValue=doDefault();
	    } else {
		/* Reduce or error.  */
		n_ = table_[n_];
		if(n_==0 || n_==table_ninf_) {
			returnValue=doErrlab();
		} else if(n_==-n_) {
			doReduce();
		} else if(n_==final_) {
			returnValue=doAcceptlab();
		} else {
			/* Shift the lookahead token.  */
#if YYDEBUG
			debugOutput("Shifting token ");
			debugOutput(looka_);
			debugOutput(" (");
			debugOutput(name_[translate_(looka_)]);
			debugOutput("), ");
#endif
			/* Discard the token being shifted unless it is eof.  */
			if (looka_ != eof_) looka_ = empty_;
			semantic_stack_.push (value_);
#if YYLSP_NEEDED
			location_stack_.push (location_);
#endif
			/* Count tokens shifted since error; after three, turn off error status. */
			if (errstatus_) --errstatus_;
			state_ = n_;
		}
	    }
	    while(returnValue==Unknown) {
		state_stack_.push (state_);
#if YYDEBUG
		debugOutput("Entering state ");
		debugOutput(state_);
		debugOutput("\n");
#endif
		/* Try to take a decision without lookahead.  */
		n_ = pact_[state_];
		if (n_ == pact_ninf_) {
		    returnValue=doDefault();
		} else {
		    break;
		}
	    }
	}
	return(returnValue);
}
std::ostream&
]b4_parser_class_name[::debug_stream ()
{
    return *yycdebug_;
}

void
]b4_parser_class_name[::set_debug_stream (std::ostream& o)
{
    yycdebug_ = &o;
}
void
]b4_parser_class_name[::set_debug_level(int l) 
{
    yydebug_=l;
}
int
]b4_parser_class_name[::debug_level() 
{
    return yydebug_;
}
void
]b4_parser_class_name[::accept() 
{
}
void
]b4_parser_class_name[::error() 
{
}
void
]b4_parser_class_name[::YYACCEPT() 
{
    doAcceptlab();
}
void
]b4_parser_class_name[::YYABORT() 
{
    doAbortlab();
}
void
]b4_parser_class_name[::YYERROR() 
{
    doErrlab1();
}
void
]b4_parser_class_name[::yyerrok() 
{
    errstatus_=0;
}

void
]b4_parser_class_name[::debugOutput(const int aInt)
{
    if(yydebug_) {
	*yycdebug_ << aInt;
    }
}
void
]b4_parser_class_name[::debugOutput(const std::string aString)
{
    if(yydebug_) {
	*yycdebug_ << aString;
    }
}

]b4_parser_class_name[::ParserState
]b4_parser_class_name[::doErrlab1() 
{
    if ((errstatus_ == 3) && (looka_ == eof_)) {
            /* If just tried and failed to reuse lookahead token after an error, discard it. */
            /* Return failure if at end of input.  */
            return(doAbortlab());
    } else {
	if(errstatus_ == 3) {
#if YYDEBUG
            debugOutput("Discarding token ");
            debugOutput(looka_);
            debugOutput(" (");
            debugOutput(name_[translate_(looka_)]);
            debugOutput(").\n");
#endif
            looka_ = empty_;
	} 
	/* Else will try to reuse lookahead token after shifting the error token.  */
	errstatus_ = 3;
	for (;;)
	{
	    n_ = pact_[state_];
	    if (n_ != pact_ninf_)
	    {
		n_ += terror_;
		if (0 <= n_ && n_ <= last_ && check_[n_] == terror_)
		{
		    n_ = table_[n_];
		    if (0 < n_)
			break;
		}
	    }
	    /* Pop the current state because it cannot handle the error token.  */
	    if (!state_stack_.height ()) {
		return(doAbortlab()); 
	    } else {
#if YYDEBUG
		if (stos_[state_] < ntokens_)
		{
		    debugOutput("Error: popping token ");
		    debugOutput(token_number_[stos_[state_]]);
		    debugOutput(" (");
		    debugOutput(name_[stos_[state_]]);
		    debugOutput(")\n");
		} else {
		    debugOutput("Error: popping nonterminal (");
		    debugOutput(name_[stos_[state_]]);
		    debugOutput(")\n");
		}
#endif
		state_ = (state_stack_.pop (), state_stack_[0]);
		semantic_stack_.pop ();
#if YYLSP_NEEDED
		location_stack_.pop ();;
#endif
#if YYDEBUG
		debugOutput("Error: state stack now");
		for (StateStack::ConstIterator i = state_stack_.begin (); i != state_stack_.end (); ++i) {
		    debugOutput(" ");
		    debugOutput(*i);
		}
		debugOutput("\n");;
#endif
	    }
	}
	if (n_ == final_) {
	    return(doAcceptlab());
	} else {
#if YYDEBUG
	    debugOutput("Shifting error token, ");
#endif
	    semantic_stack_.push (value_);
#if YYLSP_NEEDED
	    location_stack_.push (location_);
#endif
	    state_ = n_;
	}
    }
#if YYDEBUG
    debugOutput("Call error()!\n");
#endif
    error();
    return(Unknown);
}
]b4_parser_class_name[::ParserState
]b4_parser_class_name[::doErrlab() 
{
    // If not already recovering from an error, report this error.
    if (!errstatus_)
    {
        ++nerrs_;
#if YYERROR_VERBOSE
        n_ = pact_[state_];
        if (pact_ninf_ < n_ && n_ < last_)
        {
            debugOutput("syntax error, unexpected token: ");
            debugOutput(looka_);
            debugOutput( "(");
            debugOutput(name_[translate_(looka_)]);
            debugOutput(")\n");
            {
                int count = 0;
                for (int x = (n_ < 0 ? -n_ : 0); x < ntokens_ + nnts_; ++x)
                    if (check_[x + n_] == x && x != terror_)
                        ++count;
                if (count < 5)
                {
                count = 0;
                for (int x = (n_ < 0 ? -n_ : 0); x < ntokens_ + nnts_; ++x)
                    if (check_[x + n_] == x && x != terror_)
                    {
                        (!count++) ? debugOutput("expecting ") : debugOutput(" or ");
                        debugOutput(name_[x]);
                    }
                }
            }
            debugOutput("\n");
        } else
#endif
        {
#if YYDEBUG || YYERROR_VERBOSE
	    if(yydebug_) {
		debugOutput("syntax error\n");
	    } else 
#endif
            yyerror("syntax error\n");
        }
    }
    return(doErrlab1());
}
]b4_parser_class_name[::ParserState
]b4_parser_class_name[::doDefault() 
{
    n_ = defact_[state_];
    if (n_ == 0) {
	return(doErrlab());
    } 
    doReduce();
    return(Unknown);
}
void
]b4_parser_class_name[::doReduce() 
{
    SemanticType yyval;
#if YYLSP_NEEDED
    LocationType yyloc;
#endif
    int len = r2_[n_];
    if (len)
    {
        yyval = semantic_stack_[len - 1];
#if YYLSP_NEEDED
        yyloc = location_stack_[len - 1];
#endif
    } else {
        yyval = semantic_stack_[0];
#if YYLSP_NEEDED
        yyloc = location_stack_[0];
#endif
    }

#if YYDEBUG
    debugOutput("Reducing via rule ");
    debugOutput(n_ - 1);
    debugOutput(" (line ");
    debugOutput(rline_[n_]);
    debugOutput("), ");
    for(unsigned char i = prhs_[n_]; 0 <= rhs_[i]; ++i) {
        debugOutput(name_[rhs_[i]]);
        debugOutput(" ");
    }
    debugOutput("-> ");
    debugOutput(name_[r1_[n_]]);
    debugOutput("\n");
#endif

#if YYLSP_NEEDED
    if (len)
    {
        yy::Slice< LocationType, LocationStack > slice (location_stack_, len);
        YYLLOC_DEFAULT (yyloc, slice, len);
    }
#endif
   switch (n_)
    {
      ]b4_actions[
    }

]/* Line __line__ of lalr1-push.cc.  */
b4_syncline([@oline@], [@ofile@])[

    state_stack_.pop (len);
    semantic_stack_.pop (len);
#if YYLSP_NEEDED
    location_stack_.pop (len);
#endif
#if YYDEBUG
    debugOutput("state stack now");
    for (StateStack::ConstIterator i = state_stack_.begin (); i != state_stack_.end (); ++i) {
        debugOutput(" ");
        debugOutput(*i);
    }
    debugOutput("\n");
#endif

    semantic_stack_.push (yyval);
#if YYLSP_NEEDED
    location_stack_.push (yyloc);
#endif

    /* Shift the result of the reduction.  */
    n_ = r1_[n_];
    state_ = pgoto_[n_ - ntokens_] + state_stack_[0];
    if (0 <= state_ && state_ <= last_ && check_[state_] == state_stack_[0])
        state_ = table_[state_];
    else
        state_ = defgoto_[n_ - ntokens_];
}
]b4_parser_class_name[::ParserState
]b4_parser_class_name[::doAbortlab() 
{
#if YYDEBUG
    debugOutput("Parser abort!\n");
    debugOutput("Call error()!\n");
#endif
    error();
    return(Abort);
}
]b4_parser_class_name[::ParserState
]b4_parser_class_name[::doAcceptlab() 
{
#if YYDEBUG
    debugOutput("Parser accept!\n");
    debugOutput("Call accept()!\n");
#endif
    accept();
    return(Accept);
}

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
const ]b4_int_type(b4_pact_ninf, b4_pact_ninf) b4_parser_class_name::pact_ninf_ = b4_pact_ninf[;
const ]b4_int_type_for([b4_pact])[
]b4_parser_class_name[::pact_[] =
{
  ]b4_pact[
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error.  */
const ]b4_int_type_for([b4_defact])[
]b4_parser_class_name[::defact_[] =
{
  ]b4_defact[
};

/* YYPGOTO[NTERM-NUM].  */
const ]b4_int_type_for([b4_pgoto])[
]b4_parser_class_name[::pgoto_[] =
{
  ]b4_pgoto[
};

/* YYDEFGOTO[NTERM-NUM].  */
const ]b4_int_type_for([b4_defgoto])[
]b4_parser_class_name[::defgoto_[] =
{
  ]b4_defgoto[
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.  */
const ]b4_int_type(b4_table_ninf, b4_table_ninf) b4_parser_class_name::table_ninf_ = b4_table_ninf[;
const ]b4_int_type_for([b4_table])[
]b4_parser_class_name[::table_[] =
{
  ]b4_table[
};

/* YYCHECK.  */
const ]b4_int_type_for([b4_check])[
]b4_parser_class_name[::check_[] =
{
  ]b4_check[
};

#if YYDEBUG
/* STOS_[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
const ]b4_int_type_for([b4_stos])[
]b4_parser_class_name[::stos_[] =
{
  ]b4_stos[
};

/* TOKEN_NUMBER_[YYLEX-NUM] -- Internal token number corresponding
   to YYLEX-NUM.  */
const ]b4_int_type_for([b4_toknum])[
]b4_parser_class_name[::token_number_[] =
{
  ]b4_toknum[
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
const ]b4_int_type_for([b4_r1])[
]b4_parser_class_name[::r1_[] =
{
  ]b4_r1[
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
const ]b4_int_type_for([b4_r2])[
]b4_parser_class_name[::r2_[] =
{
  ]b4_r2[
};

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
const char*
const ]b4_parser_class_name[::name_[] =
{
  ]b4_tname[
};
#endif

#if YYDEBUG
/* YYRHS -- A `-1'-separated list of the rules' RHS. */
const ]b4_parser_class_name[::RhsNumberType
]b4_parser_class_name[::rhs_[] =
{
  ]b4_rhs[
};

/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
const ]b4_int_type_for([b4_prhs])[
]b4_parser_class_name[::prhs_[] =
{
  ]b4_prhs[
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
const ]b4_int_type_for([b4_rline])[
]b4_parser_class_name[::rline_[] =
{
  ]b4_rline[
};
#endif

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
]b4_parser_class_name[::TokenNumberType
]b4_parser_class_name[::translate_ (int token)
{
  static
  const TokenNumberType
  translate_[] =
  {
    ]b4_translate[
  };
  if ((unsigned) token <= user_token_number_max_)
    return translate_[token];
  else
    return undef_token_;
}

const int ]b4_parser_class_name[::eof_ = 0;
const int ]b4_parser_class_name[::last_ = ]b4_last[;
const int ]b4_parser_class_name[::nnts_ = ]b4_nterms_number[;
const int ]b4_parser_class_name[::empty_ = -2;
const int ]b4_parser_class_name[::final_ = ]b4_final_state_number[;
const int ]b4_parser_class_name[::terror_ = 1;
const int ]b4_parser_class_name[::errcode_ = 256;
const int ]b4_parser_class_name[::ntokens_ = ]b4_tokens_number[;
const int ]b4_parser_class_name[::initdepth_ = ]b4_stack_depth_init[;

const unsigned ]b4_parser_class_name[::user_token_number_max_ = ]b4_user_token_number_max[;
const ]b4_parser_class_name[::TokenNumberType ]b4_parser_class_name[::undef_token_ = ]b4_undef_token_number[;

]b4_epilogue
dnl
@output stack.hh
b4_copyright([2002])
[
#ifndef _BISON_STACK_HH
# define _BISON_STACK_HH

#include <deque>

namespace yy
{
  template < class T, class S = std::deque< T > >
  class Stack
  {
  public:

    typedef typename S::iterator Iterator;
    typedef typename S::const_iterator ConstIterator;

    Stack () : seq_ ()
    {
    }

    Stack (unsigned n) : seq_ (n)
    {
    }

    inline
    T&
    operator [] (unsigned index)
    {
      return seq_[index];
    }

    inline
    const T&
    operator [] (unsigned index) const
    {
      return seq_[index];
    }

    inline
    void
    push (const T& t)
    {
      seq_.push_front (t);
    }

    inline
    void
    pop (unsigned n = 1)
    {
      for (; n; --n)
	seq_.pop_front ();
    }

    inline
    unsigned
    height () const
    {
      return seq_.size ();
    }

    inline ConstIterator begin () const { return seq_.begin (); }
    inline ConstIterator end () const { return seq_.end (); }

  private:

    S seq_;
  };

  template < class T, class S = Stack< T > >
  class Slice
  {
  public:

    Slice (const S& stack,
	   unsigned range) : stack_ (stack),
			     range_ (range)
    {
    }

    inline
    const T&
    operator [] (unsigned index) const
    {
      return stack_[range_ - index];
    }

  private:

    const S& stack_;
    unsigned range_;
  };
}

#endif // not PUSH_BISON_STACK_HH]
dnl
@output location.hh
b4_copyright([2002])
[

#ifndef BISON_LOCATION_HH
# define BISON_LOCATION_HH

namespace yy
{
  struct Position
  {
    int line;
    int column;
  };

  struct Location
  {
    Position first;
    Position last;
  };
}

#endif // not BISON_LOCATION_HH]
