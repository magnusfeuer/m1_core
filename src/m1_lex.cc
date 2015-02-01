//
// Hand written scanner since flex is not flexible!!!!
//

#include "m1.hh"
#include "m1c.hh"
#include "m1_parse.hh"
#include "m1_lex.hh"


// This is ctype'ish. I want to be sure os locale does not screw up parsing.
#define LC_DIGIT  0x0001    // 0..9
#define LC_ODIGIT 0x0002    // 0..7
#define LC_XDIGIT 0x0004    // 0..9a-fA-F
#define LC_ALPHA  0x0010    // a..ZA .. Z
#define LC_LOWER  0x0020    // a..z
#define LC_UPPER  0x0040    // A..Z

#define LC_SPACE  0x1000   // ' ' '\t'
#define LC_BLANK  0x2000   // ' ' '\t' '\r' '\n' '\v' '\f'
#define LC_CNTRL  0x4000   // control character
#define LC_PRINT  0x8000   // printable character

#define LC_O      (LC_DIGIT|LC_ODIGIT|LC_XDIGIT|LC_PRINT)
#define LC_D      (LC_DIGIT|LC_XDIGIT|LC_PRINT)
#define LC_A      (LC_UPPER|LC_ALPHA|LC_PRINT)
#define LC_a      (LC_LOWER|LC_ALPHA|LC_PRINT)
#define LC_X      (LC_UPPER|LC_ALPHA|LC_XDIGIT|LC_PRINT)
#define LC_x      (LC_LOWER|LC_ALPHA|LC_XDIGIT|LC_PRINT)
#define LC_S      (LC_SPACE|LC_BLANK|LC_PRINT)
#define LC_B      (LC_BLANK|LC_PRINT)
#define LC_P      (LC_PRINT)
#define LC_C      (LC_CNTRL)


static unsigned int lex_lc[259] =
{
    LC_C,            // 0 - '\000' 
    LC_C,            // 1 - '\001' 
    LC_C,            // 2 - '\002' 
    LC_C,            // 3 - '\003' 
    LC_C,            // 4 - '\004' 
    LC_C,            // 5 - '\005' 
    LC_C,            // 6 - '\006' 
    LC_C,            // 7 - '\007' 
    LC_C,            // 8 - '\b' 
    LC_S,            // 9 - '\t'
    LC_B,            // 10 - '\n'
    LC_B,            // 11 - '\v'
    LC_B,            // 12 - '\f'
    LC_B,            // 13 - '\r'
    LC_C,            // 14 - '\016' 
    LC_C,            // 15 - '\017' 
    LC_C,            // 16 - '\020' 
    LC_C,            // 17 - '\021' 
    LC_C,            // 18 - '\022' 
    LC_C,            // 19 - '\023' 
    LC_C,            // 20 - '\024' 
    LC_C,            // 21 - '\025' 
    LC_C,            // 22 - '\026' 
    LC_C,            // 23 - '\027' 
    LC_C,            // 24 - '\030' 
    LC_C,            // 25 - '\031' 
    LC_C,            // 26 - '\032' 
    LC_C,            // 27 - '\e'   ESC
    LC_C,            // 28 - '\034' 
    LC_C,            // 29 - '\035' 
    LC_C,            // 30 - '\036' 
    LC_C,            // 31 - '\037' 
    LC_S,            // 32 - ' '
    LC_P,            // 33 - '!'
    LC_P,            // 34 - '"'
    LC_P,            // 35 - '#'
    LC_P,            // 36 - '$'
    LC_P,            // 37 - '%'
    LC_P,            // 38 - '&'
    LC_P,            // 39 - '\''
    LC_P,            // 40 - '('
    LC_P,            // 41 - ')'
    LC_P,            // 42 - '*'
    LC_P,            // 43 - '+'
    LC_P,            // 44 - ','
    LC_P,            // 45 - '-'
    LC_P,            // 46 - '.'
    LC_P,            // 47 - '/'
    LC_O, LC_O, LC_O, LC_O, LC_O, LC_O, LC_O, LC_O,  // '0' - '7'  [48 - 55]
    LC_D, LC_D,   // '8' - '9'  [56 - 57]
    LC_P,            // 58 - ':'
    LC_P,            // 59 - ';'
    LC_P,            // 60 - '<'
    LC_P,            // 61 - '='
    LC_P,            // 62- '>'
    LC_P,            // 63 - '?'
    LC_P,            // 64 - '@'
    LC_X,LC_X,LC_X,LC_X,LC_X,LC_X,   // A- F   [65 - 
    LC_A,LC_A,LC_A,LC_A,LC_A,        // G- K 
    LC_A,LC_A,LC_A,LC_A,LC_A,LC_A,LC_A,LC_A,LC_A,LC_A,LC_A,  // L - V
    LC_A,LC_A,LC_A,LC_A,                                     // W - Z  - 90]
    LC_P,            // 91 - '['
    LC_P,            // 92 - '\\'
    LC_P,            // 93 - ']'
    LC_P,            // 94 - '^'
    LC_P,            // 95 - '_'
    LC_P,            // 96 - '`'
    LC_x,LC_x,LC_x,LC_x,LC_x,LC_x,   // a - f  [97 - 
    LC_a,LC_a,LC_a,LC_a,LC_a,        // g - k
    LC_a,LC_a,LC_a,LC_a,LC_a,LC_a,LC_a,LC_a,LC_a,LC_a,LC_a,  // l - v
    LC_a,LC_a,LC_a,LC_a,                                     // w - z  - 122]
    LC_P,            // 123 - '{'
    LC_P,            // 124 - '|'
    LC_P,            // 125 - '}'
    LC_P,            // 126 - '~'
    LC_C,            // 127 - '\177' DEL
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,               // LEX_MORE
    0,               // LEX_EOF
    0                // LEX_ERROR
};

#include "m1_keywords.hh"

/*************
// FIXME use gperf -t to generate a hash function
// then use include to use it. !!!!
static struct {
    char* keyword;
    int   token;
} keywords[] = {
    { "nil", m1::m1Parser::token::M1NIL },
    { "this", m1::m1Parser::token::M1THIS },
    { "true", m1::m1Parser::token::M1TRUE },
    { "false", m1::m1Parser::token::M1FALSE},
    { "type", m1::m1Parser::token::TYPE },
    { "typedef", m1::m1Parser::token::TYPEDEF },
    { "library", m1::m1Parser::token::LIBRARY },
    { "application", m1::m1Parser::token::APPLICATION },
    { "interface", m1::m1Parser::token::INTERFACE },
    { "event", m1::m1Parser::token::EVENT },
    { "input", m1::m1Parser::token::INPUT },
    { "output", m1::m1Parser::token::OUTPUT },
    { "queue", m1::m1Parser::token::QUEUE },
    { "public", m1::m1Parser::token::PUBLIC },
    { "private", m1::m1Parser::token::PRIVATE },
    { "protected", m1::m1Parser::token::PROTECTED },
    { "persistent", m1::m1Parser::token::PERSISTENT },
    { "char", m1::m1Parser::token::CHAR },
    { "byte", m1::m1Parser::token::BYTE },
    { "int", m1::m1Parser::token::INT },
    { "signed", m1::m1Parser::token::SIGNED },
    { "unsigned", m1::m1Parser::token::UNSIGNED },
    { "float", m1::m1Parser::token::FLOAT },
    { "bool", m1::m1Parser::token::BOOL },
    { "string", m1::m1Parser::token::STRING},
    { "const", m1::m1Parser::token::CONST},
    { "enum", m1::m1Parser::token::ENUM },
    { "time", m1::m1Parser::token::TIME },
    { "when", m1::m1Parser::token::WHEN },
    { "script", m1::m1Parser::token::SCRIPT},
    { "step", m1::m1Parser::token::STEP},
    { "case", m1::m1Parser::token::CASE},
    { "default", m1::m1Parser::token::DEFAULT},
    { "if", m1::m1Parser::token::IF},
    { "else", m1::m1Parser::token::ELSE},
    { "switch", m1::m1Parser::token::SWITCH},
    { "foreach", m1::m1Parser::token::FOREACH},
    { "in", m1::m1Parser::token::IN},
    { "continue", m1::m1Parser::token::CONTINUE},
    { "break", m1::m1Parser::token::BREAK},
    { "return", m1::m1Parser::token::RETURN},
    { NULL, 0}
};
****/
static inline int m1_keyword(char* text, unsigned int len)
{
    struct keyword* wp = Perfect_Hash::in_word_set(text, len);
    if (wp == NULL)
	return m1::m1Parser::token::M1IDENTIFIER;
    else
	return wp->token;
}

#ifdef TEST
int m1_lex(M1Lex* aLex)
{
    int token = aLex->getToken();
    if (token == LEX_EOF)
	return 0;
    if (token == LEX_MORE)
	return -2;
    if (token == LEX_ERROR)
	return -1;
    return token;
}

int main(int argc, char** argv)
{
    CBioStream* bio;
    M1Lex* lex;
    int token;
    char* filename;

    if (argc < 2) {
	bio = new CBioStream();
	bio.open_fp_read("*stdin*", stdin, M1_USAGE_M1, false, false);
    }
    else {
	bio = new CBioStream();
	bio.open_file_read(file_name[i], M1_USAGE_M1, false, false);
    }

    if (bio == NULL) {
	fprintf(stderr, "unable to open %s\n", filename);
	exit(1);
    }
    lex = new M1Lex(bio, filename,  1);
again:
    while((token = m1_lex(lex)) > 0) {
	printf("%s:%d: %s [%s]\n", 
	       lex->fileName(),
	       lex->lineNumber(),
	       lex->tokenName(token),
	       lex->tokenText());
    }
    if (token == -2) {
	printf("AGAIN\n");
	goto again;
    }
    BIO_free(bio);
    exit(0);
}
#endif


M1Lex::M1Lex(CBioStream* aBio, string* aFileName, size_t aMaxLength)
{
    mBio    = aBio;               // maybe turn off auto close?
    mMaxReadLength = aMaxLength;  // max number of chars to read
    mReadLength = 0;              // number of chars read
    mContinue = NULL;             // continuation address

    mPtr    = mBuffer;  // current buffer position
    mPtrEnd = mBuffer;  // end of buffer

    mChar   = 0;        // current character
    mCharSp = 0;        // char stack pointer

    mTokPtr     = mTokBuffer;
    mTokPtrEnd  = mTokBuffer + MAX_TOKEN_SIZE - 1;

    mLocation.initialize(aFileName);
}

M1Lex::~M1Lex()
{

}

//
// putback a character - on a stack
// also backup the line and token pointers
//
void M1Lex::putbackChar(int c)
{
    if (mCharSp < MAX_CHAR_STACK) {
	mCharStack[mCharSp++] = c;
	mChar = c;
	mTokPtr--;
	if (c == '\n')
	    mLocation.end.lines(-1);
	else
	    mLocation.end.columns(-1);
    }
}

//
// Read the next character 
// speical return:
//    LEX_MORE  -- could not read all data
//    LEX_EOF   -- file close
//    LEX_ERROR -- i/o error
//
int M1Lex::getChar(void)
{
    int c;

    if (mCharSp)
	c = mCharStack[--mCharSp];
    else {
	if (mPtr >= mPtrEnd) {
	    size_t len = sizeof(mBuffer);
	    int n;

	    if (mMaxReadLength) {
		if (((len = mMaxReadLength - mReadLength)) >= sizeof(mBuffer))
		    len = sizeof(mBuffer);
		if (len == 0) {
		    mReadLength = 0;
		    return LEX_MORE;
		}
	    }

	    if ((n = mBio->read(mBuffer, len)) <= 0) {
		if (mBio->should_retry()) {
		    return LEX_MORE;
		}
		if (n == 0)
		    return LEX_EOF;
		return LEX_ERROR;
	    }
	    mReadLength += n;
	    mPtr = mBuffer;
	    mPtrEnd = mBuffer + n;
	}
	c = *mPtr++;
    }
    if (mTokPtr < mTokPtrEnd)
	*mTokPtr++ = c;
    if (c == '\n')
	mLocation.lines(1);
    else
	mLocation.columns(1);
    mChar = c;
    return c;
}

char* M1Lex::tokenText()
{
    *mTokPtr = '\0';
    return mTokBuffer;
}

unsigned int M1Lex::tokenLength()
{
    return mTokPtr - mTokBuffer;
}

string M1Lex::tokenString()
{
    return string(mTokBuffer, tokenLength());
}

// get token name
char* M1Lex::tokenName(int aToken) 
{
    switch(aToken) {
    case '<': return "<";
    case '>': return ">";
    case '+': return "+";
    case '-': return "-";
    case '*': return "*";
    case '/': return "/";
    case '%': return "%";
    case '&': return "&";
    case '|': return "|";
    case '=': return "=";
    case ':': return ":";
    case '.': return ".";
    case ';': return ";";
    case '{': return "{";
    case '}': return "}";
    case '[': return "[";
    case ']': return "]";
    case '(': return "(";
    case ')': return ")";
    case '~': return "~";
    case '?': return "?";
    case '@': return "@";
    case m1::m1Parser::token::M1BLOCK_COMMENT: return "M1BLOCK_COMMENT";
    case m1::m1Parser::token::M1LINE_COMMENT: return "M1LINE_COMMENT";
    case m1::m1Parser::token::M1IDENTIFIER: return "M1IDENTIFIER";
    case m1::m1Parser::token::M1CHAR: return "M1CHAR";
    case m1::m1Parser::token::M1INTEGER: return "M1INTEGER";
    case m1::m1Parser::token::M1FLOAT: return "M1FLOAT";
    case m1::m1Parser::token::M1STRING: return "M1STRING";
    case m1::m1Parser::token::SHIFTL_OP: return "SHIFTL_OP";
    case m1::m1Parser::token::SHIFTR_OP: return "SHIFTR_OP";
    case m1::m1Parser::token::LE_OP: return "LE_OP";
    case m1::m1Parser::token::GE_OP: return "GE_OP";
    case m1::m1Parser::token::EQ_OP: return "EQ_OP";
    case m1::m1Parser::token::NE_OP: return "NE_OP";
    case m1::m1Parser::token::EQL_OP: return "EQL_OP";
    case m1::m1Parser::token::NQL_OP: return "NQL_OP";
    case m1::m1Parser::token::INC_OP: return "INC_OP";
    case m1::m1Parser::token::DEC_OP: return "DEC_OP";
    case m1::m1Parser::token::POST_INC_OP: return "POST_INC_OP";
    case m1::m1Parser::token::POST_DEC_OP: return "POST_DEC_OP";
    case m1::m1Parser::token::PRE_INC_OP: return "PRE_INC_OP";
    case m1::m1Parser::token::PRE_DEC_OP: return "PRE_DEC_OP";
    case m1::m1Parser::token::AND_OP: return "AND_OP";
    case m1::m1Parser::token::OR_OP: return "OR_OP";
    case m1::m1Parser::token::COPY_ASSIGN: return "COPY_ASSIGN";
    case m1::m1Parser::token::MUL_ASSIGN: return "MUL_ASSIGN";
    case m1::m1Parser::token::DIV_ASSIGN: return "DIV_ASSIGN";
    case m1::m1Parser::token::REM_ASSIGN: return "REM_ASSIGN";
    case m1::m1Parser::token::ADD_ASSIGN: return "ADD_ASSIGN";
    case m1::m1Parser::token::SUB_ASSIGN: return "SUB_ASSIGN";
    case m1::m1Parser::token::SHIFTL_ASSIGN: return "SHIFTL_ASSIGN";
    case m1::m1Parser::token::SHIFTR_ASSIGN: return "SHIFTR_ASSIGN";
    case m1::m1Parser::token::AND_ASSIGN: return "AND_ASSIGN";
    case m1::m1Parser::token::XOR_ASSIGN: return "XOR_ASSIGN";
    case m1::m1Parser::token::OR_ASSIGN: return "OR_ASSIGN";
    case m1::m1Parser::token::M1TIDENTIFIER: return "M1TIDENTIFIER";
    case m1::m1Parser::token::CONNECT_OP: return "CONNECT_OP";
    case m1::m1Parser::token::M1NIL: return "M1NIL";
    case m1::m1Parser::token::M1THIS: return "M1THIS";
    case m1::m1Parser::token::M1TRUE: return "M1TRUE";
    case m1::m1Parser::token::M1FALSE: return "M1FALSE";
    case m1::m1Parser::token::TYPE: return "TYPE";
    case m1::m1Parser::token::LIBRARY: return "LIBRARY";
    case m1::m1Parser::token::INTERFACE: return "INTERFACE";
    case m1::m1Parser::token::EVENT: return "EVENT";
    case m1::m1Parser::token::INPUT: return "INPUT";
    case m1::m1Parser::token::OUTPUT: return "OUTPUT";
    case m1::m1Parser::token::QUEUE: return "QUEUE";
    case m1::m1Parser::token::PUBLIC: return "PUBLIC";
    case m1::m1Parser::token::PRIVATE: return "PRIVATE";
    case m1::m1Parser::token::PERSISTENT: return "PERSISTENT";
    case m1::m1Parser::token::CHAR: return "CHAR";
    case m1::m1Parser::token::BYTE: return "BYTE";
    case m1::m1Parser::token::INT: return "INT";
    case m1::m1Parser::token::SIGNED: return "SIGNED";
    case m1::m1Parser::token::UNSIGNED: return "UNSIGNED";
    case m1::m1Parser::token::FLOAT: return "FLOAT";
    case m1::m1Parser::token::BOOL: return "BOOL";
    case m1::m1Parser::token::STRING: return "STRING";
    case m1::m1Parser::token::CONST: return "CONST";
    case m1::m1Parser::token::ENUM: return "ENUM";
    case m1::m1Parser::token::TIME: return "TIME";
    case m1::m1Parser::token::WHEN: return "WHEN";
    case m1::m1Parser::token::SCRIPT: return "SCRIPT";
    case m1::m1Parser::token::RANGE: return "RANGE";
    case m1::m1Parser::token::STEP: return "STEP";
    case m1::m1Parser::token::CASE: return "CASE";
    case m1::m1Parser::token::DEFAULT: return "DEFAULT";
    case m1::m1Parser::token::IF: return "IF";
    case m1::m1Parser::token::ELSE: return "ELSE";
    case m1::m1Parser::token::SWITCH: return "SWITCH";
    case m1::m1Parser::token::FOREACH: return "FOREACH";
    case m1::m1Parser::token::IN: return "IN";
    case m1::m1Parser::token::CONTINUE: return "CONTINUE";
    case m1::m1Parser::token::BREAK: return "BREAK";
    case m1::m1Parser::token::RETURN: return "RETURN";
    default: return "????";
    }
}

//
// Convert string to internal form (i.e. replace \n => 10 etc)
//
char* M1Lex::unquote(char* s)
{
    char* q = mStringBuf;

    if (*s == '"')
	s++;
    while(*s != '\0') {
	if (*s == '\\') {
	    s++;
	    switch(*s) {
	    case 'e': *q++ = '\e'; break;
	    case 'a': *q++ = '\a'; break;
	    case 'b': *q++ = '\b'; break;
	    case 'f': *q++ = '\f'; break;
	    case 'n': *q++ = '\n'; break;
	    case 'r': *q++ = '\r'; break;
	    case 'v': *q++ = '\v'; break;
	    case '\'': *q++ = '\''; break;
	    case '\\': *q++ = '\\'; break;
	    case '\0': break;
		// Add \xHH | \xHHHH ?
	    default:
		if (lex_lc[(int)*s] & LC_ODIGIT) {
		    int ov = *s++-'0';
		    if (lex_lc[(int)*s] & LC_ODIGIT) {
			ov = ov*8 + (*s++ -'0');
			if (lex_lc[(int)*s] & LC_ODIGIT) {
			    ov = ov*8 + (*s++ -'0');
			}
		    }
		    *q++ = ov;
		}
		else
		    *q++ = *s;
		break;
	    }
	    if (*s != '\0')
		s++;
	}
	else if ((*s == '"') && (*(s+1) == '\0'))
	    break;
	else
	    *q++ = *s++;
    }
    *q = '\0';
    return mStringBuf;
}

// Scan for tokens
// return:
//     LEX_MORE   when lexer want more input
//     LEX_EOF    when processed all input
//     LEX_ERROR  error condition
//     token      otherwise
//

#define GET_CHAR(c, label) do {				\
    label:						\
        c = getChar();					\
	if (c == LEX_MORE) {				\
	    mContinue = &&label;			\
	    return LEX_MORE;				\
	}						\
	if (c == LEX_ERROR)				\
	    return LEX_ERROR;				\
    } while(0)
	
    
int M1Lex::getToken(void)
{
    int c = 0;
    
    if (mContinue) {
	void* addr = mContinue;
	mContinue = NULL;
	goto *addr;
    }

    while(1) {
	resetToken();
	switch((c = getChar())) {
	case LEX_MORE:
	    return LEX_MORE;
	case LEX_ERROR:
	    return LEX_ERROR;
	case LEX_EOF:
	    return LEX_EOF;
	case ' ': 
	case '\t':
	case '\r':
	case '\n':
	    break;
	case '"': 
	    while(1) {
		GET_CHAR(c, L_string1);
		if (c == '"')
		    return m1::m1Parser::token::M1STRING;
		else if (c == '\\') {
		    GET_CHAR(c, L_string2);
		}
	    }
	    break;

	case '\'':
	    while (1) {
		GET_CHAR(c, L_char1);
		if (c == '\'')
		    return m1::m1Parser::token::M1CHAR;
		else if (c == '\\') {
		    GET_CHAR(c, L_char2);
		}
	    }
	    break;

	case '<':
	    // < |  <- | <= | <<= | <<
	    GET_CHAR(c, L_lt1);
	    switch(c) {
	    case '-': return m1::m1Parser::token::CONNECT_OP;
	    case '=': return m1::m1Parser::token::LE_OP;
	    case '<':
		GET_CHAR(c, L_lt2);
		switch(c) {
		case '=': return m1::m1Parser::token::SHIFTL_ASSIGN;
		default: putbackChar(c); return m1::m1Parser::token::SHIFTL_OP;
		}
	    default: putbackChar(c); return '<';
	    }
	case '>':
	    // >>= | >> | >= | > 
	    GET_CHAR(c, L_gt1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::GE_OP;
	    case '>':
		GET_CHAR(c, L_gt2);
		switch(c) {
		case '=': return m1::m1Parser::token::SHIFTR_ASSIGN;
		default: putbackChar(c); return m1::m1Parser::token::SHIFTR_OP;
		}
	    default: putbackChar(c); return '>';
	    }
	case '+':
	    // +=  | +
	    GET_CHAR(c, L_plus1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::ADD_ASSIGN;
	    case '+': return m1::m1Parser::token::INC_OP;
	    default: putbackChar(c); return '+';
	    }
	case '-':
	    // -= | -
	    GET_CHAR(c, L_minus1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::SUB_ASSIGN;
	    case '-': return m1::m1Parser::token::DEC_OP;
	    default: putbackChar(c); return '-';
	    }
	case '*':
	    // *= | *
	    GET_CHAR(c, L_multiply1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::MUL_ASSIGN;
	    case '*': return m1::m1Parser::token::SUPRESS_OP;
	    default: putbackChar(c); return '*';
	    }
	case '/':
	    //  // | /* | /= | /
	    GET_CHAR(c, L_divide1);
	    switch(c) {
	    case '/': 
		do {
		    GET_CHAR(c, L_comment1);
		} while((c != '\n') && (c != LEX_EOF));
		// return M1LINE_COMMENT  (FIX comment mode)
		break;

	    case '*':
		do {
		    GET_CHAR(c, L_comment2);
		    if (c == '*') {
			int c1;
			GET_CHAR(c1, L_comment3);
			if (c1 == '/')
			    break; // return M1BLOCK_COMMENT (FIX comment mode)
			putbackChar(c1);
		    }
		} while(c != LEX_EOF);
		// FIXME: signal eof in comment
		break;

	    case '=': return m1::m1Parser::token::DIV_ASSIGN;
	    default:  putbackChar(c); return '/';
	    }
	    break;
	case '%':
	    // %= | %
	    // *= | *
	    GET_CHAR(c, L_rem1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::REM_ASSIGN;
	    default: putbackChar(c); return '%';
	    }
	case '&':
	    // '&=' | '&&' | '&'
	    GET_CHAR(c, L_amp1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::AND_ASSIGN;
	    case '&': return m1::m1Parser::token::AND_OP;
	    default: putbackChar(c); return '&';
	    }
	case '|':
	    // '|=' | '||' | '|'
	    GET_CHAR(c, L_or1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::OR_ASSIGN;
	    case '|': return m1::m1Parser::token::OR_OP;
	    default: putbackChar(c); return '|';
	    }
	case '^':
	    // '^=' | '|'
	    GET_CHAR(c, L_xor1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::XOR_ASSIGN;
	    default: putbackChar(c); return '^';
	    }
	case '=':
	    // '==' | '=' | '=:=' | '=!='
	    GET_CHAR(c, L_eq1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::EQ_OP;
	    case '!':
		GET_CHAR(c, L_eq2);
		switch(c) {
		case '=': return m1::m1Parser::token::NQL_OP;
		default:
		    putbackChar('!');
		    putbackChar('=');
		    return '=';
		}
	    case ':':
		GET_CHAR(c, L_eq3);
		switch(c) {
		case '=': return m1::m1Parser::token::EQL_OP;
		default: 
		    putbackChar(':');
		    putbackChar('=');
		    return '=';
		}
	    default: putbackChar(c); return '=';
	    }
	case ':':
	    GET_CHAR(c, L_color1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::COPY_ASSIGN;
	    default: 
		putbackChar(c);
		return ':';
	    }
	case '.':
	    GET_CHAR(c, L_dot1);
	    switch(c) {
	    case '.': return m1::m1Parser::token::RANGE;  // soon deprecated
	    default:
		if (lex_lc[c] & LC_DIGIT)
		    goto fraction;
		putbackChar(c); 
		return '.';
	    }

	case '!':
	    GET_CHAR(c, L_excl1);
	    switch(c) {
	    case '=': return m1::m1Parser::token::NE_OP;
	    default: putbackChar(c); return '!';
	    }
	case ',':  return ',';
	case ';':  return ';';
	case '{':  return '{';
	case '}':  return '}';
	case '[':  return '[';
	case ']':  return ']';
	case '(':  return '(';
	case ')':  return ')';
	case '~':  return '~';
	case '?':  return '?';
	case '@':  return '@';
	default:
	    if ((lex_lc[c] & LC_LOWER)  || (c == '_')) {
		// scan id {L} - ({A}|{D})*  and keywords
		while(1) {
		    GET_CHAR(c, L_id1);
		    if (!(lex_lc[c] & (LC_ALPHA|LC_DIGIT)) && (c != '_')) {
			putbackChar(c);
			return m1_keyword(tokenText(), tokenLength());
		    }
		}
	    }

	    if ((lex_lc[c] & LC_UPPER)) { 
		// scan type_id {U} - ({A}|{D})*
		while(1) {
		    GET_CHAR(c, L_tid1);
		    if (!(lex_lc[c] & (LC_ALPHA|LC_DIGIT)) && (c != '_')) {
			putbackChar(c);
			return m1::m1Parser::token::M1TIDENTIFIER;
		    }
		}
	    }

	    if (lex_lc[c] & LC_DIGIT) {
		// scan number
		// 0[xX]{H}+{IS}?	    M1INTEGER
		// 0{D}+{IS}?	            M1INTEGER
		// {D}+{IS}?		    M1INTEGER
		// {D}+{E}{FS}?		    M1FLOAT
		// {D}*"."{D}+({E})?{FS}?   M1FLOAT
		// {D}+"."{D}*({E})?{FS}?   M1FLOAT
		//
		// E  = [Ee][+-]?{D}+
		// FS = (f|F|l|L)
		// IS = (u|U|l|L)*
		//
		if ((c=peekChar()) == '0') {
		    GET_CHAR(c, L_num1);
		    if ((c == 'x') || (c == 'X')) {
			int x;
			GET_CHAR(x, L_xnum1);
			if (!(lex_lc[x] & LC_XDIGIT)) {
			    putbackChar(x);
			    // FIXME or 'X' (restarted L_xnum1 x = undefined)
			    putbackChar('x');
			    // return only the '0'
			    return m1::m1Parser::token::M1INTEGER;
			}
			while(lex_lc[x] & LC_XDIGIT) {
			    GET_CHAR(x, L_xnum2);
			}
			while ((x == 'u') || (x == 'U') || 
			       (x == 'l') || (x == 'L')) {
			    GET_CHAR(x, L_xnum3);
			}
			putbackChar(x);
			return m1::m1Parser::token::M1INTEGER;
		    }
		}

		while(lex_lc[c] & LC_DIGIT) {
		    GET_CHAR(c, L_num2);
		}
		while ((c == 'u') || (c == 'U') ||
		       (c == 'l') || (c == 'L')) {
		    GET_CHAR(c, L_num3);
		}
		switch(c) {
		case '.':
		case 'E':
		case 'e':
		    goto fraction;
		default:
		    putbackChar(c);
		    return m1::m1Parser::token::M1INTEGER;
		}
	    }

	    fprintf(stderr, "char '%c' not recogised\n", c);
	    break;

	fraction:
		//
		// After reading '.' | 'E' | 'e'
		// . - {D}+({E})?{FS}?   M1FLOAT
		//
		if (((c = peekChar()) == 'E') || (c == 'e'))
		    goto exponent_part;
		GET_CHAR(c, L_frac1);
		while(lex_lc[c] & LC_DIGIT) {
		    GET_CHAR(c, L_frac2);
		}
		switch(c) {
		case 'e':
		case 'E':
		    goto exponent_part;
		case 'f':
		case 'F':
		case 'l':
		case 'L':
		    break;
		default:
		    putbackChar(c);
		}
		return m1::m1Parser::token::M1FLOAT;

	    exponent_part:
		GET_CHAR(c, L_exp1);
		if ((c == '+') || (c == '-')) {
		    GET_CHAR(c, L_exp2);
		}
		while(lex_lc[c] & LC_DIGIT) {
		    GET_CHAR(c, L_exp3);
		}
		switch(c) {
		case 'f':
		case 'F':
		case 'l':
		case 'L':
		    break;
		default:
		    putbackChar(c);
		}
		return m1::m1Parser::token::M1FLOAT;
	}
    }
    return c;
}
