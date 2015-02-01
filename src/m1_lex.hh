//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//

#ifndef __M1_LEX_HH__
#define __M1_LEX_HH__

#include <string>
#include "position.hh"
#include "location.hh"
using namespace std;
#include "bio_stream.hh"

// FIXME SYNC with parser !!!
#define LEX_MORE      255   // signal retry
#define LEX_EOF       256   // signal eof
#define LEX_ERROR     257   // signal error condition

#define MAX_CHAR_STACK  64
#define MAX_BUFFER_SIZE 1024
#define MAX_LINE_SIZE   1024
#define MAX_TOKEN_SIZE  1024
#define MAX_STRING_SIZE 1024

class M1Lex {
public:
    M1Lex(CBioStream* aBio, std::string* aFileName, size_t aMaxLength=0);
    ~M1Lex();

    // read next token
    int     getToken(void);
    int     peekToken(void);
    int     putbackToken(void);

    char*   tokenName(int token);
    unsigned int tokenLength(void); // Length of current token
    char*   tokenText(void);     // current token chars
    string  tokenString(void);   // token text as C string

    void    resetToken(void) {
	if (!mContinue) {
	    mTokPtr = mTokBuffer; // reset token
	    mLocation.step();     // restart location
	}
    }

    int     getChar(void);
    int     peekChar(void)      { return mChar; }
    void    putbackChar(int c);

    m1::location& location(void)  { return mLocation; }
    string* fileName(void)        { return mLocation.begin.filename; }
    int     lineNumber(void)      { return mLocation.begin.line; }

    char*  unquote(char* s);    
private:
    CBioStream* mBio;
    size_t mMaxReadLength;
    size_t mReadLength;
    void*  mContinue;    // continuation

    // Input buffer
    char* mPtr;
    char* mPtrEnd;
    char  mBuffer[MAX_BUFFER_SIZE];

    // putback stack
    int   mChar;     // current char
    int   mCharStack[MAX_CHAR_STACK];
    int   mCharSp;

    // token buffer
    char* mTokPtr;
    char* mTokPtrEnd;
    char  mTokBuffer[MAX_TOKEN_SIZE];

    // token location
    m1::location mLocation;

    char mStringBuf[MAX_STRING_SIZE];
};

#endif
