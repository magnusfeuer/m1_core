/* C++ code produced by gperf version 3.0.4 */
/* Command-line: gperf -t -LC++ m1_keywords  */
/* Computed positions: -k'1-2' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 1 "m1_keywords"
struct keyword { char* name; int token; };

#define TOTAL_KEYWORDS 41
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 11
#define MIN_HASH_VALUE 2
#define MAX_HASH_VALUE 70
/* maximum key range = 69, duplicates = 0 */

class Perfect_Hash
{
private:
  static inline unsigned int hash (const char *str, unsigned int len);
public:
  static struct keyword *in_word_set (const char *str, unsigned int len);
};

inline unsigned int
Perfect_Hash::hash (register const char *str, register unsigned int len)
{
  static unsigned char asso_values[] =
    {
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71,  5, 20,  0,
      15, 10,  0,  5, 71,  0, 35, 71,  0, 50,
       0,  0,  5, 15, 15,  5, 30, 40,  0, 55,
       5, 71, 20, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
      71, 71, 71, 71, 71, 71, 71
    };
  return len + asso_values[(unsigned char)str[1]+1] + asso_values[(unsigned char)str[0]];
}

struct keyword *
Perfect_Hash::in_word_set (register const char *str, register unsigned int len)
{
  static struct keyword wordlist[] =
    {
      {""}, {""},
#line 40 "m1_keywords"
      {"in", m1::m1Parser::token::IN},
#line 22 "m1_keywords"
      {"int", m1::m1Parser::token::INT },
#line 20 "m1_keywords"
      {"char", m1::m1Parser::token::CHAR },
#line 13 "m1_keywords"
      {"input", m1::m1Parser::token::INPUT },
#line 14 "m1_keywords"
      {"output", m1::m1Parser::token::OUTPUT },
#line 36 "m1_keywords"
      {"if", m1::m1Parser::token::IF},
      {""},
#line 11 "m1_keywords"
      {"interface", m1::m1Parser::token::INTERFACE },
#line 28 "m1_keywords"
      {"const", m1::m1Parser::token::CONST},
#line 16 "m1_keywords"
      {"public", m1::m1Parser::token::PUBLIC },
#line 39 "m1_keywords"
      {"foreach", m1::m1Parser::token::FOREACH},
#line 41 "m1_keywords"
      {"continue", m1::m1Parser::token::CONTINUE},
#line 29 "m1_keywords"
      {"enum", m1::m1Parser::token::ENUM },
#line 19 "m1_keywords"
      {"persistent", m1::m1Parser::token::PERSISTENT },
#line 38 "m1_keywords"
      {"switch", m1::m1Parser::token::SWITCH},
#line 17 "m1_keywords"
      {"private", m1::m1Parser::token::PRIVATE },
      {""},
#line 18 "m1_keywords"
      {"protected", m1::m1Parser::token::PROTECTED },
#line 15 "m1_keywords"
      {"queue", m1::m1Parser::token::QUEUE },
#line 43 "m1_keywords"
      {"return", m1::m1Parser::token::RETURN},
#line 35 "m1_keywords"
      {"default", m1::m1Parser::token::DEFAULT},
      {""},
#line 34 "m1_keywords"
      {"case", m1::m1Parser::token::CASE},
#line 6 "m1_keywords"
      {"false", m1::m1Parser::token::M1FALSE},
#line 32 "m1_keywords"
      {"script", m1::m1Parser::token::SCRIPT},
      {""}, {""},
#line 26 "m1_keywords"
      {"bool", m1::m1Parser::token::BOOL },
#line 42 "m1_keywords"
      {"break", m1::m1Parser::token::BREAK},
#line 10 "m1_keywords"
      {"application", m1::m1Parser::token::APPLICATION },
      {""}, {""},
#line 4 "m1_keywords"
      {"this", m1::m1Parser::token::M1THIS },
      {""}, {""}, {""},
#line 3 "m1_keywords"
      {"nil", m1::m1Parser::token::M1NIL},
#line 5 "m1_keywords"
      {"true", m1::m1Parser::token::M1TRUE },
      {""}, {""},
#line 9 "m1_keywords"
      {"library", m1::m1Parser::token::LIBRARY },
      {""},
#line 21 "m1_keywords"
      {"byte", m1::m1Parser::token::BYTE },
      {""},
#line 23 "m1_keywords"
      {"signed", m1::m1Parser::token::SIGNED },
      {""},
#line 24 "m1_keywords"
      {"unsigned", m1::m1Parser::token::UNSIGNED },
#line 33 "m1_keywords"
      {"step", m1::m1Parser::token::STEP},
      {""},
#line 27 "m1_keywords"
      {"string", m1::m1Parser::token::STRING},
      {""}, {""},
#line 7 "m1_keywords"
      {"type", m1::m1Parser::token::TYPE },
#line 25 "m1_keywords"
      {"float", m1::m1Parser::token::FLOAT },
      {""},
#line 8 "m1_keywords"
      {"typedef", m1::m1Parser::token::TYPEDEF },
      {""},
#line 31 "m1_keywords"
      {"when", m1::m1Parser::token::WHEN },
      {""}, {""}, {""}, {""},
#line 37 "m1_keywords"
      {"else", m1::m1Parser::token::ELSE},
      {""}, {""}, {""}, {""},
#line 30 "m1_keywords"
      {"time", m1::m1Parser::token::TIME },
#line 12 "m1_keywords"
      {"event", m1::m1Parser::token::EVENT }
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
