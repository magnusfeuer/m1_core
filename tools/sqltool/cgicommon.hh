//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2008.
//

#include <string>
#include <list>

#define TRUSTED_DB "magdenus_trusted"
#define TRUSTED_USER "magdenus_trusted"
#define TRUSTED_PASSWD "phuc49aphu"
#define TRUSTED_HOST "127.0.0.1"

#define UNTRUSTED_DB "magdenus_untrusted"
#define UNTRUSTED_USER "magdenus_untrust"
#define UNTRUSTED_PASSWD "devuzuze9e"
#define UNTRUSTED_HOST "127.0.0.1:3310"

#define LOGFILE "/srv/www/cgi_log.txt"
#define BINDIR "/srv/www/priv_bin/"

// #define REMOTE_ADDR "74.86.82.139" // Set to 0 to accept cgi calls from any remote host.
#define REMOTE_ADDR 0 // Set to 0 to accept cgi calls from any remote host.
using namespace std;


typedef list<string> CStringList;
typedef list<string>::iterator CStringListIterator;

typedef list<int> CIntList;
typedef list<int>::iterator CIntListIterator;

struct CKeyValue {
    CKeyValue(const CKeyValue &aSource):
	mKey(aSource.mKey),
	mValue(aSource.mValue) {}
    CKeyValue(const string &aKey, const string &aValue):
	mKey(aKey),
	mValue(aValue) {}

    string mKey;
    string mValue;
};


class CKeyValueList: public list<CKeyValue> {
public:
    CKeyValueList(void) {}

    int find(const string &aKey, CStringList &aResult) {
	int res = 0;
	for (iterator iter = begin(); iter != end(); ++iter)
	    if ((*iter).mKey == aKey) {
		aResult.push_back((*iter).mValue);
		++res;
	    }
	return res;
    }
    string find(const string &aKey) {
	for (iterator iter = begin(); iter != end(); ++iter)
	    if ((*iter).mKey == aKey) {
		return (*iter).mValue;
	    }
	return string();
    }

    bool exists(const string &aKey) {
	for (iterator iter = begin(); iter != end(); ++iter)
	    if (iter->mKey == aKey) {
		return true;
	    }

	return false;
    }
private:
};    

typedef CKeyValueList::iterator CKeyValueListIterator;

extern void emit_result(FILE *log, CKeyValueList &list);
extern void emit_error(FILE *log, int error, string text);
extern void decode_query_fields(CKeyValueList &aList);
extern void log_query_fields(FILE *out, CKeyValueList &aList);
extern int parse_query_field(string field, CKeyValueList &res);
extern char *read_query_field(int aDescriptor, int aLength);
