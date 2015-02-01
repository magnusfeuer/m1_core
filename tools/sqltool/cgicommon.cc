//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2008.
//
#include "cgicommon.hh"
#include "sqlcommon.hh"

void emit_result(FILE *log, CKeyValueList &list) 
{
    printf("Content-type: text/html\r\n\r\n<HTML><HEAD>\r\n</HEAD><BODY>\r\nerror: 0\r\n");
    if (log)
	fprintf(log, "%lu: Error:     [success]\n", time_stamp());
    for(CKeyValueListIterator iter = list.begin(); iter != list.end(); ++iter) {
	printf("%s: %s\r\n", iter->mKey.c_str(), iter->mValue.c_str());
	if (log) 
	    fprintf(log, "Result field: [%s]=[%s]\n", iter->mKey.c_str(), iter->mValue.c_str());
    }
    puts("</BODY></HTML>\r");
}


void emit_error(FILE *log, int error, string text) 
{
    if (text != "")
	printf("Content-type: text/html\r\n\r\n<HTML><HEAD>\r\n</HEAD><BODY>\r\nerror: %d\r\ntext: %s\r\n</BODY></HTML>\r\n", 
	       error, text.c_str());
    else
	printf("Content-type: text/html\r\n\r\n<HTML><HEAD>\r\n</HEAD><BODY>\r\nerror: %d\r\n</BODY></HTML>\r\n", error);

    if (log) {
	fprintf(log, "%lu: Error:     [%d]\n", time_stamp(), error);
	fprintf(log, "%lu: ErrorText: [%s]\n", time_stamp(), text.c_str());
    }
}


void log_query_fields(FILE *out, CKeyValueList &lst)
{
    for(CKeyValueListIterator iter = lst.begin(); 
	iter != lst.end();
	++iter) {
	fprintf(out, "Field [%s]: [%s]\n", iter->mKey.c_str(), iter->mValue.c_str());
    }
    
}

void decode_query_fields(CKeyValueList &lst) 
{
    for(CKeyValueListIterator iter = lst.begin(); 
	iter != lst.end();
	++iter) {

	string res = "";
	int len = iter->mKey.size();

	for(int ind=0; ind < len; ++ind) {
	    switch(iter->mKey[ind]) {
	    case '+':
		res += ' ';
		break;

	    case '%':
		// Check that we have two characters.
		if (len >= ind + 2) {
		    res += (char) strtol(iter->mKey.substr(ind+1,2).c_str(), 0, 16);
		    ind += 2;
		} else {
		    res += '%';
		}
		break;
	    default:
		res += iter->mKey[ind];
	    }
	}
	iter->mKey = res;

	// Do the same for value.
	res = "";
	len = iter->mValue.size();

	for(int ind=0; ind < len; ++ind) {
	    switch(iter->mValue[ind]) {
	    case '+':
		res += ' ';
		break;

	    case '%':
		// Check that we have two characters.
		if (len >= ind + 2) {
		    res += (char) strtol(iter->mValue.substr(ind+1,2).c_str(), 0, 16);
		    ind += 2;
		} else {
		    res += '%';
		}
		break;
	    default:
		res += iter->mValue[ind];
	    }
	}
	iter->mValue = res;
    }

    return;
}


char *read_query_field(int aDescriptor, int aLength)
{
    int read_bytes = 0;
    char *res;

    if (aLength > 0xFFFF) // Sanity check.
	return 0;

    res = (char *) malloc(aLength + 1);
    while(read_bytes < aLength) {
	int len = read(aDescriptor, res + read_bytes, aLength - read_bytes);

	if (len <= 0) {
	    free(res);
	    return 0;
	}
	read_bytes += len;
    }

    // 
    //  Trim.
    //
    res[aLength] = 0;
    while(res[aLength-1] == '\r' ||
	  res[aLength-1] == '\n' ||
	  res[aLength-1] == ' ' ||
	  res[aLength-1] == '\t') {
	aLength--;
	res[aLength] = 0;
    }
	
    return res;
}


// Field should be decoded.
int parse_query_field(string fld, CKeyValueList &res)
{
    CStringList fld_lst;
    string::size_type field_ind = 0;
    string::size_type next_ind = 0;
    string::size_type tmp_ind = 0;
    int fld_len = fld.size();
    int len = fld.size();
    int kvp_count = 0;
    string field;

    field = fld;
    // Locate all & characters.
    while(field_ind < len && (next_ind = field.find('&', field_ind + tmp_ind)) != string::npos) {

	// We have a new field.
	if (next_ind - field_ind > 0) {
	    fld_lst.push_back(field.substr(field_ind, next_ind - field_ind));
	}
	field_ind = next_ind + 1;
    }

    if (field_ind < len) {
	string x;
	x = field.substr(field_ind);
	fld_lst.push_back(x);
    }

    //
    // We now have a number of fields. Divide this into key/value pairs.
    //
    for(CStringListIterator iter = fld_lst.begin();
	iter != fld_lst.end();
	++iter) {

	// Locate first '=', ignore subsequence '='
	// If nothing is found. Install key with empty value
	if ((field_ind = iter->find('=')) == string::npos) {
	    res.push_back(CKeyValue(*iter, ""));
	    ++kvp_count;
	    continue;
	}

	// Ignore entries with no key name
	if (field_ind == 0) 
	    continue;

	res.push_back(CKeyValue(iter->substr(0, field_ind) , iter->substr(field_ind+1)));	
	kvp_count++;
    }
    
    return kvp_count;
}
