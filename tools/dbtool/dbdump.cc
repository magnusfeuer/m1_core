//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007, 2008.
//

//
// Patch file
//
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include "database.hh"

int main(int argc, char *argv[])
{
    CDatabase db;
    CDatabase::CElementMapIterator iter;

    // Usage packtst root installdb-dir packfile-dir
    if (argc < 2) {
	printf("Usage: %s dbfile\n", argv[0]);
	exit(-1);
    }

    db.device(argv[1]);
    if (!db.open()) {
	fprintf(stderr, "Could not open %s as a database\n", argv[1]);
	exit(-1);
    }

    if (!db.load()) {
	fprintf(stderr, "Could open but not load %s.\n", argv[1]);
	exit(-1);
    }
    iter = db.begin();
    while(iter != db.end()) {
	switch(iter->second.mElementType) {
	case CDatabase::StringElement:
	    printf("string [%s] %s\n", iter->first.c_str(), iter->second.mStringValue.c_str());
	    break;

	case CDatabase::FloatElement:
	    printf("float  [%s]: %f\n", iter->first.c_str(), iter->second.mFloatValue);
	    break;

	case CDatabase::IntElement:
	    printf("int    [%s]: %d\n", iter->first.c_str(), iter->second.mIntValue);
	    break;
	}
	++iter;
    }
    db.close();
    exit(0);
}
