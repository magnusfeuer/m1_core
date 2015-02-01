//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

// ls utility for packfile

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
main(int argc, char *argv[])
{
    int ind = 1;
    char cont = false;
    // Usage packtst root installdb-dir packfile-dir
    if (argc < 1) {
	printf("Usage: %s packfile ...\n", argv[0]);
	exit(-1);
    }

    while(argv[ind]) {
	char buf[1024];
	FILE *in = fopen(argv[ind], "r");

	if (!in) {
	    perror(argv[ind]);
	    ++ind;
	    continue;
	}
	
	
	while(fgets(buf, 1024, in))
	    if (!strncmp(buf, "<m1-packfile-start>", 19)) {
		cont = true;
		break;
	    }

	if (!cont) {
	    fclose(in);
	    ++ind;
	    continue;
	}

	fgets(buf,1024,in); // Skip start marker
	while(fgets(buf, 1024, in) && strlen(buf) > 1) {
	    printf(buf);
	}
	fclose(in);
	++ind;
    }
    exit(0);
}
