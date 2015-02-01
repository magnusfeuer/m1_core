//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

// ls utility for packfile

#include "packfile.hh"
#include <string.h>

main(int argc, char *argv[])
{
    CStringList content;
    CPacket dummy("x", "y", "z", 1, 1, 1); // We want addContent to dissect Content: lines
    CFileList c_lst;
    int ind = 1;
    char long_ls = 0;
    // Usage packtst root installdb-dir packfile-dir
    if (argc < 1) {
	printf("Usage: %s [-l] packfile ...\n", argv[0]);
	exit(-1);
    }

    if (!strcmp(argv[1], "-l")) {
	ind = 2;
	long_ls = 1;
    }

    while(argv[ind]) {
	CKVPairList lst;

	lst.load(argv[ind]);
	lst.find("Content", content);
	++ind;
    }
    dummy.addContent(content);
    
    c_lst = dummy.content();
    if (long_ls) {
	puts("file type    perm [size]      name [ -> target]");
    }
    while(c_lst.begin() != c_lst.end()) {
	if (!long_ls) {
	    puts(c_lst.front().path().c_str());
	    c_lst.pop_front();
	    continue;
	}

	// Long listing
	switch(c_lst.front().fileType().at(0)) {
	    case 'r':
		printf("file         0%.3o %10d %s\n", c_lst.front().permission(),  c_lst.front().contentSize(), c_lst.front().path().c_str());
		break;
		    
	    case 'l': 
		printf("hard link                    %s -> %s\n", 
		       c_lst.front().path().c_str(),
		       c_lst.front().fileType().c_str() + 1);
		break;

	    case 'b': 
		printf("block device 0%.3o            %s\n", 
		       c_lst.front().permission(),
		       c_lst.front().path().c_str());
		break;

	    case 'c': 
		printf("char device  0%.3o            %s\n", 
		       c_lst.front().permission(),
		       c_lst.front().path().c_str());
		break;

	    case 'f':
		printf("fifo         0%.3o            %s\n", c_lst.front().permission(), c_lst.front().path().c_str());
		break;

	    case 's':
		printf("socket       0%.3o            %s\n", c_lst.front().permission(), c_lst.front().path().c_str());
		break;

	    case 'S': 
		printf("symlink      0%.3o            %s -> %s\n", 
		       c_lst.front().permission(),
		       c_lst.front().path().c_str(),
		       c_lst.front().fileType().c_str() + 1);
		break;

	    case 'd': 
		printf("directory    0%.3o            %s\n", 
		       c_lst.front().permission(),
		       c_lst.front().path().c_str());
		break;
	    default:
		printf("????  0%.3o %10d %s %s\n", 
		       c_lst.front().permission(), 
		       c_lst.front().contentSize(), 
		       c_lst.front().path().c_str(),
		       c_lst.front().fileType().c_str());
	}
	c_lst.pop_front();
    }

    exit(0);
}
