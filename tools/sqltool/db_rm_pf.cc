//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Remove a packfile from 
//
#include "sqlcommon.hh"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

char *glob_argv0;

void usage(void)
{
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database packet_id\n", glob_argv0);
    fputs("-u db_user       Specifies the database user to login to MySQL with\n", stderr);
    fputs("-p db_password   Specifies the password for db_user\n", stderr);
    fputs("-h db_host       Specifies the MuSQL host address\n", stderr);
    fputs("-d database      Specifies the database to use on db_host\n", stderr);
    fputs("-i packet_id     The packet id, including version, to remove.\n", stderr);
    exit(255);
}

main(int argc, char *argv[]) 
{
    MYSQL *db = 0;
    string db_user;
    string db_passwd;
    string db_host;
    string db_database;
    string packid = "";
    char c;
    string acct;
    string prov;
    string pack;
    int maj;
    int min;
    int pat;
    CPackfileList lst;

    glob_argv0 = argv[0];

    while((c = getopt(argc, argv, "i:u:p:h:d:")) != -1) {
	switch (c) {
	case 'u':
	    db_user = optarg;
	    break;

	case 'p':
	    db_passwd = optarg;
	    break;

	case 'h':
	    db_host = optarg;
	    break;

	case 'd':
	    db_database = optarg;
	    break;

	case 'i':
	    if (!optarg) usage();
	    packid = optarg;
	    break;

	case '?':

	default:
	    usage();
	}
    }

    if (packid == "") {
	fprintf(stderr, "No -i packet_id option given\n");
	usage();
	exit(255);
    }

    // Make sure we get full packet id specification including version.
    if (!extract_packet_id(packid, acct, prov, pack, maj, min, pat)) {
	printf("Malformed packet id -i [%s]. Should be user@provider/packet_name[/major.minor.patch]\n", packid.c_str());
	exit(255);
    }

    //
    // Connect to database
    //
    if (!db_connect(&db, db_host.c_str(),  db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {
	puts("Could not connect to database");
	exit(255);
    }

    // Locate packet
    if (!db_find_packfile(db, packid, lst)) {
	printf("packet [%s] not found in database\n", packid.c_str());
	exit(255);
    }
    
    if (lst.size() > 1) {
	printf("Found %d matches. Using first\n", lst.size());
    }

    while(lst.begin() != lst.end()) {
	db_delete_packfile(db, lst.front());
	lst.pop_front();
    }
    
    mysql_close(db);

    exit(0);
}

