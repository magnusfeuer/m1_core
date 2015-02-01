//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Retrieve a single key (.pub and .key) into the file system.
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
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database -k key_name\n", glob_argv0);
    fputs("-u db_user       Specifies the database user to login to MySQL with\n", stderr);
    fputs("-p db_password   Specifies the password for db_user\n", stderr);
    fputs("-h db_host       Specifies the MuSQL host address\n", stderr);
    fputs("-d database      Specifies the database to use on db_host\n", stderr);
    fputs("-k keyname       The key name keyname.pub and keyname.key will be created from DB key. \n", stderr);
    fputs("-b               In addition to [key].pub and [key].key, also write [key].bin. \n", stderr);
    exit(255);
}

main(int argc, char *argv[]) 
{
    MYSQL *db = 0;
    string db_user;
    string db_passwd;
    string db_host;
    string db_database;
    char fname[256];
    char name[256];
    int key_desc;
    int len;
    char c;
    CKeyData key;
    char do_bin = 0;


    glob_argv0 = argv[0];
    fname[0] = 0;
    name[0] = 0;
    while((c = getopt(argc, argv, "k:u:p:h:d:b")) != -1) {
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

	case 'k':
	    if (!optarg) usage();
	    strcpy(name, optarg);
	    break;

	case 'b':
	    do_bin = 1;
	    break;

	case '?':

	default:
	    usage();
	}
    }

    if (!name[0]) {
	fprintf(stderr, "No -n keyuname option given\n");
	usage();
	exit(255);
    }

    //
    // Connect to database
    //
    if (!db_connect(&db, db_host.c_str(),  db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {
	puts("Could not connect to database");
	exit(255);
    }
    // Return trusted.enc_key.id
    if (!db_get_encryption_key(db, name, key)) {
	printf("Key [%s] not in database\n", name);
	exit(255);
    }

    //
    // Open private key and read it.
    //
    sprintf(fname, "%s.key", name);
    if ((key_desc = open(fname, O_WRONLY | O_CREAT, 0644)) == -1) {
	printf("Could not open %s: %s\n", fname, strerror(errno));
	mysql_close(db);
	exit(255);
    }
    if ((len = write(key_desc, key.mPrivKeyData, key.mPrivKeyDataLength)) != key.mPrivKeyDataLength) {
	printf("Could not write to %s. Wanted to write %d bytes, wrote %d\n",
	       fname, key.mPrivKeyDataLength, len);
    }

    close(key_desc);

    sprintf(fname, "%s.pub", name);
    if ((key_desc = open(fname, O_WRONLY | O_CREAT, 0644)) == -1) {
	printf("Could not open %s: %s\n", fname, strerror(errno));
	mysql_close(db);
	exit(255);
    }
    if ((len = write(key_desc, key.mPubKeyData, key.mPubKeyDataLength)) != key.mPubKeyDataLength) {
	printf("Could not write to %s. Wanted to write %d bytes, wrote %d\n",
	       fname, key.mPubKeyDataLength, len);
    }

    close(key_desc);

    if (do_bin) {
	sprintf(fname, "%s.bin", name);
	if ((key_desc = open(fname, O_WRONLY | O_CREAT, 0644)) == -1) {
	    printf("Could not open %s: %s\n", fname, strerror(errno));
	    mysql_close(db);
	    exit(255);
	}
	if ((len = write(key_desc, key.mBinKeyData, key.mBinKeyDataLength)) != key.mBinKeyDataLength) {
	    printf("Could not write to %s. Wanted to write %d bytes, wrote %d\n",
		   fname, key.mBinKeyDataLength, len);
	}

	close(key_desc);
    }
    mysql_close(db);

    exit(0);
}

