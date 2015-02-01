//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Add a single key to the M1.
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
    fputs("-k keyname       The file names containing key. The files keyname.[bin,pub,key] will be opened.\n", stderr);
    fputs("-D               This is a device key. \n", stderr);
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
    struct stat key_stat;
    char *priv_key_buf;
    size_t priv_key_len;
    char *pub_key_buf;
    size_t pub_key_len;
    char *bin_key_buf;
    size_t bin_key_len;
    char c;
    bool dev_key = false;

    glob_argv0 = argv[0];
    fname[0] = 0;
    name[0] = 0;
    while((c = getopt(argc, argv, "k:u:p:h:d:D")) != -1) {
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

	case 'D':
	    dev_key = true;
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
    if (!db_connect(&db, db_host.c_str(), db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {
	puts("Could not connect to database");
	exit(255);
    }

    //
    // Open private key and read it.
    //
    sprintf(fname, "%s.key", name);
    if ((key_desc = open(fname, O_RDONLY)) == -1) {
	printf("Could not open %s: %s\n", fname, strerror(errno));
	mysql_close(db);
	exit(-1);
    }

    if (fstat(key_desc, &key_stat) == -1) {
	printf("Could not stat %s: %s\n", fname, strerror(errno));
	close(key_desc);
	mysql_close(db);
	exit(-1);
    }

    priv_key_len = key_stat.st_size;
    priv_key_buf = (char *) malloc(priv_key_len + 1);

    if(read(key_desc, priv_key_buf, priv_key_len) != priv_key_len) {
	printf("Could not read %s: %s\n", fname, strerror(errno));
	close(key_desc);
	mysql_close(db);
	exit(-1);
    }

    close(key_desc);



    //
    // Open public key and read it.
    //
    sprintf(fname, "%s.pub", name);
    if ((key_desc = open(fname, O_RDONLY)) == -1) {
	printf("Could not open %s: %s\n", fname, strerror(errno));
	mysql_close(db);
	exit(-1);
    }

    if (fstat(key_desc, &key_stat) == -1) {
	printf("Could not stat %s: %s\n", fname, strerror(errno));
	close(key_desc);
	mysql_close(db);
	exit(-1);
    }

    pub_key_len = key_stat.st_size;
    pub_key_buf = (char *) malloc(pub_key_len + 1);

    if(read(key_desc, pub_key_buf, pub_key_len) != pub_key_len) {
	printf("Could not read %s: %s\n", fname, strerror(errno));
	close(key_desc);
	mysql_close(db);
	exit(-1);
    }

    close(key_desc);


    //
    // Open bin key and read it.
    //
    sprintf(fname, "%s.bin", name);
    if ((key_desc = open(fname, O_RDONLY)) == -1) {
	printf("Could not open %s: %s\n", fname, strerror(errno));
	mysql_close(db);
	exit(-1);
    }

    if (fstat(key_desc, &key_stat) == -1) {
	printf("Could not stat %s: %s\n", fname, strerror(errno));
	close(key_desc);
	mysql_close(db);
	exit(-1);
    }

    bin_key_len = key_stat.st_size;
    bin_key_buf = (char *) malloc(bin_key_len + 1);

    if(read(key_desc, bin_key_buf, bin_key_len) != bin_key_len) {
	printf("Could not read %s: %s\n", fname, strerror(errno));
	close(key_desc);
	mysql_close(db);
	exit(-1);
    }

    close(key_desc);

    // Get the lots currently used.
    if (!db_add_encryption_key(db, name, dev_key, 
			       priv_key_buf, priv_key_len, 
			       pub_key_buf, pub_key_len,
			       bin_key_buf, bin_key_len)) {
	printf("Could not add key %s\n", name);
	mysql_close(db);
	exit(-1);
    }

    mysql_close(db);

    exit(0);
}

