//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Register an m1 unit in the database.
//
#include "sqlcommon.hh"
#ifndef DARWIN
#include <linux/hdreg.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

char *glob_argv0;

void usage(void)
{
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database -D disk -P part_nr\n\n", glob_argv0);
    fputs("-u db_user       Specifies the database user to login to MySQL with\n\n", stderr);
    fputs("-p db_password   Specifies the password for db_user\n\n", stderr);
    fputs("-h db_host       Specifies the MuSQL host address\n\n", stderr);
    fputs("-d database      Specifies the database to use on db_host\n\n", stderr);
    fputs("-D disk          The disk to read . \n\n", stderr);
    fputs("-P part_nr       The part number to build . \n\n", stderr);
    exit(255);
}

main(int argc, char *argv[]) 
{
    string serial;
    long serial_id;
    time_t lt;
    MYSQL *db = 0;
    string db_user;
    string db_passwd;
    string db_host;
    string db_database;
    char c;
#ifndef DARWIN
    struct hd_driveid id;
#endif
    int hd_des;
    char diskdev[256];
    char diskid[128];
    char cmd[256];
    char fname[256];
    int key_desc; // File descriptor for reading keys
    char *priv_key_buf;
    size_t priv_key_len;
    char *pub_key_buf;
    size_t pub_key_len;
    char *bin_key_buf;
    size_t bin_key_len;
    struct stat key_stat;
    int len;
    int out_des;
    int chld_stat;
    char pkeyname[128];
    string part_nr = "";

    glob_argv0 = argv[0];
    diskdev[0] = 0;
    
    while((c = getopt(argc, argv, "D:u:p:h:d:P:")) != -1) {
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

	case 'D':
	    strcpy(diskdev, optarg);;
	    break;


	case 'P':
	    part_nr = optarg;
	    break;


	case '?':

	default:
	    usage();
	}
    }

    if (part_nr == "") {
	fprintf(stderr, "No -P part_nr option given.\n");
	usage();
	exit(255);
    }
	
    if (!diskdev[0]) {
	fprintf(stderr, "No -D disk option given.\n");
	usage();
	exit(255);
    }

    // Temp testing workaround
    if (!strcmp(diskdev, "DEMO")) {
	diskdev[0] = 0;
	strcpy(diskid, "DEMO");
    }
    

#ifndef DARWIN
    if (diskdev[0] != 0) {
	if ((hd_des = open(diskdev, O_RDONLY)) == -1) {
	    fprintf(stderr, "Could not open disk [%s]: %s\n", strerror(errno));
	    exit(255);
	}

	if (ioctl(hd_des, HDIO_GET_IDENTITY, &id) == -1) {
	    perror("ioctl HDIO_GET_IDENTITY");
	    close(hd_des);
	    exit(255);
	}
	    
	close(hd_des);

	if (id.serial_no[0] == 0) {
	    fprintf(stderr, "Could not get disk identity from %s\n", diskdev);
	    exit(255);
	}

	// Protect against buf overflow.
	len = (sizeof(id.serial_no)>sizeof(diskid)-1)?sizeof(diskid)-1:sizeof(id.serial_no);

	// Copy.
	memcpy(diskid, id.serial_no, len); 
	diskid[len] = 0;
	//    printf("Disk ID: [%s]\n", diskid);
    }
#endif

    //
    // Connect to database
    //
    if (!db_connect(&db, db_host.c_str(),  db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {
	exit(255);
    }

    // Grab a serial number from db
    serial_id = db_assign_next_serial(db, serial);
    if (serial_id == 0) {
	puts("No more serial numbers available in database\n");
	mysql_close(db);
	while(1)
	    sleep(100000);
    }
       
    //
    // Dump the serial into a text file.
    //
    if ((out_des = creat("serial.txt", 0644)) == -1) {
	printf("Could not open serial.txt: %s\n", strerror(errno));
	mysql_close(db);
	exit(-1);
    }
    write(out_des, serial.c_str(), 6);
    write(out_des, "\n", 1);
    close(out_des);


    //
    // Dump the disk on module into a text file
    //
    if ((out_des = creat("dom_id.txt", 0644)) == -1) {
	printf("Could not open dom_id.txt: %s\n", strerror(errno));
	mysql_close(db);
	exit(-1);
    }
    write(out_des, diskid, strlen(diskid));
    write(out_des, "\n", 1);
    close(out_des);

    //
    // We have assigned a serial nr.
    // We have a disk ID.
    //

    //
    // Generate a device key.
    //
    if (!fork()) {
	execl("/bin/keytool", "/bin/keytool", "-name", serial.c_str(), "-b", "-u", "any", 0);
	fprintf(stderr, "Could not execute /bin/keytool -name %s -u any Error: %s\n", serial.c_str(), strerror(errno));
	exit(0);
    }
    wait(&chld_stat);

    //
    // Open private key and read it.
    //
    sprintf(fname, "%s.key", serial.c_str());
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


    //
    // Open public key and read it.
    //
    sprintf(fname, "%s.pub", serial.c_str());
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
    // Open binary key and read it.
    //
    sprintf(fname, "%s.bin", serial.c_str());
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

    // Add the m1 unit
    db_add_m1_unit(db, 
		   part_nr.c_str(),
		   serial.c_str(), 
		   priv_key_buf, 
		   priv_key_len, 
		   pub_key_buf, 
		   pub_key_len, 
		   bin_key_buf, 
		   bin_key_len, 
		   diskid, 
		   1L);

    mysql_close(db);
    exit(0);
}

