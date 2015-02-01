//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Generate serial numbers
//
#include "sqlcommon.hh"
#include <sys/time.h>
#include <time.h>

char *glob_argv0;

void usage(void)
{
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database -n serial_count\n\n", glob_argv0);
    fputs("-u db_user                Specifies the database user to login to MySQL with\n\n", stderr);
    fputs("-p db_password            Specifies the password for db_user\n\n", stderr);
    fputs("-h db_host                Specifies the MuSQL host address\n\n", stderr);
    fputs("-d database               Specifies the database to use on db_host\n\n", stderr);
    fputs("-n serial_count           Number of serial numbers to generate. \n\n", stderr);
    exit(255);
}

char *tobase(unsigned long num, int base, char *res)
{
    char chars[] = "0123456789ABCDEFGHJKLMNPQRSTUVWXYZ";
    char tmp[128];
    unsigned long r;
    int ind = 0;
    int len;
    int off;

    tmp[0] = 0;
    
    while(num >= base)
    {
        r = num % base;
	tmp[ind++] = chars[r];
        num = num / base;
    }

    tmp[ind] = chars[num];

    strcpy(res, "000000");

    // We now need to reverse the number.
    len = 0;
    off = 5-ind;
    while(len <= ind) {
	res[len+off] = tmp[ind - len];
	++len;
    }

    res[len+off] = 0;
    return res;
}

unsigned long calc_checksum(unsigned long aSerial)
{
    int i = 9;
    unsigned long csum = 0L;

    while(i--) {
	csum += (aSerial % 10L);
	aSerial /= 10;
    }

    return csum % 10L;
}

main(int argc, char *argv[]) 
{
    char serial[80];
    time_t lt;
    MYSQL *db = 0;
    string db_user;
    string db_passwd;
    string db_host;
    string db_database;
    int count = 0;
    char c;
    struct timeval tv;
    glob_argv0 = argv[0];
    int batch;

    while((c = getopt(argc, argv, "u:p:h:d:n:")) != -1) {
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

	case 'n':
	    if (!optarg) usage();
	    count = atoi(optarg);
	    break;


	case '?':

	default:
	    usage();
	}
    }

    if (count == 0) {
	usage();
    }
       
   //
    // Connect to database
    //
    if (!db_connect(&db, db_host.c_str(),  db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {
	exit(255);
    }

    
    gettimeofday(&tv, 0);
    srand(tv.tv_usec ^ tv.tv_sec);

    // Get highest batch.
    if (!db_get_max_serial_batch(db, batch)) {
	exit(255);
    }

    batch++;
    printf("This will be batch %d\n", batch);
    //
    // Grab a serial number
    //
    while(count--) {
	unsigned long raw_serial = rand()%211631615L;
	unsigned long csum = calc_checksum(raw_serial);
	char serial[7];
	long id;
	time_t assigned;
	    

	raw_serial *= 10;
	raw_serial += csum;
	tobase(raw_serial, 34, serial);

	// Check if serial already exists.
	if (db_get_serial(db, serial, id, assigned)) {
	    printf("Serial: [%s] already exists\n", serial);
	    count++;
	    continue;
	} 
	if (!db_add_serial(db, serial, 0, batch))
	    exit(255);
    }
    mysql_close(db);
    exit(0);
}
