//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Download content for a given unit.
//
#include "sqlcommon.hh"
#include "cgicommon.hh"

char *glob_argv0;
extern void usage(void) {}

main(int argc, char *argv[]) 
{
    MYSQL *db_t = 0;
    MYSQL *db_u = 0;
    CKeyValueList kvp_lst;
    CKeyValueList result;
    string email;
    char *env;
    int ind = 0;
    char *query;
    int query_len;
    char *mand_field[] = { // Matches both QUERY_STRING and trusted.account
	"email",
	"password",
	"package",
	"serial",
    };
    CPackfileList pack_lst;

    FILE *log;
    time_t now = time(0);

    stderr = log = fopen(LOGFILE, "a");
    if (!log)  {
	emit_error(0, 9, "Could not open log file");
	exit(0);
    }
    
    fprintf(log, "%lu: Log start: %s %s", time_stamp(), argv[0], ctime(&now));
    fprintf(log, "%lu: Trusted host[%s] db[%s] user[%s]\n", time_stamp(), TRUSTED_HOST, TRUSTED_DB, TRUSTED_USER);
    fprintf(log, "%lu: Untrusted host[%s] db[%s] user[%s]\n", time_stamp(), UNTRUSTED_HOST, UNTRUSTED_DB, UNTRUSTED_USER);
    fflush(log);

    //
    // Connect to database
    //
    fprintf(log, "%lu: Opening database %s.\n", time_stamp(), TRUSTED_DB);
    fflush(log);
    if (!db_connect(&db_t, TRUSTED_HOST,  TRUSTED_USER, TRUSTED_PASSWD, TRUSTED_DB)) {
	emit_error(log, 9, "Could not connect to database");
	db_t = 0;
	goto end;
    }

    fprintf(log, "%lu: Opening database %s.\n", time_stamp(), TRUSTED_DB);
    fflush(log);
    if (!db_connect(&db_u, UNTRUSTED_HOST,  UNTRUSTED_USER, UNTRUSTED_PASSWD, UNTRUSTED_DB)) {
	emit_error(log, 9, "Could not connect to database");
	db_u = 0;
	goto end;
    }
    fprintf(log, "%lu: Databases open.\n", time_stamp());
    fflush(log);
    
    if (!(env = getenv("REMOTE_ADDR")) || (REMOTE_ADDR != 0 && strcmp(env, REMOTE_ADDR))) {
 	emit_error(log, 10, "Permission denied.");
 	exit(0);
    }

    if (!(env = getenv("CONTENT_LENGTH"))) {
	emit_error(log, 10, "No CONTENT_LENGTH envuironment variable provided");
	goto end;
    }

    query_len = atoi(env);
    fprintf(log, "%lu: Reading query.\n", time_stamp()); 
    fflush(log);
   
    if (!(query = read_query_field(0, query_len))) {
	emit_error(log, 10, "Could not read input");
	goto end;
    }
	
    parse_query_field(query, kvp_lst);


    decode_query_fields(kvp_lst);
    fprintf(log, "\n");
    free(query);
    log_query_fields(log, kvp_lst);
    
    while(ind < sizeof(mand_field)/sizeof(mand_field[0])) {
	if (kvp_lst.find(mand_field[ind]) == "") {
	    char buf[129];
	    sprintf(buf, "Missing field %s", mand_field[ind]);
	    emit_error(0, 1, "Missing field");
	    goto end;
	}
	++ind;
    }
    //
    // Check that account does not exist
    //
    if (!db_find_account_by_email(db_t, kvp_lst.find("email"), &acc)) {
	fprintf(log, "%lu: Login incorrect - login\n", time_stamp());
	emit_error(log, 6, "Login incorrect");
	goto end;
    }
    fprintf(log, "%lu: Email located.\n", time_stamp());

    if (acc.mPassword != kvp_lst.find("password")) {
	fprintf(log, "%lu: Login incorrect - password\n", time_stamp());
	emit_error(log, 6, "Login incorrect");
	goto end;
    }
    
    //
    // Generate content
    //
    execl(BINDIR "/db2pf", BINDIR "/db2pf",  0);
    
    fflush(log);
    result.push_back(CKeyValue("text", "account created."));
    emit_result(log, result);

 end:
    if (db_t)
	mysql_close(db_t);

    if (db_u)
	mysql_close(db_u);
    
    fprintf(log, "%lu: ----------------------\n\n\n", time_stamp());
    fclose(log);
    exit(0);
}
