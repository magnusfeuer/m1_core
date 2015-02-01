//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Create a new account. 
// This code is invoked as a CGI script.
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
    int query_len;
    char *query;
    char *mand_field[] = { // Matches both QUERY_STRING and trusted.account
	"email",
	"password",
    };
    CAccount acc;
    long build_order_id;
    long dev_key_id;
    string dom_serial;
    long owner_id;
    long enc_key_id;
    time_t man_date;
    CM1UnitList owned_m1;
    string ser;

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



    //
    // Connect to database
    //
    fprintf(log, "%lu: Opening trusted database %s.\n", time_stamp(), TRUSTED_DB);
    if (!db_connect(&db_t, TRUSTED_HOST,  TRUSTED_USER, TRUSTED_PASSWD, TRUSTED_DB)) {
	emit_error(log, 9, "Could not connect to database");
	db_t = 0;
	goto end;
    }

    fprintf(log, "%lu: Opening untrusted database %s.\n", time_stamp(), UNTRUSTED_DB);
    if (!db_connect(&db_u, UNTRUSTED_HOST,  UNTRUSTED_USER, UNTRUSTED_PASSWD, UNTRUSTED_DB)) {
	emit_error(log, 9, "Could not connect to database");
	db_u = 0;
	goto end; 
    }
    fprintf(log, "%lu: Databases open.\n", time_stamp());
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
    if (!(query = read_query_field(0, query_len))) {
	emit_error(log, 10, "Could not read input");
	goto end;
    }
	
    parse_query_field(query, kvp_lst);

    decode_query_fields(kvp_lst);
    log_query_fields(log, kvp_lst);
    fprintf(log, "\n");
    free(query);
    
    while(ind < sizeof(mand_field)/sizeof(mand_field[0])) {
	if (kvp_lst.find(mand_field[ind]) == "") {
	    char buf[129];
	    fprintf(log, "Missing field %s\n", mand_field[ind]);
	    emit_error(log, 1, "Missing field");
	    goto end;
	}
	++ind;
    }

    //
    // Check that account does exist
    //
    fprintf(log, "%lu: Locating account [%s].\n", time_stamp(), kvp_lst.find("email").c_str());
    if (!db_find_account_by_email(db_t, kvp_lst.find("email"), &acc)) {
	fprintf(log, "%lu: Login incorrect - email\n", time_stamp());
	emit_error(log, 6, "Login incorrect");
	goto end;
    }

    //
    // Check that account does exist
    //
    if (acc.mPassword != kvp_lst.find("password")) {
	fprintf(log, "%lu: Login incorrect - password\n", time_stamp());
	emit_error(log, 6, "Login incorrect");
	goto end;
    }


    // Validate all serial numbers
    if ((ser = kvp_lst.find("m1_serial1")) != "") {
	CM1Unit unit;

	if (ser[3] == '-' || ser[3] == ' ')  ser = ser.substr(0, 3) + ser.substr(4, 3); // Kill xxx-xxx

	fprintf(log, "%lu: 1- Retrieveing serial[%s].\n", time_stamp(), ser.c_str());
	if (!db_get_m1_unit(db_t, ser, unit) || (unit.mOwnerAccountID != 1 && unit.mOwnerAccountID != acc.mDbID)) {
	    emit_error(log, 7, string("Illegal serial number - ") + ser);
	    goto end;
	}
	owned_m1.push_back(unit);
    }


    if ((ser = kvp_lst.find("m1_serial2")) != "") {
	CM1Unit unit;

	if (ser[3] == '-' || ser[3] == ' ')  ser = ser.substr(0, 3) + ser.substr(4, 3); // Kill xxx-xxx

	fprintf(log, "%lu: 2 - Retrieveing serial[%s].\n", time_stamp(), ser.c_str());
	if (!db_get_m1_unit(db_t, ser, unit) || (unit.mOwnerAccountID != 1 && unit.mOwnerAccountID != acc.mDbID)) {
	    emit_error(log, 7, string("Illegal serial number - ") + ser);
	    goto end;
	}
	owned_m1.push_back(unit);
    }


    if ((ser = kvp_lst.find("m1_serial3")) != "") {
	CM1Unit unit;

	if (ser[3] == '-' || ser[3] == ' ')  ser = ser.substr(0, 3) + ser.substr(4, 3); // Kill xxx-xxx

	fprintf(log, "%lu: 3 - Retrieveing serial[%s].\n", time_stamp(), ser.c_str());
	if (!db_get_m1_unit(db_t, ser, unit) || (unit.mOwnerAccountID != 1 && unit.mOwnerAccountID != acc.mDbID)) {
	    emit_error(log, 7, string("Illegal serial number - ") + ser);
	    goto end;
	}
	owned_m1.push_back(unit);
    }


    if ((ser = kvp_lst.find("m1_serial4")) != "") {
	CM1Unit unit;

	if (ser[3] == '-' || ser[3] == ' ')  ser = ser.substr(0, 3) + ser.substr(4, 3); // Kill xxx-xxx

	fprintf(log, "%lu: 4 - Retrieveing serial[%s].\n", time_stamp(), ser.c_str());
	if (!db_get_m1_unit(db_t, ser, unit) || (unit.mOwnerAccountID != 1 && unit.mOwnerAccountID != acc.mDbID)) {
	    emit_error(log, 7, string("Illegal serial number - ") + ser);
	    goto end;
	}
	owned_m1.push_back(unit);
    }

    if ((ser = kvp_lst.find("m1_serial5")) != "") {
	CM1Unit unit;

	if (ser[3] == '-' || ser[3] == ' ')  ser = ser.substr(0, 3) + ser.substr(4, 3); // Kill xxx-xxx

	fprintf(log, "%lu: 5 - Retrieveing serial[%s].\n", time_stamp(), ser.c_str());
	if (!db_get_m1_unit(db_t, ser, unit) || (unit.mOwnerAccountID != 1 && unit.mOwnerAccountID != acc.mDbID)) {
	    emit_error(log, 7, string("Illegal serial number - ") + ser);
	    goto end;
	}
	owned_m1.push_back(unit);
    }

    fprintf(log, "%lu: Editing account [%s].\n", time_stamp(), kvp_lst.find("email").c_str());
    db_edit_account(db_t, 
		    db_u, 
		    log,
		    kvp_lst.find("email"), 
		    //		    kvp_lst.find("new_email"), kvp_lst.exists("new_email"), 
		    "", false, 
		    kvp_lst.find("new_password"),kvp_lst.exists("new_password"),
		    kvp_lst.find("password_hint"),kvp_lst.exists("password_hint"),
		    kvp_lst.find("first_name"),kvp_lst.exists("first_name"),
		    kvp_lst.find("middle_name"),kvp_lst.exists("middle_name"),
		    kvp_lst.find("last_name"),kvp_lst.exists("last_name"),
		    kvp_lst.find("address1"),kvp_lst.exists("address1"),
		    kvp_lst.find("address2"),kvp_lst.exists("address2"),
		    kvp_lst.find("city"),kvp_lst.exists("city"),
		    kvp_lst.find("zip"),kvp_lst.exists("zip"),
		    kvp_lst.find("state"),kvp_lst.exists("state"),
		    kvp_lst.find("country"),kvp_lst.exists("country"),
		    kvp_lst.find("home_phone"),kvp_lst.exists("home_phone"),
		    kvp_lst.find("work_phone"),kvp_lst.exists("work_phone"),
		    kvp_lst.find("cell_phone"),kvp_lst.exists("cell_phone"),
		    kvp_lst.find("comments"),kvp_lst.exists("comments"),
		    owned_m1);
    

    fprintf(log, "%lu: Account edited.\n", time_stamp());
    result.push_back(CKeyValue("text", "account changed."));
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
