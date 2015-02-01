//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Create a new sales order
// This code is invoked as a CGI script.
//
#include "sqlcommon.hh"
#include "cgicommon.hh"

char *glob_argv0;
extern void usage(void) {}

main(int argc, char *argv[]) 
{
    MYSQL *db_t = 0;
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
	"trans_nr",
	"order_id",
	"part_nr",
	"quantity",
	"subtotal",
	"sh_fee",
	"tax"
    };
    CAccount acc;
    CPart part;

    FILE *log;
    time_t now = time(0);

    stderr = log  = fopen(LOGFILE, "a");
    if (!log)  {
	emit_error(0, 9, "Could not open log file");
	exit(0);
    }
    
    fprintf(log, "%lu: Log start: %s %s", time_stamp(), argv[0], ctime(&now));
    fprintf(log, "%lu: Trusted host[%s] db[%s] user[%s]\n", time_stamp(), TRUSTED_HOST, TRUSTED_DB, TRUSTED_USER);

    //
    // Connect to database
    //
    fprintf(log, "%lu: Opening database.\n", time_stamp());
    if (!db_connect(&db_t, TRUSTED_HOST,  TRUSTED_USER, TRUSTED_PASSWD, TRUSTED_DB)) {
	emit_error(log, 9, "Could not connect to database");
	db_t = 0;
	goto end;
    }
    fprintf(log, "%lu: Database open.\n", time_stamp());
    
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

    fprintf(log, "\n");
    free(query);

    log_query_fields(log, kvp_lst);
    
    while(ind < sizeof(mand_field)/sizeof(mand_field[0])) {
	if (kvp_lst.find(mand_field[ind]) == "") {
	    emit_error(0, 1, "Missing field");
	    goto end;
	}
	++ind;
    }
    //
    // Check that permission is correct.
    //
    fprintf(log, "%lu: Locating account [%s].\n", time_stamp(), kvp_lst.find("email").c_str());
    if (db_find_account_by_email(db_t, kvp_lst.find("email"), &acc) == 0) {
	fprintf(log, "Login incorrect - email\n");
	emit_error(log, 6, "Login incorrect");
	goto end;
    }
    fprintf(log, "%lu: Email located.\n", time_stamp());

    if (acc.mPassword != kvp_lst.find("password")) {
	fprintf(log, "Login incorrect - password Got[%s] Want[%s]\n", kvp_lst.find("password").c_str(), acc.mPassword.c_str());
	emit_error(log, 6, "Login incorrect");
	goto end;
    }

    
    //
    // Locate part
    //
    fprintf(log, "%lu: Locating part [%s].\n", time_stamp(), kvp_lst.find("part_nr").c_str());
    if (!db_get_part_by_part_nr(db_t, kvp_lst.find("part_nr"), part)) {
	fprintf(log, "Unknown part number [%s]\n", kvp_lst.find("part_nr").c_str());
	emit_error(log, 4, "Unknown part number");
	goto end;
    }
    fprintf(log, "%lu: Part located.\n", time_stamp());
    fprintf(log, "%lu: Creating sales order.\n", time_stamp());

    if (!(db_create_sales_order(db_t, 
				time(0),
				kvp_lst.find("trans_nr"),
				kvp_lst.find("order_id"),
				&acc,
				&part,
				atoi(kvp_lst.find("quantity").c_str()),
				atof(kvp_lst.find("subtotal").c_str()),
				atof(kvp_lst.find("sh_fee").c_str()),
				atof(kvp_lst.find("tax").c_str())))) {
	
	fprintf(log, "%lu: Internal error. Could not update database.\n", time_stamp());
	emit_error(log, 10, "Internal error.");
	goto end;
    }
    
    fprintf(log, "%lu: Order created.\n", time_stamp());
    result.push_back(CKeyValue("text", "order placed."));
    emit_result(log, result);

 end:
    if (db_t)
	mysql_close(db_t);

    fprintf(log, "%lu: ----------------------\n\n\n", time_stamp());

    fclose(log);
    exit(0);
}
