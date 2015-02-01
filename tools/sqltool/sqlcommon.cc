//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA,  2005, 2006, 2007, 2008.
//
// Some common SQL stuff.

#include "sqlcommon.hh"
#include <sys/time.h>
#include <time.h>

extern void usage(void);

unsigned long time_stamp(void)
{
    struct timeval tm;
    static unsigned long long first_ts = 0LL;
    unsigned long long ts;

    gettimeofday(&tm, 0);
    ts =tm.tv_sec*1000000LL + tm.tv_usec;

    if (first_ts == 0LL) 
	first_ts = ts;

    return (unsigned long) (ts-first_ts)/1000LL; // msec
}

bool extract_from(const string &aFrom,
		  string &aAccount, 
		  string &aProvider)
{
    string::size_type at_pos = aFrom.find("@", 0);

    aAccount = "";
    aProvider = "";

    if (at_pos == string::npos)
	return false;

    aAccount = aFrom.substr(0, at_pos);
    aProvider = aFrom.substr(at_pos + 1);
    return true;
}

// Allocate enough bytes to make a safe escape 
char *ctx_escape_mysql(MYSQL *aDBDesc, const char *aString, int aLength, CStringContext *aContext)
{
    char *res;
    unsigned long len = (aLength>0)?aLength:strlen(aString);

    if (!(res = (char *) malloc(len * 2 + 1)))
	return (char *) "";

    mysql_real_escape_string(aDBDesc, res, aString, len);

    if (aContext)
	aContext->push_back(res);

    return res;
}

char *ctx_strcpy(const char *aString, int aLength, CStringContext *aContext)
{
    char *res;
    unsigned long len = (aLength>0)?aLength:strlen(aString);

    if (!(res = (char *) malloc(len + 1)))
	return (char *) "";
    strncpy(res, aString, len);
    res[len] = 0;

    if (aContext)
	aContext->push_back(res);

    return res;
}

char *ctx_sprintf(CStringContext *aContext, char *aFormat, ...)
{
    va_list ap;
    char *res;

    va_start(ap, aFormat);
    // $10 bucks you do a man on this function...
    vasprintf(&res, aFormat, ap);
    va_end(ap);
    if (aContext)
	aContext->push_back(res);

    return res;
}

bool extract_packet_id(string &aID, 
		       string &aAccount, 
		       string &aProvider, 
		       string &aPacket,
		       int &aMajor,
		       int &aMinor,
		       int &aPatch)
{
    string tmp;
    string::size_type first_slash_pos, second_slash_pos;
    // Format is account@provider/packet
    
    if (!extract_from(aID, aAccount, tmp))
	return false;

    first_slash_pos = tmp.find("/", 0);
    second_slash_pos = tmp.find_last_of("/");

    // Did we find the slash?
    if (first_slash_pos == string::npos)
	return false;

    aProvider = tmp.substr(0, first_slash_pos);
    aMajor = aMinor = aPatch = -1; 

    // Extract version information.
    if (second_slash_pos != string::npos) {
	sscanf(tmp.substr(second_slash_pos + 1).c_str(), 
	       "%d.%d.%d", 
	       &aMajor,
	       &aMinor,
	       &aPatch);
	aPacket = tmp.substr(first_slash_pos + 1, second_slash_pos - first_slash_pos - 1);
    } else {
	aPacket = tmp.substr(first_slash_pos + 1);
    }

//     fprintf(stderr, "extract_packet_id() ID[%s] Account[%s] Provider[%s] Packet[%s] Version[%d.%d.%d]\n",
// 	   aID.c_str(),
// 	   aAccount.c_str(),
// 	   aProvider.c_str(),
// 	   aPacket.c_str(),
// 	   aMajor,
// 	   aMinor,
// 	   aPatch);
    return true;
}

int db_connect(MYSQL **aDBDesc, string aHost, string aUserName, string aPasswd, string aDatabase, string aCharacterSet) 
{
    CStringContext ctx;
    int port = 0;
    char *stmt;
    string::size_type col_pos;

    if (*aDBDesc != 0)
	mysql_close(*aDBDesc);

    *aDBDesc = mysql_init(0);

    if (aHost == "" && getenv("DB_HOST") != 0)
	aHost = getenv("DB_HOST");

    if (aUserName == "" && getenv("DB_USER") != 0)
	aUserName = getenv("DB_USER");

    if (aPasswd == "" && getenv("DB_PASSWORD") != 0)
	aPasswd = getenv("DB_PASSWORD");

    if (aDatabase == "" && getenv("DB_DATABASE") != 0)
	aDatabase = getenv("DB_DATABASE");

    //
    // Splice port if provided
    //
    col_pos = aHost.find(':', 0);

    if (col_pos != string::npos && aHost.size() > col_pos) {
	port = atoi(aHost.substr(col_pos + 1).c_str());
	aHost = aHost.substr(0, col_pos);
    }

    if (!mysql_real_connect(*aDBDesc, aHost.c_str(), aUserName.c_str(), aPasswd.c_str(), aDatabase.c_str(), port, 0, 0)) {
	fprintf(stderr, "%lu: db_connect:(): Failed to connect [%s]\n", time_stamp(), mysql_error(*aDBDesc));
	mysql_close(*aDBDesc);
	return 0;
    }

    stmt = ctx_sprintf(&ctx,  "SET NAMES %s", aCharacterSet.c_str());
    if (mysql_query(*aDBDesc, stmt)) {
	fprintf(stderr, "$lu: db_connect:(): Failed to [%s] [%s]\n", time_stamp(), stmt, mysql_error(*aDBDesc));
	mysql_close(*aDBDesc);   
	return 0;
    }
    return 1;
}    


// Locate an account id basd on aEmail address.
int db_find_account_by_email(MYSQL *aDBDesc, string aEmail, CAccount *aResult)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    char *stmt;
    CStringContext esc_ctx;
    //    char esc_email[512];
    long dbid;

    if (!aDBDesc) {
	fprintf(stderr, "%lu: db_find_account_by_email(%s): Database not open\n", time_stamp(), aEmail.c_str());
	return 0L;
    }
    //    mysql_real_escape_string(aDBDesc, esc_email, aEmail.c_str(), (aEmail.length()>255)?255:aEmail.length());

    //
    // If the provided TSPID is 0, read everything added after the last timestamp.
    //

    //
    // If TSPID is not zero, read the row 
    //
    stmt = ctx_sprintf(&esc_ctx, 
		       "SELECT "
		       "   id, "          // 0
		       "   email, "       // 1
		       "   password, "      // 2
		       "   password_hint, " // 3
		       "   first_name, "  // 4
		       "   middle_name, " // 5
		       "   last_name, "   // 6
		       "   address1, "    // 7
		       "   address2, "    // 8
		       "   city, "        // 9
		       "   zip, "         // 10
		       "   state, "       // 11
		       "   country, "     // 12
		       "   home_phone, "  // 13
		       "   work_phone, "  // 14
		       "   cell_phone, "  // 15
		       "   comments, "    // 16
		       "   free_shipping, " // 17
		       "   tax_exempt " // 18
		       "FROM "
		       "  account "
		       "WHERE "
		      "  email =\'%s\'",
		      ctx_escape_mysql(aDBDesc, aEmail.c_str(), 0, &esc_ctx));

    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "%lu: db_find_account_by_email(%s): Could not execute SQL statement\n%s\n%s\n", 
		time_stamp(), aEmail.c_str(), stmt, mysql_error(aDBDesc));
	return 0L;
    }


    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	return 0L;
    }

    dbid = atoi(row[0]);

    if (!aResult) {
	mysql_free_result(res);
	return dbid;
    }

    aResult->mDbID = dbid;
    aResult->mEmail = row[1];   
    aResult->mPassword = row[2];  
    aResult->mPasswdHint = row[3];
    aResult->mFirstName = row[4];
    aResult->mMiddleName = row[5];
    aResult->mLastName = row[6];
    aResult->mAddress1 = row[7];
    aResult->mAddress2 = row[8];
    aResult->mCity = row[9];
    aResult->mZip = row[10];
    aResult->mState = row[11];
    aResult->mCountry = row[12];
    aResult->mHomePhone = row[13];
    aResult->mWorkPhone = row[14];
    aResult->mCellPhone = row[15];
    aResult->mComment = row[16];
    aResult->mFreeShipping = atoi(row[17]);
    aResult->mTaxExempt = atoi(row[18]);
    mysql_free_result(res);
    return dbid;
}

// Locate an account id basd on account id address.
long db_find_account_by_id(MYSQL *aDBDesc, long aAccountID, CAccount *aResult)
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;

    if (!aDBDesc) {
	fprintf(stderr, "db_find_account_by_id(%s): Database not open\n", aAccountID);
	return 0L;
    }

    //
    // If the provided TSPID is 0, read everything added after the last timestamp.
    //

    //
    // If TSPID is not zero, read the row 
    //
    sprintf(buf, 
	    "SELECT "
	    "   id, "          // 0
	    "   email, "       // 1
	    "   password, "      // 2
	    "   password_hint, " // 3
	    "   first_name, "  // 4
	    "   middle_name, " // 5
	    "   last_name, "   // 6
	    "   address1, "    // 7
	    "   address2, "    // 8
	    "   city, "        // 9
	    "   zip, "         // 10
	    "   state, "       // 11
	    "   country, "     // 12
	    "   home_phone, "  // 13
	    "   work_phone, "  // 14
	    "   cell_phone, "  // 15
	    "   comments, "    // 16
	    "   free_shipping, " // 17
	    "   tax_exempt " // 18
	    "FROM "
	    "  account "
	    "WHERE "
	    "  id = %ld",
	    aAccountID);

    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_find_account_by_id(%ld): Could not execute SQL statement\n%s\n%s\n", 
	       aAccountID, buf, mysql_error(aDBDesc));
	return 0L;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	fprintf(stderr, "db_find_account_by_id(%ld): [%s] Yielded no rows. Error: [%s].\n", 
	       aAccountID, buf, mysql_error(aDBDesc));
	return 0L;
    }

    aResult->mDbID = atoi(row[0]);
    aResult->mEmail = row[1];   
    aResult->mPassword = row[2];  
    aResult->mPasswdHint = row[3];
    aResult->mFirstName = row[4];
    aResult->mMiddleName = row[5];
    aResult->mLastName = row[6];
    aResult->mAddress1 = row[7];
    aResult->mAddress2 = row[8];
    aResult->mCity = row[9];
    aResult->mZip = row[10];
    aResult->mState = row[11];
    aResult->mCountry = row[12];
    aResult->mHomePhone = row[13];
    aResult->mWorkPhone = row[14];
    aResult->mCellPhone = row[15];
    aResult->mComment = row[16];
    aResult->mFreeShipping = atoi(row[17]);
    aResult->mTaxExempt = atoi(row[18]);
    mysql_free_result(res);
    //    fprintf(stderr, "db_find_account_by_id(%ld): email [%s]\n", aAccountID, aResult->mEmail.c_str());
    return aResult->mDbID;
}

// Locate a packfile 
// account@domain/name
// account_id represents the account@domain 
// while packfile_name is the /name part.
//
int db_find_packfile(MYSQL *aDBDesc, string aPacketID, CPackfileList &aPackfiles)
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;
    char esc_name[514];
    char esc_email[514];
    string d_acc, d_prov, d_name;
    int d_maj, d_min, d_pat;
    char *email;
    int row_count = 0;
    CStringContext esc_ctx;
    char *stmt;

    if (!aDBDesc) {
	fprintf(stderr, "db_find_packfile(%s): Database not open\n", aPacketID.c_str());
	return 0;
    }

    if (!extract_packet_id(aPacketID, d_acc, d_prov, d_name, d_maj, d_min, d_pat)) {
	fprintf(stderr, "db_find_packfile(%s): Malformed packet identifier\n", aPacketID.c_str());
	return 0;
    }

    email = ctx_sprintf(&esc_ctx, "%s@%s", d_acc.c_str(), d_prov.c_str());

    //
    // Big phat select
    //
    stmt = ctx_sprintf(&esc_ctx,
		       "SELECT "
		       "   packfile.id, "                         // 0
		       "   packfile.svn_revision, "               // 1
		       "   packfile.creator_account_id, "         // 2
		       "   packfile.name, "                       // 3
		       "   packfile.restart_action, "             // 4
		       "   pf_version.id, "                       // 5
		       "   pf_version.major, "                    // 6
		       "   pf_version.minor, "                    // 7
		       "   pf_version.patch, "                    // 8
		       "   UNIX_TIMESTAMP(pf_version.created) "  // 9
		       "FROM "
		       "  account, packfile, pf_version "
		       "WHERE "
		       "  account.email LIKE '%s' AND "
		       "  packfile.creator_account_id = account.id AND "
		       "  packfile.name LIKE '%s' AND "
		       "  pf_version.packfile_id = packfile.id AND "
		       "  (pf_version.major = %ld OR %ld = -1) AND "
		       "  (pf_version.minor = %ld OR %ld = -1) AND "
		       "  (pf_version.patch = %ld OR %ld = -1) "
		       "ORDER BY "
		       "   pf_version.major DESC, "
		       "   pf_version.minor DESC, "
		       "   pf_version.patch DESC",
		       ctx_escape_mysql(aDBDesc, email, 0, &esc_ctx),
		       ctx_escape_mysql(aDBDesc, d_name.c_str(), 0, &esc_ctx),
	  
		       d_maj, d_maj,
		       d_min, d_min,
		       d_pat, d_pat);

    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_find_packfile(%s): Could not execute SQL statement\n%s\n%s\n", 
	       aPacketID.c_str(), stmt, mysql_error(aDBDesc));
	return -1;
    }

    res = mysql_store_result(aDBDesc); 

    if (!res) {
	fprintf(stderr, "db_find_packfile(%s): [%s] Yielded no rows. Error: [%s].\n", 
	       aPacketID.c_str(), stmt, mysql_error(aDBDesc));

	return 0;
    }

    while((row = mysql_fetch_row(res))) {
	row_count++;
	// Another entry.
	aPackfiles.push_back(CPackfile(atol(row[0]), 
				       atol(row[1]),
				       row[3],
				       row[4],
				       atol(row[2]),
				       atol(row[5]),
				       atol(row[6]),
				       atol(row[7]),
				       atol(row[8]),
				       atol(row[9])));
//  	fprintf(stderr, "db_find_packfile(%s): Got  Id[%ld] name[%s] Restart[%s] CreatorID[%ld] VersID[%ld] Major[%ld] Minor[%ld] Patch[%ld] Created[%.24s]\n",
//  	       aPacketID.c_str(),
//  	       aPackfiles.back().mID,
//  	       aPackfiles.back().mName.c_str(),
//  	       aPackfiles.back().mRestartAction.c_str(),
//  	       aPackfiles.back().mCreatorAccountID,
//  	       aPackfiles.back().mVersionID,
//  	       aPackfiles.back().mMajor,
//  	       aPackfiles.back().mMinor,
//  	       aPackfiles.back().mPatch,
//  	       ctime(&(aPackfiles.back().mCreatorTimeStamp)));
	     
    }
    mysql_free_result(res);

    return row_count;
}


long db_create_packfile(MYSQL *aDBDesc, CPackfile &aPackfile)
{
    char *stmt;
    time_t now;
    CStringContext str_ctx;
    CPackfileList existing;
    CAccount acct;
    
    // Check that an earlier version does not already exist.
    if (!db_find_account_by_id(aDBDesc, aPackfile.mCreatorAccountID, &acct)) {
	fprintf(stderr, "db_create_packfile(): Failed to retrieve account [%ld]\n", 
	       aPackfile.mCreatorAccountID);
	return false;
    }

    //
    // Return all existing versions of the packfile. If no versions 
    // exist, create a new packfile row, else reuse existing one.
    //
    if (!db_find_packfile(aDBDesc, acct.mEmail + string("/") + aPackfile.mName, existing)) {
	fprintf(stderr, "No packfile named %s/%s exists. Will create it\n", acct.mEmail.c_str(), aPackfile.mName.c_str());

	// Create packfile row.
	stmt = ctx_sprintf(&str_ctx,
			   "INSERT INTO packfile "
			   " (svn_revision, creator_account_id, name, restart_action) "
			   "VALUES (%ld, %ld, '%s', '%s')",
			   aPackfile.mSVNRevision,
			   aPackfile.mCreatorAccountID,
			   ctx_escape_mysql(aDBDesc, aPackfile.mName.c_str(), 0, &str_ctx),
			   aPackfile.mRestartAction.c_str());


	if (mysql_query(aDBDesc, stmt)) {
	    fprintf(stderr, "db_create_packfile:(): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	    return 0;
	}
    
	// Retrieve the auto generated trusted.id column
	aPackfile.mID = (long) mysql_insert_id(aDBDesc);
	//	fprintf(stderr, "db_create_packfile(): Created packfile id[%ld]\n", aPackfile.mID);
    } else {
	// Reuse existing packfile id
	aPackfile.mID = existing.front().mID;

    }

    //
    // Create version row.
    // 
    aPackfile.mCreatorTimeStamp = time(0);
    stmt = ctx_sprintf(&str_ctx,
		       "INSERT INTO pf_version "
		       " (packfile_id, major, minor, patch, created) "
		       "VALUES (%ld, %d, %d, %d, FROM_UNIXTIME(%lu))",
		       aPackfile.mID, 
		       aPackfile.mMajor,
		       aPackfile.mMinor,
		       aPackfile.mPatch, 
		       aPackfile.mCreatorTimeStamp);

    
    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_create_packfile:(): Failed [%s]: [%s]\n", stmt, mysql_error(aDBDesc));
	return 0;
    }
    
    // Retrieve the auto generated trusted.id column
    aPackfile.mVersionID = (long) mysql_insert_id(aDBDesc);
    //    fprintf(stderr, "db_create_packfile(): Created version id[%ld]\n", aPackfile.mVersionID);

    return aPackfile.mVersionID;
}
		     


bool db_delete_packfile(MYSQL *aDBDesc, CPackfile &aPackfile)
{
    char *stmt;
    CStringContext str_ctx;
    CAccount acct;
    CPackfileList versions;

    
    // Collect all versions of this packfile so that we know if we are to
    // remove the packfile row as well (after last version has been removed).
    if (!db_find_account_by_id(aDBDesc, aPackfile.mCreatorAccountID, &acct)) {
	fprintf(stderr, "db_delete_packfile(): Failed to retrieve account [%ld]\n", 
	       aPackfile.mCreatorAccountID);
	return false;
    }

    db_find_packfile(aDBDesc, acct.mEmail + string("/") + aPackfile.mName, versions); // Will return one element if this is the last.

    // Remove all pf_component_parts
    stmt = ctx_sprintf(&str_ctx,
		       "DELETE "
		       "  pf_component_part "
		       "FROM "
		       "  pf_component_part, "
		       "  pf_component "
		       "WHERE "
		       "  pf_component_part.pf_component_id = pf_component.id AND "
		       " pf_component.pf_version_id = %ld",
		       aPackfile.mVersionID);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_delete_packfile:(pf_component_part): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	return false;
    }
    


    // Remove all pf_component
    stmt = ctx_sprintf(&str_ctx,
		       "DELETE FROM "
		       "  pf_component "
		       "WHERE "
		       " pf_version_id = %ld",
		       aPackfile.mVersionID);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_delete_packfile:(pf_component): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	return false;
    }


    // Remove all pf_version_dependencies
    stmt = ctx_sprintf(&str_ctx,
		       "DELETE FROM "
		       "  pf_dependencies "
		       "WHERE "
		       " pf_version_id = %ld",
		       aPackfile.mVersionID);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_delete_packfile:(pf_dependencies): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	return false;
    }

    // Remove all pf_version
    stmt = ctx_sprintf(&str_ctx,
		       "DELETE FROM "
		       "  pf_version "
		       "WHERE "
		       " id = %ld",
		       aPackfile.mVersionID);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_delete_packfile:(pf_version): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	return false;
    }

    // If there are other pf_version rows still referencing
    // the packfile, return now.
    if (versions.size() != 1) {
	fprintf(stderr, "db_delete_packfile(): Leaving packfile in place since it is still in use by [%ld] pf_versions\n",
	       versions.size() - 1);
	return true;
    }
	
	
    //
    // Remove all references from use accounts to this content.
    //
    fprintf(stderr, "Deleting packfile.id[%ld]\n", aPackfile.mID);
    stmt = ctx_sprintf(&str_ctx,
		       "DELETE FROM "
		       "  content "
		       "WHERE "
		       " packfile_id = %ld",
		       aPackfile.mID);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_delete_packfile:(content): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	return false;
    }

    // Remove all pf_packfile
    stmt = ctx_sprintf(&str_ctx,
		       "DELETE FROM "
		       "  packfile "
		       "WHERE "
		       " id = %ld",
		       aPackfile.mID);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_delete_packfile:(pf_packfile): Failed to [%s] [%s]\n", stmt,mysql_error(aDBDesc));
	return false;
    }

    return true;
}
		     


// Retrieve all content entries undr the given aVersion.
int db_get_files(MYSQL *aDBDesc, long aVersionID, CContentList &aResult)
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;
    CIDMap map;
    int f_cnt = 0;

    sprintf(buf, 
	    "SELECT "
	    "   id, "               // 0
	    "   fs_path, "          // 1
	    "   fs_symlink, "       // 2
	    "   comp_type, "        // 3
	    "   permission, "       // 4
	    "   content_length, "   // 5
	    "   link_id, "          // 6
	    "   dev_major, "        // 7
	    "   dev_minor "         // 8
	    "FROM "
	    "  pf_component "
	    " WHERE "
	    "   pf_version_id = %ld "
	    "ORDER BY "
	    "   id ASC", // Make sure we get files before we get the hardlinks to them
	    aVersionID);

    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_get_files(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aVersionID, buf, mysql_error(aDBDesc));
	return 0L;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res) {
	fprintf(stderr, "db_get_files(%ld): [%s] Yielded no rows. Error: [%s].\n", 
	       aVersionID, buf, mysql_error(aDBDesc));
	return 0L;
    }

    // Build a list 
    while ((row = mysql_fetch_row(res))) { 
	aResult.push_back(CContent(row[1]?row[1]:"", // aPath
				   row[5]?atol(row[5]):0, // size
				   atol(row[4]), // perm
				   atol(row[3]), // type
				   row[2]?row[2]:"", // symlink target
				   row[7]?atol(row[7]):0, // major
				   row[8]?atol(row[8]):0, // minor
				   0, // Link ptr
				   atol(row[0]), // db id
				   row[6]?atol(row[6]):0)); // link id
				   
// 	fprintf(stderr, "db_get_files(%ld): Got path[%s] size[%lu] perm[%o]  type[%d] symlnk[%s] maj[%ld] min[%ld] id[%ld] lnk_id[%ld]\n",
// 	       aVersionID, 
// 	       aResult.back().mPath.c_str(),
// 	       aResult.back().mSize,
// 	       aResult.back().mPermission,
// 	       aResult.back().mType,
// 	       aResult.back().mSymlinkTarget.c_str(),
// 	       aResult.back().mMajor,
// 	       aResult.back().mMinor,
// 	       aResult.back().mDbID,
// 	       aResult.back().mLinkID);

	map[aResult.back().mDbID] = &(aResult.back());
	f_cnt++;
    }

    mysql_free_result(res);

    //
    // Resolve mLink ptr in aResult members.
    //
    for(CContentListIterator iter = aResult.begin(); iter != aResult.end(); ++iter) {
	CContent *target;

	if (!iter->mLinkID)
	    continue;

	if (!(target = map[iter->mLinkID])) {
	    fprintf(stderr, "Could not resolve hardlink for file[%s] db link id[%ld]\n", iter->mPath.c_str(), iter->mLinkID);
	    return 0;
	}
	fprintf(stderr, "file[%s] linked to [%s]\n", iter->mPath.c_str(), target->mPath.c_str());
	iter->mLink = target;
    }
    return f_cnt;
}

// Load content of file referenced by aContent into aResult.
// aResult will contain aContent->mSize bytes.
//
int db_get_file_content(MYSQL *aDBDesc, CContent *aContent, char *aResult)
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;
    CIDMap map;
    unsigned long bytes_read = 0L;

    if (aContent->mDbID == 0) {
	fprintf(stderr, "db_get_file_content(%s): Database index for this content is 0\n", aContent->mPath.c_str());
	return 0;
    }
	
    sprintf(buf, 
	    "SELECT "
	    "   content, "         // 0
	    "   LENGTH(content) "               // 1
	    "FROM "
	    "  pf_component_part "
	    "WHERE "
	    "   pf_component_id = %ld "
	    "ORDER BY "
	    "   id ASC",
	    aContent->mDbID);
    
    
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_get_file_content(%s): Could not execute SQL statement[%s]: %s\n", 
	       aContent->mPath.c_str(), buf, mysql_error(aDBDesc));
	return 0L;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res) {
	fprintf(stderr, "db_get_file_content(%s): [%s] Yielded no rows. Error: [%s].\n", 
	       aContent->mPath.c_str(), buf, mysql_error(aDBDesc));
	return 0L;
    }

    while((row = mysql_fetch_row(res))) {
	long len = atoi(row[1]);
	memcpy(aResult + bytes_read, row[0], len);
	bytes_read += len;
    }
    mysql_free_result(res);

    return bytes_read;
}

long db_add_file(MYSQL *aDBDesc, long aVersionID, CContent &aEntry, string aRootPrefix, int aPartSize)
{
    long comp_id;
    char esc_path[0xFFFF*2+2];
    char *stmt;
    CStringContext str_ctx;


    // Read file if regular.
    if (aEntry.mType == DB_CONT_REGULAR || aEntry.mType == DB_CONT_PACKFILE) {
	unsigned long bytes_read = 0L;
	unsigned long stmt_len;

	// Prep statement
	stmt = ctx_sprintf(&str_ctx,
			   "INSERT INTO pf_component "
			   " (pf_version_id, fs_path, fs_symlink, comp_type, permission, link_id, dev_major, dev_minor, content_length) "
			   "VALUES(%ld, '%s',NULL, %d, %d, NULL, NULL, NULL, %ld)",
			   aVersionID, 
			   ctx_escape_mysql(aDBDesc, (aRootPrefix + aEntry.mPath).c_str(), 0, &str_ctx),
			   aEntry.mType,
			   aEntry.mPermission,
			   aEntry.mSize);


	
	goto install_entry;
    }
	
    // We need to escape the symlink name.
    if (aEntry.mType == DB_CONT_SYMLINK) {
	// Setup a insert statement for a non regular file
	
	stmt = ctx_sprintf(&str_ctx,
			   "INSERT INTO pf_component "
			   " (pf_version_id, fs_path, fs_symlink, comp_type, permission, link_id, dev_major, dev_minor) "
			   "VALUES(%ld, '%s', '%s', %d, %ld, NULL, NULL, NULL)",
			   aVersionID, 
			   ctx_escape_mysql(aDBDesc, (aRootPrefix + aEntry.mPath).c_str(), 0, &str_ctx),
			   ctx_escape_mysql(aDBDesc, (aRootPrefix + aEntry.mSymlinkTarget).c_str(), 0, &str_ctx),
			   aEntry.mType, 
			   aEntry.mPermission);

	goto install_entry;
    };    

    if (aEntry.mType == DB_CONT_HARDLINK) {
	// Setup a insert statement for a non regular file
	stmt = ctx_sprintf(&str_ctx,
			   "INSERT INTO pf_component "
			   " (pf_version_id, fs_path, fs_symlink, comp_type, permission, link_id, dev_major, dev_minor) "
			   "VALUES(%ld, '%s', NULL, %d, %ld, %ld, NULL, NULL)",
			   aVersionID, 
			   ctx_escape_mysql(aDBDesc, (aRootPrefix + aEntry.mPath).c_str(), 0, &str_ctx),
			   aEntry.mType, 
			   aEntry.mPermission,
			   aEntry.mLink->mDbID);

	//	fprintf(stderr, "[%s] hardlinked to [%ld]\n", aEntry.mPath.c_str(), aEntry.mLink->mDbID);
	goto install_entry;
    };    

    if (aEntry.mType == DB_CONT_BLOCKDEV ||
	aEntry.mType == DB_CONT_CHARDEV) {
	// Setup a insert statement for a non regular file
	
	stmt = ctx_sprintf(&str_ctx,
			   "INSERT INTO pf_component "
			   " (pf_version_id, fs_path, fs_symlink, comp_type, permission, link_id, dev_major, dev_minor) "
			   "VALUES(%ld, '%s', NULL, %d, %ld, NULL, %lu, %lu)",
			   aVersionID, 
			   ctx_escape_mysql(aDBDesc, (aRootPrefix + aEntry.mPath).c_str(), 0, &str_ctx),
			   aEntry.mType, 
			   aEntry.mPermission,
			   aEntry.mMajor,
			   aEntry.mMinor);

	goto install_entry;
    };    

    // Default management
    // Setup a insert statement for a non regular file
	
    stmt = ctx_sprintf(&str_ctx,
		       "INSERT INTO pf_component "
		       " (pf_version_id, fs_path, fs_symlink, comp_type, permission, link_id, dev_major, dev_minor) "
		       "VALUES(%ld, '%s',NULL, %d, %ld, NULL, NULL, NULL)",
		       aVersionID, 
		       ctx_escape_mysql(aDBDesc, (aRootPrefix + aEntry.mPath).c_str(), 0, &str_ctx),
		       aEntry.mType);

    

 install_entry:
    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_add_file:(): Failed to insert into trusted.pf_component [%s]: [%s]\n", stmt, mysql_error(aDBDesc));
	return 0;
    }

    // Retrieve the auto generated trusted.id column
    aEntry.mDbID = (long) mysql_insert_id(aDBDesc);
    //    fprintf(stderr, "db_add_file(): Created pf_component [%s] id[%ld] size[%ld]\n", aEntry.mPath.c_str(), aEntry.mDbID, aEntry.mSize);
    // Update this entry with the db id.

    //
    // Insert component parts if necessary.
    //
    if (aEntry.mType == DB_CONT_REGULAR || aEntry.mType == DB_CONT_PACKFILE) {
	unsigned long bytes_left = aEntry.mSize;
	int fdesc = open(aEntry.mPath.c_str(), O_RDONLY);
	unsigned char inbuf[aPartSize];
	int ret;
	int part_count = 0;

	if (fdesc == -1) {
	    fprintf(stderr, "db_add_file(%s): Could not open file: %s\n", aEntry.mPath.c_str(), strerror(errno));
	    return 0;
	}

	while(bytes_left > 0) {
	    int len;
	    CStringContext part_ctx;
	    char *part_stmt;

	    // Read next part
	    len = read(fdesc, inbuf, aPartSize);
	    if (len <= 0) {
		fprintf(stderr, "db_add_file(%s): Could not read file file: %s\n", aEntry.mPath.c_str(), strerror(errno));
		close(fdesc);
		return 0;
	    }
	    bytes_left -= len;

	    part_stmt = ctx_sprintf(&part_ctx, 
				    "INSERT INTO pf_component_part "
				    " (id, pf_component_id, content) "
				    "VALUES (NULL, %ld, '%s')",
				    aEntry.mDbID,
				    ctx_escape_mysql(aDBDesc, (const char *) inbuf, len, &part_ctx));
	    
	    // We now have an insert query to execute.
	    if (mysql_query(aDBDesc, part_stmt)) {
		fprintf(stderr, "db_add_file:(): Failed to insert into trusted.pf_component_part [%s]", mysql_error(aDBDesc));
		close(fdesc);
		return 0;
	    }
// 	    fprintf(stderr, "db_add_file(): Created pf_component_part [%s] id[%ld] len[%d]\n", 
// 		   aEntry.mPath.c_str(), 
// 		   (long) mysql_insert_id(aDBDesc), 
// 		   len);
	    

	    //
	    // We have another component part to install. 
	    // Setup insert statement.
	    //
	    ++part_count;

	    // Retrieve the auto generated trusted.id column
	}
	// Flush last part.
	close(fdesc);
    }
    return aEntry.mDbID;
}			  

long db_add_dependency(MYSQL *aDBDesc, long aPacketVersionID,  long aNeededVersionID)
{
    long pf_id; // Packfile id. Automatically generated.
    long dep_id; // Version id. Automatically generated.
    long req_id; // Version id. Automatically generated.
    char buf[256];
    
    //
    // Create version row.
    // 
    sprintf(buf, 
	    "INSERT INTO pf_dependencies "
	    " (pf_version_id, requires_pf_version_id) "
	    "VALUES(%ld, %d)",
	    aPacketVersionID, 
	    aNeededVersionID);

    if (mysql_query(aDBDesc, buf)) {
	fprintf(stderr, "db_add_dependency:(): Failed to insert into trusted.pf_dependencies [%s]", mysql_error(aDBDesc));
	return 0;
    }
    
    // Retrieve the auto generated trusted.id column
    dep_id = (long) mysql_insert_id(aDBDesc);
    fprintf(stderr, "db_add_dependency pf_dependicies.id[%ld]\n", dep_id);
    return dep_id;
}

int db_get_dependencies(MYSQL *aDBDesc, long aPacketVersionID, CDependencyList *aResult)
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;
    char esc_name[514];
    char esc_email[514];
    string d_acc, d_prov, d_name;
    int d_maj, d_min, d_pat;
    string email;
    int row_count = 0;
  
    if (!aDBDesc) {
	fprintf(stderr, "db_get_dependencies(%ld): Database not open\n", aPacketVersionID);
	return 0L;
    }

    sprintf(buf, 
	    "SELECT "
	    "   account.email, "                       // 0
	    "   packfile.name, "                       // 1
	    "   pf_version.id, "                       // 2
	    "   pf_version.major, "                    // 3
	    "   pf_version.minor, "                    // 4
	    "   pf_version.patch "                     // 5
	    "FROM "
	    "  account, packfile, pf_version, pf_dependencies "
	    "WHERE "
	    "  pf_dependencies.pf_version_id = %ld AND "
	    "  pf_version.id = pf_dependencies.requires_pf_version_id AND "
	    "  packfile.id = pf_version.packfile_id AND "
	    "  account.id = packfile.creator_account_id "
	    "ORDER BY "
	    "   packfile.id ASC",
	    aPacketVersionID);

    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_get_dependencies(%ld): Could not execute SQL statement\n%s\n%s\n", 
	       aPacketVersionID, buf, mysql_error(aDBDesc));
	return -1;
    }

    res = mysql_store_result(aDBDesc); 

    if (!res) {
	fprintf(stderr, "db_get_dependencies(%ld): [%s] Yielded no rows. Error: [%s].\n", 
	       aPacketVersionID, buf, mysql_error(aDBDesc));

	return 0;
    }

    while((row = mysql_fetch_row(res))) {
	row_count++;

	// Another entry.
	aResult->push_back(CDependency(atol(row[2]), 
				       row[0],
				       row[1],
				       atol(row[3]),
				       atol(row[4]),
				       atol(row[5])));


// 	fprintf(stderr, "db_get_dependencies(%ld): Got Email[%s] Name[%s] VersID[%ld] Major[%ld] Minor[%ld] Patch[%ld]\n",
// 	       aPacketVersionID,
// 	       aResult->back().mEmail.c_str(),
// 	       aResult->back().mName.c_str(),
// 	       aResult->back().mVersionID,
// 	       aResult->back().mMajor,
// 	       aResult->back().mMinor,
// 	       aResult->back().mPatch);

    }
    mysql_free_result(res);

    return row_count;

}


// Return multiple keys
long db_find_encryption_keys(MYSQL *aDBDesc, 
			     CKeyDataList &aResult,
			     string aKeyName, // Wildcard search %ABC%
			     bool aDeviceKeyFlag)  // Device flag set or not
{
    MYSQL_RES *res;
    char *stmt;
    MYSQL_ROW row;
    CStringContext ctx;
    int count = 0;

    // 
    // Create a enc_key row for the device key
    //
    if (aKeyName != "")
	stmt = ctx_sprintf(&ctx, 
			   "SELECT "
			   "   priv_key_data, "              // 0
			   "   LENGTH(priv_key_data), "      // 1
			   "   pub_key_data, "               // 2
			   "   LENGTH(pub_key_data), "       // 3
			   "   bin_key_data, "               // 4
			   "   LENGTH(bin_key_data), "       // 5
			   "   key_name, "                   // 6
			   "   device_key_flag "             // 7
			   "FROM "
			   "  enc_key "
			   "WHERE "
			   "   key_name LIKE '%s' AND "
			   "   device_key_flag = %d",
			   ctx_escape_mysql(aDBDesc, aKeyName.c_str(), 0, &ctx),
			   aDeviceKeyFlag);
    else
	stmt = ctx_sprintf(&ctx, 
			   "SELECT "
			   "   priv_key_data, "              // 0
			   "   LENGTH(priv_key_data), "      // 1
			   "   pub_key_data, "               // 2
			   "   LENGTH(pub_key_data), "       // 3
			   "   bin_key_data, "               // 4
			   "   LENGTH(bin_key_data), "       // 5
			   "   key_name, "                   // 6
			   "   device_key_flag "             // 7
			   "FROM "
			   "  enc_key "
			   "WHERE "
			   "   device_key_flag = %d ",
			   aDeviceKeyFlag);

    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_find_encryption_keys(%s): Could not execute SQL statement[%s]: %s\n", 
	       aKeyName.c_str(), stmt, mysql_error(aDBDesc));
	return 0L;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res) {
	fprintf(stderr, "db_find_encryption_keys(%s): [%s] Yielded no rows. Error: [%s].\n", 
	       aKeyName.c_str(), stmt, mysql_error(aDBDesc));
	return 0L;
    }

    while (row = mysql_fetch_row(res))  {
	//	fprintf(stderr, "db_find_encryption_keys(%s): Adding[%s]\n",aKeyName.c_str(), row[2]);
	aResult.push_back(CKeyData(row[6], 
				   row[0], atoi(row[1]), 
				   row[2], atoi(row[3]), 
				   row[4], atoi(row[5]), 
				   atoi(row[7])));
	++count;
    }

    mysql_free_result(res);
    return count;
}

long db_get_encryption_key(MYSQL *aDBDesc, 
			   string aKeyName, 
			   CKeyData &aKeyData)
{
    MYSQL_RES *res;
    char *stmt;
    MYSQL_ROW row;
    CStringContext ctx;
    long key_id;

    // 
    // Create a enc_key row for the device key
    //
    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   id, "                     // 0
		       "   priv_key_data, "          // 1
		       "   LENGTH(priv_key_data), "  // 2
		       "   pub_key_data, "           // 3
		       "   LENGTH(pub_key_data), "   // 4
		       "   bin_key_data, "           // 5
		       "   LENGTH(bin_key_data), "   // 6
		       "   device_key_flag "         // 7
		       "FROM "
		       "  enc_key "
		       "WHERE "
		       "   key_name = '%s' ",
		       ctx_escape_mysql(aDBDesc, aKeyName.c_str(), 0, &ctx));
    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_encryption_key(%s): Could not execute SQL statement[%s]: %s\n", 
	       aKeyName.c_str(), stmt, mysql_error(aDBDesc));
	return 0L;
    }

    res = mysql_store_result(aDBDesc); 

    // Did we get a result?
    if (!res || !(row = mysql_fetch_row(res))) {
	fprintf(stderr, "db_get_encryption_key(%s): [%s] Yielded no rows. Error: [%s].\n", 
	       aKeyName.c_str(), stmt, mysql_error(aDBDesc));
	return 0L;
    }

    // Update provided key
    aKeyData.setPrivKeyData(row[1], atoi(row[2]));
    aKeyData.setPubKeyData(row[3], atoi(row[4]));
    aKeyData.setBinKeyData(row[5], atoi(row[6]));
    aKeyData.mDeviceKeyFlag = atoi(row[7]);
    aKeyData.mKeyName = aKeyName;
    key_id = atoi(row[0]);
    mysql_free_result(res);

    return key_id;
}


long db_add_encryption_key(MYSQL *aDBDesc, 
			   string aKeyName,
			   bool aDeviceKeyFlag, 
			   char *aPrivKeyData,
			   int aPrivKeyDataLength,
			   char *aPubKeyData,
			   int aPubKeyDataLength,
			   char *aBinKeyData,
			   int aBinKeyDataLength)
{
    long enc_key_id;
    char *stmt;
    CStringContext ctx;

    // 
    // Create a enc_key row for the device key
    //
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO enc_key ( "
		       "   key_name, "
		       "   priv_key_data, "
		       "   pub_key_data, "
		       "   bin_key_data, "
		       "   device_key_flag "
		       ")"
		       "VALUES ( "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   %d "
		       ")",
		       aKeyName.c_str(),
		       ctx_escape_mysql(aDBDesc, aPrivKeyData, aPrivKeyDataLength, &ctx),
		       ctx_escape_mysql(aDBDesc, aPubKeyData, aPubKeyDataLength, &ctx ),
		       ctx_escape_mysql(aDBDesc, aBinKeyData, aBinKeyDataLength, &ctx),
		       aDeviceKeyFlag);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_add_encryption_key:(): Failed to insert into trusted.enc_key [%s]\n", mysql_error(aDBDesc));
	return 0;
    }
    
    return (long) mysql_insert_id(aDBDesc);
}

			  
long db_get_m1_unit(MYSQL *aDBDesc, 
		    string aSerial, // Serial number of unit to retrieve
		    CM1Unit &aUnit)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    CIDMap map;
    unsigned long bytes_read = 0L;
    CStringContext ctx;
    long m1_id = 0;
    char *stmt;
    // 
    // Extract m1_row
    //
    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   id,             " // 0
		       "   build_order_id, " // 1
		       "   UNIX_TIMESTAMP(mfg_date), " // 2
		       "   disk_serial, "    // 3
		       "   account_id, "     // 4
		       "   enc_key_id "      // 5
		       "FROM " 
		       "  m1_unit "
		       "WHERE "
		       " serial_nr = '%s' ",
		       ctx_escape_mysql(aDBDesc, aSerial.c_str(), 0, &ctx));
    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_m1_unit(%s): Could not execute SQL statement[%s]: %s\n", 
	       aSerial.c_str(), stmt, mysql_error(aDBDesc));
	return 0;
    }

    res = mysql_store_result(aDBDesc); 

    if (!res || !(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	fprintf(stderr, "db_get_m1_unit(%s): [%s] Yielded no rows. Error: [%s].\n", 
	       aSerial.c_str(), stmt, mysql_error(aDBDesc));
	return 0;
    }

    aUnit.mDbID = atol(row[0]);
    aUnit.mSerial = aSerial;
    aUnit.mBuildOrderID = atol(row[1]);
    aUnit.mManufactureDate = atol(row[2]);
    aUnit.mDiskID = row[3];
    aUnit.mOwnerAccountID = atol(row[4]);
    aUnit.mDeviceKeyID = atol(row[5]);

    mysql_free_result(res);

    return aUnit.mDbID;
}

int db_add_m1_unit(MYSQL *aDBDesc, 
		   string aPartNr,
		   string aSerialNr,
		   char *aDevicePrivKey,
		   int aDevicePrivKeyLength,
		   char *aDevicePubKey,
		   int aDevicePubKeyLength,
		   char *aDeviceBinKey,
		   int aDeviceBinKeyLength,
		   string aDiskSerialNr,
		   long aOwnerAccountID) // Ref to account
{
    long unit_id;
    long enc_key_id;
    char *stmt;
    CStringContext ctx;
    CBuildOrder *bo = 0;
    CBuildOrderList  build_orders;
    CPart part;
    int bo_cnt;
    int bo_remain = 0;
    CLotList bo_lots;


    // Retrieve the part of the part nr. 
    if (!db_get_part_by_part_nr(aDBDesc, aPartNr, part)) {
	fprintf(stderr, "Could not find part nr [%s] in database\n", aPartNr.c_str());
	return false;
    }


    // Check that we have one build order.
    bo_cnt = db_get_build_order(aDBDesc, part.mID, DB_STATUS_ACTIVE, build_orders);   
    if (bo_cnt <= 0) {
	printf("No active build orders found for part [%s]\n", aPartNr.c_str());
	return 0L;
    }

    if (bo_cnt > 1) {
	printf("%d build orders active for part [%s]. Only one order must be active at any given time.\n",
	       bo_cnt, aPartNr.c_str());

	while(build_orders.begin() != build_orders.end()) {
	    printf("  %d - %s\n", build_orders.front().mID, build_orders.front().mDescription.c_str());
	    build_orders.pop_front();
	}
	return 0L;
    }

    bo = &(build_orders.front()); // Shorthand.

    // Check how many units are left on this build order.
    bo_remain = db_setup_build_order_lots(aDBDesc, bo->mPartID, bo_lots);

    if (bo_remain < 1) {
	printf("Build order [%ld - %s] is depleted but still active.\n",
	       bo->mID, bo->mDescription.c_str());
	return 0L;
    }

    if (!(enc_key_id = db_add_encryption_key(aDBDesc, aSerialNr, true, 
					     aDevicePrivKey, aDevicePrivKeyLength,
					     aDevicePubKey, aDevicePubKeyLength, 
					     aDeviceBinKey, aDeviceBinKeyLength)))
	return 0L;

    //
    // Create m1_unit row.
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO m1_unit ( "
		       "   build_order_id, "
		       "   serial_nr, "
		       "   mfg_date, "
		       "   disk_serial, "
		       "   account_id, "
		       "   enc_key_id) "
		       "VALUES ( "
		       "   %ld, "
		       "   '%s', "
		       "   NOW(), "
		       "   '%s', "
		       "   %ld, "
		       "   %ld)",
		       bo->mID,
		       aSerialNr.c_str(),
		       aDiskSerialNr.c_str(),
		       aOwnerAccountID,
		       enc_key_id);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_add_m1_unit:(): Failed to insert into trusted.m1_units [%s]\n", mysql_error(aDBDesc));
	return 0;
    }
    
    // Retrieve the auto generated trusted.id column
    unit_id = (long) mysql_insert_id(aDBDesc);

    //
    // Decrease the inventory of all lots used by the build order.
    //
    db_deplete_build_order(aDBDesc, bo, 1);
    return unit_id;
}

int db_add_account(MYSQL *aTrustedDesc, 
		   MYSQL *aUntrustedDesc, 
		   string aEmail,
		   string aPassword,
		   string aPasswordHint,
		   string aFirstName,
		   string aMiddleName,
		   string aLastName,
		   string aAddress1,
		   string aAddress2,
		   string aCity,
		   string aZip,
		   string aState,
		   string aCountry,
		   string aHomePhone,
		   string aWorkPhone,
		   string aCellPhone,
		   string aComment,
		   FILE *aLog) 
{
    char *stmt;
    CStringContext ctx;
    long id;

    //
    // Create a trusted.account row
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO account ( "   
		       "   id,"
		       "   email,"
		       "   password,"
		       "   password_hint,"
		       "   first_name,"
		       "   middle_name,"
		       "   last_name,"
		       "   address1,"
		       "   address2,"
		       "   city,"
		       "   zip,"
		       "   state,"
		       "   country,"
		       "   home_phone,"
		       "   work_phone,"
		       "   cell_phone,"
		       "   free_shipping,"
		       "   tax_exempt,"
		       "   comments, "
		       "   created) "
		       "VALUES ( "
		       "   NULL, "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   0, " // Free shipping
		       "   0, " // Tax exempt
		       "   '%s', "
		       "   NOW()"
		       ")",
		       ctx_escape_mysql(aTrustedDesc, aEmail.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aPassword.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aPasswordHint.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aFirstName.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aMiddleName.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aLastName.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aAddress1.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aAddress2.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aCity.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aZip.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aState.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aCountry.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aHomePhone.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aWorkPhone.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aCellPhone.c_str(), 0, &ctx),
		       ctx_escape_mysql(aTrustedDesc, aComment.c_str(), 0, &ctx));

    if (aLog) 
	fprintf(aLog, "db_add_account: Will insert into trusted [%s]\n", stmt);


    if (mysql_query(aTrustedDesc, stmt)) {
	fprintf(stderr, "db_add_account:(): Failed to insert into trusted.account [%s]\n[%s]", mysql_error(aTrustedDesc), stmt);
	return 0;
    }

    id = (long) mysql_insert_id(aTrustedDesc);
    
    //
    // Create an untrusted.account row
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO account ( "   
		       "   id,"
		       "   email,"
		       "   password_hint,"
		       "   first_name,"
		       "   middle_name,"
		       "   last_name,"
		       "   address1,"
		       "   address2,"
		       "   city,"
		       "   zip,"
		       "   state,"
		       "   country,"
		       "   home_phone,"
		       "   work_phone,"
		       "   cell_phone,"
		       "   comments) "
		       "VALUES ( "
		       "   %ld, "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s', "
		       "   '%s' "
		       ")",
		       id,
		       ctx_escape_mysql(aUntrustedDesc, aEmail.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aPasswordHint.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aFirstName.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aMiddleName.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aLastName.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aAddress1.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aAddress2.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aCity.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aZip.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aState.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aCountry.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aHomePhone.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aWorkPhone.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aCellPhone.c_str(), 0, &ctx),
		       ctx_escape_mysql(aUntrustedDesc, aComment.c_str(), 0, &ctx));

    if (aLog) 
	fprintf(aLog, "db_add_account: Will insert into untrusted [%s]\n", stmt);

    if (mysql_query(aUntrustedDesc, stmt)) {
	fprintf(stderr, "db_add_account:(): Failed to insert into untrusted.account [%s]\n[%s]", mysql_error(aUntrustedDesc), stmt);
	return 0;
    }

    // Retrieve the auto generated trusted.id column
    return id;
}


int db_edit_account(MYSQL *aTrustedDesc, 
		    MYSQL *aUntrustedDesc, 
		    FILE *aLog,
		    string aAccountEmail,
		    string aEmail, bool aUpdateEmail,
		    string aPassword, bool aUpdatePassword,
		    string aPasswordHint, bool aUpdatePasswordHint,
		    string aFirstName, bool aUpdateFirstName,
		    string aMiddleName, bool aUpdateMiddleName,
		    string aLastName, bool aUpdateLastName,
		    string aAddress1, bool aUpdateAddress1,
		    string aAddress2, bool aUpdateAddress2,
		    string aCity, bool aUpdateCity,
		    string aZip, bool aUpdateZip,
		    string aState, bool aUpdateState,
		    string aCountry, bool aUpdateCountry,
		    string aHomePhone, bool aUpdateHomePhone,
		    string aWorkPhone, bool aUpdateWorkPhone,
		    string aCellPhone, bool aUpdateCellPhone,
		    string aComment, bool aUpdateComment,
		    CM1UnitList &aOwnedUnits)
{

    char *stmt;
    CStringContext ctx;
    long id;
    CM1UnitListIterator unit_iter = aOwnedUnits.begin();

    char *cols;
    char *vals;
    bool c_flg = false;

    // Check that 
    if (!aUpdateEmail && !aUpdatePassword && !aUpdatePasswordHint && !aUpdateFirstName &&
	!aUpdateMiddleName && !aUpdateLastName && !aUpdateAddress1 && !aUpdateAddress2 &&
	!aUpdateCity && !aUpdateZip &&!aUpdateState && !aUpdateCountry && !aUpdateHomePhone &&
	!aUpdateWorkPhone && !aUpdateCellPhone && !aUpdateComment) {
	bool updated = false;
	
	// Check if any serial numbers have been provided.
	for (CM1UnitListIterator iter = aOwnedUnits.begin(); iter != aOwnedUnits.end(); iter++) {
	    if (iter->mSerial != "")
		updated = true;
	    ++iter;
	}
	if (!updated) {
	    if (aLog)  
		fprintf(aLog, "%lu: cgi_edit_account(): Nothing to be updated for [%s]\n", time_stamp(), aAccountEmail.c_str());

    	    return 1;
	}
    }


    // Setup all stmt we need for trusted.
    stmt = ctx_strcpy("UPDATE account SET ", 0, &ctx);
    if (aUpdateEmail) { stmt = ctx_sprintf(&ctx, "%s %s email='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aEmail.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdatePassword) { stmt = ctx_sprintf(&ctx, "%s %s password='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aPassword.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdatePasswordHint) { stmt = ctx_sprintf(&ctx, "%s %s password_hint='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aPasswordHint.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateFirstName) { stmt = ctx_sprintf(&ctx, "%s %s first_name='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aFirstName.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateMiddleName) { stmt = ctx_sprintf(&ctx, "%s %s middle_name='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aMiddleName.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateLastName) { stmt = ctx_sprintf(&ctx, "%s %s last_name='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aLastName.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateAddress1) { stmt = ctx_sprintf(&ctx, "%s %s address1='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aAddress1.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateAddress2) { stmt = ctx_sprintf(&ctx, "%s %s address2='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aAddress2.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateCity) { stmt = ctx_sprintf(&ctx, "%s %s city='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aCity.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateZip) { stmt = ctx_sprintf(&ctx, "%s %s zip='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aZip.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateState) { stmt = ctx_sprintf(&ctx, "%s %s state='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aState.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateCountry) { stmt = ctx_sprintf(&ctx, "%s %s country='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aCountry.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateHomePhone) { stmt = ctx_sprintf(&ctx, "%s %s home_phone='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aHomePhone.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateWorkPhone) { stmt = ctx_sprintf(&ctx, "%s %s work_phone='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aWorkPhone.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateCellPhone) { stmt = ctx_sprintf(&ctx, "%s %s cell_phone='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aCellPhone.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateComment) { stmt = ctx_sprintf(&ctx, "%s %s comments='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aTrustedDesc, aComment.c_str(), 0, &ctx)); c_flg = true; }

    stmt = ctx_sprintf(&ctx, "%s WHERE email='%s'", stmt, ctx_escape_mysql(aTrustedDesc, aAccountEmail.c_str(), 0, &ctx));
    if (aLog) 
	fprintf(aLog, "%lu: cgi_edit_account(): Updating trusted database: [%s]\n", time_stamp(), stmt);


    if (mysql_query(aTrustedDesc, stmt)) {
	fprintf(stderr, "db_add_account:(): Failed to insert into trusted.account [%s]\n[%s]", mysql_error(aTrustedDesc), stmt);
	return 0;
    }
    fprintf(aLog, "%lu: cgi_edit_account(): Done updating trusted database.\n", time_stamp());
    // 
    // Wipe all old units owned by the account.
    //
    if (aOwnedUnits.size() > 0) {
	CAccount acc;
	if (aUpdateEmail)
	    db_find_account_by_email(aTrustedDesc, aEmail, &acc);
	else
	    db_find_account_by_email(aTrustedDesc, aAccountEmail, &acc);

	stmt = ctx_sprintf(&ctx, "UPDATE m1_unit SET account_id=1 WHERE id = %ld", acc.mDbID);
	if (mysql_query(aTrustedDesc, stmt)) {
	    fprintf(stderr, "db_add_account:(): Failed to unset ownership of m1_units [%s]\n[%s]", mysql_error(aTrustedDesc), stmt);
	    return 0;
	}

	//
	// Set new ownership on all listed units.
	//
	for (CM1UnitListIterator iter = aOwnedUnits.begin(); iter != aOwnedUnits.end(); iter++) {
	    stmt = ctx_sprintf(&ctx, "UPDATE m1_unit SET account_id=%ld WHERE serial_nr = '%s'", 
			       acc.mDbID, ctx_escape_mysql(aTrustedDesc, iter->mSerial.c_str(), 0, &ctx));
	    
	    if (aLog) 
		fprintf(aLog, "%lu: cgi_edit_account(): Updating m1 unit[%s] to be owned by [%s]\n", 
			time_stamp(), iter->mSerial.c_str(), aEmail.c_str());

	    if (mysql_query(aTrustedDesc, stmt)) {
		fprintf(stderr, "db_add_account:(): Failed to set ownership of m1_unit [%s]\n[%s]", mysql_error(aTrustedDesc), stmt);
		return 0;
	    }
	    if (aLog) 
		fprintf(aLog, "%lu: cgi_edit_account(): Done updating m1 unit[%s]\n", 
			time_stamp(), iter->mSerial.c_str());
	}
    }
    
    // Setup untrusted.
    // Retrieve the auto generated trusted.id column
    c_flg = false;
    stmt = ctx_strcpy("UPDATE account SET ", 0, &ctx);
    if (aUpdateEmail) { stmt = ctx_sprintf(&ctx, "%s %s email='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aEmail.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdatePasswordHint) { stmt = ctx_sprintf(&ctx, "%s %s password_hint='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aPasswordHint.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateFirstName) { stmt = ctx_sprintf(&ctx, "%s %s first_name='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aFirstName.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateMiddleName) { stmt = ctx_sprintf(&ctx, "%s %s middle_name='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aMiddleName.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateLastName) { stmt = ctx_sprintf(&ctx, "%s %s last_name='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aLastName.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateAddress1) { stmt = ctx_sprintf(&ctx, "%s %s address1='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aAddress1.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateAddress2) { stmt = ctx_sprintf(&ctx, "%s %s address2='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aAddress2.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateCity) { stmt = ctx_sprintf(&ctx, "%s %s city='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aCity.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateZip) { stmt = ctx_sprintf(&ctx, "%s %s zip='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aZip.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateState) { stmt = ctx_sprintf(&ctx, "%s %s state='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aState.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateCountry) { stmt = ctx_sprintf(&ctx, "%s %s country='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aCountry.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateHomePhone) { stmt = ctx_sprintf(&ctx, "%s %s home_phone='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aHomePhone.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateWorkPhone) { stmt = ctx_sprintf(&ctx, "%s %s work_phone='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aWorkPhone.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateCellPhone) { stmt = ctx_sprintf(&ctx, "%s %s cell_phone='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aCellPhone.c_str(), 0, &ctx)); c_flg = true; }
    if (aUpdateComment) { stmt = ctx_sprintf(&ctx, "%s %s comments='%s'", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, aComment.c_str(), 0, &ctx)); c_flg = true; }
    // Set units. Ugly for now, but this table will be dumped in the future.
    if (unit_iter != aOwnedUnits.end()) { stmt = ctx_sprintf(&ctx, "%s %s m1_serial1=UPPER('%s')", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, unit_iter->mSerial.c_str(), 0, &ctx)); c_flg = true; ++unit_iter; }

    if (unit_iter != aOwnedUnits.end()) { stmt = ctx_sprintf(&ctx, "%s %s m1_serial2=UPPER('%s')", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, unit_iter->mSerial.c_str(), 0, &ctx)); c_flg = true; ++unit_iter; }

    if (unit_iter != aOwnedUnits.end()) { stmt = ctx_sprintf(&ctx, "%s %s m1_serial3=UPPER('%s')", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, unit_iter->mSerial.c_str(), 0, &ctx)); c_flg = true; ++unit_iter; }

    if (unit_iter != aOwnedUnits.end()) { stmt = ctx_sprintf(&ctx, "%s %s m1_serial4=UPPER('%s')", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, unit_iter->mSerial.c_str(), 0, &ctx)); c_flg = true; ++unit_iter; }

    if (unit_iter != aOwnedUnits.end()) { stmt = ctx_sprintf(&ctx, "%s %s m1_serial5=UPPER('%s')", stmt, c_flg?",":"", ctx_escape_mysql(aUntrustedDesc, unit_iter->mSerial.c_str(), 0, &ctx)); c_flg = true; ++unit_iter; }

    stmt = ctx_sprintf(&ctx, "%s WHERE email='%s'", stmt, ctx_escape_mysql(aUntrustedDesc, aAccountEmail.c_str(), 0, &ctx));
    if (aLog) 
	fprintf(aLog, "%lu: Updating untrusted database: [%s]\n", time_stamp(), stmt);


    if (mysql_query(aUntrustedDesc, stmt)) {
	fprintf(stderr, "db_add_account:(): Failed to insert into untrusted.account [%s]\n[%s]", mysql_error(aUntrustedDesc), stmt);
	return 0;
    }

    if (aLog) 
	fprintf(aLog, "%lu: Done updating untrusted database.\n", time_stamp());


    return 1;
}




long db_get_serial(MYSQL *aDBDesc, 
		   string aSerial,
		   long &aDbID,
		   time_t &aAssignedTS)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    int ind = 6;
    CStringContext ctx;
    char *stmt;

    aDbID = 0;
    aAssignedTS = 0L;

    if (aSerial.length() > 6 ||
	!isalnum(aSerial[0]) || !isalnum(aSerial[1]) || !isalnum(aSerial[2]) || 
	!isalnum(aSerial[3]) || !isalnum(aSerial[4]) || !isalnum(aSerial[5])) {
	fprintf(stderr, "db_get_serial(%s) serial number is not in correct format\n",
	       aSerial.c_str());
	return 0;
    }
    while(ind--)
	aSerial[ind] = toupper(aSerial[ind]);


	
    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   id, "        // 0
		       "   UNIX_TIMESTAMP(assigned) " // 1
		       "FROM "
		       "  m1_serial "
		       "WHERE "
		       "   serial = '%s' ",
		       ctx_escape_mysql(aDBDesc, aSerial.c_str(), 0, &ctx));
     
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_serial(%s): Could not execute SQL statement[%s]: %s\n", 
	       aSerial.c_str(), stmt, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	return 0;
    }

    aDbID = atol(row[0]);
    aAssignedTS = row[1]?atol(row[1]):0L;
    mysql_free_result(res);

    return aDbID;
}

int db_get_max_serial_batch(MYSQL *aDBDesc, int &aBatch)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    int ind = 6;
    CStringContext ctx;
    char *stmt;

    aBatch = 0;
    stmt = ctx_sprintf(&ctx, "SELECT MAX(batch) FROM m1_serial");

    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_max_serial_batch(): Could not execute SQL statement[%s]: %s\n", 
	       stmt, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	return 0;
    }
    if (row[0])
	aBatch = atoi(row[0]);
    mysql_free_result(res);

    return 1;
}

int db_add_serial(MYSQL *aDBDesc, 
		  string aSerial,
		  time_t aAssignedTS, // Set to 0 for null insertion
		  int aBatch) // Sequential for each new batch of serial numbers added to table
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;
    int ind = 6;


    if (aSerial.length() > 6 ||
	!isalnum(aSerial[0]) || !isalnum(aSerial[1]) || !isalnum(aSerial[2]) || 
	!isalnum(aSerial[3]) || !isalnum(aSerial[4]) || !isalnum(aSerial[5])) {
	fprintf(stderr, "db_add_serial(%s) serial number is not in correct format\n",
	       aSerial.c_str());
	return 0;
    }
    while(ind--)
	aSerial[ind] = toupper(aSerial[ind]);


	
    if (aAssignedTS != 0L) 
	sprintf(buf, "INSERT INTO m1_serial (serial, assigned, owner_id, batch) VALUES ('%s', FROM_UNIXTIME(%lu), 1, %d)",
		aSerial.c_str(), aAssignedTS, aBatch);
    else
	sprintf(buf, "INSERT INTO m1_serial (serial, assigned, owner_id, batch) VALUES ('%s', NULL, 1, %d) ",
		aSerial.c_str(), aBatch);


    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_add_serial(%s): Could not execute SQL statement[%s]: %s\n", 
	       aSerial.c_str(), buf, mysql_error(aDBDesc));
	return 0;
    }

    return 1;
}

long db_assign_next_serial(MYSQL *aDBDesc, string &aSerial)
{
    MYSQL_RES *res;
    char buf[2048];
    MYSQL_ROW row;
    long ser_id;

    // Get first unused serial
    if (mysql_query(aDBDesc, "SELECT serial, id FROM m1_serial WHERE ISNULL(ASSIGNED) ORDER BY serial ASC LIMIT 1") != 0) {
	fprintf(stderr, "db_assign_next_serial(): Could not execute SQL statement[SELECT * FROM m1_serial WHERE ISNULL(ASSIGNED) ORDER BY id ASC LIMIT 1]: %s\n", 
	       mysql_error(aDBDesc));
	return 0;
    }
    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	if (res)
	    mysql_free_result(res);

	fprintf(stderr, "db_assign_next_serial(): No more serial numbers are available\n");
	return 0;
    }

    aSerial = row[0];
    ser_id = atoi(row[1]);
    
    mysql_free_result(res);

    // Assign the given serial.
    sprintf(buf, "UPDATE m1_serial SET assigned=NOW() WHERE id=%ld", ser_id);
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_assign_next_serial(%ld): Could not execute SQL statement[%s]: %s\n", 
	       ser_id, buf, mysql_error(aDBDesc));
	return 0;
    }

    return ser_id;
}

long db_unassign_serial(MYSQL *aDBDesc, long aDbID)
{
    char buf[2048];

    sprintf(buf, "UPDATE m1_serial SET assigned=NULL WHERE id=%ld", aDbID);
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_assign_serial(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aDbID, buf, mysql_error(aDBDesc));
	return 0;
    }

    return 1;
}


long db_create_part(MYSQL *aDBDesc,
		    string aPartNr,   // Textual part nr.
		    string aDescription) // Description of lot.
{
    CPart part;
    MYSQL_RES *res;
    char *stmt;
    CStringContext ctx;

    //
    // Create build order insert statement
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO part ( "
		       "   id, "
		       "   description) "
		       "VALUES ( "
		       "   NULL, "
		       "   '%s')",
		       ctx_escape_mysql(aDBDesc, aDescription.c_str(), aDescription.size(), &ctx));

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_create_part:(): Failed to insert into trusted.part [%s]\n", mysql_error(aDBDesc));
	return 0;
    }
    
    // Return the auto generated trusted.id column
    return (long) mysql_insert_id(aDBDesc);
    
}		   
			   


// Get all parts in the system
int db_get_all_parts(MYSQL *aDBDesc, CPartList &aResult)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    CStringContext ctx;
    char *stmt;
    int cnt = 0;

    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   id, "             // 0
		       "   part_nr, "        // 2
		       "   description "     // 3
		       "FROM "
		       "  part");
    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_all_parts(): Could not execute SQL statement[%s]: %s\n", 
	        stmt, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res) {
	mysql_free_result(res);
	return 0;
    }

    while((row  = mysql_fetch_row(res))) {
	aResult.push_back(CPart(atol(row[0]), row[1], row[2], 0)); 
	cnt++;
    }
    mysql_free_result(res);

    return cnt;
}

long db_get_part(MYSQL *aDBDesc,
		 long aPartID,
		 CPart &aResult) 
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    CStringContext ctx;
    char *stmt;

    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   id, "           // 0
		       "   part_nr, "      // 1
		       "   description "   // 2
		       "FROM "
		       "  part "
		       "WHERE "
		       "   id = %ld ",
		       aPartID);
    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_part(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aPartID, stmt, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	return 0;
    }

    aResult.mID = atol(row[0]);
    aResult.mPartNr = row[1];
    aResult.mDescription = row[2];
    mysql_free_result(res);

    return aResult.mID;
}

long db_get_part_by_part_nr(MYSQL *aDBDesc,
			    string aPartNr,
			    CPart &aResult) 
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    CStringContext ctx;
    char *stmt;

    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   id, "           // 0
		       "   part_nr, "      // 1
		       "   description "   // 2
		       "FROM "
		       "  part "
		       "WHERE "
		       "   part_nr = '%s' ",
		       ctx_escape_mysql(aDBDesc, aPartNr.c_str(), 0, &ctx));

    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_part_by_part_nr(%s): Could not execute SQL statement[%s]: %s\n", 
		aPartNr.c_str(), stmt, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res || !(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	return 0;
    }

    aResult.mID = atol(row[0]);
    aResult.mPartNr = row[1];
    aResult.mDescription = row[2];
    mysql_free_result(res);

    return aResult.mID;
}
		 
long db_create_build_order(MYSQL *aDBDesc, 
			   CPart *aPart,
			   time_t aCreated,
			   int aStatus,
			   string aDescription,
			   int aQuantity)
{
    CPart part;
    MYSQL_RES *res;
    char *stmt;
    CStringContext ctx;

    //
    // Create build order insert statement
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO build_order ( "
		       "   id, "
		       "   part_id, "
		       "   created, "
		       "   quantity, "
		       "   remaining, "
		       "   description, "
		       "   status) "
		       "VALUES ( "
		       "   NULL, "
		       "   %ld, "
		       "   FROM_UNIXTIME(%lu), "
		       "   %d, "
		       "   %d, "
		       "   '%s', "
		       "   %d)",
		       aPart->mID,
		       aCreated?aCreated:time(0),
		       aQuantity,
		       aQuantity,
		       ctx_escape_mysql(aDBDesc, aDescription.c_str(), aDescription.size(), &ctx),
		       aStatus);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_create_build_order:(): Failed to insert into trusted.build_order [%s]\n", mysql_error(aDBDesc));
	return 0;
    }
    
    // Return the auto generated trusted.id column
    return (long) mysql_insert_id(aDBDesc);
}





int db_get_bom_by_id(MYSQL *aDBDesc, 
		     long aPartID,
		     CPartList &aResult) // The Part we want BOM for.
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    CStringContext ctx;
    char *stmt;
    int cnt = 0;

    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   part.id, "           // 0
		       "   part.part_nr, "      // 1
		       "   part.description, "  // 2
		       "   bom.quantity "          // 3
		       "FROM "
		       "  part, "
		       "  bom  "
		       "WHERE "
		       "   part.id=bom.src_part_id AND "
		       "   bom.dst_part_id = %ld ",
		       aPartID);
    
    if (mysql_query(aDBDesc, stmt) != 0) {
	fprintf(stderr, "db_get_bom(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aPartID, stmt, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res) {
	mysql_free_result(res);
	return 0;
    }

    while((row  = mysql_fetch_row(res))) {
	aResult.push_back(CPart(atol(row[0]), // mID
				row[1],
				row[2],
				atoi(row[3])));
	cnt++;

    }
    mysql_free_result(res);

    return cnt;
}



int db_get_bom_by_part_nr(MYSQL *aDBDesc, 
	       string aPartNr,
	       CPartList &aResult) // The Part we want BOM for.
{
    CPart target_part;
    if (!db_get_part_by_part_nr(aDBDesc, aPartNr, target_part))
	return -1;
    
    return db_get_bom_by_id(aDBDesc, target_part.mID, aResult);
}


int db_get_build_order(MYSQL *aDBDesc, 
		       long aPartID, 
		       int aStatus,  
		       CBuildOrderList &aResult)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    CStringContext ctx;
    char buf[512];
    int cnt = 0;

    sprintf(buf, 
	    "SELECT "
	    "   id, "            // 0
	    "   part_id, "       // 1
	    "   created, "       // 2
	    "   quantity, "      // 3
	    "   remaining, "     // 4
	    "   description, "   // 5
	    "   status "         // 6
	    "FROM "
	    "  build_order ");
    
    if (aPartID != DB_ALL)
	sprintf(buf + strlen(buf), "WHERE part_id = %ld ", aPartID);

    if (aStatus != DB_ALL)
	sprintf(buf + strlen(buf) , "%s status = %d ", (aPartID == DB_ALL)?"WHERE":"AND", aStatus);;

    strcat(buf + strlen(buf), "ORDER BY created ASC");

    if (mysql_query(aDBDesc, buf) != 0 || !(res = mysql_store_result(aDBDesc))) {
	fprintf(stderr, "db_get_build_order(%ld): Could not execute SQL statement[%s]: %s\n", 
		aPartID, buf, mysql_error(aDBDesc));
	
	return -1;
    }

    while((row = mysql_fetch_row(res))) {
	aResult.push_back(CBuildOrder(atol(row[0]), // mID
				      atol(row[1]), // mPartID,
				      atol(row[2]), // mCreated
				      atol(row[3]), // mQuantity
				      atol(row[4]), // mRemaining
				      row[5],
				      atoi(row[6])));
	++cnt;
    }
    mysql_free_result(res);

    return cnt;
}

			   
//
// Decrease relevant lot count.
// May deactivate lots as needed.
// May complete a build order.
//
bool db_deplete_build_order(MYSQL *aDBDesc, 
			    CBuildOrder *aBuildOrder,
			    int aQuantity) // Number of units to deplete.
{
    CPartList parts;
    CPart target_part;
    int res;
    bool finished = false;
    char buf[512];

    if (!db_get_part(aDBDesc, aBuildOrder->mPartID, target_part)) {
	fprintf(stderr, "Could not get part [%ld]\n", aBuildOrder->mPartID);
	return false;
    }

    res = db_get_bom_by_id(aDBDesc, target_part.mID, parts); // Retrieve the BOM
    if (res <= 0) {
	fprintf(stderr, "Could not get BOM for part [%ld]\n", target_part.mID);
	return false;
    }


    //
    // Deplete build_order row in db.
    //
    aBuildOrder->mRemaining -= aQuantity;
    sprintf(buf, 
	    "UPDATE "
	    "  build_order "
	    "SET "
	    "  remaining = %d "
	    "WHERE "
	    "  id = %ld",
	    aBuildOrder->mRemaining,
	    aBuildOrder->mID);
    
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_deplete_build_order(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aBuildOrder->mID, buf, mysql_error(aDBDesc));
	return false;
    }
    
    printf("%ld is made out of %d parts\n", target_part.mID, res);

    
    //
    // Walk through all parts, find the active lot for each part,
    // and decrease the remaining units for that lot.
    // If lot is depleted, we must deactivate that lot and the
    // current build order and activate the next lot for the same
    // part number and the next build order.
    //
    for (CPartListIterator iter = parts.begin(); iter != parts.end(); ++iter) {
	CLotList lots;
	int cnt = db_get_lots(aDBDesc, iter->mID, DB_STATUS_ACTIVE, lots);
	int rem;

	if (cnt == 0) {
	    printf("Part [%s] has no active lots. Zero units can be built.\n", iter->mPartNr.c_str());
	    return false;
	}

	if (cnt > 1) {
	    printf("Part [%s] has %d active lots. Only the first one will be used.\n", iter->mPartNr.c_str(), cnt);
	    return false;
	}
	rem = db_deplete_lot(aDBDesc, iter->mQuantity, lots.front());
	if (rem <= 0) {
	    printf("Lot [%d - %s] depleted (%d). Deactiving\n", 
		   lots.front().mID, lots.front().mDescription.c_str(), rem);

	    db_deactivate_lot(aDBDesc, lots.front().mID);
	    finished = true;
	}
    }

    // If one ore more lots have been depleted, deactivate the build order.
    if (finished || aBuildOrder->mRemaining == 0) {
	if (aBuildOrder->mRemaining > 0) 
	    printf("WARNING: Build order [%ld - %s] still has %d units left, but one or more lots are depleted. Deactivating\n", 
		   aBuildOrder->mID, aBuildOrder->mDescription.c_str(), aBuildOrder->mRemaining);

	db_deactivate_build_order(aDBDesc, aBuildOrder->mID);
	printf("This was the last unit of old build order [%ld - %s].\n", 
	       aBuildOrder->mID, 
	       aBuildOrder->mDescription.c_str());
	return true;
    }

    printf("There are [%d] units left to be built from build order [%ld - %s]\n", 
	   aBuildOrder->mRemaining, aBuildOrder->mID, aBuildOrder->mDescription.c_str());
}
			   

//
// Deactivate a single lot.
//
bool db_deactivate_lot(MYSQL *aDBDesc, 
		      long aLotID)
{
    char buf[512];

    sprintf(buf, 
	    "UPDATE "
	    "  lot "
	    "SET "
	    "  status = %d "
	    "WHERE "
	    "  id = %ld",
	    DB_STATUS_COMPLETE,
	    aLotID);
    
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_deactivate_lot(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aLotID, buf, mysql_error(aDBDesc));
	return false;
    }

    return true;
}

//
// Attempt to activate all lots necessary for the given part nr.
// Return number of aPartID units that can be built.
// 0 = lot shortage.
//
int db_setup_build_order_lots(MYSQL *aDBDesc, 
			      long aPartID, // Part to setup assembly lots for
			      CLotList &aActiveLots) // All active lots
{
    CPartList s_parts;
    int bom_cnt;
    int qty = -1;

    //
    // Retrieve the BOM for the partn
    //
    bom_cnt = db_get_bom_by_id(aDBDesc, aPartID, s_parts); // Retrieve the BOM for aPartNr

    if (bom_cnt <= 0) {
	printf("db_setup_lots(): Bill Of Material could not be retreived for part nr [%ld]\n", aPartID);
    }
   
    //
    // Walk through the BOM and check that we have active ltos
    // available for each included part, or can activate a lot for it.
    //
    while(s_parts.begin() != s_parts.end()) {
	CLotList active_lots;
	CLot new_active_lot;
	int a_cnt = db_get_lots(aDBDesc, s_parts.front().mID, DB_STATUS_ACTIVE, active_lots);

	if (a_cnt <= 0) {
	    //	    printf("BOM part [%s] has no active lot, trying to activate one.\n", s_parts.front().mPartNr.c_str());
	    if (!db_activate_next_lot(aDBDesc, s_parts.front().mID, new_active_lot)) {
		//		printf("BOM part [%s] has no more pending lots to activate.\n", s_parts.front().mPartNr.c_str());
		aActiveLots.push_back(CLot(-1, s_parts.front().mID, 0, -1, -1, "", "", -1));
		qty = 0;
		s_parts.pop_front();
		continue;
	    }
// 	    printf("BOM part [%s] has new lot [%s] as its active lot with [%d] remaining units\n", 
// 		   s_parts.front().mPartNr.c_str(), new_active_lot.mPO.c_str(), new_active_lot.mRemaining);

	    if (qty == -1 || qty > new_active_lot.mRemaining)
		qty= new_active_lot.mRemaining;

	    aActiveLots.push_back(new_active_lot);
	    s_parts.pop_front();
	    continue;
	}

	if (a_cnt > 1) 
	    printf("WARNING: BOM part [%s] has [%d] active lots. Only one is suppsed to be active. Using the first.\n", 
		   s_parts.front().mPartNr.c_str(), active_lots.size());

// 	printf("BOM part [%s] has old lot [%s] as its active lot with [%d] remaining units\n", 
// 	       s_parts.front().mPartNr.c_str(), active_lots.front().mPO.c_str(), active_lots.front().mRemaining);

	if (qty == -1 || qty > active_lots.front().mRemaining)
	    qty= active_lots.front().mRemaining;
	
	aActiveLots.push_back(active_lots.front());
	s_parts.pop_front();
    }

    return qty;
}




bool db_activate_next_lot(MYSQL *aDBDesc, 
			  long aPartID, // Part to setup assembly lots for
			  CLot &aNewActiveLot) // All active lots
{
    char buf[512];
    CLotList lst;
    int lot_cnt; 
    
    lot_cnt = db_get_lots(aDBDesc, 
			  aPartID, 
			  DB_STATUS_PENDING,
			  lst);

    // No more lots pending?
    if (lot_cnt <= 0) 
	return false;

    sprintf(buf, 
	    "UPDATE "
	    "  lot "
	    "SET "
	    "  status = %d "
	    "WHERE "
	    "  id = %ld",
	    DB_STATUS_ACTIVE, 
	    lst.front().mID);
    
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_activate_next_lot(%ld): Could not execute SQL statement[%s]: %s\n", 
	       lst.front().mID, buf, mysql_error(aDBDesc));
	return false;
    }

    aNewActiveLot = lst.front();
    return true;
}
			   

//
// Deactivate a single build order.
//
bool db_deactivate_build_order(MYSQL *aDBDesc, 
			       long aBuildOrderID)
{
    char buf[512];

    sprintf(buf, 
	    "UPDATE "
	    "  build_order "
	    "SET "
	    "  status = %d "
	    "WHERE "
	    "  id = %ld",
	    DB_STATUS_COMPLETE,
	    aBuildOrderID);
    
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_deactivate_build_order(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aBuildOrderID, buf, mysql_error(aDBDesc));
	return false;
    }

    return true;
}



long db_create_lot(MYSQL *aDBDesc,
 		   int aPartID, // Part # of lot.
 		   time_t aCreated,  // When was lot created
 		   int aQuantity, // Quantity of aPartID in lot. remaining col will be set to this value.
 		   string aPO, // Purchase order string. Ties into QB PO
 		   string aDescription, // Description of lot.
 		   int aStatus) 
{
    CPart part;
    MYSQL_RES *res;
    char *stmt;
    CStringContext ctx;

    //
    // Create build order insert statement
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO lot ( "
		       "   id, "
		       "   part_id, "
		       "   created, "
		       "   quantity, "
		       "   remaining, "
		       "   po, "
		       "   description, "
		       "   status) "
		       "VALUES ( "
		       "   NULL, "
		       "   %ld, "
		       "   FROM_UNIXTIME(%lu), "
		       "   %d, "
		       "   %d, "
		       "   '%s', "
		       "   '%s', "
		       "   %d)",
		       aPartID,
		       aCreated?aCreated:time(0),
		       aQuantity,
		       aQuantity,
		       ctx_escape_mysql(aDBDesc, aPO.c_str(), aPO.size(), &ctx),
		       ctx_escape_mysql(aDBDesc, aDescription.c_str(), aDescription.size(), &ctx),
		       aStatus);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_create_build_lot:(): Failed to insert into trusted.lot [%s]\n", mysql_error(aDBDesc));
	return 0;
    }
    
    // Return the auto generated trusted.id column
    return (long) mysql_insert_id(aDBDesc);
}		   


long db_get_lots(MYSQL *aDBDesc,
		 long aPartID, 
		 int aStatus,
		 CLotList &aResult)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    char buf[256];
    int cnt = 0;

    sprintf(buf, 
	    "SELECT "
	    "   id, "          // 0
	    "   part_id, "     // 1
	    "   UNIX_TIMESTAMP(created), "     // 2
	    "   quantity, "    // 3
	    "   remaining, "   // 4
	    "   po, "          // 5
	    "   description, " // 6
	    "   status "       // 7
	    "FROM "
	    "  lot ");

    if (aPartID != DB_ALL)
	sprintf(buf + strlen(buf), "WHERE part_id = %ld ", aPartID);

    if (aStatus != DB_ALL)
	sprintf(buf + strlen(buf) , "%s status = %d ", (aPartID == DB_ALL)?"WHERE":"AND", aStatus);
	
    strcat(buf + strlen(buf), "ORDER BY part_id ASC, created ASC");

    if (mysql_query(aDBDesc,  buf) != 0) {
	fprintf(stderr, "db_get_lots(): Could not execute SQL statement[%s]: %s\n", 
	        buf, mysql_error(aDBDesc));
	
	return 0;
    }

    res = mysql_store_result(aDBDesc); 
    if (!res) {
	mysql_free_result(res);
	return 0;
    }

    while((row  = mysql_fetch_row(res))) {
	aResult.push_back(CLot(atol(row[0]),  // mID
			       atol(row[1]),  // mPartID
			       atol(row[2]),  // mCreated
			       atoi(row[3]),  // mQuantity
			       atoi(row[4]),  // mRemaining
			       row[5],        // mPO
			       row[6],        // mDescription
			       atoi(row[7]))); // mStatus
	cnt++;
    }
    mysql_free_result(res);

    return cnt;
}		   

			   
//
// Deplete a single lot.
// Return the number of remaining units in this lot.
//
int db_deplete_lot(MYSQL *aDBDesc, 
		   int aQuantity,
		   CLot &aLot)
{
    char buf[512];

    aLot.mRemaining -= aQuantity;
    sprintf(buf, 
	    "UPDATE "
	    "  lot "
	    "SET "
	    "  remaining = %d "
	    "WHERE "
	    "  id = %ld",
	    aLot.mRemaining,
	    aLot.mID);
    
    if (mysql_query(aDBDesc, buf) != 0) {
	fprintf(stderr, "db_deplete_lot(%ld): Could not execute SQL statement[%s]: %s\n", 
	       aLot.mID, buf, mysql_error(aDBDesc));
	return 0;
    }

    return aLot.mRemaining;
}
			   

long db_create_sales_order(MYSQL *aDBDesc,
			   time_t aCreated,
			   string aTransNr,
			   string aOrderID,
			   CAccount *aAccount,  
			   CPart *aPart,
			   int aQuantity,
			   float aPrice,
			   float aShippingFee,
			   float aTax)
{
    CPart part;
    MYSQL_RES *res;
    char *stmt;
    CStringContext ctx;


    //
    // Create build order insert statement
    // 
    stmt = ctx_sprintf(&ctx, 
		       "INSERT INTO sales_order ( "
		       "   id, "
		       "   created, "
		       "   account_id, "
		       "   trans_nr, "
		       "   order_id, "
		       "   quantity, "
		       "   part_nr, "
		       "   subtotal, "
		       "   sh_fee, "
		       "   tax) "
		       "VALUES ( "
		       "   NULL, "
		       "   FROM_UNIXTIME(%lu), "
		       "   %ld, "
		       "   '%s', "
		       "   '%s', "
		       "   %d, "
		       "   '%s', "
		       "   %f, "
		       "   %f, "
		       "   %f)",
		       aCreated?aCreated:time(0),
		       aAccount->mDbID,
		       ctx_escape_mysql(aDBDesc, aTransNr.c_str(), aTransNr.size(), &ctx),
		       ctx_escape_mysql(aDBDesc, aOrderID.c_str(), aOrderID.size(), &ctx),
		       aQuantity,
		       ctx_escape_mysql(aDBDesc, aPart->mPartNr.c_str(), aPart->mPartNr.size(), &ctx),
		       aPrice,
		       aShippingFee, 
		       aTax);

    if (mysql_query(aDBDesc, stmt)) {
	fprintf(stderr, "db_create_sales_order:(): Failed to insert into trusted.lot [%s]\n", mysql_error(aDBDesc));
	return 0;
    }
    
    // Return the auto generated trusted.id column
    return (long) mysql_insert_id(aDBDesc);
}		   

int db_get_sale_order(MYSQL *aDBDesc, 
		      CSalesOrderList &aResult)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    char buf[512];
    int cnt = 0;

    sprintf(buf, 
	    "SELECT "
	    "   id, "            // 0
	    "   UNIX_TIMESTAMP(created), " // 1
	    "   account_id, "    // 2
	    "   trans_nr, "      // 3
	    "   quantity, "      // 4
	    "   part_nr, "       // 5
	    "   subtotal, "      // 6
	    "   sh_fee, "         // 7
	    "   tax "             // 8
	    "FROM "
	    "  sales_order "    
	    "ORDER BY created ASC");

    if (mysql_query(aDBDesc, buf) != 0 || !(res = mysql_store_result(aDBDesc))) {
	printf("db_get_sales_order(): Could not execute SQL statement[%s]: %s\n", 
		buf, mysql_error(aDBDesc));
	
	return -1;
    }

    while((row = mysql_fetch_row(res))) {
	aResult.push_back(CSalesOrder(atol(row[0]), // mID
				      atol(row[1]), // mCreated
				      atol(row[2]), // mAccountID
				      row[3], // mTransNr
				      row[5], // mPartNr
				      atol(row[4]), // mQuantity
				      atof(row[6]), // mPrice
				      atof(row[8]), // mTax
				      atof(row[7]))); // mShippingFee

	++cnt;
    }
    mysql_free_result(res);

    return cnt;
}

bool db_get_discount(MYSQL *aDBDesc, 
		     long aAccountID,
		     string aPartNr,
		     int &aResultStartVolume,
		     float &aResultPercent,
		     float &aResultPrice)
{
    MYSQL_RES *res;
    MYSQL_ROW row;
    int cnt = 0;
    long sales_count = 0;
    CPart part;
    char *stmt;
    CStringContext ctx;


    //
    // Create build order insert statement
    // 
    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "  SUM(quantity) " // 0
		       "FROM "
		       "  sales_order "    
		       "WHERE "
		       "  part_nr = '%s' AND "
		       "  account_id = %ld",
		       ctx_escape_mysql(aDBDesc, aPartNr.c_str(), aPartNr.size(), &ctx),
		       aAccountID);


    if (mysql_query(aDBDesc, stmt) != 0 || !(res = mysql_store_result(aDBDesc))) {
	fprintf(stderr, "db_get_discount(%ld, %s): Could not execute SQL statement[%s]: %s\n", 
		aAccountID,
		aPartNr.c_str(),
		stmt, mysql_error(aDBDesc));
	
	return false;
    }


    if ((row = mysql_fetch_row(res)) && row[0]) {
	sales_count = atol(row[0]);
		
    } else
	sales_count = 0;

    mysql_free_result(res);
    fprintf(stderr, "db_get_discount(%ld, %s): %ld sales found\n",  aAccountID, aPartNr.c_str(), sales_count);

    stmt = ctx_sprintf(&ctx, 
		       "SELECT "
		       "   start_volume, " // 0
		       "   percent, "      // 1
		       "   price "         // 2
		       "FROM "
		       "  discount "    
		       "WHERE "
		       "  part_nr = '%s' AND "
		       "  account_id = %ld AND "
		       "  start_volume <= %ld "
		       "ORDER BY start_volume DESC",
		       ctx_escape_mysql(aDBDesc, aPartNr.c_str(), aPartNr.size(), &ctx),
		       aAccountID,
		       sales_count);

    if (mysql_query(aDBDesc, stmt) != 0 || !(res = mysql_store_result(aDBDesc))) {
	fprintf(stderr, "db_get_discount(%ld, %s): Could not execute SQL statement[%s]: %s\n", 
		aAccountID,
		aPartNr.c_str(),
		stmt, mysql_error(aDBDesc));
	
	return false;
    }


    if (!(row = mysql_fetch_row(res))) {
	mysql_free_result(res);
	fprintf(stderr, "db_get_discount(%ld, %s): No discount found\n",
		aAccountID, aPartNr.c_str());
	return false;
		
    }
    aResultStartVolume = atoi(row[0]);
    aResultPercent = atof(row[1]);
    aResultPrice = atof(row[2]);
    mysql_free_result(res);
    return true;
}
