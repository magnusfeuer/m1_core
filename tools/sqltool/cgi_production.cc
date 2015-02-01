//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Manage lots, build orders and part numbers in an SQL:y kind of way. 
// No security.
//
#include "sqlcommon.hh"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include "cgicommon.hh"
char *glob_argv0;



void list_parts(MYSQL *aDB)
{
    CPartList lst;
    db_get_all_parts(aDB, lst);
    puts("<table  border=1>");
    puts("<tr><th>Part #</th><th>Description</th></tr>");
    while(lst.begin() != lst.end()) {
	puts("<tr>");
	printf("<td>%s</td><td>%s</td>\n", 
	       lst.front().mPartNr.c_str(),
	       lst.front().mDescription.c_str());
	lst.pop_front();
	puts("</tr>");
    }
    puts("</table>");
}


void list_sales_order(MYSQL *aDB)
{
    CSalesOrderList lst;
    db_get_sale_order(aDB, lst);
    puts("<table  border=1 width=2000>");
    puts("<tr>");
    puts("<th>Order date</th>");
    puts("<th>Part #</th>");
    puts("<th>Qty</th>");
    puts("<th>Trans #</th>");
    puts("<th>Price</th>");
    puts("<th>Tax</th>");
    puts("<th>S&H</th>");
    puts("<th>Name</th>");
    puts("<th>Address</th>");
    puts("<th>City</th>");
    puts("<th>State</th>");
    puts("<th>Zip</th>");
    puts("<th>Country</th>");
    puts("<th>Home #</th>");
    puts("<th>Work #</th>");
    puts("<th>Cell #</th>");
    puts("<th>Email</th>");


    puts("</tr>");
    while(lst.begin() != lst.end()) {
	CAccount acct;
	puts("<tr>");
	if (!db_find_account_by_id(aDB, lst.front().mAccountID, &acct)) {
	    printf("<td>Could not find account with DB[%ld]</td></tr>\n", lst.front().mAccountID);
	    continue;
	}

	printf("<td>%.24s</td>\n", ctime(&(lst.front().mCreated))); // Order date
	printf("<td>%s</td>\n", lst.front().mPartNr.c_str()); // Part nr.
	printf("<td>%d</td>\n", lst.front().mQuantity); // Qty
	printf("<td>%s</td>\n", lst.front().mTransNr.c_str()); // Credit card transaction number
	printf("<td>%f</td>\n", lst.front().mPrice); // Total price, less shipping
	printf("<td>%f</td>\n", lst.front().mTax); // Shipping & Handling
	printf("<td>%f</td>\n", lst.front().mShippingFee); // Shipping & Handling

	printf("<td>%s %s %s</td>\n",  // Name
	       acct.mFirstName.c_str() , acct.mMiddleName.c_str(), acct.mLastName.c_str());

	printf("<td>%s %s</td>\n",  acct.mAddress1.c_str(), acct.mAddress2.c_str()); // Addr
	printf("<td>%s</td>\n",  acct.mCity.c_str()); // City
	printf("<td>%s</td>\n",  acct.mState.c_str()); // State
	printf("<td>%s</td>\n",  acct.mZip.c_str()); // Zip
	printf("<td>%s</td>\n",  acct.mCountry.c_str()); // Country
	printf("<td>%s</td>\n",  acct.mHomePhone.c_str()); // Home phone
	printf("<td>%s</td>\n",  acct.mWorkPhone.c_str()); // Work phone
	printf("<td>%s</td>\n",  acct.mCellPhone.c_str()); // Cell phone
	printf("<td>%s</td>\n",  acct.mEmail.c_str()); // Email
	lst.pop_front();
	puts("</tr>");
    }
    puts("</table>");
}



void list_lots(MYSQL *aDB)
{
    char *status[] = { "??", "pending", "active", "depleted", "cancelled" };
    CLotList lst;
    db_get_lots(aDB, DB_ALL, DB_ALL, lst);
    puts("<table  border=1>");
    puts("<tr><th>Part #</th><th>Description</th><th>Creation date</th><th>Qty</th><th>Remain</th><th>PO</th><th>Status</th><th>Description</th></tr>");
    while(lst.begin() != lst.end()) {
	CPart part;
	db_get_part(aDB, lst.front().mPartID, part);
	puts("<tr>");
	printf("<td>%s</td><td>%s</td><td>%.24s</td><td>%ld</td><td>%ld</td><td>%s</td><td>%s</td><td>%s</td>\n", 
	       part.mPartNr.c_str(),
	       part.mDescription.c_str(),
	       ctime(&(lst.front().mCreated)),
	       lst.front().mQuantity,
	       lst.front().mRemaining, 
	       lst.front().mPO.c_str(),
	       status[lst.front().mStatus], 
	       lst.front().mDescription.c_str());
	       
	lst.pop_front();
	puts("</tr>");
    }
    puts("</table>");
}


void list_build_orders(MYSQL *aDB, CKeyValueList &aList)
{
    char *status[] = { "??", "pending", "active", "depleted", "cancelled" };
    CBuildOrderList lst;
    long part_id = -1;
    CPart part;
    string part_nr = aList.find("list_bo_part_nr");

    puts("<br><br>");
    if (part_nr != "") {
	if (!db_get_part_by_part_nr(aDB, part_nr, part)) {
	    printf("Unknown part number [%s]<br>\n",  part_nr.c_str());
	    return;
	}
	part_id = part.mID;
    }
    db_get_build_order(aDB, part_id, DB_ALL, lst);

    puts("<table  border=1>");
    puts("<tr><th>Part #</th><th>Creation date</th><th>Completed</th><th>Remain</th><th>Status</th><th>Description</th></tr>");

    while(lst.begin() != lst.end()) {
	db_get_part(aDB, lst.front().mPartID, part);
	puts("<tr>");
	printf("<td>%s</td><td>%.24s</td><td>%ld</td><td>%ld</td><td>%s</td><td>%s</td>\n", 
	       part.mPartNr.c_str(),
	       ctime(&(lst.front().mCreated)),
	       lst.front().mQuantity,
	       lst.front().mRemaining,
	       status[lst.front().mStatus], 
	       lst.front().mDescription.c_str());
	       
	lst.pop_front();
	puts("</tr>");
    }
    puts("</table>");
}

void list_build_order_quantity(MYSQL *aDB, CKeyValueList &aList)
{
    CLotList lst;
    CPart part;
    int res;
    CBuildOrderList existing_bo;
    string part_nr = aList.find("list_bo_qty_part_nr");
    char *status[] = { "??", "pending", "active", "depleted", "cancelled" };

    puts("<br><br>");
    if (!db_get_part_by_part_nr(aDB, part_nr, part)) {
	printf("Unknown part number [%s]<br>\n",  part_nr.c_str());
	return;
    }

    //
    // Check that there is no active build order for the given part
    //
    if (db_get_build_order(aDB, part.mID, DB_STATUS_ACTIVE, existing_bo) > 0) {
	printf("There is already %d active build orders for part [%s]. Complete or cancel them before querying about a new one.<br>\n", 
	       existing_bo.size(), part_nr.c_str());
	return;
    }

    res = db_setup_build_order_lots(aDB, part.mID, lst);

    //
    // List all build orders
    //
    
    printf("Current lots allows for [%d] units of part nr [%s] to be built<br><br>\n", res, part_nr.c_str());

    puts("<table  border=1>");
    puts("<tr><th>Part #</th><th>Creation date</th><th>Qty</th><th>Remain</th><th>PO</th><th>Status</th><th>Description</th></tr>");

    while(lst.begin() != lst.end()) {
	CPart part;
	db_get_part(aDB, lst.front().mPartID, part);
	puts("<tr>");
	// Check for lot shortage
	if (lst.front().mQuantity == -1) 
	    printf("<i><td>%s</td><td>n/a</td><td>n/a</td><td>n/a</td><td>n/a</td><td>n/a</td><td>No lot available</td></i>\n", 
		   part.mPartNr.c_str());
	else
	    printf("<td>%s</td><td>%.24s</td><td>%ld</td><td>%ld</td><td>%s</td><td>%s</td><td>%s</td>\n", 
		   part.mPartNr.c_str(),
		   ctime(&(lst.front().mCreated)),
		   lst.front().mQuantity,
		   lst.front().mRemaining, 
		   lst.front().mPO.c_str(),
		   status[lst.front().mStatus], 
		   lst.front().mDescription.c_str());
	       
	lst.pop_front();
	puts("</tr>");
    }
    puts("</table>");
    return;
}


void create_build_order(MYSQL *aDB, CKeyValueList &aList) //string aPartNr, string aDescription, int aQuantity)
{
    CPart part;
    CBuildOrderList existing_bo;
    int max_qty = 0;
    CLotList a_lots;
    string part_nr = aList.find("bo_part_nr");
    string description = aList.find("bo_description");
    string qty_str = aList.find("bo_quantity");
    int qty;
    long bo_id;

    puts("<br><br>");
    if (part_nr == "") {
	puts("Please enter a part number<br>\n");
	return;
    }

    if (description == "") {
	puts("Please enter a description<br>\n");
	return;
    }

    if (qty_str == "") {
	puts("Please enter a quantity<br>\n");
	return;
    }
    qty = atoi(qty_str.c_str());
    //
    // Get part
    //
    if (!(db_get_part_by_part_nr(aDB, part_nr.c_str(), part))) {
	printf("Part number [%s] does not exist<br>\n", part_nr.c_str());
	return;
    }

    //
    // Check that there is no active build order for the given part
    //
    if (db_get_build_order(aDB, part.mID, DB_STATUS_ACTIVE, existing_bo) > 0) {
	printf("There is already %d active build orders for part [%s]. Complete or cancel them before creating a new one.<br>\n", 
	       existing_bo.size(), part_nr.c_str());
	return;
    }

    
    //
    // Check new maximum qty we can build
    //
    max_qty = db_setup_build_order_lots(aDB, part.mID, a_lots);

    if (max_qty < qty) {
	printf("A build order for part [%s] cannot be more than %d units<br>\n", part_nr.c_str(), max_qty);
	return;
    }

    //
    // Create build order.
    //
    if (!(bo_id = db_create_build_order(aDB, &part, 0, DB_STATUS_ACTIVE, description, qty)) )
	printf("Could not create build order<br>\n");
    else
	printf("Build order [%ld] created.<br>\n", bo_id);

    return;
}

void create_lot(MYSQL *aDB, CKeyValueList &aList)
{
    CPart part;
    CPartList s_parts;
    int bom_cnt;
    CBuildOrderList existing_bo;
    int max_qty = 0;
    string part_nr = aList.find("lot_part_nr");
    string po = aList.find("lot_po");
    string description = aList.find("lot_description");
    string qty_str = aList.find("lot_quantity");
    int qty;
    long bo_id;

    puts("<br><br>");

    if (part_nr == "") {
	puts("Please enter a part number<br>\n");
	return;
    }

    if (po == "") {
	puts("Please enter a PO number<br>\n");
	return;
    }

    if (description == "") {
	puts("Please enter a description<br>\n");
	return;
    }

    if (qty_str == "") {
	puts("Please enter a quantity<br>\n");
	return;
    }
    qty = atoi(qty_str.c_str());

    //
    // Get part
    //
    if (!(db_get_part_by_part_nr(aDB, part_nr, part))) {
	printf("Part number [%s] does not exist<br>\n", part_nr.c_str());
	return;
    }

    if (!db_create_lot(aDB, part.mID, 0, qty, po, description, DB_STATUS_PENDING)) 
	printf("Could not create lot<br>\n");
    else
	printf("Lot created.<br>\n");
	
    return;
}

void create_account(MYSQL *aTrustedDB, MYSQL *aUntrustedDB, CKeyValueList &aList)
{
    string email = aList.find("acct_email");
    string password = aList.find("acct_password");

    puts("<br><br>");
    if (email == "") {
	puts("Please enter an email address<br>\n");
	return;
    }

    if (password == "") {
	puts("Please enter a password<br>\n");
	return;
    }

    if (!(db_add_account(aTrustedDB, aUntrustedDB,
			 email, // Email
			 password, // Password
			 aList.find("acct_password_hint"), // Passwd hint
			 aList.find("acct_first_name"), // First name
			 aList.find("acct_middle_name"), // Middle name
			 aList.find("acct_last_name"), // Last name
			 aList.find("acct_address1"), // Address1
			 aList.find("acct_address2"), // Address2
			 aList.find("acct_city"), // City
			 aList.find("acct_zip"), // Zip
			 aList.find("acct_state"), // State
			 aList.find("acct_country"), // Country
			 aList.find("acct_home_phone"), // Home Phone
			 aList.find("acct_work_phone"), // Work Phone
			 aList.find("acct_cell_phone"), // Cell phone
			 aList.find("acct_comment"), // Comment
			 0)))
	puts("Could not account<br>");
    else
	puts("Account created<br>");

    return;
}

void create_sales_order(MYSQL *aTrustedDB, CKeyValueList &aList)
{
    string email = aList.find("sales_email");
    string price_str = aList.find("sales_price");
    string sh_fee = aList.find("sales_shipping_fee");
    string tax = aList.find("sales_tax");
    string trans_nr = aList.find("sales_trans_nr");
    string order_id = aList.find("sales_order_id");
    string qty_str = aList.find("sales_quantity");
    string part_nr = aList.find("sales_part_nr");
    CAccount acct;
    CPart part;

    puts("<br><br>");
    if (email == "") {
	puts("Please enter an email address<br>\n");
	return;
    }

    if (price_str == "") {
	puts("Please enter a price<br>\n");
	return;
    }

    if (part_nr == "") {
	puts("Please enter a part number<br>\n");
	return;
    }

    if (order_id == "") {
	puts("Please enter a order_id<br>\n");
	return;
    }

    if (sh_fee == "") {
	puts("Please enter a shipping and handling fee<br>\n");
	return;
    }

    if (tax == "") 
	tax = "0.0";


    if (trans_nr == "") {
	puts("Please enter a credit card transaction number<br>\n");
	return;
    }


    if (qty_str == "") {
	puts("Please enter  quantity<br>\n");
	return;
    }

    if (!(db_find_account_by_email(aTrustedDB, email, &acct))) {
	printf("Email [%s] has no account<br>\n", email.c_str());
	return;
    }

    if (!db_get_part_by_part_nr(aTrustedDB, part_nr, part)) {
	printf("Part Number [%s] has no account<br>\n", part_nr.c_str());
	return;
    }

    if (!(db_create_sales_order(aTrustedDB,
				time(0),
				trans_nr,
				order_id,
				&acct,
				&part,
				atoi(qty_str.c_str()),
				atof(price_str.c_str()),
				atof(sh_fee.c_str()),
				atof(tax.c_str()))))
	puts("Could not create sales order<br>");
    else
	puts("Sales order created<br>");

    return;
}


void get_part_drop_down(MYSQL *aDB)
{
    CPartList parts;

    db_get_all_parts(aDB, parts);
    for(CPartListIterator iter = parts.begin(); iter != parts.end(); ++iter) 
	printf("<option>%s</option>\n", iter->mPartNr.c_str());
}

void main_page(MYSQL *aDB)
{
    puts("<html><head><title>Magden Automotive production management</title></head><body>");
    puts("<form name=\"input\" action=\"cgi_production\" method=\"post\">");
    puts("<input type=\"submit\" name=\"cmd\" value=\"list parts\"> |");
    puts("<input type=\"submit\" name=\"cmd\" value=\"list lots\"> | ");
    puts("<input type=\"submit\" name=\"cmd\" value=\"list sales orders\"><br><br>");


    puts("<hr>");
    puts("<table>");
    puts("<tr><td>Part Nr:</td>");
    puts("<td><select name=\"lot_part_nr\">");
    get_part_drop_down(aDB);
    puts("</select></td>");

    puts("<tr><td>PO#:</td><td><input size=40 type=text name=\"lot_po\"></td></tr>");
    puts("<tr><td>Quantity:</td><td><input size=40 type=text name=\"lot_quantity\"></td></tr>");
    puts("<tr><td>Description:</td><td><input size=80 type=text name=\"lot_description\"></td></tr>");
    puts("</table>");
    puts("<input type=\"submit\" name=\"cmd\" value=\"create lot\"><br><br>");

    // create build order.
    puts("<hr>");
    puts("<table>");
    puts("<tr><td>Part Nr:</td>");
    puts("<td><select name=\"bo_part_nr\">");
    get_part_drop_down(aDB);
    puts("</select></td>");

    puts("<tr><td>Quantity:</td><td><input size=40 type=text name=\"bo_quantity\"></td></tr>");
    puts("<tr><td>Description:</td><td><input size=80 type=text name=\"bo_description\"></td></tr>");
    puts("</table>");
    puts("<input type=\"submit\" name=\"cmd\" value=\"create build order\"><br><br>");
    


    

    // create lot.
    puts("<hr>");
    puts("<table>");
    puts("<tr><td>Part Nr:</td>");
    puts("<td><select name=\"lot_part_nr\">");
    get_part_drop_down(aDB);
    puts("</select></td>");

    puts("<tr><td>PO#:</td><td><input size=40 type=text name=\"lot_po\"></td></tr>");
    puts("<tr><td>Quantity:</td><td><input size=40 type=text name=\"lot_quantity\"></td></tr>");
    puts("<tr><td>Description:</td><td><input size=80 type=text name=\"lot_description\"></td></tr>");
    puts("</table>");

    puts("<input type=\"submit\" name=\"cmd\" value=\"create lot\"><br><br>");
    

    // List build orders
    puts("<hr>");
    puts("Part Nr: ");
    puts("<select name=\"list_bo_part_nr\">");
    get_part_drop_down(aDB);
    puts("</select><br>");
    puts("<input type=\"submit\" name=\"cmd\" value=\"list build orders\"><br><br>");
    


    // List build order qty
    puts("<hr>");
    puts("Part Nr: ");
    puts("<select name=\"list_bo_qty_part_nr\">");
    get_part_drop_down(aDB);
    puts("</select><br>");
    puts("<input type=\"submit\" name=\"cmd\" value=\"check build order qty\"><br><br>");

    // create sales order.
    puts("<hr>");
    puts("<table>");
    puts("<tr><td>Part Nr:</td>");
    puts("<td><select name=\"sales_part_nr\">");
    get_part_drop_down(aDB);
    puts("</select></td></tr>");

    puts("<tr><td>Quantity:</td><td><input size=40 type=text name=\"sales_quantity\"></td></tr>");
    puts("<tr><td>Price:</td><td><input type=text size=40 name=\"sales_price\"></td></tr>");
    puts("<tr><td>Shipping&Handling:</td><td><input size=40 type=text name=\"sales_shipping_fee\"></td></tr>");
    puts("<tr><td>Sales tax:</td><td><input size=40 type=text name=\"sales_tax\"></td></tr>");
    puts("<tr><td>CC Trans#:</td><td><input type=text size=40 name=\"sales_trans_nr\"></td></tr>");
    puts("<tr><td>Order ID:</td><td><input type=text size=40 name=\"sales_order_id\"></td></tr>");
    puts("<tr><td>Account email:</td><td><input size=40 type=text name=\"sales_email\"></td></tr>");
    puts("</table>");
    puts("<input type=\"submit\" name=\"cmd\" value=\"create sales order\"><br><br>");

    // create account.
    puts("<hr>");
    puts("<table>");
    puts("<tr><td>First Name:</td><td><input size=40 type=text name=\"acct_first_name\"></td></tr>");
    puts("<tr><td>Middle Name:</td><td><input size=40 type=text name=\"acct_middle_name\"></td></tr>");
    puts("<tr><td>Last Name:</td><td><input size=40 type=text name=\"acct_last_name\"></td></tr>");
    puts("<tr><td>Email:</td><td><input size=40 type=text name=\"acct_email\"></td></tr>");
    puts("<tr><td>Password:</td><td><input size=40 type=text name=\"acct_password\"></td></tr>");
    puts("<tr><td>Password Hint:</td><td><input size=80 type=text name=\"acct_password_hint\"></td></tr>");
    puts("<tr><td>Address 1:</td><td><input size=80 type=text name=\"acct_address1\"></td></tr>");
    puts("<tr><td>Address 2:</td><td><input size=80 type=text name=\"acct_address2\"></td></tr>");
    puts("<tr><td>City:</td><td><input size=40 type=text name=\"acct_city\"></td></tr>");
    puts("<tr><td>State:</td><td><input size=20 type=text name=\"acct_state\"></td></tr>");
    puts("<tr><td>Zip:</td><td><input size=5 type=text name=\"acct_zip\"></td></tr>");
    puts("<tr><td>Country:</td><td><input size=40 type=text name=\"acct_country\"></td></tr>");
    puts("<tr><td>Home phone:</td><td><input size=20 type=text name=\"acct_home_phone\"></td></tr>");
    puts("<tr><td>Work phone:</td><td><input size=20 type=text name=\"acct_work_phone\"></td></tr>");
    puts("<tr><td>Cell phone:</td><td><input size=20 type=text name=\"acct_cell_phone\"></td></tr>");
    puts("<tr><td>Comment:</td><td><textarea rows=10 cols=80 type=text name=\"acct_comment\"></textarea></td></tr>");
    puts("<tr><td>Discount %:</td><td><input size=10 type=text value=\"0\" name=\"acct_discount\"></td></tr>");
    puts("<tr><td>Reseller:</td><td><input type=checkbox value=\"1\" name=\"acct_reseller\"></td></tr>");
    puts("</table>");
    puts("<input type=\"submit\" name=\"cmd\" value=\"create account\"><br><br>");
    puts("</form></body></html>");

}






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
    string cmd;

    time_t now = time(0);

    glob_argv0 = argv[0];
    puts("Content-type: text/html\r\n\r\n<html>\n<head>\n</head>\n<body>");

    //
    // Connect to database
    //
    if (!db_connect(&db_t, TRUSTED_HOST,  TRUSTED_USER, TRUSTED_PASSWD, TRUSTED_DB)) {
	printf("Could not connect to host[%s] database [%s] user[%s]<br>\n",
	       TRUSTED_HOST, TRUSTED_DB, TRUSTED_PASSWD);

	db_t = 0;
	goto end;
    }

    if (!db_connect(&db_u, UNTRUSTED_HOST,  UNTRUSTED_USER, UNTRUSTED_PASSWD, UNTRUSTED_DB)) {
	printf("Could not connect to host[%s] database [%s] user[%s]<br>\n",
	       UNTRUSTED_HOST, UNTRUSTED_DB, UNTRUSTED_PASSWD);

	db_u = 0;
	goto end;
    }
    
    if (!(env = getenv("REMOTE_ADDR")) || strncmp(env, "192.168.0.", 10 )) {
 	puts("Permission denied.");
 	exit(0);
    }

    if (!(env = getenv("CONTENT_LENGTH"))) {
	goto end;
    }
    query_len = atoi(env);
    
    if (!(query = read_query_field(0, query_len))) {
	puts("Could not read input");
	goto end;
    }
    parse_query_field(query, kvp_lst);
    decode_query_fields(kvp_lst);

    free(query);
    //    log_query_fields(stdout, kvp_lst);

    if ((cmd = kvp_lst.find("cmd")) == "") {
	puts("No cmd query string field");
    }

    if (cmd == "list parts") {
	list_parts(db_t);
    }

     if (cmd == "list lots") {
 	list_lots(db_t);

     }


     if (cmd == "create lot") {
	 create_lot(db_t, kvp_lst);
     }

     if (cmd == "list build orders") {
 	list_build_orders(db_t, kvp_lst);
     }

     if (cmd == "check build order qty") {
 	list_build_order_quantity(db_t, kvp_lst);
     }

     if (cmd == "create build order") {
 	create_build_order(db_t, kvp_lst);
     }

     if (cmd == "create account") {
	 create_account(db_t, db_u, kvp_lst);
     }

     if (cmd == "list sales orders") {
	 list_sales_order(db_t);
     }


     if (cmd == "create sales order") {
	 create_sales_order(db_t, kvp_lst);
     }

//     if (cmd == "create_part") {
// 	if (n_opt == "" || t_opt == "" || d_opt == "") 
// 	    goto fail;
//     }
     puts("<hr><br>");
 end:
     main_page(db_t);
     puts("</body>\n</html>");

    if (db_t)
	mysql_close(db_t);

    if (db_u)
	mysql_close(db_u);
    
    exit(0);
}
