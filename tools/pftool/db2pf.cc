
//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#include "sqlcommon.hh"
#include "key.hh"
#include "key_store.hh"
#include <openssl/err.h>
#include <openssl/evp.h>
#include "pfcommon.hh"

using namespace std;
int verbose = 0;
char ignore_symlink = 0;
unsigned long restart_action = 0;
char *glob_argv0;


void usage(void)
{
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database-i packet-id -o output-packfile -S suffix -s serial [-k key] [-P]\n\n", glob_argv0);
    fputs("NOTE: Packet-id has format account@provider/packet_name[/major_version.minor_version.patch_level]\n\n", stderr);
    fputs("-u db_user                Specifies the database user to login to MySQL with\n\n", stderr);
    fputs("-p db_password            Specifies the password for db_user\n\n", stderr);
    fputs("-h db_host                Specifies the MuSQL host address\n\n", stderr);
    fputs("-d database               Specifies the database to use on db_host\n\n", stderr);
    fputs("-i packet-id              Specifies the ID of the packet to generate a packfile for. \n\n", stderr); 
    fputs("-O packet-id              [optional] Specifies id for the packet when it is installed from the created packfile.\n", stderr); 
    fputs("                                     Default is the -i packet id.\n\n", stderr); 
    fputs("-s serial                 Specifies the serial number to pivot encrption around. Key and device must be in db.\n\n", stderr);
    fputs("-k key                    Specifies the encryption key to use. Probably device key or probably 'magden'. Defaults to -s serial\n\n", stderr);
    fputs("-S suffix                 Add suffix to list of files to encrypt. Can be used multiple times. \n\n", stderr);
    fputs("-P                        Add a device priv key signed magden pub key as /m1/keys/magden.pub. \n", stderr);
    fputs("                          If /m1/m1e is seen, patch it with the device pub key. ('demo' and 'magden' key must be in db)\n\n", stderr);
    fputs("-o outfile                Name of the packfile to create.\n", stderr);
    exit(255);
}


int main(int argc, char **argv)
{
    time_t lt;
    string outfile = "";
    string packet_id = "";
    string out_packet_id = "";
    string out_account = "";
    string out_provider = "";
    int out_major;
    int out_minor;
    int out_patch;
    string packet_name;
    int major;
    int minor;
    int patch;
    long pf_ver_id;
    CDependencyList deps;
    CPackfileList pf_lst;
    FILE *out;
    int blocksize = 1024*1024;
    char buf[blocksize];
    MYSQL *db = 0;
    char c;
    string db_user;
    string db_passwd;
    string db_host;
    string db_database;
    CPackfile *pf_entry;
    CAccount acc;
    CContentList file_lst;
    char *content = 0;
    unsigned long current_size = 0;
    long cont_start;
    CKeyDataList enc_keys; // All non device keys (magden etc) loaded from database.
    CStringList enc_suffix_lst; // List over file suffix to encrypt
    bool patch_bin = false;
    CBioStream dummy; // To prime PRNG

    string enc_key_name = "";
    CKeyData enc_key_data;   // Key to use for content encryption. From database.
    CKey *enc_key_obj;

    string dev_key_name = "";
    CKeyData dev_key_data;   // Device key to be used.
    CKey *dev_key_obj;

    CKeyData magden_key_data;
    CKey *magden_key_obj;

    CKey *signed_magden_key_obj;

    CKeyData demo_key_data;
    CKey *demo_key_obj;
   
    CM1Unit unit;

    glob_argv0 = argv[0];

    while((c = getopt(argc, argv, "k:vu:p:h:d:i:S:o:O:s:P")) != -1) {
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
	    packet_id = optarg;
	    break;

	case 'O':
	    if (!optarg) usage();
	    if (!extractPacketID(optarg, out_account, out_provider, out_packet_id, out_major, out_minor, out_patch)) {
		fprintf(stderr, "Illegal -O packet id format [%s]\n", optarg);
		usage();
		exit(255);
	    }
	    break;

	case 'v':
	    verbose++;
	    break;

	case 'k': // Usually magden
	    if (!optarg) usage();
	    enc_key_name = optarg;
	    break;


	case 'o':
	    if (!optarg) usage();
	    outfile = optarg;
	    break;

	case 's':
	    if (!optarg) usage();
	    dev_key_name = optarg;
	    break;

	case 'P':
	    patch_bin = true;
	    break;

	case 'S':
	    if (!optarg) usage();
	    enc_suffix_lst.push_back(optarg);
	    break;


	case '?':

	default:
	    usage();
	}
    }

    if (enc_key_name == "" && dev_key_name != "")
	enc_key_name = dev_key_name;

    if (patch_bin && dev_key_name == "") {
	fprintf(stderr, "Missing -s serial needed with -P\n");
	usage();
	exit(255);
    }

    if (packet_id == "") {
	fprintf(stderr, "Missing -i packet-id\n");
	usage();
	exit(255);
    }


    if ((patch_bin || enc_suffix_lst.size() > 0) && enc_key_name == "") {
	fprintf(stderr, "Missing -k key needed with -P or -S suffix\n");
	usage();
	exit(255);
    }

    //
    // Connect to database
    //
    if (!db_connect(&db, db_host.c_str(),  db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {	
	fprintf(stderr, "Could not connect to database\n");
	exit(255);
    }

    //
    // Load all keys needed to patch m1e and create signed magden.pub
    //
    if (enc_key_name != "" && !(enc_key_obj = setup_priv_key(db, enc_key_name)))
	exit(255);


    // Load device key if specified
    if (dev_key_name != "" &&!(dev_key_obj = setup_priv_key(db, dev_key_name)))
	    exit(255);

    if (patch_bin) {
	//
	// Locate M1 device for this serial. We need the disk id for patching.
	//
	if (!db_get_m1_unit(db, dev_key_name, unit)) {
	    printf("Could not locate m1 device with serial [%s]\n", dev_key_name.c_str());
	    exit(255);
	}

	//
	// Load the demo key.
	// 
	if (!db_get_encryption_key(db, "demo", demo_key_data)) {
	    printf("Could not load demo key from database\n");
	    exit(255);
	}

	//
	// Load the device key.
	// 
	if (!db_get_encryption_key(db, dev_key_name, dev_key_data)) {
	    printf("Could not load device key [%s] key from database\n", dev_key_name.c_str());
	    exit(255);
	}

	//
	// Load specified magden key
	// 
	if (!db_get_encryption_key(db, "magden", magden_key_data)) {
	    printf("Could not load magden key from database\n");
	    exit(255);
	}
    }

    // Retrieve all packfiles matching the request. The top one will have the last version.
    db_find_packfile(db, packet_id, pf_lst);

    if (pf_lst.size() == 0) {
	printf("Packfile[%s] not found\n", packet_id.c_str());
	mysql_close(db);
	exit(255);
    }

    pf_entry = &(pf_lst.front());

    // Grab account data
    if (!db_find_account_by_id(db, pf_entry->mCreatorAccountID, &acc)) {
	mysql_close(db);
	exit(255);
    }
    
    // Get all deps.
    db_get_dependencies(db, pf_entry->mVersionID, &deps);

    // Get meta data of all files in this packfile. (Content will come further down)
    if (!db_get_files(db, pf_entry->mVersionID, file_lst)) {
	mysql_close(db);
	exit(255);
    }

    //
    // If we are creating a magden public key, add an entry for it first.
    //
    if (patch_bin)  
	file_lst.push_front(CContent("./m1/keys/magden.pub", 1L, 0644, DB_CONT_REGULAR));


    //
    // We now have a list of file entries and lengths to install.
    //
    if (outfile != "") {
	unlink(outfile.c_str());// Remove if exist.

	if (!(out = fopen(outfile.c_str(), "w+"))) {
	    perror(outfile.c_str());
	    exit(255);
	} 
    } else
	out = stdout;
    
    // Init encryption
    ERR_load_crypto_strings();
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();

    // Write the header.
    lt = time(0);
    fprintf(out, "# Created: %s", ctime(&lt)); // Ctime includes \n
    fprintf(out, "<m1-packfile-start>\n");
    strcpy(buf, VERSION);
    if (strchr(buf, '.'))
	*strchr(buf, '.') = 0;

    fprintf(out, "Format-Version: %s\n", buf);
    if (out_account != "") {
	fprintf(out, "From: %s@%s\n", out_account.c_str(), out_provider.c_str());
	fprintf(out, "Name: %s\n", out_packet_id.c_str());
	fprintf(out, "Version: %d.%d.%d\n" , out_major, out_minor, out_patch);
    }
    else {
	fprintf(out, "From: %s\n", acc.mEmail.c_str());
	fprintf(out, "Name: %s\n", pf_entry->mName.c_str());
	fprintf(out, "Version: %d.%d.%d\n" ,pf_entry->mMajor, pf_entry->mMinor, pf_entry->mPatch);
    }

    fprintf(out, "Restart-Action: %s\n", pf_entry->mRestartAction.c_str());
    if (dev_key_name != "") 
	fprintf(out, "Target-Device: %s\n", dev_key_name.c_str());

    // If we are encrypting content with our device key, make a note of the target device
    if (dev_key_name != "" && dev_key_name == enc_key_name && dev_key_data.mDeviceKeyFlag) 
	fprintf(out, "Target-Device: %s\n", dev_key_name.c_str());


    //
    // List all deps
    //
    for(CDependencyListIterator iter = deps.begin(); iter != deps.end(); ++iter) 
	fprintf(out, "Needs: %s/%s/%d.%d.%d\n", iter->mEmail.c_str(), iter->mName.c_str(), iter->mMajor, iter->mMinor, iter->mPatch);

    
    // List all content, and remember exactly where we should write the start parker
    for(CContentListIterator iter = file_lst.begin(); iter != file_lst.end(); ++iter) {
	char *strt;
	char *enc = "";
	//
	// Setup basic entry.
	//
	if (hasEncryptionSuffix(iter->mPath, enc_suffix_lst)) {
	    enc = "e";
	}

	sprintf(buf, "Content: %s:0000000000:0000000000:0000000000:%o:%c%s", 
		iter->mPath.c_str(),
		iter->mPermission,
		file_type_map[iter->mType], 
		enc);
	
	// 
	// Add target path after file type for hardlink or symlink
	// 
	if (iter->mType == DB_CONT_SYMLINK) 
	    strcat(buf, iter->mSymlinkTarget.c_str());
	
	if (iter->mType == DB_CONT_HARDLINK)
	    strcat(buf, iter->mLink->mPath.c_str());

	if (iter->mType == DB_CONT_BLOCKDEV || iter->mType == DB_CONT_CHARDEV) 
	    sprintf(buf + strlen(buf), "%llu", ((unsigned long long) iter->mMajor << 32) | (unsigned long long) iter->mMinor);
	
	strcat(buf, "\n");

	if (!(strt = strchr(buf, ':')) || !(strt = strchr(strt + 1, ':'))) {
	    fprintf(stderr, "Internal consistency error! Could not find 3 \":\" in [%s]\n", buf);
	    exit(255);
	}

	// Store where we should write the start marker for this file
	if (iter->mSize != 0)
	    iter->mStart = ftell(out) + (strt - buf) + 1;
	else
	    iter->mStart = 0;
	fwrite(buf, 1, strlen(buf), out);
    }    
    fprintf(out, "\n");
    
    // Remember content start.
    cont_start = ftell(out);

    if (verbose == 2)
	fprintf(stderr, "Content starts at %d\n", cont_start);

    for(CContentListIterator iter = file_lst.begin(); iter != file_lst.end(); ++iter) {
	size_t bytes_left = iter->mSize;
	size_t res;
	unsigned long tmp_pos, start_pos;
	int ret;
	CBioStream bstrm;

	// Skip everything but regular files.
	if (iter->mType != DB_CONT_REGULAR && iter->mType != DB_CONT_PACKFILE) 
	    continue;

	if (verbose == 2)
	    fprintf(stderr, "Adding [%s]\n", iter->mPath.c_str());

	//
	// Grab data from database.
	//
	// Malloc 
	if (current_size < iter->mSize)  {
	    if (content)
		free(content);
	    
	    content = (char *) malloc(iter->mSize);
	    current_size = iter->mSize;
	}
	    
	//
	// Skip first magden public signed key if present.
	// It will be setup below.
	//
	if (!patch_bin || file_lst.begin() != iter)
	    db_get_file_content(db, &(*iter), content);

	//
	// Is this the m1e binary and shall we patch it?
	//
	if (patch_bin && 
	    iter->mPath.size() >= 4 && 
	    iter->mPath.substr(iter->mPath.size() - 4, 4) == "/m1e")  {
	    patch_m1e_devkey(content, iter->mSize,
			     &demo_key_data, // Existing key
			     &dev_key_data, // New device key.
			     unit.mDiskID.c_str(), unit.mDiskID.length());  // Disk DOM ID to use as password.
			     
	}
	//
	// Check if this is the public key that we should write. If so, sign it and write it.
	//
	if (patch_bin && iter == file_lst.begin()) {
	    content = sign_pubkey(content, current_size, current_size, &magden_key_data, dev_key_obj);
	    iter->mSize = bytes_left = current_size;
	    printf("Setup [%s]. Size[%d]\n", iter->mPath.c_str(), bytes_left);
	}

	//
	// Patch in the correct starting point in the header
	//
	start_pos = ftell(out);
	//
	// Write content
	//
	if (bstrm.open_fp_write(iter->mPath, out)) {
	    fprintf(stderr, "Could not to CBioStream::open_fp_write()\n");
	    exit(255);
	}
	// Check if we should encrypt
	bstrm.write_MAGIC();
	if (hasEncryptionSuffix(iter->mPath, enc_suffix_lst)) {
	    bstrm.write_ENCRYPTION(enc_key_obj);
	}
	// We will always compress!
	bstrm.write_COMPRESSION(-1, 0); 
	bstrm.write_DATA(0);

	while(bytes_left) {
	    int write_res = bstrm.write(content + iter->mSize - bytes_left, bytes_left);
	    
	    if (write_res < 1) {
		printf("FAIL: write returned [%d]\n", write_res);
		break;
	    }
	    bytes_left -= write_res;
	}
	bstrm.flush();
	tmp_pos = ftell(out);
	bstrm.close();

	fseek(out, iter->mStart, SEEK_SET);
	sprintf(buf, "%.10d:%.10d:%.10d", start_pos - cont_start, iter->mSize, tmp_pos - cont_start);
	//	printf("[%s] ContentLength[%d] PackfileLength[%d]\n", iter->mPath.c_str(), iter->mSize, tmp_pos - start_pos);
	fwrite(buf, 1, strlen(buf), out);
	fseek(out, tmp_pos, SEEK_SET);

    }

    fprintf(out, "\n<m1-packfile-end>\n");
    fclose(out);
}
