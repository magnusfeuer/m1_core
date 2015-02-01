
//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
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
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database -i packet-id -D db-directory\n", glob_argv0);
    fprintf(stderr, "       [-r root-dir] [-S suffix] [-s serial] [-k key] [-P]\n\n");
    fputs("NOTE: Packet-id has format account@provider/packet_name[/major_version.minor_version.patch_level]\n\n", stderr);
    fputs("-u db_user                Specifies the database user to login to MySQL with\n\n", stderr);
    fputs("-p db_password            Specifies the password for db_user\n\n", stderr);
    fputs("-h db_host                Specifies the MuSQL host address\n\n", stderr);
    fputs("-d database               Specifies the database to use on db_host\n\n", stderr);
    fputs("-i packet-id              Specifies the ID of the packet to generate a packfile for. \n\n", stderr); 
    fputs("-s serial                 Specifies the serial number to pivot encrption around. Key and device must be in db.\n\n", stderr);
    fputs("-r root-dir               Use this path prefix when creating files. Defaults to ./ \n\n", stderr);
    fputs("-k key                    Specifies the encryption key to use. Probably device key or probably 'magden'. Defaults to -s serial\n\n", stderr);
    fputs("-D db-directory           Database directory (under -r root-dir) where information about installed packages should be written.\n\n", stderr);
    fputs("-S suffix                 Add suffix to list of files to encrypt. Can be used multiple times. \n\n", stderr);
    fputs("-P                        Add a device priv key signed magden pub key as /m1/keys/magden.pub. \n", stderr);
    fputs("                          If /m1/m1e is seen, patch it with the device pub key. ('demo' and 'magden' key must be in db)\n\n", stderr);
    exit(255);
}

//
// Install file
//
bool install_content(MYSQL *aDbDesc, string aRoot, CContent *aContent, CKey *aEncryptionKey)
{
    string::size_type last_slash;
    FILE *outf = 0;
    string out_name;
    char *content;
    unsigned long bytes_left;
    CBioStream bo; // For encrypted output.

    out_name = aRoot  + aContent->mPath;
    DBGFMT("install_content(): File[%s] Size[%u] type[%c]",
	   out_name.c_str(),
	   aContent->mSize
	   file_type_map[aContent->mType]);

    //
    // Everything except files we can do immediately.
    //
    unlink(out_name.c_str());
    // Check if this is a directory 
    if (aContent->mType == DB_CONT_DIRECTORY) {
	return createDirectory(out_name, aContent->mPermission);
    }

    // Creatfe relevant directory path if we have slash in path
    if ((last_slash = out_name.find_last_of("/")) != string::npos) {
	if (!createDirectory(out_name.substr(0, last_slash), 0777))
	    return false;
    }

    // Block device??
    if (aContent->mType == DB_CONT_BLOCKDEV) {
	dev_t maj_min = ((unsigned long long) aContent->mMajor << 32) | (unsigned long long) aContent->mMinor;
	if (mknod(out_name.c_str(), S_IFBLK | aContent->mPermission, maj_min) == -1) {
	    printf("Could not make block device [%s]: %s\n", out_name.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    // char device??
    if (aContent->mType == DB_CONT_CHARDEV) {
	dev_t maj_min = ((unsigned long long) aContent->mMajor << 32) | (unsigned long long) aContent->mMinor;
	if (mknod(out_name.c_str(), S_IFCHR | aContent->mPermission, maj_min) == -1) {
	    printf("Could not make char device [%s]: %s\n", out_name.c_str(), strerror(errno));

	    return false;
	}
	return true;
    }

    // Fifo?
    if (aContent->mType == DB_CONT_FIFO) {
	if (mknod(out_name.c_str(), S_IFIFO | aContent->mPermission, 0LL) == -1) {
	    printf("Could not make fifo [%s]: %s\n", out_name.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    // Socket?
    if (aContent->mType == DB_CONT_UNIXSOCK) {
	if (mknod(out_name.c_str(), S_IFSOCK | aContent->mPermission, 0LL) == -1) {
	    printf("Could not make unix socket [%s]: %s\n", out_name.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    // Symlink?
    if (aContent->mType == DB_CONT_SYMLINK) {
	if (symlink(aContent->mSymlinkTarget.c_str(), out_name.c_str()) == -1) {
	    printf("Could not make symlink [%s]->[%s]: %s\n", 
		   out_name.c_str(), aContent->mSymlinkTarget.c_str(), strerror(errno));
	    return false;
	}
	chmod(out_name.c_str(), aContent->mPermission);
	return true;
    }

    // Hardlink
    if (aContent->mType == DB_CONT_HARDLINK) {
	if (link(aContent->mLink->mPath.c_str(), out_name.c_str()) == -1) {
	    printf("Could not make hardlink [%s]->[%s]: %s\n", 
		   out_name.c_str(), aContent->mSymlinkTarget.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    if (!(content = (char *) malloc(aContent->mSize))) {
	printf("Could not malloc[%d] bytes\n", aContent->mSize);
	return false;
    }

    // Read content from DB
    db_get_file_content(aDbDesc, aContent, content);
    bytes_left = aContent->mSize;
    
    //
    // If we are not to encrypt content, just copy the file
    //
    if (!aEncryptionKey) {
	if (!(outf = fopen(out_name.c_str(), "w"))) {
	    printf("Could not open [%s] for writing: %s\n", out_name.c_str(), strerror(errno));
	    free(content);
	    return false;
	}

	fchmod(fileno(outf), aContent->mPermission);

	while(bytes_left) {
	    int res;
	    res = fwrite((void *) (content + aContent->mSize - bytes_left), 1, bytes_left, outf);
	    if (res <= 0) {
		printf("Could not write to [%s]: %s\n", out_name.c_str(), strerror(errno));
		fclose(outf);
		free(content);
		return false;
	    }
	    bytes_left -= res;
	}
	fclose(outf);
	//	printf("Wrote non encrypted [%s]\n", out_name.c_str());
	return true;
    }
   
    if (bo.open_file_write(out_name) == -1) {
	printf("CPacket::install(): Could not open encrypted target file [%s]\n", out_name.c_str());
	free(content);
	return false;
    }

    BIO_get_fp(bo.get_bio(), &outf);
    fchmod(fileno(outf), aContent->mPermission);
    
    bo.write_MAGIC();
    bo.write_ENCRYPTION(aEncryptionKey);
    bo.write_COMPRESSION(-1, 0);
    bo.write_DATA(0);
    
    while(bytes_left) {
	int res;

	if ((res = bo.write(content + aContent->mSize - bytes_left, bytes_left)) <= 0) {
	    printf("Could not write to encrypted target file[%s]\n", out_name.c_str());
	    bo.close();
	    free(content);
	    return false;
	}
	bytes_left -= res;
    }

    bo.flush();
    bo.close();
    free(content);
    //    printf("Wrote encrypted file[%s]\n", out_name.c_str());

    return true;
}





int main(int argc, char **argv)
{
    time_t lt;
    string packet_id = "";
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
    CStringList enc_suffix_lst; // List over file suffix to encrypt
    bool patch_bin = false;
    CBioStream dummy; // To prime PRNG
    string db_dir = "";

    string enc_key_name = "";
    CKeyData enc_key_data;   // Key to use for content encryption. From database.
    CKey *enc_key_obj;

    string dev_key_name = "";
    CKeyData dev_key_data;   // Device key data from database
    CKey *dev_key_obj;

    CKeyData magden_key_data;
    CKey *magden_key_obj;

    CKey *signed_magden_key_obj;

    CKeyData demo_key_data;
    CKey *demo_key_obj;
   
    CM1Unit unit;
    string root_dir = "./";
    char fname[1024];
    glob_argv0 = argv[0];

    while((c = getopt(argc, argv, "k:vu:p:h:d:i:S:s:Pr:D:")) != -1) {
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

	case 'v':
	    verbose++;
	    break;

	case 'k': // Usually magden
	    if (!optarg) usage();
	    enc_key_name = optarg;
	    break;


	case 'r':
	    if (!optarg) usage();
	    root_dir = optarg;
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

	case 'D':
	    if (!optarg) usage();
	    db_dir = optarg;
	    break;


	case '?':

	default:
	    usage();
	}
    }

    if (db_dir == "") {
	fprintf(stderr, "Missing -D database directory.\n");
	usage();
	exit(255);
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

    if (root_dir[root_dir.size() - 1] != '/')
	root_dir = root_dir + "/";

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
	// Load the device key data.
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
    // Check if we should install a magden public key.
    //
    if (patch_bin) {
	unsigned long c_size = 0;
	char *content = sign_pubkey((char *) 0, 0, c_size, &magden_key_data, dev_key_obj);
	FILE *out;
	    
	createDirectory(root_dir + "m1/keys/", 0755);

	if ((out = fopen(string(root_dir + "m1/keys/magden.pub").c_str(), "w"))) {
	    fwrite((void *) content, c_size, 1, out);
	    fclose(out);
	} else
	    printf("Could not open signed public key file [%s] for writing: %s\n",
		   string(root_dir + "m1/keys/magden.pub").c_str(), strerror(errno));

	free(content);
    }

    // Init encryption
    ERR_load_crypto_strings();
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();


    for(CContentListIterator iter = file_lst.begin(); iter != file_lst.end(); ++iter) {
	size_t bytes_left = iter->mSize;
	size_t res;
	unsigned long tmp_pos, start_pos;
	int ret;
	CBioStream bstrm;

	if (iter->mType == DB_CONT_PACKFILE) {
	    printf("WTF! A packfile [%s] as content in the database! Skipping\n", iter->mPath.c_str());
	    continue;
	}

	install_content(db, root_dir, &(*iter), hasEncryptionSuffix(iter->mPath, enc_suffix_lst)?enc_key_obj:0);
	
	//
	// Is this the m1e binary and shall we patch it?
	//
	if (patch_bin && 
	    iter->mType == DB_CONT_REGULAR && 
	    iter->mPath.size() >= 4 && 
	    iter->mPath.substr(iter->mPath.size() - 4, 4) == "/m1e")  {
	    // Labour intense...
	    char *content = (char *) malloc(iter->mSize);
	    
	    FILE *in = fopen(string(root_dir + iter->mPath).c_str(), "r");
	    FILE *out;
	    if (!in) {
		printf("Cannot open m1e file for reading to patch it[%s]: %s\n",
		       string(root_dir + iter->mPath).c_str(), strerror(errno));
		free(content);
		continue;
	    }

	    if (!fread((void *) content, iter->mSize, 1, in)) {
		printf("Cannot read [%s] to patch it: %s\n",
		       string(root_dir + iter->mPath).c_str(), 
		       strerror(errno));
		fclose(in);
		free(content);
		continue;
	    }
	    
	    fclose(in);

	    if (!patch_m1e_devkey(content, iter->mSize,
				  &demo_key_data, // Existing key
				  &dev_key_data, // New device key.
				  unit.mDiskID.c_str(), unit.mDiskID.length())) { // Disk DOM ID to use as password.
		free(content);
		continue;
	    }
	    // Open outfile
	    if (!(out = fopen(string(root_dir + iter->mPath).c_str(), "w"))) {
		printf("Cannot open m1e file for writing after patching it[%s]: %s\n",
		       string(root_dir + iter->mPath).c_str(), strerror(errno));
		free(content);
		continue;
	    }
	    if (!fwrite((void *) content, iter->mSize, 1, out)) {
		printf("Cannot write patched content to [%s]: %s\n",
		       string(root_dir + iter->mPath).c_str(), 
		       strerror(errno));
		fclose(out);
		free(content);
		continue;
	    }
	    fchmod(fileno(out), 0755);
	    fclose(out);
	    free(content);
	}
    }
    //
    // Add DB entry file
    //
    sprintf(fname, "%s%s/%s.%s.%d.%d.%d.pfl",
	    root_dir.c_str(),
	    db_dir.c_str(), 
	    acc.mEmail.c_str(), 
	    pf_entry->mName.c_str(),
	    pf_entry->mMajor,
	    pf_entry->mMinor,
	    pf_entry->mPatch);
    unlink(fname);
    if (!(out = fopen(fname, "w"))) {
	DBGFMT_WARN("CPacket::addDBEntry(): Could not open DB file [%s] for writing", fname);
	return false;
    }
    fprintf(out, "<m1-packfile-start>\n");
    fprintf(out, "Format-Version: 1\n");
    fprintf(out, "From: %s\n", acc.mEmail.c_str());
    fprintf(out, "Name: %s\n", pf_entry->mName.c_str());
    fprintf(out, "Version: %d.%d.%d\n", pf_entry->mMajor, pf_entry->mMinor, pf_entry->mPatch);

    for(CDependencyListIterator iter = deps.begin(); iter != deps.end(); ++iter) 
	fprintf(out, "Needs: %s/%s/%d.%d.%d\n", 
		iter->mEmail.c_str(),
		iter->mName.c_str(),
		iter->mMajor,
		iter->mMinor,
		iter->mPatch);


    for(CContentListIterator iter = file_lst.begin(); iter != file_lst.end(); ++iter) 
	fprintf(out, "Content: %s:0:0:0:%o:%c\n", 
		iter->mPath.c_str(),
		iter->mPermission,
		file_type_map[iter->mType]);

    fclose(out);

}
