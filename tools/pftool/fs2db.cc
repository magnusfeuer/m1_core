//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA,  2005, 2006, 2007, 2008.
//

//
// Copy one or more files into trusted database structure.
//
#include "sqlcommon.hh"
#include "packfile.hh"

char *glob_argv0 = "";
CInodeMap i_map;
int verbose = 0;
int ignore_symlink = 0;
unsigned long restart_action = 0;


void usage(void)
{
    fprintf(stderr, "Usage: %s -u db_user  -p db_password  -h db_host  -d database [-b blob-size] \n", glob_argv0);
    fprintf(stderr, "           -i packet-id  [-r prefix]  [-L] [-a post install action]  path ... \n\n");
    fputs("NOTE: Packet-id has format account@provider/packet_name[/major_version.minor_version.patch_level]\n\n", stderr);
    fputs("-u db_user                Specifies the database user to login to MySQL with\n\n", stderr);
    fputs("-p db_password            Specifies the password for db_user\n\n", stderr);
    fputs("-h db_host                Specifies the MuSQL host address\n\n", stderr);
    fputs("-d database               Specifies the database to use on db_host\n\n", stderr);
    fputs("-b blob-size              Size, in kb, of each blob part forming a file. Default=1024. See Mysql max_allow_packet.\n\n", stderr);
    fputs("-i packet-id              Specifies the ID of the created packet. \n\n", stderr);
    fputs("-r prefix                 Prefix all file paths stored in the packfile.\n\n", stderr);
    fputs("-L                        Follow symbolic links instead of storing them as is,\n\n", stderr);
    fputs("-s svn_revision           The SVN revision that this packfile was generated from,\n\n", stderr);
    fputs("-D dependency-packet-id   Specifies packets that must be either already installed or on \n", stderr);
    fputs("                          the same installation media as this packet in order for this\n", stderr);
    fputs("                          packet to be installed\n\n", stderr);
    fputs("-a post-install action    Specifies the action to take after install. Multiple options are allowed.\n", stderr);
    fputs("                          b Reboot\n", stderr);
    fputs("                          s Restart\n", stderr);
    fputs("                          m Reload M1 files\n", stderr);
    fputs("                          l Reload dynamic libraries\n", stderr);
    fputs("path ...                  Path to one or more files and directories to include in packet.\n", stderr);
    exit(255);
}

int extractPath(string aPath, CContentList &aContent)
{
    int res = 0;
    ssize_t rlnk_res;
    struct stat d_stat;
    struct dirent *d_ent;
    DIR *dir;
    char buf[128];
    CInodeMapIterator inode;

    //
    // Check if path has ':' in it!
    //
    if (aPath.find(":") != string::npos) {
	fprintf(stderr, "%s has a \":\" in its name. Skipped.\n",
		aPath.c_str());
	return 0;
    }
    
    //
    // Check if this path already exists and is not a directory.
    //
    if (lstat(aPath.c_str(), &d_stat) == -1) {
	perror(aPath.c_str());
	return 0;
    }

    //
    // Check if this inode number already exists.
    //
    inode = i_map.find(d_stat.st_ino);
	
    // Install hardlink if this inode has already been processed.
    if (inode != i_map.end()) {
	if (verbose)
	    fprintf(stderr, "Hardlink:  %s -> %s\n", aPath.c_str(), inode->second->mPath.c_str());

	aContent.push_back(CContent(aPath, d_stat.st_size, d_stat.st_mode & 07777, DB_CONT_HARDLINK, "", 0, 0, inode->second));
	return 1;
    }



    // Regular file or symlink which we should ignore (-L).
    if (S_ISREG(d_stat.st_mode) || (S_ISLNK(d_stat.st_mode) && ignore_symlink)) {
	// Check that we can open the file
	FILE *tmp_in  = fopen(aPath.c_str(), "r");
	int ftype = 0;
	    
	if (!tmp_in) {
	    fprintf(stderr, "Could not open %s\n", aPath.c_str());
	    return 0;
	}
	fclose(tmp_in);

	//
	// Restat file if symlink
	//
	if (S_ISLNK(d_stat.st_mode) && stat(aPath.c_str(), &d_stat) == -1) {
	    perror(aPath.c_str());
	    return 0;
	}

	if (aPath.length() >= 4 && aPath.substr(aPath.length() - 4) == ".pfl") 
	    ftype = DB_CONT_PACKFILE;
	else
	    ftype = DB_CONT_REGULAR;

	if (verbose)
	    fprintf(stderr, "%s  %s [%d]\n", 
		    (ftype==DB_CONT_REGULAR)?"File:    ":"Packfile:", aPath.c_str(), d_stat.st_size);

	aContent.push_back(CContent(aPath, d_stat.st_size, d_stat.st_mode & 07777, ftype));

	// Store inode.
	i_map[d_stat.st_ino] = &(aContent.back());
	return 1;
    }

    // Block device?
    if (S_ISBLK(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Block dev: %s\n", aPath.c_str());
	aContent.push_back(CContent(aPath, 
				    d_stat.st_size, 
				    d_stat.st_mode & 07777, 
				    DB_CONT_BLOCKDEV, 
				    "",
				    (d_stat.st_rdev & 0xFFFFFFFF00000000LL) >> 32, // Major
				    (d_stat.st_rdev & 0x00000000FFFFFFFFLL),  // Minor
				    0));
	// Store inode.
	i_map[d_stat.st_ino] = &(aContent.back());
	return 1;
    }

    // Char device?
    if (S_ISCHR(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Char dev:  %s\n", aPath.c_str());
	aContent.push_back(CContent(aPath, 
				    d_stat.st_size, 
				    d_stat.st_mode & 07777, 
				    DB_CONT_CHARDEV, 
				    "",
				    (d_stat.st_rdev & 0xFFFFFFFF00000000LL) >> 32, // Major
				    (d_stat.st_rdev & 0x00000000FFFFFFFFLL),  // Minor
				    0));

	// Store inode.
	i_map[d_stat.st_ino] = &(aContent.back());
	return 1;
    }

    // Fifo?
    if (S_ISFIFO(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Fifo:      %s\n", aPath.c_str());
	aContent.push_back(CContent(aPath, d_stat.st_size, d_stat.st_mode & 07777, DB_CONT_FIFO));
	// Store inode.
	i_map[d_stat.st_ino] = &(aContent.back());
	return 1;
    }


    // Socket?
    if (S_ISSOCK(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Socket:    %s\n", aPath.c_str());
	aContent.push_back(CContent(aPath, d_stat.st_size, d_stat.st_mode & 07777, DB_CONT_UNIXSOCK));
	// Store inode.
	i_map[d_stat.st_ino] = &(aContent.back());
	return 1;
    }

    // Symlink
    if (S_ISLNK(d_stat.st_mode)) {
	rlnk_res = readlink(aPath.c_str(), buf, 512);
	if (rlnk_res == -1) {
	    perror(aPath.c_str());
	    return 0;
	}
	buf[rlnk_res] = 0;
	
	if (verbose)
	    fprintf(stderr, "Symlnk:    %s -> %s\n", aPath.c_str(), buf);
	aContent.push_back(CContent(aPath, d_stat.st_size, d_stat.st_mode & 07777, DB_CONT_SYMLINK, buf));
	// Store inode.
	i_map[d_stat.st_ino] = &(aContent.back());
	return 1;
    }

    if (verbose) 
	    fprintf(stderr, "Dir:       %s\n", aPath.c_str());

    //
    // Traverse the directory that is aPath
    //
    dir = opendir(aPath.c_str());
    if (!dir) {
	perror(aPath.c_str());
	return 0;
    }

    //
    // Get all entries ending with .pfl (pack file)
    // Note: Not thread safe...
    //
    while((d_ent = readdir(dir))) {
	int sub_res;
	//
	// Be recursive. 
	// If this returns zero, it means that aPath is an empty directory,
	// if so, add explicit directory.
	//
	if (!strcmp(d_ent->d_name, ".") || !strcmp(d_ent->d_name, ".."))
	    continue;

	if (!(sub_res = extractPath(string(aPath + string("/") + string(d_ent->d_name)), aContent))) {
	    string dir =  aPath + string("/") + string(d_ent->d_name);

	    if (lstat(dir.c_str(), &d_stat) == -1) {
		perror(dir.c_str());
		return 0;
	    }
	    if (verbose)
		fprintf(stderr, "Directory: %s\n", dir.c_str());
	    aContent.push_back(CContent(dir.c_str(), 0, d_stat.st_mode & 07777, DB_CONT_DIRECTORY));
	    ++res;
	} else
	    res += sub_res;
    }
    closedir(dir);
    return res;
}

void addRestartAction(char *key)
{
    while(key && *key) {
	switch(*key) {
	    case 'b':
		restart_action |= ACTION_REBOOT;
		break;

	    case 's':
		restart_action |= ACTION_RESTART;
		break;

	    case 'l':
		restart_action |= ACTION_RELOAD_LIB;
		break;

	    case 'm':
		restart_action |= ACTION_RELOAD_M1;
		break;

	    default:
		fprintf(stderr, "-a: Allowed keys b(reboot) s(restart) l(reload shared objects) m(reload M1 files).\n");
	}
	++key;
    }
}


int main(int argc, char **argv)
{
    string packet_id = "";
    string packet_name;
    string account;
    string provider;
    int major;
    int minor;
    int patch;
    CStringList deps; 
    CLongList deps_id;
    CContentList file_list;
    char c;
    char buf[1024];
    string db_user;
    string db_passwd;
    string db_host;
    string db_database;
    int svn_revision = -1;
    MYSQL *db = 0;
    CAccount acc;
    int ver_id;
    int comp_id; 
    int blob_size = 1024 * 1024; // 1 mb default
    CPackfileList existing_pf;
    string root_prefix = "./";
    glob_argv0 = argv[0];

	
    while((c = getopt(argc, argv, "b:vu:p:h:d:i:r:Ls:D:La:")) != -1) {
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

	case 'b':
	    blob_size = atoi(optarg) * 1024;
	    break;
	    
	case 's':
	    svn_revision = atoi(optarg);
	    break;
	    
	case 'a': // Restart action
	    addRestartAction(optarg);
	    break;

	case 'r':
	    if (!optarg) usage();
	    root_prefix =  optarg;
	    break;

	case 'L':
	    ignore_symlink = 1;
	    break;

	case 'i':
	    if (!optarg) usage();
	    packet_id = optarg;
	    break;

	case 'D':
	    if (optarg) 
		deps.push_back(optarg);
	    break;

	case 'v':
	    verbose++;
	    break;

	case '?':

	default:
	    usage();
	}
    }

    //
    // Add rest to content.
    //
    argc -= optind;
    argv += optind;

    if (packet_id == "") { fprintf(stderr, "Missing packet-id (-i)\n\n"); usage(); }
    if (db_user == "") { fprintf(stderr, "Missing database user (-u)\n\n"); usage(); }
    if (db_passwd == "") { fprintf(stderr, "Missing database password (-p)\n\n"); usage(); }
    if (db_host == "") { fprintf(stderr, "Missing database host (-h)\n\n"); usage(); }
    if (db_database == "") { fprintf(stderr, "Missing database to use (-d)\n\n"); usage(); }
    if (svn_revision == -1) { fprintf(stderr, "Missing svn revision (-s)\n\n"); usage(); }

    if (root_prefix[root_prefix.size() - 1] != '/')
	root_prefix = root_prefix + "/";

    //
    // Connect to database
    //
    if (!db_connect(&db, db_host.c_str(),  db_user.c_str(), db_passwd.c_str(), db_database.c_str())) {
	fprintf(stderr, "Could not connect to database\n");
	exit(255);
    }

    //
    // Validate arguments.
    //
    if (!extractPacketID(packet_id, account, provider, packet_name, major, minor, patch)) {
	fprintf(stderr, "Malformed packet id: %s\n", packet_id.c_str());
	usage();
    }    

    if (major == -1 || minor == -1 || patch == -1) {
	fprintf(stderr, "Malformed packet id (version): %s\n", packet_id.c_str());
	usage();
   } 


    // Check that this package does not already exist 
    if (db_find_packfile(db, packet_id, existing_pf) > 0) {
	fprintf(stderr, "Packet %s already exists in database\n", packet_id.c_str());
	exit(255);
    }


    //
    // Scan all files and remember their size.
    //
    while(*argv) {
	if (!extractPath(*argv, file_list)) {
	    fprintf(stderr, "Could not open all files\n");
	    exit(255);
	}
	++argv;
    }


    // Create a packfile db entry
    buf[0] = 0;
    if (restart_action & ACTION_REBOOT)
	strcat(buf, "b");

    if (restart_action & ACTION_RESTART)
	strcat(buf, "s");

    if (restart_action & ACTION_RELOAD_LIB)
	strcat(buf, "l");

    if (restart_action & ACTION_RELOAD_M1)
	strcat(buf, "m");

    
    // Locate account id matching the provided email address.
    if (!db_find_account_by_email(db, string(account+"@"+provider).c_str(), &acc)) {
	fprintf(stderr, "No account matches %s@%s\n", account.c_str(), provider.c_str());
	exit(255);
    }


    //
    // Resolve all dependencies in deps into pf_version.id values
    // of those packfiles needed.
    //
    for(CStringListIterator iter = deps.begin(); iter != deps.end(); ++iter) {
	int lst_sz;
	CPackfileList lst;

	//
	// Locate account
	//
	if (!(lst_sz = db_find_packfile(db, *iter, lst))) {
	    fprintf(stderr, "Unresolved dependency: [%s]\n", deps.front().c_str());
	    exit(255);
	}

	// Add top entry (highest version number) as a dependency.
	deps_id.push_back(lst.front().mVersionID);
    }


    //
    // Install packfile and pf_version rows.
    //
    CPackfile pf(-1, svn_revision, packet_name, buf, acc.mDbID, -1, major, minor, patch, 0);
    if (!(ver_id = db_create_packfile(db, pf))) {
	fprintf(stderr, "Could not create packfile\n");
	exit(255);
    }

    //
    // Add all deps.
    //
    while(deps_id.begin() != deps_id.end()) {
	db_add_dependency(db, ver_id, deps_id.front());
	deps_id.pop_front();
    }

    // List all content, and remember exactly where we should write the start parker
    while(file_list.begin() != file_list.end()) {
	db_add_file(db, ver_id, file_list.front(), root_prefix, blob_size);
	//	printf("Added %s\n", file_list.front().mPath.c_str());
	file_list.pop_front();
    }   
    mysql_close(db);
}
