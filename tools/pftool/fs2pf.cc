//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <stdio.h>
#include <list>
#include <string>
#include <time.h>
#include <map>
#include <errno.h>
#include <packfile.hh>
#include "key.hh"
#include "key_store.hh"
#include <openssl/err.h>
#include "pfcommon.hh"

using namespace std;
int verbose = 0;
char ignore_symlink = 0;
#define ACTION_REBOOT     0x00000001
#define ACTION_RESTART    0x00000002
#define ACTION_RELOAD_LIB 0x00000004
#define ACTION_RELOAD_M1  0x00000008
unsigned long restart_action = 0;

typedef list<string> CStringList;
typedef list<string>::iterator CStringListIterator;
struct CPFContent {
    CPFContent(const string &aPath, size_t aStart, size_t aSize, mode_t aPermission, const string aFileType):
	mPath(aPath),
	mStart(aStart),
	mSize(aSize),
	mPermission(aPermission),
	mFileType(aFileType){}
    string mPath;
    size_t mStart;
    size_t mSize; // 0 == directory.
    mode_t mPermission; 
    string mFileType;
};

typedef list<CPFContent> CPFContentList;
typedef list<CPFContent>::iterator CPFContentListIterator;

typedef map<ino_t,string> CPFInodeMap;
typedef map<ino_t,string>::iterator CPFInodeMapIterator;

CPFInodeMap i_map;

void usage(char *aName)
{
    fprintf(stderr, "Usage: %s -o output-file -i packet-id  [-a post install action]  [-s serial-number]  \n", aName);
    fprintf(stderr, "          [-D dependency-packet-id ] [(-s serial | -k key)  -K key-directory -S suffix] path ...\n\n");
    fputs("NOTE: Packet-id has format account@provider/packet_name[/major_version.minor_version.patch_level]\n\n", stderr);
    fputs("-o output-file            The packet file to create. Should end with \".pfl\".\n\n", stderr);
    fputs("-i packet-id              Specifies the ID of the created packet. \n\n", stderr);
    fputs("-P prefix                 Prefix all file paths stored in the packfile.\n\n", stderr);
    fputs("-L                        Follow symbolic links instead of storing them as is,\n\n", stderr);
    fputs("-s serial-number          Optional. Specifies the serial number of the target m1 unit.\n", stderr);
    fputs("                          If not specified, the packet can be installed on an arbitrary\n", stderr);
    fputs("                          number of units. Only one of -s or -k can be  set, since the\n", stderr);
    fputs("                          device key must be used to encrypt the content.\n\n", stderr);
    fputs("-k key                    Encrypt all files ending with suffixes given -S with key. \n\n", stderr);
    fputs("-K key-directory          Points out where key given with -k is located.\n\n", stderr);
    fputs("-S suffix                 Specifies the suffix of files to be encrypted. -S can be given multiple times.\n\n", stderr);
    fputs("-D dependency-packet-id   Specifies packets that must be either already installed or on \n", stderr);
    fputs("                          the same installation media as this packet in order for this\n", stderr);
    fputs("                          packet to be installed\n\n", stderr);
    fputs("-a post_action            Specifies the action to take after install. Multiple options are allowed.\n", stderr);
    fputs("                          r Reboot\n", stderr);
    fputs("                          s Restart\n", stderr);
    fputs("                          m Reload M1 files\n", stderr);
    fputs("                          l Reload dynamic libraries\n\n", stderr);
    fputs("-t serial-number          Do not encrypt content, but make m1 validate serial number in packfile before installing.\n\n", stderr);
    fputs("path ...                  Path to one or more files and directories to include in packet.\n", stderr);
    exit(255);
}

int extractPath(string aPath, CPFContentList &aContent)
{
    int res = 0;
    ssize_t rlnk_res;
    struct stat d_stat;
    struct dirent *d_ent;
    DIR *dir;
    char buf[128];
    CPFInodeMapIterator inode;

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
	
    // Install link
    if (inode != i_map.end()) {
	if (verbose)
	    fprintf(stderr, "Hardlink:  %s -> %s\n", aPath.c_str(), inode->second.c_str());
	sprintf(buf, "l%s", inode->second.c_str());
	aContent.push_back(CPFContent(aPath.c_str(), 0, 0, d_stat.st_mode & 07777, buf));
	return 1;
    }

    // Store inode.
    i_map[d_stat.st_ino] = aPath;


    // Regular file?
    if (S_ISREG(d_stat.st_mode) || (S_ISLNK(d_stat.st_mode) && ignore_symlink)) {
	// Check that we can open the file
	FILE *tmp_in  = fopen(aPath.c_str(), "r");
	    
	if (!tmp_in) {

	    fprintf(stderr, "Could not open %s\n", aPath.c_str());
	    return 0;
	}
	fclose(tmp_in);


	// Restat file if symlink
	if (S_ISLNK(d_stat.st_mode) && stat(aPath.c_str(), &d_stat) == -1) {
	    perror(aPath.c_str());
	    return 0;
	}

	if (verbose)
	    fprintf(stderr, "File:      %s [%d]\n", aPath.c_str(), d_stat.st_size);
	aContent.push_back(CPFContent(aPath.c_str(), 0, d_stat.st_size, d_stat.st_mode & 07777, "r"));
	return 1;
    }

    // Block device?
    if (S_ISBLK(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Block dev: %s\n", aPath.c_str());
	sprintf(buf, "b%llu", d_stat.st_rdev);
	aContent.push_back(CPFContent(aPath.c_str(), 0, d_stat.st_size, d_stat.st_mode & 07777, buf));
	return 1;
    }

    // Char device?
    if (S_ISCHR(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Char dev:  %s\n", aPath.c_str());
	sprintf(buf, "c%llu", d_stat.st_rdev);
	aContent.push_back(CPFContent(aPath.c_str(), 0, d_stat.st_size, d_stat.st_mode & 07777, buf));
	return 1;
    }

    // Fifo?
    if (S_ISFIFO(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Fifo:      %s\n", aPath.c_str());
	aContent.push_back(CPFContent(aPath.c_str(), 0, d_stat.st_size, d_stat.st_mode & 07777, "f"));
	return 1;
    }


    // Socket?
    if (S_ISSOCK(d_stat.st_mode)) {
	if (verbose)
	    fprintf(stderr, "Socket:    %s\n", aPath.c_str());
	aContent.push_back(CPFContent(aPath.c_str(), 0, d_stat.st_size, d_stat.st_mode & 07777, "s"));
	return 1;
    }

    // Symlink
    if (S_ISLNK(d_stat.st_mode)) {
	buf[0] = 'S';
	rlnk_res = readlink(aPath.c_str(), buf + 1, 511);
	if (rlnk_res == -1) {
	    perror(aPath.c_str());
	    return 0;
	}
	buf[rlnk_res + 1] = 0;
	
	if (verbose)
	    fprintf(stderr, "Symlnk:    %s -> %s\n", aPath.c_str(), buf + 1);
	aContent.push_back(CPFContent(aPath.c_str(), 0, 0, d_stat.st_mode & 07777, buf));
	return 1;
    }

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
	    aContent.push_back(CPFContent(dir.c_str(), 0, 0, d_stat.st_mode & 07777, "d"));
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
	    case 'r':
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
		fprintf(stderr, "-a: Allowed keys b(reboot) r(restart) l(reload shared objects) m(reload M1 files).\n");
	}
	++key;
    }
}

int main(int argc, char **argv)
{
    time_t lt;
    string outfile = "";
    string packet_id = "";
    string packet_name;
    string account;
    string provider;
    int major;
    int minor;
    int patch;
    string serial = "";
    string target_dev = "";
    string key = "";
    string key_dir = "";
    CStringList deps;
    CPFContentList file_list;
    char c;
    char *pname = argv[0];
    FILE *out;
    int blocksize = 1024*1024;
    char rd_buf[blocksize];
    char buf[1024];
    size_t cont_start;
    size_t tmp_pos;
    size_t file_start;
    char prefix[256];
    int ret;
    int bytes_written = 0;
    CBioStream ostrm;
    CStringList enc_suffix_lst; // List over file suffix to encrypt
    CKey *key_obj;

    prefix[0] = 0;
    while((c = getopt(argc, argv, "o:i:D:s:t:vP:La:k:K:S:")) != -1) {
	switch (c) {
	case 'a': // Restart action
	    addRestartAction(optarg);
	    break;

	case 'P':
	    if (!optarg) usage(pname);
	    strcpy(prefix, optarg);
	    break;

	case 'L':
	    ignore_symlink = 1;
	    break;

	case 'o':
	    if (!optarg) usage(pname);
	    outfile = optarg;
	    break;

	case 'i':
	    if (!optarg) usage(pname);
	    packet_id = optarg;
	    break;

	case 's': // Serial
	    if (!optarg) usage(pname);
	    serial = optarg;
	    break;

	case 't': // Target for packfile. No encryption
	    if (!optarg) usage(pname);
	    target_dev = optarg;
	    break;

	case 'D':
	    if (optarg) 
		deps.push_back(optarg);
	    break;

	case 'v':
	    verbose++;
	    break;

	case 'k': // Private key to encrypt with.
	    if (!optarg) usage(pname);
	    key = optarg;
	    break;

	case 'K': // Directory where keys can be found
	    if (!optarg) usage(pname);
	    key_dir = optarg;
	    break;

	case 'S': // Add encryption suffix.
	    if (!optarg) usage(pname);
	    enc_suffix_lst.push_back(optarg);
	    break;


	case '?':
	default:
	    usage(pname);
	}
    }
    //
    // Add rest to content.
    //
    argc -= optind;
    argv += optind;

    if (outfile == "" || packet_id == "") {
	fprintf(stderr, "Missing out-file (-o) or packet-id (-p)\n");
	usage(pname);
	exit(255);
    }

    //
    // Check that we have a legal encryption setup, if specified.
    //
    if (key != "" || serial != "") {
	if (key != "" && serial != "") {
	    fprintf(stderr, "Only one of -k key and -s serial can be specified.\n");
	    usage(pname);
	    exit(255);
	}
	
	if (key_dir == "") {
	    fprintf(stderr, "No -K key-directory specified where key %s%s can be read.\n", key.c_str(), serial.c_str() );
	    usage(pname);
	    exit(255);
	}

	if (enc_suffix_lst.size() == 0) {
	    fprintf(stderr, "Encryption has been specified, but no -S file-suffix has\n");
	    fprintf(stderr, "been given to indicate which files are to be encrypoted.\n");
	    usage(pname);
	    exit(255);
	}

	// Init encryption
	ERR_load_crypto_strings();
	OpenSSL_add_all_ciphers();
	OpenSSL_add_all_digests();

	//
	// Laod keys.
	//
	if (!m1_keys().load_keys_dir(key_dir)) {
	    fprintf(stderr, "Could not load any keys from key directory [%s].\n", key_dir.c_str());
	    exit(255);
	}

	//
	// Setup encryption
	//
	if (key != "" && !(key_obj = m1_keys().key_by_name(key.c_str()))) {
	    fprintf(stderr, "Could not load private key %s.\n", key.c_str());
	    exit(255);
	}

	//
	// Setup encryption
	//
	if (serial != "" && !(key_obj = m1_keys().key_by_name(serial.c_str()))) {
	    fprintf(stderr, "Could not load private key %s (serial number).\n", serial.c_str());
	    exit(255);
	}
    }

    //
    // Validate packfile arguments.
    //
    if (!extractPacketID(packet_id, account, provider, packet_name, major, minor, patch)) {
	fprintf(stderr, "Malformed packet id: %s\n", packet_id.c_str());
	usage(pname);
    }    

    if (major == -1 || minor == -1 || patch == -1) {
	fprintf(stderr, "Malformed packet id (version): %s\n", packet_id.c_str());
	usage(pname);
   } 

    //
    // Validate format of all dependencies
    //
    for(CStringListIterator iter = deps.begin(); iter != deps.end(); ++iter) {
	string d_acc, d_prov, d_name;
	int d_maj, d_min, d_pat;
	if (!extractPacketID(*iter, d_acc, d_prov, d_name, d_maj, d_min, d_pat)) {
	    fprintf(stderr, "Malformed dependency: %s\n", iter->c_str());
	    usage(pname);
	}
    }

    //
    // Scan all files and remember their size.
    //
    while(*argv) {
	extractPath(*argv, file_list);
	++argv;
    }

    //
    // We now have a list of file entries and lengths to install.
    //
    unlink(outfile.c_str());// Remove if exist.

    if (!(out = fopen(outfile.c_str(), "w+"))) {
	perror(outfile.c_str());
	exit(255);
    } 

    // Write the header.
    lt = time(0);
    fprintf(out, "# Created: %s", ctime(&lt)); // Ctime includes \n
    fprintf(out, "<m1-packfile-start>\n");
    strcpy(buf, VERSION);
    if (strchr(buf, '.'))
	*strchr(buf, '.') = 0;

    fprintf(out, "Format-Version: %s\n", buf);
    fprintf(out, "From: %s@%s\n", account.c_str(), provider.c_str());
    if (target_dev != "") // This is a device specific packfile that cannot be installed anywhere else.
	fprintf(out, "Target-Device: %s\n", target_dev.c_str());
    else 
	if (serial != "") // This is a device specific packfile that cannot be installed anywhere else.
	    fprintf(out, "Target-Device: %s\n", serial.c_str());

    fprintf(out, "Name: %s\n", packet_name.c_str());
    fprintf(out, "Version: %d.%d.%d\n", major, minor, patch);

    buf[0] = 0;
    if (restart_action & ACTION_REBOOT)
	strcat(buf, "b");

    if (restart_action & ACTION_RESTART)
	strcat(buf, "s");

    if (restart_action & ACTION_RELOAD_LIB)
	strcat(buf, "l");

    if (restart_action & ACTION_RELOAD_M1)
	strcat(buf, "m");

    fprintf(out, "Restart-Action: %s\n", buf);

    // List all deps
    for(CStringListIterator iter = deps.begin(); iter != deps.end(); ++iter) 
	fprintf(out, "Needs: %s\n", iter->c_str());

    // List all content, and remember exactly where we should write the start parker
    for(CPFContentListIterator iter = file_list.begin(); iter != file_list.end(); ++iter) {
	char *strt;
	char *enc="";
	//
	// Setup basic entry.
	//
	if (iter->mPath.length() >= 4 && iter->mPath.substr(iter->mPath.length() - 4) == ".pfl") 
	    iter->mFileType.at(0) = 'p';

	if (key_obj != 0 && hasEncryptionSuffix(iter->mPath.c_str(), enc_suffix_lst)) {
	    printf("Will mar [%s] as encrypted\n", iter->mPath.c_str());
	    enc = "e";
	} 

	sprintf(buf, "Content: %s%s:0000000000:0000000000:0000000000:%o:%s%s\n", 
		prefix,
		iter->mPath.c_str(),
		iter->mPermission,
		iter->mFileType.c_str(),
		enc);

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

    for(CPFContentListIterator iter = file_list.begin(); iter != file_list.end(); ++iter) {
	size_t res;
	FILE *in;
	int bytes_left = 0;

	// Skip everything but regular files.
	if (iter->mFileType.at(0) != 'r' && iter->mFileType.at(0) != 'p') 
	    continue;

	if (verbose == 2) 
	    if (iter->mFileType.at(0) == 'p')
		fprintf(stderr, "Adding [%s] as a packfile\n", iter->mPath.c_str());
	    else
		fprintf(stderr, "Adding [%s] as a regular file\n", iter->mPath.c_str());

	in = fopen(iter->mPath.c_str(), "r");
	if (!in) {
	    fprintf(stderr, "Could not reopen source file [%s]\n", iter->mPath.c_str());
	    exit(255);
	}
	// Mark where file starts.
	file_start = ftell(out);

	//
	// Create an CBIOStream out stream for this file
	//
	ostrm.open_fp_write(iter->mPath.c_str(), out);

	ostrm.write_MAGIC();
	if (key_obj != 0 && hasEncryptionSuffix(iter->mPath.c_str(), enc_suffix_lst)) {
	    printf("Will encrypt [%s]\n", iter->mPath.c_str());
	    ostrm.write_ENCRYPTION(key_obj);
	} 
	// Always compress.
	ostrm.write_COMPRESSION(-1, 0);
	ostrm.write_DATA(0);

	//
	// Copy this file from input to packfile.
	//
	bytes_left = iter->mSize;
	while(bytes_left > 0) {
	    int read_sz = (bytes_left > blocksize)?blocksize:bytes_left;
	    int read_res;
	    int write_res;

	    // Read next chunk
	    if ((read_res = fread(rd_buf, 1, read_sz, in)) != read_sz) {
		fprintf(stderr, "Could not read from source file [%s]. Wanted %d bytes, got %d\n", 
			iter->mPath.c_str(), read_sz, read_res);
		exit(255);
	    }

	    // Write chunk, optionally encrypted but always compressed.
	    if ((write_res = ostrm.write(rd_buf, read_sz)) != read_sz) {
		fprintf(stderr, "Could not write to packfile for source [%s]. Wanted %d bytes, wrote %d\n", 
			iter->mPath.c_str(), read_sz, write_res);
		exit(255);
	    }
	    bytes_left -= read_res;
	    //	    printf("Dumped [%d] out of [%d] bytes\n", iter->mSize - bytes_left, iter->mSize);
	}

	fclose(in);

	//
	// Patch in the correct starting point and length in the header
	//
	ostrm.flush();
	tmp_pos = ftell(out);
	ostrm.close();
	fseek(out, iter->mStart, SEEK_SET);
	fprintf(out, "%.10d:%.10d:%.10d", file_start - cont_start, iter->mSize, tmp_pos - file_start);
	//	printf("[%s] ContentLength[%d] PackfileLength[%d]\n", iter->mPath.c_str(), iter->mSize, tmp_pos - file_start);
	fseek(out, tmp_pos, SEEK_SET);
    }	
    fprintf(out, "\n<m1-packfile-end>\n");
    fclose(out);
}
