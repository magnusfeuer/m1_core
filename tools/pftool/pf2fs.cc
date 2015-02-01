//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

// Install util for packfiles

#include "packfile.hh"
#include "key.hh"
#include "key_store.hh"
#include <openssl/err.h>
#include <getopt.h>

void usage(char *aName)
{
    fprintf(stderr, "Usage: %s -r root-dir -d install-db-dir  -p packfile-dir [-K key-dir] [-k private-key] [-S suffix] \n", aName);
    fprintf(stderr, "          [-d dependency-packet-id ] [(-s serial | -k key)  -K key-directory -S suffix] path ...\n\n");
    fputs("-r root-dir               Top directory to install packfile content in.\n\n", stderr);
    fputs("-d install-db-dir         Directory where to store information about installed packfiles.\n\n", stderr);
    fputs("-p packfile-dir           Directory to scan for packfiles.\n\n", stderr);
    fputs("-K key-directory          Directory to load keys from. Needed for -k or if packfile has encrypted content.\n\n", stderr);
    fputs("-k key                    Encrypt all files ending with suffixes given -S with key as the files are written to disk.\n\n", stderr);
    fputs("-S suffix                 Specifies the suffix of files to be encrypted as they are written to disk\n", stderr);
    fputs("                          This argument an be given multiple times.\n\n", stderr);
    fputs("Content in a packfile can either be encrypted in the file (by fs2pf or db2pf), or be encrypted as\n", stderr);
    fputs("it is unpacked and written to disk by this program.\n", stderr);
    fputs("To decrypt encrypted packfile content, the public key must be in the directory specified by -K key-directory.\n", stderr);
    fputs("To encrypt unencrypted packfile content as it is written to disk, the private key must be in the directory\n", stderr);
    fputs("specified by -K key-directory.\n", stderr);
    exit(255);
}


main(int argc, char *argv[])
{
    unsigned char buf[1024*1024];
    CPacketList lst;
    CStringList suffix;
    CBioStream tmp; // Forces seeding of PRNG
    string key = "";
    string key_dir = "";
    CStringList enc_suffix_lst; // List over file suffix to encrypt
    string root_dir = "";
    string db_dir = "";
    string pf_dir = "";
    char c;

    while((c = getopt(argc, argv, "r:d:p:k:K:S:")) != -1) {
	switch (c) {
	case 'r': // Restart action
	    if (!optarg) usage(argv[0]);
	    root_dir = optarg;
	    break;

	case 'd':
	    if (!optarg) usage(argv[0]);
	    db_dir = optarg;
	    break;

	case 'p':
	    if (!optarg) usage(argv[0]);
	    pf_dir = optarg;
	    break;

	case 'k': // Private key to encrypt with.
	    if (!optarg) usage(argv[0]);
	    key = optarg;
	    break;

	case 'K': // Directory where keys can be found
	    if (!optarg) usage(argv[0]);
	    key_dir = optarg;
	    break;

	case 'S': // Add encryption suffix.
	    if (!optarg) usage(argv[0]);
	    enc_suffix_lst.push_back(optarg);
	    break;

	case '?':
	default:
	    usage(argv[0]);
	}
    }

    if (root_dir == "") {
	fprintf(stderr, "No -r root-dir argument given\n\n");
	usage(argv[0]);
	exit(255);
    }

    if (db_dir == "") {
	fprintf(stderr, "No -d db-dir argument given\n\n");
	usage(argv[0]);
	exit(255);
    }

    if (pf_dir == "") {
	fprintf(stderr, "No -p packfile-dir argument given\n\n");
	usage(argv[0]);
	exit(255);
    }

    //
    // Check that we have everything needed to encrypt unencrypted packfile
    // content as it is written to disk.
    //
    if (key != "" || enc_suffix_lst.size() > 0 || key_dir != "") {
	if (key != "" && (key_dir == "" || enc_suffix_lst.size() == 0)) {
	    fprintf(stderr, "If -k key is specified, a -K key-dir and -S suffix must also be given\n");
	    exit(255);
	}
    }
	
    // Read installed-db directury
    if (!CPacketManager::installedPackets()->loadHeaders(db_dir)) 
	exit(-1);


    // Read headers of new packets
    if (!CPacketManager::newPackets()->loadHeaders(pf_dir)) {
	fprintf(stderr, "Could not load packfile headers from [%s]\n", pf_dir.c_str());
	exit(-1);
    }

    if (!CPacketManager::newPackets()->packets().size()) {
	fprintf(stderr, "No packfiles found in [%s]\n", pf_dir.c_str());
	exit(0);
    }
//    CPacketManager::installedPackets()->dump();
//    CPacketManager::newPackets()->dump();

    if (CPacketManager::newPackets()->validatePackets(lst)) {
	printf("Missing packets:\n");
	for (CPacketListIterator iter = lst.begin(); iter != lst.end(); ++iter)
	    printf("  %s@%s/%s version %d.%d.%d\n",
		   (*iter)->fromAccount().c_str(),
		   (*iter)->fromProvider().c_str(),
		   (*iter)->name().c_str(),
		   (*iter)->versionMajor(),
		   (*iter)->versionMinor(),
		   (*iter)->versionPatch());
	exit(-1);
    }

    CInstallIterator mInstallIter(CPacketManager::newPackets(), buf, 1024*1024, root_dir, db_dir);
    
    // Init encryption
    ERR_load_crypto_strings();
    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();

    //
    // Setup encryption if we are to encrypt unencrypted files
    // in the packfile as they are written to disk.
    //
    if (key_dir != "") {
	mInstallIter.addKeyPath(key_dir);
	mInstallIter.loadKeys();
    }

    if (key != "")
	mInstallIter.setEncryptionKey(key);

    // Add suffixes to encrypt upon unpacking.
    while(enc_suffix_lst.begin() != enc_suffix_lst.end())  {
	mInstallIter.addEncryptionSuffix(enc_suffix_lst.front());
	enc_suffix_lst.pop_front();
    }

    mInstallIter.reset();
    while(1) {
	if (mInstallIter.installChunk())
	    continue;

	if (mInstallIter.nextFile())
	    continue;

	if (mInstallIter.nextPacket())
	    continue;

	break;
    }

    exit(0);
}
