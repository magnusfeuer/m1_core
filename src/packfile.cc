//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#include "packfile.hh"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include "key.hh"
#include "key_store.hh"
#include <openssl/err.h>
#include "zlib.h"

#ifdef PRINT_ERR
#ifdef DBGFMT_WARN
#undef DBGFMT_WARN
#endif
#define DBGFMT_WARN(...) { printf(__VA_ARGS__); putchar('\n'); }
#endif

#ifdef PRINT_DBG
#ifdef DBGFMT
#undef DBGFMT
#endif
#define DBGFMT(...) { printf(__VA_ARGS__); putchar('\n'); }
#endif

int readPackFileLine(FILE *aIn, bool &aStartMarkerFound, string &aName, string &aValue)
{
    char buf[2048];
    char *name;
    char *value;

    if (!fgets(buf, 2048, aIn) || strstr(buf, "<m1-packfile-end>")) // EOF.
	return -1;

    aName = "";
    if (!aStartMarkerFound) {
	if (!strstr(buf, "<m1-packfile-start>")) {
	    return 0;
	}
	else {
	    aStartMarkerFound = true;
	    return 0; // We cannot do anything with this line.
	}
    }

    // Kill newline
    buf[strlen(buf)-1] = 0;
    // Find start.
    name= buf;
    while(*name && (*name == ' ' || *name == '\t'))
	++name;

    // Comment?
    if (*name == '#')
	return 0;

    // Empty line?
    if (*name == 0)
	return -2;

    // Find :
    if (!(value = strchr(name, ':'))) {
	DBGFMT_WARN("CPackFile::readLine(): No : in line[%s]", name);
	return 0;
    }

    *value++ = 0;
    // Find start of value code.
    while(*value && (*value == ' ' || *value == '\t'))
	++value;
    
    aName = name;
    aValue = value;
    return 1;
}


bool extractFrom(const string &aFrom,
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
//     DBGFMT("CPacket::extractFrom(): From[%s] -> Account[%s] Provider[%s]",
// 	   aFrom.c_str(),
// 	   aAccount.c_str(),
// 	   aProvider.c_str());
    return true;
}


bool extractPacketID(const string &aID, 
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
    
    if (!extractFrom(aID, aAccount, tmp))
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

//     DBGFMT("CPacket::extractPacketID() ID[%s] Account[%s] Provider[%s] Packet[%s] Version[%d.%d.%d]",
// 	   aID.c_str(),
// 	   aAccount.c_str(),
// 	   aProvider.c_str(),
// 	   aPacket.c_str(),
// 	   aMajor,
// 	   aMinor,
// 	   aPatch);
    return true;
}


//
// Unresolved packet we are dependent on.
//
CPacket::CPacket(const string aAccount, 
		 const string aProvider,
		 const string aPacket,
		 int aVersionMajor,
		 int aVersionMinor,
		 int aVersionPatch):
    mPackFile(""),
    mFormatVersion(1),
    mFromAccount(aAccount),
    mFromProvider(aProvider),
    mTargetDevice(""),
    mName(aPacket),
    mVersionMajor(aVersionMajor),
    mVersionMinor(aVersionMinor),
    mVersionPatch(aVersionPatch),
    mContentStart(0),
    mResolved(false),
    mEmbedded(false) 
{
}


CPacket::~CPacket(void)
{
}


			      
bool CPacket::addContent(const string &aContent)
{
    string::size_type first_colon = aContent.find(":", 0);
    string::size_type second_colon = aContent.find(":", first_colon + 1);
    string::size_type third_colon = aContent.find(":", second_colon + 1);
    string::size_type fourth_colon = aContent.find(":", third_colon + 1);
    string::size_type fifth_colon = aContent.find(":", fourth_colon + 1);
    string file_name;
    size_t start;
    size_t content_length;
    size_t packfile_length;
    string file_type;
    mode_t mode;
    // Did we find the separators
    if (first_colon == string::npos ||
	second_colon == string::npos ||
	third_colon == string::npos ||
	fourth_colon == string::npos ||
	fifth_colon == string::npos) {
	DBGFMT("Malformed content string[%s]", aContent.c_str());
	return false;
    }

    file_name = aContent.substr(0, first_colon);
    start = atoi(aContent.substr(first_colon + 1, second_colon - first_colon - 1).c_str());
    content_length = atoi(aContent.substr(second_colon + 1, third_colon - second_colon - 1).c_str());
    packfile_length = atoi(aContent.substr(third_colon + 1, fourth_colon - third_colon - 1).c_str());
    mode = strtol(aContent.substr(fourth_colon + 1, fifth_colon - fourth_colon - 1).c_str(), 0, 8);
    file_type = aContent.substr(fifth_colon + 1);

    //    printf("  CPacket:: addContent(): File[%s] start[%u] ContentLen[%u] PackfileLen[%u] Type[%c]\n", 
    //	   file_name.c_str(), start, content_length, packfile_length, file_type.at(0));

    mFileList.push_back(CFile(file_name, start, content_length, packfile_length, mode, file_type));

    return true;
}


CPacket *CPacket::addDependency(const string &aNeeded)
{
    CPacket *need;
    string account, provider, packet;
    int major, minor, patch;

    //
    // Break down user@domain/packet into its components.
    //
    if (!extractPacketID(aNeeded, account, provider, packet, major, minor, patch))
	return 0;

    need = CPacketManager::installedPackets()->find(account, provider, packet, major, minor, patch);

    // Check if it is among the new packets (resolved or unresolved)
    if (!need)
	need = CPacketManager::newPackets()->find(account, provider, packet, major, minor, patch); 

    //
    // Add an unresolved packet if not found.
    //
    if (!need) {
	need = new CPacket(account, provider, packet, major, minor, patch);
	CPacketManager::newPackets()->addPacket(need);
	DBGFMT("Created dependency on non existing packet %s@%s/%s ver %d.%d.%d", 
	       account.c_str(), 
	       provider.c_str(), 
	       packet.c_str(),major, minor, patch);
    }

    mDependencyList.push_back(need);

    return need;
}


int CPacket::validatePacket(CPacketList &aMissing)
{
    int res = 0;
    for(CPacketListIterator iter = mDependencyList.begin();
	iter != mDependencyList.end();
	++iter)
	if (!(*iter)->resolved()) {
	    aMissing.push_back(*iter);
	    res++;
	}

    return res;
}


bool CPacket::addDBEntry(const string &aDirectory)
{
    char fname[512];
    FILE *out;
    CPacketList ent;
    //
    // Firt remove old entries.
    //
    CPacketManager::installedPackets()->find(fromAccount(), fromProvider(), name(), -1, -1, -1, ent);
    for (CPacketListIterator iter = ent.begin(); iter != ent.end(); ++iter) {
	sprintf(fname, "%s/%s@%s.%s.%d.%d.%d.pfl",
		aDirectory.c_str(),
		(*iter)->fromAccount().c_str(),
		(*iter)->fromProvider().c_str(),
		(*iter)->name().c_str(),
		(*iter)->versionMajor(),
		(*iter)->versionMinor(),
		(*iter)->versionPatch());
	DBGFMT("CPacket::addDBEntry(): Will remove existing entry [%s]", fname);
	unlink(fname);
    }
	
    //
    // Copy file header into db directory of installed packets.
    //
    sprintf(fname, "%s/%s@%s.%s.%d.%d.%d.pfl",
	    aDirectory.c_str(),
	    fromAccount().c_str(),
	    fromProvider().c_str(),
	    name().c_str(),
	    versionMajor(),
	    versionMinor(),
	    versionPatch());

    DBGFMT("CPacket::addDBEntry(): Writing DB entry to [%s]", fname);

    if (!(out = fopen(fname, "w"))) {
	DBGFMT_WARN("CPacket::addDBEntry(): Could not open DB file [%s] for writing", fname);
	return false;
    }
    fprintf(out, "<m1-packfile-start>\n");
    fprintf(out, "Format-Version: %d\n", formatVersion());
    fprintf(out, "From: %s@%s\n", fromAccount().c_str(), fromProvider().c_str());
    //    fprintf(out, "Target-Device: %s\n", targetDevice().c_str());
    fprintf(out, "Name: %s\n", name().c_str());
    fprintf(out, "Version: %d.%d.%d\n", versionMajor(), versionMinor(), versionPatch());
    for(CPacketListIterator iter = mDependencyList.begin(); iter != mDependencyList.end(); ++iter) 
	fprintf(out, "Needs: %s@%s/%s/%d.%d.%d\n", 
		(*iter)->fromAccount().c_str(),
		(*iter)->fromProvider().c_str(),
		(*iter)->name().c_str(),
		(*iter)->versionMajor(),
		(*iter)->versionMinor(),
		(*iter)->versionPatch());

    for(CFileListIterator iter = mFileList.begin(); iter != mFileList.end(); ++iter) 
	if (iter->fileType().at(0) != 'p')
	    fprintf(out, "Content:%s:0:0:0:%o:%s\n", 
		    (*iter).path().c_str(),
		    (*iter).permission(),
		    (*iter).fileType().c_str());

//    fsync(fileno(out));
    fclose(out);
    return true;
}



CPacketManager::CPacketManager(void):
    mNeedReboot(false),
    mNeedRestart(false),
    mNeedLibReload(false),
    mNeedM1Reload(false)
{
}



CPacketManager *CPacketManager::installedPackets(void)
{
    static CPacketManager *manager = (CPacketManager *) 0;

    if (!manager)
	manager = new CPacketManager();
    
    return manager;
}


CPacketManager *CPacketManager::newPackets(void)
{
    static CPacketManager *manager = (CPacketManager *) 0;

    if (!manager)
	manager = new CPacketManager();
    
    return manager;
}


CPacketList &CPacketManager::packets(void)
{
    return mPackets;
}


size_t CKVPairList::load(const string &aFileName) 
{
    FILE *in = fopen(aFileName.c_str(), "r");
    size_t offset;

    if (!in) {
	DBGFMT_WARN("CKVPairList::load(): Could not open [%s]", aFileName.c_str());
	return 0;
    }
    offset = load(in);
    fclose(in);
    return offset;
}


size_t CKVPairList::load(FILE *aIn) 
{
    int res = 0;
    bool start_marker_found = false;
    string name, value;

    //
    // Read header info
    //
    while(1) {
	res = readPackFileLine(aIn, start_marker_found, name, value);

	if (res == 0)
	    continue;
	
	// EOF or empty line
	if (res < 0) 
	    break;

	push_back(CKVPair(name, value));
    }

    return ftell(aIn);
}


void CPacket::addContent(CStringList &aContent) 
{
    DBGFMT("Adding content for [%s]", name().c_str());
    for(CStringListIterator iter = aContent.begin(); iter != aContent.end(); ++iter)
	addContent(*iter);
    DBGFMT("---");
}

void CPacket::addDependencies(CStringList &aDependencies) 
{
    for(CStringListIterator iter = aDependencies.begin(); iter != aDependencies.end(); ++iter)
	addDependency(*iter);
}

CPacket *CPacketManager::loadHeader(const string &aFileName)
{
    FILE *in = fopen(aFileName.c_str(), "r");
    CPacket *res;

    //    printf("Loading header from pfl [%s]\n", aFileName.c_str());
    if (!in) {
	DBGFMT_WARN("CPacket::leadHeader(): Could not open [%s]", aFileName.c_str());
	return 0;
    }
    res = loadHeader(in, aFileName);
    fclose(in);
    return res;
}

CPacket *CPacketManager::loadHeader(FILE *aIn, const string &aFileName) 
{
    CKVPairList lst;
    string format_version;
    string version;
    int major;
    int minor;
    int patch;
    string from;
    string account;
    string provider;
    string target_dev = "";
    string name;
    string post_action;
    CStringList deps;
    CPacket *res;
    CStringList content;
    size_t content_start;

    content_start =lst.load(aIn);
    major = minor = patch = -1;

    if ((format_version = lst.find("Format-Version")) == "" ||
	(from = lst.find("From")) == "" ||
	!extractFrom(from, account, provider) ||
	(name = lst.find("Name")) == "" ||
	(version = lst.find("Version")) == "" ||
	sscanf(version.c_str(), "%d.%d.%d", &major, &minor, &patch) != 3) {
	DBGFMT_WARN("Malformed header");
	return 0;
    }
    target_dev = lst.find("Target-Device");
    if (target_dev != "" && target_dev.size() != 6)
	target_dev = "??????";

    lst.find("Needs", deps);
    lst.find("Content", content);

    post_action = lst.find("Restart-Action");
    if (post_action.find("b") != string::npos) 
	mNeedReboot = true;

    if (post_action.find("s") != string::npos) 
	mNeedRestart = true;

    if (post_action.find("l") != string::npos) 
	mNeedLibReload = true;

    if (post_action.find("m") != string::npos) 
	mNeedM1Reload = true;

    //
    // See if we can find this packet in our existing list.
    // The packet found may be unresolved.
    //
    res = find(account, provider, name, major, minor, patch); 
    if (!res) {
	res = new CPacket(account, provider, name, major, minor, patch);
// 	printf("Created new %s@%s/%s ver %d.%d.%d\n", 
// 	       account.c_str(), 
// 	       provider.c_str(), 
// 	       name.c_str(),major, minor, patch);
	addPacket(res);
    } 
//     else
// 	printf("Found existing %s@%s/%s ver %d.%d.%d\n", 
// 	       account.c_str(), 
// 	       provider.c_str(), 
//	       name.c_str(),major, minor, patch);

    res->targetDevice(target_dev);
    // Only setup dependencies if we are doing new packets
    if (this == CPacketManager::newPackets())
	res->addDependencies(deps);
    res->addContent(content);
    res->contentStart(content_start);
    res->packFile(aFileName);
    res->resolved(true);

    res->resolveEmbeddedPackfiles(aIn, this);
    return res;
}



// 
// Go through all content files and see if any of them are packfiles.
// If so, setup their content for installation as well. This one is recursive.
//
int CPacket::resolveEmbeddedPackfiles(FILE *aIn, CPacketManager *aManager)
{
    long org_pos = ftell(aIn);
    int count = 0;

    for(CFileListIterator iter = content().begin();
	iter != content().end();
	++iter) {
	if (iter->fileType().at(0) == 'p') {
	    CPacket *pack;
	    // Jump to start of file.
	    fseek(aIn, iter->start() + contentStart(), SEEK_SET); 
	    //	    printf("Will resolve internal packfile [%s]\n", iter->path().c_str());
	    pack = aManager->loadHeader(aIn, packFile()); // Will get content start offset right?
	    if (pack) {
		//		pack->packFile(packFile()); // Set the packfile to point to the same file as self.
		++count;
	    }
	}
    }

    // Reset original position
    fseek(aIn, org_pos, SEEK_SET);

    return count;
}

bool CPacketManager::loadHeaders(const string &aDirectory)
{
    struct dirent *d_ent;
    DIR *dir;

    mNeedRestart = false;
    mNeedReboot = false;
    mNeedLibReload = false;
    mNeedM1Reload = false;

    dir = opendir(aDirectory.c_str());
    if (!dir) {
	printf("CPacketManager::loadHeaders(): Could not open [%s]: %s\n",
		    aDirectory.c_str(), strerror(errno));
	return false;
    }
    //
    // Get all entries ending with .pfl (pack file)
    // Note: Not thread safe...
    //
    while((d_ent = readdir(dir))) {
	//
	// Check that it ends with .pfl and has at least one more 
	// character before the .pfl suffix
	//
	if (strlen(d_ent->d_name) < 5 || strcmp(d_ent->d_name + strlen( d_ent->d_name) - 4, ".pfl"))
	    continue;

	loadHeader(string(aDirectory + "/" + string(d_ent->d_name)).c_str());
    }

    closedir(dir);
    return true;
}

void CPacketManager::addPacket(CPacket *aPacket) 
{
    mPackets.push_back(aPacket);
}


CPacket *CPacketManager::find(const string &aAccount, 
			      const string &aProvider, 
			      const string &aPacketName,
			      int aMajor,
			      int aMinor,
			      int aPatch)
{
    for(CPacketListIterator iter = mPackets.begin(); iter != mPackets.end(); ++iter)
	if ((*iter)->fromAccount() == aAccount &&
	    (*iter)->fromProvider() == aProvider &&
	    (*iter)->name() == aPacketName &&
	    ((*iter)->versionMajor() == aMajor || aMajor == -1 || (*iter)->versionMajor() == -1) &&
	    ((*iter)->versionMinor() == aMinor || aMinor == -1 || (*iter)->versionMinor() == -1) && 
	    ((*iter)->versionPatch() == aPatch || aPatch == -1 || (*iter)->versionPatch() == -1))
	    return *iter;
    
    return (CPacket *) 0;
}

int CPacketManager::find(const string &aAccount, 
			 const string &aProvider, 
			 const string &aPacketName,
			 int aMajor,
			 int aMinor,
			 int aPatch,
			 CPacketList &aResult)
{
    int res = 0;

    for(CPacketListIterator iter = mPackets.begin(); iter != mPackets.end(); ++iter)
	if ((*iter)->fromAccount() == aAccount &&
	    (*iter)->fromProvider() == aProvider &&
	    (*iter)->name() == aPacketName &&
	    ((*iter)->versionMajor() == aMajor || aMajor == -1 || (*iter)->versionMajor() == -1) &&
	    ((*iter)->versionMinor() == aMinor || aMinor == -1 || (*iter)->versionMinor() == -1) && 
	    ((*iter)->versionPatch() == aPatch || aPatch == -1 || (*iter)->versionPatch() == -1)) {
	    aResult.push_back(*iter);
	    ++res;
	}

    
    return res;
}

int CPacketManager::validatePackets(CPacketList &aMissingPackets)
{
    int res = 0;
    for (CPacketListIterator iter = mPackets.begin(); 
	 iter != mPackets.end();
	 ++iter) {
	
	res += (*iter)->validatePacket(aMissingPackets);
    }
    return res;
}




void CPacket::dump(void)
{
//     DBGFMT("REF: %p\n", this);
    printf("%s@%s/%s/%d.%d.%d\n", 
	   fromAccount().c_str(),
	   fromProvider().c_str(),
	   name().c_str(), 
	   versionMajor(),
	   versionMinor(),
	   versionPatch());
    for (CFileListIterator iter = content().begin(); iter != content().end(); ++iter)
	printf("  File: [%s] size[%d] type[%s]\n", 
	       (*iter).path().c_str(),
	       (int)(*iter).contentSize(),
	       (*iter).fileType().c_str());

    for (CPacketListIterator iter = dependencies().begin(); iter != dependencies().end(); ++iter)
	printf("Depends  %s@%s/%s/%d.%d.%d  [%s]\n", 
	       (*iter)->fromAccount().c_str(),
	       (*iter)->fromProvider().c_str(),
	       (*iter)->name().c_str(), 
	       (*iter)->versionMajor(),
	       (*iter)->versionMinor(),
	       (*iter)->versionPatch(),
	       (*iter)->resolved()?"Resolved":"Unresolved");
}

void CPacketManager::dump(void) 
{
    for (CPacketListIterator iter = mPackets.begin(); 
	 iter != mPackets.end();
	 ++iter) {
	
	(*iter)->dump();
    }
    
}

CInstallIterator::~CInstallIterator(void)
{
}

bool createDirectory(const string &aPath, int aPermission)
{
    struct stat d_stat;
    string::size_type slash = aPath.find_last_of("/");


    // Check for null creations
    if (aPath == "")
	return true;

    if (slash != string::npos) 
	createDirectory(aPath.substr(0, slash), aPermission);
    

    //
    // Check if this path already exists and is not a directory.
    //
    if (!stat(aPath.c_str(), &d_stat)) {
	if (!S_ISDIR(d_stat.st_mode)) {
	    DBGFMT_WARN("[%s] already exists and is not a directory", aPath.c_str());
	    return false;
	} 
    } else {
// 	DBGFMT("Creating directory component [%s]", aPath.c_str());
	if (mkdir(aPath.c_str(), aPermission) == -1) {
	    DBGFMT_WARN("Could not create [%s]: [%s]", aPath.c_str(), strerror(errno));
	    return false;
	}
    }
    return true;
}


bool CInstallIterator::reset(void)  
{
    CPacketListIterator p_iter;
    CFileListIterator f_iter;
    mEnd = false;

    
    if (mOutStream.get_bio() != 0) {
	mOutStream.flush();
	mOutStream.close();
    }

    if (mInFile) {
	fclose(mInFile);
	mInFile = 0;
    }

    if (mManager->packets().begin() == mManager->packets().end())   {
	printf("No packets\n");
	mEnd = true;
	return false;
    }
    // Set to first packet with files.
    mPacketIter = mManager->packets().begin();

    // Setup first file.
    if (!preparePacketInstall()) {
	mEnd = true;
	printf("PreparePacketInstall failed\n");
	return false;
    }

    //
    // Go through all packets and all files to get a grand total
    // 
    mTotalSize = 0;
    mTotalRead = 0;
    for (p_iter = mManager->packets().begin(); 
	 p_iter != mManager->packets().end();
	 p_iter++) {
	DBGFMT("Packet [%s]", (*p_iter)->name().c_str());
	for (f_iter = (*p_iter)->content().begin();
	     f_iter != (*p_iter)->content().end();
	     ++f_iter) {

	    if (f_iter->isEncrypted())
		mTotalSize += f_iter->packfileSize();
	    else
		mTotalSize += f_iter->contentSize();


	    DBGFMT("  File [%s] Type[%c]", f_iter->path().c_str(), f_iter->fileType().at(0));
	}

    }
    return true;
}

bool CInstallIterator::nextFile(void)
{
    do {
	mFileIter++;
	// No more files
	if (mFileIter == (*mPacketIter)->content().end()) {
	    //	    printf("No more files. Closing in descriptor\n");
	    fclose(mInFile);
	    mInFile = 0;
	    (*mPacketIter)->addDBEntry(mDBDirectory);

	    return false;
	}
    } while(!prepareFileInstall());
    return true;
}

bool CInstallIterator::nextPacket(void)
{
    ++mPacketIter;

    if (mPacketIter == mManager->packets().end())
	return false;
    
    return preparePacketInstall();
}


bool CInstallIterator::preparePacketInstall(void)
{
    CPacketList ent;

    DBGFMT("CInstallIterator::preparePacketInstall(): Prepping [%s]", (*mPacketIter)->name().c_str());
    // Do we have a complete header?
    if (!(*mPacketIter)->resolved()) {
	DBGFMT_WARN("CInstallIterator::preparePacketInstall(): Packfile [%s] does not have complete headers", (*mPacketIter)->packFile().c_str());
	return false;
    }

    if (!(*mPacketIter)->checkDiskSpace(mRoot)) {
	DBGFMT_WARN("CInstallIterator::preparePacketInstall(): Not enough diskspace to install packfile [%s]", (*mPacketIter)->packFile().c_str());
	return false;
    }


    // Can we open the packfile.
    if (!(mInFile = fopen((*mPacketIter)->packFile().c_str(), "r"))) {
	DBGFMT_WARN("CInstallIterator::preparePacketInstall(): Could not open packfile [%s]", (*mPacketIter)->packFile().c_str());
	return false;
    }

    mFileIter = (*mPacketIter)->content().begin();

    return prepareFileInstall();
}

bool CInstallIterator::prepareFileInstall(void) 
{
    string::size_type last_slash;
    FILE *outf = 0;

    // packfile?
     if (mFileIter->fileType().at(0) == 'p') {
	 // Skip since it is already installed as a CPackfile entry.
	 return true;
     }

    mOutFileName = mRoot + string("/") + mFileIter->path();
    DBGFMT("CInstallIterator::prepareFileInstall(): [%s]: File[%s] Start[%d] ContSize[%u] PackfileSize[%u] type[%s]",
	   (*mPacketIter)->packFile().c_str(),
	   mOutFileName.c_str(),
	   mFileIter->start() + (*mPacketIter)->contentStart(),
	   mFileIter->contentSize(),
	   mFileIter->packfileSize(),
	   mFileIter->fileType().c_str());


    //
    // Everything except files we can do immediately.
    //

    // Check if this is a directory 
    if (mFileIter->fileType().at(0) == 'd') {
	return createDirectory(mOutFileName, mFileIter->permission());
    }

    // Create relevant directory path if we have slash in path
    if ((last_slash = mOutFileName.find_last_of("/")) != string::npos) {
	if (!createDirectory(mOutFileName.substr(0, last_slash), 0777))
	    return false;
    }

    // Block device??
    if (mFileIter->fileType().at(0) == 'b') {
	dev_t maj_min = strtoull(mFileIter->fileType().c_str() + 1, 0, 10);
	if (mknod(mOutFileName.c_str(), S_IFBLK | mFileIter->permission(), maj_min) == -1) {
	    printf("Could not make block device [%s]: %s\n", mOutFileName.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    // char device??
    if (mFileIter->fileType().at(0) == 'c') {
	dev_t maj_min = strtoull(mFileIter->fileType().c_str() + 1, 0, 10);
	if (mknod(mOutFileName.c_str(), S_IFCHR | mFileIter->permission(), maj_min) == -1) {
	    printf("Could not make char device [%s]: %s\n", mOutFileName.c_str(), strerror(errno));

	    return false;
	}
	return true;
    }

    // Fifo?
    if (mFileIter->fileType().at(0) == 'f') {
	if (mknod(mOutFileName.c_str(), S_IFIFO | mFileIter->permission(), 0LL) == -1) {
	    printf("Could not make fifo [%s]: %s\n", mOutFileName.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    // Socket?
    if (mFileIter->fileType().at(0) == 's') {
	if (mknod(mOutFileName.c_str(), S_IFSOCK | mFileIter->permission(), 0LL) == -1) {
	    printf("Could not make unix socket [%s]: %s\n", mOutFileName.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }

    // Symlink?
    if (mFileIter->fileType().at(0) == 'S') {
	DBGFMT("Symlink [%s]->[%s]", mFileIter->fileType().c_str() + 1, mOutFileName.c_str());
	if (symlink(mFileIter->fileType().c_str() + 1, mOutFileName.c_str()) == -1) {
	    printf("Could not make symlink [%s]: %s\n", mOutFileName.c_str(), strerror(errno));
	    return false;
	}
	chmod(mOutFileName.c_str(), mFileIter->permission());
	return true;
    }

    // Hardlink
    if (mFileIter->fileType().at(0) == 'l') {
	if (link(mFileIter->fileType().c_str() + 1, mOutFileName.c_str()) == -1) {
	    printf("Could not make hardlink [%s]: %s\n", mOutFileName.c_str(), strerror(errno));
	    return false;
	}
	return true;
    }


    //
    // This is a regular file, set it up for copying
    //

    if (mOutStream.get_bio() != 0) {
	DBGFMT_WARN("CInstallIterator::nextFile(): WARNING: File descriptor still open. Will close");
	mOutStream.flush();
	mOutStream.close();
    }

    mOutFile = 0;

    // 
    // Rename or remove fileType before opening it
    //
    if ((mOutFileName.size() >= 10 && mOutFileName.substr(mOutFileName.size()-10) == "/sbin/init")  ||
	(mOutFileName.size() >= 3 && mOutFileName.substr(mOutFileName.size()-3) == ".so") ||  // Is shared object? or
	(mOutFileName.size() >= 3 && mOutFileName.substr(mOutFileName.size()-6) == "m1/m1e") ||  // Is shared object? or
	mOutFileName.find(".so.", 0) != string::npos) { // cannot write due to text file busy
	string new_name = mOutFileName + "._delete";
	//printf("Renaming busy/.so regular file[%s] -> [%s]\n", mOutFileName.c_str(), new_name.c_str());
	rename(mOutFileName.c_str(), new_name.c_str());
	errno = 0;
    } else {
	if (mOutFileName.find("/boot/grub", 0) == string::npos) { // Don't ever delete the boot loader.
	    //("Removing [%s]\n", mOutFileName.c_str());
	    unlink(mOutFileName.c_str());
	}
    }


    if (mFileIter->isEncrypted() && !(mOutFile = fopen(mOutFileName.c_str(), "w"))) {
	DBGFMT_WARN("CPacket::install(): Could not open rawtarget file [%s]", mOutFileName.c_str());
	return false;
    }

    if (!mFileIter->isEncrypted() && mOutStream.open_file_write(mOutFileName) == -1) {
	DBGFMT_WARN("CPacket::install(): Could not open target file [%s]", mOutFileName.c_str());
	return false;
    }

    // Set permission
    if (!mOutFile) {
	BIO_get_fp(mOutStream.get_bio(), &outf);
    } else
	outf = mOutFile;

    if (outf) 
	fchmod(fileno(outf), mFileIter->permission());
    else
	DBGFMT_WARN("CInstallIterator::nextFile(): Could not retrieve FILE * for BIO in CBioStream. Will not chmod(%s)", 
		    mOutFileName.c_str());
    // 
    // Move to content begining + file offset.
    //
    DBGFMT("Will seek to [%u] + [%u] = [%u]", 
	   (*mPacketIter)->contentStart(), mFileIter->start(),
	   (*mPacketIter)->contentStart() + mFileIter->start());
    

    // Setup a BIO input stream
    fseek(mInFile, (*mPacketIter)->contentStart() + mFileIter->start(), SEEK_SET);
    if (!mFileIter->isEncrypted())
	mInStream.open_fp_read((*mPacketIter)->name(), mInFile, 0, false, false);
    
    
    //
    // Setup encryption of file written to disk, if enabled and packfile content is 
    //
    if (!mFileIter->isEncrypted() && mKey != 0 && hasEncryptionSuffix(mOutFileName)) {
	// Make sure that this packfile entry is not encrypted!
	if (mInStream.is_encrypted()) {
	    DBGFMT_WARN("CInstallIterator::nextFile(): File [%s] is marked for encryption when written to disk, but it is already encrypted in packfile.\n",
			mOutFileName.c_str());
	    mOutStream.close();
	    mInStream.close();

	    mOutFileName = "";
	    mBytesLeft = 0;
	    return false; 
	}

	DBGFMT("Will encrypt [%s]", mOutFileName.c_str());
	mOutStream.write_MAGIC();
	mOutStream.write_ENCRYPTION(mKey);

	if (mCompressionLevel)
	    mOutStream.write_COMPRESSION(mCompressionLevel, 0);

	mOutStream.write_DATA(0);
    }


    //
    // If this file is encrypted in the packfile, we should copy it with no
    // modifications to disk. Use pack file size. 
    //
    if (mFileIter->isEncrypted())
	mBytesLeft = mFileIter->packfileSize();
    else
	mBytesLeft = mFileIter->contentSize();

    return true;
}


bool CInstallIterator::installChunk() 
{
    int rd_res = 0, wr_res;
    int max_read_size;

    // If we reached end of file, then mOutStream.get_bio() will be 0.
    if (!mOutFile && !mOutStream.get_bio()) {
//	puts("EOF");
	return false;
    }

    if (mBytesLeft > mBufferSize)
	max_read_size = mBufferSize;
    else
	max_read_size = mBytesLeft;

    // Read next chunk either from biostream or FILE
    if (!mFileIter->isEncrypted() && (rd_res = mInStream.read(mBuffer, max_read_size)) <= 0) {
	char buf[1024];
	ERR_error_string(ERR_get_error(), buf);
	DBGFMT_WARN("CPacket::installChunk(): Could not read from packfile [%s] Wanted [%d] Got [%d]: %s", 
		    mOutFileName.c_str(), max_read_size, rd_res, buf);
	goto finish_file;
    }

    // Read next chunk either from biostream or FILE
    if (mFileIter->isEncrypted() && (rd_res = fread(mBuffer, 1, max_read_size, mInFile)) <= 0) {
	DBGFMT_WARN("CPacket::installChunk(): Could not read from packfile [%s] Wanted [%d] Got [%d]", 
		    mOutFileName.c_str(), max_read_size, rd_res);
	goto finish_file;
    }


   // Deduct from number of bytes left to read.
    mBytesLeft -= rd_res;
    // Update total read bytes.

    mTotalRead += rd_res;
    if (!mFileIter->isEncrypted() && (wr_res = mOutStream.write(mBuffer, rd_res)) != (int) rd_res) {
	printf("CPacket::installChunk(): Could not write to file [%s] Tried [%u] Got [%d]\n", 
		    mOutFileName.c_str(), rd_res, wr_res);
	goto finish_file;
    }

    if (mFileIter->isEncrypted() && (wr_res = fwrite(mBuffer, 1, rd_res, mOutFile)) != (int) rd_res) {
	printf("CPacket::installChunk(): Could not raw write to file [%s] Tried [%u] Got [%d]: %s\n", 
	       mOutFileName.c_str(), rd_res, wr_res, strerror(errno));
	goto finish_file;
    }

    mProgress = float(mTotalRead) / float(mTotalSize); // Progress 0-1

    // Check if we have read all data from self.
    if (mBytesLeft == 0) 
	goto finish_file;

    return true; // We want more data!

 finish_file:
    if (!mFileIter->isEncrypted()) {
	mOutStream.flush();
	mOutStream.close();
	mInStream.close();
    } else {
	fclose(mOutFile);
	mOutFile = 0;
    }
    mOutFileName = "";
    mBytesLeft = 0;
    return false; // We are done with this file.
}

string CInstallIterator::currentFile(void) 
{
    if (!mManager || 
	mPacketIter == mManager->packets().end() ||
	mFileIter == (*mPacketIter)->content().end())
	return "";

    return mFileIter->path();
}

string CInstallIterator::currentPacket(void) 
{
    char buf[256];
    if (!mManager || 
	mPacketIter == mManager->packets().end())
	return "";

    sprintf(buf, "%s@%s/%s %d.%d.%d\n",
	    (*mPacketIter)->fromAccount().c_str(),
	    (*mPacketIter)->fromProvider().c_str(),
	    (*mPacketIter)->name().c_str(),
	    (*mPacketIter)->versionMajor(),
	    (*mPacketIter)->versionMinor(),
	    (*mPacketIter)->versionPatch());

    return string(buf);
}

int CInstallIterator::loadKeys(void) 
{
    int res = 0;
    for (CStringListIterator iter = mKeyPaths.begin();
	iter != mKeyPaths.end();
	++iter)
	res += m1_keys().load_keys_dir(*iter);

    return res;
}

// Setup encryption key for output.
bool CInstallIterator::setEncryptionKey(string aKeyName)
{
    if (!(mKey = m1_keys().key_by_name(aKeyName.c_str())) &&
	!(mKey = m1_keys().key_by_fingerprint(aKeyName.c_str())) &&
	!(mKey = m1_keys().key_by_serial(aKeyName.c_str()))) {
	DBGFMT_WARN("signer key '%s' not found", aKeyName.c_str());
	return false;
    }
    return true;
}

bool CInstallIterator::hasEncryptionSuffix(string aPath)
{
    for (CStringListIterator iter = mEncryptionSuffix.begin();
	 iter != mEncryptionSuffix.end();
	 ++iter) 
	if (aPath.size() >= iter->size() && !aPath.compare(aPath.size() - iter->size(), iter->size(), *iter)) 
	    return true;

    return false;
}
