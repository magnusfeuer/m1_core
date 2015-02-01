//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __PACKFILE_HH__
#define __PACKFILE_HH__


//#include "m1.hh"
#include <list>
#include <string>
#include <bio_stream.hh>
using namespace std;


class CPacket;
class CPackFile;

typedef list<CPackFile *> CPackFileList;
typedef list<CPackFile *>::iterator CPackFileListIterator;

typedef list<CPacket *> CPacketList;
typedef list<CPacket *>::iterator CPacketListIterator;

typedef list<string> CStringList;
typedef list<string>::iterator CStringListIterator;

struct CKVPair {
    CKVPair(const CKVPair &aSource):
	mKey(aSource.mKey),
	mValue(aSource.mValue) {}
    CKVPair(const string &aKey, const string &aValue):
	mKey(aKey),
	mValue(aValue) {}

    string mKey;
    string mValue;
};


class CKVPairList: public list<CKVPair> {
public:
    CKVPairList(void) {}

    int find(const string &aKey, CStringList &aResult) {
	int res = 0;
	for (iterator iter = begin(); iter != end(); ++iter)
	    if ((*iter).mKey == aKey) {
		aResult.push_back((*iter).mValue);
		++res;
	    }
	return res;
    }
    string find(const string &aKey) {
	for (iterator iter = begin(); iter != end(); ++iter)
	    if ((*iter).mKey == aKey) {
		return (*iter).mValue;
	    }
	return "";
    }
    // Load key value pairs from a file. Return start of content as bytes into file.
    size_t load(const string &aFileName);
    size_t load(FILE *aIn);
private:
};    



//
// The packet manager retrieves all files in aHomeDirectory
// and reads their content. Each file describes one installed package and 
// contains the fields as provided in the original packfile's header.
//
class CPacketManager {
private:
    CPacketManager(void);

public:
    static CPacketManager *installedPackets(void); 
    static CPacketManager *newPackets(void); 

    CPacketList &packets(void); 
  //
    // Install a packet.
    // Remove any old version of this packet as well.
    //
    bool packet(CPacket *aPacket); 
    void addPacket(CPacket *aPacket);
    // Return first matching
    CPacket *find(const string &aAccount, 
		  const string &aProvider, 
		  const string &aPacketName,
		  int aMajor,
		  int aMinor,
		  int aPatch);

    // Return all matching
    int find(const string &aAccount, 
	     const string &aProvider, 
	     const string &aPacketName,
	     int aMajor,
	     int aMinor,
	     int aPatch,
	     CPacketList &aResult);

    bool loadHeaders(const string &aDirectory);
    int validatePackets(CPacketList &aMissingPackets);

    void dump(void);
    bool needReboot(void) { return mNeedReboot; }
    bool needRestart(void) { return mNeedRestart; }
    bool needLibReload(void) { return mNeedLibReload; }
    bool needM1Reload(void) { return mNeedM1Reload; }
    CPacket *loadHeader(const string &aFileName);
    CPacket *loadHeader(FILE *aIn, const string &aFileName);
    void reset(void) { mPackets.clear(); mNeedReboot = mNeedRestart = mNeedLibReload = mNeedM1Reload = false; }
private:
    CPacketList mPackets;
    bool mNeedReboot;
    bool mNeedRestart;
    bool mNeedLibReload;
    bool mNeedM1Reload;
};


class CFile {
public:
    CFile(const string &aPath,
	  size_t aStart,
	  size_t aContentSize,
	  size_t aPackfileSize,
	  mode_t aPermission,
	  const string &aFileType):
	mPath(aPath),
	mStart(aStart),
	mContentSize(aContentSize),
	mPackfileSize(aPackfileSize),
	mPermission(aPermission),
	mFileType(aFileType),
	mIsEncrypted((aFileType.size() == 2 && (aFileType[0] == 'p' || aFileType[0] == 'r') && aFileType[1] == 'e')) { }

    string path(void) { return mPath; }
    size_t start(void) { return mStart; }
    size_t contentSize(void) { return mContentSize; }
    size_t packfileSize(void) { return mPackfileSize; }
    mode_t permission(void) { return mPermission; }
    string fileType(void) { return mFileType; }
    bool isEncrypted(void) { return mIsEncrypted; }
private:
    string mPath;
    size_t mStart;
    size_t mContentSize;
    size_t mPackfileSize;
    mode_t mPermission;
    string mFileType;
    bool mIsEncrypted;
};




typedef list<class CFile> CFileList;
typedef list<class CFile>::iterator CFileListIterator;

class CPacket {
public:
    // Unresolved packet.
    CPacket(const string aAccount, 
	    const string aProvider,
	    const string aPacket,
	    int aVersionMajor,
	    int aVersionMinor,
	    int aVersionPatch);

    ~CPacket(void);

    int formatVersion(void) { return mFormatVersion; }
    const string &fromAccount(void) { return mFromAccount; }
    const string &fromProvider(void) { return mFromProvider; }
    const string &targetDevice(void) { return mTargetDevice; }
    const string &name(void) { return mName; }
    int versionMajor(void) { return mVersionMajor; }
    int versionMinor(void) { return mVersionMinor; }
    int versionPatch(void) { return mVersionPatch; }
    CPacketList &dependencies(void) { return mDependencyList; }
    CFileList &content(void) { return mFileList; }
    bool addDBEntry(const string &aDirectory);
    bool resolved(void) { return mResolved; }
    void resolved(bool aResolved) {  mResolved = aResolved; }
    bool failedDependencies(CPacketList &aMissing);
    bool checkDiskSpace(const string &aRoot) { return true; } // FIXME! statfs(2)
    int validatePacket(CPacketList &aMissing);
    void targetDevice(const string &aTargetDevice) { mTargetDevice = aTargetDevice; }
    void addContent(CStringList &aContent);
    void addDependencies(CStringList &aDependencies);
    void contentStart(size_t aContentStart) { mContentStart = aContentStart; }
    size_t contentStart(void) { return mContentStart; }
    void packFile(string aPackFile) { mPackFile = aPackFile; }
    string packFile(void) { return mPackFile; }

    int resolveEmbeddedPackfiles(FILE *aIn, CPacketManager *aManager);
    void dump(void);

    bool embedded(void) { return mEmbedded; } 
    void embedded(bool aEmbedded) { mEmbedded = aEmbedded; } 

private:
    bool addContent(const string &aContent);
    CPacket *addDependency(const string &aDependency);
    string mPackFile;
    int mFormatVersion;
    string mFromAccount;
    string mFromProvider;
    string mTargetDevice;
    string mName;
    int mVersionMajor;
    int mVersionMinor;
    int mVersionPatch;
    size_t mContentStart;
    CPacketList mDependencyList;
    CFileList mFileList;
    bool mResolved;
    bool mEmbedded; // NOT USED
};

extern "C" int installIteratorBIORead(BIO *bio,char *buf,int len);

class CInstallIterator {
public:
    CInstallIterator(CPacketManager *aManager, unsigned char *aBuffer, int aBufferSize, const string aRoot, const string aDBDirectory):
	mRoot(aRoot),
	mDBDirectory(aDBDirectory),
	mOutFileName(""),
	mManager(aManager),
	mProgress(0.0),
	mTotalSize(0),
	mTotalRead(0),
	mEnd(true),
	mInFile(0),
	mBuffer(aBuffer),
	mBufferSize(aBufferSize),
	mBytesLeft(0),
	mKey(0),
	mCompressionLevel(0) {
    }
    ~CInstallIterator(void);

    bool atEnd(void) { return mEnd; }

    void addKeyPath(string aPath) { mKeyPaths.push_back(aPath); }
    void addEncryptionSuffix(string aSuffix) { mEncryptionSuffix.push_back(aSuffix); }
    int loadKeys(void);
    bool setEncryptionKey(string aKeyName);
    void setCompressionLevel(int aLevel) { mCompressionLevel = aLevel; }

    FILE *pflDescriptor(void) { return mInFile; }
    CBioStream  *outStream(void) { return &mOutStream; }
    CBioStream  *inStream(void) { return &mInStream; }

    bool reset(void);

    bool nextPacket(void);
    bool nextFile(void);
    bool preparePacketInstall(void);
    bool prepareFileInstall();
    bool installChunk(void);
    string currentPacket(void);
    string currentFile(void);
    size_t totalSize(void) { return mTotalSize; }
    size_t totalRead(void) { return mTotalRead; }
    float progress(void) { return mProgress; }
    bool hasEncryptionSuffix(string aPath);
    
private:
    string mRoot;
    string mDBDirectory;
    string mOutFileName;
    CPacketManager *mManager;
    CPacketListIterator mPacketIter;
    CFileListIterator mFileIter;
    float mProgress; // Total progress for all packages 0.0 - 1.0
    size_t mTotalSize; // Number of bytes in all files in all packets. Calced by reset
    size_t mTotalRead; // Number of bytes read in total
    bool mEnd;
    FILE *mInFile;
    FILE *mOutFile; // For raw copying
    CBioStream mInStream;
    CBioStream mOutStream;
    unsigned char *mBuffer; // Buffer
    size_t mBufferSize; // Total buffer size.
    size_t mBytesLeft; // Bytes left to copy on the current file
    CStringList mKeyPaths; // Search path for keys.
    CStringList mEncryptionSuffix; // Search path for keys.
    CKey  *mKey;
    int mCompressionLevel;
};

// Helper functions
extern bool extractFrom(const string &aFrom,
			string &aAccount, 
			string &aProvider);

extern bool extractPacketID(const string &aID, 
			    string &aAccount, 
			    string &aProvider, 
			    string &aPacket,
			    int &aMajor,
			    int &aMinor,
			    int &aPatch);

extern int readPackFileLine(FILE *aIn, bool &aStartMarkerFound, string &aName, string &aValue);
extern bool createDirectory(const string &aPath,int aPermission);

#endif // __PACKFILE_HH__
