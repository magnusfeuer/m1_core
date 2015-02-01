//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//
//

//
//  Very simple persistent storage mechanism tailored for 
//  raw devices. No filesystems necessary. Atomic updates.
//

#ifndef __DATABASE_HH__
#define __DATABASE_HH__
#define CDATABASE_MAGIC "*&!-This is an M1 database.1234"
#include <string>

#include <map>

using namespace std;

class CDatabase {
public:
    enum EElementType {
	    StringElement = 0,
	    FloatElement = 1,
	    IntElement = 2
    };
    struct CElement {
	CElement(const char *aValue):
	    mElementType(StringElement), mFloatValue(0), mIntValue(0)
	    { mStringValue = aValue; }

	CElement(const int aValue):
	    mElementType(IntElement), mStringValue(""), mFloatValue(0)
	    { mIntValue = aValue; }

	CElement(const float aValue):
	    mElementType(FloatElement), mStringValue(""), mIntValue(0)
	    { mFloatValue = aValue; }

	EElementType mElementType;
	string mStringValue; // String value cannot be stored in union, so I give up.
	float mFloatValue;
	int mIntValue;
    };

    struct CElementCompare {
	bool operator()(const string &s1,  const string &s2) {
	    return  (s1 < s2);
	}	
    };

    typedef map<string, CElement> CElementMap;
    typedef map<string, CElement, CElementCompare>::iterator CElementMapIterator;

//     typedef list<CElement> CElementList;
//     typedef list<CElement>::iterator CElementListIterator;

    CDatabase(void);
    ~CDatabase(void);

    //
    // Open database and do basic validation (magic number).
    //
    const string device(void) { return mDevice; }
    void device(const string &aDevice) { mDevice = aDevice; }
    int blocks(void) { return mBlocks; }
    void blocks(int aBlocks) { mBlocks = aBlocks; }
 
    int blockSize(void) { return mBlockSize; }
    void blockSize(int aBlockSize) { mBlockSize = aBlockSize; }
    bool open(void);
    void close(void);
    bool format(void);
    bool load(void);
    bool flush(void);
    bool dirty(void) { return mDirty; }

    bool remove(const string &aName);
    void clear(void) { mElements.clear(); }
    bool set(const string &aName, const string &aValue);
    bool get(const string &aName, string *aValue);

    bool set(const string &aName, const int aValue);
    bool get(const string &aName, int *aValue);

    bool set(const string &aName, const float aValue);
    bool get(const string &aName, float *aValue);
    CElementMapIterator begin(void) { return mElements.begin(); }
    CElementMapIterator end(void) { return mElements.end(); }

private:
    bool loadHeader(void);

    // Device layout is:
    // Byte 0: CHeader
    //
    // Byte CHeader::mBlockSize: current area used. 
    //     If 0, then area starts at mBlockSize*2 if 1, then area starts at mBlockSize*(mBlocks + 2)
    //
    // Byte mBlockSize*2: First area.
    // Byte mBlockSize*(mBlocks * 2): Second area.
    // Flush will write all data to the currently unused area, and then switches current start from 1 to 0 or 0 to 1.
    // Should be fairly safe in case of a power loss during a write.
    struct CHeader {
	char mMagic[32]; // Magic header, including null termination.
	int mBlocks;
	int mBlockSize;
    };
    

    // Header of each element in the database.
    // Followed directly by element data in native format.
    struct CElementHeader {
	char mName[64];
	EElementType mType;
	int mValueLength; // -1 == EOF.
    } elem_hdr;

    unsigned char mCurrentArea;
    string mDevice;
    int mBlocks;
    int mBlockSize;
    int mDescriptor;
    CElementMap mElements;
    bool mDirty; // Needs flushing.
};

#endif 
