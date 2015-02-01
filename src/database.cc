//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

//
// Simple persistent storage manager.
//
#include "database.hh"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include "m1.hh"
CDatabase::CDatabase(void):
    mCurrentArea(0),
    mDevice(""),
    mBlocks(128),
    mBlockSize(4096),
    mDescriptor(-1),
    mDirty(false)
{
}


CDatabase::~CDatabase(void)
{
    close();
}


void CDatabase::close(void)
{
    if (mDescriptor != -1) {
	DBGFMT("CDatabase::close() Closing file[%s]", mDevice.c_str());
	fsync(mDescriptor);
	::close(mDescriptor);
	mDescriptor = -1;
    }
}

bool CDatabase::loadHeader(void) 
{
    CHeader hdr;
    unsigned int read_size;
    DBGFMT("loadJeader(): Loading header from [%s]", mDevice.c_str());
    //
    // Read header block
    //
    lseek(mDescriptor, 0, SEEK_SET);
    
    if ((read_size = read(mDescriptor, (void *) &hdr, sizeof(CHeader)) != sizeof(CHeader))) {
	DBGFMT("loadHeader(): Could not read header. Wanted [%d] bytes got [%u]", (int) sizeof(CHeader), read_size);
	close();
	return false;
    }
    
    //
    // Check magic number.
    //
    if (memcmp(hdr.mMagic, CDATABASE_MAGIC, 32)) {
	DBGFMT("loadHeader(): Incorrect magic string at the beginning of header");
	close();
	return false;
    }
	
    
    if (hdr.mBlockSize < 0 || hdr.mBlockSize > 1024*1024) {
	DBGFMT("loadHeader(): Block size is out of range (0-1048576): [%d]", (int) hdr.mBlockSize);
	close();
	return false;
    }

    if (hdr.mBlocks < 1 || hdr.mBlocks > 1024*1024) {
	DBGFMT("loadHeader(): Block count is out of range (1-1048576): [%d]", hdr.mBlocks);
	close();
	return false;
    }

    //
    // Update block size.
    //
    mBlockSize = hdr.mBlockSize;
    mBlocks = hdr.mBlocks;

    //
    // Read area code.
    //
    lseek(mDescriptor, mBlockSize, SEEK_SET);
    if ((read_size = read(mDescriptor, (char *) &mCurrentArea, 1)) != 1) {
	DBGFMT("loadHeader(): Failed to read single byte area code.");
	close();
	return false;
    }

    if (mCurrentArea > 1) {
	DBGFMT("loadHeader(): Block count is out of range (0-1): [%d]", mCurrentArea);
	close();
	return false;
    }

    return true;
}

// Load header and entire database.
bool CDatabase::load(void) 
{
    int read_size;
    if (!open() || !loadHeader()) 
	return false;

    //
    // Read data from correct area.
    //
    lseek(mDescriptor, mBlockSize * 2 + (mCurrentArea * mBlocks)*mBlockSize, SEEK_SET ) ;
    //
    // Each record has format:
    // Name[64]
    // ValueLength[4]
    // Value[ValueLength]
    //
    mElements.clear();
    while(true) {
	char value[128*1024];
	char name[65];
	CElementHeader elem_hdr;

	if ((read_size = read(mDescriptor, (void *) &elem_hdr, sizeof(elem_hdr))) != sizeof(elem_hdr)) {
	    DBGFMT("load(): Could not read element header. Wanted [%d] bytes, got [%u].",
		   (int) sizeof(elem_hdr), read_size);
	    mElements.clear();
	    close();
	    return false;
	}
	//
	// Check for zero length value to see if it is time to stop reading.
	//
	if (elem_hdr.mValueLength == -1) 
	    break;

	strncpy(name, elem_hdr.mName, 64); name[64] = 0;
	       
	//
	// Read value part
	//
	elem_hdr.mValueLength = (elem_hdr.mValueLength > 128*1024)?128*1024:elem_hdr.mValueLength;

	if (elem_hdr.mValueLength > 0  && 
	    (read_size =read(mDescriptor, (void *) value, elem_hdr.mValueLength)) != (int) elem_hdr.mValueLength) {
	    DBGFMT("load(): Could not read element value. Wanted [%d] bytes, got [%d].",
		   elem_hdr.mValueLength, read_size);
	    mElements.clear();
	    close();
	    return false;
	}
	value[elem_hdr.mValueLength] = 0; // Null terminate.
	//
	// Install in list.
	//
	switch(elem_hdr.mType) {
	    case StringElement:
		mElements.insert(CElementMap::value_type(name, CElement(value)));
		break;

	    case IntElement:
		mElements.insert(CElementMap::value_type(name, CElement(*((int *) value))));
		break;

	    case FloatElement:
		mElements.insert(CElementMap::value_type(name, CElement(*((float *)value))));
		break;
	}
    }

    close();
    return true;
}

//
// format the database.
//
bool CDatabase::format(void)
{
//    char buf[mBlockSize];
//    int ind = (mBlocks * 2) + 2;
    CHeader header;
    CElementHeader elem_hdr;

    if (!open()) {
	DBGFMT("format(): Could not open [%s] for formatting.", mDevice.c_str());
	return false;
    }
    
    DBGFMT("format(): Will format [%d] blocks with block size [%d]", mBlocks, mBlockSize);
    //
    // First. Wipe area.
    //

// Not needed, I think
//    lseek(mDescriptor,  0, SEEK_SET);
//    memset(buf, 0, mBlockSize);
//    while(ind--)
//	write(mDescriptor, buf, mBlockSize);

    //
    // Write header.
    //
    memcpy(header.mMagic, CDATABASE_MAGIC, 32);
    header.mBlocks = mBlocks;
    header.mBlockSize = mBlockSize;
    lseek(mDescriptor, 0, SEEK_SET);
    write(mDescriptor, (void *) &header, sizeof(header));

    //
    // Write an area code.
    //
    lseek(mDescriptor, mBlockSize, SEEK_SET);
    mCurrentArea = 0; // Next flush will use area 0.
    write(mDescriptor, &mCurrentArea, 0);
    
    //
    // Write a null header at area 0
    //
    lseek(mDescriptor, mBlockSize * 2, SEEK_SET ) ;
    memset((void *) &elem_hdr, 0, sizeof(elem_hdr));
    elem_hdr.mType = StringElement;  
    elem_hdr.mValueLength = -1;
    write(mDescriptor, &elem_hdr, sizeof(elem_hdr));

    //
    // Write a null header at area 1
    //
    lseek(mDescriptor, mBlockSize * 2 + mBlocks*mBlockSize, SEEK_SET );
    lseek(mDescriptor, mBlockSize * 2, SEEK_SET ) ;
    write(mDescriptor, &elem_hdr, sizeof(elem_hdr));
    close();
    return true;
}



//
// flush all entries to database.
//
bool CDatabase::flush(void)
{
    int tot_written = 0;

    if (!mDirty) {
//	printf("CDatabase::flush(): Not dirty. No action\n");
	return true;
    }

    if (!open()) {
	DBGFMT("flush(): Could not open [%s] for writing.", mDevice.c_str());
	return false;
    }

    // Reload header.
    if (!loadHeader()) {
	DBGFMT("flush(): Could not load header [%s] of database. Reformatting", mDevice.c_str());
	format();
	open();
    }
	
    //
    // Toggle the current area.
    //
    mCurrentArea = (mCurrentArea==1)?0:1;

    //
    // Position at right point in file.
    //
    lseek(mDescriptor,mBlockSize * 2 + (mCurrentArea * mBlocks)*mBlockSize, SEEK_SET );

    //
    // Write all elements.
    //
    CElementMapIterator iter = mElements.begin();
    while(iter != mElements.end()) {
	CElementHeader elem_hdr;
	int hdr_sz;

	memset((void *) &elem_hdr, 0, sizeof(elem_hdr));
	hdr_sz = sizeof(elem_hdr);

	// Copy name and type.
	strncpy(elem_hdr.mName, iter->first.c_str(), 64);
	elem_hdr.mType = iter->second.mElementType;

	// Install value
	switch (iter->second.mElementType) {
	    case StringElement:
//		printf("CDatabase::flush(string) [%s]->[%s]\n", iter->first.c_str(), iter->second.mStringValue.c_str());
		elem_hdr.mValueLength = (iter->second.mStringValue.length()>1024*128)?(1024*128):(iter->second.mStringValue.length());
		
		write(mDescriptor, (void *) &elem_hdr, sizeof(elem_hdr));
		if (elem_hdr.mValueLength > 0)
		    write(mDescriptor, (void *) iter->second.mStringValue.c_str(), elem_hdr.mValueLength);
		
		break;

	    case IntElement:
//		printf("CDatabase::flush(int) [%s]->[%d]\n", iter->first.c_str(), iter->second.mIntValue);
		elem_hdr.mValueLength = sizeof(int);
		write(mDescriptor, (void *) &elem_hdr, sizeof(elem_hdr));
		write(mDescriptor, (void *) &(iter->second.mIntValue), elem_hdr.mValueLength);
		break;

	    case FloatElement:
//		printf("CDatabase::flush(float) [%s]->[%f]\n", iter->first.c_str(), iter->second.mFloatValue);
		elem_hdr.mValueLength = sizeof(float);
		write(mDescriptor, (void *) &elem_hdr, sizeof(elem_hdr));
		write(mDescriptor, (void *) &(iter->second.mFloatValue), elem_hdr.mValueLength);
		break;

	    default:
		DBGFMT("CDatabase::flush():Unknown element type [%d]", iter->second.mElementType);
	}

	tot_written +=hdr_sz + elem_hdr.mValueLength;

	
	++iter;
    }
    //
    // Write a terminating null header.
    //
    memset((void *) &elem_hdr, 0, sizeof(elem_hdr));
    elem_hdr.mType = StringElement;  
    elem_hdr.mValueLength = -1;
    write(mDescriptor, &elem_hdr, sizeof(elem_hdr));
    tot_written += sizeof(elem_hdr);

    DBGFMT("flush(): Wrote [%d] bytes to [%s]. Max[%d]", 
	   tot_written, mDevice.c_str(),
	   mBlocks * mBlockSize);
    //
    // Write new area code in order to commit the writing above.
    // 
    lseek(mDescriptor,  mBlockSize, SEEK_SET);    
    write(mDescriptor, &mCurrentArea, 1);
    close();
    mDirty = false;
    return true;
}

bool CDatabase::remove(const string &aName)
{
    CElementMapIterator iter = mElements.find(aName); 

    if (iter != mElements.end()) {
	mElements.erase(iter);
	return true;
    }
    return false;

}

bool CDatabase::set(const string &aName, const string &aValue)
{
    string tmp;
    // Check that we really update the value
    if (get(aName, &tmp) && tmp == aValue)
	return false;

    // Avoid dupe
    remove(aName);

    // Install new entry
    mElements.insert(CElementMap::value_type(aName, CElement(aValue.c_str())));
    mDirty = true;
    return true;
}


bool CDatabase::set(const string &aName, const int aValue)
{
    int tmp;
    // Check that we really update the value
    if (get(aName, &tmp) && tmp == aValue) 
	return false;

    remove(aName);
    mElements.insert(CElementMap::value_type(aName, CElement(aValue)));
    mDirty = true;
    return true;
}

bool CDatabase::set(const string &aName, const float aValue)
{
    float  tmp;
    // Check that we really update the value
    if (get(aName, &tmp) && tmp == aValue)
	return false;

    remove(aName);
    mElements.insert(CElementMap::value_type(aName, CElement(aValue)));
    mDirty = true;
    return true;
}


bool CDatabase::get(const string &aName, string *aValue)
{
    CElementMapIterator iter;

    if (!open())
	return false;

    // Find. 
    iter = mElements.find(aName);

    if (iter == mElements.end() || iter->second.mElementType != StringElement)
	return false;
    
    *aValue = iter->second.mStringValue;
//    printf("CDatabase::get(string) [%s]->[%s]\n", aName.c_str(), aValue->c_str());
    return true;
}



bool CDatabase::get(const string &aName, int *aValue)
{
    CElementMapIterator iter;

    if (!open())
	return false;

    // Find. 
    iter = mElements.find(aName);

    if (iter == mElements.end() || iter->second.mElementType != IntElement)
	return false;

    *aValue = iter->second.mIntValue;
//    printf("CDatabase::get(int) [%s]->[%d]\n", aName.c_str(), *aValue);	   
    return true;
}



bool CDatabase::get(const string &aName, float *aValue)
{
    CElementMapIterator iter;

    if (!open())
	return false;

    // Find. 
    iter = mElements.find(aName);


    if (iter == mElements.end() || iter->second.mElementType != FloatElement)
	return false;

    *aValue = iter->second.mFloatValue;

//    printf("CDatabase::get(float) [%s]->[%f]\n", aName.c_str(), *aValue);	   
    return true;
}



bool CDatabase::open(void)
{
    if (mDescriptor != -1)
	return true;

    if (mDevice == "") {
	DBGFMT("CDatabase::open(): No device specified. No action.");
	return false;
    }

    mDescriptor = ::open(mDevice.c_str(), O_RDWR | O_CREAT, 0666);

    if (mDescriptor == -1) {
	DBGFMT("CDatabase::open(): Could not open [%s]: %s",
	       mDevice.c_str(), strerror(errno));
	return false;
    }


    return true;
}

