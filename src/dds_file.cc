//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

//
//  Bitmap storage management functionality.
//
#include "dds_component.hh"
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>

CDDSFileList CDDSFileFactory::mStorage;

CDDSFile::CDDSFile(const char *aFileName):
    mError(FBK),
    mOffsetArray(0),
    mPixmaps(0),
    mMappedSize(0)
{
    off_t current, tmp_current;
    struct stat cache_stat;
    int in_des;

    mFileName = strdup(aFileName);
    mHeader.mVersion = 0;
    mHeader.mImageCount = 0;
    mHeader.mHeight = 0;
    mHeader.mWidth = 0;

    in_des = open(aFileName, O_RDONLY);
    if (in_des == -1) {
	printf("Could not open file [%s]: %s\n", aFileName, strerror(errno));
	mError = FB_CouldNotOpenFile;
	return;
    }

    //
    // Read the header.
    //
    if (read(in_des, (char *) &mHeader, sizeof(mHeader)) != sizeof(mHeader)) {
	printf("CDDSFile(): Could not read DDS header of file [%s]: Wrong size.\n", aFileName);
	close(in_des);
	mError = FBllegalFileFormat;
	return;
    }

    //
    // Validate height.
    //
    if (mHeader.mHeight < 1 || mHeader.mHeight > 2048) {
	printf("Could not read DDS header of file [%s]: Height out of range: [%d] (Allowed: 1-2048).\n", 
	       aFileName, mHeader.mHeight);
	close(in_des);
	mError = FBllegalFileFormat;
	return;
    }

    if (mHeader.mWidth < 1 || mHeader.mWidth > 4096) {
	printf("Could not read DDS header of file [%s]: Width out of range: [%d] (Allowed: 1-4096).\n", 
	       aFileName, mHeader.mWidth);
	close(in_des);
	mError = FBllegalFileFormat;
	return;
    }

    if (mHeader.mImageCount < 1 || 
	mHeader.mImageCount > 10000) {
	printf("Could not read DDS header of file [%s]: Image count is out of range: [%d] (Allowed: 1-10000).\n", 
	       aFileName, mHeader.mImageCount);
	close(in_des);
	mError = FBllegalFileFormat;
	return;
    }
    tmp_current = current = lseek(in_des, 0, SEEK_CUR);

    //   printf("After reading %d bytes for header, now standing at %d\n",
    // 	 sizeof(mHeader),  current);

	
    //
    // Read the offset table.
    //
    mOffsetArray = new int[mHeader.mImageCount];

    if (read(in_des, (char *) mOffsetArray, sizeof(int) * mHeader.mImageCount) != 
	(int) sizeof(int) * (int) mHeader.mImageCount) {
	printf("Could not read DDS header of file [%s]: Wrong offset table size.\n", aFileName);
	close(in_des);
	delete[] mOffsetArray;
	mError = FBllegalFileFormat;
	return;
    }
	
    //
    // We now stand at the first byte of the image data. 
    // Figure out the size of the file
    //
    current = lseek(in_des, 0, SEEK_CUR);
    fstat(in_des, &cache_stat);

//     printf("After reading %d entries, %d ( == %ld) bytes, for offset table now standing at %lu. Total size = %lu\n",
// 	   mHeader.mImageCount,
// 	   sizeof(int) * mHeader.mImageCount, 
// 	   current - tmp_current, 
// 	   current,
// 	   cache_stat.st_size);

//     //
//     // Map the rest of the file
//     //
//     printf("Mapping [%ld] bytes of pixel data from [%s]. H[%d] W[%d] C[%d]\n", 
// 	   cache_stat.st_size - current,
// 	   aFileName,
// 	   mHeader.mHeight,
// 	   mHeader.mWidth,
// 	   mHeader.mImageCount
// 	);
    
    mPixmaps = (unsigned char *) mmap(0,
				      cache_stat.st_size,
				      PROT_READ, MAP_SHARED, 
				      in_des, 
				      0);
    mMappedSize = cache_stat.st_size;
    close(in_des);

    if (mPixmaps == (unsigned char *) 0xFFFFFFFF) {
	printf("Could not map images to process address space: %s\n", strerror(errno));
	delete[] mOffsetArray;
	mOffsetArray = 0;
	mPixmaps = 0;
	mError = FB_GenericError;
	return;
    }

    //
    // We are happy!
    //
    return;
}

enum CDDSFile::EErrorCode CDDSFile::error(void) 
{
    return mError;
}

// int CDDSFile::IncreaseReferenceCount(int aCount)
// {  
//     mReferenceCount += aCount;
//     return mReferenceCount;
// }

// int CDDSFile::DecreaseReferenceCount(int aCount)
// {  
//     mReferenceCount -= aCount;
//     return mReferenceCount;
// }

const char *CDDSFile::fileName(void) 
{
    return mFileName;
}

int CDDSFile::frameCount(void) 
{
    return mHeader.mImageCount;
}

int CDDSFile::height(void) 
{
    return mHeader.mHeight;
}

int CDDSFile::width(void) 
{
    return mHeader.mWidth;
}

int CDDSFile::colorDepth(void) 
{
    return EPIXEL_SIZE(mHeader.mPixelType);
}

int CDDSFile::pixelType(void)
{
    return mHeader.mPixelType;
}

unsigned char *CDDSFile::pixmap(int aIndex) 
{
    unsigned char* base;
    unsigned char* ptr0;
    unsigned char* ptr1;
    int n = frameCount();

    if ((aIndex < 0) || (aIndex >= n)) {
//     printf("WARNING: Somebody tried to access [%s] win an index [%d] outside range [0-%d]\n",
// 	   FileName().c_str(), aIndex, ImageCount());
	aIndex = 0;
    }

    base = mPixmaps + sizeof(mHeader)+sizeof(int)*mHeader.mImageCount;

    ptr0 = base + mOffsetArray[aIndex];
    if (aIndex+1 == n)
	ptr1 = mPixmaps + mMappedSize;  /* point after end of file */
    else
	ptr1 = base + mOffsetArray[aIndex+1];

    return ptr0;
}



CDDSFile::~CDDSFile(void)
{
    if (mPixmaps) {
	if (munmap(mPixmaps,mMappedSize) == -1) 
	    printf("CDDSFile::~CDDSFile(): Could not unmap pixmap from memory: %s\n", strerror(errno));

	delete[] mOffsetArray;
    }	  
}

CDDSFile *CDDSFileFactory::load(const char *aFileName)
{
    CDDSFileListIterator iter = mStorage.begin();
    CDDSFile *new_map = 0;
    //
    // First check if we have it already!
    //
    while(iter != mStorage.end()) {
	if (!strcmp((*iter)->fileName(), aFileName)) {
//	    (*iter)->IncreaseReferenceCount();
// 	    printf("CDDSFileFactory::Load(): Found file [%s] in cache.\n",
// 	      aFileName);

	    return *iter;
	}  
	++iter;
    }
  
    //
    // Not in cache. Load it
    //
//    printf("CDDSFileFactory::Load(): Loading file [%s] into cache\n",
// 	   aFileName);

    new_map = new CDDSFile(aFileName);
  
    //
    // Check that we did not run into an error
    //
    if (new_map->error() != CDDSFile::FBK) {
	printf("CDDSFileFactory::Load(): Failed to load file [%s] into cache\n",
	       aFileName);
	delete new_map;
	return 0;
    }

//    new_map->IncreaseReferenceCount();
    mStorage.push_back(new_map);
    return new_map;
}

bool CDDSFileFactory::unload(CDDSFile *aMap)
 {
     CDDSFileListIterator iter = mStorage.begin();

     //
     // Locate the map to unload in memory.
     //
     while(iter != mStorage.end()) {
 	if (*iter == aMap) {
 	    mStorage.erase(iter);
	    delete aMap;
	    return true;
 	}
 	++iter;
     }
    return false;
}
