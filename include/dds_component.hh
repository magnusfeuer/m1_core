//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2007.
//

#ifndef __DDS_COMPONENT_H__
#define __DDS_COMPONENT_H__

#include "component.hh"
#include <sys/types.h>
#include <list>

#include "epx.h"
#include "dds_common.h"


// CDDSFile
//  Contains a single bitmap seqeuence, as uploaded from a DDS file.
//  Ref counter stripped for now
//
class CDDSFile {
public:
    enum EErrorCode { FBK=0, FB_CouldNotOpenFile = 1, FBllegalFileFormat = 2, FB_GenericError =3};
    CDDSFile(const char *aFileName);
    ~CDDSFile(void);
    enum EErrorCode error(void);
    const char *fileName(void);
    int frameCount(void);
    int height(void);
    int width(void);
    int colorDepth(void); // In bytes.
    int pixelType(void);  // Target pixel type

    unsigned char *pixmap(int aIndex);
private:  // Size of this array will be mInstrumentStat.mImageCount.
    CDDSHeader mHeader;
    char* mFileName;
    enum EErrorCode  mError;
    int *mOffsetArray; 
    unsigned char *mPixmaps;
    int mMappedSize; // Number of bytes mapped in mPixmaps;
};

typedef std::list<CDDSFile *> CDDSFileList;
typedef std::list<CDDSFile *>::iterator CDDSFileListIterator;

// CDDSFileFactory
//  A simple singleton keeping track of all loaded bitmaps
//  used by CFBInstrument instances.
//
class CDDSFileFactory {
public:
    static CDDSFile *load(const char *aFileName);
    static bool unload(CDDSFile *aBitmap);

private:
    static CDDSFileList mStorage;
};

class CDDSComponent;


//
// A DDS file component.
//
class CDDSComponent: public CLayerComponent {
public:
    XDERIVED_OBJECT_TYPE(CDDSComponent,
			 CLayerComponent, 
			 "DDS", 
			 "Animation component",
			 (CDDSComponent_ddsFile,
			  CDDSComponent_value,
			  CDDSComponent_frameStart,
			  CDDSComponent_frameStop),
			 XFIELD(CDDSComponent,Q_PUBLIC,ddsFile, 
				event_string_type(),
				""),
			 XFIELD(CDDSComponent,Q_PUBLIC,value,
				event_float_type(),
				""),
			 XFIELD(CDDSComponent,Q_PUBLIC,frameStart,
				event_float_type(),
				""),
			 XFIELD(CDDSComponent,Q_PUBLIC,frameStop,
				event_float_type(),
				"")
	);

public:
    CDDSComponent(CExecutor* aExec,
		  CBaseType *aType = CDDSComponentType::singleton());
    ~CDDSComponent(void);

    void redraw(CSystem* aSys, CRedrawContext *aContext);

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);

protected:
    void loadDDS(CExecutor* aExec, bool aStart);
    CDDSFile *mDDS;
    EventString mDDSFile;
    int mCurrentFrame;
    EventFloat mValue;
    EventFloat mFrameStart;
    EventFloat mFrameStop;
};


#endif // __DDS_COMPONENT_H__
