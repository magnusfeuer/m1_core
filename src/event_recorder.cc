//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include <errno.h>
#include "event_recorder.hh"
#include <string.h>

XOBJECT_TYPE_BOOTSTRAP(CEventRecorder);

CEventRecorder::CEventRecorder(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mFileName(this),
    mEnabled(this),
    mX(this), // Horizontal placement
    mY(this), // Vertical placement.
    mButton1Down(this), 
    mButton2Down(this), 
    mButton3Down(this), 
    mButton4Down(this), 
    mButton5Down(this), 

    mRockerDirection(this),
    mRockerDown(this),
    mKeyDown(this), 
    mKeyValue(this)
{
    mFile = NULL;
    mStartTime = 0;
    mIsRecording = false;

    mX.putValue(aExec,0);
    mY.putValue(aExec,0);
    mButton1Down.putValue(aExec, 0);
    mButton2Down.putValue(aExec, 0);
    mButton3Down.putValue(aExec, 0);
    mButton4Down.putValue(aExec, 0);
    mButton5Down.putValue(aExec, 0);
    mRockerDown.putValue(aExec, 0);
    mRockerDirection.putValue(aExec, 0);
    mKeyValue.putValue(aExec, 0);
    mKeyDown.putValue(aExec, 0);

    eventPut(aExec, XINDEX(CEventRecorder,fileName), &mFileName);
    eventPut(aExec, XINDEX(CEventRecorder,enabled), &mEnabled);

    eventPut(aExec, XINDEX(CEventRecorder,x), &mX);
    eventPut(aExec, XINDEX(CEventRecorder,y), &mY);
    eventPut(aExec, XINDEX(CEventRecorder,button1Down), &mButton1Down); 
    eventPut(aExec, XINDEX(CEventRecorder,button2Down), &mButton2Down); 
    eventPut(aExec, XINDEX(CEventRecorder,button3Down), &mButton3Down); 
    eventPut(aExec, XINDEX(CEventRecorder,button4Down), &mButton4Down); 
    eventPut(aExec, XINDEX(CEventRecorder,button5Down), &mButton5Down); 

    eventPut(aExec, XINDEX(CEventRecorder,rockerDirection), &mRockerDirection); 
    eventPut(aExec, XINDEX(CEventRecorder,rockerDown), &mRockerDown); 

    eventPut(aExec, XINDEX(CEventRecorder,keyDown), &mKeyDown); 
    eventPut(aExec, XINDEX(CEventRecorder,keyValue), &mKeyValue);

}

CEventRecorder::~CEventRecorder()
{
    // close file if we missed did not doit else where
    if (mFile)
	fclose(mFile);
}

void CEventRecorder::execute(CExecutor* aExec)
{
    TimeStamp currentTime;
    Time td;
    static Time lastFlush = 0;
    if (mFileName.updated()) {
	if (mFile)
	    fclose(mFile);
	mFile = NULL;
	if (mFileName.value() != "") {
	    mFile = fopen(mFileName.value().c_str(), "w");
	    if (!mFile)
		fprintf(stderr, "CEventRecorder: could not open file [%s] %s\n",
			mFileName.value().c_str(), strerror(errno));
	}
    }
    if (!mIsRecording && mFile && mEnabled.value()) {
	mIsRecording = true;
	mStartTime = aExec->cycleTime();
    }

    if (mIsRecording && !mEnabled.value()) {
	mIsRecording = false;
	if (mFile)
	    fclose(mFile);
	mFile = NULL;
    }

    if (!mFile || !mIsRecording)
	return;

    currentTime = aExec->cycleTime();
    td = (currentTime - mStartTime) / STAMP_TIME;

    if (mX.updated())
	fprintf(mFile, "x %lu %d\n", td, mX.value());

    if (mY.updated())
	fprintf(mFile, "y %lu %d\n", td, mY.value());

    if (mButton1Down.updated())
	fprintf(mFile, "1 %lu %lu\n", td, mButton1Down.value());

    if (mButton2Down.updated())
	fprintf(mFile, "2 %lu %lu\n", td, mButton2Down.value());

    if (mButton3Down.updated())
	fprintf(mFile, "3 %lu %lu\n", td, mButton3Down.value());

    if (mButton4Down.updated())
	fprintf(mFile, "4 %lu %lu\n", td, mButton4Down.value());

    if (mButton5Down.updated())
	fprintf(mFile, "5 %lu %lu\n", td, mButton5Down.value());

    if (mRockerDown.updated())
	fprintf(mFile, "r %lu %lu\n", td, mRockerDown.value());

    if (mRockerDirection.updated())
	fprintf(mFile, "d %lu %d\n", td, mRockerDirection.value());

    if (mKeyDown.updated())
	fprintf(mFile, "k %lu %lu\n", td, mKeyDown.value());

    if (mKeyValue.updated())
	fprintf(mFile, "v %lu %d\n", td, mKeyValue.value());

    if (td - lastFlush > 1000) {
	fflush(mFile);
	lastFlush = td;
    }
}
    
