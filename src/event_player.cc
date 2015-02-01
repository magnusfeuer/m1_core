//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include <errno.h>
#include "event_player.hh"

XOBJECT_TYPE_BOOTSTRAP(CEventPlayer);

#define MAX_PLAY_EVENTS 32  //  Never play more than 32 event / execute

CEventPlayer::CEventPlayer(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mInputStrategy(this),
    mFileName(this),
    mEnabled(this),
    mLoop(this),
    mSpeed(this)
{
    setFlags(ExecuteEverySweep);  // FIXME - make this updateable!?
    
    mInputStrategy.putValue(aExec, 0);
    mFile      = NULL;
    mIsPlaying = false;
    mStartTime = 0;
    mQHead = mQTail = 0;

    mFileName.putValue(aExec, "");
    mEnabled.putValue(aExec, false);
    mLoop.putValue(aExec, false);
    mSpeed.putValue(aExec, 1.0);

    eventPut(aExec, XINDEX(CEventPlayer,inputStrategy), &mInputStrategy);
    eventPut(aExec, XINDEX(CEventPlayer,fileName), &mFileName);
    eventPut(aExec, XINDEX(CEventPlayer,enabled), &mEnabled);
    eventPut(aExec, XINDEX(CEventPlayer,loop), &mLoop);
    eventPut(aExec, XINDEX(CEventPlayer,speed), &mSpeed);
}

CEventPlayer::~CEventPlayer()
{
    if (mFile)
	fclose(mFile);
}

void CEventPlayer::replayEvent(REvent* aEvent)
{
    if (!mInputStrategy.value())
	return;
    // should we test if inputStrategy is recording ? or may it be a feature
    switch(aEvent->e) {
    case 'x':
	mInputStrategy.value()->x(aEvent->v.i);
	break;
    case 'y':
	mInputStrategy.value()->y(aEvent->v.i);
	break;
    case 'd':
	mInputStrategy.value()->rockerDirection(aEvent->v.i);
	break;
    case 'v':
	mInputStrategy.value()->keyValue(aEvent->v.i);
	break;
    case '1':
	mInputStrategy.value()->button1Down(aEvent->v.u);
	break;
    case '2':
	mInputStrategy.value()->button2Down(aEvent->v.u);
	break;
    case '3':
	mInputStrategy.value()->button3Down(aEvent->v.u);
	break;
    case '4':
	mInputStrategy.value()->button4Down(aEvent->v.u);
	break;
    case '5':
	mInputStrategy.value()->button5Down(aEvent->v.u);
	break;
    case 'r':
	mInputStrategy.value()->rockerDown(aEvent->v.u);
	break;
    case 'k':
	mInputStrategy.value()->keyDown(aEvent->v.u);
	break;
    }
}

void CEventPlayer::execute(CExecutor* aExec)
{
    char mBuffer[1024];
    TimeStamp currentTime;
    Time td;
    int n = MAX_PLAY_EVENTS;

    if (mFileName.updated()) {
	if (mFile)
	    fclose(mFile);
	mFile = NULL;
	if (mFileName.value() != "") {
	    mFile = fopen(mFileName.value().c_str(), "r");
	    if (!mFile)
		fprintf(stderr, "CEventPlayer: could not open file [%s] %s\n",
			mFileName.value().c_str(), strerror(errno));
	}
    }
    if (!mIsPlaying && mFile && mEnabled.value()) {
	mIsPlaying = true;
	mStartTime = aExec->cycleTime();
    }
    if (mIsPlaying && !mEnabled.value()) {
	mIsPlaying = false;
	if (mFile)
	    fclose(mFile);
	mFile = NULL;
    }
    if (!mFile || !mIsPlaying)
	return;
    currentTime = aExec->cycleTime();
    td = (currentTime - mStartTime) / STAMP_TIME;

replay:
    while (mQHead != mQTail) {
	if (mEventQueue[mQHead].t > td)
	    return; // wait for time to catch up
	replayEvent(&mEventQueue[mQHead]);
	mQHead =(mQHead+1) % REVENT_QUEUE_SIZE;
	n--;
	if (n<=0)
	    return;  // should send too many events per execute
    }
    if (queueFull())
	return;      // we still have plenty events to send
    
    if (feof(mFile)) {
	if (mLoop.value()) {
	    rewind(mFile);
	    mStartTime = aExec->cycleTime();
	}
	else {
	    mIsPlaying = false;
	    if (mFile)
		fclose(mFile);
	    mFile = NULL;
	    return;
	}
    }

    while(fgets(mBuffer, sizeof(mBuffer), mFile)) {
	switch(mBuffer[0]) {
	case 'x':
	case 'y':
	case 'd':
	case 'v':
	    sscanf(mBuffer+1, "%lu %d", 
		   &mEventQueue[mQTail].t, &mEventQueue[mQTail].v.i);
	    break;
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case 'r':
	case 'k':
	    sscanf(mBuffer+1, "%lu %lu", 
		   &mEventQueue[mQTail].t, &mEventQueue[mQTail].v.u);
	    break;
	default:
	    fprintf(stderr, "CEventPlayer: bad event name '%c'\n", 
		    mBuffer[0]);
	    continue;
	}
	mEventQueue[mQTail].e = mBuffer[0];
	mQTail = (mQTail + 1) % REVENT_QUEUE_SIZE;
	if (queueFull())
	    break;
    }
    if (!queueEmpty())
	goto replay;
}
