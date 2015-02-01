//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//


#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>

#include "time_sensor.hh"

XOBJECT_TYPE_BOOTSTRAP(CTimer);
XOBJECT_TYPE_BOOTSTRAP(CTimeout);

//
// CTimer
//
CTimer::CTimer(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec,aType),
    mCycleInterval(this),
    mEnabled(this),
    mActive(this),
    mLoop(this),
    mAutoDisconnect(this),
    mStartTime(this),
    mStopTime(this),
    mCycleTime(this),
    mTick(this),
    mFraction(this),
    mValue(this)
{ 
    setFlags(ExecuteEverySweep);
    // Default values
    mActive.putValue(aExec, true);
    mAutoDisconnect.putValue(aExec, false);
    mLoop.putValue(aExec, false);
    mStartTime.putValue(aExec, 0.0);
    mStopTime.putValue(aExec, 0.0);
    mCycleInterval.putValue(aExec, 1.0);
    mActivationTime   = 0.0;
    mDeactivationTime = 0.0;
    // Inputs
    eventPut(aExec,XINDEX(CTimer, enabled), &mEnabled);
    eventPut(aExec,XINDEX(CTimer, loop), &mLoop);
    eventPut(aExec,XINDEX(CTimer, startTime), &mStartTime);
    eventPut(aExec,XINDEX(CTimer, stopTime), &mStopTime);
    eventPut(aExec,XINDEX(CTimer, cycleInterval), &mCycleInterval);
    eventPut(aExec,XINDEX(CTimer, autoDisconnect), &mAutoDisconnect);
    // Outputs
    eventPut(aExec,XINDEX(CTimer, active),    &mActive);
    eventPut(aExec,XINDEX(CTimer, cycleTime), &mCycleTime);
    eventPut(aExec,XINDEX(CTimer, tick),      &mTick);
    eventPut(aExec,XINDEX(CTimer, fraction),  &mFraction);
    eventPut(aExec,XINDEX(CTimer, value),  &mValue);

    // Make timer start (setting updated!)
    mEnabled.putValue(aExec, true);
}

CTimer::~CTimer(void)
{

}

void CTimer::execute(CExecutor* aExec)
{
    TimeStamp tTime = aExec->cycleTime();
    double fTime = m1_TimeStampToSec(tTime);
    bool enabled = mEnabled.value();
    Time tick    = m1_TimeStampToTime(tTime);

    if (mEnabled.updated()) {
	if (enabled) {
	    if (mStartTime.value() <= 0.0)
		mActivationTime = fTime; // Activate now
	    else
		mActivationTime = mStartTime.value();
	    
	    if (mStopTime.value() <= 0.0)
		mDeactivationTime = mActivationTime + mCycleInterval.value();
	    else
		mDeactivationTime = mStopTime.value();
	    DBGFMT("Timer: Enabled");
	}
	else {
	    mActive.putValue(aExec, false);
	    DBGFMT("Timer: Disabled");
	}
    }
    else {
	// Some cases for live update on timers
	if (mStartTime.updated()) {
	    if (mStartTime.value() <= 0.0)
		mActivationTime = fTime; // Activate now
	    else
		mActivationTime = mStartTime.value();
	}
	if (mCycleInterval.updated() || mStopTime.updated()) {
	    if (mStopTime.value() <= 0.0)
		mDeactivationTime = mActivationTime + mCycleInterval.value();
	    else
		mDeactivationTime = mStopTime.value();
	}
    }


    if (!enabled)
	return;

    if (!mActive.value()) {
	// activate check
	if ((fTime >= mActivationTime) && (fTime < mDeactivationTime))
	    mActive.putValue(aExec, true);
	else if (mDisconnect.value()) 
	    disconnectOutput(aExec);
    }

    if (mActive.value()) {
	if (fTime >= mDeactivationTime) {
	    mTick.putValue(aExec, tick);             // Generate time tick
	    mCycleTime.putValue(aExec,tick);        // Emit cycle done event
	    mValue.putValue(aExec,fTime);
	    if (mCycleInterval.value() > 0.0)
		mFraction.putValue(aExec, 1.0);
	    if (mLoop.value() && 
		((mStopTime.value() <= 0.0) || (fTime < mStopTime.value()))) {
		mActivationTime = fTime;
		mDeactivationTime = mActivationTime + mCycleInterval.value();
	    }
	    else {
		mActive.putValue(aExec, false);   // Not ticking anymore
		if (mAutoDisconnect.value())
		    mDisconnect.assign(aExec, UTrue(), false, TRIGGER_YES);
	    }
	}
	else {
	    mTick.putValue(aExec,tick);               // Generate time tick
	    mValue.putValue(aExec,fTime);
	    if (mCycleInterval.value() > 0.0) {
		mFraction.putValue(aExec,
				   (fTime-mActivationTime) / 
				   mCycleInterval.value());
	    }
	}
    }
}

//
// CTimeout
//

CTimeout::CTimeout(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec,aType),
    mDuration(this),
    mEnabled(this),
    mReset(this),
    mAutoDisconnect(this),
    mActive(this),
    mTimeout(this),
    mRemain(this)
{ 
    setFlags(ExecuteEverySweep);

    // Default values
    mActive.putValue(aExec, false);
    mAutoDisconnect.putValue(aExec, false);
    mDuration.putValue(aExec, 0.0);
    mRemain.putValue(aExec, 0.0);
    mReset.putValue(aExec, false);

    mActivationTime   = 0.0;
    mTimeoutTime      = 0.0;

    // Inputs
    eventPut(aExec,XINDEX(CTimeout, duration),       &mDuration);
    eventPut(aExec,XINDEX(CTimeout, enabled),        &mEnabled);
    eventPut(aExec,XINDEX(CTimeout, reset),          &mReset);
    eventPut(aExec,XINDEX(CTimeout, autoDisconnect), &mAutoDisconnect);
    // Outputs
    eventPut(aExec,XINDEX(CTimeout, active),    &mActive);
    eventPut(aExec,XINDEX(CTimeout, timeout),   &mTimeout);
    eventPut(aExec,XINDEX(CTimeout, remain),    &mRemain);

    // Make timer start (setting updated!)
    mEnabled.putValue(aExec, true);
}

CTimeout::~CTimeout(void)
{
    DBGFMT_CLS("CTimeout::~CTimeout(): Called");
}

void CTimeout::execute(CExecutor* aExec)
{
    TimeStamp tTime = aExec->cycleTime();
    double fTime = m1_TimeStampToSec(tTime);
    bool enabled = mEnabled.value();

    if (mReset.updated() && mReset.value() == true) {
	mReset.putValue(aExec, false);
	mRemain.putValue(aExec, mDuration.value());
	mTimeoutTime = fTime + mDuration.value();
	mActivationTime = fTime;
	DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Reset updated. Will reset. Enabled[%c] Timeout in [%f] seconds TimeoutTime=%f",
		   fTime,
		   this, aExec->cycle(),
		   (enabled?'Y':'N'), 
		   mDuration.value(),
		   mTimeoutTime);
    }

    if (mEnabled.updated()) {
	if (enabled) {
	    mActivationTime = fTime;         // Activate now
	    mTimeoutTime    = mActivationTime;
	    if (mRemain.value() > 0.0) // re-enable old timer
		mTimeoutTime += mRemain.value();
	    else if (mDuration.value() > 0.0) {
		mRemain.putValue(aExec, mDuration.value());
		mTimeoutTime += mDuration.value();
	    }
	    mActive.putValue(aExec, true);
	    DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Now enabled, TimeoutTime=%f", 
		       fTime,
		       this, aExec->cycle(),
		       mTimeoutTime);
	}
	else {
	    DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Now disabled!",
		       fTime,
		       this, aExec->cycle());
	    mActive.putValue(aExec, false);
	}
    }
    else if (mDuration.updated()) {
	mTimeoutTime = mActivationTime;
	if (mDuration.value() > 0.0)
	    mTimeoutTime += mDuration.value();
	DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] duration updated=%f, TimeoutTime=%f", 
		   fTime,
		   this, aExec->cycle(),
		   mDuration.value(),
		   mTimeoutTime);
    }



    if (!enabled) {
	return;
    }

    if (!mActive.value()) {
	DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Not active!", 
		   fTime,
		   this,aExec->cycle());
	// activate check
	if ((fTime >= mActivationTime) && (fTime <= mTimeoutTime))
	    mActive.putValue(aExec, true);
	else if (mDisconnect.value())
	    disconnectOutput(aExec);
    }

    if (mActive.value()) {
	if (fTime >= mTimeoutTime) {
	    DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Timeout on duration[%f]!", 
		       fTime,
		       this,aExec->cycle(),
		       mDuration.value());
	    mTimeout.putValue(aExec, true);   // generate timeout
	    mRemain.putValue(aExec, 0.0);
	    mActive.putValue(aExec, false);
	    if (mAutoDisconnect.value()) {
		DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Disconnect!",
		       fTime,
			   this,aExec->cycle());
		mDisconnect.assign(aExec, UTrue(), false, TRIGGER_YES);
	    }
	}
	else {
	    mRemain.putValue(aExec, mTimeoutTime - fTime); // remaining time
	    DBGFMT_CLS("CTimeout::execute(%f): [%p,cycle=%ld] Remain[%f]", 
		       fTime,
		       this, aExec->cycle(),
		       mRemain.value());
	}
    }
}


