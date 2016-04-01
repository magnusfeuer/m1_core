//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include "sampler.hh"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <math.h>
#include <float.h>


XOBJECT_TYPE_BOOTSTRAP(CSampleData);
XOBJECT_TYPE_BOOTSTRAP(CSamplerBase);
XOBJECT_TYPE_BOOTSTRAP(CRTSampler);


CSamplerBase::CSamplerBase(CExecutor* aExec,CBaseType *aType):
    CExecutable(aExec, aType),
    mSampleCount(this),
    mMinValue(this),
    mMaxValue(this),
    mMaxSamples(this),
    mIndex(this),
    mNewValue(this),
    mNewTimeStamp(this)
{
    CArrayType* t  = CArrayType::create(CSampleData::CSampleDataType::singleton(), 0);
    // Create an empty array
    CArray *a = new CArray(aExec, t, sizeof(CSampleData *), 0);

    mMinValue.putValue(aExec,-1.0);
    mMaxValue.putValue(aExec, 1.0);
    mSampleCount.putValue(aExec, 0);
    mIndex.putValue(aExec, 0);
    mNewValue.putValue(aExec,0.0);
    mNewTimeStamp.putValue(aExec, 0);

    eventPut(aExec, XINDEX(CSamplerBase,sampleCount),   &mSampleCount);
    eventPut(aExec, XINDEX(CSamplerBase,maxSamples),    &mMaxSamples);
    eventPut(aExec, XINDEX(CSamplerBase,minValue),      &mMinValue);
    eventPut(aExec, XINDEX(CSamplerBase,maxValue),      &mMaxValue);
    eventPut(aExec, XINDEX(CSamplerBase,index),         &mIndex);
    eventPut(aExec, XINDEX(CSamplerBase,newValue),      &mNewValue);
    eventPut(aExec, XINDEX(CSamplerBase,newTimeStamp),  &mNewTimeStamp);
    
    put(aExec, XINDEX(CSamplerBase,values), UArray(a));
}


CSamplerBase::~CSamplerBase(void) 
{
    m1ReleaseArray(at(XINDEX(CSamplerBase,values)).arr);
}

void CSamplerBase::execute(CExecutor* aExec)
{
    if (mIndex.updated()) {
	if (mIndex.value() < 0) 
	    mIndex.putValue(aExec, 0);

	if (mIndex.value() >= sampleCount())
	    mIndex.putValue(aExec, sampleCount() - 1);
    }

    // Add sample.
    if (mNewValue.updated()) {
	Time ts;
	if (!mNewTimeStamp.updated() || mNewTimeStamp.value() == 0) {
	    TimeStamp aTimeStamp = aExec->cycleTime();
	    ts = m1_TimeStampToTime(aTimeStamp);
	}
	else
	    ts = mNewTimeStamp.value();
	addSample(aExec, mNewValue.value(), ts);
    }
}


void CSamplerBase::addSample(CExecutor* aExec,float aValue, Time aTimeStamp) 
{
    CArray* values = at(XINDEX(CSamplerBase,values)).arr;
    CSampleData *new_sample =  m1New(CSampleData, CSampleData::CSampleDataType::singleton());

    // Size up in chunks of 25%
    if (values->size() == mSampleCount.value()) {
	if (mSampleCount.value() == 0.0) {
	    values->resize(500);
	}
	else {
	    values->resize(int(float(mSampleCount.value())*1.25));
	}
    }

    new_sample->val(aValue);
    new_sample->ts(aTimeStamp);
    values->put(aExec, mSampleCount.value(), UObject(new_sample));
    mSampleCount.putValue(aExec, mSampleCount.value() + 1);
}



CSampleData *CSamplerBase::sample(int aIndex) 
{ 
    CArray* values = at(XINDEX(CSamplerBase,values)).arr;

    if ((aIndex < 0) || (aIndex >= (int) values->size()))
	return 0;
	
    return dynamic_cast<CSampleData *>(values->at(aIndex).o);
}



CRTSampler::CRTSampler(CExecutor* aExec,CBaseType* aType) : 
    CSamplerBase(aExec,aType),
    mEnabled(this),
    mLoop(this),
    mStartTime(this),
    mStopTime(this),
    mActive(this)
{
    mActive.putValue(aExec, false);
    mStartTime.putValue(aExec, 0.0);
    mStopTime.putValue(aExec, 0.0);
    mEnabled.putValue(aExec, true);
    mStartTime.putValue(aExec, 0.0);
    mStopTime.putValue(aExec, 0.0);
    mLoop.putValue(aExec, true);

    mActivationTime   = 0.0;
    mDeactivationTime = 0.0;

    // Inputs
    eventPut(aExec,XINDEX(CRTSampler,enabled),    &mEnabled);
    eventPut(aExec,XINDEX(CRTSampler,startTime),  &mStartTime);
    eventPut(aExec,XINDEX(CRTSampler,stopTime),   &mStopTime);
    eventPut(aExec,XINDEX(CRTSampler,loop),       &mLoop);
    // Outputs
    eventPut(aExec,XINDEX(CRTSampler,active),     &mActive);
}

CRTSampler::~CRTSampler(void)
{
}

void CRTSampler::execute(CExecutor* aExec)
{
    bool enabled =  mEnabled.value();
    TimeStamp tTime = aExec->cycleTime();
    double fTime = m1_TimeStampToSec(tTime);

    if (mEnabled.updated()) {
	if (enabled) {
	    if (mStartTime.value() <= 0.0)
		mActivationTime = fTime; // Activate now
	    else
		mActivationTime = mStartTime.value();
	    
	    if (mStopTime.value() <= 0.0)
		mDeactivationTime = 0.0;
	    else
		mDeactivationTime = mStopTime.value();
	    DBGFMT("CRTSampler: Enabled");
	}
	else {
	    mActive.putValue(aExec, false);
	    DBGFMT("CRTSampler: Disabled");
	}
    }
    else {
	if (mStartTime.updated()) {
	    if (mStartTime.value() <= 0.0)
		mActivationTime = fTime; // Activate now
	    else
		mActivationTime = mStartTime.value();
	}
	if (mStopTime.updated()) {
	    if (mStopTime.value() <= 0.0)
		mDeactivationTime = 0.0;
	    else
		mDeactivationTime = mStopTime.value();
	}
    }


    if (!enabled)
	return;

    if (!mActive.value()) {
	// activate check
	if ((fTime >= mActivationTime) &&
	    ((mDeactivationTime == 0.0) || (fTime < mDeactivationTime)))
	    mActive.putValue(aExec, true);
    }

    // Insert new value
    if (mActive.value() && mNewValue.updated()) {
	if ((mDeactivationTime > 0.0) && (fTime >= mDeactivationTime)) {
	    mDeactivationTime = fTime;
	    mActive.putValue(aExec, false);
	}
	else {
	    // Check if we are to add at end, or update an existing index.
	    if (index() < (int) sampleCount()) {
		sample(index())->val(mNewValue.value());
		sample(index())->ts(m1_TimeStampToTime(tTime));
		nextIndex();
	    }
	    else {
		addSample(aExec, mNewValue.value(), m1_TimeStampToTime(tTime));
		index(sampleCount() - 1);
	    }

	    if (sampleCount() != 0 && index() >= (int) sampleCount()) {
		if (mLoop.value())
		    index(0);
		else {
		    mActive.putValue(aExec, false);
		    mDeactivationTime = fTime;
		}
	    }
	}
    }
}
