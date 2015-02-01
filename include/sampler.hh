//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __SAMPLER_HH__
#define __SAMPLER_HH__

#include "component.hh"

struct SampleData {
    UData          v;
    Time           t;
};


class CSampleData : public CObject {
public:
    XBASE_TYPE(CSampleData, "SampleData", 
	       "Sample data",
	       (CSampleData_value,
		CSampleData_timeStamp),
	       XFIELD(CSampleData,Q_PUBLIC,value,
		      float_type(),
		      "Float sample value"),
	       XFIELD(CSampleData,Q_PUBLIC,timeStamp,
		      time_type(),
		      "Sample time")
	);
public:
    CSampleData(CBaseType *):mValue(0.0),mTimeStamp(0) { }
    CSampleData(float aValue, Time aTS):mValue(aValue), mTimeStamp(aTS) { }
    ~CSampleData() {}

    CType* type() { return CSampleDataType::singleton(); }
    CType* typeAt(int index) { return float_type(); }
    CType* typeAt(string aName) { return float_type(); }
    
    UData  at(int index) {
	UData r;
	if (index == XINDEX(CSampleData,value)) 
	    r.f = mValue;
	else if (index == XINDEX(CSampleData,timeStamp)) 
	    r.tm = mTimeStamp;
	return r;
    }

    void put(CExecutor* aExec, int index, UData value, Trig_t trig) {
	if (index == XINDEX(CSampleData,value))
	    mValue = value.f;
	else if (index == XINDEX(CSampleData,timeStamp))
	    mTimeStamp = value.tm;
    }

    void copy(CExecutor* aExec, CType* aType, int index, UData value, Trig_t trig) {
	put(aExec,index,value,trig); 
    }
    float val(void) { return mValue; }
    Time ts(void) { return mTimeStamp; }

    void val(float aValue)   { mValue = aValue; }
    void ts(Time aTimeStamp) { mTimeStamp = aTimeStamp; }

private:
    float mValue;
    Time mTimeStamp;
};

//
// Simple container for CSampleData objects.
//
class CSamplerBase : public CExecutable {
public:
    XOBJECT_TYPE(CSamplerBase, "SamplerBase",
		 "Sample collector",
		 (CSamplerBase_values,
		  CSamplerBase_sampleCount,
		  CSamplerBase_newValue,
		  CSamplerBase_newTimeStamp,
		  CSamplerBase_maxSamples,
		  CSamplerBase_minValue,
		  CSamplerBase_maxValue,
		  CSamplerBase_index),
		 XFIELD(CSamplerBase,Q_PUBLIC,values,
			CArrayType::create(CSampleData::CSampleDataType::singleton(), 0),
			"Value array"),
		 XFIELD(CSamplerBase,Q_PUBLIC,sampleCount,
			output_unsigned_type(),
			"???"),
		 XFIELD(CSamplerBase,Q_PUBLIC,newValue,
			input_float_type(),
			"Input sample value."), 
		 XFIELD(CSamplerBase,Q_PUBLIC,newTimeStamp,
			input_time_type(), 
			"Input sample time stamp"), 
		 XFIELD(CSamplerBase,Q_PUBLIC,maxSamples,
			input_signed_type(),
			"Maximum number of samples to hold in the buffer."),
		 XFIELD(CSamplerBase,Q_PUBLIC,minValue,
			input_float_type(),
			"minimum sample value"),
		XFIELD(CSamplerBase,Q_PUBLIC,maxValue,
		       input_float_type(),
		       "maximum sample value"),
		XFIELD(CSamplerBase,Q_PUBLIC,index,
		       event_signed_type(),
		       "Current sample index. Normally not set.")
	);

public:
    CSamplerBase(CExecutor* aExec,
		 CBaseType *aType = CSamplerBase::CSamplerBaseType::singleton());
    ~CSamplerBase(void);

    void execute(CExecutor* aExec);

    void addSample(CExecutor* aExec, float aValue, Time aTimeStamp);

    CSampleData *sample(int aIndex);

    unsigned int sampleCount(void) { return mSampleCount.value(); }
    int maxSamples(void)    { return mMaxSamples.value(); }

    float minValue(void)  { return mMinValue.value(); }
    float maxValue(void)  { return mMaxValue.value(); }

    void minValue(float aNewValue)  { mMinValue.putValue(NULL,aNewValue); }
    void maxValue(float aNewValue)  { mMaxValue.putValue(NULL,aNewValue); }

    int index(void) { return mIndex.value(); }
    void index(int aNewIndex) { mIndex.putValue(NULL,aNewIndex); }
    int nextIndex(void) { 
	if (mIndex.value()==sampleCount())
	    return mIndex.value();
	else
	    return mIndex.putValue(NULL,mIndex.value() + 1);
    }

    int prevIndex(void) { 
	if (!mIndex.value())
	    return 0;
	else 
	    return mIndex.putValue(NULL,mIndex.value() - 1);
    }

protected:
    // Value array.
    EventUnsigned mSampleCount;;

    //! minimum sample value
    EventFloat    mMinValue;

    //! maximum sample value
    EventFloat    mMaxValue;

    //! Maximum number of samples to hold in the buffer.
    EventSigned mMaxSamples;

    //! current sample index. Normally not set 
    EventUnsigned mIndex;

    //! Input sample value.
    EventFloat    mNewValue;

    //! Input sample time stamp
    EventTime    mNewTimeStamp;
};


// Real time sampler
class CRTSampler : public CSamplerBase {
public:
    XDERIVED_OBJECT_TYPE(CRTSampler, CSamplerBase, "RealTimeSampler",
			 "Real time sampler",
			 (CRTSampler_enabled,
			  CRTSampler_loop,
			  CRTSampler_startTime,
			  CRTSampler_stopTime,
			  CRTSampler_active),
			 XFIELD(CRTSampler,Q_PUBLIC,enabled,
				input_bool_type(),
				"Enabled sampling"),
			 XFIELD(CRTSampler,Q_PUBLIC,loop,
				input_bool_type(),
				"If loop is true the sampling will continue by circular overwrite"
				" the buffer. If loop is false the sampling will disable when the"
				" buffer is full."
			     ),
			 XFIELD(CRTSampler,Q_PUBLIC,startTime,
				input_float_type(),
				"startTime is a given in seconds since system start and"
				" can be used to delay start of sampling."
			     ),
			 XFIELD(CRTSampler,Q_PUBLIC,stopTime,
				input_float_type(),
				"stopTime is a given in seconds since system start and"
				" can be used to stop the sampling at a certain time in the future"
			     ),
			 XFIELD(CRTSampler,Q_PUBLIC,active,
				output_bool_type(),
				"Output true when sampling starts and false when"
				" sampling stops."
			     )
	);

public:
    CRTSampler(CExecutor* aExec,CBaseType *aType = CRTSamplerType::singleton());
    ~CRTSampler(void);

    void execute(CExecutor* aExec);

private:
    //! Is sampling enabled?
    EventBool     mEnabled;


    //! If loop is true the sampling will continue by circular overwrite
    //! the buffer. If loop is false the sampling will disable when the
    //! buffer is full.
    EventBool     mLoop;

    //! startTime is a given in seconds since system start and
    //! can be used to delay start of sampling
    EventFloat    mStartTime;
    //! stopTime is a given in seconds since system start and
    //! can be used to stop the sampling at a certain time in the future
    EventFloat    mStopTime;
    //! output true when sampling starts and false when
    //! sampling stops.
    EventBool mActive;

    float        mActivationTime;   // When to start sampling
    float        mDeactivationTime; // When to stop  sampling
};


#endif
