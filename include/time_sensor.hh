//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
//

//
// Time sensor. 
//

#ifndef __TIME_SENSOR_H__
#define __TIME_SENSOR_H__

#include "m1.hh"

class CTimer: public CExecutable {
public:
    XOBJECT_TYPE(CTimer, "Timer",
		 "Time tick generator",
		 (CTimer_cycleInterval,
		  CTimer_enabled,
		  CTimer_loop,
		  CTimer_startTime,
		  CTimer_stopTime,
		  CTimer_autoDisconnect,
		  CTimer_active,
		  CTimer_cycleTime,
		  CTimer_tick,
		  CTimer_value,
		  CTimer_fraction),
		 XFIELD(CTimer,Q_PUBLIC,cycleInterval,
			input_float_type(),
			"The number of seconds until the timer cycle is done."),
		 XFIELD(CTimer,Q_PUBLIC,enabled,
			input_bool_type(),
		       "Timer status is enabled by default."),
		 XFIELD(CTimer,Q_PUBLIC,loop,
			input_bool_type(),
		       "If loop is true then a new cylce will be started "
			"after cycleInterval seconds."),
		 XFIELD(CTimer,Q_PUBLIC, startTime,
			input_float_type(),
		       "Optional start time for the timer. The time "
		       "is given as seconds from system start time."),
		 XFIELD(CTimer,Q_PUBLIC,stopTime,
			input_float_type(),
		       "Optional stop time for the timer. The time "
		       "is given as seconds from system start time."),
		 XFIELD(CTimer,Q_PUBLIC,autoDisconnect,
			input_bool_type(),
		       "When autoDisconnec is true, default is false, "
		       "the connected output fields are disconnected when "
		       "the timer stops."),
		 XFIELD(CTimer,Q_PUBLIC, active,
			output_bool_type(),
		       "active is set to true when the the time starts."),
		 XFIELD(CTimer,Q_PUBLIC,cycleTime, 
			output_time_type(),
		       "cycleTime is set to the current time once for "
		       "each completed interval."),
		 XFIELD(CTimer,Q_PUBLIC, tick,
			output_time_type(),
		       "tick is set to inow() for each system cycle."),
		 XFIELD(CTimer,Q_PUBLIC, value,
			output_float_type(),
		       "Value is number of seconds since system start, as "
		       "a floating point number."),
		 XFIELD(CTimer,Q_PUBLIC, fraction, 
			output_float_type(),
		       "The fraction is a scaled value and will go from "
		       "0.0 to 1.0 during the cycleInterval.")
	);
public:
    CTimer(CExecutor* aExec, CBaseType *aType = CTimerType::singleton());
    ~CTimer(void);

    void execute(CExecutor* aExec);
    
private:
    //! cycleInterval is used in loop mode and is given as seconds
    EventFloat mCycleInterval;
    //! enabled tells the time sensor to run
    EventBool mEnabled;
    //! output true when ticks starting to generate and false when
    //! ticks stop coming.
    EventBool mActive;
    //! loop is used to restart time sensor after mCycleInterval
    EventBool mLoop;
    //! if autoDisconnect is true then time sensor will disconect
    //! all output connection when mCycleInterval is done and
    //! output has been generated. This applieds only if loop=false
    EventBool mAutoDisconnect;
    //! startTime is a given in seconds since system start and
    //! can be used to delay the time start
    EventFloat mStartTime;
    //! stopTime is a given in seconds since system start and
    //! can be used to stop the timer at a certain time in the future
    EventFloat mStopTime;
    //! cycleTime is set to the current time every completed cycleInterval
    EventTime mCycleTime;
    //! time is set to the current when timer is active
    EventTime mTick;
    //! time is set to the fraction of the cycleInterval completed
    EventFloat mFraction;
    //! value is the current seconds
    EventFloat mValue;

    float        mActivationTime;   // When to start generating ticks
    float        mDeactivationTime; // When to stop  generating ticks
};

//
// Timeout handling
//

class CTimeout : public CExecutable {
public:
    XOBJECT_TYPE(CTimeout, "Timeout",
		 "Timeout timer",
		 (CTimeout_duration,
		  CTimeout_enabled,
		  CTimeout_reset,
		  CTimeout_autoDisconnect,
		  CTimeout_active,
		  CTimeout_timeout,
		  CTimeout_remain),
		 XFIELD(CTimeout,Q_PUBLIC,duration,
			input_float_type(),
			"Timeout time, in seconds."),
		 XFIELD(CTimeout,Q_PUBLIC,enabled,
			input_bool_type(),
			"Timeout status is enabled by default"),
		 XFIELD(CTimeout,Q_PUBLIC,reset,
			input_bool_type(),
			"Reset active or inactive timer"),
		 XFIELD(CTimeout,Q_PUBLIC,autoDisconnect, 
			input_bool_type(),
			"When autoDisconnec is true, default is false, "
			"the connected output fields are disconnected when "
			"reaching timeout."),
		 XFIELD(CTimeout,Q_PUBLIC,active,
			output_bool_type(),
			"active is set to true when the the timeout starts."),
		 XFIELD(CTimeout,Q_PUBLIC,timeout,
			output_bool_type(),
			"The timeout value has been reached."),
		 XFIELD(CTimeout,Q_PUBLIC,remain,
			output_float_type(),
			"Value is number of seconds remain until timeout.")
	);
public:
    CTimeout(CExecutor* aExec, CBaseType *aType = CTimeoutType::singleton());
    ~CTimeout(void);

    void execute(CExecutor* aExec);
    // Too lazy to extract it through symbol.
    float remain(void) { return mRemain.value(); }
    
private:
    EventFloat mDuration;
    EventBool mEnabled;
    EventBool mReset;
    EventBool mAutoDisconnect;
    EventBool mActive;
    EventBool mTimeout;
    EventFloat mRemain;

    float        mActivationTime;   // When count down started
    float        mTimeoutTime;      // When count down shoudl stop
};


#endif 
