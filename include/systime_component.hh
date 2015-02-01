//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007
//

#ifndef __SYSTIME_COMPONENT_H__
#define __SYSTIME_COMPONENT_H__

#include "component.hh"
#include "time.h"

class CSysTimeComponent: public CExecutable {
public:
    XOBJECT_TYPE(CSysTimeComponent, "SysTime",
		 "System time",
		 (CSysTimeComponent_year,
		  CSysTimeComponent_month,
		  CSysTimeComponent_day,
		  CSysTimeComponent_hour,
		  CSysTimeComponent_minute,
		  CSysTimeComponent_second,
		  CSysTimeComponent_setTime),
		 XFIELD(CSysTimeComponent,Q_PUBLIC,year,
			event_unsigned_type(),
			"1970-2036"),
		 XFIELD(CSysTimeComponent,Q_PUBLIC,month,
			event_unsigned_type(),
			"1-12"),
		XFIELD(CSysTimeComponent,Q_PUBLIC,day,
		       event_unsigned_type(),
		       "1-31"),
		XFIELD(CSysTimeComponent,Q_PUBLIC,hour,
		       event_unsigned_type(),
		       "0-23"),
		XFIELD(CSysTimeComponent,Q_PUBLIC,minute,
		       event_unsigned_type(),
		       "0-59"),
		XFIELD(CSysTimeComponent,Q_PUBLIC,second,
		       event_unsigned_type(),
		       "0-59"),
		XFIELD(CSysTimeComponent,Q_PUBLIC,setTime,
		       input_bool_type(),
		       "Set the system time")
	);
public:
    CSysTimeComponent(CExecutor* aExec,
		      CBaseType *aType = CSysTimeComponentType::singleton());
    ~CSysTimeComponent(void);

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);
private:
    bool setTime(void);
    void getTime(CExecutor* aExec);
    EventUnsigned mYear;
    EventUnsigned mMonth;
    EventUnsigned mDay;
    EventUnsigned mHour;
    EventUnsigned mMinute;
    EventUnsigned mSecond;
    EventBool mSetTime;
    time_t mLastTime;
};


#endif 
