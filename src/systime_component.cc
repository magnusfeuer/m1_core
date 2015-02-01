//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include "systime_component.hh"
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#ifndef DARWIN
#include <linux/rtc.h>
#include <sys/ioctl.h>
#endif

XOBJECT_TYPE_BOOTSTRAP(CSysTimeComponent);

CSysTimeComponent::CSysTimeComponent(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mYear(this),
    mMonth(this),
    mDay(this),
    mHour(this),
    mMinute(this),
    mSecond(this),
    mSetTime(this)
{ 
    DBGFMT("CSysTimeComponent::CSysTimeComponent(): Called");
    setFlags(ExecuteEverySweep);

    mLastTime = 0;
    mSetTime.putValue(aExec, false);
    // Inputs
    eventPut(aExec, XINDEX(CSysTimeComponent,year), &mYear);
    eventPut(aExec, XINDEX(CSysTimeComponent,month), &mMonth);
    eventPut(aExec, XINDEX(CSysTimeComponent,day), &mDay);
    eventPut(aExec, XINDEX(CSysTimeComponent,hour), &mHour);
    eventPut(aExec, XINDEX(CSysTimeComponent,minute), &mMinute);
    eventPut(aExec, XINDEX(CSysTimeComponent,second), &mSecond);
    eventPut(aExec, XINDEX(CSysTimeComponent,setTime), &mSetTime);
}

CSysTimeComponent::~CSysTimeComponent(void)
{
    DBGFMT("CSysTimeComponent::CSysTimeComponent(): Called");
}

void CSysTimeComponent::start(CExecutor* aExec)
{
    getTime(aExec);
}

void CSysTimeComponent::getTime(CExecutor* aExec) 
{
    time_t ct = time(0);

    if (ct != mLastTime) {
	struct tm *lt = localtime(&ct);
	mLastTime = ct;

	mYear.putValue(aExec, (unsigned int) lt->tm_year + 1900);
	mMonth.putValue(aExec, (unsigned int) lt->tm_mon + 1);
	mDay.putValue(aExec,   (unsigned int) lt->tm_mday);
	mHour.putValue(aExec,  (unsigned int) lt->tm_hour);
	mMinute.putValue(aExec, (unsigned int) lt->tm_min);
	mSecond.putValue(aExec, (unsigned int) lt->tm_sec);
    }
}

bool CSysTimeComponent::setTime(void) 
{
    struct tm lt;
#ifndef DARWIN
    time_t ot = time(0);
    struct rtc_time htime;
    int rtc_des;
    struct timeval tval;
#endif 

//      printf("CSysTimeComponent::Year: [%d]\n", mYear.self());
//      printf("CSysTimeComponent::Mon:  [%d]\n", mMonth.self());
//      printf("CSysTimeComponent::Day:  [%d]\n", mDay.self());
//      printf("CSysTimeComponent::Hour: [%d]\n", mHour.self());
//      printf("CSysTimeComponent::Min:  [%d]\n", mMinute.self());
//      printf("CSysTimeComponent::Sec:  [%d]\n", mSecond.self());
    lt.tm_year = mYear.value() - 1900;
    lt.tm_mon = mMonth.value() - 1;
    lt.tm_mday = mDay.value();
    lt.tm_wday = -1;
    lt.tm_hour = mHour.value();
    lt.tm_min = mMinute.value();
    lt.tm_sec = mSecond.value();
    lt.tm_isdst = -1;
    lt.tm_yday = -1;

#ifndef DARWIN
    tval.tv_sec = mktime(&lt);;
    tval.tv_usec = 0;

    if (settimeofday(&tval, 0) == -1) {
	ERRFMT("CSysTimeComponent::setTime(): Could not settimeofday: %s",  strerror(errno));
	return false;
    }

    // Adjust start time in m1 system to keep all relative timers happy.
    m1_system().adjTime(ot, tval.tv_sec);

    // Open rtc dev
    if ((rtc_des = open("/dev/rtc", O_RDONLY)) == -1) {
	ERRFMT("CSysTimeComponent::setTime(): Could not open hardware clock device /dev/rtc: %s",  strerror(errno));
	return false;
    }

    //
    // Set HW clock.
    //
    htime.tm_year = lt.tm_year;
    htime.tm_mon = lt.tm_mon;
    htime.tm_mday = lt.tm_mday;
    htime.tm_hour = lt.tm_hour;
    htime.tm_min = lt.tm_min;
    htime.tm_sec = lt.tm_sec;
    htime.tm_wday = -1;
    htime.tm_yday = -1;
    htime.tm_isdst = -1;

    // ioctl in new value
    if (ioctl(rtc_des, RTC_SET_TIME, &htime) == -1) {
	ERRFMT("CSysTimeComponent::setTime(): Could not set hardware clock : %s",  strerror(errno));
	return false;
    }
	
    close(rtc_des);
#endif

    return true;
}

void CSysTimeComponent::execute(CExecutor* aExec)
{

    //
    // Since we don't care about time zones, 
    // we just run localtime and hope for the best.
    //
    if (mSetTime.updated())
	setTime();

    getTime(aExec);
}
