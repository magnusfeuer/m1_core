//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __EVENT_PLAYER_HH__
#define __EVENT_PLAYER_HH__

#include "m1.hh"
#include "input_strategy.hh"

#define REVENT_QUEUE_SIZE 64

class CEventPlayer : public CExecutable {
public:
    XOBJECT_TYPE(CEventPlayer,
		 "EventPlayer",
		 "Read events from file and send them trough input strategy.",
		 (CEventPlayer_fileName,
		  CEventPlayer_inputStrategy,
		  CEventPlayer_enabled,
		  CEventPlayer_loop,
		  CEventPlayer_speed),
		 XFIELD(CEventPlayer,Q_PUBLIC,fileName,
			input_string_type(),
			"The recording file name."),
		 XFIELD(CEventPlayer,Q_PUBLIC,inputStrategy, 
			CEventType::create(CInputStrategyBase::CInputStrategyBaseType::singleton(), E_INPUT),
			"The event receiver."),
		 XFIELD(CEventPlayer,Q_PUBLIC,enabled,
			input_bool_type(),
			"Enables or disables the reply."),
		 XFIELD(CEventPlayer,Q_PUBLIC,loop,
			input_bool_type(),
			"Automatically restart the reply when reaching end."),
		 XFIELD(CEventPlayer,Q_PUBLIC,speed,
			input_float_type(),
			"Scale the time stamps on input events.")
	);
public:
    CEventPlayer(CExecutor* aExec, CBaseType *aType = CEventPlayerType::singleton());
    ~CEventPlayer(void);

    void execute(CExecutor* aExec);

private:
    // Events has the form "<e> <t> <v>"
    typedef struct {
	char e;   // event name = x,y,1,2,3,4,5,r,d,k,v
	Time t;   // relative milliseconds since start of replay
	union {
	    int i;           // (x,y,d,v)
	    unsigned long u;  // (1,2,3,4,5,r,k)
	} v;
    } REvent;

    void replayEvent(REvent* aEvent);
    bool queueEmpty(void) { return mQHead == mQTail; }
    bool queueFull(void) { return ((mQTail+1)%REVENT_QUEUE_SIZE) == mQHead; }

    EventObject<CInputStrategyBase*> mInputStrategy;
    EventString mFileName;
    EventBool mEnabled;
    EventBool mLoop;
    EventFloat mSpeed;

    REvent mEventQueue[REVENT_QUEUE_SIZE];
    int mQHead;
    int mQTail;
    FILE* mFile;
    TimeStamp mStartTime;
    bool  mIsPlaying;
};

#endif

