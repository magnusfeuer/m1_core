//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __EVENT_RECORDER_HH__
#define __EVENT_RECORDER_HH__

#include "m1.hh"

class CEventRecorder : public CExecutable {
public:
    XOBJECT_TYPE(CEventRecorder,
		 "EventRecorder",
		 "Input event recorder",
		 (CEventRecorder_fileName,
		  CEventRecorder_enabled,
		  CEventRecorder_x,
		  CEventRecorder_y,
		  CEventRecorder_button1Down,
		  CEventRecorder_button2Down,
		  CEventRecorder_button3Down,
		  CEventRecorder_button4Down,
		  CEventRecorder_button5Down,
		  CEventRecorder_rockerDirection,
		  CEventRecorder_rockerDown,
		  CEventRecorder_keyDown,
		  CEventRecorder_keyValue),
		  
		 XFIELD(CEventRecorder,Q_PUBLIC,fileName,
			input_string_type(),
		       "The recording file name."),
		 XFIELD(CEventRecorder,Q_PUBLIC,enabled,
			input_bool_type(),
			"Enables or disables the recording."),
		 XFIELD(CEventRecorder,Q_PUBLIC,x, 
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,y,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,button1Down,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,button2Down,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,button3Down,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,button4Down,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,button5Down,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,rockerDirection,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,rockerDown,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,keyDown,
			event_unsigned_type(),
			""),
		 XFIELD(CEventRecorder,Q_PUBLIC,keyValue,
			event_unsigned_type(),
			"")
	);

public:
    CEventRecorder(CExecutor* aExec, CBaseType *aType = CEventRecorderType::singleton());
    ~CEventRecorder(void);

    void execute(CExecutor* aExec);

    bool enabled(void) { return mEnabled.value(); }

    void x(int aX) { mX.putValue(NULL, aX); }
    void y(int aY) { mY.putValue(NULL, aY); }
    void button1Down(Time aTime) { mButton1Down.putValue(NULL, aTime); }
    void button2Down(Time aTime) { mButton2Down.putValue(NULL, aTime); }
    void button3Down(Time aTime) { mButton3Down.putValue(NULL, aTime); }
    void button4Down(Time aTime) { mButton4Down.putValue(NULL, aTime); }
    void button5Down(Time aTime) { mButton5Down.putValue(NULL, aTime); }
    void rockerDown(Time aTime)  { mRockerDown.putValue(NULL, aTime); }
    void rockerDirection(int aDir) { mRockerDirection.putValue(NULL, aDir); }
    void keyDown(Time aTime) { mKeyDown.putValue(NULL, aTime); }
    void keyValue(int aValue) { mKeyValue.putValue(NULL, aValue); }

private:
    EventString mFileName;
    EventBool   mEnabled;
    EventSigned mX; // Horizontal placement
    EventSigned mY; // Vertical placement.
    EventTime mButton1Down; // Timestamp for button 1 down (0 if not down).
    EventTime mButton2Down; // Timestamp for button 2 down (0 if not down).
    EventTime mButton3Down; // Timestamp for button 3 down (0 if not down).
    EventTime mButton4Down; // Timestamp for button 4 down (0 if not down).
    EventTime mButton5Down; // Timestamp for button 5 down (0 if not down).
    EventSigned mRockerDirection; // Timestamp for rocker down. See mRockerDirection for direciton.
    EventTime mRockerDown; // Timestamp for rocker down. See mRockerDirection for direciton.
    EventTime mKeyDown; // Timestamp for keyboard key down. Set in pair with mKeyValue
    EventSigned mKeyValue; // Timestamp for keyboard key down. Set in pair with mKeyDown
    FILE* mFile;             // file where the events are written
    TimeStamp mStartTime;    // Start time stamp of recording
    bool      mIsRecording;  // Are we recording
};

#endif
