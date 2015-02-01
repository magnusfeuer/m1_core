//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __INPUT_STRATEGY_HH__
#define __INPUT_STRATEGY_HH__

#include "m1.hh"
#include "component.hh"
#include "event_recorder.hh"
#include <map>
//
// Base class for input strategy
// This works by
//
//  CToouchDeviceBaseSubclass  CKeyboardDeviceSubclass
//            |                           |
//            -----------------------------
//                         |
//                         |
//               CInputStrategySubclass
//                         |
//                         |
//                 CEpicScreenComponent
//                         |
//                    { children }
//
class CInputStrategyBase : public CExecutable {
public:
    XOBJECT_TYPE(CInputStrategyBase, 
		 "InputStrategy", 
		 "Input event router",
		 (CInputStrategyBase_layer,
		  CInputStrategyBase_recorder,
		  CInputStrategyBase_x,
		  CInputStrategyBase_y,
		  CInputStrategyBase_button1Down,
		  CInputStrategyBase_button2Down,
		  CInputStrategyBase_button3Down,
		  CInputStrategyBase_button4Down,
		  CInputStrategyBase_button5Down,
		  CInputStrategyBase_rockerDirection,
		  CInputStrategyBase_rockerDown,
		  CInputStrategyBase_keyDown,
		  CInputStrategyBase_keyValue),

		 XFIELD(CInputStrategyBase,Q_PUBLIC,layer,
			CEventType::create(CLayerComponent::CLayerComponentType::singleton(),E_INPUT),
		     "Layer to operate on. Often a screen."),

		XFIELD(CInputStrategyBase,Q_PUBLIC,recorder,
		       CEventType::create(CEventRecorder::CEventRecorderType::singleton(),E_INPUT),
		       "Event recorder."),

		 XFIELD(CInputStrategyBase,Q_PUBLIC,x,
			event_unsigned_type(),
			"X (scaled)"),

		 XFIELD(CInputStrategyBase,Q_PUBLIC,y,
			event_unsigned_type(),
			"Y (scaled)"),

		 XFIELD(CInputStrategyBase,Q_PUBLIC,button1Down,
		       event_unsigned_type(),
		       "Timestamp of button1Down."),

		XFIELD(CInputStrategyBase,Q_PUBLIC,button2Down,
		       event_unsigned_type(),
		       "Timestamp of button2Down."),

		XFIELD(CInputStrategyBase,Q_PUBLIC,button3Down,
		       event_unsigned_type(),
		       "Timestamp of button3Down."),

		 XFIELD(CInputStrategyBase,Q_PUBLIC,button4Down,
		       event_unsigned_type(),
			"Timestamp of button4Down."),

		XFIELD(CInputStrategyBase,Q_PUBLIC,button5Down,
		       event_unsigned_type(),
		       "Timestamp of button5Down."),

		XFIELD(CInputStrategyBase,Q_PUBLIC,rockerDirection,
		       event_unsigned_type(),
		       "1-8 (clockwise from top), 0 = no rocker pressed"),

		XFIELD(CInputStrategyBase,Q_PUBLIC,rockerDown,
		       event_unsigned_type(),
		       "Timestamp of rocker down."),

		 XFIELD(CInputStrategyBase,Q_PUBLIC,keyDown,
			event_unsigned_type(),
			"Key down timestamp"),

		XFIELD(CInputStrategyBase,Q_PUBLIC,keyValue,
		       event_unsigned_type(),
		       "Key value, if pressed.")
	);

public:
    CInputStrategyBase(CExecutor* aExec, CBaseType *aType = 0);
    ~CInputStrategyBase(void);

    void execute(CExecutor* aExec);

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

protected:
    // CLayerPathEntry
    //  Used to build a list of components matching a specific 
    //  X/Y coordinate.
    class CLayerPathEntry {
    public:
	CLayerPathEntry(CLayerComponent *aComponent, 
			float aLocalX = -1,
			float aLocalY = -1):
	    mComponent(aComponent),
	    mLocalX(aLocalX),
	    mLocalY(aLocalY) {
	    m1Retain(CLayerComponent, mComponent);
	}
	CLayerPathEntry(const CLayerPathEntry& aSrc) {
	    mComponent = m1Retain(CLayerComponent, aSrc.mComponent);
	    mLocalX    = aSrc.mLocalX;
	    mLocalY    = aSrc.mLocalY;
	}
	~CLayerPathEntry() {
	    m1Release(CLayerComponent, mComponent);
	}

	CLayerComponent* component(void) { return mComponent; }
	float localX(void) { return mLocalX; }
	float localY(void) { return mLocalY; }

    private:
	CLayerComponent *mComponent; // The component we are talking about.
	float mLocalX;              // X according to local coordinate system.
	float mLocalY;              // Y according to local coordinate system.
    };

    typedef list<CLayerPathEntry> CLayerPathEntryList;
    typedef list<CLayerPathEntry>::iterator CLayerPathEntryListIterator;
    
    void resetLayer(CExecutor* aExec, CLayerComponent*aLayer);
    int collectLayers(CLayerComponent *aLayer, 
		      CLayerPathEntryList *aResult, 
		      float aX, 
		      float aY, 
		      float aTopOffset, 
		      float aLeftoffset);
    
    void uniq(CLayerPathEntryList *aOld, 
	      CLayerPathEntryList *aNew, 
	      CLayerComponentList *aOnlyInOld);
    void postEvent(CExecutor* aExec, CLayerComponent* aLayer,
		   MessageName aMessagename, float x, float y, int aValue);
protected:
    EventObject<CLayerComponent *> mLayer;   // Screen to use.
    EventObject<CEventRecorder *> mRecorder; // Event recorder 
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

    CLayerPathEntryList mMouseOverList[2]; // Current and previous list of mouseover items.
    int mMouseOverInd;  // Current index to use.
};


#endif
