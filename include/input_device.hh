//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __INPUT_DEVICE_HH__
#define __INPUT_DEVICE_HH__

#include "m1.hh"
#include "input_strategy.hh"

//
// Base class used by all base class plugins.
// Sets up a number of public and event variables and an
// internal interface structure to be used by subclasses.
//  
// A single public pure virtual function must be properly defined
// unsigned long profile() = 0;
//  Define to return a correct collection of bits from the INP_ suite.
//
// The internal (protected) methods are:
//
// 
// setAbsoluteX()/setAbsoluteY() 
//  These two are used to set a raw x and y value read by the touchscreen/whatever. 
//  The raw values will be converted through [min|max]Unscaled[X|Y] to a normalized absolute
//  x and y value propagated through the "x" and "y" event symbols.
//
// addRelativeX()/addRelativeY()
//  Processes a relative delta (positive or negative) to be applied to the "x" and "y" symbols.
//  Will be multiplied by the relativeXMultiplier and relativeYMultiplier symbols before
//  being added to x/y.
// 
// keyDown(int keyCode)/keyUp(int keyCode)
//  For keyboard down or up. TODO: Define a map between keycodes and unicode!
// 
// rockerDown/rockerUp(int aDirection)
//  One of the rocker directions has been activated. aDirection has range 1-8, where 1 is up,
//  2 is up/right, 3 is right, 4 is down right, etc. If It is a four way rocker,
//  Only 1, 3, 5, and 7 will be reporeted.
//  A two-way rocker will report 1 and 5.
//
// buttonDown(int aButtonNumber)/buttonUp(aButtonNumber)
//  One of the (non rocker) buttons have been pressed or released. 
// 
//
// The following event variables are created and set by
// the CInputDeviceBase when one or more of the methods above are called.
//
// absoluteX/Y 
//  Active when INP_ABSOLUTE_POSITION or INP_RELATIVE_POSITION is set
//  The absolue X and Y values of the pointer (or finger). 
// 
// keyDown 
//   Active when INP_KEYBOARD is set.
//   Set to the unicode of the pressed key. When the key is released, it is set to 0.
//
// rockerDown 
//   Active when INP_?_ROCKER is set.
//   Set to the direction of the rocker (1-8)
//   When rocker is released, it is set to 0.
//
// button1Down
//   Set to true when the button one is pressed.
//   Set to false when the button is released.
//
// button2Down
//   Set to true when the button two is pressed.
//   Set to false when the button is released.
//
// button3Down
//   Set to true when the button three is pressed.
//   Set to false when the button is released.
//
// button4Down
//   Set to true when the button four is pressed.
//   Set to false when the button is released.
//
// button5Down
//   Set to true when the button five is pressed.
//   Set to false when the button is released.
//
// The reason why multiple buttons are defined instead of just having 
// a single button symbol being set to a value is that multiple
// can be pressed simultaneously.
//
#define INP_ABSOLUTE_POSITION 0x00000001  // Will call setAbsoluteX/Y
#define INP_RELATIVE_POSITION 0x00000002  // Will call addRelativeX/Y
#define INP_KEYBOARD          0x00000004  // Will call keyDown/keyUp
#define INP_2_WAY_ROCKER      0x00000008  // Will call rockerUp/rockerDown Collapse these bitfield to a range.
#define INP_4_WAY_ROCKER      0x00000010  // Will call rockerUp/rockerDown/left/right
#define INP_8_WAY_ROCKER      0x00000020  // Will call rockerUp/rockerDown/left/right/
                                          // rockerUpLeft/rockerDownLeft/rockerDownRight/rockerUpRight
#define INP_1_BUTTONS         0x00000040 // Has one button apart from rocker. Will call buttonDown(0)/buttonUp(0);
#define INP_2_BUTTONS         0x00000080 // Has two buttons apart from rocker. Will call buttonDown(0-1)/buttonUp(0-1);
#define INP_3_BUTTONS         0x00000100 // Has three butons apart from rocker. Will call buttonDown(0-2)/buttonUp(0-2);
#define INP_4_BUTTONS         0x00000200 // Has four buttons apart from rocker. Will call buttonDown(0-3)/buttonUp(0-3);
#define INP_5_BUTTONS         0x00000400 // Has five buttons apart from rocker. Will call buttonDown(0-4)/buttonUp(0-4);

// CInputDeviceBase
//  The base class for mice, rockers, touch screen devices, keyboard etc.
//
class CInputDeviceBase : public CExecutable {
public:
    XVIRTUAL_OBJECT_TYPE(CInputDeviceBase,
			 "InputDevice", 
			 "Input device base", 
			 (CInputDeviceBase_inputStrategy,
			  CInputDeviceBase_absoluteDevice,
			  CInputDeviceBase_x,
			  CInputDeviceBase_y,
			  CInputDeviceBase_rawX,
			  CInputDeviceBase_rawY,
			  CInputDeviceBase_button1Down,
			  CInputDeviceBase_button2Down,
			  CInputDeviceBase_button3Down,
			  CInputDeviceBase_button4Down,
			  CInputDeviceBase_button5Down,
			  CInputDeviceBase_rockerDirection,
			  CInputDeviceBase_rockerDown,
			  CInputDeviceBase_minUnscaledX,
			  CInputDeviceBase_maxUnscaledX,
			  CInputDeviceBase_minUnscaledY,
			  CInputDeviceBase_maxUnscaledY,
			  CInputDeviceBase_scaledHeight,
			  CInputDeviceBase_scaledWidth,
			  CInputDeviceBase_swapXY,
			  CInputDeviceBase_invertX,
			  CInputDeviceBase_invertY,
			  CInputDeviceBase_keyDown,
			  CInputDeviceBase_keyValue),
			  
			 XFIELD(CInputDeviceBase,Q_PUBLIC,inputStrategy, 
				CInputStrategyBase::CInputStrategyBaseType::singleton(),
				"The input strategy instance we want to feed."), 

			 XFIELD(CInputDeviceBase,Q_PUBLIC,absoluteDevice,
				event_bool_type(),
				""),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,x,
				event_signed_type(),
				"Scaled and normalized X."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,y,
				event_signed_type(),
				"Scaled and normalized Y."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,rawX,
				event_signed_type(),
				"Raw X value."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,rawY,
				event_signed_type(),
				"Raw Y value."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,button1Down,
				event_time_type(),
				"Timestamp of button1Down."), 
			 
			 XFIELD(CInputDeviceBase,Q_PUBLIC,button2Down,
				event_time_type(),
				"Timestamp of button2Down."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,button3Down,
				event_time_type(),
				"Timestamp of button3Down."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,button4Down,
				event_time_type(),
				"Timestamp of button4Down."), 

			 XFIELD(CInputDeviceBase,Q_PUBLIC,button5Down,
				event_time_type(),
				"Timestamp of button5Down."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,rockerDirection,
				event_signed_type(),
				"1-8 (clockwise from top), 0 = no rocker pressed"),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,rockerDown,
				event_time_type(),
				"Timestamp of rocker down."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,minUnscaledX,
				event_unsigned_type(),
				"Min unscaled X - Only used for INP_ABSOLUTE_POSITION"),
			 
			 XFIELD(CInputDeviceBase,Q_PUBLIC,maxUnscaledX,
				event_unsigned_type(),
				"Max unscaled X - Only used for INP_ABSOLUTE_POSITION"),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,minUnscaledY,
				event_unsigned_type(),
				"Min unscaled Y - Only used for INP_ABSOLUTE_POSITION"),

			XFIELD(CInputDeviceBase,Q_PUBLIC,maxUnscaledY,
			       event_unsigned_type(),
			       "Max unscaled Y - Only used for INP_ABSOLUTE_POSITION"),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,scaledHeight,
				event_unsigned_type(),
				"Scaled height."),
			 
			 XFIELD(CInputDeviceBase,Q_PUBLIC,scaledWidth,
				event_unsigned_type(),
				"Scaled width."),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,swapXY,
				event_bool_type(),
				"Shall we swap the X/Y channels. (Touchscreen rotated 90 degrees)"
			     ),

			 XFIELD(CInputDeviceBase,Q_PUBLIC,invertX,
				event_bool_type(),
				"Shall we invert the unscaled X value when scaling?"
			     ),
			 
			 XFIELD(CInputDeviceBase,Q_PUBLIC,invertY,
				event_bool_type(),
				"Shall we invert the unscaled Y value when scaling?"),
			 XFIELD(CInputDeviceBase,Q_PUBLIC,keyDown,
				event_unsigned_type(),
				"Keyboard down timestamp."
			     ),
			 XFIELD(CInputDeviceBase,Q_PUBLIC,keyValue,
				event_unsigned_type(),
				"Keyboard value, if pressed."
			     )
	);

public:
    CInputDeviceBase(CExecutor* aExec, CBaseType *aType = 0);
    ~CInputDeviceBase(void);

    void execute(CExecutor* aExec);
    
private:
    int calcAbsX(int aX);
    int calcAbsY(int aY);
    
protected:
    CInputStrategyBase* inputStrategy();

    void buttonDown(CExecutor* aExec, int aButton, Time aMSec); // Nr of msec button has been down for.
    void buttonUp(CExecutor* aExec, int aButton);

    void rockerDown(CExecutor* aExec, int aDirection, Time aMSec);
    void rockerUp(CExecutor* aExec);

    void rawAbsoluteX(CExecutor* aExec, int aRawX, bool aNoSwap = false); // Called by touchscreens, pads, etc, to report absolute pos
    void rawAbsoluteY(CExecutor* aExec, int aRawY, bool aNoSwap = false); // Called by touchscreens, pads, etc, to report absolute pos

    void rawRelativeX(CExecutor* aExec, int aRawX, bool aNoSwap = false); // Called by mice,e tc to report relative pos change. Can be neg.
    void rawRelativeY(CExecutor* aExec, int aRawY, bool aNoSwap = false);// Called by mice,e tc to report relative pos change. Can be neg.

    void absoluteDevice(bool aIsAbsolute) { mAbsoluteDevice.putValue(NULL, aIsAbsolute); }
    bool absoluteDevice(void) { return mAbsoluteDevice.value(); }

    void keyDown(CExecutor* aExec, int aKeyCode, TimeStamp aTimeStamp);

    void keyUp(CExecutor* aExec, int aKeyCode);
protected:
    EventBool mAbsoluteDevice;
    EventSigned mX; // Horizontal placement
    EventSigned mY; // Vertical placement.

    EventSigned mRawX; // Raw X value from device.
    EventSigned mRawY; // Raw Y value from device.

    EventTime mButton1Down; // Timestamp for button 1 down (0 if not down).
    EventTime mButton2Down; // Timestamp for button 2 down (0 if not down).
    EventTime mButton3Down; // Timestamp for button 3 down (0 if not down).
    EventTime mButton4Down; // Timestamp for button 4 down (0 if not down).
    EventTime mButton5Down; // Timestamp for button 5 down (0 if not down).

    EventSigned mRockerDirection; // Timestamp for rocker down. See mRockerDirection for direciton.
    EventTime mRockerDown; // Timestamp for rocker down. See mRockerDirection for direciton.

    EventSigned mMinUnscaledX; 
    EventSigned mMaxUnscaledX;
    EventSigned mMinUnscaledY;
    EventSigned mMaxUnscaledY;
    
    EventSigned mScaledHeight; 
    EventSigned mScaledWidth;
    EventBool mSwapXY;

    EventBool mInvertX;
    EventBool mInvertY;
    EventTime mKeyDown;
    EventSigned mKeyValue;
};

#endif
