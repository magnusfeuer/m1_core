//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//

#include "input_device.hh"

XOBJECT_TYPE_BOOTSTRAP(CInputDeviceBase);

//
// Helper function.
//
void updateInputStrategyObject(CExecutor* aExec, 
			       CInputStrategyBase *aInputStrategy, 
			       int aTargetIndex, 
			       unsigned int aValue)
{
    if (aInputStrategy)
	aInputStrategy->put(aExec, aTargetIndex, UUnsigned(aValue));
}


CInputDeviceBase::CInputDeviceBase(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType),
    mAbsoluteDevice(this),
    mX(this), // Horizontal placement
    mY(this), // Vertical placement.
    mRawX(this), 
    mRawY(this), 
    mButton1Down(this), 
    mButton2Down(this), 
    mButton3Down(this), 
    mButton4Down(this), 
    mButton5Down(this), 
    mRockerDirection(this),
    mRockerDown(this),
    mMinUnscaledX(this), 
    mMaxUnscaledX(this),
    mMinUnscaledY(this),
    mMaxUnscaledY(this),
    mScaledHeight(this), 
    mScaledWidth(this),
    mSwapXY(this),
    mInvertX(this),
    mInvertY(this),
    mKeyDown(this),
    mKeyValue(this)
{ 
    mX.putValue(aExec, 0);
    mY.putValue(aExec, 0);
    mRawX.putValue(aExec, 0);
    mRawY.putValue(aExec, 0);

    mButton1Down.putValue(aExec, 0);
    mButton2Down.putValue(aExec, 0);
    mButton3Down.putValue(aExec, 0);
    mButton4Down.putValue(aExec, 0);
    mButton5Down.putValue(aExec, 0);

    mMinUnscaledY.putValue(aExec, -1);
    mMinUnscaledX.putValue(aExec, -1);
    mMaxUnscaledX.putValue(aExec, -1);
    mMaxUnscaledY.putValue(aExec, -1);
    mScaledHeight.putValue(aExec, -1);
    mScaledWidth.putValue(aExec, -1);
    mSwapXY.putValue(aExec, false);
    mInvertY.putValue(aExec, false);
    mInvertX.putValue(aExec, false);

    mKeyDown.putValue(aExec, 0);
    mKeyValue.putValue(aExec, 0);

    eventPut(aExec, XINDEX(CInputDeviceBase,absoluteDevice), &mAbsoluteDevice);
    eventPut(aExec, XINDEX(CInputDeviceBase,x), &mX);
    eventPut(aExec, XINDEX(CInputDeviceBase,y), &mY);
    eventPut(aExec, XINDEX(CInputDeviceBase,rawX), &mRawX);
    eventPut(aExec, XINDEX(CInputDeviceBase,rawY), &mRawY);
    eventPut(aExec, XINDEX(CInputDeviceBase,button1Down), &mButton1Down); 
    eventPut(aExec, XINDEX(CInputDeviceBase,button2Down), &mButton2Down); 
    eventPut(aExec, XINDEX(CInputDeviceBase,button3Down), &mButton3Down); 
    eventPut(aExec, XINDEX(CInputDeviceBase,button4Down), &mButton4Down); 
    eventPut(aExec, XINDEX(CInputDeviceBase,button5Down), &mButton5Down); 
    eventPut(aExec, XINDEX(CInputDeviceBase,rockerDirection), &mRockerDirection); 
    eventPut(aExec, XINDEX(CInputDeviceBase,rockerDown), &mRockerDown); 
    eventPut(aExec, XINDEX(CInputDeviceBase,minUnscaledX), &mMinUnscaledX);
    eventPut(aExec, XINDEX(CInputDeviceBase,maxUnscaledX), &mMaxUnscaledX);
    eventPut(aExec, XINDEX(CInputDeviceBase,minUnscaledY), &mMinUnscaledY);
    eventPut(aExec, XINDEX(CInputDeviceBase,maxUnscaledY), &mMaxUnscaledY);

    eventPut(aExec, XINDEX(CInputDeviceBase,scaledHeight), &mScaledHeight);
    eventPut(aExec, XINDEX(CInputDeviceBase,scaledWidth), &mScaledWidth);

    eventPut(aExec, XINDEX(CInputDeviceBase,swapXY), &mSwapXY);
    eventPut(aExec, XINDEX(CInputDeviceBase,invertX), &mInvertX);
    eventPut(aExec, XINDEX(CInputDeviceBase,invertY), &mInvertY);

    eventPut(aExec, XINDEX(CInputDeviceBase,keyDown), &mKeyDown);
    eventPut(aExec, XINDEX(CInputDeviceBase,keyValue), &mKeyValue);

    mAbsoluteDevice.putValue(aExec, false);
}

CInputDeviceBase::~CInputDeviceBase(void) 
{
}

CInputStrategyBase* CInputDeviceBase::inputStrategy()
{
    return (CInputStrategyBase*) at(XINDEX(CInputDeviceBase,inputStrategy)).o;
}

int CInputDeviceBase::calcAbsX(int aX)
{ 
    int x = aX;

    if (x < mMinUnscaledX.value())
	x = mMinUnscaledX.value();

    if (x > mMaxUnscaledX.value())
	x = mMaxUnscaledX.value();

    x = (int) (((float) mScaledWidth.value() / 
		(float) (mMaxUnscaledX.value() - mMinUnscaledX.value())) * 
	       (float) (x - mMinUnscaledX.value()));
    
    if (mInvertX.value()) 
	x = mScaledWidth.value() - x;

	
    return x;
}

int CInputDeviceBase::calcAbsY(int aY)
{ 
    int y = aY;

    if (y < mMinUnscaledY.value())
	y = mMinUnscaledY.value();

    if (y > mMaxUnscaledY.value())
	y = mMaxUnscaledY.value();

    y = (int) (((float) mScaledHeight.value() / 
		(float) (mMaxUnscaledY.value() - mMinUnscaledY.value())) * 
	       (float) (y - mMinUnscaledY.value()));

    if (mInvertY.value()) 
	y = mScaledHeight.value() - y;

    return y;
}


void CInputDeviceBase::buttonDown(CExecutor* aExec, int aButton, Time aMSec)
{
    switch(aButton) {
    case 1:
	// Make sure that we have been down at least one msec.
	mButton1Down.putValue(aExec, aMSec);
	updateInputStrategyObject(aExec,
				  inputStrategy(), 
				  XINDEX(CInputStrategyBase,button1Down),
				  (unsigned int) mButton1Down.value());
	break;
    case 2:
	// Make sure that we have been down at least one msec.
	mButton2Down.putValue(aExec, aMSec);
	updateInputStrategyObject(aExec,
				  inputStrategy(),
				  XINDEX(CInputStrategyBase,button2Down),
				  (unsigned int) mButton2Down.value());
	break;
    case 3:
	// Make sure that we have been down at least one msec.
	mButton3Down.putValue(aExec, aMSec);
	updateInputStrategyObject(aExec,
				  inputStrategy(), 
				  XINDEX(CInputStrategyBase,button3Down),
				  (unsigned int) mButton3Down.value());
	break;
    case 4:
	// Make sure that we have been down at least one msec.
	mButton4Down.putValue(aExec, aMSec);
	updateInputStrategyObject(aExec,
				  inputStrategy(),
				  XINDEX(CInputStrategyBase,button4Down),
				  (unsigned int) mButton4Down.value());
	break;
    case 5:
	// Make sure that we have been down at least one msec.
	mButton5Down.putValue(aExec, aMSec);
	updateInputStrategyObject(aExec,
				  inputStrategy(),
				  XINDEX(CInputStrategyBase,button5Down),
				  (unsigned int) mButton5Down.value());
	break;
    default:
	DBGFMT("CInputDeviceBase::buttonDown(): Unknown button [%d]", aButton);
    }
}


void CInputDeviceBase::buttonUp(CExecutor* aExec, int aButton)
{
    buttonDown(aExec, aButton, 0);
}


void CInputDeviceBase::rockerDown(CExecutor* aExec, int aDirection, Time aMSec)
{
    mRockerDirection.putValue(aExec, aDirection);
    mRockerDown.putValue(aExec, aMSec);
}

void CInputDeviceBase::rockerUp(CExecutor* aExec)
{
    mRockerDirection.putValue(aExec, 0);
    mRockerDown.putValue(aExec, 0);
}

void CInputDeviceBase::rawAbsoluteX(CExecutor* aExec, int aRawX, bool aNoSwap)
{
    mRawX.putValue(aExec, aRawX);
    if (mSwapXY.value() && !aNoSwap) {
	rawAbsoluteY(aExec, aRawX, true);
	return;
    }

    mX.putValue(aExec, calcAbsX(aRawX));

    updateInputStrategyObject(aExec,
			      inputStrategy(),
			      XINDEX(CInputStrategyBase,x),
			      (unsigned int) mX.value());
    if (!absoluteDevice())
	absoluteDevice(true);
}

void CInputDeviceBase::rawAbsoluteY(CExecutor* aExec, int aRawY, bool aNoSwap)
{
    mRawY.putValue(aExec, aRawY);
    if (mSwapXY.value() && !aNoSwap) {
	rawAbsoluteX(aExec, aRawY, true);
	return;
    }

    mY.putValue(aExec, calcAbsY(aRawY));
    updateInputStrategyObject(aExec,
			      inputStrategy(),
			      XINDEX(CInputStrategyBase,y),
			      (unsigned int) mY.value());

    if (!absoluteDevice())
	absoluteDevice(true);
}


void CInputDeviceBase::rawRelativeX(CExecutor* aExec,int aRelX, bool aNoSwap)
{
    mRawX.putValue(aExec, aRelX);
    if (mSwapXY.value() && !aNoSwap) {
	rawRelativeX(aExec, aRelX, true);
	return;
    }

    mX.putValue(aExec, aRelX + mX.value());
    if (mX.value() < 0) {
	mX.putValue(aExec, 0);
    }

    if (mX.value() > mScaledWidth.value()) {
	mX.putValue(aExec, mScaledWidth.value());
    }

    updateInputStrategyObject(aExec,
			      inputStrategy(),
			      XINDEX(CInputStrategyBase,x),
			      (unsigned int) mX.value());

    if (absoluteDevice())
	absoluteDevice(false);
}

void CInputDeviceBase::rawRelativeY(CExecutor* aExec,int aRelY, bool aNoSwap)
{
    mRawY.putValue(aExec, aRelY);
    if (mSwapXY.value() && !aNoSwap) {
	rawRelativeY(aExec, aRelY, true);
	return;
    }

    mY.putValue(aExec, aRelY + mY.value());
    if (mY.value() < 0)
	mY.putValue(aExec, 0);

    if (mY.value() > mScaledHeight.value())
	mY.putValue(aExec, mScaledHeight.value());

    updateInputStrategyObject(aExec,
			      inputStrategy(),
			      XINDEX(CInputStrategyBase,y),
			      (unsigned int) mY.value());

    if (absoluteDevice())
	absoluteDevice(false);
}

void CInputDeviceBase::keyDown(CExecutor* aExec, int aKeyCode, TimeStamp aTimeStamp)
{
    mKeyValue.putValue(aExec, aKeyCode);
    mKeyDown.putValue(aExec, m1_TimeStampToTime(aTimeStamp));
    updateInputStrategyObject(aExec,
			      inputStrategy(), 
			      XINDEX(CInputStrategyBase,keyValue),
			      (unsigned int) mKeyValue.value());

    updateInputStrategyObject(aExec,
			      inputStrategy(), 
			      XINDEX(CInputStrategyBase,keyDown),
			      (unsigned int) mKeyDown.value());

    
}

void CInputDeviceBase::keyUp(CExecutor* aExec, int aKeyCode)
{
    mKeyValue.putValue(aExec, aKeyCode);
    mKeyDown.putValue(aExec, 0);

    updateInputStrategyObject(aExec,
			      inputStrategy(),
			      XINDEX(CInputStrategyBase,keyValue),
			      (unsigned int) mKeyValue.value());

    updateInputStrategyObject(aExec,
			      inputStrategy(),
			      XINDEX(CInputStrategyBase,keyDown),
			      0);
}


void CInputDeviceBase::execute(CExecutor* aExec)
{
    return;
}

