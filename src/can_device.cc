//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include "can_device.hh"

XOBJECT_TYPE_BOOTSTRAP(CCanFrame);
XOBJECT_TYPE_BOOTSTRAP(CCanDevice);

CCanFrame::CCanFrame(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mCode(this),
    mMask(this),
    mId(this),
    mLength(this)
{
    mData = new CArray(aExec,(CArrayType*)typeAt(XINDEX(CCanFrame,data)),
		       sizeof(UData), 8);
    mCode.putValue(aExec, 0x00000000);
    mMask.putValue(aExec, 0xffffffff);
    mId.putValue(aExec, 0);
    mLength.putValue(aExec, 0);

    eventPut(aExec,XINDEX(CCanFrame,code), &mCode);
    eventPut(aExec,XINDEX(CCanFrame,mask), &mMask);
    eventPut(aExec,XINDEX(CCanFrame,id), &mId);
    eventPut(aExec,XINDEX(CCanFrame,length), &mLength);

    put(aExec, XINDEX(CCanFrame,data), UArray(mData));
}

CCanFrame::~CCanFrame()
{
    
}

//
// Check mCode and mMask match
// mCode checks for bits set
// mMask checks for bits clear
//
bool CCanFrame::match(unsigned long aId)
{
    unsigned int code = mCode.value();
    unsigned int mask = mMask.value();
    return (((aId & code) == code) && ((aId & mask) == aId));
}

void CCanFrame::execute(CExecutor* aExec)
{
}


CCanDevice::CCanDevice(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mFormat(this),
    mCanSpeed(this),
    mPort(this),
    mPortSpeed(this),
    mState(this),
    mOutputs(this)
{
    CArray* frame_in;

    // Create initial input array of zero size
    frame_in  = new CArray(aExec,
			   (CArrayType*)typeAt(XINDEX(CCanDevice,inputs)), 
			   sizeof(UData), 0);

    mFormat.putValue(aExec, CAN_BASE_FRAME_FORMAT);  // default base frame format
    mCanSpeed.putValue(aExec, 250);                 // default 250K bit 
    mPortSpeed.putValue(aExec, 230400);             // default 230400 Baud

    eventPut(aExec,XINDEX(CCanDevice,format), &mFormat);
    eventPut(aExec,XINDEX(CCanDevice,canSpeed), &mCanSpeed);
    eventPut(aExec,XINDEX(CCanDevice,port), &mPort);
    eventPut(aExec,XINDEX(CCanDevice,portSpeed), &mPortSpeed);
    eventPut(aExec,XINDEX(CCanDevice,state), &mState);
    eventPut(aExec,XINDEX(CCanDevice,outputs), &mOutputs);
    put(aExec, XINDEX(CCanDevice,inputs), UArray(frame_in));
}

CCanDevice::~CCanDevice()
{
}


void CCanDevice::execute(CExecutor* aExec)
{
    // Device updated ...
}




