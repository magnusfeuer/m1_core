//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __CAN_DEVICE_HH__
#define __CAN_DEVICE_HH__

#include "m1.hh"

typedef enum {
    CAN_BASE_FRAME_FORMAT = 1,
    CAN_EXTENDED_FRAME_FORMAT = 2
};

ENUM_TYPE(CCanFrameFormat, "CanFrameFormat",
	  ENUMERATION(base,     CAN_BASE_FRAME_FORMAT),
	  ENUMERATION(exteded,  CAN_EXTENDED_FRAME_FORMAT)
    );

typedef enum {
    CAN_DEVICE_STATE_NONE   = 0,  // none
    CAN_DEVICE_STATE_NODEV  = 1,  // could not open device, or no support
    CAN_DEVICE_STATE_INIT   = 2,  // initialize
    CAN_DEVICE_STATE_CLOSED = 3,  // can channel closed
    CAN_DEVICE_STATE_OPEN   = 4   // can channel open
};

ENUM_TYPE(CCanDeviceState, "CanDeviceState",
	  ENUMERATION(none, CAN_DEVICE_STATE_NONE),
	  ENUMERATION(nodev, CAN_DEVICE_STATE_NODEV),
	  ENUMERATION(init, CAN_DEVICE_STATE_INIT),
	  ENUMERATION(closed, CAN_DEVICE_STATE_CLOSED),
	  ENUMERATION(open, CAN_DEVICE_STATE_OPEN)
    );


class CCanFrame : public CExecutable {
public:
    XOBJECT_TYPE(CCanFrame, "CanFrame",
		 "Can frame data structure",
		 (CCanFrame_code,
		  CCanFrame_mask,
		  CCanFrame_id,
		  CCanFrame_length,
		  CCanFrame_data),
		 XFIELD(CCanFrame,Q_PUBLIC,code,
			event_unsigned_type(),
			"Can match bits. For an input frame to match then all"
			" bits set in code must also be set in frame id."
		     ),
		 XFIELD(CCanFrame,Q_PUBLIC,mask,
			event_unsigned_type(),
			"Can mask bits. For an input frame to match then all"
			" bits zero in mask must also be zero in frame id."
		     ),
		 XFIELD(CCanFrame,Q_PUBLIC,id,
			event_unsigned_type(),
			"Match frame id."),
		 XFIELD(CCanFrame,Q_PUBLIC,length,
			event_byte_type(),
			"Real length of frame data, between 0 and 8."
		     ),
		 XFIELD(CCanFrame,Q_PUBLIC,data,
			CArrayType::create(event_byte_type(), 8),
			"Frame data")
	);

public:
    CCanFrame(CExecutor* aExec,
	      CBaseType *aType = CCanFrameType::singleton());
    ~CCanFrame(void);

    void execute(CExecutor* aExec);
    bool match(unsigned long aId);
public:
    EventUnsigned mCode;
    EventUnsigned mMask;
    EventUnsigned mId;
    EventByte     mLength;
    CArray*       mData;
};

//
// This is the base class for all CanDevices (inherit this for implementation)
//
class CCanDevice: public CExecutable {
public:
    XOBJECT_TYPE(CCanDevice, "CanDevice",
		 "Base class for can devices",
		 (CCanDevice_format,
		  CCanDevice_canSpeed,
		  CCanDevice_port,
		  CCanDevice_portSpeed,
		  CCanDevice_state,
		  CCanDevice_inputs,
		  CCanDevice_outputs),
		 XFIELD(CCanDevice,Q_PUBLIC,format,
			EVENT_TYPE(CCanFrameFormatType, E_INOUT),
			"CAN frame type base or extended"),

		 XFIELD(CCanDevice,Q_PUBLIC,canSpeed,
			input_unsigned_type(),
			"CAN bus speed in Kbits 10,20...,1024"),

		XFIELD(CCanDevice,Q_PUBLIC,port,
		       event_string_type(),
		       "CAN device name or identifier"),

		XFIELD(CCanDevice,Q_PUBLIC,portSpeed,
		       input_unsigned_type(),
		       "CAN device speed, 9600, ... 230400"),

		XFIELD(CCanDevice,Q_PUBLIC,state, 
		       EVENT_TYPE(CCanDeviceStateType, E_OUTPUT),
		       "CAN device state"),

		 XFIELD(CCanDevice,Q_PUBLIC,inputs, 
			ARRAY_TYPE(CCanFrame::CCanFrameType, 0),
			"Input array of can frame receivers."),

		 XFIELD(CCanDevice,Q_PUBLIC,outputs, 
			EVENT_QUEUE_TYPE(CCanFrame::CCanFrameType, E_INPUT),
			"Output queue of frame senders.")
	);
public:
    CCanDevice(CExecutor* aExec,
		  CBaseType *aType = CCanDeviceType::singleton());
    ~CCanDevice(void);

    void execute(CExecutor* aExec);

protected:
    EventSigned   mFormat;            // CAN frame format
    EventUnsigned mCanSpeed;          // CAN bus speed
    EventString   mPort;              // Port or file we are to read data from
    EventUnsigned mPortSpeed;         // CAN bus speed
    EventSigned   mState;             // CAN device status
    EventQueueObject<CCanFrame*> mOutputs; // Queue of output frames
};


#endif
