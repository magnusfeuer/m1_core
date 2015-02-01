//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __MESSAGE_HH__
#define __MESSAGE_HH__

#include "m1.hh"

/////////////////////////////////////////////////////////////////////////////
//  Message object
/////////////////////////////////////////////////////////////////////////////

// Message masks (top 8 bits of 16)
#define MESSAGE_MASK_NONE     0x0000
#define MESSAGE_MASK_OTHER    0x0100
#define MESSAGE_MASK_BUTTON   0x0200
#define MESSAGE_MASK_ROCKER   0x0400
#define MESSAGE_MASK_KEY      0x0800
#define MESSAGE_MASK_MOTION   0x1000
#define MESSAGE_MASK_CROSSING 0x2000
#define MESSAGE_MASK_ALL      0xFF00

// Message "names" (note that mask is one component in messages)
typedef enum {
    MESSAGE_NONE         = MESSAGE_MASK_OTHER    | 0,
    MESSAGE_USER         = MESSAGE_MASK_OTHER    | 1,
    MESSAGE_BUTTON_UP    = MESSAGE_MASK_BUTTON   | 1,
    MESSAGE_BUTTON_DOWN  = MESSAGE_MASK_BUTTON   | 2,
    MESSAGE_ROCKER_UP    = MESSAGE_MASK_ROCKER   | 1,
    MESSAGE_ROCKER_DOWN  = MESSAGE_MASK_ROCKER   | 2,
    MESSAGE_MOTION       = MESSAGE_MASK_MOTION   | 3,
    MESSAGE_KEY_UP       = MESSAGE_MASK_KEY      | 1,
    MESSAGE_KEY_DOWN     = MESSAGE_MASK_KEY      | 2,
    MESSAGE_ENTER        = MESSAGE_MASK_CROSSING | 1,
    MESSAGE_LEAVE        = MESSAGE_MASK_CROSSING | 2
} MessageName;

ENUM_TYPE(CMessageName, "MessageName",
	  ENUMERATION(none,        MESSAGE_NONE),
	  ENUMERATION(buttonUp,    MESSAGE_BUTTON_UP),
	  ENUMERATION(buttonDown,  MESSAGE_BUTTON_DOWN),
	  ENUMERATION(motion,      MESSAGE_MOTION),
	  ENUMERATION(keyUp,       MESSAGE_KEY_UP),
	  ENUMERATION(keyDown,     MESSAGE_KEY_DOWN),
	  ENUMERATION(rockerUp,    MESSAGE_BUTTON_UP),
	  ENUMERATION(rockerDown,  MESSAGE_BUTTON_DOWN),
	  ENUMERATION(enter,       MESSAGE_ENTER), 
	  ENUMERATION(leave,       MESSAGE_LEAVE), 
	  ENUMERATION(user,        MESSAGE_USER)
    );

class CMessage : public CExecutable {
public:
    XOBJECT_TYPE(CMessage, "Message",
		 "Input message event record",
		 (CMessage_name,
		  CMessage_x,
		  CMessage_y,
		  CMessage_z,
		  CMessage_x0,
		  CMessage_y0,
		  CMessage_timeStamp,
		  CMessage_value,
		  CMessage_message_mask_none,
		  CMessage_message_mask_other,
		  CMessage_message_mask_button,
		  CMessage_message_mask_rocker,
		  CMessage_message_mask_key,
		  CMessage_message_mask_motion,
		  CMessage_message_mask_crossing,
		  CMessage_message_mask_all),
		 XFIELD(CMessage,Q_PUBLIC,name,
			CMessageNameType::singleton(),
			"Message name/type"),
		 XFIELD(CMessage,Q_PUBLIC,x,
			float_type(),
			"Local layer x coordinate"),
		 XFIELD(CMessage,Q_PUBLIC,y,
			float_type(),
			"Local layer y coordinate"),
		 XFIELD(CMessage,Q_PUBLIC,z,
			float_type(),
			"Local layer z coordinate or sensitivity level"),
		 XFIELD(CMessage,Q_PUBLIC,x0,
			float_type(),
			"Screen x coordinate."),
		 XFIELD(CMessage,Q_PUBLIC,y0,
			float_type(),
			"Screen y coordinate."),
		 XFIELD(CMessage,Q_PUBLIC,timeStamp,
			time_type(),
			"Timestamp, when message was sent."),
		 XFIELD(CMessage,Q_PUBLIC,value,
			signed_type(),
			"Button number, rocker direction, key code etc"),
		 // Message type constants
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_none,
			unsigned_type(),
			"no events", 
			UUnsigned(MESSAGE_MASK_NONE)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,
			message_mask_other,
			unsigned_type(),
			"unspecified events", 
			UUnsigned(MESSAGE_MASK_OTHER)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_button,
			unsigned_type(),
			"button events",
			UUnsigned(MESSAGE_MASK_BUTTON)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_rocker, 
			unsigned_type(),
			"rocker events",
			UUnsigned(MESSAGE_MASK_ROCKER)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_key, 
			unsigned_type(),
			"key events", 
			UUnsigned(MESSAGE_MASK_KEY)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_motion,
			 unsigned_type(),
			 "motion events",
			 UUnsigned(MESSAGE_MASK_MOTION)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_crossing,
			 unsigned_type(),
			 "crossing events",
			 UUnsigned(MESSAGE_MASK_CROSSING)),
		 XFIELDC(CMessage,Q_PUBLIC|Q_CONST,message_mask_all,
			 unsigned_type(),
			 "all events", 
			 UUnsigned(MESSAGE_MASK_ALL))
	);

public:
    CMessage(CExecutor* aExec, CBaseType *aType = CMessageType::singleton());
    ~CMessage(void);
private:
};


#endif
