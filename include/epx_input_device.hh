//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __EPX_INPUT_DEVICE_H__
#define __EPX_INPUT_DEVICE_H__

#include "component.hh"
#include "screen_component.hh"
#include "input_device.hh"

#include "epx.h"

class CEpxInputDevice: public CInputDeviceBase {
public:
    XDERIVED_OBJECT_TYPE(CEpxInputDevice, CInputDeviceBase,
			 "EpxInputDevice", 
			 "Input device source",
			 (CEpxInputDevice_screen,
			  CEpxInputDevice_revents,
			  CEpxInputDevice_screenActive),

			 XFIELD(CEpxInputDevice,Q_PUBLIC, screen, 
				CEventType::create(CScreenComponent::CScreenComponentType::singleton(),E_INPUT),
				"Screen associated with mouse input."),

			 XFIELD(CEpxInputDevice,Q_PRIVATE, revents,
				input_signed_type(),
				"Input ready"),

			 XFIELD(CEpxInputDevice,Q_PRIVATE, screenActive,
				input_bool_type(),
				"Screen is active")
	);
public:
    CEpxInputDevice(CExecutor* aExec,
		     CBaseType *aType = CEpxInputDeviceType::singleton());
    ~CEpxInputDevice(void);

    void backend(CExecutor* aExec,epx_backend_t* aBackend);
    
    void execute(CExecutor* aExec);
private:
    CFileSource* mSource;      // File source for reading epx events
    epx_backend_t *mBackend;        // Backend passed from EpxScreen
    EventObject<CScreenComponent *> mScreen; // To extract backend from
    EventSigned mRevents;     // mSource connector
    EventBool mScreenActive; // Trigger from screen when graphics mode has been entered.
};


#endif
