//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __EPIC_INPUT_DEVICE_H__
#define __EPIC_INPUT_DEVICE_H__

#include "component.hh"
#include "screen_component.hh"
#include "input_device.hh"

#include "epic.h"

class CEpicInputDevice: public CInputDeviceBase {
public:
    XDERIVED_OBJECT_TYPE(CEpicInputDevice, CInputDeviceBase,
			 "EpicInputDevice", 
			 "Input device source",
			 (CEpicInputDevice_screen,
			  CEpicInputDevice_revents,
			  CEpicInputDevice_screenActive),

			 XFIELD(CEpicInputDevice,Q_PUBLIC, screen, 
				CEventType::create(CScreenComponent::CScreenComponentType::singleton(),E_INPUT),
				"Screen associated with mouse input."),

			 XFIELD(CEpicInputDevice,Q_PRIVATE, revents,
				input_signed_type(),
				"Input ready"),

			 XFIELD(CEpicInputDevice,Q_PRIVATE, screenActive,
				input_bool_type(),
				"Screen is active")
	);
public:
    CEpicInputDevice(CExecutor* aExec,
		     CBaseType *aType = CEpicInputDeviceType::singleton());
    ~CEpicInputDevice(void);

    void backend(CExecutor* aExec,EBackend* aBackend);
    
    void execute(CExecutor* aExec);
private:
    CFileSource* mSource;      // File source for reading epic events
    EBackend *mBackend;        // Backend passed from EpicScreen
    EventObject<CScreenComponent *> mScreen; // To extract backend from
    EventSigned mRevents;     // mSource connector
    EventBool mScreenActive; // Trigger from screen when graphics mode has been entered.
};


#endif
