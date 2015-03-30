// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#include <stdio.h>
#include <iostream>

#include "m1.hh"
#include "epx.h"
#include "epx_input_device.hh"

using namespace std;

XOBJECT_TYPE_BOOTSTRAP(CEpxInputDevice);

CEpxInputDevice::CEpxInputDevice(CExecutor* aExec, CBaseType *aType) :
    CInputDeviceBase(aExec, aType),
    mSource(NULL),
    mBackend(NULL),
    mScreen(this),
    mRevents(this),
    mScreenActive(this)
{
    mScreen.putValue(aExec, NULL);
    mScreenActive.putValue(aExec, false);

    eventPut(aExec,XINDEX(CEpxInputDevice,screen), &mScreen);
    eventPut(aExec,XINDEX(CEpxInputDevice,screenActive), &mScreenActive);
    eventPut(aExec,XINDEX(CEpxInputDevice,revents), &mRevents);

    // XLib eat events so poll method alone will not work, check pending
    setFlags(ExecuteEverySweep);

    mSource = m1New(CFileSource, aExec);
    m1Retain(CFileSource, mSource);
    connect(XINDEX(CEpxInputDevice,revents), 
	    mSource, XINDEX(CFileSource,revents));
}

CEpxInputDevice::~CEpxInputDevice(void)
{
    if (mBackend != NULL)
	epx_backend_event_detach(mBackend);
    disconnect(XINDEX(CEpxInputDevice,revents));
    m1Release(CFileSource, mSource);
}

void CEpxInputDevice::backend(CExecutor* aExec, epx_backend_t* aBackend)
{
    DBGFMT("CEpxInputDevice::backend(%p)", aBackend);
    if (mBackend != NULL)
	epx_backend_event_detach(mBackend);

    if (aBackend != NULL) {
	int fd = (int) epx_backend_event_attach(aBackend);
	DBGFMT("CEpxInputDevice::fd = %d", fd);
	mSource->setDescriptor(aExec, fd, POLLIN);
    }

    mBackend = aBackend;
}

void CEpxInputDevice::execute(CExecutor* aExec)
{
    Time tTime = m1_TimeStampToTime(aExec->cycleTime());
    epx_event_t e;

    if (mScreen.updated()) {
	connect(XINDEX(CEpxInputDevice,screenActive),
		mScreen.value(),XINDEX(CLayerComponent,active));
	return;
    }

    if (!mBackend && mScreen.value() && mScreen.value()->backend()) {
	backend(aExec, mScreen.value()->backend());
	return;
    }

    if (mBackend == NULL)
	return;

    if (!mRevents.updated() && (mBackend->pending == 0))
	return;


#warning "TONY FIXME: EpxBackendEventRead took event mask, but epx_backend_evnt_read does not"
    while(epx_backend_event_read(mBackend, &e
				 // , 
				 // EPX_EVENT_BUTTON_PRESS|
				 // EPX_EVENT_BUTTON_RELEASE|
				 // EPX_EVENT_KEY_PRESS | 
				 // EPX_EVENT_KEY_RELEASE |
				 // EPX_EVENT_POINTER_MOTION
	      ) > 0) {

	switch(e.type) {
	    case EPX_EVENT_POINTER_MOTION:
		rawAbsoluteX(aExec, e.pointer.x);
		rawAbsoluteY(aExec, e.pointer.y);
		break;

	    case EPX_EVENT_BUTTON_PRESS:
		DBGFMT("CEpxInputDevice: press %d\n", e.pointer.button);

		if (e.pointer.button & EPX_BUTTON_LEFT)
		    buttonDown(aExec, 1, 1);

		if (e.pointer.button & EPX_BUTTON_MIDDLE)
		    buttonDown(aExec, 2, 1);

		if (e.pointer.button & EPX_BUTTON_RIGHT)
		    buttonDown(aExec, 3, 1);

		break;

	    case EPX_EVENT_BUTTON_RELEASE:
		DBGFMT("CEpxInputDevice: release %d\n", e.pointer.button);

		if (e.pointer.button & EPX_BUTTON_LEFT)
		    buttonUp(aExec, 1);

		if (e.pointer.button & EPX_BUTTON_MIDDLE)
		    buttonUp(aExec, 2);

		if (e.pointer.button & EPX_BUTTON_RIGHT)
		    buttonUp(aExec, 3);
		break;

	    case EPX_EVENT_KEY_PRESS:
		DBGFMT("CEpxInputDevice: press %d", e.key.sym);
		keyDown(aExec, e.key.sym, tTime);
		break;

	    case EPX_EVENT_KEY_RELEASE:
		DBGFMT("CEpxInputDevice: release %d", e.key.sym);
		keyUp(aExec, e.key.sym);
		break;

	    default:
		DBGFMT("CEpxInputDevice: Got unwanted event %d", e.type);
		break;
	}
    }
}


