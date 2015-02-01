// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#include <stdio.h>
#include <iostream>

#include "m1.hh"
#include "epic.h"
#include "epic_input_device.hh"

using namespace std;

XOBJECT_TYPE_BOOTSTRAP(CEpicInputDevice);

CEpicInputDevice::CEpicInputDevice(CExecutor* aExec, CBaseType *aType) :
    CInputDeviceBase(aExec, aType),
    mSource(NULL),
    mBackend(NULL),
    mScreen(this),
    mRevents(this),
    mScreenActive(this)
{
    mScreen.putValue(aExec, NULL);
    mScreenActive.putValue(aExec, false);

    eventPut(aExec,XINDEX(CEpicInputDevice,screen), &mScreen);
    eventPut(aExec,XINDEX(CEpicInputDevice,screenActive), &mScreenActive);
    eventPut(aExec,XINDEX(CEpicInputDevice,revents), &mRevents);

    // XLib eat events so poll method alone will not work, check pending
    setFlags(ExecuteEverySweep);

    mSource = m1New(CFileSource, aExec);
    m1Retain(CFileSource, mSource);
    connect(XINDEX(CEpicInputDevice,revents), 
	    mSource, XINDEX(CFileSource,revents));
}

CEpicInputDevice::~CEpicInputDevice(void)
{
    if (mBackend != NULL)
	EBackendEventDetach(mBackend);
    disconnect(XINDEX(CEpicInputDevice,revents));
    m1Release(CFileSource, mSource);
}

void CEpicInputDevice::backend(CExecutor* aExec, EBackend* aBackend)
{
    DBGFMT("CEpicInputDevice::backend(%p)", aBackend);
    if (mBackend != NULL)
	EBackendEventDetach(mBackend);

    if (aBackend != NULL) {
	int fd = (int) EBackendEventAttach(aBackend);
	DBGFMT("CEpicInputDevice::fd = %d", fd);
	mSource->setDescriptor(aExec, fd, POLLIN);
    }

    mBackend = aBackend;
}

void CEpicInputDevice::execute(CExecutor* aExec)
{
    Time tTime = m1_TimeStampToTime(aExec->cycleTime());
    EEvent e;

    if (mScreen.updated()) {
	connect(XINDEX(CEpicInputDevice,screenActive),
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


    while(EBackendEventRead(mBackend, &e, 
			    EEVENT_BUTTON_PRESS|
			    EEVENT_BUTTON_RELEASE|
			    EEVENT_KEY_PRESS | 
			    EEVENT_KEY_RELEASE |
			    EEVENT_POINTER_MOTION) > 0) {

	switch(e.type) {
	    case EEVENT_POINTER_MOTION:
		rawAbsoluteX(aExec, e.pointer.x);
		rawAbsoluteY(aExec, e.pointer.y);
		break;

	    case EEVENT_BUTTON_PRESS:
		DBGFMT("CEpicInputDevice: press %d\n", e.pointer.button);

		if (e.pointer.button & EBUT_LEFT)
		    buttonDown(aExec, 1, 1);

		if (e.pointer.button & EBUT_MIDDLE)
		    buttonDown(aExec, 2, 1);

		if (e.pointer.button & EBUT_RIGHT)
		    buttonDown(aExec, 3, 1);

		break;

	    case EEVENT_BUTTON_RELEASE:
		DBGFMT("CEpicInputDevice: release %d\n", e.pointer.button);

		if (e.pointer.button & EBUT_LEFT)
		    buttonUp(aExec, 1);

		if (e.pointer.button & EBUT_MIDDLE)
		    buttonUp(aExec, 2);

		if (e.pointer.button & EBUT_RIGHT)
		    buttonUp(aExec, 3);
		break;

	    case EEVENT_KEY_PRESS:
		DBGFMT("CEpicInputDevice: press %d", e.key.sym);
		keyDown(aExec, e.key.sym, tTime);
		break;

	    case EEVENT_KEY_RELEASE:
		DBGFMT("CEpicInputDevice: release %d", e.key.sym);
		keyUp(aExec, e.key.sym);
		break;

	    default:
		DBGFMT("CEpicInputDevice: Got unwanted event %d", e.type);
		break;
	}
    }
}


