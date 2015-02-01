//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#include "m1.hh"

XOBJECT_TYPE_BOOTSTRAP(CFileSource);


CFileSource::CFileSource(CExecutor* aExec, CBaseType *aType): 
    CExecutable(aExec, aType),
    mRevents(this),
    mDescriptor(-1), 
    mEvents(POLLIN) 
{
    eventPut(aExec, "revents", &mRevents);
}

CFileSource::~CFileSource(void) 
{
}	

void CFileSource::ready(short aRevents)
{
    mRevents.putValue(NULL, aRevents);
}

void CFileSource::stop(CRtExecutor* aExec)
{
    // This is done in stop since we need the Executor to 
    // delete the poll descriptor!
    if (mDescriptor != -1)
	((CExecutor*)aExec)->deletePollDescriptor(mDescriptor);
    CExecutable::stop(aExec);
}

void CFileSource::setDescriptor(CExecutor* aExec, int aDescpriptor, short aEvents) 
{
    if (mDescriptor != -1)
	aExec->deletePollDescriptor(mDescriptor);

    mDescriptor = aDescpriptor; 
    mEvents = aEvents;
    if (mDescriptor != -1)
	aExec->addPollDescriptor(mDescriptor, mEvents, this);
}

void CFileSource::execute(CExecutor* aExec)
{
}

void CFileSource::post_execute(CExecutor* aExec)
{
}
