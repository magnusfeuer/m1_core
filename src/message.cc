//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//
#include "m1.hh"
#include "message.hh"

#include "epic.h"
#include <math.h> // for nearbyintf() rounding function. Link with -m.

XOBJECT_TYPE_BOOTSTRAP(CMessage);

CMessage::CMessage(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType)
{
}

CMessage::~CMessage(void)
{
}

