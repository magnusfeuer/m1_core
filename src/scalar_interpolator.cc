//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//


#include "scalar_interpolator.hh"

XOBJECT_TYPE_BOOTSTRAP(CScalarInterpolatorComponent);

CScalarInterpolatorComponent::CScalarInterpolatorComponent(CExecutor* aExec,
							   CBaseType *aType) :
    CInterpolatorComponent(aExec, aType),
    mValue(this)
{
    eventPut(aExec, XINDEX(CScalarInterpolatorComponent,value), &mValue);
}

CScalarInterpolatorComponent::~CScalarInterpolatorComponent(void) 
{
    m1ReleaseArray(at(XINDEX(CScalarInterpolatorComponent,keyValue)).arr);
}


void CScalarInterpolatorComponent::interpolate(CExecutor* aExec, float t, int i, int j)
{
    CArray* keyValue = at(XINDEX(CScalarInterpolatorComponent,keyValue)).arr;
    UData v0;
    UData v1;

    // Provisioning check
    if (!keyValue || keyValue->size() == 0)
	return;

    // Avoid out of bounds crashes
    if (i < 0 || j < 0)  {
	mValue.putValue(aExec, v0.f);
	return;
    }

    if (j >= (int) keyValue->size() || i >= (int) keyValue->size()) {
	mValue.putValue(aExec, v1.f);
	return;
    }
	
    v0 = keyValue->at(i);
    v1 = keyValue->at(j);
    mValue.putValue(aExec, v0.f + (v1.f - v0.f)*t);
}
