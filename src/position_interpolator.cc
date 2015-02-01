//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//


#include "position_interpolator.hh"

XOBJECT_TYPE_BOOTSTRAP(CPositionInterpolatorComponent);

CPositionInterpolatorComponent::CPositionInterpolatorComponent(CExecutor* aExec,
							       CBaseType *aType) :
    CInterpolatorComponent(aExec,aType),
    mXValue(this),
    mYValue(this)
{
    eventPut(aExec,XINDEX(CPositionInterpolatorComponent,xvalue), &mXValue);
    eventPut(aExec,XINDEX(CPositionInterpolatorComponent,yvalue), &mYValue);
}

CPositionInterpolatorComponent::~CPositionInterpolatorComponent(void) 
{
    m1ReleaseArray(at(XINDEX(CPositionInterpolatorComponent,keyValue)).arr);
}

void CPositionInterpolatorComponent::interpolate(CExecutor* aExec,float t, int i, int j)
{
    CArray* keyValue = at(XINDEX(CPositionInterpolatorComponent,keyValue)).arr;
    CPoint* c0;
    CPoint* c1;
    float x, y;

    // Provisioning check.
    if (!keyValue || keyValue->size() == 0)
	return;

    c0 = (CPoint*) keyValue->at(i).o;
    c1 = (CPoint*) keyValue->at(j).o;

    x = c0->x() + (c1->x() - c0->x())*t;
    y = c0->y() + (c1->y() - c0->y())*t;

    mXValue.putValue(aExec, x);
    mYValue.putValue(aExec, y);
}
