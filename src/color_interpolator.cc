//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//


#include "color_interpolator.hh"

XOBJECT_TYPE_BOOTSTRAP(CColorInterpolatorComponent);

CColorInterpolatorComponent::CColorInterpolatorComponent(CExecutor* aExec,
							 CBaseType *aType) :
    CInterpolatorComponent(aExec,aType),
    mValue(this)
{
    eventPut(aExec,XINDEX(CColorInterpolatorComponent,value), &mValue);
}

CColorInterpolatorComponent::~CColorInterpolatorComponent(void) 
{
    m1ReleaseArray(at(XINDEX(CColorInterpolatorComponent,keyValue)).arr);
}

void CColorInterpolatorComponent::interpolate(CExecutor* aExec,float t, int i, int j)
{
    CArray* keyValue;
    epx_pixel_t c0;
    epx_pixel_t c1;

    
    keyValue = at(XINDEX(CColorInterpolatorComponent,keyValue)).arr;

    // Check that we are configured.
    if (!keyValue || keyValue->size() == 0)
	return;

    c0.px = keyValue->at(i).u;
    c1.px = keyValue->at(j).u;

    DBGFMT("ColorInterpolator: t=%f, i=%d, j=%d\n", t, i, j);

    c0.a = u_int8_t(c0.a + (c1.a - c0.a)*t);
    c0.r = u_int8_t(c0.r + (c1.r - c0.r)*t);
    c0.g = u_int8_t(c0.g + (c1.g - c0.g)*t);
    c0.b = u_int8_t(c0.b + (c1.b - c0.b)*t);
    mValue.putValue(aExec, c0.px);
}
