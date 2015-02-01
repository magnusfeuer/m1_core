// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//

#include "m1.hh"
#include "geometric.hh"
#include "component.hh"
#include "scalar_interpolator.hh"
#include "color_interpolator.hh"
#include "position_interpolator.hh"

XOBJECT_TYPE_BOOTSTRAP(CInterpolatorComponent);

CInterpolatorComponent::CInterpolatorComponent(CExecutor* aExec,CBaseType* aType) :
    CExecutable(aExec,aType),
    mFraction(this)
{
    setFlags(ExecuteOnEventUpdate);
    eventPut(aExec, XINDEX(CInterpolatorComponent,fraction), &mFraction);
}

CInterpolatorComponent::~CInterpolatorComponent() 
{
    m1ReleaseArray(at(XINDEX(CInterpolatorComponent,key)).arr);
}


void CInterpolatorComponent::execute(CExecutor* aExec)
{
    CArray* key;
    float t;
    size_t sz;
    float t0;
    float t1;

    // Validate update and provisioning.
    if (!mFraction.updated() ||
	!(key = at(XINDEX(CInterpolatorComponent, key)).arr) ||
	((sz = key->size()) == 0))
	return;

    t = mFraction.value();
    t0 = 0.0;
    t1 = key->at(0).f;
	
//	DBGFMT("CInterpolatorComponent::execute(): fraction[%f]", t);
    if (t <= t1)
	interpolate(aExec, 0.0, 0, 0);
    else if (t >= key->at(sz-1).f)
	interpolate(aExec, 0.0, sz-1, sz-1);
    else {
	int i = 0;
	// Do this with bsearch!
	while ((i < (int)sz) && (t > t1)) {
	    t0 = t1;
	    i++;
	    t1 = key->at(i).f;
	}
	if (i == (int)sz)
	    interpolate(aExec, 0.0, sz-1, sz-1);
	else {
	    // scale interpolation point t' into [0.0 .. 1.0]
	    // so that value will be calculate as v(i) + (v(i+1)-v(i))*t
	    // for each pair of values
	    interpolate(aExec, 1-(t1-t)/(t1-t0), i-1, i);
	}
    }
}

