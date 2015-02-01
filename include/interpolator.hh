//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __INTERPOLATOR_HH__
#define __INTERPOLATOR_HH__

#include "m1.hh"
#include "component.hh"


class CInterpolatorComponent: public CExecutable { // CBaseComponent {
public:
    XVIRTUAL_OBJECT_TYPE(CInterpolatorComponent, "Interpolator",
			 "Interpolator base component",
			 (CInterpolatorComponent_fraction,
			  CInterpolatorComponent_key),
			 XFIELD(CInterpolatorComponent,Q_PUBLIC,fraction,
				input_float_type(),
				""),
			 XFIELD(CInterpolatorComponent,Q_PUBLIC,key,
				CArrayType::create(float_type(),0),
				"")
	);
public:
    CInterpolatorComponent(CExecutor* aExec,CBaseType* aType);
    ~CInterpolatorComponent();
    
    //! Interpolate function
    virtual void interpolate(CExecutor* aExec, float t, int i, int j) {}

    void execute(CExecutor* aExec);

protected:
    EventFloat mFraction;
};

#endif
