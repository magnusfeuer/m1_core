//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __SCALAR_INTERPOLATOR_HH__
#define __SCALAR_INTERPOLATOR_HH__

#include "interpolator.hh"


class CScalarInterpolatorComponent : public CInterpolatorComponent
{
public:
    XDERIVED_OBJECT_TYPE(CScalarInterpolatorComponent,CInterpolatorComponent,
			 "ScalarInterpolator",
			 "Scalar interpolator",
			 (CScalarInterpolatorComponent_value,
			  CScalarInterpolatorComponent_keyValue),
			 XFIELD(CScalarInterpolatorComponent,Q_PUBLIC,value,
				output_float_type(),
				""),
			 XFIELD(CScalarInterpolatorComponent,Q_PUBLIC,keyValue,
				CArrayType::create(float_type(),0),
				"")
	);
public:
    CScalarInterpolatorComponent(CExecutor* aExec, CBaseType *aType);
    ~CScalarInterpolatorComponent(void);

    void interpolate(CExecutor* aExec,float t, int i, int j);

private:
    EventFloat mValue;
};

#endif
