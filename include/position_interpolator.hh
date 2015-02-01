//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __POSITION_INTERPOLATOR_HH__
#define __POSITION_INTERPOLATOR_HH__

#include "geometric.hh"
#include "interpolator.hh"


class CPositionInterpolatorComponent : public CInterpolatorComponent
{
public:
    XDERIVED_OBJECT_TYPE(CPositionInterpolatorComponent,CInterpolatorComponent,
			 "PositionInterpolator",
			 "Position interpolator",
			 (CPositionInterpolatorComponent_xvalue,
			  CPositionInterpolatorComponent_yvalue,
			  CPositionInterpolatorComponent_keyValue),
			 XFIELD(CPositionInterpolatorComponent,Q_PUBLIC,xvalue,
				output_float_type(),
				""),
			 XFIELD(CPositionInterpolatorComponent,Q_PUBLIC,yvalue,
				output_float_type(),
				""),
			 XFIELD(CPositionInterpolatorComponent,Q_PUBLIC,keyValue,
				CArrayType::create(CPoint::CPointType::singleton(),
						   0),
				"")
	);
public:
    CPositionInterpolatorComponent(CExecutor* aExec,CBaseType *aType);
    ~CPositionInterpolatorComponent(void);

    void interpolate(CExecutor* aExec,float t, int i, int j);

private:
    EventFloat mXValue;
    EventFloat mYValue;
    int mKeyValueIndex;
};

#endif
