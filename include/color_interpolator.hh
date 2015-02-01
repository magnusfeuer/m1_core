//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __COLOR_INTERPOLATOR_HH__
#define __COLOR_INTERPOLATOR_HH__

#include "interpolator.hh"


class CColorInterpolatorComponent : public CInterpolatorComponent
{
public:
    XDERIVED_OBJECT_TYPE(CColorInterpolatorComponent,CInterpolatorComponent,
			 "ColorInterpolator",
			 "Color interpolator",
			 (CColorInterpolatorComponent_value,
			  CColorInterpolatorComponent_keyValue),
			 XFIELD(CColorInterpolatorComponent,Q_PUBLIC,value,
				output_unsigned_type(),
				""),
			 XFIELD(CColorInterpolatorComponent,Q_PUBLIC,keyValue,
				CArrayType::create(unsigned_type(),0),
				"")
	);
public:
    CColorInterpolatorComponent(CExecutor* aExec,CBaseType *aType);
    ~CColorInterpolatorComponent(void);

    void interpolate(CExecutor* aExec,float t, int i, int j);

private:
    EventUnsigned mValue;
    int mKeyValueIndex;
};

#endif
