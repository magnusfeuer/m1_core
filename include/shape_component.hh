//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __SHAPE_COMPONENT_H__
#define __SHAPE_COMPONENT_H__

#include "component.hh"
#include <sys/types.h>

#include "epic.h"

typedef enum {
    SHAPE_NONE = 'n',
    SHAPE_RECTANGLE = 'r',
    SHAPE_ELLIPSE = 'e',
    SHAPE_TRIANGLE = 't'
} ShapeForm;


ENUM_TYPE(CShapeForm, "ShapeForm",
	  ENUMERATION(none,       SHAPE_NONE),
	  ENUMERATION(rectangle,  SHAPE_RECTANGLE),
	  ENUMERATION(ellipse,    SHAPE_ELLIPSE),
	  ENUMERATION(triangle,   SHAPE_TRIANGLE)
    );
	    
//
// A Shape component.
//
class CShapeComponent: public CLayerComponent {
public:
    XDERIVED_OBJECT_TYPE(CShapeComponent,
			 CLayerComponent,
			 "Shape",
			 "Shape drawing component",
			 (CShapeComponent_shape,
			  CShapeComponent_fill,
			  CShapeComponent_fillColor,
			  CShapeComponent_borderWidth,
			  CShapeComponent_borderColor),
			 XFIELD(CShapeComponent,Q_PUBLIC,shape,
				CEventType::create(CShapeFormType::singleton(),E_INPUT),
				"Selects the type of shape"),
			 XFIELD(CShapeComponent,Q_PUBLIC,fill,
				input_bool_type(),
				"Fill the interior if the shape"),
			 XFIELD(CShapeComponent,Q_PUBLIC,fillColor,
				input_unsigned_type(),
				"Color to fill the interior with"),
			 XFIELD(CShapeComponent,Q_PUBLIC,borderWidth,
				input_unsigned_type(),
				"Border width of shape, 0 means no border"),
			 XFIELD(CShapeComponent,Q_PUBLIC,borderColor,
				input_unsigned_type(),
				"Border color to use"
			     )
	);
public:
    CShapeComponent(CExecutor* aExec,
		    CBaseType *aType = CShapeComponentType::singleton());
    ~CShapeComponent(void);

    void redraw(CSystem* aSys, CRedrawContext *aContext);

    void execute(CExecutor* aExec);

private:
    EventSigned mShape;
    EventBool mFill;
    EventUnsigned mFillColor;
    EventUnsigned mBorderWidth;
    EventUnsigned mBorderColor;
};



#endif // __SHAPE_COMPONENT_H__
