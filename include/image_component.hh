//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __IMAGE_COMPONENT_H__
#define __IMAGE_COMPONENT_H__

#include "component.hh"
#include <sys/types.h>

#include "epx.h"

//
// A Image file component.
//

class CImageComponent: public CLayerComponent {
public:
    XDERIVED_OBJECT_TYPE(CImageComponent,
			 CLayerComponent,
			 "Image",
			 "Image drawing component",
			 (CImageComponent_imageFile,
			  CImageComponent_pixelType,
			  CImageComponent_ignoreAlpha,
			  CImageComponent_borderWidth,
			  CImageComponent_borderColor),
			 XFIELD(CImageComponent,Q_PUBLIC,imageFile,
				input_string_type(),
				"Filename of the image file."),
			 XFIELD(CImageComponent,Q_PUBLIC,pixelType,
				input_string_type(),
				"Pixel type to use for the image"
			     ),
			 XFIELD(CImageComponent,Q_PUBLIC,ignoreAlpha,
				input_bool_type(),
				"Ignore alpha channels in image"
			     ),
			 XFIELD(CImageComponent,Q_PUBLIC,borderWidth,
				input_unsigned_type(),
				"Border width of image, 0 means no border"),
			 XFIELD(CImageComponent,Q_PUBLIC,borderColor,
				input_unsigned_type(),
				"Border color to use")
	);
public:
    CImageComponent(CExecutor* aExec, 
		    CBaseType *aType = CImageComponentType::singleton());
    ~CImageComponent(void);

    void redraw(CSystem* aSys, CRedrawContext *aContext);

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);

private:
    void loadImage(CExecutor* aExec, bool aStart);

    EventString   mImageFile;
    EventString   mPixelTypeString;  // config
    EventBool     mIgnoreAlpha;    // config - used with background 
    EventUnsigned mBorderWidth;
    EventUnsigned mBorderColor;

    epx_pixmap_t* mImage;   // current pixmap
    int mPixelType;    // Pixel type to use when loading image
    bool mUseAlpha;    // Image has some pixels with alpha info
};



#endif // __IMAGE_COMPONENT_H__
