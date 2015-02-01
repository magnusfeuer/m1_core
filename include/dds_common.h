//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __DDS_COMMON_H__
#define __DDS_COMMON_H__

typedef struct {
    u_int32_t  mVersion; 
    u_int32_t  mImageCount;
    u_int32_t  mHeight;
    u_int32_t  mWidth;
    u_int32_t  mPixelType;
} CDDSHeader;

#endif
