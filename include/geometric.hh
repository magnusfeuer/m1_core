//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005.
//

#ifndef __GEOMETRIC_HH__
#define __GEOMETRIC_HH__

#include "m1.hh"

// Defintion of useful geomtrical objects

/////////////////////////////////////////////////////////////////////////////
//  Point object
/////////////////////////////////////////////////////////////////////////////

class CPoint : public CObject {
public:
    XBASE_TYPE(CPoint, "Point", 
	       "Geometrical 2D point",
	       (CPoint_x,
		CPoint_y),
	       XFIELD(CPoint,Q_PUBLIC,x,
		      float_type(),
		      "X coordinate"),
	       XFIELD(CPoint,Q_PUBLIC,y,
		      float_type(),
		      "Y coordinate")
	);
public:
    CPoint(CBaseType *)      { mX = 0.0; mY = 0.0; }
    CPoint(float aX, float aY) { mX = aX; mY = aY; }
    ~CPoint() {}

    CType* type() { return CPointType::singleton(); }
    CType* typeAt(int index) { return float_type(); }
    CType* typeAt(string aName) { return float_type(); }
    
    UData  at(int index) {
	UData r;
	if (index == XINDEX(CPoint,x)) r.f = mX;
	else if (index == XINDEX(CPoint,y)) r.f = mY;
	return r;
    }
    void put(CExecutor* aExec, int index, UData value, Trig_t trig) {
	if (index == XINDEX(CPoint,x)) mX = value.f;
	else if (index == XINDEX(CPoint,y)) mY = value.f;
    }
    void copy(CExecutor* aExec,CType* aType, int index, UData value, Trig_t trig) {
	put(aExec,index,value,trig); 
    }
    float x() { return mX; }
    float y() { return mY; }

private:
    float mX;
    float mY;
};


/////////////////////////////////////////////////////////////////////////////
//  Rect object
/////////////////////////////////////////////////////////////////////////////



class CRect : public CObject {
public:
    XBASE_TYPE(CRect, "Rect", 
	       "Rectangular object",
	       (CRect_x,
		CRect_y,
		CRect_width,
		CRect_height),
	       XFIELD(CRect,Q_PUBLIC,x,
		      float_type(),
		      "X coordinate"), 
	       XFIELD(CRect,Q_PUBLIC,y,
		      float_type(),
		      "Y coordinate"),
	       XFIELD(CRect,Q_PUBLIC,width,
		      float_type(),
		      "Width of rectangular area"),
	       XFIELD(CRect,Q_PUBLIC, height,
		      float_type(),
		      "Height of rectangular area.")
	);
public:
    CRect(CBaseType *)   { mX = 0.0; mY = 0.0; mWidth = 0.0; mHeight=0.0; }
    CRect(float aX, float aY, float aW, float aH) {
	mX = aX; mY = aY; mWidth = aW; mHeight = aH;
    }
    ~CRect() {}

    CType* type() { return CRectType::singleton(); }
    CType* typeAt(int index) { return float_type(); }
    CType* typeAt(string aFieldName) { return float_type(); }

    UData  at(int index) { 
	UData r;
	// FIXME!!!
	if (index == XINDEX(CRect,x)) r.f = mX;
	else if (index == XINDEX(CRect,y)) r.f = mY;
	else if (index == XINDEX(CRect,width)) r.f = mWidth;
	else if (index == XINDEX(CRect,height)) r.f = mHeight;
	return r;
    }

    void put(CExecutor* aExec, int index, UData value, Trig_t trig) {
	// FIXME!!!
	if (index == XINDEX(CRect,x)) mX = value.f;
	else if (index == XINDEX(CRect,y)) mY = value.f;
	else if (index == XINDEX(CRect,width)) mWidth = value.f;
	else if (index == XINDEX(CRect,height)) mHeight = value.f;
    }

    void copy(CExecutor* aExec, CType* aType, int index, UData value, Trig_t trig) { 
	put(aExec,index,value, trig); 
    }

    float x() { return mX; }
    float y() { return mY; }
    float width() { return mWidth; }
    float height() { return mHeight; }

    float top()    { return mY; }
    float left()   { return mX; }
    float bottom() { return mY+mHeight-1; }
    float right()  { return mX+mWidth-1; }

private:
    float mX;
    float mY;
    float mWidth;
    float mHeight;
};


#endif
