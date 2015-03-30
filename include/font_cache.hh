//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007
//

#ifndef __FONT_MANAGER_HH__
#define __FONT_MANAGER_HH__

#include <math.h>
#include "m1.hh"
#include "epx.h"

#define PT_INCH  0.013889  // Points to inch
#define PT_MM    0.352781  // Points to mm
#define MM_INCH  0.039370  // mm to inch


class CFont : public CRuntime {
public:
    CFont(epx_font_t* aFont) {
	mLastUsed = 0;
	mPermanent = false;
	mFont = aFont;
    }
    ~CFont(void) { epx_font_destroy(mFont);  }

    string debugName(void);

    // permament to make sure the font is not unloaded
    void permanent() { 
	if (!mPermanent) {
	    m1Retain(CFont, this); 
	    mPermanent = true; 
	}
    }

    // transient allow font to be unloaded when not referenced
    void transient() {
	if (mPermanent) {
	    m1Release(CFont, this); 
	    mPermanent = false; 
	}
    }

    //! unmap font file (temporarily)
    void unmap() { if (mFont && epx_font_is_mapped(mFont)) epx_font_unmap(mFont); }

    //! (re)map an unmapped file
    void map()   { if (mFont && !epx_font_is_mapped(mFont)) epx_font_map(mFont); }

    //! mark that the font is in use
    void use(TimeStamp aTimeStamp) { map(); mLastUsed = aTimeStamp; }

    epx_font_t* epxFont(void) { return mFont; }

    //! Match a font
    bool match(int aResolution, string aName,int aSize,
	       epx_font_weight_t aWeight, epx_font_slant_t aSlant);

private:
    TimeStamp mLastUsed;   // Mark time when font entry was last used
    bool   mPermanent;    // Make sure it's not onloaded
    epx_font_t* mFont;         // The EPX font structure 
};

typedef list<CFont*> CFontList;

//
// A Font Dictionary Cache
// Keep's track on all open Font Files.
// It allows search for:
//   Font file/name/size/slant
//
class CFontManager : public CRuntime {
public:
    CFontManager(void) { }

    ~CFontManager(void) { clear(); }

    int mark(Mark_t aMark);
    string debugName(void);

    void appendPath(string path) {
	mFontPath.remove(path);     // remove current instances
	mFontPath.push_back(path);  // add last
    }

    void insertPath(string path) {
	mFontPath.remove(path);     // remove current instances
	mFontPath.push_front(path);  // add first
    }

    void removePath(string path) {
	mFontPath.remove(path);     // remove current instances
    }
    
    // Load all fonts found in the directory paths
    void load(void);

    // UnLoad all fonts
    void clear(void);

    // Find first matching font
    CFont* match(int aResolution,string aName,int aSize,
		 epx_font_weight_t aWeight, epx_font_slant_t aSlant);

private:
    void loadFonts(string aPath);

    list<string> mFontPath;
    CFontList mFontList;
};

extern CFontManager& m1_fonts();

#endif
