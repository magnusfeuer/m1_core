//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#include <sys/types.h>
#include <dirent.h>
#include "font_cache.hh"


CFontManager& m1_fonts()
{
    static CFontManager* fonts = NULL;
    if (!fonts) {
	fonts = new CFontManager();
	m1Retain(CFontManager, fonts);
    }
    return *fonts;
}


bool CFont::match(int aResolution, string aName,int aSize,
		  EFontWeight aWeight,EFontSlant aSlant)
{
    if ((aName != "") && (strcmp(aName.c_str(), mFont->family_name) != 0))
	return false;
    if (aSize > 0) {
	float scale = (float)mFont->font_info.resolution_y/(float)aResolution;
	int   fsize = (int)round(scale*(mFont->font_info.point_size / 10.0));
	DBGFMT("Test %s point size=%d, scaled point size=%d",
	       mFont->family_name,
	       (int)round(mFont->font_info.point_size / 10.0),
	       fsize);
	if (aSize != fsize)
	    return false;
    }
    if ((aWeight != 0) && (aWeight != (int) mFont->font_info.weight))
	return false;
    if ((aSlant != 0) && (aSlant != (int) mFont->font_info.slant))
	return false;
    return true;
}

string CFont::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "Font: %s #%lu 0x%lx", 
	    mFont ? mFont->family_name : "", refCount(), (unsigned long) this);
    return string(ndata);
}
//
// Load all efnt files found in driectory
//
void CFontManager::loadFonts(string aPath)
{
    DIR* dir;
    struct dirent* dp;

    if ((dir = opendir(aPath.c_str())) == NULL) {
	fprintf(stderr, "CFontManager: unable to oped dir %s\n",
		aPath.c_str());
	return;
    }

    while((dp = readdir(dir)) != NULL) {
	char* fname = dp->d_name;
	char* sfx = strrchr(fname, '.');
	if (sfx && strcmp(sfx, ".efnt") == 0) {
	    EFont* font;
	    string filename = aPath + "/" + string(fname);

	    if ((font = EFontOpen((char*)filename.c_str())) != NULL) {
		CFont* cfnt = new CFont(font);
		mFontList.push_back(cfnt);
		m1Retain(CFont, cfnt);
		DBGFMT("Loaded font [%s] size=%d, pxsize=%d,res=%dx%d, weight=%d, slant=%d\n",
			font->family_name,
			(int) round(font->font_info.point_size / 10.0),
			font->font_info.pixel_size,
			font->font_info.resolution_x,
			font->font_info.resolution_y,
			font->font_info.weight,
			font->font_info.slant);
	    }
	    else {
		DBGFMT_WARN("Could not load font file %s", filename.c_str());
	    }
	}
    }

    closedir(dir);
}

void CFontManager::load(void) 
{
    list<string>::iterator iter;
    
    for (iter = mFontPath.begin(); iter != mFontPath.end(); iter++)
	loadFonts(*iter);
}

void CFontManager::clear(void) 
{
    CFontList::iterator iter;

    for (iter = mFontList.begin(); iter != mFontList.end(); iter++) {
	(*iter)->transient();
	m1Release(CFont, *iter);
    }
    mFontList.clear();
}

CFont* CFontManager::match(int aResolution, string aName,int aSize,
			 EFontWeight aWeight,EFontSlant aSlant) 
{
    CFontList::iterator iter;
    for (iter = mFontList.begin(); iter != mFontList.end(); iter++) {
	if ((*iter)->match(aResolution, aName, aSize, aWeight, aSlant))
	    return *iter;
    }
    return NULL;
}

int CFontManager::mark(Mark_t aMark) 
{
    int marked = 0;
    if (mMark != aMark) {
	CFontList::iterator iter;

	marked += CRuntime::mark(aMark);
	for (iter = mFontList.begin(); iter != mFontList.end(); iter++)
	    marked += (*iter)->mark(aMark);
    }
    return marked;
}


string CFontManager::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "FontManager: #%lu 0x%lx", 
	    refCount(), (unsigned long) this);
    return string(ndata);
}
