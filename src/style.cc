//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006.
//

#include "style.hh"
#include "screen_component.hh"

XOBJECT_TYPE_BOOTSTRAP(CStyle);

// Create "default" styles
static CStyle* create_style(CExecutor* aExec, string aName)
{
    CStyle* style;

    style = m1New(CStyle,aExec,CStyle::CStyleType::singleton());
    if (aName != "")
	style->put(aExec,"name",  UString(m1New(CString, aName)));
    style->put(aExec,"fontName",  UString(m1New(CString, "Helvetica")));
    style->put(aExec,"fontSize",  UUnsigned(12));
    style->start(aExec);
    return style;    
}


// Create a "default" style
CStyle* m1_default_style(void)
{
    static CStyle* style = NULL;
    if (!style)
	style = create_style(m1_system().executor(), "default");
    return style;
}

// Create the global style manager object
CStyleManager& m1_styles(void)
{
    static CStyleManager* styles = NULL;
    if (!styles) {
	styles = new CStyleManager();
	m1Retain(CStyleManager, styles);
	// make sure default style is loaded, this will lead to a 
	// recursive call to m1_styles so make sure static styles is set !
	m1_default_style();
    }
    return *styles;
}

//
//
//
CStyleItem::CStyleItem(string aName, CStyle* aStyle) : 
    mStyle(NULL) 
{
    mName = aName;
    mStyle.putValue(NULL, aStyle);
}

// Wow copy constuctors really are handy ;-)
CStyleItem::CStyleItem(const CStyleItem &aItem) :
    mName(aItem.mName),
    mStyle(NULL)
{
    // mStyle.putValue(NULL, aItem.mStyle.value());
}

CStyleItem::~CStyleItem(void) 
{
    // Nothing right now
}

int CStyleItem::mark(Mark_t aMark)
{
    return mStyle.mark(aMark);
}

void CStyleItem::setStyle(CExecutor* aExec, CStyle* aStyle)
{
    mStyle.putValue(aExec, aStyle);
    // Since style items are not executables this is a bit special
    // so propagate manually.
    mStyle.propagate(aExec);  
}

int CStyleManager::mark(Mark_t aMark)
{
    int marked = 0;
    if (mMark != aMark) {
	CStyleList::iterator iter;
	marked += CRuntime::mark(aMark);
	iter = mStyleList.begin();
	while(iter != mStyleList.end()) {
	    marked += iter->mark(aMark);
	    iter++;
	}
    }
    return marked;
}
//
// Find style by name
//
CStyleList::iterator CStyleManager::findStyle(string name)
{
    CStyleList::iterator iter = mStyleList.begin();
    while(iter != mStyleList.end()) {
	if (iter->name() == name)
	    return iter;
	iter++;
    }
    return iter;
}

//
// Find style by style pointer
//
CStyleList::iterator CStyleManager::findStyle(CStyle* style)
{
    CStyleList::iterator iter = mStyleList.begin();
    while(iter != mStyleList.end()) {
	if (iter->style() == style)
	    return iter;
	iter++;
    }
    return iter;
}
//
// Get a style by name
//
CStyle* CStyleManager::lookupStyle(string name)
{
    CStyleList::iterator iter = findStyle(name);
    if (iter == mStyleList.end())
	return NULL;
    return
	iter->style();
}

//
// Create a style or find an existing style
// returns a pointer to the Event field that handles the style
//

EventObject<CStyle*>* CStyleManager::createStyle(CExecutor* aExec, string aName)
{
    CStyleList::iterator iter;

    if ((iter = findStyle(aName)) == mStyleList.end()) {
	create_style(aExec, aName);  // will auto install
	if ((iter = findStyle(aName)) == mStyleList.end()) {
	    ERRFMT("Installed style '%s' was not found", aName.c_str());
	    return NULL;
	}
    }
    return iter->styleEvent();
}

bool CStyleManager::member(CStyle* aStyle)
{
    CStyleList::iterator iter = findStyle(aStyle);
    return iter != mStyleList.end();
}

bool CStyleManager::unregisterStyle(CExecutor* aExec, string name)
{
    CStyleList::iterator iter = findStyle(name);
    if (iter != mStyleList.end()) {
	fprintf(stderr, "style %s removed", iter->name().c_str());
	mStyleList.erase(iter);
	return true;
    }
    return false;
}

bool CStyleManager::unregisterStyle(CExecutor* aExec, CStyle* style)
{
    CStyleList::iterator iter = findStyle(style);
    if (iter != mStyleList.end()) {
	fprintf(stderr, "style %s cleared", iter->name().c_str());
	iter->setStyle(aExec, NULL);
	return true;
    }
    return false;
}

bool CStyleManager::registerStyle(CExecutor* aExec, CStyle* style)
{
    CStyleList::iterator iter = findStyle(style->name());
    if (iter != mStyleList.end()) {
	iter->setStyle(aExec, style);  // change style
	printf("style %s updated\n", style->name().c_str());
    }
    else {
	mStyleList.push_back(CStyleItem(style->name(), style));
	printf("style %s registerd\n", style->name().c_str());
    }
    return true;
}


static int resolution(CScreenComponent* aScreen)
{
    if (aScreen)
	return aScreen->resolution_y();
    else
	return 75;
}

///////////////////////////////////////////////////////////////////////////////
//
// The Style Link
//
///////////////////////////////////////////////////////////////////////////////
CStyleLink::CStyleLink(CExecutable* aOwner, CExecutor* aExec) :
    mStyle(aOwner),
    mStyleUpdated(aOwner),
    mClass(aOwner)
{
    mStyle.putValue(aExec,NULL);
    mClass.putValue(aExec, "");
    // We can not use XINDEX here since we do not know how is using the
    // style link, but I think we should be able to improve this a bit.
    aOwner->eventPut(aExec, "style",      &mStyle);
    aOwner->eventPut(aExec, "class",      &mClass);
}

CStyleLink::~CStyleLink()
{
    /* Nothing yet */
}

bool CStyleLink::update(CExecutor* aExec, bool aStart)
{
    bool update = false;

    if (mStyle.assigned()) {
	CStyle* style = mStyle.value();     // new style value
	if (aStart) mStyle.cancel(aExec);
	mStyleUpdated.deleteFromSource();  // Remove from old style (if any)
	if (style != NULL) {
	    CEvent* styleEvt = style->structureChangedEvent();
	    if (styleEvt)
		styleEvt->addSubscriber(&mStyleUpdated);
	}
	else {
	    // FIXME: maybe switch back to class attribute if 
	    //  someone sets style=nil ? 
	}
	// fprintf(stderr, "StyleLink::mStyle was updated\n");
	update = true;
    }

    if (mClass.assigned()) {
	if (mClass.value() != "") {
	    EventObject<CStyle*>* styleEvt = 
		m1_styles().createStyle(aExec, mClass.value());
	    CStyle* style;

	    // Avoid multi trigger
	    if (aStart) mClass.cancel(aExec);

	    // Subscribe on updates to the style class name
	    styleEvt->addSubscriber(&mStyle);

	    // Subscribe on structure change in the used style
	    if ((style = styleEvt->value()) == NULL) {
		mStyleUpdated.deleteFromSource();
	    }
	    else {
		CEvent* styleEvt = style->structureChangedEvent();
		mStyle.putValue(aExec, style);   // assign named style
		styleEvt->addSubscriber(&mStyleUpdated);
		// We do not want style to be run again!
		mStyle.cancel(aExec);
	    }
	}
	else {
	    // FIXME:  What to do if class == "" ?
	}
	// fprintf(stderr, "StyleLink::mClass was updated\n");
	update = true;
    }
    return update;
}


///////////////////////////////////////////////////////////////////////////////
//
// Style
//
///////////////////////////////////////////////////////////////////////////////

CStyle::CStyle(CExecutor* aExec,CBaseType *aType) :
    CExecutable(aExec, aType),
    mName(this),
    mForegroundColor(this),
    mBackgroundColor(this),
    mBorderColor(this),
    mFillColor(this),
    mFill(this),
    mBorderWidth(this),
    mFontName(this),
    mFontSize(this),
    mFontWeight(this),
    mFontSlant(this),
    mFontColor(this),  // maps to foreground color
    mGlyphDeltaX(this),
    mGlyphDeltaY(this),
    mGlyphDotKerning(this),
    mGlyphFixedWidth(this)
{
    useStructureChanged();   // Create the special event.

    mFont = NULL;
    EGcInit(&mGC);

    mName.putValue(aExec, "");                     // default to unnamed
    mForegroundColor.putValue(aExec, 0xffffff);    // white
    mFontColor.putValue(aExec,       0xfffffff);   // white
    mBackgroundColor.putValue(aExec, 0x000000);    // black
    mBorderColor.putValue(aExec,     0x000000);    // black
    mFillColor.putValue(aExec,       0x000000);    // black
    mFill.putValue(aExec, false);
    mBorderWidth.putValue(aExec, 0);
    mFontName.putValue(aExec, "Helvetica");
    mFontSize.putValue(aExec, 12);
    mFontWeight.putValue(aExec, EFONT_WEIGHT_BOLD);
    mFontSlant.putValue(aExec,  EFONT_SLANT_ROMAN);
    mGlyphDeltaX.putValue(aExec, 0);
    mGlyphDeltaY.putValue(aExec, 0);
    mGlyphDotKerning.putValue(aExec, 0);
    mGlyphFixedWidth.putValue(aExec, 0);

    eventPut(aExec, XINDEX(CStyle,name),   &mName);
    eventPut(aExec, XINDEX(CStyle,foregroundColor), &mForegroundColor);
    eventPut(aExec, XINDEX(CStyle,backgroundColor), &mBackgroundColor);
    eventPut(aExec, XINDEX(CStyle,borderWidth), &mBorderWidth);
    eventPut(aExec, XINDEX(CStyle,borderColor), &mBorderColor);
    eventPut(aExec, XINDEX(CStyle,fill),        &mFill);
    eventPut(aExec, XINDEX(CStyle,fillColor),   &mFillColor);
    eventPut(aExec, XINDEX(CStyle,fontName),   &mFontName);
    eventPut(aExec, XINDEX(CStyle,fontSize),   &mFontSize);
    eventPut(aExec, XINDEX(CStyle,fontWeight), &mFontWeight);
    eventPut(aExec, XINDEX(CStyle,fontSlant),  &mFontSlant);
    eventPut(aExec, XINDEX(CStyle,fontColor),  &mFontColor);
    eventPut(aExec, XINDEX(CStyle,glyphDeltaX), &mGlyphDeltaX);
    eventPut(aExec, XINDEX(CStyle,glyphDeltaY), &mGlyphDeltaY);
    eventPut(aExec, XINDEX(CStyle,glyphDotKerning), &mGlyphDotKerning);
    eventPut(aExec, XINDEX(CStyle,glyphFixedWidth), &mGlyphFixedWidth);
}

CStyle::~CStyle()
{
    CStyleList::iterator iter;

    if (m1_styles().member(this)) {
	fprintf(stderr, "style \"%s\" still in style list in destructor\n",
		name().c_str());
    }
}

int CStyle::mark(Mark_t aMark)
{
    if (mMark == aMark)
	return 0;
    else
	return CExecutable::mark(aMark) + m1Mark(mFont, aMark);
}

void CStyle::loadFont(int aResolution)
{
    string name;
    CFont* font;
    
    name = mFontName.value();
    if (!(font = m1_fonts().match(aResolution,name,
				  mFontSize.value(),
				  mFontWeight.value(),
				  mFontSlant.value()))) {
	printf("WARNING: Could not load font [%s] Size[%d]\n", 
	       name.c_str(), mFontSize.value());
    }
    m1SetRetain(CFont, &mFont, font);
}

bool CStyle::update(CExecutor* aExec, bool aStart)
{
    bool update = false;

    if (mName.assigned()) {
	if (mName.value() == "")
	    m1_styles().unregisterStyle(aExec, this);
	else
	    m1_styles().registerStyle(aExec, this);
	if (aStart) mName.cancel(aExec);
    }

    if (mFontName.assigned()) {
	if (aStart) mFontName.cancel(aExec);
	update = true;
    }
    if (mFontSlant.assigned()) {
	if (aStart) mFontSlant.cancel(aExec);
	update = true;
    }
    if (mFontWeight.assigned()) {
	if (aStart) mFontWeight.cancel(aExec);
	update = true;
    }
    if (mFontSize.assigned()) {
	if (aStart) mFontSize.cancel(aExec);
	update = true;
    }

    // If font is updated we reload the font 
    if (update) {
	loadFont(resolution(m1_default_screen()));
	if (mFont)
	    EGcSetFont(&mGC, mFont->epicFont());
	update = true;
    }

    if (mGlyphDeltaX.assigned()||mGlyphDeltaY.assigned()) {
	EGcSetGlyphDelta(&mGC, mGlyphDeltaX.value(), mGlyphDeltaY.value());
	update = true;
    }

    if (mGlyphDotKerning.assigned()) {
	EGcSetGlyphDotKern(&mGC, mGlyphDotKerning.value());
	update = true;
    }

    if (mGlyphFixedWidth.assigned()) {
	EGcSetGlyphWidth(&mGC, mGlyphFixedWidth.value());
	update = true;
    }

    if (mFill.assigned()) {
	if (mFill.value())
	    EGcSetFillStyle(&mGC, EPIC_FILL_STYLE_BLEND|EPIC_FILL_STYLE_AALIAS);
	else
	    EGcSetFillStyle(&mGC, EPIC_FILL_STYLE_NONE);
	if (aStart) mFill.cancel(aExec);
	update = true;
    }

    if (mForegroundColor.assigned()) {
	EPixel_t color;
	color.px = mForegroundColor.value();
	color.a  = 255; // FIXME
	EGcSetForegroundColor(&mGC, color);
	if (aStart) mForegroundColor.cancel(aExec);
	update = true;
    }

    if (mFontColor.assigned()) {
	EPixel_t color;
	color.px = mFontColor.value();
	color.a  = 0; // FIXME
	EGcSetForegroundColor(&mGC, color);
	if (aStart) mFontColor.cancel(aExec);
	update = true;
    }

    if (mBackgroundColor.assigned()) {
	EPixel_t color;
	color.px = mBackgroundColor.value();
	color.a  = 255; // FIXME
	EGcSetBackgroundColor(&mGC, color);
	if (aStart) mBackgroundColor.cancel(aExec);
	update = true;
    }

    if (mBorderColor.assigned()) {
	EPixel_t color;
	color.px = mBorderColor.value();
	color.a  = 255; // FIXME
	EGcSetBorderColor(&mGC, color);
	if (aStart) mBorderColor.cancel(aExec);
	update = true;
    }

    if (mFillColor.assigned()) {
	EPixel_t color;
	color.px = mFillColor.value();
	color.a  = 255; // FIXME
	EGcSetFillColor(&mGC, color);
	if (aStart) mFillColor.cancel(aExec);
	update = true;
    }
    return update;
}


void CStyle::start(CExecutor* aExec)
{
    update(aExec, true);
}

void CStyle::execute(CExecutor* aExec)
{
    if (update(aExec, false)) {
	setStructureChanged(aExec);
	// fprintf(stderr, "Style %s structure was updated\n", mName.value().c_str());
    }
}

