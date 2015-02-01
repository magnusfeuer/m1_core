//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//

#include "input_strategy.hh"
#include "message.hh"

XOBJECT_TYPE_BOOTSTRAP(CInputStrategyBase);


CInputStrategyBase::CInputStrategyBase(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType),
    mLayer(this),
    mRecorder(this),
    mX(this),            // Horizontal placement
    mY(this),            // Vertical placement.
    mButton1Down(this),
    mButton2Down(this),
    mButton3Down(this),
    mButton4Down(this),
    mButton5Down(this),
    mRockerDirection(this),
    mRockerDown(this),
    mKeyDown(this), 
    mKeyValue(this)
{ 
    mX.putValue(aExec, 0);
    mY.putValue(aExec, 0);
    mLayer.putValue(aExec, 0);
    mRecorder.putValue(aExec, 0);
    mButton1Down.putValue(aExec, 0);
    mButton2Down.putValue(aExec, 0);
    mButton3Down.putValue(aExec, 0);
    mButton4Down.putValue(aExec, 0);
    mButton5Down.putValue(aExec, 0);
    mKeyValue.putValue(aExec, 0);
    mKeyDown.putValue(aExec, 0);

    mMouseOverInd = 0;

    eventPut(aExec,XINDEX(CInputStrategyBase,layer), &mLayer);
    eventPut(aExec,XINDEX(CInputStrategyBase,recorder), &mRecorder);
    eventPut(aExec,XINDEX(CInputStrategyBase,x), &mX);
    eventPut(aExec,XINDEX(CInputStrategyBase,y), &mY);
    eventPut(aExec,XINDEX(CInputStrategyBase,button1Down), &mButton1Down); 
    eventPut(aExec,XINDEX(CInputStrategyBase,button2Down), &mButton2Down); 
    eventPut(aExec,XINDEX(CInputStrategyBase,button3Down), &mButton3Down); 
    eventPut(aExec,XINDEX(CInputStrategyBase,button4Down), &mButton4Down); 
    eventPut(aExec,XINDEX(CInputStrategyBase,button5Down), &mButton5Down); 

    eventPut(aExec,XINDEX(CInputStrategyBase,rockerDirection), &mRockerDirection); 
    eventPut(aExec,XINDEX(CInputStrategyBase,rockerDown), &mRockerDown); 

    eventPut(aExec,XINDEX(CInputStrategyBase,keyDown), &mKeyDown); 
    eventPut(aExec,XINDEX(CInputStrategyBase,keyValue), &mKeyValue);
}

//
// Post an event to a layer
//
void CInputStrategyBase::postEvent(CExecutor* aExec, 
				   CLayerComponent* aLayer,
				   MessageName aMessageName,
				   float x, float y,
				   int aValue)
{
    CMessage* message;

    if (!(aLayer->messageMask() & aMessageName))
	return;

    message = m1New(CMessage, aExec, CMessage::CMessageType::singleton());
    message->put(aExec,XINDEX(CMessage,name), UUnsigned(aMessageName));
    message->put(aExec,XINDEX(CMessage,x),  UFloat(x));
    message->put(aExec,XINDEX(CMessage,y),  UFloat(y));
    message->put(aExec,XINDEX(CMessage,z),  UFloat(0.0));
    message->put(aExec,XINDEX(CMessage,x0), UFloat(float(mX.value())));
    message->put(aExec,XINDEX(CMessage,y0), UFloat(float(mY.value())));
    message->put(aExec,XINDEX(CMessage,timeStamp),
		 UUnsigned(m1_TimeStampToTime(aExec->cycleTime())));
    message->put(aExec,XINDEX(CMessage,value), USigned(aValue));

//    cerr << "Post message: ";
//    CMessage::CMessageType::singleton()->print(&cerr, UObject(message));
//    cerr << "\n";

    aLayer->postMessage(message);
}

//
// Reset the layer's input stats when we loose focus
//
void CInputStrategyBase::resetLayer(CExecutor* aExec, CLayerComponent *aLayer)
{
    int i;
    
    for(i = 0; i < 5; ++i) {
	if (aLayer->buttonTest(i)) {
	    aLayer->buttonClr(i);
	    postEvent(aExec, aLayer, MESSAGE_BUTTON_UP, -1.0, -1.0, i+1);
	}
    }
}

//
// Identify all layers that are under the new mouse position only and in the old
// mouse position only.
// FIXME: Speedup with maps? Probably not since each list contains approx 20 eleements.
//
void CInputStrategyBase::uniq(CLayerPathEntryList *aFirst, 
			      CLayerPathEntryList *aSecond, 
			      CLayerComponentList *aOnlyInFirst)
{
     CLayerPathEntryListIterator i_first; // Iterator first
     CLayerPathEntryListIterator i_second; // Iterator second

     // Loop through second elements
     for (i_first = aFirst->begin(); i_first != aFirst->end() ; ++i_first) {
	 for (i_second = aSecond->begin(); i_second != aSecond->end() ;  ++i_second) {
// 	     printf("Checking [%f][%f] vs. [%f][%f]\n",
// 		    (*i_first).mComponent->top(), (*i_first).mComponent->left(),
// 		    (*i_second).mComponent->top(), (*i_second).mComponent->left());

	     if ((*i_second).component() == (*i_first).component())
		 break;

	 }
	 // Check if second element is not in first list
	 if (i_second == aSecond->end()) { 
	     aOnlyInFirst->push_back((*i_first).component());
	 }
     }
}


//
// Recursive way of locating a focus layer.
//
int CInputStrategyBase::collectLayers(CLayerComponent *aLayer, 
				      CLayerPathEntryList *aResult,
				      float aX, 
				      float aY, 
				      float aTopOffset, 
				      float aLeftOffset)
{
    int res = 0; // Number of collected layers.
    unsigned int i = 0;
    float ft,fl;
    float fw, fh;

#ifdef DEBUG
    static int index =0;
    static char index_buf[80];

    memset(index_buf, 32, 80);
    index_buf[index * 2] = 0;
#endif

    // Check if we ourselves want focus. If so return.
    // If aLayer is not set, return.
    if (!aLayer)
	return 0;

    //
    // Check if we want focus. If so, we are done.
    //

    //
    // Check if {aX, aY} are within the child's area.
    // If not, return. 
    // Also return if we are fully transparent.
    //
    ft = aLayer->top() + aTopOffset;
    if (aY < ft) {
	return 0;
    }

    fh = aLayer->height();
    fh = (fh < 0) ? aLayer->contentHeight() : fh;
    if (aY > (ft + fh)) {
	return 0;
    }
    
    fl = aLayer->left() + aLeftOffset;
    if (aX < fl) {
	return 0;
    }
    fw = aLayer->width();
    fw = (fw < 0) ? aLayer->contentWidth() : fw;

    if (aX > (fl + fw)) {
	return 0;
    }

    //
    // If we don't want focus, return now since none
    // of our kids should have it either (even if they
    // have wantFocus = true).
    //
    if (!aLayer->wantFocus()) {
	return 0;
    }

    //
    // Just collect invisible objects.
    // FIXME? aLayer->enabled() instead to allow non visible objects
    // to get events?
    // FIXME! we MUST check the calculated visibility!!!
    //  
    if (!aLayer->visible()) {
	return 0;
    }

#ifdef DEBUG
//        printf("%s%p: X[%.3f] Y[%.3f] TopOffset[%.3f] Top[%3.0f] LeftOffset[%.3f] Left[%3.0f] Height[%3.0f] Width[%3.0f] WantFocus[%c] Transparency[%f]\n", 
//    	   index_buf, aLayer, aX, aY, aTopOffset, aLayer->top(), aLeftOffset, 
//    	   aLayer->left(), aLayer->height(), aLayer->width(), aLayer->wantFocus()?(aLayer->exclusiveFocus()?'E':'Y'):' ',
//    	   aLayer->transparency());
#endif

    aResult->push_front(CLayerPathEntry(aLayer, aX - fl, aY - ft));
    
    //
    // Traverse all children to see if any of them want focus.
    //
    while(i < aLayer->childCount()) {
	CLayerComponent *res;
	CLayerComponent *child = aLayer->child(i);


// 	child->type()->print(&cout, UObject(child));
// 	cout << endl << endl;
	
	// Call child to see if we have any luck.
#ifdef DEBUG
	++index;
#endif
	res += collectLayers(child, aResult,aX,aY,ft,fl);
#ifdef DEBUG
	--index;
#endif
	// Try next child
	++i;
    }

    // Return the number of collected layers.
    return res;
}


CInputStrategyBase::~CInputStrategyBase(void) 
{

}
				   
//
// The default strategy is to give focus on mouse over.
//
void CInputStrategyBase::execute(CExecutor* aExec)
{
//    TimeStamp tTime = aExec->cycleTime();
    CLayerComponentList only_old, only_new;
//     int button_down = -1;
//     int button_up = -1;
    float fx, fy;

    //
    // Check for early exit.
    //
    if (!mX.updated() &&
	!mY.updated() &&
	!mButton1Down.updated() &&
	!mButton2Down.updated() &&
	!mButton3Down.updated() &&
	!mButton4Down.updated() &&
	!mButton5Down.updated() &&
	!mRockerDown.updated() &&
	!mRockerDirection.updated() &&
	!mKeyDown.updated() &&
	!mKeyValue.updated())
	return;

    if (mRecorder.value() && mRecorder.value()->enabled()) {
	if (mX.updated())
	    mRecorder.value()->x(mX.value());
	if (mY.updated())
	    mRecorder.value()->y(mY.value());
	if (mButton1Down.updated())
	    mRecorder.value()->button1Down(mButton1Down.value());
	if (mButton2Down.updated())
	    mRecorder.value()->button2Down(mButton2Down.value());
	if (mButton3Down.updated())
	    mRecorder.value()->button3Down(mButton3Down.value());
	if (mButton4Down.updated())
	    mRecorder.value()->button4Down(mButton4Down.value());
	if (mButton5Down.updated())
	    mRecorder.value()->button5Down(mButton5Down.value());
	if (mRockerDown.updated())
	    mRecorder.value()->rockerDown(mRockerDown.value());
	if (mRockerDirection.updated())
	    mRecorder.value()->rockerDirection(mRockerDirection.value());
	if (mKeyDown.updated())
	    mRecorder.value()->keyDown(mKeyDown.value());
	if (mKeyValue.updated())
	    mRecorder.value()->keyValue(mKeyValue.value());
    }

    //
    // Collect all layers that are matching.
    // The first layer in the resulting list who has wantFocus set
    // is the topmost layer within the list that should get focus.
    //
    fx = float(mX.value());
    fy = float(mY.value());

    mMouseOverInd ^= 1; // Flip between zero and one.

    mMouseOverList[mMouseOverInd].clear();
    // Collect all layers we are currently over.
    collectLayers(mLayer.value(), &mMouseOverList[mMouseOverInd], fx, fy, 0.0, 0.0);
    
    // Separate layers only in current mouse over and old mouse over
    // Hmmm keep an internal flag in each layer to let collectLayers do this!
    uniq(&mMouseOverList[mMouseOverInd ^ 1], &mMouseOverList[mMouseOverInd], &only_old);

    //
    // Reset all old layers 
    //
    while(only_old.begin() != only_old.end()) {
	resetLayer(aExec, only_old.front());
	only_old.pop_front();
    }

    for (CLayerPathEntryListIterator iter = mMouseOverList[mMouseOverInd].begin(); 
	 iter != mMouseOverList[mMouseOverInd].end(); ++iter) {
	int i;
	EventTime* but[] = { &mButton1Down,
			     &mButton2Down,
			     &mButton3Down,
			     &mButton4Down,
			     &mButton5Down};
	// Update buttons if necessary.
	for (i = 0; i < 5; i++) {
	    if ((but[i]->value() == 0) && (*iter).component()->buttonTest(i)) {
		(*iter).component()->buttonClr(i);
		postEvent(aExec, (*iter).component(), MESSAGE_BUTTON_UP, 
			  (*iter).localX(), (*iter).localY(), i+1);
	    }
	    else if ((but[i]->value() == 1) && !(*iter).component()->buttonTest(i)) {
		(*iter).component()->buttonSet(i);
		postEvent(aExec, (*iter).component(), MESSAGE_BUTTON_DOWN, 
			  (*iter).localX(), (*iter).localY(), i+1);
	    }
	}

	if (mRockerDirection.updated() || mRockerDown.updated()) {
	    if ((mRockerDown.value() != 0) && (mRockerDirection.value() != 0)) {
		postEvent(aExec, (*iter).component(), MESSAGE_ROCKER_DOWN, 
			  (*iter).localX(), (*iter).localY(), mRockerDirection.value());
	    }
	    else {
		postEvent(aExec, (*iter).component(), MESSAGE_ROCKER_UP,
			  (*iter).localX(), (*iter).localY(), 0);
	    }
	}

	if (mX.updated() || mY.updated()) {
	    postEvent(aExec, (*iter).component(), MESSAGE_MOTION, 
		      (*iter).localX(), (*iter).localY(), 0);
	}

	if (mKeyDown.updated() && mKeyValue.updated()) {
	    if (mKeyDown.value() == 0) {
		postEvent(aExec, (*iter).component(), MESSAGE_KEY_UP,
			  (*iter).localX(),(*iter).localY(),mKeyValue.value());
	    }
	    else {
		postEvent(aExec, (*iter).component(), MESSAGE_KEY_DOWN,
			  (*iter).localX(),(*iter).localY(),mKeyValue.value());
	    }
	}

	if ((*iter).component()->exclusiveFocus())
	    break;
    }
    return;
}
