//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA,  2006.
//

#include <sys/time.h>
#include <poll.h>

#include "m1.hh"
#include "m1vm.hh"
#include "component.hh"

extern void m1_dump_type_stat(ostream* os);

//
// CSystem (system environment)
//

CSystem::CSystem(void):
	mPollVector(new struct pollfd[100]),
	mPollVectorSize(0),
	mPollVectorAllocSize(100),
	mFileSourceDescriptorMap(100, (CFileSource *) 0) 
{
    mExecutor = new CExecutor(this);  // A default executor
    mSystemStart    = m1_timeStamp(); // Time when system started
    mCurrentCycle   = 0UL;            // System cycle number
    mCycleTime      = 0UL;            // TimeStamp at start of cycle

    // Statistics
    mStatStart      = 0UL;            // Statistics TimeStamp
    mCycleCount     = 0;              // Cycles during statistcs
    mTotalCycleTime = 0;              // Accumulated cycle time in us
}

int CSystem::mark(unsigned long aMark)
{
    list<CExecutable*>::iterator iter;
    CExecutable* ptr;
    int marked = 0;
    int i;
    // Mark mQueue and mNextQueue objects
    for (ptr = mQueue.front(); ptr; ptr = ptr->qLink())
	marked += ptr->mark(aMark);

    for (ptr = mNextQueue.front(); ptr; ptr = ptr->qLink())
	marked += ptr->mark(aMark);

    for(iter = mEverySweepComponents.begin(); 
	iter != mEverySweepComponents.end(); ++iter)
	marked += (*iter)->mark(aMark);

    // Mark objects referenced trough mFileSourceDescriptorMap
    for (i = 0; i < (int) mFileSourceDescriptorMap.size(); i++) {
	CFileSource* src = mFileSourceDescriptorMap[i];
	if (src) marked += src->mark(aMark);
    }

    // Mark objects referenced trough executors
    marked += mExecutor->mark(aMark);
    return marked;
}

// Adjust starttime.
// Oldtime was the time just before the clock was adjusted.
// NewTime is the time just after the clock was adjusted.,
// Both aOldTime and aNewTime are in secs since epoch.
//
// Use this to reset the system start timestamp.
void CSystem::adjTime(time_t aOldTime, time_t aNewTime)
{
    // Since cannot be sure if time_t is signed or not, we cannot do
    // mSystemStart += (aNewTime - aOldTime) * 1000000, but must rather check which value is greater.

    if (aNewTime > aOldTime) {
	mSystemStart += ((unsigned long long) aNewTime - (unsigned long long) aOldTime) * 1000000LL;
	
    } else {
	mSystemStart -= ((unsigned long long) aOldTime - (unsigned long long) aNewTime) * 1000000LL;
    }

    // Retract system start one second to make sure we don't get a CSystem::timeStamp that is lesser than an earlier value.
    mSystemStart -= 1000000LL;
}


// best resolution timestamp
TimeStamp CSystem::timeStamp(void)
{
    return (m1_timeStamp() - mSystemStart);
}

//
// schedule the executable for execution and return 
// just returns the Later Update status flag
//
UpdateStatus CSystem::schedule(CExecutable *aComponent)
{
    DBGFMT_SCHD("CSweper::schedule %p last_cycle=%lu, type=%s", aComponent,
		aComponent->cycle(),
		aComponent->type()->cname());

    if (!aComponent->isScheduled()) {
	// Not scheduled already 
	if (!aComponent->isScheduledLater()) {
	    // Nor scheduled later

	    if (aComponent->cycle() == mCurrentCycle) {
		// Component was executed already in this round
		mNextQueue.enqueue(aComponent);
		aComponent->setScheduledLater();
		DBGFMT_SCHD("  SCHEDULED LATER");
		return UpdateStatusLater;
	    }
	    else {
		// Component needs to be run
		mQueue.enqueue(aComponent);
		aComponent->setScheduled();
		DBGFMT_SCHD("  WAS SCHEDULED");
		return UpdateStatusNo;
	    }
	}
	else {
	    DBGFMT_SCHD("  ALREADY SCHEDULED LATER");
	    return UpdateStatusLater;
	}
    }
    else {
	DBGFMT_SCHD("  ALREADY SCHEDULED");
    }
    return UpdateStatusNo;
}

void CSystem::addComponent(CExecutable *aComponent) 
{
    if (aComponent->executeEverySweep()) {
	// I think we should avoid reference counting this since
	// timers must be garbed when nothing is refering to it.
	mEverySweepComponents.push_back(aComponent);
    }
}


void CSystem::removeComponent(CExecutable* aComponent)
{
    // Time sensors etc must be removed from EverySweep list
    if (aComponent->executeEverySweep())
	mEverySweepComponents.remove(aComponent);
    // It is a very unlikely that component will be in mQueue at this point
    if (aComponent->isScheduled()) {
	if (mQueue.remove(aComponent))
	    aComponent->clrScheduled();
    }
    // It may however be scheduled later
    if (aComponent->isScheduledLater()) {
	if (mNextQueue.remove(aComponent)) 
	    aComponent->clrScheduledLater();
    }
}

bool CSystem::deletePollDescriptor(int aDescriptor)
{
    int p_ind = mPollVectorSize;

    // FIXME: map fd -> mPollVector index
    // Find the descriptor in the poll vector.
    while(p_ind--) 
	if (mPollVector[p_ind].fd == aDescriptor)
	    break;

    if (p_ind == -1)
	return false;
    
    // FIXME: swap !!!  constant operatation.
    // Shift back all subsequent elements
    while(p_ind < mPollVectorSize - 1) {
	mPollVector[p_ind] = mPollVector[p_ind + 1];
	++p_ind;
    }
    mPollVectorSize--;
    
    //
    // Kill the corresponding mFileSourceDescriptorMap entry
    // m1Set will release pointer but not retain the 0
    // use m1SetRetain to release the old pointer and retain the new
    m1Set(CFileSource, &mFileSourceDescriptorMap[aDescriptor], 0);
    
    return true;
}

void CSystem::addPollDescriptor(int aDescriptor, short aPollEvents, 
				CFileSource *aFileSource)
{
    //
    // Delete old entry, if there.
    //
    deletePollDescriptor(aDescriptor);

    //
    // Check if we need to size up the mPollVector
    //
    if (mPollVectorSize == mPollVectorAllocSize) {
	struct pollfd *new_vec;
	
	DBGFMT("CSweeper::addPollDescriptor(): Sizing up mPollVector from [%d] to [%d] in order to contain [%d] elements",
	       mPollVectorAllocSize, mPollVectorAllocSize + 100, 
	       mPollVectorSize + 1);
	
	// 100 new elements to be adedd.
	mPollVectorAllocSize += 100;
	
	// Alloc.
	new_vec = new struct pollfd[mPollVectorAllocSize];       

	// Copy old elements to new.
	memcpy(new_vec, mPollVector, sizeof(struct pollfd) * mPollVectorSize);
	
	// Kill old vector.
	delete[] mPollVector;

	// Reassign.
	mPollVector = new_vec;
    }	

    //
    // Add at end of descriptor vector.
    //
    mPollVector[mPollVectorSize].fd = aDescriptor;
    mPollVector[mPollVectorSize].events = aPollEvents;
    mPollVector[mPollVectorSize].revents = 0;

    //
    // Install an entry into the fd->CExecutable map
    // We must retain the object while beeing referenced.
    //
    mFileSourceDescriptorMap[aDescriptor] = m1Retain(CFileSource,aFileSource);
    
    // One extra elem
    ++mPollVectorSize;

    return;
}


void CSystem::executePolledComponents(TimeStamp)
{
    int p_ind;

    if (!mPollVectorSize)
	return;

    //
    // Poll. 
    // FIXME: Add poll thread
    // FIXME: If poll return -1 then scan and remove bad filedescriptors if any
    //
    if (m1Poll(mPollVector, mPollVectorSize, 0) <= 0)
	return;
    
    //
    // Scan result and schedule all objects
    //
    p_ind = mPollVectorSize;
    while(p_ind--) {
	CFileSource *desc;

	// Skip non triggered descriptors.
	if (!mPollVector[p_ind].revents)
	    continue;

	// Validate that we do have a corresponding CFileDescriptor
	if (!(desc = mFileSourceDescriptorMap[mPollVector[p_ind].fd])) {
	    printf("CSweeper::executePolledComponents(): Got hit on fd[%d] which is not claimed by a CFileDescriptor Component!\n", mPollVector[p_ind].fd);
	    continue;
	}

	// Mark "ready" as updated, which will trigger subscription cascade.
	desc->ready(mPollVector[p_ind].revents); 
 	desc->setCycle(mCurrentCycle);
    }
    return;
}


void CSystem::sweep(void) 
{
    int n;
    TimeStamp ct = timeStamp();
    int marked;
    list<CExecutable*>::iterator iter;

    //
    // Update statistics. 
    // All time not spent in this method is considered redraw time.
    //
    if (!mCycleTime) mCycleTime = ct;
    if (!mStatStart) mStatStart = ct;

    mCycleTime = ct;
    DBGFMT_SCHD("SWEEP BEGIN %lu time=%lu mQueue.size=%ld", 
		mCurrentCycle, mCycleTime, mQueue.size());

    mExecutor->reset();

    // Move next round elements to queue and updated the schedule flags
    DBGFMT_SCHD("SWEEP (1): mNextQueueSize=%lu", mNextQueue.size());
    if (!mNextQueue.empty()) {
	size_t nn = 0;
	CExecutable* ptr;

	for (ptr = mNextQueue.front(); ptr; ptr = ptr->qLink()) {
	    ptr->clrScheduledLater();
	    ptr->setScheduled();
	    DBGFMT_SCHD("CSystem::sweep %p cycle=%lu, type=%s MOVED TO QUEUE",
			ptr, ptr->cycle(), ptr->type()->cname());
	    nn++;
	}
	mQueue.merge(&mNextQueue);
	DBGFMT_SCHD("SWEEP (2): Executables moved=%lu",  nn);
    }
    
    DBGFMT_SCHD("SWEEP (3) mQueue.size=%ld, mNextQueueSize=%lu", 
		mQueue.size(), mNextQueue.size());	
    //
    // Execute all polling components. 
    // Now with improved technology. 
    //
    executePolledComponents(mCycleTime);

    //
    // Schedule all "execute every time" components
    //
    for(iter = mEverySweepComponents.begin();
	iter != mEverySweepComponents.end();
	++iter) {
	schedule(*iter);
    }

    DBGFMT_SCHD("SWEEP (4) mQueue.size=%ld, mNextQueueSize=%lu", 
		mQueue.size(), mNextQueue.size());

    while(!mQueue.empty()) {
	CExecutable *exec;
	
	// Dequeue element (front) element 
	exec = mQueue.dequeue();

	// Must clear schedule flag here in order for the object
	// to be able to reschedule while executing.
	exec->clrScheduled(); 

	// Set the objects execution cycle
	exec->setCycle(mCurrentCycle);

	// Now let the object execute
	mExecutor->execute(exec);
    }

    DBGFMT_SCHD("SWEEP END %lu, mQueue.size=%ld, mNextQueueSize=%lu",
		mCurrentCycle, mQueue.size(), mNextQueue.size());

    // Clean up cycle objects
#ifdef DEBUG
#ifdef DEBUG_MARK
    if (mCurrentCycle == 0) 
	m1_dump_stat(&cerr);
#endif
#endif
    n = m1DeleteZeroObjects(mExecutor);
    if (n) {
	DBGFMT_MNFO("collected: %d", n);
#ifdef DEBUG
#ifdef DEBUG_MARK
	m1_dump_stat(&cerr);
#endif
#endif
    }
    
    ct = timeStamp();

    //
    // Update statistics.
    //
    mTotalCycleTime += (ct - mCycleTime);  // usec
    mCycleCount++;

    // just skip when mCurrentCycle == MARK_NO_SWEEP since this mark
    // is special for objects not taking part in this special event
    if ((mCurrentCycle != MARK_NO_SWEEP)) {
#ifdef DEBUG
	// m1_dump_type_stat(&cerr);
#endif
	marked = 0;
#ifdef DEBUG
#ifdef DEBUG_MARK
	marked = m1_mark(mCurrentCycle);
	cerr << "Marked " << marked << " number of objectes\n";
	n = m1DeletedUnMarkedObjects(mExecutor, mCurrentCycle);
	cerr << "Deleted " << n << " unreferenced objects\n";
#endif
#endif
    }


    // Every 500 msec, update symbols
    if (ct - mStatStart > 500000) {
	double sweepTm = double(mTotalCycleTime) / double(mCycleCount);
	m1_main().m1AvgSweepTime.putValue(mExecutor, sweepTm);

	// Reset. 
	mCycleCount = 0;
	mTotalCycleTime = 0;
	mStatStart = ct;
    }
	
    // Advance to next cycle 
    mCurrentCycle++;
}
//
// CStack (executor stack object)
//
//
CStack::CStack()
{
    mValueStackBegin = (UData*) malloc(sizeof(UData)*MAX_VALUE_STACK_SIZE);
    mValueStackEnd   = mValueStackBegin + MAX_VALUE_STACK_SIZE;

    mReturnStackBegin = (UData*) malloc(sizeof(UData)*MAX_RETURN_STACK_SIZE);
    mReturnStackEnd   = mReturnStackBegin + MAX_RETURN_STACK_SIZE;

    reset();
}

CStack::~CStack()
{
    free(mValueStackBegin);
    free(mReturnStackBegin);
}

void CStack::reset(void)
{
    mValueStackPtr  = mValueStackBegin;
    mReturnStackPtr = mReturnStackBegin;
    mFramePtr       = mValueStackBegin;

    mReturnStackPtr[0].ud = mValueStackBegin;
    mReturnStackPtr[1].addr = NULL;  // FIXME point on default continue
    mReturnStackPtr[2].addr = NULL;  // FIXME point on default break
    mReturnStackPtr[3] = nil;        // default return value
    mReturnStackPtr += 4;
}

//
// CExector (executing environment)
//
CExecutor::CExecutor(CSystem* aSystem) :
    mSystem(aSystem),
    mStack(NULL),
    mCurrent(NULL)
{
    mStack = m1New(CStack);
    m1Retain(CStack, mStack);

    mState.mActivation[SCOPE_UNIVERSE]  = NULL;
    mState.mActivation[SCOPE_GLOBAL] = NULL;
    mState.mActivation[SCOPE_OBJECT]  = NULL;
    mState.mActivation[SCOPE_STACK]   = (CBaseObject*) mStack;
    mEventSourceFile = NULL;
    mEventSourceLine = 0;
}

CExecutor::~CExecutor()
{
    m1Release(CStack, mStack);
}

int CExecutor::mark(unsigned long aMark)
{
    int marked = 0;

    // mark the stack object it self (NOT the elements in the stack)
    marked += mStack->mark(aMark);
    // mark current executable if present
    if (mCurrent)
	marked += mCurrent->mark(aMark);
    return marked;
}

void CExecutor::reset(void)
{
    mStack->reset();   // This shold not be needed
    mCurrent = NULL;
    // FIXME: Trace this, should not be needed
    mState.mActivation[SCOPE_STACK]    = (CBaseObject*) mStack;
    mState.mActivation[SCOPE_UNIVERSE] = &m1_context();
}

void CExecutor::execute(CExecutable* aObj)
{
    CExecutableType* aObjType;

    aObjType = (CExecutableType*) aObj->type();

    activate(aObj);

    DBGFMT_SCHD("CExector::execute %p cycle=%lu, type=%s",
		aObj, aObj->cycle(), aObjType->cname());

    // Set UpdateStatusYes on all event variables in the updated list
    aObj->setAllUpdated(this);

    // Execute the object
    aObjType->execute(this, aObj);

    // Propagate all updated events
    aObj->propagateAllUpdated(this);

    // And now clear them (but keep the events marked as later)
    aObj->clearAllUpdated(this);

    deactivate(aObj);
}


void CExecutor::save(ExecutorState* state) 
{
    *state = mState;
}

void CExecutor::restore(ExecutorState* state)
{
    mState = *state;
    mCurrent = (CExecutable*) mState.mActivation[SCOPE_OBJECT];
}

void CExecutor::activate(CBaseObject* aObject)
{
#ifdef DEBUG
    if (M1DBG_IS_SET(M1DBG_SCHD)) {
	fprintf(stderr, "ACTIVATE OBJECT[%s]\n",
		aObject->debugName().c_str());
    }
#endif
    mCurrent = (CExecutable*) aObject;
    mState.mActivation[SCOPE_OBJECT] = aObject;
}

void CExecutor::deactivate(CBaseObject* aObject) 
{
#ifdef DEBUG
    if (M1DBG_IS_SET(M1DBG_SCHD)) {
	fprintf(stderr, "DEACTIVATE OBJECT[%s]\n", 
		aObject->debugName().c_str());
    }
#endif
    mCurrent = NULL;
    mState.mActivation[SCOPE_OBJECT] = NULL;
}

//
// Special to activate global contexts (file_scope and library )
//
void CExecutor::activate_global(CBaseObject* aGlobal, CBaseObject** aSave)
{
#ifdef DEBUG
    if (M1DBG_IS_SET(M1DBG_SCHD)) {
	fprintf(stderr, "ACTIVATE GLOBAL[%s]\n", 
		aGlobal ? aGlobal->debugName().c_str() : "nil");
    }
#endif
    *aSave = mState.mActivation[SCOPE_GLOBAL];
    mState.mActivation[SCOPE_GLOBAL] = aGlobal;
}

void CExecutor::restore_global(CBaseObject* aGlobal)
{
#ifdef DEBUG
    if (M1DBG_IS_SET(M1DBG_SCHD)) {
	fprintf(stderr, "RESTORE GLOBAL[%s]\n", 
		aGlobal ? aGlobal->debugName().c_str() : "nil");
    }
#endif
    mState.mActivation[SCOPE_GLOBAL] = aGlobal;
}

