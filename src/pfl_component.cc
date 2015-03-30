//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006, 2007.
//
#include "pfl_component.hh"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mount.h>
#include "m1vm.hh"
using namespace std;

XOBJECT_TYPE_BOOTSTRAP(CPacketComponent);
XOBJECT_TYPE_BOOTSTRAP(CPacketInfoComponent);

//
// FIXME: Make more efficient than byte-by-byte read.
//
int readLine(int aDescriptor, char *aBuf, int aMaxLen)
{
    int read_len = 0;

    while(read_len < aMaxLen) {
	int x = read(aDescriptor, &aBuf[read_len], 1);

	if (x <= 0)
	    return -1;

	if (aBuf[read_len] == '\n') {
	    aBuf[read_len] = 0;
	    return read_len;
	}
	++read_len;
    }
    return read_len;
}


CPacketComponent::CPacketComponent(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mFifoName(this),
    mInstallState(this),
    mInstall(this),
    mProgress(this),
    mFifoDesc(-1),
    mPostAction(this),
    mFifoSource(NULL),
    mFifoRevents(this),
    mInFileSource(NULL),
    mInFileRevents(this),
    mInstallIter(0)
{
    mPostAction.putValue(aExec, 0);
    mInstallState.putValue(aExec, -1);

    eventPut(aExec, XINDEX(CPacketComponent,fifo), &mFifoName);
    eventPut(aExec, XINDEX(CPacketComponent,installState), &mInstallState);
    eventPut(aExec, XINDEX(CPacketComponent,install), &mInstall);
    eventPut(aExec, XINDEX(CPacketComponent,progress), &mProgress);
    eventPut(aExec, XINDEX(CPacketComponent,fifoRevents), &mFifoRevents);
    eventPut(aExec, XINDEX(CPacketComponent,inFileRevents), &mInFileRevents);
    eventPut(aExec, XINDEX(CPacketComponent,postAction), &mPostAction);

    put(aExec, XINDEX(CPacketComponent,packets),
	UArray(new CArray(aExec, CArrayType::create(string_type(), 0), sizeof(CString *), 0)));
    put(aExec, XINDEX(CPacketComponent,failedDeps),
	UArray(new CArray(aExec, CArrayType::create(string_type(), 0), sizeof(CString *), 0)));

    put(aExec, XINDEX(CPacketComponent,wrongTarget),
	UArray(new CArray(aExec, CArrayType::create(string_type(), 0), sizeof(CString *), 0)));

    put(aExec, XINDEX(CPacketComponent,root), UString(m1New(CString, (char*) "/")));
    put(aExec, XINDEX(CPacketComponent,dbDirectory), UString(m1New(CString, (char*) "/m1/install_db")));

    mFifoSource = m1New(CFileSource, aExec);
    m1Retain(CFileSource, mFifoSource);
    connect(XINDEX(CPacketComponent,fifoRevents), 
	    mFifoSource, XINDEX(CFileSource,revents));

    mInFileSource = m1New(CFileSource, aExec);
    m1Retain(CFileSource, mInFileSource);
    connect(XINDEX(CPacketComponent,inFileRevents),
	    mInFileSource,XINDEX(CFileSource,revents));
}


CPacketComponent::~CPacketComponent(void)
{
    if (mInstallIter)
	delete mInstallIter;

    m1Release(CFileSource, mFifoSource);
    m1Release(CFileSource, mInFileSource);
}


bool CPacketComponent::setupFifoReader(CExecutor* aExec)
{
    struct stat not_used;
//    printf("mFifoName[%s]\n", mFifoName.value().c_str());

    if (mFifoDesc != -1)
	close(mFifoDesc);

    if (mFifoName.value() == "")
	return false;

    // Create fifo if not therre.
    if (stat(mFifoName.value().c_str(), &not_used) == -1 && 
	errno == ENOENT && 
	mkfifo(mFifoName.value().c_str(), 0666) == -1) {
	printf("Could not create fifo [%s]\n", mFifoName.value().c_str());
	mFifoName.value() = "";
	mFifoName.cancel(aExec);
	return false;
    }

    if ((mFifoDesc = open(mFifoName.value().c_str(), O_RDONLY | O_NONBLOCK)) == -1) {
	printf("Could not open fifo[%s]\n", mFifoName.value().c_str());
	return false;
    }
    DBGFMT("Using fifo[%s]\n", mFifoName.value().c_str());

    mFifoSource->setDescriptor(aExec, mFifoDesc, POLLIN);
    return true;
}

void CPacketComponent::start(CExecutor* aExec)
{
    setupFifoReader(aExec);
    if (!CPacketManager::installedPackets()->loadHeaders("/m1/install_db"))
	DBGFMT_WARN("Could not read install database from /m1/install_db");
}

void CPacketComponent::execute(CExecutor* aExec)
{
    CPacketList lst;
    char buf[512];
    int ind = 0;
    bool do_abort = false;
    CStringList wrongTargetLst;

    // File to read.
    if (mFifoRevents.updated() && mInstallState.value() == -1) {
	printf("Triggered on fifo\n");
	mProgress.putValue(aExec, 0.0);
	while(readLine(mFifoDesc, buf, 512) != -1) {

	    printf("Reading headers from[%s]\n", buf);
	    CPacketManager::newPackets()->reset();
	    if (!CPacketManager::newPackets()->loadHeaders(buf)) {
		printf("Could not read headers from[%s]\n", buf);
		do_abort = true;
		break;
	    }

	    if (!CPacketManager::newPackets()->packets().size()) {
		printf("No packets found\n");
		do_abort = true;
		break;
	    }

//   	    puts("\n\nEXISTING PACKETS");
//   	    CPacketManager::installedPackets()->dump();

//   	    puts("\n\nNEW PACKETS");
//  	    CPacketManager::newPackets()->dump();
	
	    //
	    // Check if any packets have the wrong target.
	    //
	    at(XINDEX(CPacketComponent,wrongTarget)).arr->resize(0);
	    // Build a list over all packets with an incorrect target
	    for (CPacketListIterator iter = CPacketManager::newPackets()->packets().begin(); 
		 iter != CPacketManager::newPackets()->packets().end(); 
		 ++iter) {
		if ((*iter)->targetDevice() != "" && (*iter)->targetDevice() !=  m1_main().m1Serial->c_str()) {
		    sprintf(buf, "%s@%s/%s [%s-%s]", 
			    (*iter)->fromAccount().c_str(),
			    (*iter)->fromProvider().c_str(),
			    (*iter)->name().c_str(),
			    (*iter)->targetDevice().substr(0, 3).c_str(),
			    (*iter)->targetDevice().substr(3, 3).c_str());

		    wrongTargetLst.push_back(buf);
		    printf("Wrong target: %s\n", buf);
		}
	    }
	    //
	    // If we have hits, resize wrongTarget array and copy list to it.
	    //
	    if (wrongTargetLst.size() > 0) {
		printf("Resizing wrongTarget list to [%d] components\n", wrongTargetLst.size());
		at(XINDEX(CPacketComponent,wrongTarget)).arr->resize(wrongTargetLst.size());
		ind = 0;
		while(wrongTargetLst.begin() != wrongTargetLst.end()) {
		    at(XINDEX(CPacketComponent,wrongTarget)).arr->put(aExec, ind, UString(m1New(CString, wrongTargetLst.front().c_str())));
		    wrongTargetLst.pop_front();
		    ++ind;
		}
		CPacketManager::newPackets()->packets().clear(); // Wipe packets.
		mInstallState.putValue(aExec, 4); // Wrong target.
		do_abort = true;
	    }

	    //
	    // Check if we have any failed dependencies.
	    //
	    at(XINDEX(CPacketComponent,failedDeps)).arr->resize(0);
	    if (CPacketManager::newPackets()->validatePackets(lst)) {
		printf("Missing packets:\n");
		at(XINDEX(CPacketComponent,failedDeps)).arr->resize(lst.size());
		ind = 0;
		for (CPacketListIterator iter = lst.begin(); iter != lst.end(); ++iter) {
		    char major[8];
		    char minor[8];
		    char patch[8];

		    if ((*iter)->versionMajor() == -1)
			strcpy(major, "X");
		    else

			sprintf(major, "%d", (*iter)->versionMajor());

		    if ((*iter)->versionMinor() == -1)
			strcpy(minor, "X");
		    else
			sprintf(minor, "%d", (*iter)->versionMinor());

		    if ((*iter)->versionPatch() == -1)
			strcpy(patch, "X");
		    else
			sprintf(patch, "%d", (*iter)->versionPatch());

		    sprintf(buf, "%s@%s/%s %s.%s.%s", 
			    (*iter)->fromAccount().c_str(),
			    (*iter)->fromProvider().c_str(),
			    (*iter)->name().c_str(),
			    major, minor, patch);
		    at(XINDEX(CPacketComponent,failedDeps)).arr->put(aExec, ind, UString(m1New(CString, buf)));
		    printf("   %s\n", buf);
		    ++ind;

		}
		CPacketManager::newPackets()->packets().clear(); // Wipe packets.
		
		mInstallState.putValue(aExec, 3); // Missing deps (enum?)
		do_abort = true;
	    }
	}
	// Reopen FIFO
	close(mFifoDesc);

	if (!(mFifoDesc = open(mFifoName.value().c_str(), O_RDONLY | O_NONBLOCK)) == -1) {
	    DBGFMT_WARN("CPacketManager(): Could not open fifo[%s]\n", mFifoName.value().c_str());
	    mInstallState.putValue(aExec,  1);
	    return;
	}

	mFifoSource->setDescriptor(aExec, mFifoDesc, POLLIN);

	if (do_abort)
	    return;
	    
	
	//	printf("Adding[%d] packets\n", CPacketManager::newPackets()->packets().size());

	at(XINDEX(CPacketComponent,packets)).arr->resize(CPacketManager::newPackets()->packets().size());
	for (CPacketListIterator iter = CPacketManager::newPackets()->packets().begin(); 
	     iter != CPacketManager::newPackets()->packets().end(); 
	     ++iter) {
	    sprintf(buf, "%s@%s/%s %d.%d.%d", 
		    (*iter)->fromAccount().c_str(),
		    (*iter)->fromProvider().c_str(),
		    (*iter)->name().c_str(),
		    (*iter)->versionMajor(),
		    (*iter)->versionMinor(),
		    (*iter)->versionPatch());
	    at(XINDEX(CPacketComponent,packets)).arr->put(aExec, ind, UString(m1New(CString, buf)));
	    ind++;
	}
#ifdef DEBUG
	printf("Setting mInstallState to 0\n");
#endif
	mInstallState.putValue(aExec, 0);
	return;
    }

    // Install
    if (mInstall.updated() && mInstall.value() && mInstallState.value() == 0) {
	mPostAction.putValue(aExec,  0);
	// Remount root as read write
	//	printf("Will install new packets\n");
#ifndef DARWIN
	if (mount("/dev/hda1", "/", "", MS_REMOUNT, 0) == -1) {
 	    printf("remount rw (pre) failed /dev/hda1: [%s]\n", strerror(errno));
// 	    CPacketManager::newPackets()->packets().clear(); // Wipe packets.
// 	    return;
	}
#endif

	if (!mInstallIter) {
	    //	    printf("Creating new installer iterator\n");
	    mInstallIter = new CInstallIterator(CPacketManager::newPackets(), mBuffer, PFL_CHUNK_SIZE, "/", "/m1/install_db");
	}

	//	printf("Resetting iterator\n");
	if (!mInstallIter->reset() && mInstallIter->atEnd()) {
	    printf("Failed to reset iterator - USB stick probably out\n");
	    mInstallState.putValue(aExec, 16);
	    
#ifndef DARWIN
	    sync();
	    if (umount("/mnt") == -1)
		printf("umount /mnt failed: [%s]\n", strerror(errno));

	    
	    if (mount("/dev/hda1", "/", "", MS_REMOUNT, 0) == -1)
		printf("remount rw (post) failed: [%s]\n", strerror(errno));

	    if (mount("/dev/hda1", "/", "", MS_REMOUNT | MS_RDONLY, 0) == -1) 
		printf("remount ro (post) failed: [%s]\n", strerror(errno));
#endif
	    return;
	}

	mInFileSource->setDescriptor(aExec, fileno(mInstallIter->pflDescriptor()), POLLIN);
	
//	printf("mInstallState set to 10\n");
	mInstallState.putValue(aExec, 10); // Install in progress.
	return;
    }

    if (mFifoName.updated()) {
//	printf("Fifoname updated to [%s]\n", mFifoName.value().c_str());
	setupFifoReader(aExec);
	return;
    }


    //
    // Install another chunk?
    //
    if(mInFileRevents.updated()) {
	// Check if we have more of current file
	// Check that we can actually write.
	
	mProgress.putValue(aExec, mInstallIter->progress());
	if (mInstallIter->installChunk()) {
	    return;
	}

	// Check if we have another file in packet
	if (mInstallIter->nextFile()) 
	    return;

	// Check if we have another packet to install
	if (mInstallIter->nextPacket()) {
	    mInFileSource->setDescriptor(aExec, fileno(mInstallIter->pflDescriptor()), POLLIN);
	    return;
	}

	// Clear file source event subscription
	mInFileSource->setDescriptor(aExec, -1, 0);

	// We are done. Remount disk.
#ifndef DARWIN
	sync();
	if (umount("/mnt") == -1)
	    perror("umount /mnt");
	    
	if (mount("/dev/hda1", "/", "", MS_REMOUNT, 0) == -1 ||
	    mount("/dev/hda1", "/", "", MS_REMOUNT | MS_RDONLY, 0) == -1) {
	    perror("mount -oremount,ro /dev/hda1 /");
	}
#endif
	mPostAction.putValue(aExec, 2);
	// Update post install action required.
	if (CPacketManager::newPackets()->needReboot()) {
	    mPostAction.putValue(aExec,  1); 
	}
	else
	    // Update post install action required.
	    if (CPacketManager::newPackets()->needRestart()) {
		mPostAction.putValue(aExec, 2); 
	    }
	    else
		// Update post install action required.
		if (CPacketManager::newPackets()->needM1Reload()) {
		    mPostAction.putValue(aExec, 3);
		}
		else 
		    // Update post install action required. 
		    if (CPacketManager::newPackets()->needLibReload()) {
			mPostAction.putValue(aExec, 4);
		    }

	mInstallState.putValue(aExec, 100);
	delete mInstallIter;
	mInstallIter = 0;
	return;
    }

}


















CPacketInfoComponent::CPacketInfoComponent(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType)
{
    put(aExec, 
	XINDEX(CPacketInfoComponent,packets),
	UArray(new CArray(aExec, CArrayType::create(string_type(), 0), sizeof(CString *), 0)));

    put(aExec, XINDEX(CPacketInfoComponent,serial), UString(m1New(CString, (char*) "")));
}

CPacketInfoComponent::~CPacketInfoComponent(void)
{
}



void CPacketInfoComponent::start(CExecutor* aExec)
{
    int ind = 0;
    if (!CPacketManager::installedPackets()->loadHeaders("/m1/install_db")) {
	DBGFMT_WARN("Could not read install database from /m1/install_db");
	return;
    }
    
    // Setup all installed packages
    at(XINDEX(CPacketInfoComponent,packets)).arr->resize(CPacketManager::installedPackets()->packets().size());
    for (CPacketListIterator iter = CPacketManager::installedPackets()->packets().begin(); 
	 iter != CPacketManager::installedPackets()->packets().end(); 
	 ++iter) {
	char buf[256];
	sprintf(buf, "%s@%s/%s %d.%d.%d", 
		(*iter)->fromAccount().c_str(),
		(*iter)->fromProvider().c_str(),
		(*iter)->name().c_str(),
		(*iter)->versionMajor(),
		(*iter)->versionMinor(),
		(*iter)->versionPatch());
	at(XINDEX(CPacketInfoComponent,packets)).arr->put(aExec, ind, UString(m1New(CString, buf)));
	ind++;
    }
    put(aExec, XINDEX(CPacketInfoComponent, serial), UString(m1New(CString, m1_main().m1Serial->c_str())));
}

void CPacketInfoComponent::execute(CExecutor* aExec)
{
    // Nuttin yet.
}
