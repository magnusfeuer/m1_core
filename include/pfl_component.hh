//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __PFL_COMPONENT_H__
#define __PFL_COMPONENT_H__

#include "component.hh"
#include "packfile.hh"
#include <stdio.h>

// Setup a 1Mb chunk read size.
#define PFL_CHUNK_SIZE 1024*1024

class CPacketComponent : public CExecutable {
public:
    XOBJECT_TYPE(CPacketComponent,
		 "PacketManager",
		 "Packet manager",
		 (CPacketComponent_root,
		  CPacketComponent_dbDirectory,
		  CPacketComponent_fifo,
		  CPacketComponent_installState,
		  CPacketComponent_install,
		  CPacketComponent_packets,
		  CPacketComponent_failedDeps,
		  CPacketComponent_wrongTarget,
		  CPacketComponent_progress,
		  CPacketComponent_postAction,
		  CPacketComponent_fifoRevents,
		  CPacketComponent_inFileRevents),
		 //
		 // Root directory is the root from where everything is to be installed.
		 // Defaults to "/"
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,root, 
			string_type(), ""),

		 //
		 // DB directory is where the information on installed
		 // packages is stored. It will contain a number of files.
		 // with the packfile headers of all installed packets.
		 // Defaults to /m1/install_db
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,dbDirectory, 
			string_type(), ""),

		 //
		 // Points to a FIFO in the file system. When a directory is
		 // fed into this FIFO (terminated by NL), the packet component
		 // will scan the directory (non recursively) for all files ending in 
		 // .pfl and validate them.
		 //
		 // This fifo will be fed by the udev system when a USB memory
		 // stick is plugged in.
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,fifo,
			event_string_type(), ""),  

		 //
		 // Error can be set to the following values at the following times.
		 // Immediately after fifo has been written to:
		 // -1 - No packages are ready to install. 
		 // 0 - PFL files are ready for install.
		 // 1 - Cannot open directory or fifo.
		 // 2 - No pfl files found
		 // 3 - PFL file(s) have missing dependencies. See failedDeps for exact info.
		 // 4 - PFL file(s) have incorrect target device.
		 //
		 // When installState is 0 and install is set to true,
		 // the first file is then prepared for installation and
		 // installState is set to 10 (install in progress).
		 // Each time install is (re-)set to true, the next
		 // chunk is installed. 
		 // installState can be set to true until the following installState takes one of 
		 // the following values.
		 // 
		 // 
		 // 
		 // After install has been set to true to trigger install.
		 // 11 - Disk full
		 // 12 - Failed to copy .PFL file.
		 // 13 - Failed to write DB entry.
		 // 14 - Failed to extract or exec pre-script
		 // 15 - Failed to extract or exec post-script
		 // 16 - Previously available pfl was not available at install (USB stick pulled).
		 // 100 - Install complete.
		 XFIELD(CPacketComponent,Q_PUBLIC,installState,
			event_signed_type(), ""),

		 //
		 // Set install to true to trigger an install after installState is set to 0.
		 // Setting this to true at any other time will be ignored.
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,install,
			event_bool_type(), ""), 

		 //
		 // After error has been set to 0, this array will contain
		 // the packet ids (account@provider/packet/version) of all 
		 // packets that are ready to be installed.
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,packets,
			CArrayType::create(string_type(), 0), ""),

		 //
		 // If error is set to 3,
		 // This array will contain the id of all packets needed
		 // to satisfy all dependencies.
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,failedDeps,
			CArrayType::create(string_type(), 0), ""),

		 //
		 // If error is set to 4,
		 // This array will contain the id of all packets on the
		 // stick with incorrect targets.
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,wrongTarget,
			CArrayType::create(string_type(), 0), ""),

		 //
		 // After install has been set to true, but before error has been set to 100,
		 // progress will move from 0.0 to 1.0 to specify the install progress.
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,progress,
			output_float_type(), ""),

		 //
		 // If a post install action is needed, this 
		 // member will show what needs to be done
		 // 0 = none
		 // 1 = reboot
		 // 2 = m1 restart.
		 // 3 = m1 code reload.
		 // 4 = m1 plugin reload. (.so files).
		 //
		 XFIELD(CPacketComponent,Q_PUBLIC,postAction,
			output_signed_type(), ""),

		 // For reading FIFO
		 XFIELD(CPacketComponent,Q_PUBLIC,fifoRevents,
			input_signed_type(), ""), 
		 // For reading pack files
		 XFIELD(CPacketComponent,Q_PUBLIC,inFileRevents,
			input_signed_type(), "") 
	);

public:
    CPacketComponent(CExecutor* aExec, 
		     CBaseType *aType = CPacketComponentType::singleton());
    ~CPacketComponent(void);

    void start(CExecutor* aExec);
    void execute(CExecutor* aExec);

private:
    bool setupFifoReader(CExecutor*);
    EventString mFifoName;
    EventSigned mInstallState;
    EventBool mInstall;
    EventFloat mProgress;
    int mFifoDesc;
    EventSigned mPostAction;

    CFileSource* mFifoSource;  // File source for reading fifo
    EventSigned mFifoRevents; // FIFO events
    
    CFileSource* mInFileSource;  // File source for reading pack file
    EventSigned mInFileRevents; // Input file events.

    CInstallIterator *mInstallIter;
    unsigned char mBuffer[PFL_CHUNK_SIZE];
};


//
// Component used in the about box to list serial number
// and all intalled packages.
//
class CPacketInfoComponent : public CExecutable {
public:
    XOBJECT_TYPE(CPacketInfoComponent,
		 "PacketInfo",
		 "Packet Information",
		 (CPacketInfoComponent_serial,
		  CPacketInfoComponent_packets),
		 //
		 // The serial number of this unit.
		 //
		 XFIELD(CPacketInfoComponent,Q_PUBLIC,serial, string_type(), ""),

		 //
		 // All installed packets are listed here.
		 //
		 XFIELD(CPacketInfoComponent,Q_PUBLIC,packets, CArrayType::create(string_type(), 0), "")
	);

public:
    CPacketInfoComponent(CExecutor* aExec, CBaseType *aType = CPacketInfoComponentType::singleton());
    ~CPacketInfoComponent(void);

    void start(CExecutor* aExec);
    void execute(CExecutor* aExec);

private:
};





#endif
