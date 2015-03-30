//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006, 2007.
//
#include "dev_component.hh"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mount.h>
#include "m1vm.hh"
using namespace std;

XOBJECT_TYPE_BOOTSTRAP(CDeviceHandlerBase);
XOBJECT_TYPE_BOOTSTRAP(CDeviceHandlerTrigger);
XOBJECT_TYPE_BOOTSTRAP(CDeviceHandlerProducerBase);
XOBJECT_TYPE_BOOTSTRAP(CDeviceManager);


CDeviceHandlerBase::CDeviceHandlerBase(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType)
{
    put(aExec, XINDEX(CDeviceHandlerBase,device), UString(m1New(CString, (char*) "/")));
    put(aExec, XINDEX(CDeviceHandlerBase,manufacturer), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerBase,product), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerBase,serial), UString(m1New(CString, (char*) ""))); 
    put(aExec, XINDEX(CDeviceHandlerBase,vendorID), UUnsigned(0));
    put(aExec, XINDEX(CDeviceHandlerBase,productID), UUnsigned(0));
}


CDeviceHandlerBase::~CDeviceHandlerBase(void)
{
    printf("CDeviceHandlerBase::~CDeviceHandlerBase(): Called\n");

}



void CDeviceHandlerBase::execute(CExecutor* aExec)
{
    printf("CDeviceHandlerBase::execute(): Called\n");
}



CDeviceHandlerTrigger::CDeviceHandlerTrigger(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType)
{
    CArrayType* t  = CArrayType::create(CDeviceHandlerBase::CDeviceHandlerBaseType::singleton(), 0);
    CArray *a = new CArray(aExec, t, sizeof(CDeviceHandlerBase *), 0);

    put(aExec, XINDEX(CDeviceHandlerTrigger,owner), UArray(a));
}


CDeviceHandlerTrigger::~CDeviceHandlerTrigger(void)
{
    printf("CDeviceHandlerTrigger::~CDeviceHandlerTrigger(): Called\n");
    m1ReleaseArray(at(XINDEX(CDeviceHandlerTrigger,owner)).arr);
}


void CDeviceHandlerTrigger::execute(CExecutor* aExec)
{
    printf("CDeviceHandlerTrigger::execute(): Called\n");
}


CDeviceHandlerProducerBase::CDeviceHandlerProducerBase(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mTrigger(this),
    mRegexInitialized(false)
{
    mProductPattern[0] = mManufacturerPattern[0] = mSerialPattern[0] = 0;

    put(aExec, XINDEX(CDeviceHandlerProducerBase,action), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,devicePath), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,product), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,manufacturer), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,serial), UString(m1New(CString, (char*) ""))); 
    put(aExec, XINDEX(CDeviceHandlerProducerBase,vendorID), UUnsigned(0));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,productID), UUnsigned(0));

    put(aExec, XINDEX(CDeviceHandlerProducerBase,productPattern), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,manufacturerPattern), UString(m1New(CString, (char*) "")));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,serialPattern), UString(m1New(CString, (char*) ""))); 
    put(aExec, XINDEX(CDeviceHandlerProducerBase,vendorIDPattern), UUnsigned(0));
    put(aExec, XINDEX(CDeviceHandlerProducerBase,productIDPattern), UUnsigned(0));
    eventPut(aExec, XINDEX(CDeviceHandlerProducerBase, trigger), &mTrigger);
}


CDeviceHandlerProducerBase::~CDeviceHandlerProducerBase(void)
{
    printf("CDeviceHandlerProducerBase::~CDeviceHandlerProducerBase(): Called\n");
    if (mRegexInitialized) {
	regfree(&mProductRegex);
	regfree(&mManufacturerRegex);
	regfree(&mSerialRegex);
    }
}

void CDeviceHandlerProducerBase::match(CExecutor *aExec, 
				       char *aAction,
				       char *aDevicePath,
				       char *aProduct, 
				       char *aManufacturer, 
				       char *aSerial,
				       unsigned long aVendorID,
				       unsigned long aProductID) 
{
    int reg_err = 0;
    char errbuf[256];
    //
    // First check if we need to recompile our existing regexps.
    //
    if (!mRegexInitialized ||
	strcmp(mProductPattern, productPattern()) ||
	strcmp(mManufacturerPattern, manufacturerPattern()) ||
	strcmp(mSerialPattern, serialPattern())) {
	// Check if we should free old regex context
	if (mRegexInitialized) {
	    regfree(&mProductRegex);
	    regfree(&mManufacturerRegex);
	    regfree(&mSerialRegex);
	}
	
	// Setup new patterns.
	strncpy(mProductPattern, productPattern(), 256); mProductPattern[255] = 0;
	strncpy(mManufacturerPattern, manufacturerPattern(), 256); mManufacturerPattern[255] = 0;
	strncpy(mSerialPattern, serialPattern(), 256); mSerialPattern[255] = 0;

	//
	// Compile new regex pattern
	//
	if ((reg_err = regcomp(&mProductRegex, mProductPattern, REG_NOSUB | REG_EXTENDED)) != 0)  {
	    regerror(reg_err, &mProductRegex, errbuf, 256);
	    WARNFMT("CDeviceHandlerProducerBase::match(product): [%s] could not be regex compiled: %s",
		    mProductPattern, errbuf);
	}

	if ((reg_err = regcomp(&mManufacturerRegex, mManufacturerPattern, REG_NOSUB | REG_EXTENDED)) != 0) {
	    regerror(reg_err, &mProductRegex, errbuf, 256);
	    WARNFMT("CDeviceHandlerProducerBase::match(manufacturer): [%s] could not be regex compiled: %s",
		    mManufacturerPattern, errbuf);
	}

	if ((reg_err = regcomp(&mSerialRegex, mSerialPattern, REG_NOSUB | REG_EXTENDED)) != 0) {
	    regerror(reg_err, &mProductRegex, errbuf, 256);
	    WARNFMT("CDeviceHandlerProducerBase::match(serial): [%s] could not be regex compiled: %s",
		    mSerialPattern, errbuf);
	}
	mRegexInitialized = true;
	puts("New stuff initialized");
    }

    // 
    // Check if we have a match
    //
    if ((mProductPattern[0] == 0 || !regexec(&mProductRegex, aProduct, 0, 0, 0)) &&
	(mManufacturerPattern[0] == 0 || !regexec(&mManufacturerRegex, aManufacturer, 0, 0, 0)) &&
	(mSerialPattern[0] == 0 || !regexec(&mSerialRegex, aSerial, 0, 0, 0)) &&
	(aVendorID == 0 || vendorIDPattern() == 0 || aVendorID == vendorIDPattern()) &&
	(aProductID == 0 || productIDPattern() == 0 || aProductID == productIDPattern())) {

	//	CDeviceHandlerTrigger *trigger;

	// Setup the data that triggered the match.
	setAction(aExec, aAction);
	setDevicePath(aExec, aDevicePath);
	setProduct(aExec, aProduct);
	setManufacturer(aExec, aManufacturer);
	setSerial(aExec, aSerial);
	setVendorID(aExec, aVendorID);
	setProductID(aExec, aProductID);

	// Post a new trigger.
	mTrigger.putValue(NULL, true);
	printf("Trigger posted\n");
    }


}

void CDeviceHandlerProducerBase::execute(CExecutor* aExec)
{
    printf("CDeviceHandlerProducerBase::execute(): Called\n");
}


CDeviceManager::CDeviceManager(CExecutor* aExec, CBaseType *aType) :
    CExecutable(aExec, aType),
    mFifoPath(this),
    mFifoSource(0),
    mFifoRevents(this),
    mFifoDesc(-1)
{
    CArrayType* t  = CArrayType::create(CDeviceHandlerProducerBase::CDeviceHandlerProducerBaseType::singleton(), 0);

    CArray *a = new CArray(aExec, t, sizeof(CDeviceHandlerProducerBase *), 0);
    put(aExec, XINDEX(CDeviceManager,producers), UArray(a));

    
    eventPut(aExec, XINDEX(CDeviceManager,fifoPath), &mFifoPath);

    // Setup fifo event triggers..
    eventPut(aExec, XINDEX(CDeviceManager, fifoRevents), &mFifoRevents);
    mFifoSource = m1New(CFileSource, aExec);
    m1Retain(CFileSource, mFifoSource);
    connect(XINDEX(CDeviceManager, fifoRevents), 
	    mFifoSource, 
	    XINDEX(CFileSource, revents));

    mFifoPath.putValue(aExec, "");
}


CDeviceManager::~CDeviceManager(void)
{
    printf("CDeviceManager::~CDeviceManager(): Called\n");
    m1Release(CFileSource, mFifoSource);
    m1ReleaseArray(at(XINDEX(CDeviceManager,producers)).arr);
}

bool CDeviceManager::setupFifoReader(CExecutor* aExec)
{
    struct stat not_used;
    printf("mFifoName[%s]\n", mFifoPath.value().c_str());

    if (mFifoDesc != -1) {
	close(mFifoDesc);
	mFifoDesc = -1;
    }

    if (mFifoPath.value() == "")
	return false;

    // Create fifo if not therre.
    if (stat(mFifoPath.value().c_str(), &not_used) == -1 && 
	errno == ENOENT && 
	mkfifo(mFifoPath.value().c_str(), 0666) == -1) {
	printf("Could not create fifo [%s]\n", mFifoPath.value().c_str());
	mFifoPath.value() = "";
	mFifoPath.cancel(aExec);
	return false;
    }

    if ((mFifoDesc = open(mFifoPath.value().c_str(), O_RDONLY | O_NONBLOCK)) == -1) {
	printf("Could not open fifo[%s]\n", mFifoPath.value().c_str());
	return false;
    }
    printf("Using fifo[%s]\n", mFifoPath.value().c_str());

    mFifoSource->setDescriptor(aExec, mFifoDesc, POLLIN);
    return true;
}


//
// Trigger all producers matching the provided arguments
//
int CDeviceManager::triggerProducers(CExecutor *aExec,
				     char *aAction,
				     char *aDevicePath,
				     char *aManufacturer,
				     char *aProduct,
				     char *aSerial,
				     int aManufacturerID,
				     int aProductID) 
{
    CArray *producers = at(XINDEX(CDeviceManager, producers)).arr;
    unsigned int vec_size = producers->size();
    unsigned int i;

    for (i = 0; i < vec_size; ++i) {
	CObject* obj = producers->at(i).o;
	CDeviceHandlerProducerBase *prod = dynamic_cast<CDeviceHandlerProducerBase *>(obj);

	if (!prod) {
	    printf("CDeviceManager::triggerProducers(): Non CDeviceHandlerProducerBase object found in producer array. Ignored.\n");
	    continue;
	}
	prod->match(aExec, aAction, aDevicePath, aProduct, aManufacturer, aSerial, aManufacturerID, aProductID);

    }
    return 1;
}


				  

bool CDeviceManager::processFifoData(CExecutor *aExec)
{
    char buf[2050];  // extra nl and null
    int len;
    int tot_len = 0;
    int line_count = 0;
    char *lines[7];
    char *ptr;
    char *next;
    printf("CDeviceManager::processFifoData(): Called\n");
    
    while(tot_len < 2048 && 
	  (len = read(mFifoDesc, buf + tot_len, 2048 - tot_len)) > 0) 
	tot_len += len;

    // Ensure newline termination of last line
    if (buf[tot_len-1] != '\n') 
	buf[tot_len++] = '\n';
    
    buf[tot_len] = 0;
    // Divvy up the stuff into lines.
    ptr = buf;
    while ((next = strchr(ptr, '\n')) && line_count < 8) {
	lines[line_count++] = ptr;
	*next = 0; 
	ptr = next + 1;
    }

    if (line_count != 7) {
	printf("Need exactly 7 lines, got [%d].\n", line_count);
	goto fail;
    }

    for(int i = 0; i < line_count; ++i) 
	printf("Line[%s]\n", lines[i]);

    
    triggerProducers(aExec, 
		     lines[0], // Action [A]dd or [R]emove
		     lines[1], // Absolute dev path  (/dev/ttyUSB0)
		     lines[2], // USB device manufacturer string
		     lines[3], // USB device product string
		     lines[4], // USB serial string
		     atoi(lines[5]), // USB manufacturer ID
		     atoi(lines[6])); // USB product ID  
		     

    setupFifoReader(aExec); // Re-open fifo/
    return true;

 fail:
    setupFifoReader(aExec); // Re-open fifo/
    return false;
}


void CDeviceManager::execute(CExecutor* aExec)
{
    printf("CDeviceManager::execute(): Called\n");

    if (mFifoPath.updated()) {
	setupFifoReader(aExec);
    }	

    if (mFifoRevents.updated()) {
	processFifoData(aExec);
    }
}




