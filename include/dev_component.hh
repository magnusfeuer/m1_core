//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __DEV_COMPONENT_H__
#define __DEV_COMPONENT_H__

#include "component.hh"
#include "packfile.hh"
#include <stdio.h>
#include <sys/types.h>
#include <regex.h>

// CDeviceHandlerBase
//  An instance of a CDeviceHandlerComponent subclass (written in mpl) will be created
//  by the CDeviceManagerComponent when it is fed a matching string by the udev
//  subsystem.
//
class CDeviceHandlerBase: public CExecutable {
public:
    XOBJECT_TYPE(CDeviceHandlerBase,
		 "DeviceHandler",
		 "Device Handler Component",
		 (CDeviceHandlerBase_device, // Path to device
		  CDeviceHandlerBase_product, // Product string
		  CDeviceHandlerBase_manufacturer, // Manufacturer
		  CDeviceHandlerBase_serial, // Serial number
		  CDeviceHandlerBase_vendorID, // Vendor ID
		  CDeviceHandlerBase_productID), // Product ID.
		 //! Device path
		 XFIELD(CDeviceHandlerBase,Q_PUBLIC,device,
			string_type(),
			"Absolute path to device that triggered this object"),
		 //! product string
		 XFIELD(CDeviceHandlerBase,Q_PUBLIC,product,
			string_type(),
			"USB Product string "),
		 //! Manufacturer string
		 XFIELD(CDeviceHandlerBase,Q_PUBLIC,manufacturer,
			string_type(),
			"USB Manufacturer string "),
		 //! serial numbr
		 XFIELD(CDeviceHandlerBase,Q_PUBLIC,serial,
			string_type(),
			"USB serial string "),
		 //! Vendor ID
		 XFIELD(CDeviceHandlerBase,Q_PUBLIC,vendorID,
			unsigned_type(),
			"USB Vendor ID"),
		 //! Product id 
		 XFIELD(CDeviceHandlerBase,Q_PUBLIC,productID,
			unsigned_type(),
			"USB Product ID")
		 );
public:
    CDeviceHandlerBase(CExecutor* aExec,
		       CBaseType *aType = CDeviceHandlerBaseType::singleton());
    ~CDeviceHandlerBase(void);


    void execute(CExecutor* aExec);

private:
};


// CDeviceHandlerTrigger
//  Workaround since we cannot have 
//  event DeviceHandler arr[]; 
//  in m1 that triggers when arr is reassigned.
//
//  This object has a single member, owner, which is an DeviceHandler array.
//
//  The sequence is that CDeviceManagerComponent gets a new record from the udev.
//  CDeviceManager 
//  
class CDeviceHandlerTrigger: public CExecutable {
public:
    XOBJECT_TYPE(CDeviceHandlerTrigger,
		 "DeviceHandlerTrigger",
		 "Device Handler Component",
		 (CDeviceHandlerTrigger_owner), 
		 //! Owner array to place newly created object in.
		 XFIELD(CDeviceHandlerTrigger,Q_PUBLIC,owner,
			CArrayType::create(CDeviceHandlerBase::CDeviceHandlerBaseType::singleton(), 0),
			"Owner array"));
public:
    CDeviceHandlerTrigger(CExecutor* aExec,
			  CBaseType *aType = CDeviceHandlerTriggerType::singleton());
    ~CDeviceHandlerTrigger(void);

    void execute(CExecutor* aExec);

private:
    
};

//
// CDeviceHandlerProducerBase
//  Instances of CDeviceHandlerProducerBase subclasses are responsible
//  for producing variious CDeviceHandler instances
//  The subclass is expected to install a new instance 
//  of a CDeviceHandlerComponent at the end of the
//  CDeviceHandlerComponent array hosted by the trigger member
//  when trigger is updated to refer to a new object.
//
class CDeviceHandlerProducerBase: public CExecutable {
public:
    XOBJECT_TYPE(CDeviceHandlerProducerBase,
		 "DeviceHandlerProducer",
		 "Device Handler Producer base class",
		 (CDeviceHandlerProducerBase_trigger, // Trigger boolean.
		  //		  CDeviceHandlerProducerBase_collection, // All objects created by this producer.
		  CDeviceHandlerProducerBase_action, // Action that triggered the producer
		  CDeviceHandlerProducerBase_devicePath, // Product string that triggered this producer
		  CDeviceHandlerProducerBase_product, // Product string that triggered this producer
		  CDeviceHandlerProducerBase_manufacturer, // Manufacturer that triggered this producer
		  CDeviceHandlerProducerBase_serial, // Serial number that triggered this producer
		  CDeviceHandlerProducerBase_vendorID, // Vendor ID that triggered this producer
		  CDeviceHandlerProducerBase_productID, // Product ID that triggered this producer.

		  CDeviceHandlerProducerBase_productPattern, // Product string pattern that should trigger  producer
		  CDeviceHandlerProducerBase_manufacturerPattern, // Manufacturer pattern that should trigger  producer
		  CDeviceHandlerProducerBase_serialPattern, // Serial number that should trigger producer
		  CDeviceHandlerProducerBase_vendorIDPattern, // Vendor ID that should trigger this producer
		  CDeviceHandlerProducerBase_productIDPattern), // Product ID that should trigger this producer.
		 //! Trigger object
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC, trigger, 
			event_queue_bool_type(),
			"Trigger boolean that will check product/manufacturer/serial/vendorID/productID against pattern."),
		 //! Array to receive newly created objects.
// 		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC, collection, 
// 			CArrayType::create(CDeviceHandlerBase::CDeviceHandlerBaseType::singleton(), 0),
// 			"Array to receive objects created by this instance."),
		 //! product string that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,action,
			string_type(),
			"Action [A]dd device or [R]emove device. "),
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,devicePath,
			string_type(),
			"Device absolute path."),
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,product,
			string_type(),
			"USB Product string that triggered this producer. "),
		 //! Manufacturer string that triggered this producer. 
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,manufacturer,
			string_type(),
			"USB Manufacturer string that triggered this producer."),
		 //! serial numbr that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,serial,
			string_type(),
			"USB serial string that triggered this producer."),
		 //! Vendor ID that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,vendorID,
			unsigned_type(),
			"USB Vendor ID that triggered this producer."),
		 //! Product id  that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,productID,
			unsigned_type(),
			"USB Product ID that triggered this producer."),

		 //! product string that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC, productPattern,
			string_type(),
			"USB Product string pattern that should trigger this producer. "),
		 //! Manufacturer string that triggered this producer. 
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,manufacturerPattern,
			string_type(),
			"USB Manufacturer string pattern that should rigger this producer."),

		 //! serial numbr that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,serialPattern,
			string_type(),
			"USB serial string pattern that should  trigger this producer."),

		 //! Vendor ID that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,vendorIDPattern,
			unsigned_type(),
			"USB Vendor ID value that should trigger this producer."),

		 //! Product id  that triggered this producer.
		 XFIELD(CDeviceHandlerProducerBase,Q_PUBLIC,productIDPattern,
			unsigned_type(),
			"USB Product ID value that should trigger this producer.")
		 );
public:
    CDeviceHandlerProducerBase(CExecutor* aExec,
			   CBaseType *aType = CDeviceHandlerProducerBaseType::singleton());
    ~CDeviceHandlerProducerBase(void);

    void execute(CExecutor* aExec);
    
    // Functions to return the given pattern.
    const char *productPattern(void) { return at(XINDEX(CDeviceHandlerProducerBase, productPattern)).str->c_str(); }
    const char *manufacturerPattern(void) { return at(XINDEX(CDeviceHandlerProducerBase, manufacturerPattern)).str->c_str(); }
    const char *serialPattern(void) { return at(XINDEX(CDeviceHandlerProducerBase, serialPattern)).str->c_str(); }
    unsigned int vendorIDPattern(void) { return at(XINDEX(CDeviceHandlerProducerBase, vendorIDPattern)).u; }
    unsigned int productIDPattern(void) { return at(XINDEX(CDeviceHandlerProducerBase, productIDPattern)).u; }

    
    // Functions to set the data that triggered the producer.
    void setAction(CExecutor *aExec, char *aAction) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, action), UString(m1New(CString, aAction))); 
    }

    void setDevicePath(CExecutor *aExec, char *aDevicePath) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, devicePath), UString(m1New(CString, aDevicePath))); 
    }

    void setProduct(CExecutor *aExec, char *aProduct) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, product), UString(m1New(CString, aProduct))); 
    }

    void setManufacturer(CExecutor *aExec, char *aManufacturer) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, manufacturer), UString(m1New(CString, aManufacturer))); 
    }

    void setSerial(CExecutor *aExec, char *aSerial) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, serial), UString(m1New(CString, aSerial))); 
    }

    void setVendorID(CExecutor *aExec, unsigned long aVendorID) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, vendorID), UUnsigned(aVendorID)); 
    }
		 
    void setProductID(CExecutor *aExec, unsigned long aProductID) { 
	put(aExec, XINDEX(CDeviceHandlerProducerBase, productID), UUnsigned(aProductID)); 
    }

    void match(CExecutor *aExec, 
	       char *aAction,
	       char *aDevicePath,
	       char *aProduct, 
	       char *aManufacturer, 
	       char *aSerial,
	       unsigned long aVendorID,
	       unsigned long aProductID);

private:

	EventBool mTrigger;
    //    EventQueueObject<bool_type()> mTrigger;  // Trigger for this producer

    //
    // Regex time.
    //
    char mProductPattern[256];
    char mManufacturerPattern[256];
    char mSerialPattern[256];
    regex_t mProductRegex;
    regex_t mManufacturerRegex;
    regex_t mSerialRegex;
    bool mRegexInitialized;
};

// CDeviceManager 
//  Instances of this class are fed device names and data from the 
//  udev system when a new device is detected or removed by the system.
//  A CDeviceManagerComponent object also acts as a factory where various producers
//  can associate themselves with a specific device.
//  When udev reports a new device, all producers who match the given 
//  device pattern string will be triggered to produce a new instance.
//  These instances will be handed the device info and will be 
//  able to handle the new device.
//
class CDeviceManager : public CExecutable {
public:
    XOBJECT_TYPE(CDeviceManager,
 		 "DeviceManager",
 		 "Device manager",
 		 (CDeviceManager_fifoPath,   // Fifo to feed data into.
		  CDeviceManager_fifoRevents,
 		  CDeviceManager_producers), // Array of producers that can handle 
		  
 		 //
 		 // Path to Fifo fed by the udev subsystem
 		 // Each new device plugged in or unplugged will have
 		 // the following new-line separated fields
 		 // action                 [A for added. R for removed.]
 		 // path                   [Absolute path to device file (/dev/ttyUSB0)]
 		 // manufacturer string    [USB device manufacturer string]
 		 // product string         [USB device product string]
 		 // serial string          [USB device serial number]
 		 // manufacturer id        [USB manufacturer ID integer]
 		 // product id             [USB product ID integer]
 		 //
 		 XFIELD(CDeviceManager,Q_PUBLIC,fifoPath, input_string_type(), "Path to FIFO fed by udev system."),
		 // For reading FIFO
		 XFIELD(CDeviceManager,Q_PUBLIC,fifoRevents,
			input_signed_type(), "Fifo event trigger."), 

 		 //
 		 // Array of all producers to query if there is a 
 		 //
		 XFIELD(CDeviceManager,Q_PUBLIC,producers,
			CArrayType::create(CDeviceHandlerProducerBase::CDeviceHandlerProducerBaseType::singleton(), 0),
			"Producer array.")
		 );

public:
    CDeviceManager(CExecutor* aExec, CBaseType *aType = CDeviceManagerType::singleton());
    ~CDeviceManager(void);
    void execute(CExecutor* aExec);

private:
    bool processFifoData(CExecutor *aExec);
    bool setupFifoReader(CExecutor *aExec);
    int triggerProducers(CExecutor *aExec,
			 char *aAction,
			 char *aDevicePath,
			 char *aManufacturer,
			 char *aProduct,
			 char *aSerial,
			 int aManufacturerID,
			 int aProductID);

    EventString mFifoPath; // Path to fifo.o
    CFileSource* mFifoSource;  // File source for reading fifo
    EventSigned mFifoRevents; // FIFO events
    int mFifoDesc;
};


#endif
