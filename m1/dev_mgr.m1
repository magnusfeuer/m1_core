// 
// Device manager factory test
//
application Tst {
    // 
    // BaseProduct is the base class for all
    // products managed by class.
    //
    type DeviceHandler1: DeviceHandler {
	script DeviceHandler1 {
	    printf("DeviceHandler1::DeviceHandler1(): Created\n");
	}
    };

    // 
    // BaseProduct is the base class for all
    // products managed by class.
    //
    type DeviceHandlerProducer1: DeviceHandlerProducer {
	script trigger {
	    printf("DeviceHandlerProducer1::trigger: Called\n");

	}
	script DeviceHandlerProducer1 {
	    printf("DeviceHandlerProducer1::DeviceHandlerProducer1(): Created\n");
	}
    };

    //
    // Create a new factory with two producers.
    //
    DeviceManager manager = @DeviceManager { 
	fifoPath = "/dev/ttyusb_fifo", 
	producers = { 
	    @DeviceHandlerProducer1 {
		productPattern="M1/ZT2",
		manufacturerPattern="",
		serialPattern="",
		vendorIDPattern=0,
		productIDPattern=0
	    } 
	}
    };
}
