// 
// Factory test
//
application Tst {
    // 
    // BaseProduct is the base class for all
    // products managed by class.
    //
    type BaseProduct {
    };


    
    //
    // Trigger is used to trigger the
    // factory to create a new object.
    //
    type Trigger {
	string name;
	BaseProduct owner[];	    
    };


    // 
    // BaseProducer is the base class for all 
    // producers.
    //
    type BaseProducer {
	string name;
	event Trigger trigger;
    };

    //
    // Product1 is the first product that can be 
    // produced by the factory.
    //
    type Product1: BaseProduct {
	script Product1 {
	    printf("I am Product1\n");
	}
    };

    // Producer class for Product1.
    type Product1Producer: BaseProducer  {
	//
	// When trigger is set to a new object,
	// add a new instance of Product1 to the end of
	// the trigger.owner array.
	//
	script trigger when trigger != nil {
	    trigger.owner += @Product1 {};
	}

	
	//
	// Constrctor script that sets the symbolic
	// class name used to trigger the manufacturing
	// of a Product1 object.
	//
	script Product1Producer {
	    name = "Product1";
	}
    };

    // Product2 that can be produced by factory.
    type Product2: BaseProduct {
	script Product2 {
	    printf("I am Product2\n");
	}
    };

    // Producer class for Product2.
    type Product2Producer: BaseProducer  {
	script trigger when trigger != nil {
	    trigger.owner += @Product2 {};
	}

	script Product2Producer {
	    name = "Product2";
	}
    };

    //
    // The factory.
    //
    type Factory {
	BaseProducer producers[]; // List of all producers registered with factory.
	event queue Trigger trigger; // Trigger queue containing descriptions of objects to produce.

	//
	// Process the trigger object in front of queue
	//
	script trigger  {
	    BaseProducer p_walk;
	    foreach p_walk in producers {
		if (p_walk.name == trigger.name) 
		    // Tell the producer to add a new instance of "trigger.name" at end of trigger.owner.
		    p_walk.trigger = trigger; 
	    }
	}
    };

    //
    // Create a new factory with two producers.
    //
    Factory factory = @Factory { producers = { @Product1Producer {}, @Product2Producer {} }};

    //
    // All triggers handed to the factory will point to master_lst
    // so that the created objects can be found. Please note that
    // each trigger can point out a unique list to receive the
    // reference to the created object.
    //
    BaseProduct master_lst[] = {};

    // 
    // Ask the factory to produce a Product1 and Product2
    //
    factory.trigger = @Trigger { name = "Product1", owner = master_lst };
    factory.trigger = @Trigger { name = "Product2", owner = master_lst };
}
