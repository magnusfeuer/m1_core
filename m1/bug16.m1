
type Trigger {
    event bool t;
};

type MyClass {
    event bool x;
    signed count = 0;
    script x {
	count = count + 1;
	if (count == 2) {
	    printf("PASS!\n");
	    .M1.halt = true;
	}
	    
	//The following line creates a sigseg.
	x <- @Trigger { t = true }.t; 
	 
    };
};

MyClass obj = MyClass {};

obj.x <- @Trigger { t = true }.t;

