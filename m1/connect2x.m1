//
// Connect variants
//


library Connect2x {
    event int    iparam = 13;
    string sparam = "Hello";

    event string foo;

    script foo {
	printf("Connect2x:foo %s\n", foo);
    }
    
    script Connect2x {
    	type T0 {
	     public input event int i0;
	     public string p0;
	     public event string out;

	    script {
	    	   printf("Connect2x:T0:script\n");
	     	   out = sprintf("Connect2: %d %s", i0, p0);
	    }
	    
	    script T0 {
	    	printf("Connect2:T0\n");
	        **out = sprintf("Connect2: %d %s", i0, p0);
	    }

	    script ~T0 {
	    	printf("Connect2:~T0\n");
	    }
	};

	printf("Connect2x\n");
	foo <- @T0 { i0 <- iparam, p0 = sparam}.out;

	.M1.halt = 1;
    }
}
