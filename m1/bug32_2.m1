//
// Same as bug32/bug32_1 but with a library
//
library Lib {
    type Base {
    	 type Enc {
	 	type SubEnc: Layer {
	 	   string name;
		};
    	  };
	 Layer x = @:Lib:Base:Enc:SubEnc { name = "hello world" };
    };

    script Lib {
	Base b = @Base {};
	printf("b.x = %p\n", b);
	printf("Bug32_1: PASS\n");
	.M1.halt = true;
    }
}

