//
// Class reference issue
//
type Base {
    type Enc {
	type SubEnc: Layer {
	    string name;
	};
    };

    Layer x = @Enc:SubEnc { name = "hello world" };
};
Base b = @Base {};
printf("b.x = %p\n", b);
printf("Bug32: PASS\n");
.M1.halt = true;

