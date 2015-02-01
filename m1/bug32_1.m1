//
// Class reference issue
//
type Base {
    type Enc {
	type SubEnc: Layer {
	    string name;
	};
    };

    Layer x = @Base:Enc:SubEnc { name = "hello world" };
};
Base b = @Base {};
printf("b.x = %p\n", b);
printf("Bug32_1: PASS\n");
.M1.halt = true;

