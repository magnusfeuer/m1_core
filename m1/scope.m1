//
//
//

type A {
    int a = 1;
    event bool run;
    event bool create;

    type B : A {
	int b = 2;

	script B { printf("a=%d, b=%d\n", a, b); }
    };

    B b;

    script create {
        b = @B {};
    }
};

A obj = @A { };

obj.create = true;

.M1.halt = 1;

