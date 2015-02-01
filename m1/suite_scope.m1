int n = 5;

type A {
	int a = 6;
	
	type B {
		int b = 11;

		script B { 
			printf("%d\n", b + a);	// BAD
			printf("%d\n",	b + n);	// OK
		}
	};
};

A:B ab = @A:B { b = 7 };

.M1.halt = true;

