//
//
//
library Stack {
    event int foo = 1;
    int a = 1;
    int b = 2;

    script foo {
    	int x = 23;
	int y = 12;

	foo = x * y;
    }

    script Stack {
	int c = a + b;

	{ 
	    int d = c * a;
	    const int e = 12;

	    printf("d=%d, e=%d\n", d, e);
	}

	{
	    int e = b * c;

	    printf("e=%d\n", e);

	}
	
	{
	    int a = a * b; // FIXME the a shold be taken from above context?

	    printf("a1=%d\n", a);
	}

	.M1.halt = true;
    }
}
