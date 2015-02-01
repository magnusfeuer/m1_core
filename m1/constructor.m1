
type A {
     int x = 12;
     event float ev = 0.0;

     script A {
     	    printf("Construct A, x=%d\n", x);
     }

     script ~A {
     	    printf("Destruct A, x=%d\n", x);
     }

     script ev {
     	    printf("Event A:ev\n");
    }

    printf("TopLevel A\n");
};


type B : A {
     int x = 13;

     script B {
     	    printf("Construct B, x=%d, A.x=%d\n", x, @A.x);
     }

     script ~B {
     	    printf("Destruct B, x=%d, A.x=%d\n", x, @A.x);
     }

     script ev {
     	    printf("Event B:ev\n");
    }
};

B b = @B {};

b = nil;
.M1.halt = 2;

