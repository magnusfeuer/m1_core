type Test {
     event signed x;

     script x {
	    printf("PASSED\n");
     }
};

Test obj = Test {};
event unsigned y;
obj.x <- y; // Signed subsribes to unsigned.
y = 1;
.M1.halt = 2;

