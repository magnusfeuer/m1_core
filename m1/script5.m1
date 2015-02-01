//
//
//

type EType {
     const signed a = 10;
     input event float t;
     event float x;
     input event float y <- x * x;
     
     output event float z;

     script y {
     	    z = y+a;
	    printf("z=%f\n", z);
     }

     x <- @Timer { cycleInterval=3, loop=true }.fraction +
          @Timer { cycleInterval=5, loop=true }.fraction + t;
};

@EType { a = 0, t <- @Timer { cycleInterval=17}.fraction };
