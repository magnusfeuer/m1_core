//
// Test of switch statement
//

enum MyEnum { a, b, c, d, e };

MyEnum foo = d;
MyEnum array[] = { a,b,c,d,e };

type MyHalt {
     float waitTime;
     event time cycleDone;
     event bool start;
     Timer t1;

     script cycleDone { .M1.halt = true; }

     script start {
          t1 = Timer { cycleInterval = waitTime };
     	  cycleDone <- t1.cycleTime;
     }
};

MyHalt h = MyHalt { waitTime = 5.0 };

foreach foo in array {
  printf("foo = %d : ", foo);
  switch(foo) {
  default: printf("default ");
  case a: printf("a "); break;
  case b: printf("b ");
  case c: printf("c "); break;
  }
  printf("\n");
}

h.start = true;




	
     