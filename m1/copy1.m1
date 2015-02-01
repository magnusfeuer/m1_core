//
// Test clone and copy
//

type A {
     int v;
};


type B {
     A a1;
     A a2;
};


B b1 = @B { a1 = @A { v=1 }, a2 = @A { v = 2 } };
B b2;
B b3;

b2 = clone(b1);
b3 = copy(b1);
b1.a1.v = 13;

printf("b1 = %p\n", b1);
printf("b2 = %p\n", b2);
printf("b3 = %p\n", b3);

if ((b1.a1.v == b2.a1.v) && (b3.a1.v == 1))
   printf("PASS\n");
else
   printf("FAIL\n");
.M1.halt = true;


