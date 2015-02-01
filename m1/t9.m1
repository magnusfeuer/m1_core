
type A {
     signed ax = 17;

     type B {
          signed bx = 33 + ax;
     };

     B b = @B {};
};
A a = @A{};

printf("A.ax = %d\n", a.ax);
printf("A.b.bx = %d\n", a.b.bx);
.M1.halt = true;
