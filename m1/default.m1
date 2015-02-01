//
// Changing default on parent type
//

type A {
     signed x = 1;
};

type X {
     signed x = 1;
};

type B : A {
     signed y = 2;
};

type C : B {
     signed x = 10;
     signed @A.x = 3;
     signed @B.y = 3;
     signed z = 7;
};

printf("%p\n", @C { });

.M1.halt = true;
