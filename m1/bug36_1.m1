//
// Cast should be impossible
//

type A : Rect {
     int a;
};

type B : Point {
     int b;
};

A a;
B b;

a = @A { a = 1};
b = @B(a);

.M1.halt = true;
