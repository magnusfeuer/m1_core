
type A {
     int x = 0;
};

type B {
     event A obj;

     script obj {
       printf("Object has been assigned to [%p]\n", obj);
     }
};

B b = @B{};

b.obj = @A { x=13 };

.M1.halt = 2;
