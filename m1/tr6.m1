
type MyBase {
     signed x;
     signed y;
};

type MyDerived : MyBase {
     signed z;
     signed t;
};

type MyFinal : MyDerived {
     float a;
     float b;
};

MyFinal f = MyFinal { x=1, y=2, z=3, t=4, a=5, b=6 };
printf("f=%p\n", f);
.M1.halt = true;
