
type Foo {
     int   a;

     script Foo { printf("Foo %u created\n", this); }
     script ~Foo { printf("Foo %u destroyed\n", this); }
};

@Foo { a = 1 };

.M1.halt = 3;



