//
//  Test object array + - operators
//
type Base {
     int b = 0;
};

type Derived : Base {
     int d = 1;
};

Derived a[] = {@Derived{d=1},@Derived{d=2},@Derived{d=3},@Derived{d=50}};
Derived b[] = {@Derived{d=100},@Derived{d=200},@Derived{d=300},@Derived{d=50}};
Base c[];

c = b - b[1];		printf("c=%p\n", c);
c = (a + b) - b[3];	printf("c=%p\n", c);
c = a + (b - b[3]);     printf("c=%p\n", c);

a += @Derived{d=6};     printf("a=%p\n", a);
a -= a[0];              printf("a=%p\n", a);
a += a;                 printf("a=%p\n", a);
a += b;                 printf("a=%p\n", a);
a -= a;                 printf("a=%p\n", a);

printf("a=%p\n", a);
printf("b=%p\n", b);
printf("c=%p\n", c);

.M1.halt = true;
 
