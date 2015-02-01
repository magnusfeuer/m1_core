//
// Example
//

const signed a = 10;

event signed value;

type Base {
     event signed y;
     event signed z;
     event signed x;
     signed b = 12;

     x <- (y + b)*z + a;
};

script value {
       printf("value = %d\n", value);
       .M1.halt = true;
}

// result should be (2 + 12)*3 + 10 == 52
value <- @Base { y=2, z=3 }.x;


