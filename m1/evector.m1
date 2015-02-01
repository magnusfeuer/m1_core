//
// Tests with event vector
//

event int b[] = {3,4,5};

script b[0] {
       printf("b[0] = %d\n", b[0]);
}

script b[1] {
       printf("b[1] = %d\n", b[1]);
}

printf("b = %p\n", b);

*b[1] = 2;
*b[0] = 1;

.M1.halt = 1;
