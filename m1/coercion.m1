type Test {
     signed   ai  = 2;
     unsigned au  = 3;
     float    af  = 5.0;
     byte     ab  = 1;
};

Test t = @Test {};
     
signed xi = 17;
float  xf = 3.0;
float  rf = xi*xf;
float  tf = 1 - t.ai * t.af;
byte   xb = xi + xf;

printf("x1*y1 = %f\n", rf);
printf("t.ai* t.af = %f\n", tf);

.M1.halt = true;

