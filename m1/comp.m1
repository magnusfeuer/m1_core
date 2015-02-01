//
// Comparison tests
//

signed i;
signed xi   = 2;
unsigned xu = 4;
float xf    = 6;
float xa    = 4;
float xff   = 11;
bool xt = 1;


if (xi < 3)
   printf("1:OK\n");
else
   printf("1:BUG\n");

if (xf < 7.0)
   printf("2:OK\n");
else
   printf("2:BUG\n");

if (xu < 5)
   printf("3:OK\n");
else
   printf("3:BUG\n");

if (xf + xa < xff)
   printf("4:OK\n");
else
   printf("4:BUG\n");

if (xf + xa <= xff)
   printf("5:OK\n");
else
   printf("5:BUG\n");

if (xff >= xf + xa)
   printf("6:OK\n");
else
   printf("6:BUG\n");

if (xf < xf+1.0)
   printf("7:OK\n");
else
   printf("7:BUG\n");

if (xf-1.0 < xf)
   printf("8:OK\n");
else
   printf("8:BUG\n");

if (xf > xf)
   printf("9:BUG\n");
else
   printf("9:OK\n");


.M1.halt = true;
