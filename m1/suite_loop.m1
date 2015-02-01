/*
 *  Test of loop / break / continue
 */

printf("BREAK/CONTINUE: ");
{
   int i;
   int a;
   int b;

   foreach i in [1:100] {
       if (i == 10)
	   continue;
       a++;
       if (i == 50)
	   break;
       b++;
   }

   if ((i != 50) || (a != 49) || (b != 48))
       printf("FAILED\n");
   else
       printf("PASSED\n");
}

printf("FOREACH BASIC ARRAY: ");
{
    int i;
    int v[] = {1,2,3,4,5,6,7,8,9,10};
    int sum1 = 0;
    int sum2 = 55;

    foreach i in v { sum1 += i; }
    foreach i in v { sum2 -= i; }

    if ((sum1 != 55) || (sum2 != 0))
	printf("FAILED\n");
    else
	printf("PASSED\n");
}

printf("FOREACH OBJECT ARRAY: ");

{
    type Item { int value; };
    Item v[] = { @Item { value=1 }, @Item { value=2 },
		 @Item { value=3 }, @Item { value=4 },
		 @Item { value=5 }, @Item { value=6 },
		 @Item { value=7 }, @Item { value=8 },
		 @Item { value=9 }, @Item { value=10 }};
    Item i;
    int sum = 0;
    foreach i in v { sum += i.value; }
    
    if (sum != 55)
	printf("FAILED\n");
    else
	printf("PASSED\n");
}


.M1.halt = 1;

