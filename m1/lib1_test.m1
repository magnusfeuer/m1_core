//
// Test file scope
//

int count = 5;

type Q : :Lib1:T {

     script Q {
     	    printf("*:Q: count=%d Lib1.count=%d\n", count, .Lib1.count);
     }
};

:Lib1:T obj1;
Q obj2;

obj1 = @:Lib1:T {};
obj2 = @Q {};

// printf("%d\n", obj1.count);
printf("%d\n", .Lib1.count);
*obj1.print = true;

.M1.halt = 2;
