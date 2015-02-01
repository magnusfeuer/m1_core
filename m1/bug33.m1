//
// Bug33.
// Deep copy is not recursive for memebers of the copied object.
//

type Class {
    int x[];
};

Class o1 = Class { x = { 1, 2, 3}  };
Class o2;

o2 = copy(o1);

o2.x[0] = 10;

if (o1.x[0] == 10) 
    printf("Bug33.m1 FAIL\n");
else
    printf("Bug33.m1 PASS\n");

.M1.halt = true;


	
