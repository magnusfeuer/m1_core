//
// Array test
//

signed ivec[10];
float  matrix[3][3] = { {1,0,0}, {0,1,0}, {0,0,1} };
float  dynamic[] = {};
Point dpoints[];
Point points[4];
string svec[3] = { "a", "b", "c"};
string dsvec[] = { "x", "y", "z" };

type Foo {
     Point pos = @Point { x=1, y=2 };
     Point v[3] = { @Point { x=1, y=2 } };
     Rect  r;
};

signed i;

ivec[0] = 1;
ivec[1] = 2;
ivec[2] = 3;

foreach i in [0:2] {
	printf("ivec[%d]=%d\n", i, ivec[i]);
}
printf("\n");

printf("matrix = %p\n", matrix);
printf("{ ");
foreach i in [0:2] {
	signed j;
	printf("{");
	foreach j in [0:2] {
	   printf("%f,", matrix[i][j]);
	}
	printf("}\n");
}
printf("}\n");

matrix[0][0] = 1;
matrix[0][1] = 2;
matrix[0][2] = 3;
matrix[1][0] = 4;
matrix[1][1] = 5;
matrix[1][2] = 6;
matrix[2][0] = 7;
matrix[2][1] = 8;
matrix[2][2] = 9;

printf("matrix = %p\n", matrix);
printf("{ ");
foreach i in [0:2] {
	signed j;
	printf("{");
	foreach j in [0:2] {
	   printf("%f,", matrix[i][j]);
	}
	printf("}\n");
}
printf("}\n");


// Test empty vector
printf("size(dynamic)=%d\n", size(dynamic));
foreach i in [0:size(dynamic)-1] {
	printf("dynamic[%d]=%f\n", i, dynamic[i]);
}

dynamic = { 1, 2, 3 };
// Test dynamic vector
printf("size(dynamic)=%d\n", size(dynamic));
foreach i in [0:size(dynamic)-1] {
	printf("dynamic[%d]=%f\n", i, dynamic[i]);
}

dynamic[3] = 1.2;
dynamic[5] = 2.3;
printf("size(dynamic)=%d\n", size(dynamic));
foreach i in [0:size(dynamic)-1] {
	printf("dynamic[%d]=%f\n", i, dynamic[i]);
}

dynamic = { };
// Test dynamic vector
printf("size(dynamic)=%d\n", size(dynamic));
foreach i in [0:size(dynamic)-1] {
	printf("dynamic[%d]=%f\n", i, dynamic[i]);
}

dynamic = nil;

// Test dynamic record arrays
printf("1: dpoints=%p\n", dpoints);
dpoints = { @Point { x=1, y=2 }, @Point { x=2, y=3 }};
printf("2: dpoints=%p\n", dpoints);

// Test fixed record arrays
printf("1: points=%p\n", points);
points = { @Point { x=1, y=2 }, @Point { x=2, y=3 }, 
           @Point { x=3, y=4}, nil };

printf("2: points=%p\n", points);

printf("Foo: %p\n", @Foo {});

// Test vector ops
foreach i in [0:size(ivec)-1] { ivec[i] = i; }
printf("ivec: %p\n", ivec);
reverse(ivec);
printf("reverse(ivec): %p\n", ivec);
reverse(ivec);
printf("reverse(ivec): %p\n", ivec);
foreach i in [-3:3] {
	rotate(ivec, i);
	printf("rotate(ivec): %p\n", ivec);
}
reverse(ivec);
printf("reverse(ivec): %p\n", ivec);
sort(ivec);
printf("sort(vec): %p\n", ivec);

printf("svec = %p\n", svec);

printf("dsvec = %p\n", dsvec);
dsvec = { "hello", "world" };
printf("dsvec = %p\n", dsvec);

.M1.halt = true;
