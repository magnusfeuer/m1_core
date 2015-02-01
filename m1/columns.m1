//
// Read and write columns csv file data
//

float a1[4] = {1,2,3,4};
float b1[4] = {0,1,0,1};
float c1[4] = {1.2,3.4,5.6};
float m1[][] = { a1, b1, c1 };

float a2[] = {};
float b2[] = {};
float c2[] = {};
float m2[][] = { a2, b2, c2};

int rows;

rows = wcolumns("columns.output", ";", m1);

printf("wrote %d rows\n", rows);

rows = rcolumns("columns.output", ";", m2);

printf("read %d rows\n", rows);

printf("m2 = %p\n", m2);

.M1.halt = true;
