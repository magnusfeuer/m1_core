//
// Loop
//

signed i;
signed a;
signed array[] = {2,4,6,8};

printf("VERSION = %s\n", .M1.version);

foreach i in [0:5] {
	byte j;
	foreach j in [0:10] {
		printf("i = %d, j=%d\n", i, j);
	}
}

// maybe allow {2,3,4,8} as expression ???
foreach a in  array {
	printf("a = %d\n", a);
	if (a > 4) break;  // break the foreach loop
}

.M1.halt = true;
