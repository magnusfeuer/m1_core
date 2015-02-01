//
// Connect variants
//


library Connect2 {
    event int    iparam = 13;
    string sparam = "Hello";

    event string foo;

    script foo {
	printf("foo = %s\n", foo);
    }
    
    script Connect2 {
	printf("Connect2\n");
	foo <- sprintf("Connect2: %d %s", iparam, sparam);
    }

    .M1.halt = 3;
}
