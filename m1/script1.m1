     type Base {
     	  event signed x;
     	  event signed y;

	  script x {  printf("Base:x %d\n", x); }
	  script y {  printf("Base:y %d\n", y); }
     };

     type Derived : Base {
     	  event signed z;
	  	 
	  script x { y = 10; printf("Derived:x %d\n", x); }
	  script z { printf("Derived:z %d\n", z); }
     };

     Derived a = @Derived {};
     a.x = 17;
}