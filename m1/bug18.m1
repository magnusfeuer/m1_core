type Base {
	 event signed base_x;
	 public signed b = 0;

	 script base_x {
	     b = b + 1;	     
	     printf("PASSED %d\n", b);
	 };
};

type Master {
     type Encapsulated: Base {
	 script base_x {
	     b = b + 1;
	     printf("Subclass script called, but not master.?\n");
	 };
     };

     event signed master_y;

     Encapsulated e_obj = Encapsulated {};

     e_obj.base_x <- master_y;
};

Master obj = Master {};

obj.master_y = 42;
.M1.halt = 2;


