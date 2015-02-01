
type Subclass: Layer {
     script width {
	    printf("Subclass: width=%f\n", width);
     }
};

type SubSubclass: Subclass {

     script width {
     	    if (width > 5)
	       width = 5;
     	    printf("SubSubClass: width=%f\n", width);
    }
};

SubSubclass x = SubSubclass {};

x.width = 10;


