//
// Base class elements initialized through constructor list
// are not accessible by subclass elements that are initialized
// as they are delcared.
//
application Bug46 {

    //
    // External class references by BaseClass.
    //
    type SomeClass {
	string name;
    };


    // BaseClass 
    type BaseClass {
	SomeClass obj_ref;  // External object reference.
    };


    // Subclass 
    type SubClass: BaseClass {
	string some_class_name = (obj_ref != nil)?obj_ref.name:"bug46: does NOT pass."; // obj_ref is not initialized here!

	script SubClass {
	    printf("%s\n", some_class_name);
	}
    };

    // Create external object to be referenced in constructor list for SubClass.
    SomeClass some_obj = @SomeClass { name = "bug46: pass." };

    // CreateSubclass
    SubClass x = @SubClass { obj_ref = some_obj };


    .M1.halt = 3;
}
