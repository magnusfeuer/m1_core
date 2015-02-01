//
// Test pre+ post- scripts
//

type Base {
    event bool foo;

    script foo {
     	    printf("Base:foo\n");
    }

};

type Derived : Base {


    +script foo {
     	    printf("pre- Derived:foo\n");
    }

    -script foo {
     	    printf("post - Dervied:foo\n");
    }

};

type Sub : Derived {

    +script foo {
     	    printf("pre- Sub:foo\n");
    }

    -script foo {
     	    printf("post - Sub:foo\n");
    }
};

Sub obj = @Sub {};

obj.foo = true;

.M1.halt = 1;
