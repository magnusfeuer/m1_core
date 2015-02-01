type Base {
};

type Owned {
    Base parent;
};

type Container: Base {
     Owned child;
};


Container obj = Container { 
    child = Owned { 
	parent = obj // obj is still nil.
    }
};

if (@Container(obj.child.parent) == obj)
    printf("PASSED\n");
else 
    printf("FAILED\n");
.M1.halt = true;
