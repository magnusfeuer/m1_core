type BaseClass {
     public event float base_member = 0;
};


type OwnerClass {
    // The following line will create a core.
    public BaseClass owned = BaseClass {}; 

    // The following line will work.
    // public BaseClass owned = nil;
};

OwnerClass x = OwnerClass { owned = BaseClass { } };
printf("bug11.m1 PASS\n");
