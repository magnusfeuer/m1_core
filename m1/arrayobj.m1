//
// Fun with arrays and objects.
//
type MyClass {
    string name;
    int val;
};

MyClass arr[] = { 
    @MyClass { name = "first", val = 1 },
    @MyClass { name = "second", val = 2 },
    @MyClass { name = "third", val = 3 }
};

event int c_ind;

event MyClass current <- arr[c_ind];

script current {
    printf("Current element is name[%s] val[%d]\n", current.name, current.val);
}

*c_ind = 2;

 
 
