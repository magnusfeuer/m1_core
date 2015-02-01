type MyClass: DDS {
    event int y = 0;
};

MyClass obj1 = @MyClass { y = 10 };
MyClass obj2 = copy(obj1);

printf("obj1 = %p\n", obj1);
printf("obj2 = %p\n", obj2);

if (obj1.y != obj2.y) 
    printf("bug35.m1: FAIL\n");
else
    printf("bug35.m1: PASS\n");

.M1.halt = true;
