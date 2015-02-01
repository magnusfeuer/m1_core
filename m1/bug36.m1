//
// Downcast is impossible
//
type MyLayer: Layer {
    int x;
};

Layer obj1;
MyLayer obj2;

obj1 = @MyLayer { x = 10 };
obj2 = @MyLayer(obj1); // Compile error: Downcast is not allowed!

if (obj2.x == 10) 
   printf("Bbu36 PASS!\n");
else
   printf("Bbu36 FAIL!\n");

.M1.halt = true;
