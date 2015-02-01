//
//
//
type Class {
    int x;
};

// Create object
Class obj = Class { x = 0 };
Class ref; // Reference

ref = obj; // Set ref to reference obj.
ref = copy(obj); // Reassign ref to deep copy of obj.
ref.x = 2; // Update new object.

if (ref.x != obj.x) 
   printf("Bug 31: PASS\n");
else
   printf("Bug 31: FAIL. obj.x[%d] ref.x[%d]\n", obj.x, ref.x);

.M1.halt = true;
	

