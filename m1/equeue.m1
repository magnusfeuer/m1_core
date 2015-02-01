//
// Test object event queue
//

type Object {
     int id = 0;
     script Object { printf("Object %d created\n", id); }
     script ~Object { printf("~Object %d destroyed\n", id); }
};

event queue Object q1;
Object storage[];
int i;

script q1 {
       if (q1 == nil)
	   printf("a1: got nil\n");
       else if (q1.id > 0) {
	   printf("a1: id=%d, ref=%u\n", q1.id, refcount(q1));
	   storage[q1.id] = q1;
       }
       else {
	   printf("a1: id=%d, ref=%u\n", q1.id, refcount(q1));
	   storage[-q1.id] = nil;
       }
}

*q1= nil;  // Get it started!!!

// Put elements into queue
foreach i in [1:10] {
	q1 = @Object { id = i };
}
q1 = nil;

// Remove elements from queue
foreach i in [1:10] {
	q1 = @Object { id = -i };
}



.M1.halt = 30;








