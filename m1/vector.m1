//
// Defintion of event vector
//

type Vector3f {
     event bool is_updated;
     event float x;
     event float y;
     event float z;

     script { is_updated = true; }
};

type Vector3fDot {
     public output event float value;
     public input event Vector3f a;
     public input event Vector3f b;

     private event bool a_updated;
     private event bool b_updated;
     private Vector3f ao;
     private Vector3f bo;

     script a when (a != ao) {
     	    printf("a is set\n");
     	    ao = a;
	    if (a == nil)
	       a_updated <- nil;
	    else 
	       a_updated <- a.is_updated;
    }

     script b when (b != bo) {
     	    printf("a is set\n");
     	    bo = b;
	    if (b == nil)
	       b_updated <- nil;
	    else 
	       b_updated <- b.is_updated;
    }

     script a || b || a_updated || b_updated {
     	    printf("%p * %p \n", a, b);
     	    if ((a == nil) || (b == nil))
	       value = 0.0;
	    else
	       value = a.x*b.x + a.y*b.y + a.z*b.z;
     }
};

Timer t = Timer { cycleInterval = 1 };

Vector3f x = Vector3f { x <- t.fraction, y = 2, z = 3 };
Vector3f y = Vector3f { x = 10, y = 11, z <- 1- t.fraction };

Vector3fDot m = Vector3fDot { a = x, b = y};

event float m_value <- m.value;
event time  done <- t.cycleTime;

script m_value {
       printf("m.value = %f\n", m_value);       
}

script done {
       .M1.halt = true;
}
