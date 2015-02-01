//
//
// Semantics of some syntax suger
//
//

// Calculate  o <- i(0,10) + 1  (where i in (0,1))
type MyExpr1 {
     event float i;  // input and output!
     output event float o;

     script i {
     	    o = i + 1;
     	    printf("MyExpr:i = %f, o = %f\n", i, o);
     }
};

type MySink {
     event input float i;

     script i {
     	    printf("MySink:i %f\n", i);
      }
};

Timer t1 = Timer { cycleInterval = 5.0, enabled=false };

ScalarInterpolator map = ScalarInterpolator { fraction <- t1.fraction, 
		       	 		      key={0,1}, keyValue={0,10}};

MyExpr1 expr = MyExpr1 { i <- map.value };

MySink sink = MySink { i <- expr.o };

t1.enabled = true;
