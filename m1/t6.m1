
type TmpExpr {
     event input float x1;
     event input float x2;
     event output bool value;
     private unsigned t1 = 12;
     private float    t2 = 1.0;

     script {
     	    value = (x1 > x2);
	    printf("TmpExpr: %f > %f = %d\n", x1, x2, value);
    }
};

Timer t1 = Timer { loop = true };
ScalarInterpolator s1 = 
    ScalarInterpolator { fraction <- t1.fraction,
			 key = {0.0, 0.1},
			 keyValue = {1.0, 100.0}};

ScalarInterpolator s2 = 
    ScalarInterpolator { fraction <- t1.fraction,
			 key = {0.0, 0.1},
			 keyValue = {100.0, 1.0}};

TmpExpr x1 = TmpExpr { x1 <- s1.value,
		       x2 <- s2.value };

x1.value;

		       


    
    


