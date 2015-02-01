//
// Test recursive type INHERITANCE and enums
//

enum MyOperator {
     plus,
     minus,
     times = 1,
     divide = 2,
     remainder
};

type MyExpr {
     input event bool print;
};

type MyVariable : MyExpr {
     string id;

     script print {
     	    printf("MyVariable: id=%s\n", id);
    }
};

type MyUnaryExpr : MyExpr {
     MyOperator operator;
     MyExpr mid;

     script print {
     	    if (operator == plus)
	       printf("MyUnary: +\n");
	    else if (operator == minus)
	       printf("MyUndary: -\n");
	    mid.print = true;
    }
};

type MyBinaryExpr : MyExpr {
     MyOperator operator;
     MyExpr left;
     MyExpr right;

     script print {
     	    if (operator == plus)
	       printf("MyBinary: +\n");
	    else if (operator == minus)
	       printf("MyBinary: -\n");
	    else if (operator == times)
	       printf("MyBinary: *\n");
	    else if (operator == divide)
	       printf("MyBinary: /\n");
	    else if (operator == remainder)
	       printf("MyBinary: %\n");
	    left.print = true;
	    right.print = true;
    }
};

type MyHalt {
     input event time done;

     script done { .M1.halt = true; }
};


MyExpr expr1 =
       MyBinaryExpr {
       		    operator = plus,
		    left = MyVariable { id = "foo" },
		    right = MyUnaryExpr { operator = minus,
		    	    		  mid = MyVariable { id = "bar" }
					}
		    };


Timer t = Timer { cycleInterval = 2.0, enabled = true };
MyHalt aHalt = MyHalt { done <- t.cycleTime };

expr1.print = true;

