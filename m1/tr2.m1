//
// Test recursive type and enums
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
     	    printf("%s", id);
    }
};

type MyUnaryExpr : MyExpr {
     MyOperator operator;
     MyExpr mid;
};

type MyBinaryExpr : MyExpr {
     MyOperator operator;
     MyExpr left;
     MyExpr right;
};

.M1.halt = true;
