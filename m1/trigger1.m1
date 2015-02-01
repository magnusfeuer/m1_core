
type T {
     event int t;

     script t { printf("T.t = %d, trigger\n", t);  }

     script T { printf("T start\n"); t = 15; }

     script ~T { printf("T stop\n");  }
};


type Ta {
     event int a;

     script a { printf("Ta.a = %d, trigger\n", a); *a = 102; }

     script Ta { printf("Ta start\n"); *a = 101; }

     script ~Ta { printf("Ta stop\n");  }
};

T t = @T {};
Ta ta = @Ta {};

.M1.halt = 3;



