
library Lib2 {
    public int j = 7;
    public Psigned var;

    type T {
	public event bool run;

	script run {
	    printf("Lib2:run called\n");
	    .Lib2.j = 13;
	    printf("var.value=%d, j=%d\n", var.value, j);
	}
    };

    type T1 {
	 public event bool run;
 	 private int t1 = 1;

	 script run {
	 	printf("Lib2:T1:run: t1=%d, j=%d\n", t1, j);
	}
   };

    type T2 : T1 {
	 private int t2 = 1;

	 script run {
	 	int k = 3;
	 	printf("Lib2:T2:run: t2=%d, j=%d, k=%d\n", t2, j, k);
	}
    };

    script Lib2 {
	var = @Psigned { key = "test_a1.var", value = -1 };
	printf("Lib2 initialized\n");
    }
}




