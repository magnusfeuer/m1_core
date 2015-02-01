
library Lib3 {
    public int i = 5;

    private event time setit;
    private :Lib2:T t = @:Lib2:T { };

    type T3 : :Lib2:T2 {
    	 private int t3 = 2;

	 script run {
	 	printf("Lib3:T3:run: t3=%d, i=%d\n", t3, i);
	}
    };

    type Trigger {
    	 setit = true;
    };

    script setit {
    	printf("Lib3:setit called\n");
    	.Lib2.var.value = i++;
    	t.run = true;
    }	

    script Lib3 {
    	@Trigger {};
	printf("Lib3 initialized\n");
    }
}
