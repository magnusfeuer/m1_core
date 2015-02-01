type Trigger {
    event int i;
};

event int x;

type Test {
     // Why can we script on x here???
     script x when x == 1 {
	    printf("Scope mismatch!\n");
    }
};

x <- @Trigger { i = 1 }.i;
.M1.halt = true;