//
// The 'x' script will be invoked twice for a single update.
//
type Test {
    event bool clicked;
    event bool trigger;

    script trigger {
        printf("Setting clicked to true\n");
    	clicked = true;
    }
};

type Owner {
    Test test = Test {};
    event bool x <- test.clicked;

    script x {
        printf("X updated. x=%d, cycle=%u\n", x, cycle());
    }
    printf("Owner: construct x=%d\n", x);
};

Owner obj = Owner {};
obj.test.trigger = true;
.M1.halt = 2;

