//
// Event queue test
//

event queue int mq_a;
event queue int mq_b;

type Trigger {
     int i;
     foreach i in [1:10:2] { mq_a = i; }
     foreach i in [2:10:2] { mq_b = i; }
};

script mq_a && mq_b {
       printf("%d+%d = %d\n", mq_a, mq_b, mq_a+mq_b);
}

@Trigger {};

.M1.halt = 10;


