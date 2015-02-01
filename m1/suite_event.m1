//
//
//
application Suite {
    Timeout tmo;
    event bool timeout;
    signed n_value = 0;
    event bool trigger = false;

    script trigger {
	n_value += 1;
	*trigger = true;
    }

    script timeout {
	printf("Timout is done, triggered = %d\n", n_value);
	.M1.halt = true;
    }

    script Suite {
    	tmo = Timeout { duration = 5.0 };
	timeout <- tmo.timeout;
	*trigger = true;
    }
}



