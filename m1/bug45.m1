application Bug45 {

    type Base {
	event int ev1 = 0;
	event int ev2 = 0;
	event int set_ev = 0;

	// Just sets ev to ten to trigger script chain.
	script set_ev when set_ev {
	    *ev1 = 1; 
	    *ev2 = 2;  // BUG Never triggers.
	    printf("set_ev called\n");
	}

	script ev1 {
	    printf("ev1 called: [%d]\n", ev1);
	}

	script ev2 {
	    printf("ev2 called: [%d]\n", ev2);
	}

    };


    Base obj = @Base {};
    
    obj.set_ev =1;
    .M1.halt = 3;
}
