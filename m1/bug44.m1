application Bug44 {

    type Base {
	event int ev = 0;

	script ev {
	    printf("Base:ev called: [%d]\n", ev);
	}
    };

    type Sub: Base {
	event bool set_ev;

	// Just sets ev to ten to trigger script chain.
	script set_ev when set_ev {
	    *ev = 10; // Triggers script chain incorrectly
	    printf("Sub:set_ev called [%d]\n", ev);
	}


	script ev {
	    printf("Sub:ev called [%d]\n", ev);
	}
    };

    type SubSub: Sub {
	script ev {
	    printf("SubSub:ev called[%d]\n", ev);
	}
    };

    SubSub obj = @SubSub {};
    
    obj.set_ev = true;
}
