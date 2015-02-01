library Test {
	ApexDevice dev = @ApexDevice { port = "/dev/ttyUSB0" };
	event int state <- dev.state;

	script state when state == 3 {
	    printf("Will set gap time to 3\n");
	    dev.iso_gap_time = 3;
	}
}
