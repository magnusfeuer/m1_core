//
//
//
library Bug42 {
	event int evi;
	event bool evb;

	script Bug42 {
	       evi <- evb;   // loosing reference here
	       .M1.halt = 3;
	}
}
