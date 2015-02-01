//
// dds_perf
//
//   check performance of different dds files
//
application DDSPerf {

    const int number_of_tests = 6;

    const string ptype = "bgra";

    string test_name[] = {
	"Blank", 
	"Background.dds", 
	"Gauge", 
	"Background.png", 
	"Background&Gauge.dds",
	"Font"
    };

    DDS dds_bg = @DDS { top = 0, left = 0, transparency = 0,
		   ddsFile = "../images/TB-BACKGROUND-BlueGlow_"+ptype+".dds" };
    DDS dds_gauge = 
	    @DDS { top = 81, left = 168, transparency = 0,
		   ddsFile = "../images/BlueGlow-TB-Big-Analog-MPH.dds" };

    Image png_bg = @Image { top = 0, left = 0, transparency = 0,
		        pixelType = ptype,
			imageFile = "../images/TB-BACKGROUND-BlueGlow.png" };

    Text txt_1 = @Text { top = 0, left = 0,
		    halign = center,
		    valign = center,
		    fontName = "Radio Space",
		    fontSize = 23,
		    fontWeight = medium,
		    fontColor = 0x00FF00,
		    text  = "0123456789ABCDEFGHIJLKMNOPQRSTUVXYZ" };

    int  test_count;
    bool first_time = true;
    int first_test;
    int last_test;
    int current_test;

    int nsamples = 0;
    float afps = 0.0;

    event bool next_test;
    Timeout test_timer;
    event time sample;

    event float current_fps;


    Screen screen;

		   
// Mac performance (2.33 GHz Intel core 2 duo, memory 667 MHz DDR2 SDRAM)
// backend: x11
//     0:   117 fps  (blank)???
//     1:   126 fps  (dds background)
//     2:   105 fps  (dds gauge)
//     3:   114 fps  (png background)
//     4:   100 fps  (dds background & dds gauge)
//     5:            (centered text Hello world )
//
    script sample {
	afps += .M1.fps;
	nsamples++;
	current_fps = afps / nsamples;
    }

    script next_test {
	if (first_time) 
	    first_time = false;
	else {
	    printf("TEST '%s': fps = %f\n",
		   test_name[current_test], afps / nsamples);
	    test_count++;
	    if (test_count == (last_test-first_test+1)*3)
		.M1.halt = 1;
	    current_test++;
	}
	if (current_test > last_test)
	    current_test = first_test;
	afps = 0.0;
	nsamples = 0;
	next_test <- test_timer.timeout;
	test_timer.duration = 3.0;
	test_timer.enabled = true;

	switch(current_test) {
	case 0:
	    screen.background = true;
	    screen.children = { };
	    break;

	case 1:
	    screen.background = false;
	    screen.children = { dds_bg };
	    break;

	case 2:
	    dds_gauge.value <- @Timer { cycleInterval= 5, autoDisconnect=true }.fraction;
	    screen.background = true;
	    screen.children = { dds_gauge };
	    break;

	case 3:
	    screen.background = false;
	    screen.children = { png_bg };
	    break;

	case 4:
	    dds_gauge.value <- @Timer { cycleInterval= 5, autoDisconnect=true }.fraction;
	    screen.background = false;
	    screen.children = { dds_bg, dds_gauge };
	    break;

	case 5:
	    screen.background = true;
	    txt_1.height = screen.height;
	    txt_1.width  = screen.width;
	    screen.children = { txt_1 };
	    break;
	    
	default:
	    .M1.halt = true;
	    break;
	}
    }

    script DDSPerf {
	string be = getenv("M1_BACKEND");
	string fixed_test = getenv("DDS_TEST");

	if (be == "")
	    be = "x11";

	if (fixed_test == "") {
	    first_test = 0;
	    last_test  = number_of_tests-1;
	}
	else {
	    first_test = atoi(fixed_test);
	    last_test = first_test;
	}
	printf("FIRST=%d, LAST=%d\n", first_test, last_test);
	current_test = first_test;

	test_timer = @Timeout { duration = 0.01 };
	next_test <- test_timer.timeout;
	test_count = 0;

	sample <- @Timer { cycleInterval = 0.5, loop = true }.cycleTime;

	screen = @Screen { height = 480, width  = 640,
			   pixelType = ptype,
			   backendType = be,
			   background  = true,
			   backgroundColor = 0xffffff,
			   frameRate = 100000,
			   children = {  }
	};
    }

}
