//
// Test of aggregates
//

type MyAggregate {
     input event bool enabled = false;
     event float value;
     float start = 0.0;
     float stop  = 1.0;
     bool  growing = true;  // going from start => stop
     Timer t1 = Timer { cycleInterval = 3.0, enabled=false };
     ScalarInterpolator dir;
     event time cycleDone;

     script enabled when enabled {
     	    printf("Enabled\n");
     	    dir = ScalarInterpolator { key={0,1}, keyValue={start,stop} };
     	    t1.enabled = true;
	    cycleDone    <- t1.cycleTime;
	    dir.fraction <- t1.fraction;
	    value        <- dir.value;
     }
     
     script enabled when !enabled {
     	    printf("Disabled\n");
	    value <- nil;
     	    t1.enabled = false;
     }

     script cycleDone {
     	    printf("Cycle done\n");
	    if (growing) 
     	       dir.keyValue = {stop, start};
	    else
       	       dir.keyValue = {start, stop};
	    growing = !growing;
	    t1.enabled = true;  // re-enable timer
    }
};

MyAggregate my_aggr = MyAggregate { start = 10, stop = 100, enabled = true };

Shape blue_circle = 
    Shape { top  = 0,
	    left = 0,
	    contentWidth  <- my_aggr.value,
	    contentHeight <- my_aggr.value,
	    shape = 'e',
	    fill = true,
	    fillColor    = 0x0000ff,
	    transparency = 0.0,
	    borderWidth = 0
    };

Layer my_layer = Layer
      { left = 320,
      	top  = 240,
	enabled = true,
	children = { blue_circle }
      };
      	
Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "auto",
	     background = true,
	     backgroundColor = 0x000000,
	     framebufferDevice = "/dev/fb0",
	     children = {  my_layer }
};