//
// Script implementation of tmp_main
//
type SlowSwitch {
     Layer theLayer;
     input event unsigned doSwitch;
     input event bool hasFocus;
     private signed current = 0;
     private signed next    = 1;
     private unsigned size;
     private input event time doneSwitch;


     script doSwitch when doSwitch != 0 {
       	    Timer t = Timer { cycleInterval = 0.3, enabled=false };

    	    ScalarInterpolator tn = ScalarInterpolator { 
			       key={0,1},
			       keyValue={1,0}, 
			       fraction <- t.fraction
	    };

      	    printf("SlowSwitch: current[%d], next[%d]\n", current, next);

	    size = size(theLayer.children);

	    if (size <= 1) {
	       printf("Break time\n");
	       break;
	    }

	    theLayer.children[current].transparency <- t.fraction;
            theLayer.children[next].transparency <- tn.value;

            doneSwitch <- t.cycleTime;

            t.enabled = true;
    }

    script doneSwitch {
            printf("doneSwitch current=%d, next=%d, size=%d\n",  current, next, size);

	    doneSwitch <- nil;
     	    theLayer.children[current].transparency <- nil;
     	    theLayer.children[next].transparency <- nil;

	    current = next;
	    next = (current + 1) % size;
     }

     script hasFocus when hasFocus {
	    printf("Focus!\n");
     }

     script hasFocus when !hasFocus {
	    printf("No focus!\n");
     }

     printf("The size = %d\n", size);
};

Timer timer = Timer { cycleInterval = 2.0,  loop = true };


ScalarInterpolator cycle1 =
ScalarInterpolator {
   key = {0.0, 0.5, 1.0 },
   keyValue = { 0.0, 1.0, 0.0 },
   fraction <- timer.fraction
};

ScalarInterpolator cycle2 =
ScalarInterpolator {
   key = {0.0, 0.5, 0.6, 1.0 },
   keyValue = { 0.0, 0.5, 0.0, 0.0 },
   fraction <- timer.fraction
};

Layer background = 
    DDS { top = 0, left = 0,
	  enabled = true,
	  ddsFile = "../images/TB-BACKGROUND-BlueGlow.dds",
	  transparency = 0.0
};


Layer kph = DDS {
    top = 0, left = 0,
    ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
    value       <- cycle1.value
};

Layer mph = DDS {
    top = 0, left = 0,
    ddsFile = "../images/BlueGlow-TB-Small-Incre-Normal.dds",
    value       <- cycle1.value
};

Layer top_left = Layer {
      top = 23, left = 43,
      height = 100, width = 100,
      wantFocus = true,
      children = { kph, mph }
};

Layer top_right = 
	DDS {
	    top = 23, left = 444,
	    enabled = true,
	    ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	    value       <- cycle1.value,
	    transparency <- cycle1.value
};

Layer bot_left = 
	DDS {
	    top = 247, left = 18,
	    enabled = true,
	    ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	    value       <- cycle1.value,
	    wantFocus = true
};

Shape bot_left_focus = 
  Shape {   
            top  = 247,
	    left = 18,
	    contentWidth  = 100,
	    contentHeight = 100,
	    shape = ellipse,
	    fill = true,
	    fillColor = 0xFF0000,
	    transparency = 0.0,
	    borderWidth = 0
};

Layer bot_right = 
	DDS {
	    top = 247, left = 469,
	    enabled = true,
	    ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	    value       <- cycle1.value,
	    transparency <- cycle2.value
};

Layer instrument =
    Layer { top = 0, left = 0,
	    height = 480, width = 639,
    	    enabled = true,
	    children = { top_left, top_right,
		         bot_left_focus, bot_left, bot_right }
};

Screen s = Screen { height = 480,  
       	   	    width  = 640,
		    pixelType = "bgra",
		    backendType = "x11",
		    framebufferDevice = "/dev/fb0",
		    children = {  background, instrument  } 
		  };

/**
Screen s2 = Screen { height = 480,  
       	   	    width  = 640,
		    pixelType = "bgra",
		    backendType = "x11",
		    framebufferDevice = "/dev/fb0",
		    children = {  background, instrument  } 
		  };
**/



InputStrategy input_strategy = InputStrategy {
	       layer = s
};

EpicInputDevice touch = EpicInputDevice {
	    inputStrategy = input_strategy,
	    screen = s, // Used to extract mouse
	    maxUnscaledX = 640,
	    maxUnscaledY = 480,
	    minUnscaledX = 0,
	    minUnscaledY = 0,
	    scaledWidth = 640,
	    scaledHeight = 480
};

SlowSwitch bot_left_sw = SlowSwitch { theLayer = top_left, doSwitch <-
top_left.button1Down, hasFocus <- top_left.hasFocus};
