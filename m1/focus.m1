//
// Script implementation of tmp_main
//

Timer timer = Timer { cycleInterval = 5.000, loop = true };

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

Layer top_left = 
	DDS {
	    top = 23, left = 43,
	    enabled = true,
	    ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	    value       <- cycle1.value,
	    transparency <- cycle1.value,
	    wantFocus = true
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
	    transparency <- cycle2.value
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
	    height = 480, width = 640,
    	    enabled = true,
	    children = { top_left, top_right,
	                 bot_left, bot_right }
};

Screen s = Screen { height = 480,  
       	   	    width  = 640,
		    pixelType = "bgra",
		    backendType = "x11",
		    framebufferDevice = "/dev/fb0",
		    children = {  background, instrument  } 
		  };


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
