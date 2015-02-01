//
// Type for switching children (skin switch / intrument switch )
//

//
// done <- @Timer{}.cycleTime
//
type SlowSwitchLayer : Layer {
     input event unsigned  doSwitch;

     private signed current = 0;
     private signed next    = 1;
     private unsigned size;
     private input event time doneSwitch;

     script button1Down when button1Down != 0 {
     	    printf("doswitch\n");
	    size = size(children);
	    if (size > 1) {
	       	    Timer t = Timer{ cycleInterval=1.0,enabled=false};
	    	    ScalarInterpolator tn = ScalarInterpolator { key={0,1},
		    		       	    		       keyValue={1,0}};
           	    printf("SlowSwitch current=%d, next=%d\n", current, next);
     	            children[current].transparency <- t.fraction;
	            children[next].transparency <- tn.value;

	            tn.fraction <- t.fraction;
	            doneSwitch <- t.cycleTime;
	            t.enabled = true;
	    }
    }

    script doneSwitch {
            printf("doneSwitch current=%d, next=%d, size=%d\n", 
	    		       current, next, size);
     	    children[current].transparency <- nil;
     	    children[next].transparency <- nil;
	    current = next;
	    next = (current + 1) % size;
     }

     printf("SlowSwitchLayer: init\n");
};

Timer t30 = Timer { cycleInterval = 1/30, loop = true };


Timer swt = Timer { cycleInterval = 2.0, loop = true };


SlowSwitchLayer layer  = 
      SlowSwitchLayer {
      	      top = 240,
      	      left = 320,
	      children = {
		  Video { transparency = 0,
		  	  loop = true,
		          background = false,
			  streamName = "../images/wg_gdo_1.mpg",
			  frameTime <- t30.cycleTime
		  },

	          Shape { contentWidth = 50, 
		  	  contentHeight = 50,
		  	  transparency = 1,
			  shape = 'r',
			  fill = true,
			  fillColor = 0xFF0000,
			  borderWidth = 4,
			  borderColor = 0x000000 
		  },

		  Image {  transparency = 1,
		           background = false,
		  	   backgroundColor = 0x00FF00,
			   imageFile = "../images/image.png",
			   borderWidth = 0 
		  },


		  Video { transparency = 1,
		  	  loop = true,
		          background = false,
			  streamName = "../images/wg_gdo_2.mpg",
			  frameTime <- t30.cycleTime
		  }
	        }
	     };
			  

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 0x000000,
	     children = { layer }
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

