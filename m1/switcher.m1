//
// Type for switching children (skin switch / intrument switch )
//

type FastSwitch {
     Layer theLayer;
     input event time doSwitch;

     private signed current = 0;
     private signed next    = 1;
     private unsigned size;

     script doSwitch  {
     	    size = size(theLayer.children);
	    if (size > 1) {
	       printf("FastSwitch current=%d, next=%d\n", current, next);
	       theLayer.children[current].transparency = 1.0;
	       theLayer.children[next].transparency = 0.0;
	       current = next;
	       next = (current + 1) % size;
	    }
     }
};
//
// done <- @Timer{}.cycleTime
//
type SlowSwitch {
     Layer theLayer;
     input event time doSwitch;

     private signed current = 0;
     private signed next    = 1;
     private unsigned size;
     private input event time doneSwitch;

     script doSwitch {
	    size = size(theLayer.children);
	    if (size > 1) {
	       	    Timer t = Timer { enabled=false };
	    	    ScalarInterpolator tn = ScalarInterpolator { key={0,1},
		    		       	    		       keyValue={1,0}};
           	    printf("SlowSwitch current=%d, next=%d\n", current, next);
     	            theLayer.children[current].transparency <- t.fraction;
	            theLayer.children[next].transparency <- tn.value;

	            tn.fraction <- t.fraction;
	            doneSwitch <- t.cycleTime;
	            t.enabled = true;
	    }
    }

    script doneSwitch {
            printf("doneSwitch current=%d, next=%d, size=%d\n", 
	    		       current, next, size);
     	    theLayer.children[current].transparency <- nil;
     	    theLayer.children[next].transparency <- nil;
	    current = next;
	    next = (current + 1) % size;
     }

     printf("The size = %d\n", size);
};

Timer t30 =
    Timer { cycleInterval = 1/30, loop = true };

Layer layer = 
      Layer { top = 240,
      	      left = 320,
	      children = {
	          Shape { width = 50, height = 50,
		  	  transparency = 0,
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
			  streamName = "../images/wg_gdo_1.mpg",
			  frameTime <- t30.cycleTime
		  },

		  Video { transparency = 1,
		  	  loop = true,
		          background = false,
			  streamName = "../images/wg_gdo_2.mpg",
			  frameTime <- t30.cycleTime
		  }
	        }
	     };
			  
Timer swt = Timer { cycleInterval = 2.0, loop = true };

// FastSwitch sw = FastSwitch { theLayer = layer, doSwitch <- swt.cycleTime };

SlowSwitch sw = SlowSwitch { theLayer = layer, doSwitch <- swt.cycleTime };


Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     backgroundColor = 0x000000,
	     children = { layer }
};

