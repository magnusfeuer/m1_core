//
// Type for switching children (skin switch / intrument switch )
//

type FastSwitchLayer : Layer {
     input event time doSwitch;

     private signed current = 0;
     private signed next    = 1;
     private unsigned size;

     script doSwitch  {
     	    size = size(children);
	    if (size > 1) {
	       printf("FastSwitch current=%d, next=%d\n", current, next);
	       children[current].transparency = 1.0;
	       children[next].transparency = 0.0;
	       current = next;
	       next = (current + 1) % size;
	    }
     }

     printf("FastSwitchLayer: init\n");
};


Timer swt = Timer { cycleInterval = 1.0, loop = true };

Layer layer  = 
      FastSwitchLayer {
      	      top = 240,
      	      left = 320,
	      children = {
	          Shape { contentWidth = 50, contentHeight = 50,
		  	  transparency = 0,
			  shape = rectangle,
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
		  }

	        },

      	      doSwitch <- swt.cycleTime

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

// .M1.halt=true;


