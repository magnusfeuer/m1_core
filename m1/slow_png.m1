//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Device definition for a specific platform.
//

library ScreenLib {
     string thePixelType = "bgra";

     event time sample <-
	 @Timer { cycleInterval = 1.0, loop = true }.cycleTime;


    // Current resolution index into resolui


    //
    // Constructor code. 
    // Setup the core screen and input devices.,
    //
    Screen screen =  Screen { 
	width  = 800,
	height = 480,
	pixelType = thePixelType,
	backendType = "none",
	wantFocus = true,
	background = true,
	backgroundColor = 0x00ff00,
	frameRate = 10000,
//      	recorder = @Video { 
//      	    top = 0, left = 0, width = 640, height = 480, 
//      	    mode = out, enabled = true, streamName = "menu_tst.mpg" 
//      	},
	children = { 
	    @Image { 
	    	   pixelType = thePixelType,
	    	   imageFile= "../images/LIFESTYLE-800x480-Alum1-Overlay.png" 
            },
	    @Image { 
	      	   pixelType = thePixelType,
		   imageFile="../images/LIFESTYLE-800x480-Alum1-HiLiteLayer.png"
	    }
       }
    };

    // script for updating the fps counter
     script sample {
	 printf("fps[%f]\n", .M1.fps);
     }


}

    
