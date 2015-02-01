//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// Device definition for a specific platform.
//

library ScreenLib {
     event time sample <-
	 @Timer { cycleInterval = 1.0, loop = true }.cycleTime;
     Timer ct =
	 @Timer { cycleInterval = 10.0, loop = true };
     ColorInterpolator ci = 
     	@ColorInterpolator { key={0,0.25,0.5,0.75,1},
			     keyValue={0, 
			     	       0xff0000, 0x00ff00, 0x0000ff,
				       0},
			     fraction <- ct.fraction };
    // Current resolution index into resolui


    //
    // Constructor code. 
    // Setup the core screen and input devices.,
    //
    Screen screen =  Screen { 
	width  = 800,
	height = 480,
	pixelType = "bgra",
	backendType = "none", // macos_gl",
	wantFocus = true,
	background = true,
	backgroundColor <- ci.value, // 0x00ff00,
	frameRate = 10000,
//      	recorder = @Video { 
//      	    top = 0, left = 0, width = 640, height = 480, 
//      	    mode = out, enabled = true, streamName = "menu_tst.mpg" 
//      	},
	children = { 
	    @DDS { ddsFile =  "../images/LIFESTYLE-800x480-Alum1-Overlay.dds" },
	    @DDS { ddsFile =  "../images/LIFESTYLE-800x480-Alum1-HiLiteLayer.dds" }
	}
    };


    // script for updating the fps counter
     script sample {
	 printf("fps[%f]\n", .M1.fps);
     }


}
