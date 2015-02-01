
type Focus: Layer {
     public string name;
     //
     // Scripts
     //
     script button1Down {
	    printf("[%s]: Button1Down\n", name);
     }
};



Layer level_4 = Focus {
      top = 0, left = 0, height = 480, width = 639,
      wantFocus = true,
      name = "Level 4 I want events, but should not get it since parent does not want it."
};

Layer level_3 = Focus {
      top = 0, left = 0, height = 480, width = 639,
      wantFocus = false,
      name = "Level 3 I DO NOT WANT EVENTS!",
      children = { level_4 }
};

Layer level_2 = Focus {
      top = 0, left = 0, height = 479, width = 640,
      wantFocus = true,
      name = "Level 2: I want events",
      children = { level_3 }
};

Layer level_1 = Focus {
      top = 0, left = 0, height = 479, width = 640,
      wantFocus = true,
      name = "Level 1: I want events",
      children = { level_2 }
};




Screen s = Screen { height = 480,  
       	   	    width  = 640,
		    pixelType = "bgra",
		    backendType = "x11",
		    framebufferDevice = "/dev/fb0",
		    wantFocus = true,
		    children = {  level_1  } 
		    		    
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
