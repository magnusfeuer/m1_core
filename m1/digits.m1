DDSDigit digit = DDSDigit { 
	 ddsFile = "../images/TB-Numbers-RedYellFlame-Big.dds",
	 format = "%1.4f", // Default is %.0f
	 spacing = -5,
	 pointSpacing = -15,
	 top = 100,
	 left = 100
};

Timer t = Timer { cycleInterval = 10.0, loop = true };

Screen s = Screen { height = 480,  
       	   	    width  = 640,
		    pixelType = "bgra",
		    backendType = "x11",
		    framebufferDevice = "/dev/fb0",
		    children = {   
			     Shape { 
				    top=100, 
				    left=100, 
				    contentWidth=300, 
				    contentHeight=300, 
				    shape = 'r', 
				    fill=true, 
				    fillColor = 0xffffff
			     },
			     digit	  
	            }
};

digit.displayValue <- t.fraction;
