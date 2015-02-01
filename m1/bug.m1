Timer t1 =
    Timer { cycleInterval = 2.000,
    	       	 loop = true };

Timer t3 =
    Timer { cycleInterval = 1.000,
    	       	 loop = true };

Point p1[] = { Point { x = 0,  y = 00 },
	       Point { x = 100,  y = 0 },
	       Point { x = 100,  y = 100 },
	       Point { x =0,  y = 100 },
	       Point { x =0,  y =0 } };



PositionInterpolator path1 =
    PositionInterpolator {
    		fraction <- t1.fraction,
    		key = { 0.0, 0.25, 0.5, 0.75, 1.0 },
		keyValue = p1  };		


ColorInterpolator red = 
     ColorInterpolator {
     		fraction <- t3.fraction,
		key      = { 0.0, 0.5, 1.0 },
		keyValue = { 0x000000,
			     0xFF0000,
			     0x000000 
		}
	};

Shape red_circle = 
  Shape {   
            top  <- path1.yvalue,
            left <- path1.xvalue,
	    contentWidth  = 100,
	    contentHeight = 100,
	    shape = 'e',
	    fill = true,
	    fillColor <- red.value,
	    transparency = 0.0,
	    borderWidth = 0
};


Layer front = 
    Layer { top=0, left=0,
	    enabled = true,
	    children = {  red_circle  } 
    };


Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "argb",
	     backendType = "auto",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 0xFFFFFFFF,
	     children = {  front }
};
