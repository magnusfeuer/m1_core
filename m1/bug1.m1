
Timer t = Timer { cycleInterval = 5.000, loop = true };
ScalarInterpolator x = ScalarInterpolator { key={0,1}, keyValue={10, 641} };

Shape bg = 
    Shape { top = 2, 
    	    left = 2,
	    width <- x.value, 
	    height <- x.value,
	    shape = 'r',
	    fill = true,
	    fillColor =0xFF0000,
	    transparency = 0.0,
	    borderWidth = 4,
	    borderColor = 0x0000ff

};

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 000000,
	     children = {  bg }
};

x.fraction <- t.fraction;

