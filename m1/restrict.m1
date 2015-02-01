
event float frac;

Timer t = @Timer { cycleInterval=2, loop=true, enabled=true };

ScalarInterpolator v = @ScalarInterpolator { 
		        key={0,1}, keyValue={10,2}, fraction <- t.fraction };

Timer a = @Timer { cycleInterval<-v.value, loop=true, enabled=true };

Shape shape = @Shape { top = 200,
      	      	       left = 200,
      	      	       width = 100,	
		       height = 100,
		       shape = ellipse,
		       fill = true,
		       fillColor = 0xFF0000,
		       valign = center,
		       halign = center,
		       contentHeight = 80,
		       contentWidth  = 89,
		       background = true,
		       backgroundColor = 0x00ff00
		      };

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     children = {  shape }
};

// script frac { printf("a=%f\n", frac); }
// frac <- a.fraction;

shape.contentWidth  <- a.fraction*100;
shape.contentHeight <- a.fraction*100;





