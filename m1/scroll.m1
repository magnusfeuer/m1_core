//
// Simple rotate tester
//
//   current is 
//    
//
type Quad {
     input event float fraction;  // 0 ... 1 
     output event float value;    // 0 ... 1

     script fraction {
	    value = fraction*fraction;
     }
};


type Scroll : Layer {
     public event float position;

     // Object layer
     Layer objL = @Layer { top = 128, left = 0, width = 2000, height = 32  };
     // Blank object background layer
     Shape fgL = Shape { contentWidth = 560, contentHeight = 320,
		         shape = rectangle, fill = true,
		         fillColor = 0x000000
		       };
     width  = 560;
     height = 320;
     clip   = true;
     children = { fgL, objL };
     objL.left <- position;

     {
	// Constructor
	signed i;
	float x = 0;

	foreach i in [0:19] {
	  objL.children[i] = 
		  DDS { top = 0, left = x,
		       backgroundColor <-
		        @ColorInterpolator { 
		   			   key = {0, 1.0},
					   keyValue = {0xff0000, 0x0000ff},
					   fraction = i/10.0 }.value,
		       background = true,
		       ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
		       value <- @Timer {startTime=i/5.0,loop=true}.fraction };
	          x += 150.0;
        }
   }
};


Shape bgL = Shape { contentWidth = 640, contentHeight = 640,
		    shape = 'r', fill = true,
		    fillColor = 0xcccccc 
		  };

ScalarInterpolator pos = ScalarInterpolator {
		   key = {0, 0.5, 1.0},
		   keyValue = {-2000.0, 0, -2000.0 }};

Timer t = Timer { cycleInterval = 20.0, loop = true };
Quad q = Quad { fraction <- t.fraction };

pos.fraction <- t.fraction;


@Screen { height = 480,
	  width  = 640,
	  pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	  backendType = (getenv("M1_BACKEND") == "") ? "x11" : 
	     		 getenv("M1_BACKEND"),
	  frameRate = 30,
	  framebufferDevice = "/dev/fb0",
	  background = true,
	  backgroundColor = 0x000000,
	  children = { bgL, @Scroll { top=80, left=40, position <- pos.value} }
};



