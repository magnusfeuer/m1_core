//
// Simple rotate tester
//
//
library RotateApp {

const int gauges  = 10;
string image_dir = 
       "/Users/tony/Pictures/iPhoto Library/iPhoto Library/Originals/2006/Roll 1/";
string image_file[] = {
      		"../images/image.png",
       		image_dir + "DSC02369.jpg",
       		image_dir + "DSC02370.jpg",
       		image_dir + "DSC02371.jpg",
       		image_dir + "DSC02372.jpg",
       		image_dir + "DSC02373.jpg",
       		image_dir + "DSC02374.jpg" };

//
// Interval generate index 0 .. (nobjects-1)
// pausing in between each integral index
//
type TimeInterleave {
     public float rotateTime  = 0.2;    // config
     public float pauseTime   = 1.0;    // config
     public int startIndex = 0;      // config
     public int numIndex   = 1;      // config

     public event float index; 		   // generated output event
     // PRIVATE 
     private int maxIndex;
     private int curIndex;

     private input event time pauseDone;
     private input event time rotateDone;

     private Timer tp = @Timer { cycleInterval=pauseTime,
     	     		     		   enabled=true };
     private Timer tr = @Timer { cycleInterval=rotateTime,
     	     		     		   enabled=false };
     private ScalarInterpolator s = 
     	     @ScalarInterpolator { key={0,1}, 
     				   keyValue={startIndex,startIndex}};
     script pauseDone {
	    printf("pauseDone\n");
	    if (curIndex <= maxIndex) {
	       s.keyValue[0] = curIndex;
	       s.keyValue[1] = curIndex+1;
	    }
	    else {
	       curIndex = 0;
	       s.keyValue[0] = 0;
	       s.keyValue[1] = 1;
	    }
	    curIndex += 1;
	    tr.enabled = true;
     }

     script rotateDone {
     	    printf("rotateDone\n");
	    tp.enabled = true;
     }

     // calculate the initial values (uses overriden values!)
     maxIndex = numIndex-1;
     curIndex = startIndex % numIndex;

     // Setup internal events
     pauseDone <- tp.cycleTime;
     rotateDone <- tr.cycleTime;
     s.fraction <- tr.fraction;
     index <- s.value;
};


// DDS with a depth
type ZLayer : Layer {
     int   id;	    // id number
     float z;	    // current depth
     Layer child;   // The child

     children = { child };
};

type Rotate : Layer {
     int nobjects;
     // index of the currently displayed item
     public event   float index;
     private const  float offset = 6*pi()/4;

     script index {
          int j;
	  int n = size(children);
	  // index should be offseted at 270 degrees
	  foreach j in [0:n-1] {
	      ZLayer zl = @ZLayer(children[j]);
	      Layer  child = zl.child;
	      int i = zl.id;
	      float angle =  offset + (2*pi())/n*(index+i);
	      float x = 100*cos(angle);  // -100 - 100
	      float y = 100*sin(angle);  // -100 - 100
	      float z = (y + 100)/200;
	      child.transparency = z;
	      if (z > 0.7) z = 0.7;
	      child.vscale = 1 - z;
	      child.hscale = 1 - z;
	      zl.left = x+190;
	      zl.top  = 128-40*z;
	      zl.z = z;
	  }
	  sort(children, &ZLayer.z); // rsort?
	  reverse(children);
     }

     backgroundColor = 0x000000;
     background = true;
     width  = 560;
     height = 320;
     clip   = true;

     {
	// Constructor
	int i;
	int nimages;

	nimages = size(image_file);
	foreach i in [0:nimages-1] {
		Image img = @Image { transparency = 1.0,
				     imageFile = image_file[i] };
		children[i] =
		  	@ZLayer { top = 128, left = 0, id = i, child = img };
	}

	foreach i in [0:gauges-1] {
	  DDS dds = 
	    DDS { transparency = 1.0,
	    	  background = true,
		  backgroundColor <-
		        @ColorInterpolator { 
		   			   key = {0, 1.0},
					   keyValue = {0xff0000, 0x0000ff},
					   fraction = i/10.0 }.value,
      	    	  value <- @Timer {startTime=i/5.0,loop=true}.fraction,
	      	  ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds"
		};
	  children[nimages+i] = 
	  	ZLayer { top = 128, left = 0, id = nimages+i, child = dds };
        }
	nobjects = gauges + nimages;
   }
};


TimeInterleave ti = @TimeInterleave { startIndex=0, numIndex=gauges+size(image_file) };

Rotate rotate =  @Rotate { top=80, left=40, index <- ti.index };


rotate.index = 0;


@Screen { height = 480,
	  width  = 640,
	  pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	  backendType = "x11",
	  framebufferDevice = "/dev/fb0",
	  background = true,
	  backgroundColor = 0xcccccc,
	  children = { rotate }
};

}
