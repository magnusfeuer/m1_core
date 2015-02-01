
type Wave {
     input  event float t;
     output event float y;         // output event 
     input  event float amplitude = 1.0;  // amplitude
     input  event float frequency = 1;    // angular frequency (radian / s)
     input  event float phase = 0.0;      // phase shift
};

type SineWave : Wave {
     script { y = amplitude*sin(t*frequency + phase); }
};

type CosWave : Wave {
     script { y = amplitude*cos(t*frequency + phase); }
};

type Random : Wave {
     script { y = amplitude*randomf(); }
};

Timer t1 = @Timer { cycleInterval = 1, loop=true };

Timer zt = @Timer { cycleInterval = 5, loop=true };

ScalarInterpolator zoom = 
		   @ScalarInterpolator { key=     {0, 0.1, 0.2, 0.7, 1},
		   		       	 keyValue={0.5, 1,   1,   0.01, 1},
					 fraction <- zt.fraction };

Plot plot1 = 
     @Plot { top = 10, left = 10,
	     contentWidth = 400, contentHeight = 80,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = rectangle,
     	     plotTime <- zoom.value,
	     color = 0xff0000,
	     samples = @RealTimeSampler { maxSamples = 65536, loop = true,
	     	       		  newValue <- @SineWave { t <- t1.value,
				  	   	       frequency = 4*pi() }.y
			        }
	     };

Plot plot2 = 
     @Plot { top = 110, left = 10,
	     contentWidth = 400, contentHeight = 80,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 1,
	     color = 0x00ff00,
	     samples = @RealTimeSampler { maxSamples = 65536, loop = true,
	     	       		  newValue <- @Random { t <- t1.value }.y
			        }
	     };

Plot plot3 = 
     @Plot { top = 210, left = 10,
	     contentWidth = 400, contentHeight = 80,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = rectangle,
     	     plotTime = 1,
	     color = 0x0000ff,
	     samples = @RealTimeSampler { maxSamples = 65536, loop = true,
	     	       		  newValue <- t1.fraction 
				}
	     };

Screen screen = 
   @Screen { height = 480, width  = 640,
   	     pixelType = "bgra",
	     backendType = "x11",
	     background = true,
	     backgroundColor = 0x00000000,
	     children = { plot1, plot2, plot3 }
};
