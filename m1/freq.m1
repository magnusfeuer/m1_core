
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

type Add {
     input event float a;
     input event float b;
     output event float value;

     script { value = a + b; }
};


type AddWave {
     event float t;
     event float y;
     
     private int start_freq = 2;
     private int stop_freq  = 256;
     private int freq       = start_freq;
     input event time next <- @Timer { cycleInterval = 2,loop=true }.cycleTime;
     Add  sum;

     script next {
	 freq <<= 1;
	 printf("freq = %d\n", freq);
	 if (freq > stop_freq) {
	     freq = start_freq;
	     sum = @Add { a <- 0.0, b <- 0.0 };
	 }
	 printf("Frequency %d added\n", freq);
	 sum = @Add { a <- @SineWave { t <- t, frequency = freq*2*pi() }.y,
		      b <- sum.value };
	 y <- sum.value;
     }

     sum = @Add { a <- @SineWave { t <- t, frequency = freq*2*pi() }.y,
		  b <- 0.0 };
     y <- sum.value;
};


Timer t1 = @Timer { cycleInterval = 0.01, loop=true };

RealTimeSampler vs = @RealTimeSampler { maxSamples = 1024*32, loop = true,
	     	     		       	minValue   = -8, maxValue = 8,
		        		newValue <- @AddWave { t <- t1.value }.y };

Plot samples = 
     @Plot { top = 10, left = 10,
	     contentWidth = 400, contentHeight = 80,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = line,
	     color = 0xff0000,
	     samples = vs
     };

Plot freq = 
     @Plot { top = 110, left = 10,
	     contentWidth = 400, contentHeight = 80,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
	     viewType = frequency,
     	     plotTime = 2,
	     color = 0x00ff00,
	     samples = vs
	     };

Screen screen = 
   @Screen { height = 480, width  = 640,
   	     pixelType = "bgra",
	     backendType = "x11",
	     background = true,
	     backgroundColor = 0x00000000,
	     children = { samples, freq }
};
