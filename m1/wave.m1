type Wave {
     input  event float t;
     output event float y;         // output event 
     input  event float amplitude = 1.0;  // amplitude
     input  event float frequency = 1;    // angular frequency (radian / s)
     input  event float phase = 0.0;      // phase shift

     printf("Wave:amplitude=%f\n", amplitude);
     printf("Wave:frequency=%f\n", frequency);
     printf("Wave:phase=%f\n", phase);
};

type SineWave : Wave {
     script { 	
     	    y = amplitude*sin(t*frequency + phase); 
     	    // printf("t=%f, y=%f\n", t, y);
     }

     printf("SineWave:amplitude=%f\n", amplitude);
     printf("SineWave:frequency=%f\n", frequency);
     printf("SineWave:phase=%f\n", phase);
};

event float value;

Timer t1 = @Timer { cycleInterval = 1 };

script value {
       printf("%f\n", value);
}


value <-@SineWave { t <- t1.value, amplitude = 2, frequency = 4*pi() }.y;


