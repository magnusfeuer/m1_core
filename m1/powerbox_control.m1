//
//

Screen s;
EpicInputDevice touch;
InputStrategy input_strategy;

type ValueVSlider : :Slider:Vertical {
    public string   labelFontName  = "BankGothic Md BT";
    public unsigned labelFontSize  = 12;
    public unsigned labelFontColor = 0x0000FF;
    public string   name  = "";

    script ValueVSlider {
        Float label;

    	if (name != "") {
	   Text  header;

	   header = @Text { left = 0,
	   	    	    top <- top - 50,
			    text = name,
			    fontName = labelFontName,
			    fontSize = 14,
			    fontWeight = medium,
			    fontSlant = roman,
			    fontColor = labelFontColor };
           children += header;
	}
	label = @Float { left = 0,
			 top  <- height,
			 value <- value,
      	       	      	 format="%.2f",
            	         fontName = labelFontName,
			 fontSize = labelFontSize,
			 fontWeight = medium,
			 fontSlant = roman,
			 fontColor = labelFontColor };
	children += label;
    }
};

//
// pwm - channel 1
//

ValueVSlider pwm1 = @ValueVSlider { name="PWM-1",
	     	    		    top=30, left=100,
				    height = 400,
				    value = 0.0 };

//
// pwm - channel 4
//
ValueVSlider pwm4 = @ValueVSlider { name="PWM-4",
	     	    		    top=30, left=185,
				    height = 400,
				    value = 0.0 };

//
// pwm - channel 7
//
ValueVSlider pwm7 = @ValueVSlider { name="PWM-7",
	     	    		    top=30, left=270,
				    height = 400,
				    value = 0.0 };

s = @Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     wantFocus = true,
	     background = true,
	     backgroundColor = 0xcccccc,
	     children = {  pwm1, pwm4, pwm7 }
};

input_strategy = @InputStrategy { layer = s };

.PowerBox.cset[1].pwm <- pwm1.value;
.PowerBox.cset[4].pwm <- pwm4.value;
.PowerBox.cset[7].pwm <- pwm7.value;

touch = EpicInputDevice {
	    inputStrategy = input_strategy,
	    screen = s, // Used to extract mouse
	    maxUnscaledX = 640,
	    maxUnscaledY = 480,
	    minUnscaledX = 0,
	    minUnscaledY = 0,
	    scaledWidth = 640,
	    scaledHeight = 480
};

