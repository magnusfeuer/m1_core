
application SliderTest {

Screen s;
EpicInputDevice touch;
InputStrategy input_strategy;

//
// ColorMixer accepts r,g,b as floating point [0,1] and
// mix into RGB 32-bit value
//
type ColorMixer {
    input event float r;
    input event float g;
    input event float b;
    output event unsigned value;

    script {
	value = (@byte(r*255) << 16) | (@byte(g*255) << 8) | @byte(b*255);
    }
};


//
// Vertical slider with numeric footer label
//
type ValueVSlider : :Slider:Vertical {
    public string   labelFontName  = "BankGothic Md BT";
    public unsigned labelFontSize  = 12;
    public unsigned labelFontColor = 0x0000FF;
    private Float label;

    script ValueVSlider {
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
// Height control for the other sliders (BIG 31)
//
:Slider:Horizontal sliderH0 =  @:Slider:Horizontal { top=10, left=10, 
						     height = 31,
						     width=200,
						     value = 0.5 };

//
// Height control for the other sliders (SMALL 15)
//

:Slider:Horizontal sliderH1 =  @:Slider:Horizontal { top=10, left=220, 
						     height = 15,
						     width=200,
						     value = 0.5 };

//
// EventPropagte 
// to output value
//
type EventCross {
     event float a;
     event float b;
     output event float value;

     script a {  value = a; }
     script b {  value = b; }
};

// Feed both in to a demultiplexor
EventCross sliderH = @EventCross { a <- sliderH0.value, 
	   	     		   b <- sliderH1.value };

//
// Red component control
//

ValueVSlider sliderR = @ValueVSlider { top=50, left=100,
				       height <- 50 + sliderH.value*300,
				       value = 0.3 };

//
// Green component control
//
ValueVSlider sliderG = @ValueVSlider { top=50, left=185,
				       height <- 50 + sliderH.value*300,
				       value = 0.3 };

//
// Blue component control
//
ValueVSlider sliderB = @ValueVSlider { top=50, left=270,
				       height <- 50 + sliderH.value*300,
				       value = 0.3 };

//
// Setup the color mixer
//
ColorMixer cm = @ColorMixer { r <- sliderR.value,
			      g <- sliderG.value,
			      b <- sliderB.value };
			     

s = @Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     wantFocus = true,
	     background = true,
	     backgroundColor = 0xffffff,
	     children = {  sliderH0, sliderH1, sliderR, sliderG, sliderB }
};

s.backgroundColor <- cm.value;

input_strategy = @InputStrategy { layer = s };

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


}