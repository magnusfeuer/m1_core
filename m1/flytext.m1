//
//  Send characters along a position interpolator to form
//  a text.
//
type Sequence {
     public event signed index;
     public signed start = 0;
     public signed stop;
     public float  delay = 1.0;

     private event time delay_done;
     private signed current = start;
     private Timer t;

     script delay_done {
	 printf("Sequence: delay_done=%u, start=%d, stop=%d, index=%d\n", 
		delay_done, start, stop, current);
	 index = current;
	 current = current + 1;
	 if (current <= stop)
	     t.enabled = true;
	 else
	     delay_done <- nil;
     }

     t = @Timer { cycleInterval = delay };
     delay_done <- t.cycleTime;
};


type CharAnim : Char {
     event float src_x;
     event float src_y;
     event float dst_x;
     event float dst_y;
     float dst_xoffs;         // parameter
     float dst_yoffs;         // parameter
     signed index;
     string src_text;
     Text   dst_text;
     event float animation_time = 1;

     private event time animation_done;
     private PositionInterpolator pos;
     private Timer t;

     script animation_done {
	 printf("animation_done: %d\n", index);
	 dst_text.text = substr(src_text, 0, index+1);
	 transparency = 1.0;
     }

     script src_x || src_y {
	 pos.keyValue[0].x = src_x;
	 pos.keyValue[0].y = src_y;
     }

     script dst_x || dst_y {
	 pos.keyValue[1].x = dst_x + dst_xoffs;
	 pos.keyValue[1].y = dst_y + dst_yoffs;
     }

     pos = @PositionInterpolator { key={0, 1},
     	   			   keyValue = {
				  	   Point {x=src_x, y=src_y},
					   Point {x=dst_x+dst_xoffs, 
						  y=dst_y+dst_yoffs} }
				 };
     // printf("CharAnim: %c position = %p\n", value, pos);
     t = @Timer { cycleInterval = animation_time };
     pos.fraction <- t.fraction;
     transparency <- 1-t.fraction;
     top  <- pos.yvalue;
     left <- pos.xvalue;
     animation_done <- t.cycleTime;
};


//
// Send the letters Hello World one by one
//

type TextAnim : Layer {
    public event Text theText;

    private event signed index;
    private string txtString;
    private float txtDx = 0;
    private float txtDy = 0;
    private float xoffs[] = {};
    private float yoffs[] = {};

    script index {
	char c     = txtString[index];
	children[index] =
	    @CharAnim {
	             value = c,
		     index = index,
		     src_text = txtString,
		     dst_text = theText,
		     fontName = theText.fontName,
		     fontSize = theText.fontSize,
		     fontWeight = theText.fontWeight,
		     fontSlant = theText.fontSlant,
		     fontColor = theText.fontColor,
		     src_x = 320,  src_y = 240,
		     dst_x <- theText.left, 
		     dst_y <- theText.top,
		     dst_xoffs = xoffs[index], 
		     dst_yoffs = yoffs[index]
	};
    }


    txtString = theText.text;
    index <- @Sequence { delay = 0.8, start = 0, stop = size(txtString)-1 }.index;
    {
       signed i;

       foreach i in [0:size(txtString)-1] {
           string sub = substr(txtString,0,i+1);
	   xoffs[i] = @Text { text = sub,
			      fontName = theText.fontName,
			      fontSize = theText.fontSize,
			      fontWeight = theText.fontWeight,
			      fontSlant = theText.fontSlant,
			      fontColor = theText.fontColor }.contentWidth;
	   yoffs[i] = 0;
       }
       theText.text = "";
    }
};

Timer myT = @Timer { cycleInterval=5, loop=true };
PositionInterpolator myPos = 
    PositionInterpolator { fraction <- myT.fraction,
			   key = { 0, 0.25, 0.5, 0.75, 1},
			   keyValue = { 
			       Point { x=10, y=50 },
			       Point { x=350, y=50 },
			       Point { x=350, y=400 },
			       Point { x=10, y=400 },
			       Point { x=10, y=50 } }};

Text myText = @Text { top <- myPos.yvalue, left <- myPos.xvalue,
		      text = "Hello Magden",
		      fontName = "Radio Space Italic",
		      fontSize = 40,
		      fontWeight = none,
		      fontSlant = italic,
		      fontColor = 0xFFFFFF };

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = (getenv("M1_BACKEND") == "") ? "x11" : 
	     		 getenv("M1_BACKEND"),
	     frameRate = 85,
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 0x00555555,
	     children = { myText, @TextAnim { theText = myText }}

};
