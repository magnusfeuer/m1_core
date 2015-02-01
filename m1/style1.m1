//
// Style test
//
Screen s;
Layer  styleView;

type Counter  {
     event signed value = 0;
     value <- value + 1;
};

Counter counter = @Counter {};

type GothicStyle : Style {
    input event string @Style.fontName = "BankGothic Md BT";
    input event FontWeight @Style.fontWeight = medium;
    input event FontSlant @Style.fontSlant = roman;
    input event unsigned @Style.backgroundColor = 0xffffff;
};

type RadioStyle : Style {
    input event string @Style.fontName = "Radio Space";
    input event FontWeight @Style.fontWeight = medium;
    input event FontSlant @Style.fontSlant = roman;
    input event unsigned @Style.backgroundColor = 0xffffff;
};

type RadioNumberStyle : RadioStyle {
    input event int @Style.glyphDeltaX     = 2;
    input event int @Style.glyphDeltaY     = -2;

    script RadioNumberStyle { 
	glyphFixedWidth = fontSize-3; 
	printf("Set glyphFixedWidth = %d\n", glyphFixedWidth);
    }
};

unsigned int test_size[] = { 10, 12, 14, 16, 18, 20, 22, 24, 26, 28,
			     30, 32, 34, 36, 38, 40, 42, 44, 46, 48 };

event int test_ind = 0;
event time next_ind;

Style test_style =
    @GothicStyle { name = "test", fontSize = 28, fontColor = 0xffffff,
    		   backgroundColor = 0xff00ff };


script next_ind {
    test_ind = (test_ind + 1) % size(test_size);
};

next_ind <- @Timer { cycleInterval = 0.5, loop=true}.cycleTime;


@GothicStyle { name = "gothic" };
@GothicStyle { name = "gothic.12", fontSize=12 };
@GothicStyle { name = "gothic.24.blue", fontSize=24, fontColor = 0x0000FF };
@RadioStyle { name = "radio.23.blue", fontSize=23, fontColor = 0x0000FF };
@RadioNumberStyle { name = "radio.23.number", fontSize=23, fontColor=0xffffff };

styleView = @Layer {
    children = {
	Text { top = 10, left = 10, class = "test", text = "Test1" },
	Text { top = 40, left = 10, class = "test", text = "Test2" },
	Text { top = 70, left = 10, class = "test", text = "Test3" },
	Text { top = 100, left = 10, background=true, class = "test", text = "Test4" },

	Text { top = 150, left = 50, class = "gothic.24.blue",
	       text = "Hello quit",
	       background = true
	},

	Text { top = 150, left = 200, class = "radio.23.blue",
	       text = "Hello quit",
	       background = true
	},

	Signed { top = 150, left = 350, 
		 style = @RadioNumberStyle { fontColor = 0xff0000, fontSize=12 },
		 format = "%04d",
		 value <- counter.value },

	Signed { top = 170, left = 350, 
		 style = @RadioNumberStyle { fontColor = 0x00ff00, fontSize=18 },
		 format = "%04o",
		 value <- counter.value },

	Signed { top = 190, left = 350, 
		 style = @RadioNumberStyle { fontColor = 0x0000ff, fontSize=24 },
		 format = "%04x",
		 value <- counter.value },

	Signed { top = 220, left = 350,  
		 class = "radio.23.number",
		 format = "%04d",
		 value <- counter.value }

    }
};
    

s = @Screen { height = 480,
	      width  = 640,
	      pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	      backendType = "x11",
	      framebufferDevice = "/dev/fb0",
	      background = true,
	      backgroundColor = 0x555555,
	      children = { styleView  }
};

// Changing fontColor on "test" style
test_style.fontColor <-
  @ColorInterpolator {
    key = {0,0.25,0.5,0.75,1},
    keyValue = {0xffffff, 0xff0000, 0x00ff00, 0x0000ff, 0xffffff},
    fraction <- @Timer { cycleInterval = 5, loop = true}.fraction
   }.value;

// Changing fontSize on "test" style (with array trick)
test_style.fontSize <- test_size[test_ind];

  

counter.value = 1;


