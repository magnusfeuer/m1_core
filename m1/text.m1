    
type Counter  {
     event signed value = 0;
     value <- value + 1;
};

Counter counter = @Counter {};

Timer t1 = @Timer { cycleInterval = 5, loop=true };

Layer textView =
      Layer { top = 0, left = 0,
      	      children = {
	            Text { top = 150, left = 50,
		           fontName = "BankGothic Md BT",
		           fontSize = 24,
			   fontWeight = medium,
			   fontSlant = roman,
			   fontColor = 0x0000FF,
			   text = "Hello quit",
		    	   backgroundColor  = 0xffffff,
		    	   background = true
		    },

	            Text { top = 150, left = 200,
		           fontName = "Radio Space",
		           fontSize = 23,
			   fontWeight = medium,
			   fontSlant = roman,
			   fontColor = 0x0000FF,
			   text = "Hello quit",
		    	   backgroundColor = 0xffffff,
		    	   background = true
			  },

	            Signed { top = 150, left = 350,
		           fontName = "radio space",
		           fontSize = 23,
			   fontWeight = medium,
			   fontSlant = roman,
			   fontColor = 0x0000FF,
			   glyphDeltaX = 2,
			   glyphDeltaY = -2,
			   glyphFixedWidth = 20,
			   format = "%04d",
			   value <- counter.value },

  	            Float { top = 190, left = 350,
		           fontName = "Radio Space Italic",
			   fontSize = 38,
			   fontWeight = none,
			   fontSlant = italic,
			   fontColor = 0xFF0000,
			   format = "%.4f",
			   value <- t1.fraction },

  	            Float { top = 230, left = 350,
		           fontName = "Radio Space Italic",
			   fontSize = 38,
			   fontWeight = none,
			   fontSlant = italic,
			   fontColor = 0xFF0000,
			   glyphFixedWidth = 30,
			   glyphDotKerning = -10,
			   format = "%.4f",
			   value <- t1.fraction },

	            Text { top = 50, left = 50,
		           fontName = "Arial",
			   fontSize = 13,
			   fontWeight = medium,
			   fontColor = 0xFFFFFF,
			   text = "Hello world" },

	            Text { top = 100, left = 50,
		           fontName = "Arial",
			   fontSize = 16,
			   fontWeight = medium,
			   fontColor = 0x00FF00,
			   text = "Hello world" },

	            Text { top = 120, left = 50,
		           fontName = "Times New Roman",
			   fontSize = 24,
			   fontWeight = medium,
			   fontColor = 0xFF0000,
			   text = "Times Medium 24" },



	            Text { top = 120, left = 300,
		           fontName = "Bitstream Vera Sans",
			   fontSize = 27,
			   fontWeight = medium,
			   fontColor = 0xFF0000,
			   text = "Bitstream Vera Sans 27" },

	            Float { top = 200, left = 10,
		    	   background = true,
		    	   backgroundColor  = 0x0ffff0,
		           fontName = "TB-Numbers-Small-OrangeBlack",
			   fontSize = 0,
			   fontWeight = none,
			   fontSlant = none,	
			   format = "%f",
			   value <- t1.fraction },

	            Float { top = 300, left = 10,
		    	   background = true,
		    	   backgroundColor  = 0x0ffff0,
		           fontName = "TB-Numbers-Big-OrangeBlack",
			   fontSize = 0,
			   fontWeight = none,
			   fontSlant = none,
			   glyphDeltaX = 40,
			   format = "%.4f",
			   value <- t1.fraction }


		}
	   };

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     frameRate = 1000,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = (getenv("M1_BACKEND") == "") ? "x11" : 
	     		 getenv("M1_BACKEND"),
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 0x00555555,
	     children = { textView  }
};

counter.value = 1;
