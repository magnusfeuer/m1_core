
// Create a style type
type RadioText : Text {
     input event string @Text.fontName = "BankGothic Md BT";
     input event unsigned @Text.fontSize = 24;
     input event FontWeight @Text.fontWeight = medium;
     input event unsigned @Text.fontColor = 0x000000;
};

Text txt1 = @RadioText { valign = center, halign = center,
            		    text = "Hi There" };

// A View that contain the text
Layer textView1 =
      Layer { top = 10, left = 10,
      	      width = 300, height = 100,
	      backgroundColor = 0x00ff00,
	      background = true,
	      children = { txt1 }
      };

// 
Text txt2 = @RadioText { fontColor = 0xffffff, text = "Hi Again" };

Layer textView2 =
      Layer { top = 100, left = 300,
      	      width = 300, height = 100,
	      backgroundColor = 0x0000ff,
	      background = true,
	      children = { txt2 }
	      };

// A background just for show
Layer bgView =
      Layer { top = 0, left = 0,
      	      width = 640, height = 480,
	      backgroundColor = 0xff0000,
	      background = true,
	      children = { textView1, textView2 }
	      };

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 0x00555555,
	     children = { bgView }
};

txt1.width = textView1.width;
txt1.height = textView1.height;


// Center txt item within textView
txt2.left <- (textView2.width - txt2.contentWidth) / 2;
txt2.top <- (textView2.height - txt2.contentHeight) / 2;

