//
// Use video4liux camera support
//
Timer tm = Timer { cycleInterval = 1/25.0,  loop = true };

Video video = 
      Video { top = 10, left = 10, 
      	      width = 320, 
	      height = 240,
      	      contentWidth = 320,
	      contentHeight = 240,
              halign = scale,
      	      valign = scale,
      	      enabled = true,
	      standard = "pal",
	      format = "video4linux",
	      streamName = "/dev/video0",
	      frameRate  = 25.0,
	      frameTime <- tm.cycleTime
	      };

Screen s = 
    Screen { width  = 640,
	     height = 480,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     backgroundColor = 0xffffff,
	     frameRate = 1.0,
	     children = {  video }
};
