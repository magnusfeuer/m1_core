//
// Test alignment with in w/h
//

type Alignment {
     VAlign v;
     HAlign h;
};

event VAlign valign;
event HAlign halign;
event time   next;

// Combinations using top,bottom,right,left,center (but not scale yet)
Alignment test[] = {
	  Alignment { v = top, h = left },
	  Alignment { v = center, h = left },
	  Alignment { v = bottom, h = left },
	  Alignment { v = bottom, h = center },
	  Alignment { v = center, h = center},
	  Alignment { v = top,    h = center },
	  Alignment { v = top,    h = right },
	  Alignment { v = center, h = right },
	  Alignment { v = bottom, h = right } };

signed index = 0;


Timer t = Timer { cycleInterval = 2.0, loop = true };

// Align with with in an area
Shape area = 
      Shape { top = 0, left = 0, width = 50, height = 50,
	      valign <- valign,
	      halign <- halign,
      	      contentWidth = 20, contentHeight = 20,
	      background = true,
	      backgroundColor = 0xff00ff,
	      shape = ellipse,
	      fill = true,
	      fillColor = 0xff0000 };

// Align with a horizontal line
Shape hline = 
      Shape { top = 0, left = 100, width = 50, height = 0,
	      valign <- valign,
	      halign <- halign,
      	      contentWidth = 20, contentHeight = 20,
	      background = true,
	      backgroundColor = 0xff00ff,
	      shape = ellipse,
	      fill = true,
	      fillColor = 0x00ff00 };

// Align with a vertical line
Shape vline = 
      Shape { top = 0, left = 180, width = 0, height = 100,
	      valign <- valign,
	      halign <- halign,
      	      contentWidth = 20, contentHeight = 20,
	      background = true,
	      backgroundColor = 0xff00ff,
	      shape = ellipse,
	      fill = true,
	      fillColor = 0x0000ff };

// Align with a point
Shape point = 
      Shape { top = 100, left = 100, width = 0, height = 0,
	      valign <- valign,
	      halign <- halign,
      	      contentWidth = 20, contentHeight = 20,
	      background = true,
	      backgroundColor = 0xff00ff,
	      shape = ellipse,
	      fill = true,
	      fillColor = 0x00ffff };
      
Layer content = 
      Layer { top = 50, left = 50,
      	      width = 200, height = 200,
	      backgroundColor = 0xff0000,
	      background = true,
	      children = { area, hline, vline, point } };

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     backgroundColor = 0xffffff,	
	     background = true,
	     children = {  content }
};

//
// Local scripts
//

script next {
       index = (index + 1) % size(test);
       valign = test[index].v;
       halign = test[index].h;
};

next <- t.cycleTime;

