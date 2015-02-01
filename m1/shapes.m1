// -*- c++ -*-
// Random stuff demo
//
application Shapes {

    Timer t1 = @Timer { cycleInterval = 5.000, loop = true };
    Timer t2 = Timer { cycleInterval = 2.000, loop = true };
    Timer t3 = Timer { cycleInterval = 20.000,loop = true};

    Point p1[] = { Point { x =-100,  y=-100 },
		   Point { x = 100,  y=-100 },
		   Point { x = 100,  y= 100 },
		   Point { x =-100,  y= 100 },
		   Point { x =-100,  y=-100 } };

    ScalarInterpolator tran1 = 
	ScalarInterpolator {
	fraction <- t1.fraction,
	key = { 0.0, 0.5, 1.0 },
	keyValue = { 0.0, 1.0, 0.0 } };

    ColorInterpolator col1 = 
	ColorInterpolator {
	fraction <- t3.fraction,
	key      = { 0.0, 0.25, 0.5, 0.75, 1.0 },
	keyValue = { 0xFF0000,
		     0x00FF00,     // R -> G
		     0x0000FF,     // G -> B
		     0x00FF00,     // B -> G
		     0xFF0000      // G -> R
	}};


    ColorInterpolator blue = 
	ColorInterpolator {
	fraction <- t3.fraction,
	key      = { 0.0, 0.5, 1.0 },
	keyValue = { 0x000000,
		     0x0000FF,
		     0x000000 
	}};

    Point p2[] = { Point { x =-50,  y=-50 },
		   Point { x =-50,  y= 50 },
		   Point { x = 50,  y= 50 },
		   Point { x = 50,  y=-50 },
		   Point { x =-50,  y=-50 }};

    PositionInterpolator path2 =
	PositionInterpolator {
	fraction <- t2.fraction,
	key = { 0.0, 0.25, 0.5, 0.75, 1.0 },
	keyValue =  p2
    };

    Shape bg = 
	Shape { top = 2, left = 2,
		contentWidth = 636, contentHeight = 476,
		shape = rectangle,
		fill = true,
		fillColor <- col1.value,
		transparency = 0.0,
		borderWidth = 4,
		borderColor = 0x000000
    };

    Layer background = 
	Layer { top = 0, left = 0,
		children = { bg }
    };

    Shape blue_circle = 
	Shape { top  <- path2.yvalue,
		left <- path2.xvalue,
		contentWidth  = 100,
		contentHeight = 100,
		shape = 'e',
		fill = true,
		fillColor <- blue.value,
		transparency = 0.0,
		borderWidth = 0
    };

    PositionInterpolator path1 =
	PositionInterpolator {
	fraction <- t1.fraction,
	key = { 0.0, 0.25, 0.5, 0.75, 1.0 },
	keyValue = p1  };		
    
    ColorInterpolator red = 
	ColorInterpolator {
	fraction <- t3.fraction,
	key      = { 0.0, 0.5, 1.0 },
	keyValue = { 0x000000,
		     0xFF0000,
		     0x000000 
	}};

    Shape red_circle = 
	Shape {   
	top  <- path1.yvalue,
	left <- path1.xvalue,
	contentWidth  = 100,
	contentHeight = 100,
	shape = 'e',
	fill = true,
	fillColor <- red.value,
	transparency = 0.0,
	borderWidth = 0
    };

    Shape green_tri = 
	Shape { top = 0, left = 0,
		contentWidth  = 100,
		contentHeight = 100,
		shape = 't',
		fill = true,
		fillColor = 0x00FF00,
		transparency <- tran1.value,
		borderWidth = 0
    };

    Image image1 = 
	Image { top = 0, left = 0,
		background = false,
		backgroundColor = 0x00FF00,
		imageFile = "../images/image.png",
		transparency = 0.1,
		borderWidth = 0
    };

    Image image2 = 
	Image { top = 0, left = 0,
		background = false,
		backgroundColor = 0x00FF00,
		imageFile = "../images/Qwerty.png",
		transparency <- tran1.value,
		borderWidth = 0
    };

    
// 30fps = 1/30
    Timer t30 = Timer { cycleInterval = 1/30.0,  loop = true };
    
    Timer t24 = Timer { cycleInterval = 1/24.0,  loop = true };

    Video video1 = 
	Video { top = 0, left = 0,
		loop = true,
		enabled = true,
		streamName = "../images/wg_gdo_1.mpg",
		frameTime <- t30.cycleTime,
		transparency = 0.5
    };
    
    Timer tv3sz = Timer { cycleInterval = 3.000, loop = true };

    
    ScalarInterpolator video3_sz = 
	ScalarInterpolator {
	fraction <- tv3sz.fraction,
	key = { 0.0, 1.0 },
	keyValue = { 0.1, 3 } };
    

    ScalarInterpolator video3_alpha = 
	ScalarInterpolator { fraction <- tv3sz.fraction,
			     key = { 0.0, 0.8, 1.0 },
			     keyValue = { 0.2, 0.5, 1.0 } };
    

    Video video3 = 
	Video { top = 0, left = 0,
		hscale  <- video3_sz.value,
		vscale  <- video3_sz.value,
		loop = true,
		enabled = true,
		streamName = "../images/wg_gdo_2.mpg",
		frameTime <- t30.cycleTime,
		transparency <- video3_alpha.value
    };
    
    DDS dds1 = 
	DDS { top = 0, left = 0,
	      // @Timer { cycleInterval=5,loop=true }.fraction,
	      transparency = 0.8,
	      ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	      value <- @Timer { cycleInterval=4, loop=true }.fraction };

    DDS dds2 = 
	DDS { top = 0, left = 130,
	      transparency <- @Timer { cycleInterval=7,loop=true }.fraction,
	      ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	      value <- @Timer { cycleInterval=3, loop=true }.fraction };

    Timer text_t = Timer { cycleInterval=2, loop=true };

    ColorInterpolator text_color = 
	ColorInterpolator { fraction <- text_t.fraction,
			    key = {0.0, 1.0},
			    keyValue = { 0xFF0000, 0x0000FF }};

    Text text = 
	Text { top = 0, left = 0,
	       text = .M1.version,
	       fontName = "Times",
	       fontSize = 18,
	       fontWeight = bold,
	       fontSlant = italic,
	       fontColor <- text_color.value };


    Point qpos[] = { Point { x =-640,  y=3  },
		     Point { x =-640,  y=3  },
		     Point { x = 25,   y=3  },
		     Point { x = 25,   y=3  },
		     Point { x = -640,  y=3 } };


    PositionInterpolator qpath =
	PositionInterpolator {	fraction <- t3.fraction,
				key = { 0.0, 0.5, 0.55, 0.8, 1.0 },
				keyValue = qpos  };

    Layer qwerty =
	Layer { top <- qpath.yvalue,
		left <- qpath.xvalue,
		enabled = true,
		children = { image2 }};

    Layer foreground = 
	Layer { top=0, left=0,
		// width=300, height=300, clip=true,
		children =
		{
		    Layer { left = 90, top  = 240,
			    children = { blue_circle, red_circle  } },
		    
		    Layer { left = 300, top  = 50,
			    children = { green_tri, image1 }
		    },
		    
		    Layer { left = 200, top  = 200,
			    children = { video1 }
		    },

		    Layer { left = 0, top  = 0,
			    children = { video3 }
		    },
		    
		    Layer { left = 100, top = 100,
			    children = { dds1, dds2 } },

		    Layer { left = 400, top = 50,
			    children = { text } },

		    Layer { left = 450, top  = 240,
			    children = { red_circle, blue_circle }
		    }}
    };

    Timer skin_t = Timer { cycleInterval = 5.0, loop = true };

    ScalarInterpolator skin_sz =
	ScalarInterpolator { fraction <- skin_t.fraction,
			     key = { 0.0, 0.5, 0.9, 1 },
			     keyValue = { 1, 1, 0.1, 1 } };

    Layer skin = Layer {
	top = 0, left = 0,
	height=480, width=640,
	hscale  <- skin_sz.value,
	vscale  <- skin_sz.value,      
	children = { background, foreground }
    };
//
// Video recorder
// NOTE that this is also installed as a layer component to
//      display a scaled down image of the display
//
    Video record = 
	Video { top = 0, left = 512, width = 128, height = 96, 
		halign = scale,
		valign = scale,
		mode = out,
		enabled = true,
		streamName = "shapes.mpg" };

    event time stop_record <- @Timer { cycleInterval = 10.0 }.cycleTime;

    Screen s;

    script stop_record {
	printf("Recording stopped\n");
	record.enabled = false;
    }

    script Shapes {
	string be = getenv("M1_BACKEND");
	
	if (be == "")
	    be = "x11";

	s = @Screen { height = 480,
		      width  = 640,
		      pixelType = "bgra",
		      backendType = be,
		      frameRate = 60,
		      framebufferDevice = "/dev/fb0",
		      background = true,
		      recorder = record,
		      children = {  skin, qwerty, record }
	};
    }
}
