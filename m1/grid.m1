//
// Sample rectangluar layout
//
//   +------+------+#|------+------+
//   |      |      |#|      |      |
//   |  AA  |  AB  |#|  BA  |  BB  |
//   |      |      |#|      |      |
//   +------+------+#+------+------+
//   |      |      |#|      |      |
//   |  AC  |  AD  |#|  BC  |  BD  |
//   |      |      |#|      |      |
//   +------+------+#|------+------+
//   +======+======|#+======+======+
//   +------+------+#|------+------+
//   |      |      |#|      |      |
//   |  CA  |  CB  |#|  DA  |  DB  |
//   |      |      |#|      |      |
//   +------+------+#+------+------+
//   |      |      |#|      |      |
//   |  CC  |  CD  |#|  DC  |  DD  |
//   |      |      |#|      |      |
//   +======+======|#+======+======+
//

application GridApp {

type MyTimer : Timer {
     // Just for debugging to see that Timers are released
     script MyTimer {
     	    printf("Timer created\n");
    }

     script ~MyTimer {
     	    printf("Timer destroyed\n");
    }

};

type MyColor : ColorInterpolator {
     script MyColor {
     	    printf("Color created\n");
    }

     script ~MyColor {
     	    printf("Color destroyed\n");
    }
};

// Hilight shape (highlight on focus)
type HShape : Shape {
     unsigned origColor;
     input event time done;
     bool running = false;

     script HShape {
	 wantFocus = true;
	 messageMask = @Message.message_mask_button;
     }

     script message {
     	 printf("message: cycle=%d, sender = %s\n", cycle(),sender(message));
	 switch(message.name) {
	 case buttonDown: {
	     printf("button-%d-down time=%u,x=%f,y=%f,x0=%f,y0=%f\n", 
		    message.value, message.timeStamp,
		    message.x, message.y,
		    message.x0, message.y0);
	     if ((message.value == 1) && !running) {
		 MyColor c = @MyColor { key={0,1}, 
					keyValue={0xffffff,fillColor}};
		 MyTimer t = @MyTimer { cycleInterval = 0.5, 
					autoDisconnect=true };
		 c.fraction <- t.fraction;
		 origColor = fillColor;
		 fillColor <- c.value;
		 done <- t.cycleTime;
		 running = true;
	     }
	     break;
	 }
	 case buttonUp:
	     printf("button-%d-up time=%u,x=%f,y=%f,x0=%f,y0=%f\n", 
		    message.value, message.timeStamp,
		    message.x, message.y,
		    message.x0, message.y0);
	     break;
	 case keyDown:
	     printf("key-%d-down time=%u,x=%f,y=%f,x0=%f,y0=%f\n", 
		    message.value, message.timeStamp,
		    message.x, message.y,
		    message.x0, message.y0);
	     break;
	 case keyUp:
	     printf("key-%d-up time=%u,x=%f,y=%f,x0=%f,y0=%f\n", 
		    message.value, message.timeStamp,
		    message.x, message.y,
		    message.x0, message.y0);
	     break;
	 case motion:
	     printf("motion time=%u,x=%f,y=%f,x0=%f,y0=%f\n", 
		    message.timeStamp, message.x, message.y,
		    message.x0, message.y0);
	     break;
	 default:
	     printf("message-%d value=%d, time=%u,x=%f,y=%f,x0=%f,y0=%f\n", 
		    message.name, message.value, message.timeStamp,
		    message.x, message.y,
		    message.x0, message.y0);
	 }
     }
	     
     script done {
            printf("done: done=%d, sender = %s\n", done, sender(done));
     	    fillColor <- nil;
	    fillColor = origColor;
	    // done <- nil;   // Release the timer?
	    // done = 0; // ???? (done is triggered twice????)
	    running = false;
     }

};



// Ax
Shape aa = HShape { top=0, left=0, contentWidth=160, contentHeight=120, 
      	   	   shape = 'r', fill=true, fillColor = 0xFF0000 };

Shape ab = HShape { top=0, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x00FF00 };
 
Shape ac = HShape { top=120, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x0000FF };
 		  
Shape ad = HShape { top=120, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0xFF00FF };
 
 // Bx
Shape ba = HShape { top=0, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0xCC0000 };
 
Shape bb = HShape { top=0, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x00CC00 };
 
Shape bc = HShape { top=120, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x0000CC };
 		  
Shape bd = HShape { top=120, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0xCC00CC };
 
 // Cx
Shape ca = HShape { top=0, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x550000 };
 
Shape cb = HShape { top=0, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x005500 };
 
Shape cc = HShape { top=120, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x000055 };
 		  
Shape cd = HShape { top=120, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x550055 };
 
 // Dx
Shape da = HShape { top=0, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x333300 };
 
Shape db = HShape { top=0, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x303303 };
 
Shape dc = HShape { top=120, left=0, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x003333 };
 		  
Shape dd = HShape { top=120, left=160, contentWidth=160, contentHeight=120,
       	   	   shape = 'r', fill=true, fillColor = 0x332233 };
 
 
 
Layer a = Layer { top = 0, left = 0, width = 320, height = 240,
      	  	  wantFocus = true,
       	  	  children = { aa, ab, ac, ad }};
 
Layer b = Layer { top = 0, left = 320, width = 320, height = 240,
      	  	  wantFocus = true,
       	  	  children = { ba, bb, bc, bd }};
 
Layer c = Layer { top = 240, left = 0, width = 320, height = 240,
      	  	  wantFocus = true,
       	  	  children = { ca, cb, cc, cd }};
 
Layer d = Layer { top = 240, left = 320, width = 320, height = 240,
      	  	  wantFocus = true,
       	  	  children = { da, db, dc, dd }};
 
Layer l = 
      Layer { top = 0, left = 0, width = 640, height = 480,
      	      wantFocus = true,
       	      children = { a, b, c, d } };
 
 
Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = (getenv("M1_BACKEND") == "") ? "x11" : 
	     		 getenv("M1_BACKEND"),
	     framebufferDevice = "/dev/fb0",
  	     wantFocus = true,
	     children = {  l } };


InputStrategy input_strategy = @InputStrategy {
	      layer = s
};

EpicInputDevice touch = @EpicInputDevice {
	    inputStrategy = input_strategy,
	    screen = s, // Used to extract mouse
	    maxUnscaledX = 640,
	    maxUnscaledY = 480,
	    minUnscaledX = 0,
	    minUnscaledY = 0,
	    scaledWidth = 640,
	    scaledHeight = 480
};

EventRecorder record;
EventPlayer replay;

event time start_recording;
event time stop_recording;

script start_recording {
    record = @EventRecorder { fileName = "/tmp/m1_recording.1",
			      enabled  = true };
    input_strategy.recorder = record;
    stop_recording <- @Timer { cycleInterval = 10 }.cycleTime;
    printf("Start recording - will stop in 10s\n");
}

script stop_recording {
       input_strategy.recorder = nil;
       record.enabled = false;
       printf("Recording stopped\n");
}

if (size(.M1.argv) >= 1) {
   if (.M1.argv[0] == "record")
      start_recording <- @Timer { cycleInterval = 2 }.cycleTime;
   else if (.M1.argv[0] == "reply") {
       printf("Replay\n");
       replay = @EventPlayer { fileName = "/tmp/m1_recording.1",
			       enabled  = true,
			       inputStrategy = input_strategy };
   }
}
      

// do interesting message dispatching
ba.message <- aa.message;
bb.message <- ab.message;
bc.message <- ac.message;
bd.message <- ad.message;

ca.message <- ba.message;
cb.message <- bb.message;
cc.message <- bc.message;
cd.message <- bd.message;

da.message <- ca.message;
db.message <- cb.message;
dc.message <- cc.message;
dd.message <- cd.message;


}
