//
//
//
application UI_test {
    Screen s;
    InputStrategy input_strategy;
    EpicInputDevice touch;

    :UI:TextButton button;
    :UI:Dialog     dialog;
    event bool buttonPressed;

    //
    // Test to inherit screen
    //

     type Screen1 : Screen {
         private Image pointer = @Image { 
	     transparency = 1.0,
	     imageFile = "../images/highlight_pointer32.png" 
	  };
	  private float offsetX = pointer.contentWidth  / 2;
	  private float offsetY = pointer.contentHeight / 2;
	  private bool down;
          private Timer t;

	  script message when down && message.name == motion {
	      pointer.left = message.x - offsetX;
	      pointer.top  = message.y - offsetY;
	  }

	  script message when message.name == buttonDown && message.value==1 {
	      printf("DOWN\n");
	      down = true;
	      t = nil;
	      pointer.left = message.x - offsetX;
	      pointer.top = message.y - offsetY;
	      pointer.transparency <- nil;
	      pointer.transparency = 0.0;
	  }
	  
	  script message when message.name == buttonUp && message.value==1 {
	      printf("UP\n");
	      down = false;
	      t = @Timer { cycleInterval=0.3, autoDisconnect=true };
	      pointer.transparency <- t.fraction;
	  }

	  script Screen1 {
	      wantFocus = true;
	      messageMask = @Message.message_mask_button;
	      children[size(children)] = pointer;
	  }
    };


    script buttonPressed {
	printf("ButtonPressed\n");
    }


    printf("M1 version = %s\n", .M1.version);

    button = :UI:TextButton { top = 50, left = 50, text = "PressMe" };
    buttonPressed <- button.pressed;

    dialog = :UI:Dialog { text = "Do you want to quit now?",
			  top = 240, left = 100, backgroundColor = 0x3fee3f };


    s = @Screen1 { height = 480,
		   width  = 640,
		   pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
		   backendType = "x11",
		   framebufferDevice = "/dev/fb0",
		   wantFocus = true,
		   messageMask = @Message.message_mask_button,
		   background = true,
		   backgroundColor = 000000,
		   children = {  button, dialog   }
    };

    input_strategy = InputStrategy { layer = s };

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

