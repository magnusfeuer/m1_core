//
// Magden style slider....
//
library Slider {

// Vertical slider
// width = 83
// min height = 41 + 41 + 1 = 83
// max height = 41 + 41 + 398 = 480
//
type Vertical : Layer {
     public event float value;
     public float minValue = 0.0;
     public float maxValue = 1.0;

     private bool down;
     private Image itop;
     private Image ibot;
     private Layer lmid;
     private Image ibutton;
     private ScalarInterpolator ypos;

     script Vertical {
	 itop = @Image { imageFile = "../images/slider_top_bg.png" };
	 lmid = @Layer {
	     top = 41,
	     width = 83,
	     clip = true,
	     children = {
		 @Image { imageFile = "../images/slider_mid_bg.png" }
	     }
	 };
	 ibot = @Image { imageFile = "../images/slider_bot_bg.png" };
	 ibutton = @Image { imageFile = "../images/slider_button.png" };
	 if (width != 83) width = 83;
	 if (height < 83) height = 83;
	 else if (height > 480) height = 480;

	 ibutton.top = 2;
	 ibutton.left = 2;
	 lmid.height <- height - 82;
	 ibot.top <- lmid.height + 41;
	 ypos = @ScalarInterpolator { key={minValue,maxValue}, 
				      keyValue={2, height-79}};
	 ypos.fraction <- value;
	 ibutton.top <- ypos.value;
	 children = {itop, lmid, ibot, ibutton };
	 messageMask = @Message.message_mask_button;
	 wantFocus = true;
     }

     script height {
	 if (height < 83) height = 83;
	 else if (height > 480) height = 480;
	 ypos.keyValue[1] = height-79;
	 value = value; // (ibutton.top+79) / height;
     }

     script width {
	 width = 83;
     }

     script value {
	 printf("value: %f\n", value);
     }

     script message when (message.name == buttonDown) && (message.value == 1) {
	 down = true;
	 // Turn on motion events
	 messageMask |= @Message.message_mask_motion;
	 value = message.y / height;
     }

     script message when (message.name == buttonUp) && (message.value == 1) {
	 down = false;
	 // Turn off motion events
	 messageMask &= ~@Message.message_mask_motion; 
     }

     script message when down && (message.name == motion) {
	 value = message.y / height;
     }
};

type FloatValueButton : Layer {
    public  bool  big = false;
    public  event float value;
    private Image left_but;
    private Image right_but;
    private Float mid_but;

    script FloatValueButton {
	int    sz    = big ? 31 : 15;
	int   fsz    = sz - 3;

	left_but = @Image { top = 0,
			    left = 0,
			    imageFile = sprintf("../images/left_hbutton_black_%d.png", sz)
	};
	mid_but = @Float { backgroundColor = 0x000000,
			   background = true,
			   fontName  = "BankGothic Md BT",
			   fontWeight = medium,
			   fontSlant = roman,
			   fontColor = 0xffffff,
			   fontSize = fsz,
			   glyphFixedWidth = fsz-4,
			   glyphDotKerning = -(fsz-2)/3,
			   top = 0,
			   height = sz,
			   left <- left_but.contentWidth,
			   format = "%.2f",
			   value <- value
	};
	right_but = @Image { top = 0,
			     left <- mid_but.left + mid_but.contentWidth,
			     imageFile = sprintf("../images/right_hbutton_black_%d.png",sz)
	};
	width <- left_but.contentWidth + 
	          mid_but.contentWidth + 
	          right_but.contentWidth;
	children = {left_but, mid_but, right_but};
    }
};


type Horizontal : Layer {
     public event float value;
     public float minValue = 0.0;
     public float maxValue = 1.0;

     private bool down;
     private Image ibutton;
     private Shape ruler;
     private ScalarInterpolator xpos;
     private event float xscale;

     script Horizontal {
	 ruler = @Shape { shape = rectangle,
	       	 	  top = height / 2,
			  contentWidth <- width,
			  contentHeight = 3,
			  fill = true,
			  fillColor = 0xff000000 };
	 ibutton = @FloatValueButton { big = (height >= 31), value <- value };
	 ibutton.top  = 2;
	 ibutton.left = 2;
	 xscale <- width - ibutton.width;
	 xpos = @ScalarInterpolator { key={minValue,maxValue}, 
				      keyValue={2, xscale}};
	 xpos.fraction <- value;
	 ibutton.left <- xpos.value;
	 children = {ruler, ibutton };
	 messageMask = @Message.message_mask_button;
	 wantFocus = true;
     }

     script width {
	 if (width < 50) width = 50;
	 else if (width > 640) width = 640;
	 printf("IButton width = %f\n", ibutton.width);
	 value = value;
     }

     script xscale {
	 xpos.keyValue[1] = xscale;
     }


     script height {
     }

     script value {
	 printf("value: %f\n", value);
     }

     script message when (message.name == buttonDown) && (message.value == 1) {
	 down = true;
	 messageMask |= @Message.message_mask_motion;
	 value = message.x / width;
     }

     script message when (message.name == buttonUp) && (message.value == 1) {
	 down = false;
	 messageMask &= ~@Message.message_mask_motion; 
     }

     script message when down && (message.name == motion) {
	 value = message.x / width;
     }
};



}
