// A test library

library UI {

bool modalActive = false;


type TextButton : Layer {
     output event bool pressed;
     event string text;

     private Timer   t1;
     private Text         textField;

     script message when message.name == buttonDown && message.value==1 {
	 printf("Button pressed\n");
	 t1 = Timer { cycleInterval = 0.3 };
     }

     script message when message.name == buttonUp && message.value==1 {
	 t1 = nil;
	 printf("Button activated\n");
	 if (message.x > -1)
	     pressed = true;
     }

     printf("CONSTRUCT: :UI:TextButton\n");

     // setup children
     textField = Text { text <- text, 
     	       	        wantFocus = true,
     	       	        background=true, 
     	       	      	backgroundColor = 0x4455cc };
     width  <- textField.contentWidth;
     height <- textField.contentHeight;
     wantFocus = true;
     messageMask = @Message.message_mask_button;
     background = true;
     backgroundColor = 0x00FF00;

     children = { textField };
};

type Dialog : Layer {
     event string ok = "Ok";
     event string cancel = "Cancel";
     event string text = "Enter text?";
     output event signed accept = false;
     // implementation section
     private event bool okPressed;
     private event bool cancelPressed;
     private TextButton okButton     = TextButton { text <- ok };
     private TextButton cancelButton = TextButton { text <- cancel };
     private Text textField          = Text { text = "Hello world\n" };
     // Layer defaults for dialog
     unsigned @Layer.backgroundColor = 0xeecc00;

     script okPressed {
     	    printf("%s\n", ok); 
	    accept = true;
     }

     script cancelPressed { 
     	    printf("%s\n", cancel); 
	    accept = false; 
     }

     printf("CONSTRUCT: :UI:Dialog\n");
     okPressed     <- okButton.pressed;
     cancelPressed <- cancelButton.pressed;

     textField.top  = 10;
     textField.left = 10;

     okButton.top     <- 30 + textField.contentHeight;
     okButton.left    <- 20;

     printf("okButton.left = %f\n", okButton.left);

     cancelButton.top <- 30 + textField.contentHeight;

     printf("cancelButton.top = %f\n", cancelButton.top);

     cancelButton.left <- okButton.left + okButton.contentWidth + 20;

     printf("cancelButton.left = %f\n", cancelButton.left);

     height <- 50  + okButton.height + textField.height;

     printf("height = %f\n", height);

     width  <- 100  + okButton.width + cancelButton.width + textField.height;

     printf("width = %f\n", width);


     wantFocus = true;
     messageMask = @Message.message_mask_button;
     background = true;

     children = { textField, okButton, cancelButton };
};



}  // UI




