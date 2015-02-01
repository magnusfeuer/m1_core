//
// Test usage of remote constants
//

Message msg = @Message {};

const unsigned m2 = msg.message_mask_button;

const unsigned m1 = @Message.message_mask_button |
      	       	    @Message.message_mask_motion;



printf("m1 = %x\n", m1);
printf("m2 = %x\n", m2);
printf("m3 = %x\n", msg.message_mask_all);

.M1.halt = true;
