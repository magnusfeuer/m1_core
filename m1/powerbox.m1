//
// Powerbox can testing
//
//
library PowerBox {

    enum PowerBoxCommand {
	powerBoxSet    = 1,
	powerBoxGet    = 2,
	powerBoxNotify = 3
    };

    // Channel info for storing values
    type PowerBoxChannel {
	unsigned       chan;      // channel id 0 - 8
	event float    pwm;       // pwm value [0,200] => [0.0, 1.0]
	event unsigned frequency; // frequency 
	event float    an1;       // current   [0,1023] => [0,1.0] (0-+3.3v)
	event bool     test;      // test value
    };

    type PowerBoxFrame : CanFrame {
	event unsigned @CanFrame.length = 8;
	unsigned       chan;      // own channel id 0 - 8
	event float    pwm;       // pwm
	event unsigned frequency; // frequency
	event float    an1;       // current
	event bool     test;      // test value
    };

    // CanFrame for SET/GET commands
    type PowerBoxOutputFrame : PowerBoxFrame {
	event unsigned @CanFrame.id = 0x1F;
    };

    // CanFrame for NOTIFY's
    type PowerBoxInputFrame : PowerBoxFrame {
	event unsigned @CanFrame.id = 0xF1;
    };

    PowerBoxChannel     cin[9];   // 0 is a test channel
    PowerBoxOutputFrame cset[9];  // issue set commands per channel
    PowerBoxOutputFrame cget[9];  // issue get commands per channel

    type PowerBoxNotify : PowerBoxInputFrame {
	script when data[0] == 3 { // NOTIFY
	    unsigned cid    = (data[2]<<8)|data[3];
	    unsigned value = (data[4]<<24)|(data[5]<<16)|
	                      (data[6]<<24)|data[7];

	    if ((cid >= 0x31) && (cid <= 0x38)) {
		cid -= 0x30;
		cin[cid].pwm = (value >= 0xC8) ? 1.0 : (value / 200.0);
	    }
	    else if ((cid >= 0x41) && (cid <= 0x48)) {
		cid -= 0x40;
		cin[cid].frequency = (value == 0) ? 0 : (15000000 / value);
	    }
	    else if ((cid >= 0x121) && (cid <= 0x128)) {
		cid -= 0x120;
		cin[cid].an1 = value / 1023.0;
	    }
	    else if (cid == 2) {
		cin[0].frequency = value;  // should be 0x01020304
		cin[0].test = true;
	    }
	    printf("Got: ~p\n", this);
	}
    };

    PowerBoxNotify notify;
    CanDevice device;

    // This is the writer side of the channel
    type PowerBoxSetFrame : PowerBoxOutputFrame {

	script pwm {
	    unsigned value = @unsigned(pwm*200);
	    unsigned cid = chan + 0x30;
	    printf("Sending SET pwm=%f chan=%d, cid=%x\n", pwm, chan, cid);
	    data[0] = 1;  // SET
	    data[1] = 0;
	    data[2] = (cid >> 8) & 0xff;
	    data[3] = cid & 0xff;
	    data[4] = (value >> 24) & 0xff;
	    data[5] = (value >> 16) & 0xff;
	    data[6] = (value >> 8) & 0xff;
	    data[7] = value & 0xff;
	    device.outputs = this;  // queue command
	}

	script frequency {
	    unsigned value = frequency ? (15000000 / frequency) : 0;
	    unsigned cid  = chan + 0x40;
	    printf("Sending freuency=%d, chan=%d\n", frequency, chan);
	    data[0] = 1; // SET
	    data[1] = 0;
	    data[2] = (cid >> 8) & 0xff;
	    data[3] = cid & 0xff;
	    data[4] = (value >> 24) & 0xff;
	    data[5] = (value >> 16) & 0xff;
	    data[6] = (value >> 8) & 0xff;
	    data[7] = value & 0xff;
	    device.outputs = this;  // queue command
	}

	script test {
	    printf("Sending test frame\n");
	    data[0] = 2;  // GET
	    data[1] = 0;
	    data[2] = 0;
	    data[3] = 2;
	    data[4] = 0;
	    data[5] = 0;
	    data[6] = 0;
	    data[7] = 0;
	    device.outputs = this;  // queue command
	}
    };

    type PowerBoxGetFrame : PowerBoxOutputFrame {
	script pwm {
	    unsigned cid = chan + 0x30;
	    printf("Sending GET pwm chan=%d\n", chan);
	    data[0] = 2;  // GET
	    data[1] = 0;
	    data[2] = (cid >> 8) & 0xff;
	    data[3] = cid & 0xff;
	    data[4] = 0;
	    data[5] = 0;
	    data[6] = 0;
	    data[7] = 0;
	    device.outputs = this;  // queue command
	}

	script frequency {
	    unsigned cid  = chan + 0x40;
	    printf("Sending GET freuency, chan=%d\n", chan);
	    data[0] = 2; // GET
	    data[1] = 0;
	    data[2] = (cid >> 8) & 0xff;
	    data[3] = cid & 0xff;
	    data[4] = 0;
	    data[5] = 0;
	    data[6] = 0;
	    data[7] = 0;
	    device.outputs = this;  // queue command
	}

	script an1 {
	    unsigned cid  = chan + 0x120;
	    printf("Sending GET AN1, chan=%d\n", chan);
	    data[0] = 2; // GET
	    data[1] = 0;
	    data[2] = (cid >> 8) & 0xff;
	    data[3] = cid & 0xff;
	    data[4] = 0;
	    data[5] = 0;
	    data[6] = 0;
	    data[7] = 0;
	    device.outputs = this;  // queue command
	}
    };


    //
    // Initialize the power box com
    //
    script PowerBox {
	int i;
	device = @CanUSBDevice { format = base,
				 canSpeed  = 250,
				 port   = "/dev/tty.usbserial-LWQ6UYOM",
				 portSpeed = 115200
	};

	foreach i in [0:8] {
	    cin[i]  = @PowerBoxChannel   { chan = i };
	    cset[i] = @PowerBoxSetFrame  { chan = i };
	    cget[i] = @PowerBoxGetFrame  { chan = i };
	}

	notify  = @PowerBoxNotify  {};
	device.inputs  += notify;
    };
}
