//
// Circuit sorting
//

type Random {
    output event float value;
    private event float tick <- @Timer { loop=true }.tick;

    script tick {
	value = randomf();
    }
};

// "Circuit" that sorts two inputs
type Sort2 {
    event float iv[2];
    event float ov[2];

    script {
	if (iv[0] < iv[1]) { 
	    ov[0] = iv[0];
	    ov[1] = iv[1];
	}
	else {
	    ov[0] = iv[1];
	    ov[1] = iv[0];
	}
    }
};

// "Circuit" that sorts four inputs
type Sort4 {
     event float iv[4];
     event float ov[4];
     Sort2  a01 = @Sort2 { iv[0] <- iv[0], iv[1] <- iv[1] };
     Sort2  a23 = @Sort2 { iv[0] <- iv[2], iv[1] <- iv[3] };

     Sort2  b01 = @Sort2 { iv[0] <- a01.ov[0], iv[1] <- a23.ov[0] };
     Sort2  b23 = @Sort2 { iv[0] <- a01.ov[1], iv[1] <- a23.ov[1] };

     ov[0] <- b01.ov[0];
     ov[1] <- b01.ov[1];
     ov[2] <- b23.ov[0];
     ov[3] <- b23.ov[1];
};



Sort2 s2 = @Sort2 { iv[0] = 2.0, iv[1] = 1.0 };
Sort4 s4 = @Sort4 { iv[0] <- @Random{}.value, 
		    iv[1] <- @Random{}.value,
		    iv[2] <- @Random{}.value,
		    iv[3] <- @Random{}.value };

event float updated2 <- s2.ov[0];
event float updated4 <- s4.ov[0];

script updated2 {
    printf("s2.ov = %p\n", s2.ov);
}

script updated4 {
    printf("s4.ov = %p\n", s4.ov);
}


.M1.halt = 10;
