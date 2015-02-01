//
// Script implementation of tmp_main
//
//
// To run this:
//
// Check out and build m1/plugins/pi_sys3_channel
// Start m1e with first argument pointing to directory where
// pi_sys3_channel.so resides.
// The caterhamdump.pi3 file is available in plugins/pi_sys3_channel
//


Layer background = 
    DDS { top = 0, left = 0,
	  enabled = true,
	  ddsFile = "../images/TB-BACKGROUND-BlueGlow.dds",
	  transparency = 0.0
};


Layer tbsmall =
	DDS {
	    top = 0, left = 0,
	    enabled = true,
	    ddsFile = "../images/BlueGlow-TB-Small-Solid-Normal.dds",
	    value = 0.5,
	    transparency = 0.0
//	     value  <- rpm_to_gauge.value
	    // transparency <- cycle.value
};

Screen s = Screen { 
       height = 480,  
       width  = 640,
       pixelType = "bgra",
       backendType = "auto",
       framebufferDevice = "/dev/fb0",
       children = {  background, tbsmall  } 
};

