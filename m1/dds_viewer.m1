
Screen screen;
string fileArg;
Layer  clipLayer;

printf("dds_view: %p\n", .M1.argv);

screen = 
    @Screen { height = 480, width  = 800,
    	      pixelType = "bgra",
	      backendType = "x11",
	      background = true,
	      backgroundColor <-
	      @ColorInterpolator { key = {0,0.25,0.5,0.75,1.0},
				   keyValue = {0xffffff,
					       0xff0000,
					       0x00ff00,
					       0x0000ff,
					       0xffffff},
				   fraction <-
				   @Timer { cycleInterval=10, 
					    loop=true }.fraction }.value,
	      children = {}
};

if (getenv("DDS_VIEWER_CLIP") != "") {
   printf("dds_viewer: using clip\n");
   clipLayer = @Layer { top=0, left=0, width=300, height=480, clip=true};
   screen.children += clipLayer;
}

foreach fileArg in .M1.argv {
    DDS dds = @DDS { top = 0, left = 0,
		     transparency = 0.0,
		     ddsFile = fileArg,
		     value <- @Timer { cycleInterval=4, loop=true }.fraction
    };		     
    if (clipLayer != nil)
        clipLayer.children += dds;
    else
        screen.children += dds;
}


