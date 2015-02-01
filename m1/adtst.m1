application MyApp {
    ADDevice dev = @ADDevice { port = "/dev/ttyUSB0", portSpeed=115200 };
    int i;
    
    foreach i in [0:15] {
	dev.channels[i].sampleInterval = 10;
	printf("value[%d] interval[%d] ts[%d]\n", 
	       dev.channels[i].value,
	       dev.channels[i].sampleInterval,
	       dev.channels[i].timeStamp);
    }
}
