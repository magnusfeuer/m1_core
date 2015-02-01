type TimeText:  Text {
     SysTime timer = SysTime {};
     event unsigned year <- timer.year;
     event unsigned month <- timer.month;
     event unsigned day <- timer.day;
     event unsigned hour <- timer.hour;
     event unsigned minute <- timer.minute;
     event unsigned second <- timer.second;

     script year || month || day || hour || minute || second {
	 text = sprintf("%u-%u-%u %u:%u:%u",
			year, month, day,
			hour, minute, second);
     }
};

Screen s = 
    Screen { height = 480,
	     width  = 640,
	     pixelType = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
	     children = { 
		 TimeText { 
		     fontName = "Radio Space",
		     fontSize = 24,
		     fontWeight = medium,
		     fontSlant = roman,
		     fontColor = 0xFFFFFF,
		     top = 100,
		     left = 100
		 }
	     }
};
