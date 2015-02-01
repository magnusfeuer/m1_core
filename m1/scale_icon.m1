//
// Icon scaling example
//

Image icon0 = @Image {
      transparency = 0,
      hscale = 1.0,
      vscale = 1.0,
      transparency = 0,
      pixelType = "bgra",
      imageFile = "../../../skins/src/gui/trunk/OS-MenuIcon-GearCalibration-0.png" };

Image icon1 = @Image {
      hscale = 1.0,
      vscale = 1.0,
      transparency = 0.5,
      pixelType = "bgra",
      imageFile = "../../../skins/src/gui/trunk/OS-MenuIcon-GearCalibration-1.png" };

ScalarInterpolator scale = @ScalarInterpolator {
    key      = { 0.0, 0.5, 1.0 },
    keyValue = { 1.0, 2.0, 1.0 }
};

// Used to center the icons for scaling
Layer icon_center = @Layer { 
    top  = 240, 
    left = 320,
    children = { icon0, icon1 }
};

Timer t = @Timer { cycleInterval = 0.5, loop = true };

Screen s = 
    @Screen { height     = 480,
	     width       = 640,
	     pixelType   = "bgra", // 32 bit LE mode working with ffmepg!!!
	     backendType = "x11",
	     framebufferDevice = "/dev/fb0",
	     background = true,
  	     wantFocus = true,
	     children = {  icon_center }
};

// Let 0,0 be in center of image
icon0.top = -icon0.contentHeight/2;
icon0.left = -icon0.contentWidth/2;

icon1.top   = -icon1.contentHeight/2;
icon1.left  = -icon1.contentWidth/2;

scale.fraction <- t.fraction;
icon0.hscale <- scale.value;
icon0.vscale <- scale.value;
