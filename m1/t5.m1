
Timer t2 = Timer { cycleInterval = 2.0, loop=true};

Point pos2[] = { 
      Point { x =-50,  y=-50 },
      Point { x =-50,  y= 50 },
      Point { x = 50,  y= 50 },
      Point { x = 50,  y=-50 },
      Point { x =-50,  y=-50 }};

PositionInterpolator path2 =
    PositionInterpolator {
    		fraction <- t2.fraction,
    		key = { 0.0, 0.25, 0.5, 0.75, 1.0 },
		keyValue =  pos2
		}; 
