type OuterLayer: Layer {
    type InnerLayer {
	// ERROR SHOULD NOT COMPILE!
	// We cannot access OuterLayer:transparency from an
	// encapsulated class instance.
	// Gives runtime nil scope exception today.
	printf("bug37.m1: FAIL\n");
	transparency = 1.0; 
    };
};

OuterLayer:InnerLayer layer = @OuterLayer:InnerLayer {};
.M1.halt = true;
    
