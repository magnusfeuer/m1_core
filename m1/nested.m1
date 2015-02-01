//
// Test of nested event objects
//

event float scalar <- 
      @ScalarInterpolator { key={0,1}, keyValue={0,100},
      			    fraction <- @Timer {}.fraction }.value;
script scalar { printf("value=%f\n", scalar); }

		     