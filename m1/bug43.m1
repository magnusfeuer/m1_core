//
// As bug 42 but with call expression
//
library Bug43 {
	event string evs;
	event int param;

	script Bug43 {
	       evs <- sprintf("param=%d", param);  // loosing reference here
	       .M1.halt = 3;
	}
}
