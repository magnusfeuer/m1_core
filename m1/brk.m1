
string items[] = { "a", "b", "c" };

event int main;

type Search {
     input event string what;
     output event int index;

     script what {
     	    int i;
	    foreach i in [0:size(items)-1] {
	    	    if (items[i] == what) {
		       index = i;
		       break;
		    }
	    }
     }
};

script main {
       printf("index = %d\n", main);
       .M1.halt = true;
}

main <- @Search { what = "c" }.index;





