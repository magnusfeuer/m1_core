const signed a = 10;

event signed value;

type Base0 {
     event signed y;
     event signed z;

     event signed x;
     signed b = 12;
     
     type T1 {
     	  input event signed i0;
	  input event signed i1;
	  output event signed out;

	  script {
	  	 out = (i0 + b)*i1 + a;
          }
     };

     x <- @T1 { i0 <- y, i1 <- z }.out;
};

script value {
       printf("value = %d\n", value);
       .M1.halt = true;
}

// result should be (2 + 12)*3 + 10 == 52
value <- @Base0 { y=2, z=3 }.x;



