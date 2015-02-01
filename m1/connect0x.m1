
int a = 13;
int b = 14;

event int c;

type T0 {
     int p0;
     int p1;
     event int out;

     script {
     	     out = p0 + p1;
     }

     script T0 {
     	    **out = p0 + p1;
     }
};
   
c <- @T0 { p0 = a, p1 = b }.out;

.M1.halt = true;
