
event signed g = 1;

type X {
     event signed x;

     script { printf("X:x=%d\n", x); }
};

type Y { 
     event signed y;

     type A : X {
          event signed y_ax;
	  script x { printf("Y_A:X x=%d\n", x);  }
	  script { printf("Y_A:X y_ax=%d\n", y_ax); }
     };

     type B {
          event signed b = 14;

          type C : A {
	       event signed y_b_ca;
	       script x { printf("Y_B_C:A x=%d\n", x);  }
	       script b { printf("Y_B_C:A b=%d\n", b);  }
	       script { printf("Y_B_C:A y_b_ca=%d\n", y_b_ca); }
     	  };

	  C c = @C {};

	  script { printf("Y_B b=%d\n", b); }


     };

     A a = @A {};
     B b = @B {};
};

X x = @X {};
Y y = @Y {};

y.b.c.y_b_ca = 17;
y.b.c.x = 7;
.M1.halt = 2;


