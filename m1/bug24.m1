type Owned {
     public int owned_init_val = 0;

     printf("Owned: owned_init_val = %d\n", owned_init_val);
};

type Test {
     public int init_val = 1;
     Owned my_obj;

     my_obj = Owned { owned_init_val = init_val };
     printf("Test: init_val = %d, my_obj.owned_init_val=%d\n", 
          init_val, my_obj.owned_init_val);
};

Test obj = Test { init_val = 2 };

if (obj.my_obj.owned_init_val == 2) 
   printf("Bug24.m1: PASS\n");
else
   printf("Bug24.m1: FAIL\n");
.M1.halt = 1;
 
