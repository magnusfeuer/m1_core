type Base {
     signed x = 10;

     printf("Base constructor x=%d\n", x);
};

type Mid : Base {
     signed y = 11;

     printf("Mid constructor y=%d\n", y);
};

type Sub : Mid {
     signed z = 12;

     printf("Sub constructor z=%d\n", z);
     x = 1;
};
     

Sub obj = Sub{ y = 2 };

printf("obj.x = %d, obj.y = %d, obj.z = %d\n", obj.x, obj.y, obj.z);
.M1.halt = true;

