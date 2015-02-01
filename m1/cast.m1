
type Base {
     signed x = 7;
};

type Derived : Base {
     signed y = 8;
};

type Derived2 : Base {
     signed b = 8;
};

Base object = Derived {};

Base object2 = @Derived2(object);

Text a = Text {};

Layer b = Text { text="Hej" };

Layer c = nil;

signed foo = nil;

printf("x = %d\n", @Derived(object).y);
printf("width = %f\n", @Text(b).contentWidth);

.M1.halt = true;




