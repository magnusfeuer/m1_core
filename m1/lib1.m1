
library Lib1 {

	int count = 4;

	type T {
	     event bool print;
	     private int count = 7;

	     script print {
	     	 T t = @T {};
		 printf("Lib1:T:print:  count = %d, .Lib1.count=%d t.count=%d\n", count, .Lib1.count, t.count);
	     }

	     script T {
		 printf("Lib1:T: count = %d, .Lib1.count=%d\n", count, .Lib1.count);
	     }
	};
}
