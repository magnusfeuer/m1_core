type Test {
     public event string key;
     public event string val;
     Database local_db = Database { };
   
     script val {
	    printf("Value[%s]\n", val);
     };
     
     local_db.getString <- key;
     val <- local_db.stringValue;
};

Database db = Database { device = "/tmp/m1.db" };

Test t1 = Test {};
Test t2 = Test {};

t1.key = "key1";
t2.key = "key2";
