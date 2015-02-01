//
// Test register / reregister
//

type AStyle : Style {

     script AStyle { printf("Style %s[%u] created\n", name, this); }
     script ~AStyle { printf("Style %s[%u] destroyed\n", name, this); }
};

@AStyle { name = "a", fontName = "Radio Space", 
  	  fontWeight=medium, fontSlant=roman, fontSize = 12 };

@AStyle { name = "b", fontName = "Radio Space", 
  	  fontWeight=medium, fontSlant=roman, fontSize = 14 };

@AStyle { name = "a", fontName =  "BankGothic Md BT",
  	  fontWeight=medium, fontSlant=roman, fontSize = 12 };	

@AStyle { fontName =  "BankGothic Md BT",
  	  fontWeight=medium, fontSlant=roman, fontSize = 14 };	

.M1.halt = 4;


