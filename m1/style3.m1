

Style s1 = @Style { name = "plain", fontName = "Radio Space", 
      	          fontWeight=medium, fontSlant=roman, fontSize = 12 };

String t1 = @String { class = "plain", value = "Hej" };

type Later {
     event time doit;

     script doit {
     printf("t1 font = %s\n", t1.style.fontName);
     .M1.halt = true;
     }
};
Later cmd;

printf("t1 font = %s\n", t1.style.fontName);


@Style { name = "plain", fontName = "BankGothic Md BT",
      	 fontWeight=medium, fontSlant=roman, fontSize = 14 };

cmd = @Later{ doit <- @Timer { cycleInterval=1, loop=true }.cycleTime };

