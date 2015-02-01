//
// Subclass skin to a specific one
//

type TeddyBear: :SkinLib:SkinBase {
     vec = { 
	 @:SkinLib:SkinBase:Gauge { owner = this, name = "kph", top = 0, left = 0 },
	 @:SkinLib:SkinBase:Gauge { owner = this, name = "rpm", top = 0, left = 100 }
     };
};

TeddyBear tb_skin = TeddyBear {};

printf("First gauge[%s]\n", tb_skin.vec[0].name);
printf("Second gauge[%s]\n", tb_skin.vec[1].name);


