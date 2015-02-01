//
// Subclass skin to a specific one
//

type SubGauge: :SkinLib:SkinBase:Gauge {
     string tp;
};

type TeddyBear: :SkinLib:SkinBase {
     vec = { 
	 @SubGauge { owner = this, name = "kph", tp = "speedo", top = 0, left = 0 },
	 @SubGauge { owner = this, name = "rpm", tp = "tacho", top = 0, left = 100 }
     };
};

TeddyBear tb_skin = TeddyBear {};

printf("First gauge type[%s] [%s]\n", @SubGauge(tb_skin.vec[0]).tp, tb_skin.vec[0].name);
printf("Second gauge type[%s] [%s]\n", @SubGauge(tb_skin.vec[1]).tp, tb_skin.vec[1].name);

