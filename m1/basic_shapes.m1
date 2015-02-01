
type Ellipse : Shape {
     ShapeForm @Shape.shape = ellipse;
     float @Shape.left = 10;
     float @Shape.contentWidth = 100;
     float @Shape.contentHeight = 75;
};

Ellipse ellipse0 = @Ellipse { top = 10, 
		   	      fill = true, 
			      fillColor = 0x000000 };
Ellipse ellipse1 = copy(ellipse0);
Ellipse ellipse2 = copy(ellipse0);

Layer shapes[] = 
{
	ellipse0,
	ellipse1,
	ellipse2,

	@Shape { shape = rectangle,
	         top = 10, left = 150,
		 contentWidth = 100, contentHeight = 75,
		 fill = true, fillColor = 0xff0000 },

	@Shape { shape = rectangle,
	         top = 92, left = 150,
		 contentWidth = 100, contentHeight = 75,
		 fill = true, fillColor = 0x00ff00,
		 borderWidth = 5, borderColor = 0xf0ff0f
	       },

	@Shape { shape = rectangle,
	         top = 178, left = 150,
		 contentWidth = 100, contentHeight = 75,
		 borderWidth = 5, borderColor = 0x00ff00
	       },


	@Shape { shape = triangle,
	         top = 10, left = 300,
		 contentWidth = 100, contentHeight = 75,
		 fill = true, fillColor = 0xff0000 },

	@Shape { shape = triangle,
	         top = 92, left = 300,
		 contentWidth = 100, contentHeight = 75,
		 fill = true, fillColor = 0x00ff00,
		 borderWidth = 5, borderColor = 0x000000
	       },

	@Shape { shape = triangle,
	         top = 178, left = 300,
		 contentWidth = 100, contentHeight = 75,
		 borderWidth = 5, borderColor = 0x0000ff
	       }

};


Screen screen = 
   @Screen { height = 480, width  = 640,
	     backendType = "auto",
	     background = true,
	     backgroundColor = 0x777777,
	     children = shapes
};

ellipse0.fillColor = 0xff0000;

ellipse1.top = 92;
ellipse1.borderWidth = 5;
ellipse1.fillColor = 0x00ff00;
ellipse1.borderColor = 0xff00ff;

ellipse2.top = 178;
ellipse2.fill = false;
ellipse2.borderWidth = 5;
ellipse1.fillColor = 0x0000ff;
ellipse2.borderColor = 0xf0ff0f;

