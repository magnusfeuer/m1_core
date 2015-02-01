//
// Object event queue test
//
enum EventType { mouse, keyboard, usb };
     
type Event {
     EventType eventType;
     float     eventTime;
     script Event { eventTime = now(); }
};

type MouseEvent : Event {
     Point xy;
     script MouseEvent { eventType = mouse; }
};

type KeyBoardEvent : Event {
     char key;
     script KeyBoardEvent { eventType = keyboard; }
};

type UsbEvent : Event {
     unsigned int vendor;
     unsigned int prod;
     script UsbEvent { eventType = usb; }
};

event queue Event evq;


script evq {
       printf("Got event: %p sender=%s\n", evq, sender(evq));
};

*evq = @MouseEvent { xy = @Point { x=1, y=1 } };  // TRIGGER to get it started!
evq = @MouseEvent { xy = @Point { x=32, y=30 } };
evq = @KeyBoardEvent { key = 'H' };
evq = @KeyBoardEvent { key = 'E' };
evq = @KeyBoardEvent { key = 'L' };
evq = @KeyBoardEvent { key = 'L' };
evq = @KeyBoardEvent { key = 'O' };
evq = @UsbEvent { vendor = 0xFF00FF, prod = 0x1234 };

.M1.halt = 20;

       
