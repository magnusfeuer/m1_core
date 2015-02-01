//
//  Test connect / script semantics
//
//
//

type Trigger {
    int id = 1;
     event int trigger = 5;     
     int trigger_count = 0;

     script trigger {
	    trigger_count++;
     	    printf("Trigger::trigger[%d]: %d\n", trigger_count, trigger);
    }

    script Trigger { printf("CONSTRUCT Trigger\n");   }
    script ~Trigger { printf("DESTRUCT Trigger[%d]\n", trigger_count);  }
};

enum ContainerOp { add, del, noop };

type ContainerCommand {
    public ContainerOp op;
    public Trigger     elem;
};

type AddCommand : ContainerCommand {
    ContainerOp @ContainerCommand.op = add;
};

type DelCommand : ContainerCommand {
    ContainerOp @ContainerCommand.op = del;
};

type NoopCommand : ContainerCommand {
    ContainerOp @ContainerCommand.op = noop;
    Trigger @ContainerCommand.elem = nil;
};

    

type Container {
     private Trigger tv[];	// vector (set) of triggers
     private int n = 0;         // actual number of triggers in tv

     public event queue ContainerCommand cmd;

     script cmd  {
     	 Trigger t;
	 int i;
	 printf("CONTAINER: { ");
     	 foreach i in [0:size(tv)-1] {
	 	 if (tv[i] == nil) printf("nil ");
		 else printf("id=%d[%u] ", tv[i].id, refcount(tv[i]));
	 }
	 printf("} \n");

	 switch(cmd.op) {
	 case add:
	     printf("got add[%d] %p\n", n, cmd.elem);
	     tv[n++] = cmd.elem;
	     break;
	 case del: {
	     int i;
	     printf("got del %p n=%d\n", cmd.elem, n);
	     foreach i in [0:n-1] {
		 if (tv[i] == cmd.elem) {
		     int j;
		     tv[i] = nil;
		     foreach j in [i:n-2] {
			 tv[j] = tv[j+1];
		     }
		     tv[n-1] = nil;
		     n--;
		     break;
		 }
	     }
	     break;
	 }
	 case noop:
	     printf("got noop %p n=%d\n", cmd.elem, n);
	     break;
	 }
	 // cmd.elem = nil;
     }
};

Container container = @Container {};
Trigger   t14 = @Trigger { id = 14 };

printf("t14 = %p\n", t14);

// container.cmd = @AddCommand { elem=@Trigger { id = 13 }};
container.cmd = @NoopCommand { };
container.cmd = @AddCommand { elem=t14 };
// container.cmd = @AddCommand { elem=@Trigger { id = 15 }};
container.cmd = @NoopCommand { };
container.cmd = @NoopCommand { };
container.cmd = @DelCommand { elem=t14 };
t14 = nil;
// container.cmd = @AddCommand { elem=@Trigger { id = 16 }};
container.cmd = @NoopCommand { };
container.cmd = @NoopCommand { };


.M1.halt = 10;
