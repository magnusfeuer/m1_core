type Base {
};

type Owned {
     Base parent;
};

type Container : Base {
	Owned child;

	if (child != nil) child.parent = this;
};

Container obj = Container { child = Owned {} };
// Alt obj.child.parent = obj;