//
// Dispatch performance
//
library SwitchApp {

const int n = 10;

type Dispatch {
    input  event int in_message;
    output event int out_message;
    int id = -1;
    unsigned count = 0;

    script in_message {
	printf("id:%d in_message=%d\n", id, in_message);
	out_message = in_message + 1;
    }
};

Dispatch vec[n];

{
    int i;

    foreach i in [0:n-1] {
	vec[i] = @Dispatch { id=i };
    }

    foreach i in [0:n-2] {
	vec[i].in_message <- vec[i+1].out_message;
    }

    vec[n-1].in_message <- vec[0].out_message;

    // Lets start the loop
    vec[0].in_message = 1;
}

}
