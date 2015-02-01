//
// Timeout test
//
library TimeoutApp {

Timeout tmo;

event bool timeout;
float time_start;

script timeout {
       printf("Timeout: duration=%f\n", now() - time_start);
       .M1.halt = 1;
}

tmo = @Timeout { duration = 5.4 };
timeout <- tmo.timeout;
time_start = now();

}

