//
// Timer test
//

event bool  my_active;
event float my_fraction;
event time  my_tick;
Timer t;

script my_active when my_active { printf("Activated\n"); }

script my_active when !my_active { printf("Deacivated\n"); }

script my_fraction { printf("Fraction = %f\n", my_fraction); }

script my_tick { printf("Tick = %u\n", my_tick); }

t = Timer { startTime = 5.0, cycleInterval = 4.0 };
my_active   <- t.active;
my_fraction <- t.fraction;
my_tick     <- t.tick;

