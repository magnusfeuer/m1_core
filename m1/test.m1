// -*- c++ -*-
// Write a test
//
bool fail = false;
string result;

if ((result = sprintf("%.0f", 170.0)) != "170") {
   printf("18: %s != 170\n", result);
   fail = true;
}

if ((result = sprintf("%.0f", -170.0)) != "-170") {
   printf("18b: %s != -170\n", result);
   fail = true;
}

if ((result = sprintf("%4.0f", 0.1)) != "   0") {
    printf("19: [%s] != [   0]\n", result);
    fail = true;
}

if ((result = sprintf("%3.0f", -0.4999)) != " -0") {
    printf("20: [%s] != [ -0]\n", result);
    fail = true;
}

if ((result = sprintf("%1.*f", 0, 0.064)) != "0") {
    printf("21: [%s] != [0]\n", result);
    fail = true;
}

if ((result = sprintf("%5.0f", 0.0)) != "    0") {
    printf("22: [%s] != [    0]\n", result);
    fail = true;    
}

if ((result = sprintf("%5.0f", -0.5)) != "   -1") {
    printf("23: [%s] != [   -1]\n", result);
    fail = true;
}

if (fail)
   printf("FAIL\n");
else
   printf("PASSED\n");
.M1.halt = true;
