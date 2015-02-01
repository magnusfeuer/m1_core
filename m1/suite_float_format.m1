//
// Float format test
//

bool fail = false;
string result;

printf("Nan = %f\n", 0xFFFFFFFF);

if ((result = sprintf("%.2f", 0.12)) != "0.12") {
   printf("1: %s != 0.12\n", result);
   fail = true;
}


if ((result = sprintf("%.2f", 0.125)) != "0.12") {
   printf("2: %s != 0.12\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 0.126)) != "0.13") {
   printf("3: %s != 0.13\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 3.016)) != "3.02") {
   printf("4: %s != 3.02\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 3.013)) != "3.01") {
   printf("5: %s != 3.01\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 3.015)) != "3.02") {
   printf("6: %s != 3.02\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 3.045)) != "3.04") {
   if (result == "3.05")
      printf("7: %s != 3.04 (float precission problem!)\n", result);
   else {
      printf("7: %s != 3.04\n", result);
      fail = true;
   }
}

if ((result = sprintf("%.2f", 3.04501)) != "3.05") {
   printf("8: %s != 3.05\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 1.999)) != "2.00") {
   printf("9: %s != 2.00\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 9.999)) != "10.00") {
   printf("10: %s != 10.00\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", 99.996)) != "100.00") {
   printf("11: %s != 100.00\n", result);
   fail = true;
}
 
if ((result = sprintf("%.2f", -2.1349)) != "-2.13") {
   printf("12: %s != -2.13\n", result);
   fail = true;
}

if ((result = sprintf("%.2f", -2.1350)) != "-2.14") {
   if (result == "-2.13") 
      printf("13: %s != -2.14 (float precission problem!)\n", result);
   else {
      printf("13: %s != -2.14 (internal precission problem) \n", result);
      fail = true;
   }
}

if ((result = sprintf("%.2f", 0.0056)) != "0.01") {
   printf("14: %s != 0.01\n", result);
   fail = true;
}

if ((result = sprintf("%.2e", 0.00123)) != "1.23e-03") {
   printf("15: %s != 1.23e-03\n", result);
   fail = true;
}

if ((result = sprintf("%.2e", 0.001239)) != "1.24e-03") {
   printf("16: %s != 1.24e-03\n", result);
   fail = true;
}

if ((result = sprintf("%.2e", 1240.0)) != "1.24e+03") {
   printf("17: %s != 1.24e+03\n", result);
   fail = true;
}

if ((result = sprintf("%.0f", 170.0)) != "170") {
   printf("18: %s != 170\n", result);
   fail = true;
}

if (fail)
   printf("FAIL\n");
else
   printf("PASSED\n");
.M1.halt = true;

