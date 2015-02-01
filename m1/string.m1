string str1 = "0123456789";
string str2 = "0123456789";
string str3 = "ABCDEFGHIJ";
event string str4 = "ABC";
string str5 = "";
bool str4_updated = false;

signed res = 0;

script str4 {
       printf("str4 = updated\n");
       str4_updated = true;
}

//
// Strlen
//
if (strlen(str1) == 10)
   printf("strlen passed\n");
else
   printf("strlen FAILED.\n");


//
// String compare
//
if (str1 == str2) 
   printf("Positive string compare passed\n");
else
   printf("Positive string compare FAILED.\n");

if (str1 == str3)  // bug10.m1
   printf("Negative string compare FAILED\n");
else
   printf("Negative string compare passed.\n");


//
// strcat
//
if (strcat(str1, str3) == "0123456789ABCDEFGHIJ")
   printf("strcat passed\n");
else
   printf("strcat FAILED.\n");

//
//  string + operator
//

if (str1 + str3 == "0123456789ABCDEFGHIJ")
   printf("string + passed\n");
else
   printf("string + FAILED.\n");

//
//  string += operator
//
str5 += str1;
str5 += str2;
str5 += str3;
if (str5 == "01234567890123456789ABCDEFGHIJ")
   printf("string += passed\n");
else
   printf("string += FAILED.\n");

// 
// Substr
//
if (substr(str1, 1, 2) == "12")
   printf("Positive substr passed\n");
else
   printf("Positive substr FAILED.\n");


if (substr(str1, 1, 2) == "AB")
   printf("Negative substr FAILED\n");
else
   printf("Negative substr PASSED.\n");

//
// strchr
//
res = strchr(str1, '4');

if (res == 4)
    printf("Positive strchr passed\n");
else
    printf("Positive strchr FAILED.\n");

res = strchr(str1, 'X');

if (res == -1)
    printf("Negative strchr passed\n");
else
    printf("Negative strchr FAILED.\n");



//
// Atoi
//
if (atoi("4711") == 4711) 
    printf("atoi passed\n");
else
    printf("atoi  FAILED.\n");

//
// Atou
//
if (atou("4711") == 4711) 
    printf("atou passed\n");
else
    printf("atou  FAILED.\n");

//
// Atof
//
if (atof("4711.1174") == 4711.1174) 
    printf("atof passed\n");
else
    printf("atof  FAILED.\n");

//
// sprintf
//
if (sprintf("%d", 4711) == "4711")
    printf("int sprintf passed\n");
else
    printf("int sprintf  FAILED.\n");

if (sprintf("%s", "Hello world") == "Hello world")
    printf("string sprintf passed\n");
else
    printf("string sprintf  FAILED.\n");

if (sprintf("%.2f", 4711.1174) == "4711.12")
    printf("float sprintf passed\n");
else
    printf("float sprintf  FAILED.\n");

if (str1[1] == '1')
    printf("str1[1] passed\n");
else
    printf("str1[1] failed '%c'\n", str1[1]);

// Simple access on event string
if (str4[1] == 'B')
    printf("str4[1] passed\n", str4[1]);
else
    printf("str4[1] failed '%c'\n", str4[1]);

// Simple updated on event string
str4[1] = 'X';
if (str4[1] == 'X')
    printf("str4[1] passed\n", str4[1]);
else
    printf("str4[1] failed '%c'\n", str4[1]);


// Bug10.m1 workaround
printf("INT:    4711 == [%s]\n", sprintf("%d", 4711));
printf("STRING: Hello world == [%s]\n", sprintf("%s", "Hello world"));
printf("FLOAT:  4711.11 == [%s]\n", sprintf("%.2f", 4711.1174));

.M1.halt = true;
