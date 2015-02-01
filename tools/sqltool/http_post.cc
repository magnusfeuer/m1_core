 
#include <stdio.h>
#include <curl/curl.h>
#include <stdlib.h>
 
int main(int argc, char *argv[])
{
    CURL *curl;
    CURLcode res;
    char buf[512];

    if (argc != 4) {
	printf("Usage: %s host cgi-script query-field\n", argv[0]);
	exit(0);
    }

    if (!(curl = curl_easy_init())) {
	puts("No curl init()");
	exit(-1);
    }

    sprintf(buf, "%s/cgi-bin/%s", argv[1], argv[2]);
    curl_easy_setopt(curl, CURLOPT_POST, 1);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, argv[3]);
	
    curl_easy_setopt(curl, CURLOPT_URL, buf);
    res = curl_easy_perform(curl);
 
    /* always cleanup */
    curl_easy_cleanup(curl);
    return 0;
}
