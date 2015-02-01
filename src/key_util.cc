//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#include <ctype.h>

#include "key_util.hh"



char* string_trim(char* ptr)
{
    char* string;

    while(isspace(*ptr)) ptr++;
    string = ptr;
    ptr += (strlen(string) - 1);
    while((ptr > string) && isspace(*ptr)) ptr--;
    ptr++;
    *ptr = '\0';
    return string;
}

static int d_hex(int c)
{
    switch(c) {
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': 
	return (c-'A')+10;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': 
	return (c-'a')+10;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	return (c-'0');	
    default:
	return -1;
    }
}

int decode_hex(char* src, int src_len, unsigned char* dst, int dst_len)
{
    int len = 0;

    while((src_len >= 2) && (len < dst_len)) {
	int c1 = d_hex(src[0]);
	int c2 = d_hex(src[1]);
	if ((c1 < 0) || (c2 < 0))
	    return -1;
	*dst++ = (c1 << 4) | c2;
	len++;
	src += 2;
	src_len -= 2;
    }
    if ((src_len != 0) || (len >= dst_len))
	return -1;
    return len;
}

int encode_hex(unsigned char* src, int src_len, char* dst, int dst_len)
{
    char hc[17] = "0123456789ABCDEF";
    int len = 0;
    while(src_len > 0) {
	int c = *src++;
	if (dst_len < 3) 
	    return -1;
	dst[0] = hc[c >> 4];
	dst[1] = hc[c & 0xf];
	dst += 2;
	dst_len -= 2;
	src_len--;
	len++;
    }
    if (dst_len < 1)
	return -1;
    *dst = '\0';
    return len;
}


// Decode one base64 letter
#define B64MASK 0x3f
#define B64END  0x40
#define B64NONE 0x41

static int d_b64(int c)
{

    switch(c) {
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': 
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': 
    case 'Y': case 'Z':
	return c-'A';
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': 
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x': 
    case 'y': case 'z':
	return (c-'a')+26;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
	return (c-'0')+52;
    case '+': return 62;
    case '/': return 63;
    case '=': return B64END; // 0 when masked!!!
    default: return -1;
    }
}
//
// Convert b64 (ignore white space) from '\0' terminated src buffer
// and put the result in dst buffer with a max length dst_len
// return length of resulting buffer or -1 on error:
//  buffer to small
//  bad base64 characters
//  bad base64 padding sequence
//
int decode_base64(char* src, unsigned char* dst, int dst_len)
{
    int len = 0;

    while(*src && (len<dst_len)) {
	int c0,c1,c2,c3;

	c0=c1=c2=c3=-1;

	while(isspace(*src)) src++;
	if (*src) c0 = d_b64(*src++);
	
	while(isspace(*src)) src++;
	if (*src) c1 = d_b64(*src++);

	while(isspace(*src)) src++;
	if (*src) c2 = d_b64(*src++);

	while(isspace(*src)) src++;
	if (*src) c3 = d_b64(*src++);

	if ((c0<0)||(c1<0)||(c2<0)||(c3<0))
	    return -1;
	if ((c0==B64END)||(c1==B64END))
	    return -1;
	*dst++ = (c0<<2)|(c1>>4);
	len++;
	if ((c2==B64END) && (c3==B64END)) break;
	if (c2==B64END) return -1;
	if (len>=dst_len) return -1;
	*dst++ = (c1<<4)|(c2>>2);
	len++;
	if (c3==B64END) break;
	if (len>=dst_len) return -1;
	*dst++ = (c2<<6)|c3;
	len++;
    }
    return len;
}

int encode_base64(unsigned char* src, int src_len, char* dst, int dst_len)
{
    char b64c[65] = 
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	"abcdefghijklmnopqrstuvwxyz"
	"0123456789"
	"+/";
    int len = 0;

    while((src_len >= 3) && (dst_len >= 4)) {
	int c0 = src[0];
	int c1 = src[1];
	int c2 = src[2];
	
	dst[0] = b64c[c0>>2];
	dst[1] = b64c[((c0<<4)&0x3f)|(c1>>4)];
	dst[2] = b64c[((c1<<2)&0x3f)|(c2>>6)];
	dst[3] = b64c[(c2 & 0x3f)];
	
	src += 3;
	src_len -= 3;

	dst += 4;
	dst_len -= 4;
	len += 4;
    }
    if (src_len > 0) {
	if (dst_len < 4) return -1;
	if (src_len == 1) {
	    int c0 = src[0];

	    dst[0] = b64c[c0>>2];
	    dst[1] = b64c[((c0<<4)&0x3f)];
	    dst[2] = '=';
	    dst[3] = '=';
	}
	else if (src_len == 2) {
	    int c0 = src[0];
	    int c1 = src[1];

	    dst[0] = b64c[c0>>2];
	    dst[1] = b64c[((c0<<4)&0x3f)|(c1>>4)];
	    dst[2] = b64c[((c1<<2)&0x3f)];
	    dst[3] = '=';
	}
	dst += 4;
	dst_len -= 4;
	len += 4;
    }
    if (dst_len < 1) return -1;
    *dst = '\0';
    return len;
}

int encode_fingerprint(unsigned char* src, int src_len, char* dst, int dst_len)
{
    char hc[17] = "0123456789ABCDEF";
    int i, j;

    if (dst_len < src_len*3)
	return -1;

    for (j = 0, i = 0; i < src_len; i++, j += 3) {
	int c    = src[i];
	dst[j]   = hc[c >> 4];
	dst[j+1] = hc[c & 0xf];
	dst[j+2] = ':';
    }
    dst[j-1] = '\0';
    return j;

}
