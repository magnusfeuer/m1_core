//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#ifndef __KEY_UTIL_HH__
#define __KEY_UTIL_HH__

#include <stdio.h>
#include <string.h>
#include <ctype.h>

extern char* string_trim(char* ptr);
extern int decode_hex(char* src, int src_len, unsigned char* dst, int dst_len);
extern int encode_hex(unsigned char* src, int src_len, char* dst, int dst_len);
extern int decode_base64(char* src, unsigned char* dst, int dst_len);
extern int encode_base64(unsigned char* src, int src_len, 
			 char* dst, int dst_len);
extern int encode_fingerprint(unsigned char* src, int src_len, 
			      char* dst, int dst_len);

#endif

