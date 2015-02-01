//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#ifndef __PFCOMMON__
#define __PFCOMMON__
#include "sqlcommon.hh"


#define PF_CONT_REGULAR 'r'
#define PF_CONT_HARDLINK 'l'
#define PF_CONT_SYMLINK 'S'
#define PF_CONT_DIRECTORY 'd'
#define PF_CONT_BLOCKDEV 'b'
#define PF_CONT_CHARDEV 'c'
#define PF_CONT_FIFO 'f'
#define PF_CONT_UNIXSOCK 's'
#define PF_CONT_PACKFILE 'p'

// map DB_CONT_XXX to PF_CONT_XXX
const char file_type_map[] = {
    0,
    PF_CONT_REGULAR,
    PF_CONT_HARDLINK,
    PF_CONT_SYMLINK,
    PF_CONT_DIRECTORY,
    PF_CONT_BLOCKDEV,
    PF_CONT_CHARDEV,
    PF_CONT_FIFO,
    PF_CONT_UNIXSOCK,
    PF_CONT_PACKFILE
};


extern bool hasEncryptionSuffix(string aPath, CStringList &aList);

extern char *memsearch(char *start, unsigned char *pattern, int file_len, int pat_len);

extern int encrypt_pubkey(unsigned char* pubkey, unsigned int pubkey_len,
			  unsigned char* out_buf, unsigned int out_len,
			  unsigned char* pass, unsigned int pass_len);


extern bool patch_m1e_devkey(char *content, 
			     int content_len,
			     CKeyData *old_key,
			     CKeyData *new_key,
			     const char *passwd,
			     int passwd_len);

extern char *sign_pubkey(char *content, 
			 unsigned long content_size, 
			 unsigned long &new_content_size, 
			 CKeyData *magden_key_data, 
			 CKey *dev_key_obj);

extern CKey *setup_priv_key(MYSQL *db, string key_name);


#endif // __PFCOMMON__

