//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#ifndef __KEY_MANAGER_HH__
#define __KEY_MANAGER_HH__

#include <stdio.h>
#include <string.h>
#include <openssl/evp.h>
#include <list>
using namespace std;

#include "m1rt.hh"
#include "key.hh"
#include "bio_stream.hh"


typedef list<CKey*> CKeyList;
    
class CKeyManager : public CRuntime {
public:
    CKeyManager();

    ~CKeyManager();

    int mark(Mark_t aMark);

    string debugName(void);

    int load_keys_dir(string aPath);
    bool load_key_file(string aFileName);
    bool load_key_mem(char *aKeyData, int aKeyLength);

    CKey* key_by_fingerprint(string aFingerPrint);
    CKey* key_by_name(string aName);
    CKey* key_by_serial(string aSerial);

private:
    bool  add_key(CKey*);
    bool  del_key(CKey*);
    bool  del_by_fingerprint(string aKeyId);

    bool load_key_bio(CBioStream* bi);

    list<CKey*> mKeyList;
};

extern CKeyManager& m1_keys(void);

#endif
