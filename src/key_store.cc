//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <dirent.h>
#include <openssl/evp.h>
#include <openssl/err.h>

#include "key_store.hh"
#include "key_util.hh"

CKeyManager& m1_keys(void)
{
    static CKeyManager* keys = NULL;
    if (!keys) {
	keys = new CKeyManager();
	m1Retain(CKeyManager, keys);
    }
    return *keys;
}

//
// Sigleton component for handling key storage
//
CKeyManager::CKeyManager()
{
    add_key(CKey::device_key());
}

CKeyManager::~CKeyManager()
{
    CKeyList::iterator iter;

    for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
	m1Release(CKey, *iter);
    }
}

int CKeyManager::mark(Mark_t aMark)
{
    int marked = 0;
    if (mMark != aMark) {
	CKeyList::iterator iter;

	marked += CRuntime::mark(aMark);
	for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++)
	    marked += (*iter)->mark(aMark);
    }
    return marked;
}


string CKeyManager::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "KeyStore: #%lu 0x%lx", 
	    refCount(), (unsigned long) this);
    return string(ndata);
}

bool CKeyManager::add_key(CKey* aKey)
{
    m1Retain(CKey, aKey);
    mKeyList.push_back(aKey);

    DBGFMT("Added key: '%s' fingerprint='%s' [%s]", 
	   aKey->getName().c_str(),
	   aKey->fingerprint().c_str(),
	   aKey->isPrivate() ? "private" : "public");
    return true;
}

bool CKeyManager::del_key(CKey* aKey)
{
    CKeyList::iterator iter;
    for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
	if ((*iter) == aKey) {
	    m1Release(CKey, *iter);
	    mKeyList.erase(iter);
	    return true;
	}
    }
    return false;    
}

// 
// Delete key by finger print
//
bool CKeyManager::del_by_fingerprint(string aFingerPrint)
{
    CKeyList::iterator iter;
    for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
	if ((*iter)->fingerprint() == aFingerPrint) {
	    m1Release(CKey, *iter);
	    mKeyList.erase(iter);
	    return true;
	}
    }
    return false;
}

//
// Find key by finger print
//
CKey* CKeyManager::key_by_fingerprint(string aFingerPrint)
{
    CKeyList::iterator iter;
    for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
	if ((*iter)->fingerprint() == aFingerPrint)
	    return *iter;
    }
    return NULL;
}

// Key by name
CKey* CKeyManager::key_by_name(string aName)
{
    CKeyList::iterator iter;
    for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
	if ((*iter)->getName() == aName)
	    return *iter;
    }
    return NULL;
}

// Key by serial
CKey* CKeyManager::key_by_serial(string aSerial)
{
    CKeyList::iterator iter;
    for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
	if ((*iter)->getSerial() == aSerial)
	    return *iter;
    }
    return NULL;
}

//
// Load a key by BioStream
//

bool CKeyManager::load_key_bio(CBioStream* bi)
{
    CKey* key = m1New(CKey);

    if (key->read_key(bi, true)) {
	CKey* key1;

	if ((key1 = key_by_name(key->getName())) != NULL) {
	    if (key1->isResolved()) {
		if (key1->isPrivate()) {
		    // We already have a private key so ignore reading
		    DBGFMT_WARN("key '%s' already loaded", key->getName().c_str());
		    return false;
		}
		else {
		    // Replace with new key, delete the old one
		    DBGFMT_WARN("key '%s' replaced", key1->getName().c_str());
		    del_key(key1);
		}
	    }
	}
	add_key(key);
	if (key->isResolved()) {
	    bool resolved = true;

	    while(resolved) {
		CKeyList::iterator iter;

		resolved = false;
		for (iter = mKeyList.begin(); iter != mKeyList.end(); iter++) {
		    if (!(*iter)->isResolved()) {
			(*iter)->resolve();
			if ((*iter)->isResolved())
			    resolved = true;
		    }
		}
	    }
	}
	return true;
    }
    return false;
}


bool CKeyManager::load_key_mem(char *aKeyData, int aKeyLength)
{
    CBioStream* bi;
    bool result;

    bi = new CBioStream();
    if (bi->open_mem_read("-memory-", aKeyData, aKeyLength,0, false,false) < 0)
	return false;
    result = load_key_bio(bi);
    bi->close(); // will be delete by sweeper 
    return result;
}

bool CKeyManager::load_key_file(string aFileName)
{
    CBioStream* bi;
    bool result;

    DBGFMT("loading key '%s'", aFileName.c_str());
    bi = new CBioStream();
    if (bi->open_file_read(aFileName.c_str(),0,false,false) < 0)
	return false;
    result = load_key_bio(bi);
    bi->close(); // will be delete by sweeper 
    return result;
}

int CKeyManager::load_keys_dir(string aPath)
{
    DIR* dir;
    struct dirent* dp;
    int key_count = 0;

    if ((dir = opendir(aPath.c_str())) == NULL) {
	DBGFMT_WARN("unable to oped dir '%s'",aPath.c_str());
	return 0;
    }

    while((dp = readdir(dir)) != NULL) {
	char* fname = dp->d_name;
	char* sfx = strrchr(fname, '.');
	if (sfx && ((strcmp(sfx, ".pub") == 0) ||
		    (strcmp(sfx, ".key") == 0))) {
	    string filename = aPath + "/" + string(fname);
	    if (!load_key_file(filename))
		DBGFMT_WARN("key file '%s' not loaded", filename.c_str());
	    key_count++;
	}
    }
    closedir(dir);
    return key_count;
}
