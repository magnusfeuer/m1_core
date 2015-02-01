/*
 * All rights reserved. Reproduction, modification, use or disclosure
 * to third parties without express authority is forbidden.
 * Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/rand.h>

#include <iostream>

#include "key.hh"
#include "key_store.hh"
#include "bio_stream.hh"

/* operation flags */
#define CONTENT_SIGN     0x01    /* sign input file */
#define CONTENT_VERIFY   0x02    /* verify input file */
#define CONTENT_ENCRYPT  0x04    /* sign and encrypt output file */
#define CONTENT_DECRYPT  0x08    /* decrypt input file */

/* 
 * Magden content signing tool
 */

/* usage: signtool <mode> [opts] <content-file>
 *       
 * mode:
 *   sign       Sign the content data but do not encrypt it.
 *
 *   encrypt    Encrypt and sign content data using a random password that is 
 *              stored encypted with public key encryption.
 *
 *   decrypt    Decrypt content data 
 *              (accept key-file.pub or file-file.key)
 *
 *   verify     Check the signature and possibly decryption
 *              (accept key-file.pub or file-file.key)
 *
 *  -s <name>   Signer key (needed for encryption and signing)          
 *  -d          Print some debug info.
 *  -o <file>   Output file name
 *  -K <path>   Add path to key storage.
 *  -c <level>  Compress content (before encrypt)
 *
 */
#define streq(a, b) (strcmp((a),(b)) == 0)

static bool is_suffix(string str, string suffix)
{
    int pos = (int)str.length() - (int)suffix.length();
    if (pos < 0)
	return false;
    return str.substr(pos) == suffix;
}

void usage()
{
    fprintf(stderr, "usage: signtool <mode> [opts] [<file>]\n"
	    "  mode:\n"
	    "      encrypt, sign, decrypt or verify\n"
	    "  opts:\n"
	    "   -d            debug output\n"
	    "   -o <file>     Ouput file name\n"
	    "   -s <signer>   signer key name (private key)\n"
	    "   -K <path>     add key directory path\n"
	    "   -c <level>    compress content\n"
	);
    exit(1);
}


void init(void)
{
    ERR_load_crypto_strings();
    OpenSSL_add_all_ciphers();
}


CKey* load_key(CBioStream* bi, bool verify)
{
    CKey* key = new CKey();
    if (key->read_key(bi, verify)) 
	return key;
    return NULL;
}

/*
 *  Sign a public key
 */
int verisign_key(CKey* sigkey, CBioStream* bi, CBioStream* bo,
		 string filename, int op)
{
    CKey* key;

    if (!(sigkey->getUsage() & M1_KEY_USAGE_KEY)) {
	fprintf(stderr, "key '%s' can not be used to sign other keys\n",
		sigkey->getName().c_str());
	return -1;
    }

    switch(op) {
    case CONTENT_VERIFY:
	if ((key = load_key(bi, true)) == NULL) {
	    fprintf(stderr, "could not load public key file\n");
	    return -1;
	}
	printf("key '%s' is ok\n", key->getName().c_str());
	return 1;

    case CONTENT_DECRYPT:
	if ((key = load_key(bi, true)) == NULL) {
	    fprintf(stderr, "could not load public key file\n");
	    return -1;
	}
	if (key->write_public(bo, false) < 0)
	    return -1;
	return 1;

    case CONTENT_ENCRYPT:
    case CONTENT_SIGN:
	if ((key = load_key(bi, false)) == NULL) {
	    fprintf(stderr, "could not load public key file\n");
	    return -1;
	}
	key->setSigner(sigkey->fingerprint());
	if (key->write_public(bo, true) < 0)
	    return -1;
	return 0;
    default:
	return -1;
    }
}


int main(int argc, char** argv)
{
    string sigkey_name;
    CKey*  sigkey;
    string content_filename;
    string output_filename;
    int i;
    int r;
    int c;
    int op;
    int compression_level = 0;
    CBioStream* bi;
    CBioStream* bo;
    string key_path[100];
    int ik = 0;
    char* ptr;

    // Appdend font paths from environment
    if ((ptr = getenv("M1_KEY_PATH")) != NULL) {
	do {
	    char* ep;
	    if ((ep = strchr(ptr, ':')) != NULL) {
		key_path[ik++] = string(ptr, (ep-ptr));
		ptr = ep+1;
	    }
	    else {
		key_path[ik++] = string(ptr);
		ptr = NULL;
	    }
	} while(ptr && *ptr);
    }

    if (argc < 2)
	usage();
    if (streq(argv[1], "sign"))
	op = CONTENT_SIGN;
    else if (streq(argv[1], "verify"))
	op = CONTENT_VERIFY;
    else if (streq(argv[1], "encrypt"))
	op = CONTENT_ENCRYPT;
    else if (streq(argv[1], "decrypt"))
	op = CONTENT_DECRYPT;
    else
	usage();

    // shift a way mode
    for (i = 1; i < argc; i++)
	argv[i] = argv[i+1];
    argc--;

    while((c = getopt(argc, argv, "s:K:o:c:d")) != -1) {
	switch(c) {
	case 's':
	    sigkey_name = string(optarg);
	    break;
	case 'o':
	    output_filename = string(optarg);
	    break;
	case 'c':
	    compression_level = atoi(optarg);
	    break;
	case 'd':
	    m1_debug_mask |= (M1DBG_INFO|M1DBG_WARN);
	    break;
	case 'K':
	    key_path[ik++] = string(optarg);
	    break;
	case '?':
	default:
	    usage();
	}
    }
    argc -= optind;
    argv += optind;

    init();
    bi = new CBioStream(); // Force seed of PRNG system on Linux.

    if (argc > 0) // ignore extra file right now
	content_filename = string(argv[0]);

    // Load all keys 
    for (i = 0; i < ik; i++) {
	DBGFMT("loading keys from directory %s", key_path[i].c_str());
	m1_keys().load_keys_dir(key_path[i]);
    }


    if (!(sigkey = m1_keys().key_by_name(sigkey_name)) &&  
	!(sigkey = m1_keys().key_by_fingerprint(sigkey_name)) &&
	!(sigkey = m1_keys().key_by_serial(sigkey_name))) {
	fprintf(stderr, "signer key '%s' not found\n",
		sigkey_name.c_str());
	exit(1);
    }

/*     fprintf(stderr, "signer key=%s, fingerprint=%s\n",  */
/* 	    sigkey->getName().c_str(), */
/* 	    sigkey->fingerprint().c_str()); */
	    

    // private key is required for encrypt / sign operation
    if (((op&CONTENT_SIGN) || (op&CONTENT_ENCRYPT)) && !sigkey->isPrivate()) {
	fprintf(stderr, "signer key '%s' is not private\n",
		sigkey_name.c_str());
	exit(1);
    }


    if (content_filename != "")
	bi->open_file_read(content_filename, 0, false, false);
    else
	bi->open_fp_read("**stdin*", stdin, 0, false, false);

    bo = new CBioStream();
    if (output_filename != "")
	bo->open_file_write(output_filename);
    else 
	bo->open_fp_write("*stdout*", stdout);

    // private key is usable for verify as well, since the public part
    // is embedded.
    if (is_suffix(content_filename, ".pub") ||
	is_suffix(content_filename, ".key"))
	r = verisign_key(sigkey, bi, bo, content_filename, op);
    else {
	switch(op) {
	case CONTENT_ENCRYPT:
	    bo->write_MAGIC();
	    bo->write_ENCRYPTION(sigkey);
	    if (compression_level)
		bo->write_COMPRESSION(compression_level, 0);
	    bo->write_DATA(0);
	    bo->stream_DATA(bi);
	    bo->flush();
	    bi->close();
	    bo->close();
	    r = 1;
	    break;
	case CONTENT_SIGN:
	    bo->write_MAGIC();
	    bo->write_SIGNED(bi);
	    bo->stream_DATA(bi);
	    bo->flush();
	    bo->write_SIGNATURE(sigkey, bi);
	    bi->close();
	    bo->close();
	    r = 1;
	    break;
	case CONTENT_VERIFY:
	case CONTENT_DECRYPT:
	    bo->stream_DATA(bi);
	    bo->flush();
	    bi->close();
	    bo->close();
	    r = 1;
	    break;
	}
    }	       
	    
    switch(op) {
    case CONTENT_ENCRYPT:
	if (r < 0) {
	    cout << content_filename << ": Could not be encrypted\n";
	    exit(1);
	}
	break;
    case CONTENT_SIGN:
	if (r < 0) {
	    cout << content_filename << ": Could not be signed\n";
	    exit(1);
	}
	break;
    case CONTENT_VERIFY:
	if (r==1) {
	    cout << content_filename << ": OK\n";
	    exit(0);
	}
	else {
	    cout << content_filename << ": FAILED\n";
	    exit(1);
	}
	break;
    case CONTENT_DECRYPT:
	if (r < 0) {
	    cout << content_filename << ": Could not be decrypted\n";
	    exit(1);
	}
	break;
    default:
	cout << content_filename << ": bad operation code "<< op << "\n";
	exit(1);
    }
    exit(0);
}
