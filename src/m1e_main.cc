//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007
//
#include <dlfcn.h>
#include <errno.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/engine.h>
#include <unistd.h>

#include "m1.hh"
#include "m1vm.hh"
#include "m1c.hh"
#include "font_cache.hh"
#include "screen_component.hh"

#include "db_component.hh"
#include "key_store.hh"
#include "m1_parser.hh"


#define M1_DEFAULT_ENGINE "padlock"

static bool vmload = false;

static char* strndup(char* s, size_t n)
{
    char* copy = (char*) malloc(n+1);
    memcpy(copy, s, n);
    copy[n] = '\0';
    return copy;
}

void usage()
{
    fprintf(stderr, "m1e: usage: m1e [-K path] [-L path] [-F path] [-Dopts] [-e <engine>] [-d] [-M] file...\n");
    exit(1);
}

void crypto_init(char* engine_id)
{
    ENGINE* e = NULL;

    CBioStream x; // Forces seeding of PRNG
    ERR_load_crypto_strings();

    OpenSSL_add_all_ciphers();
    OpenSSL_add_all_digests();

    if ((engine_id != NULL) && (e = ENGINE_by_id(engine_id)) == NULL)
	printf("Warning: could not find engine '%s'\n", engine_id);
    if (e != NULL) {
	if (!ENGINE_init(e)) {
	    fprintf(stderr, "Warning: could initialize engine '%s'\n", 
		    engine_id);
	}
	else {
	    if (!ENGINE_register_complete(e)) {
		fprintf(stderr, "Warning: could not register engine methods\n");
	    }
	}
    }
}


int load(CExecutor* aExec, std::string* fname, CBioStream* bio)
{
    VmEvalType* vmType;
    VmMachineType* vmMachine = NULL;
    UData object;
    M1Lex* lexer;
    M1Parser* parser;
    M1TypeSpecifier* def;

    lexer = new M1Lex(bio, fname, 0);
    parser = new M1Parser(lexer);
    parser->set_debug_level(0);

    DBGFMT("--- PARSE BEGIN %s ---", (*fname).c_str());
    if (parser->parse(&def) != 0)
	exit(1);
    delete lexer;
    delete parser;
    if (M1DBG_IS_SET(M1DBG_PRNT))
	def->print(&cout, 0);
    DBGFMT("--- LINT BEGIN %s ---", (*fname).c_str());
    if ((vmType = lintDefinitions(def)) == NULL) {
	DBGFMT("--- LINT ERROR ---");
	exit(1);
    }
    if (M1DBG_IS_SET(M1DBG_PRNT))
	printDefinitions(vmType, &cout);
    cout.flush();

    if (vmload) {
	DBGFMT("--- VMLOAD BEGIN ---");
	vmMachine = loadDefinitions(vmType);
	if (M1DBG_IS_SET(M1DBG_PRNT))
	    vmMachine->printCode(&cout);
	DBGFMT("--- VMLOAD END ---");
    }

    // Evaluate defintions in m1_defs put bindings into environment
    DBGFMT("--- EVAL BEGIN ---");
    if (vmMachine) {
	object = vmMachine->produce(aExec, NULL, NULL);
	if (vmMachine->isLibrary()) {
	    CBaseType* ct = (CBaseType*) m1_context().type();
	    ct->updateSubType(Q_PUBLIC, vmMachine->name(), vmMachine);
	}
    }
    else {
	object = vmType->produce(aExec, NULL, NULL);
    }

    // propagte value that may been updated in constructor
    if (isAType(CExecutable*, object.o))
	((CExecutable*)object.o)->propagateAllUpdated(aExec);

    DBGFMT("--- EVAL END ---");
    return 0;
}

extern CScreenList m1_screenList;

bool do_halt = false;
int  halt_cycle_count = 1;

void VmMain::execute(CExecutor* aExec)
{
    if (m1Halt.updated()) {
	if ((halt_cycle_count = m1Halt.value()) <= 0) {	
	    do_halt = false;
	    halt_cycle_count = 1;
	}
	else
	    do_halt = true;
    }
}


int main(int argc, char** argv)
{
    CScreenList::iterator sp;
    char* ptr;
    char* lib_path[100];
    char* key_path[100];
    string file_name[100];
    int il = 0;
    int ik = 0;
    int in = 0;
    int c;
    int i;
    int simd_mode = EPIC_SIMD_AUTO;
    char* engine_id = M1_DEFAULT_ENGINE;
#if defined(M1_DRM)
    bool  requireEncryption = true;
#else
    bool  requireEncryption = false;
#endif
    bool  requireSignature  = false;

    setlinebuf(stdout);
    setlinebuf(stderr);
    // Appdend font paths from environment
    if ((ptr = getenv("M1_FONT_PATH")) != NULL) {
	do {
	    char* ep;
	    if ((ep = strchr(ptr, ':')) != NULL) {
		string path = string(ptr, (ep - ptr));
		m1_fonts().appendPath(path);
		ptr = ep+1;
	    }
	    else {
		m1_fonts().appendPath(ptr);
		ptr = NULL;
	    }
	} while(ptr && *ptr);
    }

    // Appdend font paths from environment
    if ((ptr = getenv("M1_LIB_PATH")) != NULL) {
	do {
	    char* ep;
	    if ((ep = strchr(ptr, ':')) != NULL) {
		lib_path[il++] = strndup(ptr, (ep-ptr));
		ptr = ep+1;
	    }
	    else {
		lib_path[il++] = strdup(ptr);
		ptr = NULL;
	    }
	} while(ptr && *ptr);
    }

    // Appdend font paths from environment
    if ((ptr = getenv("M1_KEY_PATH")) != NULL) {
	do {
	    char* ep;
	    if ((ep = strchr(ptr, ':')) != NULL) {
		key_path[ik++] = strndup(ptr, (ep-ptr));
		ptr = ep+1;
	    }
	    else {
		key_path[ik++] = strdup(ptr);
		ptr = NULL;
	    }
	} while(ptr && *ptr);
    }

    m1_debug_mask = M1DBG_WARN;  // always warn!!!

    while((c = getopt(argc, argv, "e:E:L:K:D:F:dMc:")) != -1) {
	switch (c) {
	case 'M':
	    vmload = true;
	    break;
	case 'd':
	    m1_debug_mask |= (M1DBG_INFO|M1DBG_WARN);
	    break;
	case 'D':
	    m1_debug_mask |= M1DBG_WARN;
	    if (optarg == NULL)
		m1_debug_mask |= M1DBG_INFO;
	    if (optarg != NULL) {
		while((c=*optarg++)) {
		    switch(c) {
		    case 'A': m1_debug_mask |= M1DBG_ALL; break;
		    case 'I': m1_debug_mask |= M1DBG_INFO; break;
		    case 'M': m1_debug_mask |= M1DBG_MEM; break;
		    case 'm': m1_debug_mask |= M1DBG_MNFO; break;
		    case 'L': m1_debug_mask |= M1DBG_LINT; break;
		    case 'C': m1_debug_mask |= M1DBG_COMP; break;
		    case 'E': m1_debug_mask |= M1DBG_EVAL; break;
		    case 'X': m1_debug_mask |= M1DBG_EXEC; break;
		    case 'P': m1_debug_mask |= M1DBG_PRNT; break;
		    case 'W': m1_debug_mask |= M1DBG_WARN; break;
		    case 'T': m1_debug_mask |= M1DBG_TYPE; break;
		    case 'S': m1_debug_mask |= M1DBG_SCHD; break;
		    default: break;
		    }
		}
	    }
	    break;
	case 'E':
	    if (optarg != NULL) {
		while((c=*optarg++)) {
		    switch(c) {
		    case 'A': simd_mode = EPIC_SIMD_AUTO; break;
		    case 'e': simd_mode = EPIC_SIMD_EMU; break;
		    case 'm': simd_mode |= EPIC_SIMD_MMX; break;
		    case 's': simd_mode |= EPIC_SIMD_SSE2; break;
		    case 'a': simd_mode |= EPIC_SIMD_ALTIVEC; break;
		    default: break;
		    }
		}
	    }
	    break;
	case 'L':
	    lib_path[il++] = strdup(optarg);
	    break;
	case 'K':
	    key_path[ik++] = strdup(optarg);
	    break;
	case 'F':
	    m1_fonts().appendPath(optarg);
	    break;
	case 'e':
	    engine_id = optarg;
	    break;

	case 'c':
	    DBGFMT_ADDCLS(optarg);
	    break;
	case '?':
	default:
	    usage();
	}
    }
    argc -= optind;
    argv += optind;

    lib_path[il] = NULL;
    key_path[ik] = NULL;

    crypto_init(engine_id);

    m1_init(lib_path);
    epic_debug(EDBG_INFO);
    epic_init(simd_mode);

    m1_fonts().load();               // Load all fonts
    m1_system().executor()->reset(); // fix global scope
    m1_styles();                     // make sure styles are created

    // Load all keys 
    for (i = 0; i < ik; i++)
	m1_keys().load_keys_dir(string(key_path[i]));

    // Temporary store of all filenames
    for (i = 0; i < argc; i++) {
	if (strcmp(argv[i], "++") == 0) {
	    i++;
	    break;
	}
	file_name[i] = string(argv[i]);
	in++;
    }
    file_name[i] = string("");

    // Store all m1e arguments
    if (i < argc) {
	CArray* args = m1_main().at("argv").arr;
	while(i < argc) {
	    args->elementAppend(m1_system().executor(),
				UString(m1New(CString, argv[i])));
	    i++;
	}
    }

    if (in == 0) {
	CBioStream* bio = new CBioStream();
	bio->open_fp_read("*stdin*", stdin,M1_KEY_USAGE_M1,
			  requireEncryption, requireSignature);
	file_name[0] =  string("*stdin*");
	load(m1_system().executor(), &file_name[0], bio);
    }
    else {
	for (i = 0; i < in; i++) {
	    CBioStream* bio = new CBioStream();
	    int r;

	    r = bio->open_file_read(file_name[i],M1_KEY_USAGE_M1,
				    requireEncryption, requireSignature);
	    if (r < 0) {
		fprintf(stderr, "m1e: unable to open file %s (%s)\n",
			file_name[i].c_str(), strerror(errno));
		bio->close();
		continue;
	    }
	    load(m1_system().executor(), &file_name[i], bio);
	    bio->close();  // close it, sweeper will delete it!
	}
    }

    while(halt_cycle_count > 0) {
	m1_system().sweep();
	sp = m1_screenList.begin();
	while(sp != m1_screenList.end()) {
	    (*sp)->redraw(&m1_system(),  NULL); 
	    sp++;
	}
	if (CDatabaseComponent::database())
	    CDatabaseComponent::database()->flush(); // Will only flush if dirty.
	if (do_halt)
	    halt_cycle_count--;
    }

#ifdef DEBUG
    m1_dump_stat(&cerr);
#endif    
    exit(0);
}

