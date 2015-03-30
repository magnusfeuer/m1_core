//
// M1 Language tokenizer/parser/compiler
//

#include <errno.h>
#include <openssl/bio.h>

#include "m1c.hh"
#include "epx.h"
#include "m1_parser.hh"
#include "key.hh"
#include <unistd.h>

static char* strndup(char* s, size_t n)
{
    char* copy = (char*) malloc(n+1);
    memcpy(copy, s, n);
    copy[n] = '\0';
    return copy;
}

void usage()
{
    fprintf(stderr, "m1c: usage: m1e [-L path] [-d] file...\n");
    exit(1);
}


int compile(std::string* fname, CBioStream* bio)
{
    int n;
    VmEvalType* vmType;
    VmMachineType* vmMachine;
    M1Lex* lexer;
    M1Parser* parser;
    M1TypeSpecifier* def;

    lexer = new M1Lex(bio, fname, 0);
    parser = new M1Parser(lexer);
    parser->set_debug_level(1);

    DBGFMT("--- PARSE BEGIN %s ---", fname->c_str());
    if (parser->parse(&def) != 0)
	return -1;
    delete lexer;
    delete parser;
    if (M1DBG_IS_SET(M1DBG_PRNT))
	def->print(&cout, 0);
    DBGFMT_LINT("--- LINT BEGIN %s ---", fname->c_str());
    if ((vmType = lintDefinitions(def)) == NULL) {
	DBGFMT_LINT("--- LINT ERROR ---");
	return -2;
    }
    if (M1DBG_IS_SET(M1DBG_PRNT))
	printDefinitions(vmType, &cout);

    vmMachine = loadDefinitions(vmType);
    vmMachine->printCode(&cout);

    n = m1DeleteZeroObjects(m1_system().executor());
    fprintf(stderr, "Cleaned up %d objects\n", n);    
    return 0;
}

void VmMain::execute(CExecutor* aExec)
{
}

int main(int argc, char** argv)
{
    char* ptr;
    char* lib_path[100];
    string file_name[100];
    int il = 0;
    int c;
    int i;
    int res = 0;

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

    m1_debug_mask = M1DBG_WARN;  // always warn!!!

    while((c = getopt(argc, argv, "L:D:d")) != -1) {
	switch (c) {
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
		    case 'L': m1_debug_mask |= M1DBG_LINT; break;
		    case 'C': m1_debug_mask |= M1DBG_COMP; break;
		    case 'X': m1_debug_mask |= M1DBG_EXEC; break;
		    case 'E': m1_debug_mask |= M1DBG_EVAL; break;
		    case 'P': m1_debug_mask |= M1DBG_PRNT; break;
		    case 'T': m1_debug_mask |= M1DBG_TYPE; break;
		    default: break;
		    }
		}
	    }
	    break;
	case 'L':
	    lib_path[il] = optarg;
	    il++;
	    break;
	case '?':
	default:
	    usage();
	}
    }
    argc -= optind;
    argv += optind;

    lib_path[il] = NULL;

    m1_init(lib_path);
    epx_set_debug(0);
    epx_init(EPX_SIMD_AUTO);

    // A dummy machine type for initialization of instructions
    new VmMachineType(T_TYPE);  

    if (argc == 0) {
	CBioStream* bio = new CBioStream();
	bio->open_fp_read("*stdin*", stdin,M1_KEY_USAGE_M1,false,false);
	file_name[0] =  string("*stdin*");
	res = compile(&file_name[0], bio);
    }
    else {
	for (i = 0; (i< argc) && (res==0); i++) {
	    CBioStream* bio = new CBioStream();
	    int r;

	    file_name[i] = string(argv[i]);

	    r = bio->open_file_read(file_name[i],M1_KEY_USAGE_M1,false,false);
	    if (r < 0) {
		fprintf(stderr, "m1c: unable to open file %s (%s)\n",
			file_name[i].c_str(), strerror(errno));
		bio->close();
		continue;
	    }
	    // Note!  bio will be cleanup and closed by compile
	    res = compile(&file_name[i], bio);
	}
    }
#ifdef DEBUG
    m1_dump_stat(&cerr);
#endif    
    if (res < 0)
	exit(-res);
    exit(0);
}

