//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

//
// M1 VM 
//
//
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <memory.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <dirent.h>
#include <errno.h>
#include <dlfcn.h>
#include <float.h>

#include <algorithm>
#include <unistd.h>
#include <sys/reboot.h>

#include "m1vm_opcodes.hh"
#include "m1vm_format.hh"
#include "m1vm.hh"
#include "key_store.hh"
#include "font_cache.hh"
#include "style.hh"

#ifdef DEBUG
// #define DEBUG_EVT
#endif

InstructionSpec vm_instruction[256];

extern string formatStorageClass(int sclass);
extern int m1_mark_screens(Mark_t aMark);

/* statistics */
unsigned long m1_stat_live_objects  = 0;
unsigned long m1_stat_live_types    = 0;
unsigned long m1_stat_live_elements = 0;

// m1 global random state
static char m1_random_state[256];

UData nil = { 0 };

list<VmObject*> m1_applicationList;

//
// Create the system instance
//
CSystem &m1_system() 
{
    static CSystem* sys = new CSystem();
    return *sys;
}

//
// Create global type context, storage for libraries
// global types etc
//
CBindingObject& m1_context()
{
    static CBindingObject* ctx = NULL;
    
    if (!ctx) {
	CBindingType* bt = m1New(CBindingType);
	ctx = m1New(CBindingObject, m1_system().executor(), bt, 0);
	ctx->retainThis();  // Make contexts permanent
	m1TypeRegister("_Contexts", bt);
    }
    return *ctx;
}

//
// Create the main (library) context, keeps globals defined in VmMain
//
VmMain& m1_main(void)
{
    static VmMain* vm_main = NULL;

    if (!vm_main) {
	vm_main = m1New(VmMain, m1_system().executor());

	// Add M1 object and type to the contexts
	m1_context().addField(Q_PUBLIC, "M1", vm_main->type());
	m1_context().put(m1_system().executor(), "M1", UObject(vm_main));
    }
    return *vm_main;
}

int m1_mark_applications(Mark_t aMark)
{
    list<VmObject*>::iterator ap = m1_applicationList.begin();
    int marked = 0;

    while(ap != m1_applicationList.end()) {
	marked += (*ap)->mark(aMark);
	ap++;
    }    
    return marked;
}


//
// Mark all objects accessible, return number of objects marked
//
int m1_mark(Mark_t aMark)
{
    int marked = 0;

    // mark fonts 
    marked += m1_fonts().mark(aMark);

    // mark keys
    marked += m1_keys().mark(aMark);

    // mark styles
    marked += m1_styles().mark(aMark);
    marked += m1_default_style()->mark(aMark);

    // libraries in m1_contex()
    marked += m1_context().mark(aMark);

    // applications

    marked += m1_mark_applications(aMark);

    // objects in screenList
    marked += m1_mark_screens(aMark);

    // objects in Queue & nextQueue
    marked += m1_system().mark(aMark);

    return marked;
}


//
// Register a "global" type
//
void m1TypeRegister(int aStorage, string aName, CType* aType)
{
    // fprintf(stderr, "m1TypeRegister: %s\n", aName.c_str());
    // FIXME: register none global types with it's scope (if needed!!!!)
    ((CBaseType*)m1_context().type())->addSubType(aStorage, aName, aType);
}

void m1TypeRegister(string aName, CType* aType)
{
    m1TypeRegister(Q_PUBLIC, aName, aType);
}

void m1TypeRegister(CType* aType)
{
    m1TypeRegister(Q_PUBLIC, aType->name(), aType);
}

//
// Unregister type (by nuking it)
//
void m1TypeUnRegister(CType* aType)
{
    ((CBaseType*)m1_context().type())->updateSubType(Q_NONE,aType->name(),NULL);
}

//
// Lookup a "global" type
//
CType* m1TypeLookup(string aName)
{
    CField* fld;

    fld = ((CBaseType*)m1_context().type())->subTypes()->field(aName);
    if (fld) 
	return fld->type();
    return NULL;
}

CType* m1TypeFmtLookup(char* fmt, ...)
{
    va_list ap;
    char typeName[512];

    va_start(ap, fmt);
    vsnprintf(typeName, 512, fmt, ap);
    va_end(ap);
    return m1TypeLookup(typeName);
}

//
//  Go through all types recursivly
//
void eachType(CBaseType* aBaseType, string indent, void (*aFunc)(CType*, ostream*), ostream* os)
{
    CBaseType* types;
    int i;

    if ((aBaseType == NULL) || ((types = aBaseType->subTypes()) == NULL))
	return;
    for (i=0; i < (int) types->fieldCount(); i++) {
	CType* t = types->typeAt(i);
	if (isAType(CBaseType*, t)) {
	    if (t->name() != "") {
		*os << indent;
		aFunc(t, os);
		eachType((CBaseType*) t, indent+" ", aFunc, os);
	    }
	}
    }
}
    
void eachType(void (*aFunc)(CType*, ostream*), ostream* os)
{
    eachType((CBaseType*)m1_context().type(), "", aFunc, os);
}

void dumpType(CType* t, ostream* os)
{
    *os << "SCOPE: " << t->scopeName() << " TYPE:" << t->name() << "  #OBJS:" 
	<< t->mStatObjects << "\n";
}

void m1_dump_type_stat(ostream* os)
{
    *os << "--- Type Info----------------------\n";
    eachType(dumpType, os);
    *os << "-----------------------------------\n";
}

void m1VmPrintStackElem(ostream* os, int c, UData elem)
{
    switch(c) {
    case 's':
	if (elem.o == NULL)
	    *os << "nil";
	else
	    string_type()->print(os, elem);
	break;
    case 'o':
	if (elem.o == NULL)
	    *os << "nil";
	else {
	    *os << elem.o->type()->name() << "#";
	    unsigned_type()->print(os, elem);
	    // elem.o->type()->print(os, elem); 
	}
	break;
    case 'e': unsigned_type()->print(os, elem); break;
    case 'u': unsigned_type()->print(os, elem); break;
    case 'i': signed_type()->print(os, elem); break;
    case 'f': float_type()->print(os, elem); break;
    case 'b': byte_type()->print(os, elem); break;
    case 'c': char_type()->print(os, elem); break;
    case 't': bool_type()->print(os, elem); break;
    }
}

void m1VmPrintStack(ostream* os, int N, char* spec, UData* stack)
{
    int n = strlen(spec);
    int i = -n;
    int j = 0;

    while(j < n) {
	if (spec[j] == 'n') {
	    if (spec[j+1] == '*') {
		int k;

		i = (i + 3) - N;
		for (k = 0; k < N; k++) {
		    m1VmPrintStackElem(os, spec[j+2], stack[i]); 
		    *os << " ";
		    i++;
		}
	    }
	    j += 3;
	}
	else {
	    m1VmPrintStackElem(os, spec[j], stack[i]);
	    *os << " ";
	    j++;
	    i++;
	}
    }
}

void m1VmPrintArg(ostream* os, int c, int &N,
		  Instruction* base, 
		  Instruction* table,
		  Instruction* ptr)
{
    switch(c) {
    case 'u': 
	*os << " " << (unsigned int)*ptr;
	break;
    case 'i':
	*os << " " << (int)*ptr;
	break;
    case 'f':
	*os << " " << *((float*)ptr);
	break;
    case 's':
	*os << " ";
	string_type()->print(os, UString(((CString*)*ptr)));
	break;
    case 'F':
	*os << " '" << ((UBuiltin*)*ptr)->name << "'";
	break;
    case 'T':
	*os << " '" << ((CType*)*ptr)->cname() << "'";
	break;
    case 'L':
	if (base) {
	    if ((int)*ptr == 0)
		*os << " 0";
	    else
		*os << " L" << ((table + (int)*ptr) - base);
	}
	else
	    *os << " " << (int)*ptr;
	break;
    case 'n':
	N = (int)*ptr;
	*os << " n=" << N;
	break;
    default:
	*os << char(c) << "???";
	break;
    }
}


int m1VmPrintArgs(ostream* os, int n, int& N,
		  char* spec, Instruction* base, Instruction* ptr)
{
    int c;
    Instruction* ptr0 = ptr;

    while((c = *spec++)) {
	if (c != '*') {
	    ptr++;
	    m1VmPrintArg(os, c, N, base, ptr, ptr);
	}
	else {
	    int i;
	    c = *spec++;
	    Instruction* table = ptr;

	    *os << " [";
	    if (c == 'L')
		table += (N+1);
	    for (i = 0; i < N; i++) {
		ptr++;
		m1VmPrintArg(os, c, N, base, table, ptr);
	    }
	    *os << " ]";
	}
    }
    return ptr - ptr0;
}

void m1InstructionBegin(ostream* os, int op, Instruction* ptr, UData* s)
{
    if (M1DBG_IS_SET(M1DBG_EXEC|M1DBG_PRNT)) {
	int N;
	int n = vm_instruction[op].args;
	*os << vm_instruction[op].name << " ";
	m1VmPrintArgs(os, n, N, vm_instruction[op].arg_spec, 
		      NULL, ptr-1);
	*os << " : ";
	m1VmPrintStack(os, N, vm_instruction[op].pre_spec, s);
    }
}

void m1InstructionEnd(ostream* os, int op, UData* s)
{
    if (M1DBG_IS_SET(M1DBG_EXEC|M1DBG_PRNT)) {
	*os << " --- ";
	m1VmPrintStack(os, 0, vm_instruction[op].post_spec, s);
	*os << "\n";
    }
}

int vmOpcodeFromInstruction(Instruction instr)
{
    int i;

    if (instr == NULL)
	return -1;
    for (i = 0; i < 256; i++) {
	if (vm_instruction[i].instr == instr)
	    return i;
    }
    return -1;
}

int m1VmDisAssemble(ostream* os, string indent, Instruction* base, Instruction* ptr)
{
    int op;
    
    if ((op = vmOpcodeFromInstruction(*ptr)) >= 0) {
	char buf[8];
	int  args;
	int  i = 0;
	int  N;
	sprintf(buf, "%04d", ptr - base);
	*os << indent << buf << ": " << vm_instruction[op].name;
	if ((args = vm_instruction[op].args) > 0)
	    i=m1VmPrintArgs(os, args, N,vm_instruction[op].arg_spec, base, ptr);
	*os << "\n";
	return 1+i;
    }
    return 0;
}

//
// Bytecode loader
//
/* Load a "pascal" string  <n:8> <c1> ... <cn> */
u_int8_t* load_string(u_int8_t* ptr, u_int8_t* ptr_end, string* result)
{
    size_t len;

    if (ptr+1 > ptr_end) goto error;
    len = *ptr++;
    if (ptr+len > ptr_end) goto error;
    *result = string((char*)ptr, len);
    return ptr+len;
error:
    *result = string();
    return NULL;
}

#define tagstr(tag) ((char*) &(tag))

static inline VmTag tagload(u_int8_t* ptr, u_int8_t* ptr_end)
{
    VmTag tag = 0;

    if (ptr + sizeof(VmTag) <= ptr_end)
	tag = TAG_MAKE(ptr[0],ptr[1],ptr[2],ptr[3]);
    else 
	tag = TAG_MAKE('?', '?', '?', '?');
    return tag;
}

/* Update a type name with array indication
 * float => float[n]
 * float[2]  => float[n][2]
 *  n == 0 => []
 */
static string addDimension(string aTypeName, size_t n)
{
    int pos;
    string rstr;
    string ostr;
    ostringstream oStream;

    // fprintf(stderr, "addDimension [%d] to %s = ", n, aTypeName.c_str());

    if (n == 0)
	oStream << "[]";
    else
	oStream << "[" << n << "]";
    ostr = oStream.str();

    if ((pos = aTypeName.find('[')) >= 0) {
	rstr = aTypeName;
	rstr.insert(pos, ostr);
    }
    else
	rstr = aTypeName + ostr;
    return rstr;
}

#define VM_GET_U32(ptr) \
    ((u_int32_t) ((((u_int8_t*)ptr)[0]<<24) | (((u_int8_t*)ptr)[1]<<16) | \
		  (((u_int8_t*)ptr)[2]<<8) | (((u_int8_t*)ptr)[3])))



int m1Load(u_int8_t* ptr, size_t len)
{
    u_int8_t* ptr_end = ptr + len;
    VmTag tag;
    VmMachineType* type = NULL;

    if ((ptr+sizeof(VmTag) >= ptr_end) ||
	((tag=tagload(ptr, ptr_end)) != tag_MAGIC)) {
	ERRFMT("VmLoader: bad magic %.4s", tagstr(tag));
	return -1;
    }

    ptr += sizeof(VmTag);

    while(ptr && (ptr+sizeof(VmTag)) < ptr_end) {
	tag = tagload(ptr, ptr_end);
	ptr += sizeof(VmTag);
	switch(tag) {
	case tag_TYPE: {
	    if (ptr+sizeof(VmTag) >= ptr_end) goto err_too_short;
	    tag = tagload(ptr, ptr_end);
	    ptr += sizeof(VmTag);	    
	    switch(tag) {
	    case tag_TYPE_DECL: {
		string eName;
		string aName;

		if (ptr+sizeof(VmTag) > ptr_end) goto err_too_short;
		tag = tagload(ptr, ptr_end); ptr += sizeof(VmTag);

		if ((ptr=load_string(ptr,ptr_end,&eName))==NULL)
		    goto err_too_short;

		switch(tag) {
		case tag_TYPE_ARRAY: { // <size>
		    size_t n;
		    if (ptr+sizeof(unsigned int) > ptr_end) goto err_too_short;
		    n = VM_GET_U32(ptr);
		    ptr += sizeof(unsigned int);
		    aName = addDimension(eName, n);
		    if (m1TypeLookup(aName) == NULL)
			m1New(CArrayType, eName, n);
		    DBGFMT("VmLoader: Declared ARRAY=%s", aName.c_str());
		    break;
		}

		case tag_TYPE_EVT: {
		    aName = "event "+ eName;
		    if (m1TypeLookup(aName) == NULL)
			m1New(CEventType, eName, false);
		    DBGFMT("VmLoader: Type Declared EVENT=%s", aName.c_str());
		    break;
		}

		case tag_TYPE_OBJECT:  {
		    if (m1TypeLookup(eName) == NULL)
			m1New(VmMachineType, eName, T_TYPE);
		    DBGFMT("VmLoader: Type Declared OBJECT=%s", eName.c_str());
		    break;
		}

		default:
		    ERRFMT("VmLoader: unknown tag %.4s", tagstr(tag));
		    return -1;
		}
		break;
	    }

	    case tag_TYPE_OBJECT: {
		string tName;

		// TYPE NAME
		if ((ptr=load_string(ptr,ptr_end,&tName))==NULL)
		    goto err_too_short;
		type = m1New(VmMachineType, tName, T_TYPE);
		if ((ptr = type->loadFields(ptr, ptr_end)) == NULL) {
		    ERRFMT("VmLoader: bad object declaration section %.4s", 
			   tagstr(tag));
		    return -1;
		}
		DBGFMT("VmLoader: Object Defined %s", tName.c_str());
		break;
	    }

	    default:
		ERRFMT("VmLoader: unknown tag %.4s", tagstr(tag));
		return -1;
	    }
	    break;
	}

	case tag_CODE:
	    if (type == NULL) {
		ERRFMT("VmLoader: bad script defintion without objec declaration %d", (int) len);
		return -1;
	    }
	    if (ptr+sizeof(VmTag) >= ptr_end) goto err_too_short;
	    tag = tagload(ptr, ptr_end);
	    ptr += sizeof(VmTag);
	    switch(tag) {
	    case tag_PRESCRIPT:
		ptr = type->loadPreScript(ptr, ptr_end);
		break;
	    case tag_POSTSCRIPT:
		ptr = type->loadPostScript(ptr, ptr_end);
		break;
	    case tag_CONSTRUCTOR:
		ptr = type->loadConstructor(ptr, ptr_end);
		break;
	    case tag_DESTRUCTOR:
		ptr = type->loadDestructor(ptr, ptr_end);
		break;
	    case tag_DEFAULTS:
		ptr = type->loadDefaults(ptr, ptr_end);
		break;
	    default:
		ERRFMT("VmLoader: unknown tag %.4s", tagstr(tag));
		return -1;
	    }
	    break;
	default:
	    ERRFMT("VmLoader: unknown tag %.4s", tagstr(tag));
	    return -1;
	}
    }
    if (ptr == NULL)
	goto err_too_short;
    if (ptr != ptr_end) 
	goto err_too_long;

    return 0;

err_too_long:
    ERRFMT("VmLoader: trailing bytes: %d", (int) len);
    return -1;
err_too_short:
    ERRFMT("VmLoader: truncated: %d", (int) len);
    return -1;
}




#ifdef DEBUG
#define INSTRUCTION_BEGIN(i) m1InstructionBegin(&cerr, op_##i, I, S)
#define INSTRUCTION_END(i)   m1InstructionEnd(&cerr, op_##i, S)
#define INSTRUCTION_NL do { \
	if (M1DBG_IS_SET(M1DBG_EXEC|M1DBG_PRNT)) { cerr << "\n"; } \
    } while(0)
#else
#define INSTRUCTION_BEGIN(op)
#define INSTRUCTION_END(op)
#define INSTRUCTION_NL
#endif

#define STRING(x) #x

#define INIT_INSTRUCTION(i,iarg,ia,iconsume,iproduce,ipre,ipost) do {	\
	vm_instruction[op_##i].name = (char*) #i;			\
	vm_instruction[op_##i].instr = &&i;			\
	vm_instruction[op_##i].consume = iconsume;		\
	vm_instruction[op_##i].produce = iproduce;		\
	vm_instruction[op_##i].args    = iarg;			\
	vm_instruction[op_##i].pre_spec = (char*) ipre;		\
	vm_instruction[op_##i].post_spec = (char*) ipost;	\
	vm_instruction[op_##i].arg_spec = (char*) ia;		\
    } while(0)

//
// EXEC_UPDATE:  object index value -- value'
//   calculation: object[index]  op= value
//
#define EXEC_UPDATE(op, sel, rsel) do {		\
	T = S[-3].o;					\
	S[-3].sel = T->at(S[-2].i).sel op S[-1].sel;	\
	T->put(aExec, S[-2].i, S[-3]);				\
	S[-3].rsel = S[-3].sel;				\
	S -= 2;						\
    } while(0)

//
// IEXEC_UPDATE:  object index signed -- value'
//   calculation: object[index]  op= signed
//
#define IEXEC_UPDATE(op, sel, rsel) do {		\
	T = S[-3].o;		  \
	S[-3].sel = T->at(S[-2].i).sel op S[-1].i;	\
	T->put(aExec, S[-2].i, S[-3]);				\
	S[-3].rsel = S[-3].sel;				\
	S -= 2;						\
    } while(0)

//
// IEXEC_ADD:  object index -- value'
//   calculation: object[index]  += param
//

#define EXEC_ADD(param, sel, rsel) do {		\
	T = S[-2].o;					\
	S[-2].sel = T->at(S[-1].i).sel + (param);	\
	T->put(aExec, S[-1].i, S[-2]);				\
	S[-2].rsel = S[-2].sel;				\
	S -= 1;						\
    } while(0)


// S[-2] = S[-2] op= S[-1]
#define EXEC_BINARY(op, sel) do {		\
	S[-2].sel op##= S[-1].sel;		\
	S--;					\
    } while(0)

#define IEXEC_BINARY(op, sel) do {		\
	S[-2].sel op##= S[-1].i;		\
	S--;					\
    } while(0)

#define REXEC_BINARY(op, sel) do {		\
	S[-2].sel = S[-2].sel op S[-1].sel;		\
	S--;						\
    } while(0)

#define EXEC_UNARY(op, sel) do {		\
	S[-1].sel = op S[-1].sel;		\
    } while(0)




// Return absolute time in micro seconds
TimeStamp m1_timeStamp(void)
{
    struct timeval tm;

    gettimeofday(&tm, 0);
    return ((TimeStamp) tm.tv_sec)*1000000L + (TimeStamp)(tm.tv_usec);
}

void m1_init_random(TimeStamp tm)
{
    int fd;
    unsigned long seed;
    struct stat tmp_buf;

    if (stat("/dev/hw_random", &tmp_buf) != -1)
	fd = open("/dev/hw_random", O_RDONLY);
    else 
	fd = open("/dev/random", O_RDONLY);

    if (fd < 0) {
	fprintf(stderr, "could not open /dev/hw_random or /dev/random %s\n", 
		strerror(errno));
	seed = (tm >> 32) ^ (tm & 0xffffffff);
    }
    else {
	read(fd, &seed, sizeof(seed));
	close(fd);
	seed ^= ((tm >> 32) ^ (tm & 0xffffffff));
    }
    initstate(seed, m1_random_state, sizeof(m1_random_state));
}

// Move to CSystem!
void m1_init(char **shared_object_directories)
{
    int loaded_so_counter = 0;

    // Ensure system structure is initialize
    m1_system();  
    m1_init_random(m1_system().timeStart());

    // Ensure sysem type/library are is initialized
    m1_context();

    // Create m1 main object
    m1_main();

    // Preload all primitive types
    error_type();

    void_type();
    any_type();
    numeric_type();

    byte_type();
    char_type();
    bool_type();
    signed_type();
    unsigned_type();
    float_type();
    string_type();
    time_type();

    event_byte_type();
    event_char_type();
    event_bool_type();
    event_signed_type();
    event_unsigned_type();
    event_float_type();
    event_string_type();
    event_time_type();

    input_byte_type();
    input_char_type();
    input_bool_type();
    input_signed_type();
    input_unsigned_type();
    input_float_type();
    input_string_type();
    input_time_type();

    output_byte_type();
    output_char_type();
    output_bool_type();
    output_signed_type();
    output_unsigned_type();
    output_float_type();
    output_string_type();
    output_time_type();

    // Make sure device key is loaded
    // FIXME: make sure we can verify shared objects before loading!!!
    // This includes loading keys before loading shared objects
    m1_keys();

    /* 
     * Load all shared object located in the null terminated 
     * shared_object_directories array.
     */
    while(shared_object_directories && 
	  *shared_object_directories) {
	void *lib;
	DIR *dir_desc = opendir(*shared_object_directories);
	struct dirent dir, *res;

	if (!dir_desc) {
	    printf("Could not open plugin directory [%s] for scanning: [%s]\n",
		   *shared_object_directories, strerror(errno));
	    ++shared_object_directories;
	    continue;
	}

	DBGFMT("Scanning plugin directory [%s]", *shared_object_directories);
	while(1) {
	    char fname[1024];

	    readdir_r(dir_desc, &dir, &res);
	    if (!res)
		break;
	    //
	    // Check that the filename ends in .so
	    //
	    if (strlen(dir.d_name) < 3 || 
		strcmp(dir.d_name + strlen(dir.d_name) - 3, ".so")) {
		DBGFMT("  Skipping [%s]", dir.d_name);
		continue;
	    }

	    // We need a slash in the filename to disable .so search mechanism.
	    sprintf(fname, "%s/%s", *shared_object_directories, dir.d_name);

	    //
	    // Open file using dlopen.
	    //
	    DBGFMT("Loading plugin [%s]", fname);
	    lib = dlopen(fname, RTLD_NOW | RTLD_GLOBAL);
	    if (!lib) {
		printf("Could not load plugin [%s]: %s\n",  fname, dlerror());
		continue;
	    }
	    ++loaded_so_counter;
	}
	closedir(dir_desc);
	++shared_object_directories;
    }	
    DBGFMT("Loaded [%d] shared objects.", loaded_so_counter);
}

void m1_dump_stat(ostream* os)
{
    *os << "#LIVE:\n";
    *os << "     total: " << m1_stat_live << "\n";
    *os << "   objects: " << m1_stat_live_objects << "\n";
    *os << "     types: " << m1_stat_live_types << "\n";
    *os << "  elements:" << m1_stat_live_elements << "\n";
    *os << "#CREATED: " << m1_stat_created << "\n";
    *os << "#DELETED: " << m1_stat_deleted << "\n";
    *os << " #RETAIN: " << m1_stat_retain << "\n";
    *os << "#RELEASE: " << m1_stat_release << "\n";
}

//
// Builtin functions
//



// int size( *[] )
static UData builtin_size(CExecutor* aExec, UData* args)
{
    UData r;

    if (args[0].arr == NULL)
	r.u = 0;
    else
	r.u = args[0].arr->size();
    return r;
}

// int strsize( string )
static UData builtin_strsize(CExecutor* aExec, UData* args)
{
    UData r;

    if (args[0].str)
	r.u = args[0].str->size();
    else
	r.u = 0;
    return r;
}

// Rotate array elements
// void rotate(*[], int)
static UData builtin_rotate(CExecutor* aExec, UData* args)
{
    if (args[0].arr != NULL)
	args[0].arr->rotate(args[1].i);
    return nil;
}

// void reverse(*[])
static UData builtin_reverse(CExecutor* aExec, UData* args)
{
    if (args[0].arr != NULL)
	args[0].arr->reverse();
    return nil;
}

// void swap(*[], int, int)
static UData builtin_swap(CExecutor* aExec, UData* args)
{
    if (args[0].arr != NULL)
	args[0].arr->swap(args[1].i, args[2].i);
    return nil;
}


// FIXME: use STL agorithms properly!!!!
static CType* sort_compare_type;
static bool sort_compare(UData a, UData b) 
{
    return sort_compare_type->compare(a, b);
}

static UData builtin_sort(CExecutor* aExec, UData* args)
{
    if (args[0].arr != NULL) {
	UDataVector* vec = args[0].arr->base();
	CArrayType*  t   = (CArrayType*) (args[0].arr->type());
	sort_compare_type = t->elementType();
	sort(vec->begin(), vec->end(), sort_compare);
    }
    return nil;
}

// How do you make this thread safe ? can sort accept extra args???
static int    isort_compare_index;

static bool isort_compare(UData a, UData b)
{
    CBaseObject* ao = (CBaseObject*) a.o;
    CBaseObject* bo = (CBaseObject*) b.o;
    CType* t;
    int i = isort_compare_index;

    if ((i < 0) || (i >= (int) ao->size()) ||(i >= (int) bo->size())) {
	m1BreakHere(__FILE__, __LINE__, (char*) "field index exception");
	throw M1StatusError;
    }
    if (((t = ao->typeAt(i)) != bo->typeAt(i)))
	return -1;
    return (t->compare(ao->at(i), bo->at(i)) < 0);
}

static UData builtin_sort_index(CExecutor* aExec, UData* args)
{
    if (args[0].arr != NULL) {
	UDataVector* vec = args[0].arr->base();
	CArrayType*  t   = (CArrayType*) (args[0].arr->type());
	CType*       etype = t->elementType();
	int          index = args[1].i;

	switch(etype->typeTag()) {
	case M1TYPE_ARRAY:
	case M1TYPE_OBJECT:
	    isort_compare_index= index;
	    sort(vec->begin(), vec->end(), isort_compare);
	    break;
	default:
	    break;
	}
    }
    return nil;
}

// Like popen but has two ends so it actually works like I want!!!
// inf   is the input where caller may read data
// outf  is the output where caller may write data
//
static pid_t open_pipe(const char* command, FILE** infp, FILE** outfp)
{
    int p_stdin[2];
    int p_stdout[2];
    pid_t pid;
    char *shell = getenv("M1_SHELL");
    
    if (!shell)
	shell = (char*) "/bin/sh";

    p_stdin[0]  = p_stdin[1]  = -1;
    p_stdout[0] = p_stdout[1] = -1;

    if ((pipe(p_stdin) != 0) || (pipe(p_stdout) != 0))
	goto error;
    pid = fork();

    if (pid < 0)
	goto error;
    else if (pid == 0) {
	close(p_stdin[1]);
	if (dup2(p_stdin[0], 0) == -1)
	    perror("dup2-1");
	close(p_stdout[0]);

	if (dup2(p_stdout[1], 1) == -1)
	    perror("dup2-2");

	close(p_stdout[0]);
	close(p_stdin[1]);
	execl(shell, shell, "-c", command, NULL);
	perror("execl");
	exit(1);
    }

    // fprintf(stderr, "p_stdin[0]=%d, p_stdin[1]=%d\n", p_stdin[0], p_stdin[1]);
    // fprintf(stderr, "p_stdout[0]=%d, p_stdout[1]=%d\n",p_stdout[0], p_stdout[1]);
    if (outfp == NULL)
	close(p_stdin[1]);
    else 
	if (!(*outfp = fdopen(p_stdin[1], "w"))) 
	    perror("fdopen stdin");


    if (infp == NULL) 
	close(p_stdout[0]);
    else
	if (!(*infp = fdopen(p_stdout[0], "r")))
	   perror("fdopen stdout");

    // fprintf(stderr, "open_pipe: inf=%p, outf=%p\n", *infp, *outfp);
   close(p_stdin[0]);
   close(p_stdout[1]);
   return pid;

error:
   if (p_stdin[0] != -1) close(p_stdin[0]);
   if (p_stdin[1] != -1) close(p_stdin[1]);
   if (p_stdout[0] != -1) close(p_stdout[0]);
   if (p_stdout[1] != -1) close(p_stdout[1]);
   return -1;
}

/*
 * Helper function for w/r/rw columns
 */
static int open_data(string sfile, FILE** infp, FILE** outfp)
{
    FILE* f;
    const char* name = sfile.c_str();

    while(isblank(*name))
	name++;
    if (*name == '|')
	return open_pipe(name+1, infp, outfp);
    else if ((infp != NULL) && (outfp != NULL)) {
	if ((f = fopen(name, "r+")) == NULL)
	    return -1;
	*infp = f;
	*outfp = f;
	return 0;
    }
    else if (infp != NULL) {
	if ((f = fopen(name, "r")) == NULL)
	    return -1;	
	*infp = f;
	return 0;
    }
    else if (outfp != NULL) {
	if ((f = fopen(name, "w")) == NULL)
	    return -1;
	*outfp = f;
	return 0;
    }
    return -1;
}

static void close_data(FILE* inf, FILE* outf)
{
    if (inf)
	fclose(inf);
    if ((inf != outf) && outf)
	fclose(outf);
}


static int read_row(CExecutor* aExec,FILE* f, CArray* matrix,
		    string separator, int i)
{
    char  line[1024];
    bool  skip_blank = true;
    int   columns = matrix->size();
    int   j = 0;
    char* ptr = line;
    float row[columns];

    if ((separator.find(' ',0) != string::npos) ||
	(separator.find('\t',0) != string::npos))
	skip_blank = false;
    
    if (fgets(line,sizeof(line),f) == NULL)
	return 0;
    while(*ptr && (*ptr != '\n') && (j < columns)) {
	while(isblank(*ptr)) ptr++;  // always skip blank before
	if (separator.find(*ptr,0) != string::npos) {
	    row[j] = 0.0;
	}
	else {
	    char* endptr = NULL;
	    row[j] = strtod(ptr, &endptr);
	    if (endptr == ptr) return -1;
	    ptr = endptr;
	    if (skip_blank) {
		while(isblank(*ptr))
		    ptr++;
	    }
	    if (*ptr != '\n') {
		if (*ptr && (separator.find(*ptr,0) == string::npos))
		    return -1;
		ptr++;
	    }
	}
	j++;
    }
    while(j < columns)
	row[j++] = 0.0;

    for (j = 0; j < columns; j++) {
	CArray* column = (matrix->at(j)).arr;
	if (column != NULL) {
	    UData value;
	    value.f = row[j];
	    if (i >= (int)column->size())
		column->resize(i+1); // check non dynamic arrays!!!
	    column->put(aExec, i, value);
	}
    }
    return 1;
}

static int write_row(FILE* f, CArray* matrix, string separator, int i)
{
    int   columns = matrix->size();
    float row[columns];
    int j;

    for (j = 0; j < columns; j++) {
	UData v = matrix->at(j);  // Get the matrix
	CArray* column = v.arr;
	if (column == NULL)
	    row[j] = 0.0;
	else if (i < (int)column->size())
	    row[j] = (column->at(i)).f;
	else
	    return 0;
    }
    fprintf(f, "%g", row[0]);
    for (j = 1; j < columns; j++)
	fprintf(f, "%s%g", separator.c_str(), row[j]);
    fprintf(f, "\n");
    return 1;
}


/*
 *  int wcolumns("/tmp/foo", ";", matrix)
 *
 *  for each column (matrix[i]) in matrix write
 *  a row to the file (or pipe) where the values are
 *  separated with ; and each line is separated with \n
 *  return -1 on error 
 *  return number of rows written.
 *
 *  int wcolumns(string file, string separator, float[][])
 */
static UData builtin_wcolumns(CExecutor* aExec, UData* args)
{
    UData r;
    CArray* matrix;
    string  separator;
    int i = 0;
    FILE* fout;
    int res;

    if (open_data(args[0].str->str(),NULL,&fout) < 0)
	goto error;
    separator = args[1].str->str();
    if ((matrix = args[2].arr) == NULL)
	goto error;
    if (matrix->size() == 0)
	goto done;

    while((res = write_row(fout, matrix, separator, i)) > 0)
	i++;
    if (res < 0)
	goto error;
done:
    close_data(NULL, fout);
    r.i = i;
    return r;    
error:
    close_data(NULL, fout);
    r.i = -1;
    return r;
}


/*
 *  int rcolumns("/tmp/foo", ";", matrix)
 *
 *  for each row in file /tmp/foo update the
 *  values (separated with ;) in each columns of matrix.
 *  return -1 on error 
 *  return number of rows read
 *
 *  int rcolumns(string file, string separator, float[][])
 */
static UData builtin_rcolumns(CExecutor* aExec, UData* args)
{
    UData r;
    CArray* matrix;
    string  separator;
    int i = 0;
    FILE* fin;
    int res;

    if (open_data(args[0].str->str(),&fin,NULL) < 0)
	goto error;
    separator = args[1].str->str();
    if ((matrix = args[2].arr) == NULL)
	goto error;
    if (matrix->size() == 0)
	goto done;

    while((res = read_row(aExec, fin, matrix, separator, i)) > 0)
	i++;
    if (res < 0)
	goto error;
done:
    close_data(fin, NULL);
    r.i = i;
    return r;    
error:
    close_data(fin, NULL);
    r.i = -1;
    return r;
}

/*
 *  int rwcolumns("| echo", ";", wmatrix, ";", rmatrix)
 *
 *  for each column (wmatrix[i]) in matrix write
 *  a row to the file (or pipe) where the values are
 *  separated with ; and each line is separated with \n
 *  return -1 on error 
 *  then try to read line by line into rmatrix
 *  return number of rows read.
 *
 *  int rwcolumns(string file, string wseparator, float w[][],
 *                             string rseparator, float r[][])
 *
 */
static UData builtin_rwcolumns(CExecutor* aExec, UData* args)
{
    UData r;
    CArray* w_matrix;
    string  w_separator;
    CArray* r_matrix;
    string  r_separator;
    int     w_i = 0;
    int     r_i = 0;
    FILE*   fin;
    FILE*   fout;
    int     r_fd;
    struct  pollfd r_pfd;
    int     w_res;

    if (open_data(args[0].str->str(),&fin,&fout) < 0)
	goto error;
    w_separator = args[1].str->str();
    r_separator = args[3].str->str();
    if ((w_matrix = args[2].arr) == NULL)
	goto error;
    if ((r_matrix = args[4].arr) == NULL)
	goto error;
    if (w_matrix->size() == 0)
	goto done;
    if (r_matrix->size() == 0)
	goto done;
    
    w_res = 1;
    r_fd  = fileno(fin);
    r_pfd.fd = r_fd;
    r_pfd.events = POLLIN;

    while(w_res > 0) {
	int r_res;
	if ((w_res = write_row(fout, w_matrix, w_separator, w_i)) > 0)
	    w_i++;
	fflush(fout);
	//	fprintf(stderr, "wrote row %d\n", w_i);
	// Read data to ensure that we do not dead-lock
	while (m1Poll(&r_pfd, 1, 0) == 1) {
	    if ((r_res = read_row(aExec, fin, r_matrix, r_separator, r_i)) > 0)
		r_i++;
	    else
		goto done;
	    //	    fprintf(stderr, "1:read row %d\n", r_i);
	}
    }
    // close the output side 
    fclose(fout);

    // wait for trailing data to arrive
    while (m1Poll(&r_pfd, 1, -1) == 1) {
	int r_res;
	if ((r_res = read_row(aExec, fin, r_matrix, r_separator, r_i)) > 0)
	    r_i++;
	else
	    goto done;
	//	fprintf(stderr, "2:read row %d\n", r_i);
    }

done:
    close_data(fin, NULL);
    r.i = r_i;
    return r;    
error:
    close_data(fin, fout);
    r.i = -1;
    return r;
}



// float sin(float)
static UData builtin_sin(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = sinf(args[0].f);
    return r;
}

// float cos(float)
static UData builtin_cos(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = cosf(args[0].f);
    return r;
}

// float pi(void)
static UData builtin_pi(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = M_PI;
    return r;
}

// float sqrt(float)
static UData builtin_sqrt(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = sqrtf(args[0].f);
    return r;
}

// NOTE: this is handy since m1 do not support the intermediate
// double values needed to calculate this with best posible precission.
// float dist(float a, float b)  == sqrt(a*a + b*b)
static UData builtin_dist(CExecutor* aExec, UData* args)
{
    UData r;
    double a = args[0].f;
    double b = args[1].f;

    r.f = (float) sqrt(a*a + b*b);
    return r;
}


// int min( int, int);
static UData builtin_imin(CExecutor* aExec, UData* args) {
    UData r;
    
    r.i = (args[0].i < args[1].i)?args[0].i:args[1].i;
    return r;
}

// float min( float, float);
static UData builtin_fmin(CExecutor* aExec, UData* args) {
    UData r;
    
    r.f = (args[0].f < args[1].f)?args[0].f:args[1].f;
    return r;
}

// int max( int, int);
static UData builtin_imax(CExecutor* aExec, UData* args) {
    UData r;
    
    r.i = (args[0].i < args[1].i)?args[0].i:args[1].i;
    return r;
}

// float max( float, float);
static UData builtin_fmax(CExecutor* aExec, UData* args) {
    UData r;
    
    r.f = (args[0].f < args[1].f)?args[0].f:args[1].f;
    return r;
}


// float fabs(float a)
static UData builtin_fabs(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = fabs(args[0].f);
    return r;
}

// int iabs(int a)
static UData builtin_iabs(CExecutor* aExec, UData* args)
{
    UData r;

    r.i = abs(args[0].i);
    return r;
}

// int fsign(float a)
static UData builtin_fsign(CExecutor* aExec, UData* args)
{
    UData r;

    r.i = int(nearbyintf((args[0].f < 0.0) ? -1.0 : ((args[0].f > 0.0) ? 1.0 : 0.0)));
    return r;
}

// int isign(int a)
static UData builtin_isign(CExecutor* aExec, UData* args)
{
    UData r;

    r.i = (args[0].i < 0) ? 1 : ((args[0].i > 0.0) ? 1 : 0);
    return r;
}

// float trunc(float a)
static UData builtin_trunc(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = truncf(args[0].f);
    return r;
}

// float round(float a)
static UData builtin_round(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = roundf(args[0].f);
    return r;
}

// float ceil(float a)
static UData builtin_ceilf(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = ceilf(args[0].f);
    return r;
}

// float floor(float a)
static UData builtin_floorf(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = floorf(args[0].f);
    return r;
}


// We could possibly improve the resolution if we want!
// for some applications we may need it.
static UData builtin_now(CExecutor* aExec, UData* args)
{
    UData r;

    r.f = (aExec->timeStamp())/double(STAMP_SEC);
    return r;
}

static UData builtin_inow(CExecutor* aExec, UData* args)
{
    UData r;

    r.u = m1_TimeStampToTime(aExec->timeStamp());
    return r;
}

static UData builtin_cycle(CExecutor* aExec, UData* args)
{
    UData r;

    r.u = aExec->cycle();
    return r;
}

// Return a random number in range (0..2^31 -1)
static UData builtin_random(CExecutor* aExec, UData* args)
{
    UData r;
    r.u = random();
    return r;
}

// Return a random number in range (0..1)
static UData builtin_randomf(CExecutor* aExec, UData* args)
{
    UData r;
    union { 
	u_int32_t u;
	float f;
    } fu;
    fu.u = (random() & 0x7FFFFF) | 0x3F800000;
    r.f = fu.f - 1;
    return r;
}

// DEBUG - TESTING read the refcount
// unsigned refcount(Object)
static UData builtin_refcount_obj(CExecutor* aExec, UData* args)
{
    UData r;

    if (args[0].o == NULL)
	r.u = 0;
    else
	r.u = args[0].o->refCount();
    return r;
}

// unsigned refcount(*[])
static UData builtin_refcount_arr(CExecutor* aExec, UData* args)
{
    UData r;

    if (args[0].arr == NULL)
	r.u = 0;
    else
	r.u = args[0].arr->refCount();
    return r;
}




#if 0 
// Not use but keep for reference!
// Maybe nice to have some day...
double rnd01_64(void)
{
    union { 
	u_int32_t u[2];
	double d;
    } du;
    long a = random();
    long b = random();
    
    // High is using 20 bits of A
    du.u[_QUAD_HIGHWORD] = (a & 0xFFFFF) | 0x3FF00000;
    // Low is using 10 bits of A and 22 bits of B
    du.u[_QUAD_LOWWORD] = (b << 10) | ((a >> 20) & 0x3ff);
    return du.d - 1;
}
#endif


//
// Basic string ops. 
//
static UData builtin_strlen(CExecutor* aExec, UData *args)
{
    UData r;

    if (args[0].str)
	r.u = args[0].str->size(); // size==strlen
    else
	r.u = 0;
    return r;
}


static UData builtin_strcat(CExecutor* aExec, UData *args)
{
    UData r;
    CString* new_str = NULL;

    if (args[0].str && args[1].str)
	new_str = m1New(CString, args[0].str->str() + args[1].str->str());
    else if (args[0].str)
	new_str = args[0].str;
    else if (args[1].str)
	new_str = args[1].str;
    r.str = new_str;
    return r;
}

// Arg0 = string.
// Arg1 = (unsigned) first char (0 == first)
// Arg2 = (unsigned) Nr of char (0 = remainder of string)
static UData builtin_substr_3(CExecutor* aExec, UData *args)
{
    UData r;
    CString *new_str = NULL;

    if (args[0].str) {
	string::size_type nr_char = string::npos;// Default to all chars.

	if (args[2].u != 0)
	    nr_char = args[2].u;
	new_str = m1New(CString, args[0].str->str().substr(args[1].u, nr_char));
    }
    r.str = new_str;
    return r;
}

// Arg0 = string.
// Arg1 = (unsigned) first char (0 == first)
static UData builtin_substr_2(CExecutor* aExec, UData *args)
{
    UData r;
    CString *new_str = NULL;

    if (args[0].str)
	new_str = m1New(CString, args[0].str->str().substr(args[1].u));
    r.str = new_str;
    return r;
}

// Locate index of character in string.
// args[0] == string.
// args[1] (char) = char.
// Returns signed. -1 == not found.
static UData builtin_strchr(CExecutor* aExec, UData *args)
{
    UData r;

    if (args[0].str) {
	string::size_type res = args[0].str->str().find(args[1].c, 0);
	r.i = (res != string::npos)?res:-1;
	return r;
    }
    r.i = -1;
    return r;
}


// Convert to integer
// args[0] == string.
// Returns signed.
static UData builtin_atoi(CExecutor* aExec, UData *args)
{
    UData r;

    if (args[0].str) {
	r.i = atoi(args[0].str->c_str());
	return r;
    }

    r.i = 0;
    return r;
}

// Convert to integer
// args[0] == string.
// Returns unsigned.
static UData builtin_atou(CExecutor* aExec, UData *args)
{
    UData r;

    if (args[0].str) {
	r.u = atoi(args[0].str->c_str());
	return r;
    }

    r.u = 0;
    return r;
}

// Convert to float
// args[0] == string.
// Returns float.
static UData builtin_atof(CExecutor* aExec, UData *args)
{
    UData r;

    if (args[0].str) {
	r.f = atof(args[0].str->c_str());
	return r;
    }

    r.f = 0;
    return r;
}


#define MAX_DS 32

#define FMT_SPACE     0x0001
#define FMT_PLUS      0x0002
#define FMT_MINUS     0x0004
#define FMT_NUMBER    0x0008
#define FMT_ZERO      0x0010

#define QUAL_LONG      0x0001
#define QUAL_DBL_LONG  0x0002
#define QUAL_SHORT     0x0004

static char X_digit[] = "0123456789ABCDEF";
static char x_digit[] = "0123456789abcdef";

/*
** conv_int:
**
** Format an unsigned long in a base <= 16
** Return number of digits formated
** The ds array must be at least 32 digits long
*/

static int conv_int(unsigned long x, int base, char* dv, char* ds)
{
    if (x == 0) {
	ds[0] = '0';
	return 1;
    }
    else {
	int w = 0;
	int i;

	while(x) {
	    ds[31-w] = dv[x % base];
	    x /= base;
	    w++;
	}
	for (i = 0; i < w; i++)
	    ds[i] = ds[31-(w-1)+i];
	return w;
    }
}

static int format_pad(ostream* os, int c, int len)
{
    int i = len;
    while(i > 0) {
	*os << char(c);
	i--;
    }
    return len;
}

/*
** Format an integer
** Handle output of flags:
** d, i, b, o, u, x, X, b, B
**
*/
static int format_integer(ostream* os, long x, int code, 
			  int flags, int width, int prec)
{
    const char* prefix = "";    /* "", " ", "+","-","0x" */
    int plen;
    char ds[MAX_DS];
    char* dv;
    int len;
    int base;
    int dwidth;

    switch(code) {
    case 'd':
    case 'i':
	base = 10;
	dv = x_digit;
	if (x < 0) {
	    x = -x;
	    prefix = "-";
	}
	else {
	    if ((x > 0) && (flags & FMT_PLUS))
		prefix = "+";
	    else if (flags & FMT_SPACE)
		prefix = " ";
	}
	break;
    case 'B':
    case 'b':
	base = 2;
	dv = x_digit;
	if (flags & FMT_NUMBER)
	    prefix = "0b";
	break;
    case 'o':
	base = 8;
	dv = x_digit;
	if (flags & FMT_NUMBER)
	    prefix = "0";
	break;
    case 'u':
	base = 10;
	dv = x_digit;
	break;
    case 'x':
    case 'X':
	base = 16;
	dv = (code == 'x') ? x_digit : X_digit;
	if (flags & FMT_NUMBER)
	    prefix = (code == 'x') ? "0x" : "0X";
	break;
    default:
	return -1;
    }

    len = conv_int((unsigned long) x, base, dv, ds);
    plen = strlen(prefix);

    if (prec == -1)
	prec = 1;

    if (prec > len)
	dwidth = prec + plen;
    else
	dwidth = len + plen;

    if (width < dwidth)
	width = dwidth;

    if (dwidth == width) {
	*os << string(prefix, plen);
	if (prec > len)
	    format_pad(os, '0', (prec - len));
	*os << string(ds, len);
    }
    else {
	if (flags & FMT_MINUS) {
	    *os << string(prefix,plen);
	    if (prec > len)
		format_pad(os, '0', (prec-len));
	    *os << string(ds, len);
	    format_pad(os, ' ', width - dwidth);
	}
	else {
	    if (flags & FMT_ZERO) {
		*os << string(prefix, plen);
		format_pad(os, '0', width - dwidth);
	    }
	    else {
		format_pad(os, ' ', width - dwidth);
		*os << string(prefix, plen);
	    }
	    if (prec > len)
		format_pad(os, '0', (prec-len));
	    *os << string(ds, len);
	}
    }
    return width;
}


/*
** unscale_flt:
**
** Convert a non negative double floating point number
** The floating point is converted to a normal representaion 
** 0.xxxxxx E [+-yyy]
** The exponent yyy is returned from the function
**
** NOTE: this implemenation is not very efficent (nor correct ???)
*/
static int unscale_flt(double* x)
{
    double y = *x;
    int exp = 0;

    if (y == 0.0)
	return 0;
    else if (y < 0.1) {
	while(y < 0.1) {
	    y *= 10;
	    exp--;
	}
    }
    else if (y >= 1.0) {
	while (y >= 1.0) {
	    y /= 10;
	    exp++;
	}
    }
    *x = y;
    return exp;
}
/*
** conv_flt 
*/
static int conv_flt(double x, int* ds, int len)
{
    int i;

    for (i = 0; i < len; i++) {
	x *= 10.0;
        /* truncate */
	ds[i] = (int) x;
	x -= (double)ds[i];
    }
    return len;
}
//
// round float digits
// ld is the index of the last digit to keep in the output after the .
// make sure ds[ld] is the digit following that digit
// return 0 on ok
// return 1 on borrow (we need to prepend a 1)
//
static int round_flt(int* ds, int ld, int len)
{
    if ((ld+1>=0) && (ld+1 < len)) {
	int i;
	if (ds[ld+1] >= 6)
	    goto riple;
	else if (ds[ld+1] == 5) {
	    bool ztrail = true;
	    for (i = ld+2; ztrail && (i < len); i++)
		ztrail = (ds[i]==0);
	    if (!ztrail || (ds[ld]&1))
		goto riple;
	}
	return 0;

    riple:
	i = ld;
	while((i >= 0) && (ds[i]==9)) {
	    ds[i] = 0;
	    i--;
	}
	if (i < 0)
	    return 1;
	ds[i]++;
	return 0;
    }
    return 0;
}

/*
** E format (exponent format)
** [+|-] d.dddE<+|->Exp
**
** 
*/
static int format_e(ostream* os, char* prefix, int* ds, int exp, int code,
		    int flags, int width, int prec)
{
    int  plen;               /* prefix length */
    char exp_ds[32];         /* exponent digits */
    int  exp_len;            /* number of digits in exponent */
    int  exp_sign;
    int  dwidth;
    int i;

    // Round and fixup
    if (prec >= 0) {
	if (round_flt(ds, prec, MAX_DS)) {
	    *--ds = 1;
	    exp++;
	}
    }

    /* We want one real digit */

    exp--;
    if (exp <= 0) {
	exp_sign = '-';
	exp = -exp;
    }
    else
	exp_sign = '+';

    exp_len = conv_int(exp, 10, x_digit, exp_ds);
    if (exp_len == 1) {
	exp_ds[1] = exp_ds[0];
	exp_ds[0] = '0';
	exp_len++;
    }
    plen = strlen(prefix);
    /*       [+|-| ] d   .   ddd    E  [+|-] dd      */
    dwidth = plen +  1 + 1 + prec + 1 + 1 + exp_len;

    /* FIXME: handle width */
    *os << string(prefix, plen);
    *os << x_digit[ds[0]];
    if (prec > 0) {
	*os << '.';

	for (i = 1; i <= prec; i++) 
	    *os << x_digit[ds[i]];
    }    

    *os << char(code);
    *os << char(exp_sign);
    *os << string(exp_ds, exp_len);
    return dwidth;
}

static int format_f(ostream* os, char* prefix, int* ds, int exp, int code, 
		    int flags, int width, int prec)
{
    int  plen;               /* prefix length */
    int dwidth;
    
    plen = strlen(prefix);

    /* FIXME: handle width */
    if (exp < 0) {
	int i;
	dwidth = plen+(-exp)+1+1+prec;
	*os << string(prefix, plen);
	
	if (prec >= 0) {
	    if (round_flt(ds, prec+exp-1, MAX_DS)) {
		*--ds = 1;
		exp++;
	    }
	}

	*os << x_digit[0];
	*os << '.';
	while ((exp < 0) && prec) {
	    *os << x_digit[0];
	    exp++;
	    prec--;
	}
	if (prec) {
	    for (i = 0; i < prec; i++)
		*os << x_digit[ds[i]];
	}
    }
    else {
	int i, j;
	
	dwidth = plen+exp+1+prec;
	*os << string(prefix, plen);

	if (prec >= 0) {
	    if (round_flt(ds, prec+exp-1, MAX_DS)) {
		*--ds = 1;
		exp++;
	    }
	}

	if (exp == 0) {
	    i = 0;
	    *os << x_digit[0];
	}
	else {
	    for (i = 0; i < exp; i++)
		*os << x_digit[ds[i]];
	}
	if (prec > 0) {
	    *os << '.';

	    for (j = 1; j <= prec; j++, i++)
		*os << x_digit[ds[i]];
	}
    }
    return dwidth;
}

static int format_g(ostream* os, char* prefix, int* ds, int exp, int code, 
		    int flags, int width, int prec)
{
    int dwidth;

    if ((exp < -4) || (exp >= prec))
	dwidth = format_e(os, prefix, ds, exp, ((code=='g') ? 'e' : 'E'),
			  flags, width, prec);
    else
	dwidth = format_f(os, prefix, ds, exp, code, flags, width, prec);
    return dwidth;
}


/*
** Format an double float
** Handle output of flags
** f, e, E, g, G
*/
static int format_float(ostream* os, double x, int code, 
			int flags, int width, int prec)
{
    int  exp;          /* exponent returned from unscale */
    char* prefix = (char*) ""; /* "", "+", "-" or " " */
    int  ds[MAX_DS+1]; /* fraction digits 0.xxxxxx E+-yyy */
    int* dp = ds+1;    /* spare digit for round up digit */
    int  dwidth;

    if (prec == -1)
	prec = 6;

    if (x < 0) {
	x = -x;
	prefix = (char*)  "-";
    }
    else {
	if ((x > 0) && (flags & FMT_PLUS))
	    prefix = (char*)  "+";
	else if (flags & FMT_SPACE)
	    prefix = (char*)  " ";
    }

    if (isinf(x)) {
	*os << prefix << "inf";
	return strlen(prefix)+3;
    }
    else if (isnan(x)) {
	*os << prefix << "Nan";
	return strlen(prefix)+3;
    }
	

    exp = unscale_flt(&x);
    // leave a spare digit to allow for round up digit in ds

    ds[0] = 0;
    conv_flt(x, dp, MAX_DS);


    switch(code) {
    case 'G':
    case 'g':
	if (prec == 0)
	    prec = 1;   /* significant digits */
	dwidth = format_g(os, prefix, dp, exp, code, flags, width, prec);
	break;
    case 'E':
    case 'e':
	dwidth = format_e(os, prefix, dp, exp, code, flags, width, prec);
	break;
    case 'f':
	dwidth = format_f(os, prefix, dp, exp, code, flags, width, prec);
	break;
    default:
	dwidth = -1;
    }
    return dwidth;
}

static int format_string(ostream* os, char* s, int flags, int width, int prec)
{
    if (width == 0)
	width = strlen(s);
    if (prec == -1)
	prec = strlen(s);

    if (prec >= width) {
	*os << string(s, prec);
	return prec;
    }
    else {
	if (flags & FMT_MINUS) {
	    *os << string(s, prec);
	    format_pad(os, ' ', width - prec);
	}
	else {
	    format_pad(os, ' ', width - prec);
	    *os << string(s, prec);
	}
	return width;
    }
}

static int format_object(ostream* os, UData object,
			 int flags, int width, int prec)
{
    CObject* obj = object.o ? dynamic_cast<CObject*>(object.o) : NULL;
    
    if ((width == 0) && (prec == -1)) {
	if (object.o == NULL)  // nil is ok
	    *os <<  "nil";
	else if (obj == NULL) {
	    char tmp[128];
	    sprintf(tmp, "0x%08x", object.u);
	    *os << tmp;
	}
	else
	    obj->type()->print(os, object);
    }
    else {
	string ostr;
	ostringstream oStream;

	if (object.o == NULL)  // nil is ok
	    oStream <<  "nil";
	else if (obj == NULL) {
	    char tmp[128];
	    sprintf(tmp, "0x%08x", object.u);
	    oStream << tmp;
	}
	else
	    obj->type()->print(&oStream, object);
	ostr = oStream.str();

	if (width == 0)
	    width = ostr.size();
	if (prec == -1)
	    prec = ostr.size();

	if (flags & FMT_MINUS) {
	    *os << ostr.substr(0, prec);
	    format_pad(os, ' ', width - prec);
	}
	else {
	    format_pad(os, ' ', width - prec);
	    *os << ostr.substr(0,prec);
	}
	return width;	
    }
    return 1;
}


int m1_format_args(ostream* os, UData* va)
{
    int nchar = 0;
    int c;
    int flags;
    int qual;
    int width;
    int prec;
    char* fmt = (char*) va[0].str->c_str();
    char* fp;
    int  vi = 1;

    while(1) {
	while(((c = *fmt) != '%') && (c != '\0')) {
	    *os << char(c);
	    nchar++;
	    fmt++;
	}
	if (c == '\0')
	    break;
	/* We have an argument */
	flags = 0;
	fp = fmt;
    L_flags:
	fmt++;
	switch(*fmt) {
	case ' ': flags |= FMT_SPACE;  goto L_flags;
	case '0': flags |= FMT_ZERO;   goto L_flags;
	case '-': flags |= FMT_MINUS;  goto L_flags;
	case '+': flags |= FMT_PLUS;   goto L_flags;
	case '#': flags |= FMT_NUMBER; goto L_flags;
	default: break;
	}

	if (*fmt == '*') {
	    width = va[vi++].i;
	    if (width < 0) {
		width = -width;
		flags |= FMT_MINUS;
	    }
	    fmt++;
	}
	else {
	    width = 0;
	    while ((*fmt >= '0') && (*fmt <= '9')) {
		width = 10*width + (*fmt - '0');
		fmt++;
	    }
	}
	if (*fmt != '.')
	    prec = -1;
	else if (*++fmt == '*') {
	    prec = va[vi++].i;
	    fmt++;
	}
	else {
	    prec = 0;
	    while ((*fmt >= '0') && (*fmt <= '9')) {
		prec = 10*prec + (*fmt - '0');
		fmt++;
	    }
	}

	qual = 0;
	switch(*fmt) {
	case 'h': qual |= QUAL_SHORT; fmt++; break;
	case 'L': qual |= QUAL_DBL_LONG; fmt ++; break;
	case 'l': qual |= QUAL_LONG; fmt++; break;
	default: break;
	}

	switch((c = *fmt++)) {
	case 'c':
	    c = (int) va[vi++].c;
	    *os << char(c);
	    nchar++;
	    break;

	case 'd':
	case 'i':
	    nchar += format_integer(os, va[vi++].i, c, flags, width, prec);
	    break;

	case 'o': 
	case 'u': 
	case 'x': 
	case 'X': 
	case 'b': 
	case 'B': 
	    nchar += format_integer(os, va[vi++].u, c, flags, width, prec);
	    break;

	case 'e':
	case 'E':
	case 'f':
	case 'g':
	case 'G':
	    nchar += format_float(os, va[vi++].f, c, flags, width, prec);
	    break;
	    
	case 'n':
	    if (flags & QUAL_SHORT) 
		*((int*) va[vi++].p) = nchar;
	    else if (flags & QUAL_LONG)
		*((int*) va[vi++].p) = nchar;
	    else
		*((int*) va[vi++].p) = nchar;
	    break;

	case 'p':
	    nchar += format_object(os, va[vi++], flags, width, prec);
	    break;
	    
	case 's':
	    nchar += format_string(os, (char*) va[vi++].str->c_str(), flags, width, prec);
	    break;

	case '\0':
	    break;
	case '%':
	default:
	    *os << char(c);
	    nchar++;
	}
    }
    return nchar;
}

// args = fmt ... emit output on stdout
static UData builtin_printf(CExecutor* aExec, UData* va)
{
    UData r;

    r.u = m1_format_args(&cout, va);
    return r;
}

// args = fmt ... returned as a string
static UData builtin_sprintf(CExecutor* aExec, UData* va)
{
    ostringstream oStream;
    UData r;

    (void) m1_format_args(&oStream, va);

    r.str = m1New(CString, oStream.str());
    return r;
}

//
//  string getenv(string name)
//
static UData builtin_getenv(CExecutor* aExec, UData* args)
{
    UData r;
    char* env = NULL;
    if (args[0].str)
	env = getenv(args[0].str->c_str());
    if (env != NULL)
	r.str = m1New(CString, string(env));
    else
	r.str = m1New(CString, (char*) "");
    return r;
}


static UData builtin_reload_m1(CExecutor* aExec, UData* args)
{
    UData r;

    r.u = 0;
    m1_main().m1Halt.putValue(aExec, 1);

    // FIXME: Reload m1 files here.

    return r;
}


static UData builtin_reload_lib(CExecutor* aExec, UData* args)
{
    UData r;

    r.u = 0;
    m1_main().m1Halt.putValue(aExec, 1);
    // FIXME: Reload .so libs here
    return r;
}

static UData builtin_reboot(CExecutor* aExec, UData* args)
{
    UData r;

    sync();
    sync();
    sync();
    reboot(RB_AUTOBOOT); // Brutal!

    r.u = 0;
    m1_main().m1Halt.putValue(aExec, 1);
    return r;
}

static UData builtin_debug(CExecutor* aExec, UData* args)
{
    UData r;

#ifdef DEBUG
    r.t = true;
#else
    r.t = false;
#endif

    return r;
}


//
// object copy(object x)
// 
//    Return a deep copy of object x
//
static UData builtin_copy(CExecutor* aExec, UData* args)
{
    if (args[0].o) {
	CType* t = args[0].o->type();
	UData copy = t->produce(aExec);
	t->deepCopy(aExec, args[0], &copy);
	return copy;
    }
    return nil;
}


//
// object clone(object x)
// 
//    Return a shallow copy of object x
//
static UData builtin_clone(CExecutor* aExec, UData* args)
{
    if (args[0].o) {
	CType* t = args[0].o->type();
	UData copy = t->produce(aExec);
	t->shallowCopy(aExec, args[0], &copy);
	return copy;
    }
    return nil;
}

//
//  bool updated(event <type> x)
//
//  builtin function to check for updated events, when the
//  script guard does not really apply.
//
static UData builtin_updated(CExecutor* aExec, UData* args)
{
    UData r;
    CEvent* evt = args[0].evt;

    r.i = evt ? evt->updated() : false;
    return r;
}

//
//  string sender(event <type> x)
//
//  builtin function to report which code module and line that
//  triggered the event variable (debugging)
//

static UData builtin_sender(CExecutor* aExec, UData* args)
{
    UData r;
    ostringstream oStream;
    CEvent* evt = args[0].evt;

    if (evt && evt->senderFile()) {
	oStream << string(evt->senderFile()) << ":" << evt->senderLine();
	r.str = m1New(CString, oStream.str());
    }
    else 
	r.str = m1New(CString, (char*) "");
    return r;
}

/* FIXME: the new table should use type signature strings to
 * allow for better type checking
 */

/*
  { "i", "abs", true, builtin_iabs, "i", NULL },
  { "f", "abs", true, builtin_fabs, "f", NULL },
  { "i", "sign", true, builtin_isign, "i", NULL },
  { "f", "sign", true, builtin_isign, "f", NULL },
  ...
  { "v", "rotate", false, builtin_rotate, { "*[]", "i", NULL} },
  { "v", "reverse", false, builtin_rotate, { "*[]", NULL}  },
  ...
  { "v", "sort", false, builtin_sort, { "*[]", "i", NULL} },

  {T}E   = event type of type T
  {T}[]  = array of type T
*/

typedef struct _builtin_template {
    char* rtype;
    char* name;
    bool  pure;
    UFunction func;
    char* atype[MAX_BUILTIN_ARGS];
} TBuiltin;

static TBuiltin m1_template_builtin[] =
{
    { (char*) "i", (char*) "min", true, builtin_imin,     {(char*)"i", (char*) "i", NULL, } },

    { (char*) "f", (char*) "min", true, builtin_fmin,     {(char*)"f", (char*) "f", NULL, } },

    { (char*) "i", (char*) "max", true, builtin_imax,     {(char*)"i", (char*) "i", NULL, } },

    { (char*) "f", (char*) "max", true, builtin_fmax,     {(char*)"f", (char*) "f", NULL, } },


    { (char*) "i", (char*) "abs", true, builtin_iabs,     {(char*)"i",NULL,} },

    { (char*) "f", (char*) "abs", true, builtin_fabs,     {(char*)"f",NULL,} },

    { (char*) "i", (char*) "sign", true, builtin_isign,   {(char*)"i",NULL,} },

    { (char*) "i", (char*) "sign", true, builtin_fsign,   {(char*)"f",NULL,} },

    { (char*) "f", (char*) "trunc", true, builtin_trunc,  {(char*)"f",NULL,} },

    { (char*) "f", (char*) "round", true, builtin_round,  {(char*)"f",NULL,} },

    { (char*) "f", (char*) "ceil", true, builtin_ceilf,   {(char*)"f",NULL,} },

    { (char*) "f", (char*) "floor", true, builtin_floorf, {(char*)"f",NULL,} },

    { (char*) "f", (char*) "sin", true, builtin_sin,      {(char*)"f",NULL,} },

    { (char*) "f", (char*) "cos", true, builtin_cos,      {(char*)"f",NULL,} },

    { (char*) "f", (char*) "pi",  true, builtin_pi,       {NULL,} },

    { (char*) "f", (char*) "sqrt", true, builtin_sqrt,    {(char*)"f",NULL,} },

    { (char*) "f", (char*) "dist", true, builtin_dist,    {(char*)"f",(char*)"f",NULL,} },

    // SYSTEM
    { (char*) "f",(char*)"now", false, builtin_now,       {NULL,} },

    { (char*) "u",(char*)"inow", false, builtin_inow,     {NULL,} },

    { (char*) "u",(char*)"cycle", false, builtin_cycle,   {NULL,} },

    { (char*) "u",(char*)"refcount", false, builtin_refcount_arr,  {(char*)"{T}[]", NULL,} },

    { (char*) "u",(char*)"refcount", false, builtin_refcount_obj,  {(char*)"({T})", NULL,} },

    { (char*) "f",(char*)"randomf", false, builtin_randomf,        {NULL,} },

    { (char*) "u",(char*)"random", false, builtin_random,          {NULL,} },

    { (char*) "u",(char*)"printf", false, builtin_printf,          {(char*)"s",(char*)"...",NULL} },

    { (char*) "s",(char*)"sprintf", false, builtin_sprintf,        {(char*)"s",(char*)"...",NULL} },

    { (char*) "s", (char*) "getenv", false, builtin_getenv,         {(char*)"s",NULL} },

    { (char*) "u", (char*) "reload_m1", false, builtin_reload_m1,   { NULL} },

    { (char*) "u", (char*) "reload_lib", false, builtin_reload_lib, { NULL} },

    { (char*) "u", (char*) "reboot", false, builtin_reboot,         { NULL} },

    { (char*) "b", (char*) "debug", false, builtin_debug,         { NULL} },

    // Array operations
    { (char*) "u",(char*)"size", false, builtin_size,              {(char*)"{T}[]",NULL} },

    { (char*) "v",(char*)"rotate", false, builtin_rotate,  {(char*)"{T}[]",(char*)"i",NULL} },

    { (char*) "v",(char*)"reverse", false, builtin_reverse, {(char*)"{T}[]",NULL} },

    { (char*) "v", (char*) "swap", false, builtin_swap,      {(char*)"{T}[]",(char*)"i", (char*) "i", NULL} },

    { (char*) "v",(char*)"sort", false, builtin_sort,      {(char*)"{T}[]",NULL} },

    { (char*) "v",(char*)"sort", false, builtin_sort_index, {(char*)"{T}[]",(char*)"i",NULL} },

    // Temporary hacks
    { (char*) "i", (char*) "wcolumns", false, builtin_wcolumns, { (char*) "s", (char*) "s", (char*) "f[][]", NULL} },

    { (char*) "i", (char*) "rcolumns", false, builtin_rcolumns, { (char*) "s", (char*) "s", (char*) "f[][]", NULL} },

    { (char*) "i", (char*) "rwcolumns", false, builtin_rwcolumns,
      {(char*)"s", (char*) "s", (char*) "f[][]", (char*) "s", (char*) "f[][]", NULL }},
    
    //
    //  Object functions
    //
    { (char*) "{(T)}", (char*) "clone", false, builtin_clone, {(char*)"{(T)}", NULL}  },

    { (char*) "{(T)}", (char*) "copy", false, builtin_copy, {(char*)"{(T)}", NULL}  },

    //
    // String ops.
    //
    { (char*) "u",(char*)"size", true, builtin_strsize,      {(char*)"s",NULL} },

    { (char*) "u", (char*) "strlen", true, builtin_strlen,    { (char*) "s", NULL } },

    { (char*) "s", (char*) "strcat", true, builtin_strcat,    { (char*) "s", (char*) "s", NULL } },

    { (char*) "s", (char*) "substr", true, builtin_substr_3,  { (char*) "s", (char*) "u", (char*) "u", NULL } },

    { (char*) "s", (char*) "substr", true, builtin_substr_2,  { (char*) "s", (char*) "u", NULL } },

    { (char*) "i", (char*) "strchr", true, builtin_strchr,    { (char*) "s", (char*) "c", NULL } },

    { (char*) "i", (char*) "atoi",   true, builtin_atoi,      { (char*) "s", NULL } },

    { (char*) "u", (char*) "atou", true, builtin_atou,        { (char*) "s", NULL } },

    { (char*) "f", (char*) "atof", true, builtin_atof,        { (char*) "s", NULL } },

    //
    //  Event functions
    //

    { (char*) "i", (char*) "updated", false, builtin_updated, { (char*) "{T}E", NULL }},

    { (char*) "s", (char*) "sender", false,  builtin_sender,   { (char*) "{T}E", NULL }},
 
    { NULL,  NULL,   false,  NULL,             {NULL,}}
};



static UBuiltin m1_builtin[] =
{
    { M1TYPE_SIGNED, (char*) "min", true, builtin_imin, 
      {M1TYPE_SIGNED, M1TYPE_SIGNED, BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "min", true, builtin_fmin, 
      {M1TYPE_FLOAT, M1TYPE_FLOAT, BUILTIN_ARG_END,} },

    { M1TYPE_SIGNED, (char*) "max", true, builtin_imax, 
      {M1TYPE_SIGNED, M1TYPE_SIGNED, BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "max", true, builtin_fmax, 
      {M1TYPE_FLOAT, M1TYPE_FLOAT, BUILTIN_ARG_END,} },

    { M1TYPE_SIGNED, (char*) "abs", true, builtin_iabs, 
      {M1TYPE_SIGNED,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "abs", true, builtin_fabs, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_SIGNED, (char*) "sign", true, builtin_isign, 
      {M1TYPE_SIGNED,BUILTIN_ARG_END,} },

    { M1TYPE_SIGNED, (char*) "sign", true, builtin_fsign, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "trunc", true, builtin_trunc, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "round", true, builtin_round, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "ceil", true, builtin_ceilf, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "floor", true, builtin_floorf, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "sin", true, builtin_sin, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "cos", true, builtin_cos, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "pi",  true, builtin_pi, 
      {BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "sqrt", true, builtin_sqrt, 
      {M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT, (char*) "dist", true, builtin_dist, 
      {M1TYPE_FLOAT,M1TYPE_FLOAT,BUILTIN_ARG_END,} },

    // SYSTEM
    { M1TYPE_FLOAT,(char*)"now", false, builtin_now, 
      {BUILTIN_ARG_END,} },

    { M1TYPE_UNSIGNED,(char*)"inow", false, builtin_inow, 
      {BUILTIN_ARG_END,} },

    { M1TYPE_UNSIGNED,(char*)"cycle", false, builtin_cycle, 
      {BUILTIN_ARG_END,} },

    { M1TYPE_UNSIGNED,(char*)"refcount", false, builtin_refcount_arr, 
      {M1TYPE_ARRAY, BUILTIN_ARG_END,} },

    { M1TYPE_UNSIGNED,(char*)"refcount", false, builtin_refcount_obj, 
      {M1TYPE_OBJECT, BUILTIN_ARG_END,} },

    { M1TYPE_FLOAT,(char*)"randomf", false, builtin_randomf, 
      {BUILTIN_ARG_END,} },

    { M1TYPE_UNSIGNED,(char*)"random", false, builtin_random, 
      {BUILTIN_ARG_END,} },

    { M1TYPE_UNSIGNED,(char*)"printf", false, builtin_printf,
      {M1TYPE_STRING,BUILTIN_ARG_VARG,BUILTIN_ARG_END} },

    { M1TYPE_STRING,(char*)"sprintf", false, builtin_sprintf,
      {M1TYPE_STRING,BUILTIN_ARG_VARG,BUILTIN_ARG_END} },

    { M1TYPE_STRING, (char*) "getenv", false, builtin_getenv,
      {M1TYPE_STRING,BUILTIN_ARG_END} },

    { M1TYPE_UNSIGNED, (char*) "reload_m1", false, builtin_reload_m1,
      { BUILTIN_ARG_END} },

    { M1TYPE_UNSIGNED, (char*) "reload_lib", false, builtin_reload_lib,
      { BUILTIN_ARG_END} },

    { M1TYPE_UNSIGNED, (char*) "reboot", false, builtin_reboot,
      { BUILTIN_ARG_END} },

    { M1TYPE_BOOL, (char*) "debug", false, builtin_debug,
      { BUILTIN_ARG_END} },

    // Array operations
    { M1TYPE_UNSIGNED,(char*)"size", false, builtin_size,
      {M1TYPE_ARRAY,BUILTIN_ARG_END} },

    { M1TYPE_VOID,(char*)"rotate", false, builtin_rotate,
      {M1TYPE_ARRAY,M1TYPE_SIGNED,BUILTIN_ARG_END} },

    { M1TYPE_VOID,(char*)"reverse", false, builtin_reverse,
      {M1TYPE_ARRAY,BUILTIN_ARG_END} },

    { M1TYPE_VOID,(char*)"swap", false, builtin_swap,
      {M1TYPE_ARRAY,M1TYPE_SIGNED, M1TYPE_SIGNED, BUILTIN_ARG_END} },

    { M1TYPE_VOID,(char*)"sort", false, builtin_sort,
      {M1TYPE_ARRAY,BUILTIN_ARG_END} },

    { M1TYPE_VOID,(char*)"sort", false, builtin_sort_index,
      {M1TYPE_ARRAY,M1TYPE_SIGNED,BUILTIN_ARG_END} },

    { M1TYPE_SIGNED, (char*) "wcolumns", false, builtin_wcolumns,
      { M1TYPE_STRING, M1TYPE_STRING, M1TYPE_ARRAY, BUILTIN_ARG_END} },

    { M1TYPE_SIGNED, (char*) "rcolumns", false, builtin_rcolumns,
      { M1TYPE_STRING, M1TYPE_STRING, M1TYPE_ARRAY, BUILTIN_ARG_END} },

    { M1TYPE_SIGNED, (char*) "rwcolumns", false, builtin_rwcolumns,
      {M1TYPE_STRING, 
       M1TYPE_STRING, M1TYPE_ARRAY,
       M1TYPE_STRING, M1TYPE_ARRAY,
       BUILTIN_ARG_END }},

    //
    //  Object functions
    //
    { M1TYPE_OBJECT, (char*) "clone", false, builtin_clone,
      {M1TYPE_OBJECT, BUILTIN_ARG_END}  },

    { M1TYPE_OBJECT, (char*) "copy", false, builtin_copy,
      {M1TYPE_OBJECT, BUILTIN_ARG_END}  },

    //
    // String ops.
    //
    { M1TYPE_UNSIGNED,(char*)"size", true, builtin_strsize,
      {M1TYPE_STRING,BUILTIN_ARG_END} },

    { M1TYPE_UNSIGNED, (char*) "strlen", true, builtin_strlen, 
      { M1TYPE_STRING, BUILTIN_ARG_END } },

    { M1TYPE_STRING, (char*) "strcat", true, builtin_strcat, 
      { M1TYPE_STRING, M1TYPE_STRING, BUILTIN_ARG_END } },

    { M1TYPE_STRING, (char*) "substr", true, builtin_substr_3, 
      { M1TYPE_STRING, M1TYPE_UNSIGNED, M1TYPE_UNSIGNED, BUILTIN_ARG_END } },

    { M1TYPE_STRING, (char*) "substr", true, builtin_substr_2, 
      { M1TYPE_STRING, M1TYPE_UNSIGNED, BUILTIN_ARG_END } },

    { M1TYPE_SIGNED, (char*) "strchr", true, builtin_strchr, 
      { M1TYPE_STRING, M1TYPE_CHAR, BUILTIN_ARG_END } },

    { M1TYPE_SIGNED, (char*) "atoi", true, builtin_atoi, 
      { M1TYPE_STRING, BUILTIN_ARG_END } },

    { M1TYPE_UNSIGNED, (char*) "atou", true, builtin_atou, 
      { M1TYPE_STRING, BUILTIN_ARG_END } },

    { M1TYPE_FLOAT, (char*) "atof", true, builtin_atof, 
      { M1TYPE_STRING, BUILTIN_ARG_END } },

    //
    //  Event functions
    //

    { M1TYPE_SIGNED, (char*) "updated", false, builtin_updated,
      { M1TYPE_EVENT, BUILTIN_ARG_END }},

    { M1TYPE_STRING, (char*) "sender", false, builtin_sender,
      { M1TYPE_EVENT, BUILTIN_ARG_END }},
 
    { M1TYPE_NONE, NULL,     false,  NULL,             {BUILTIN_ARG_END,}}
};

UBuiltin* lookupBuiltin(char* name)
{
    int i = 0;

    while(m1_builtin[i].name != NULL) {
	if (strcmp(name, m1_builtin[i].name) == 0)
	    return &m1_builtin[i];
	i++;
    }
    return NULL;
}

static inline CType* unfoldType(CType* a)
{
    if ((a!=NULL) && isAType(CEventType*, a))
	return ((CEventType*)a)->baseType();
    return a;
}

/*
 * Find the closest match - allow for polymorph builtin functions
 * argment and function must match - if no match the most suitable
 * built in function is return as a reference (same name etc)
 * if name do not match any of the builtin function then 
 * NULL is returned and alfBt is also set to NULL
 */
UBuiltin* matchBuiltin(char* name, CType** types, int nargs, 
		       UBuiltin** altBf)
{
    int i = 0;
    UBuiltin* selected = NULL; // the function selected
    UBuiltin* matched = NULL;  // current best match
    bool matchedArity = false; // if match has all arguments
    int matchedArgs = 0;       // number of arguments that matched
    int xmatchedArgs = 0;      // number of arguments that matched exactly

    DBGFMT_TYPE("matchBuiltin with %s", 
		formatCall(name, types, nargs).c_str());
    while(m1_builtin[i].name != NULL) {
	UBuiltin* bf = &m1_builtin[i];
	if (strcmp(name, bf->name) == 0) {
	    int xia = 0;
	    int  ia = 0;

	    while(ia<nargs) {
		M1TypeTag tia = unfoldType(types[ia])->typeTag();
		if ((tia == bf->atype[ia]) ||
		    (types[ia]->typeTag() == bf->atype[ia])) {
		    ia++;
		    xia++;
		}
		else {
		    int atg = tia & M1T_TAG_MASK;
		    int btg = bf->atype[ia] & M1T_TAG_MASK;
		    if ((atg != M1T_INTEGER) && (atg != M1T_FLOAT))
			break;
		    if ((btg != M1T_INTEGER) && (btg != M1T_FLOAT))
			break;
		    ia++;
		}
	    }

	    /* all arguments matched */
	    if ((bf->atype[ia] == BUILTIN_ARG_END) ||
		(bf->atype[ia] == BUILTIN_ARG_VARG)) {
		DBGFMT_TYPE("matched %s", formatBuiltin(bf).c_str());
		if ((matched == NULL) || (xia > xmatchedArgs)) {
		    matchedArity = true;  // we met the arity condition
		    selected = bf;
		    matched = bf;
		    matchedArgs  = ia;
		    xmatchedArgs  = xia;
		}
		else if ((ia > matchedArgs)) {
		    matched = bf;
		    matchedArgs = ia;
		    xmatchedArgs = xia;
		}
	    }
	    else {
		if ((xia > xmatchedArgs) || (ia > matchedArgs)) {
		    DBGFMT_TYPE("matched %s", formatBuiltin(bf).c_str());
		    matched = bf;
		    matchedArgs = ia;
		    xmatchedArgs = xia;
		}
	    }
	}
	i++;
    }
    if (selected)
	DBGFMT_TYPE("selected %s", formatBuiltin(selected).c_str());
    else
	DBGFMT_TYPE("no function selected");
    if (altBf) *altBf = matched;
    return selected;
}

/* format a built in function with argument type descriptions */
string formatTypeTag(M1TypeTag tag)
{
    switch(tag) {
    case M1TYPE_BYTE:     return "byte";
    case M1TYPE_CHAR:     return "char";
    case M1TYPE_BOOL:     return "bool";
    case M1TYPE_SIGNED:   return "int";
    case M1TYPE_UNSIGNED: return "unsigned int";
    case M1TYPE_FLOAT:    return "float";
    case M1TYPE_STRING:   return "string";
    case M1TYPE_ARRAY:    return "[]";
    case M1TYPE_OBJECT:   return "@";
    case M1TYPE_EVENT:    return "event";

    case M1TYPE_NONE:     return "none";
    case M1TYPE_ERROR:    return "error";
    case M1TYPE_VOID:     return "void";
    case M1TYPE_ANY:      return "any";
    case BUILTIN_ARG_VARG: return "...";
    default: return "?";
    }
}

int nargsBuiltin(UBuiltin* bf)
{
    int i = 0;
    while((i < MAX_BUILTIN_ARGS) && ((bf->atype[i]) != BUILTIN_ARG_END)) {
	if (bf->atype[i] == BUILTIN_ARG_VARG)
	    break;
	i++;
    }
    return i;
}

/* Format a function call as type template */
string formatCall(char* fname, CType** types, int nargs)
{
    string fmt = string(fname) + "(";
    int i = 0;

    while(i < nargs) {
	if (i == 0)
	    fmt += types[i]->typeName();
	else
	    fmt += "," + types[i]->typeName();
	i++;
    }
    return fmt + ")";
}


string formatBuiltin(UBuiltin* bf)
{
    M1TypeTag tag = bf->rtype;
    string fmt = formatTypeTag(tag) + " " + string(bf->name) + "(";
    int i = 0;

    while((i < MAX_BUILTIN_ARGS) && ((tag=bf->atype[i]) != BUILTIN_ARG_END)) {
	if (i == 0)
	    fmt += formatTypeTag(tag);
	else
	    fmt += "," + formatTypeTag(tag);
	if (tag == BUILTIN_ARG_VARG)
	    break;
	i++;
    }
    return fmt + ((i == 0) ? "void)" : ")");
}

//
// Type signatures
// basic:
//     'i'	    =>  signed_type()
//     'u'  	    =>  unsigned_type()
//     'b'	    =>	byte_type()
//     'c'	    =>	char_type()
//     't' 	    =>	bool_type()
//     'f' 	    =>	float_type()
//     'T'          =>  time_type()
//     'v'          =>  void_type()
//     's'          =>  string_type()
//
// event:
//     <type> 'I': 	    =>  CEventType::create(T,E_INPUT)
//     <type> 'O':    	    =>  CEventType::create(T,E_OUTPUT)
//     <type> 'E':     	    =>  CEventType::create(T,E_INOUT)
//
// event queue:
//     <type> 'Q':     	    =>  CEventType::createQueue(T,E_INOUT)
//     <type> 'R': 	    =>  CEventType::createQueue(T,E_INPUT)
//
// array:
//     <type> '[' [<size>] ']' : T type_ARRAY =>  CArrayType::create(T, <size>)
//
// type-name:
//     '(' type-name ')'
//
// type-var:
//     '{' type-var '}'
//
// definition:
//      '/'type-name ['/' parent-type-name ] ':'
//      	<type> ':' member-name ';'
//      	<type> ':' member-name ';'
//      	<type> ':' member-name ';'
//       ';'
//
// NOTE we may have to make this a bit more complex in the future
//
CType* typeFromSignature(string signature)
{
    CType* t = NULL;
    int c = 0;
    int array_size;
    string type_name;
    int len = signature.size();
    int i = 0;

    while(i < len) {
	switch(signature[i++]) {
	case 'i': t = signed_type(); break;
	case 'u': t = unsigned_type(); break;
	case 'b': t = byte_type();   break;
	case 'c': t = char_type();   break;
	case 't': t = bool_type();   break;
	case 'f': t = float_type();  break;
	case 's': t = string_type(); break;
	case 'v': t = void_type(); break;
	case 'T': t = time_type(); break;
	case 'I': t = CEventType::create(t, E_INPUT); break;
	case 'O': t = CEventType::create(t, E_OUTPUT); break;
	case 'E': t = CEventType::create(t, E_INOUT);  break;
	case 'Q': t = CEventType::createQueue(t, E_INOUT);  break;
	case 'R': t = CEventType::createQueue(t, E_INPUT); break;
	case '[':
	    c = 0;
	    array_size = 0;
	    while((i < len) && ((c = signature[i++]) != ']')) {
		if ((c >= '0') && (c <= '9'))
		    array_size = array_size*10 + (c-'0');
		else
		    return NULL;
	    }
	    if (c != ']')
		return NULL;
	    t = CArrayType::create(t, array_size);
	    break;
	case '(':
	    c = 0;
	    type_name = "";
	    while((i < len) && ((c = signature[i++]) != ')'))
		type_name += c;
	    if (c != ')')
		return NULL;
	    if ((t = m1TypeLookup(type_name)) == NULL)
		return NULL;
	    break;
	}
    }
    return t;
}


// Singletons (prepare for fast inline access)
typedef enum {
    ErrorTypeIndex = 0,
    VoidTypeIndex  = 1,
    AnyTypeIndex   = 2,
    ByteTypeIndex  = 3,
    CharTypeIndex  = 4,
    BoolTypeIndex  = 5,
    SignedTypeIndex  = 6,
    UnsignedTypeIndex  = 7,
    IntegerTypeIndex  = 8,
    FloatTypeIndex  = 9,
    StringTypeIndex  = 10,
    NumericTypeIndex = 11,
    TimeTypeIndex  = 12,
    NextTypeIndex  = 13
} ETypeBaseIndex;

static CType* gBaseTypeArray[NextTypeIndex];
static CType* gEventTypeArray[NextTypeIndex];
static CType* gInputTypeArray[NextTypeIndex];
static CType* gOutputTypeArray[NextTypeIndex];
static CType* gQEventTypeArray[NextTypeIndex];
static CType* gQInputTypeArray[NextTypeIndex];

#define BASE_TYPE_CREATE(ID)				\
    if (!gBaseTypeArray[ID##Index])			\
	gBaseTypeArray[ID##Index]= m1New(C##ID);	\
    return gBaseTypeArray[ID##Index]

CType* error_type()    { BASE_TYPE_CREATE(ErrorType);    }
CType* void_type()     { BASE_TYPE_CREATE(VoidType);     }
CType* any_type()      { BASE_TYPE_CREATE(AnyType);      }
CType* numeric_type()  { BASE_TYPE_CREATE(NumericType);  }
CType* byte_type()     { BASE_TYPE_CREATE(ByteType);     }
CType* char_type()     { BASE_TYPE_CREATE(CharType);     }
CType* bool_type()     { BASE_TYPE_CREATE(BoolType);     }
CType* signed_type()   { BASE_TYPE_CREATE(SignedType);   }
CType* unsigned_type() { BASE_TYPE_CREATE(UnsignedType); }
CType* float_type()    { BASE_TYPE_CREATE(FloatType);    }
CType* time_type()     { BASE_TYPE_CREATE(TimeType);     }
CType* string_type()   { BASE_TYPE_CREATE(StringType);   }

// Create basic event type (and base type if needed)
#define EVENT_TYPE_CREATE(ID)						\
    if (!gEventTypeArray[ID##Index]) {					\
	if (!gBaseTypeArray[ID##Index])					\
	    gBaseTypeArray[ID##Index]= m1New(C##ID);			\
	gEventTypeArray[ID##Index] =					\
	    CEventType::create(gBaseTypeArray[ID##Index],E_INOUT);	\
    }									\
    return gEventTypeArray[ID##Index]

CType* event_byte_type()     { EVENT_TYPE_CREATE(ByteType);     }
CType* event_char_type()     { EVENT_TYPE_CREATE(CharType);     }
CType* event_bool_type()     { EVENT_TYPE_CREATE(BoolType);     }
CType* event_signed_type()   { EVENT_TYPE_CREATE(SignedType);   }
CType* event_unsigned_type() { EVENT_TYPE_CREATE(UnsignedType); }
CType* event_float_type()    { EVENT_TYPE_CREATE(FloatType);    }
CType* event_time_type()     { EVENT_TYPE_CREATE(TimeType);     }
CType* event_string_type()   { EVENT_TYPE_CREATE(StringType);   }


#define EVENT_QUEUE_TYPE_CREATE(ID)					\
    if (!gQEventTypeArray[ID##Index]) {					\
	if (!gBaseTypeArray[ID##Index])					\
	    gBaseTypeArray[ID##Index]= m1New(C##ID);			\
	gQEventTypeArray[ID##Index] =					\
	    CEventType::createQueue(gBaseTypeArray[ID##Index],E_INOUT); \
    }									\
    return gQEventTypeArray[ID##Index]

CType* event_queue_byte_type()     { EVENT_QUEUE_TYPE_CREATE(ByteType);     }
CType* event_queue_char_type()     { EVENT_QUEUE_TYPE_CREATE(CharType);     }
CType* event_queue_bool_type()     { EVENT_QUEUE_TYPE_CREATE(BoolType);     }
CType* event_queue_signed_type()   { EVENT_QUEUE_TYPE_CREATE(SignedType);   }
CType* event_queue_unsigned_type() { EVENT_QUEUE_TYPE_CREATE(UnsignedType); }
CType* event_queue_float_type()    { EVENT_QUEUE_TYPE_CREATE(FloatType);    }
CType* event_queue_time_type()     { EVENT_QUEUE_TYPE_CREATE(TimeType);     }
CType* event_queue_string_type()   { EVENT_QUEUE_TYPE_CREATE(StringType);   }


// Create basic event type (and base type if needed)
#define INPUT_TYPE_CREATE(ID)					\
    if (!gInputTypeArray[ID##Index]) {				\
	if (!gBaseTypeArray[ID##Index])					\
	    gBaseTypeArray[ID##Index]= m1New(C##ID);			\
	gInputTypeArray[ID##Index] =				\
	    CEventType::create(gBaseTypeArray[ID##Index],E_INPUT);	\
    }								\
    return gInputTypeArray[ID##Index]

CType* input_byte_type()     { INPUT_TYPE_CREATE(ByteType);     }
CType* input_char_type()     { INPUT_TYPE_CREATE(CharType);     }
CType* input_bool_type()     { INPUT_TYPE_CREATE(BoolType);     }
CType* input_signed_type()   { INPUT_TYPE_CREATE(SignedType);   }
CType* input_unsigned_type() { INPUT_TYPE_CREATE(UnsignedType); }
CType* input_float_type()    { INPUT_TYPE_CREATE(FloatType);    }
CType* input_time_type()     { INPUT_TYPE_CREATE(TimeType);     }
CType* input_string_type()   { INPUT_TYPE_CREATE(StringType);   }

#define INPUT_QUEUE_TYPE_CREATE(ID)				\
    if (!gQInputTypeArray[ID##Index]) {					\
	if (!gBaseTypeArray[ID##Index])					\
	    gBaseTypeArray[ID##Index]= m1New(C##ID);			\
	gQInputTypeArray[ID##Index] =					\
	    CEventType::createQueue(gBaseTypeArray[ID##Index],E_INPUT);	\
    }									\
    return gQInputTypeArray[ID##Index]

CType* input_queue_byte_type()     { INPUT_QUEUE_TYPE_CREATE(ByteType);     }
CType* input_queue_char_type()     { INPUT_QUEUE_TYPE_CREATE(CharType);     }
CType* input_queue_bool_type()     { INPUT_QUEUE_TYPE_CREATE(BoolType);     }
CType* input_queue_signed_type()   { INPUT_QUEUE_TYPE_CREATE(SignedType);   }
CType* input_queue_unsigned_type() { INPUT_QUEUE_TYPE_CREATE(UnsignedType); }
CType* input_queue_float_type()    { INPUT_QUEUE_TYPE_CREATE(FloatType);    }
CType* input_queue_time_type()     { INPUT_QUEUE_TYPE_CREATE(TimeType);     }
CType* input_queue_string_type()   { INPUT_QUEUE_TYPE_CREATE(StringType);   }



// Create basic event type (and base type if needed)
#define OUTPUT_TYPE_CREATE(ID)					\
    if (!gOutputTypeArray[ID##Index]) {				\
	if (!gBaseTypeArray[ID##Index])					\
	    gBaseTypeArray[ID##Index]= m1New(C##ID);			\
	gOutputTypeArray[ID##Index] =				\
	    CEventType::create(gBaseTypeArray[ID##Index],E_OUTPUT);	\
    }									\
    return gOutputTypeArray[ID##Index]

CType* output_byte_type()     { OUTPUT_TYPE_CREATE(ByteType);     }
CType* output_char_type()     { OUTPUT_TYPE_CREATE(CharType);     }
CType* output_bool_type()     { OUTPUT_TYPE_CREATE(BoolType);     }
CType* output_signed_type()   { OUTPUT_TYPE_CREATE(SignedType);   }
CType* output_unsigned_type() { OUTPUT_TYPE_CREATE(UnsignedType); }
CType* output_float_type()    { OUTPUT_TYPE_CREATE(FloatType);    }
CType* output_time_type()     { OUTPUT_TYPE_CREATE(TimeType);     }
CType* output_string_type()   { OUTPUT_TYPE_CREATE(StringType);   }

// Convert a tagType into a "basic" CType
CType* tag_type(int tag)
{
    switch(tag) {
    case M1TYPE_ERROR:    return error_type();
    case M1TYPE_VOID:     return void_type();
    case M1TYPE_ANY:      return any_type();
    case M1TYPE_NUMERIC:  return numeric_type();
    case M1TYPE_BYTE:     return byte_type();
    case M1TYPE_CHAR:     return char_type();
    case M1TYPE_BOOL:     return bool_type();
    case M1TYPE_SIGNED:   return signed_type();
    case M1TYPE_UNSIGNED: return unsigned_type();
    case M1TYPE_FLOAT:    return float_type();
    case M1TYPE_STRING:   return string_type();
    case M1TYPE_ARRAY:    return CArrayType::create(any_type(), 0);
    case M1TYPE_OBJECT:   return any_type();
    default:
	DBGFMT("tag_type called with bad tag=%d", tag);
	return error_type();
    }
}


//////////////////////////////////////////////////////////////////////////////
//!
//! VmMain: main object contain globals
//!
//////////////////////////////////////////////////////////////////////////////

XOBJECT_TYPE_BOOTSTRAP(VmMain);

VmMain::VmMain(CExecutor* aExec, CBaseType* aType) : 
    CExecutable(aExec, aType),
    m1Halt(this),
    m1FPS(this),
    m1AvgSweepTime(this),
    m1AvgRedrawTime(this)
{
    CString* cstr;
    CArray*  args;
    char ser[7];
    int ser_des = open("/m1/serial.txt", O_RDONLY);

    strcpy(ser, "??????"); // Probably cracked if seial is removed.
    if (ser_des != -1) {
	read(ser_des, ser, 6);
	close(ser_des);
    } 

    m1Halt.putValue(aExec, 0);
    m1FPS.putValue(aExec, 0);
    m1AvgSweepTime.putValue(aExec, 0);
    m1AvgRedrawTime.putValue(aExec, 0);

    cstr = m1New(CString, (char*)VERSION);
    m1Version = m1RetainString(cstr);

    cstr = m1New(CString, ser);
    m1Serial = m1RetainString(cstr);

    args = new CArray(aExec, (CArrayType*)typeAt("argv"), sizeof(UData), 0);

    put(aExec, XINDEX(VmMain,version),               UString(m1Version), TRIGGER_AUTO);
    put(aExec, XINDEX(VmMain,serial),                UString(m1Serial),  TRIGGER_AUTO);
    eventPut(aExec, XINDEX(VmMain,halt),              &m1Halt);
    eventPut(aExec, XINDEX(VmMain,fps),               &m1FPS);
    eventPut(aExec, XINDEX(VmMain,averageSweepTime),  &m1AvgSweepTime);
    eventPut(aExec, XINDEX(VmMain,averageRedrawTime), &m1AvgRedrawTime);

    put(aExec, XINDEX(VmMain,argv), UArray(args), TRIGGER_AUTO);

    aExec->deactivate(this);
};

VmMain::~VmMain(void) 
{
}

//////////////////////////////////////////////////////////////////////////////
//!
//! CBaseObject : Mother of all VmObjects
//!
//////////////////////////////////////////////////////////////////////////////

CBaseObject::CBaseObject(CExecutor* aExec, CBaseType* aType, 
			 size_t elemSize, size_t n) 
{
    int i;
    
    mType     = m1RetainBaseType(aType);
    STAT_INC(mType->mStatObjects);

    mElements = new UDataVector(n);
    for (i = 0; i < (int)n; i++)
	mElements->at(i).o = 0;
    if (aType->typeLevel() == 1) {
	if (aType->isLibrary()) {
	    m1_context().put(aExec, aType->name(), UObject(this));
	    // Assign the library singleton
	    aType->setSingleton(this);
	}
    }
    DBGFMT("CBaseObject::CBaseObject(): nElem[%d]:elemSize[%d]): %s", 
	   (int) n, (int) elemSize, aType->cname());
}

CBaseObject::~CBaseObject()
{
    STAT_DEC(mType->mStatObjects);
    if (mElements)
	delete mElements;
    m1ReleaseBaseType(mType);
}

// Mark all accessible objects
int CBaseObject::mark(Mark_t aMark)
{
    int marked = 0;
    if (aMark != mMark) {
	size_t n = size();
	int i;

	marked += CObject::mark(aMark);
	marked += type()->mark(aMark);

	for (i = 0; i < (int) n; i++) {
	    CType* t = typeAt(i);
	    CObject* obj;
	    CString* str;
	    CArray* arr;
	    CEvent* evt;

	    switch(t->typeTag()) {
	    case M1TYPE_OBJECT:
		obj = at(i).o;
		marked += m1Mark(obj, aMark);
		break;
	    case M1TYPE_STRING:
		str = at(i).str;
		marked += m1Mark(str, aMark);
		break;
	    case M1TYPE_ARRAY:
		arr = at(i).arr;
		marked += m1Mark(arr, aMark);
		break;
	    case M1TYPE_EVENT:
		evt = eventAt(i);
		marked += m1Mark(evt, aMark);
		break;
	    default:
		break;
	    }
	}
    }
    return marked;
}

int CBaseObject::eventPut(CExecutor* aExec, int aIndex, CEvent* aEvt)
{
    CEvent* oldEvent;

    if (!mElements)
	return -1;

    if (aEvt) {
	aEvt->setIndex(aIndex);
	aEvt->cancel(aExec);
    }

    if ((oldEvent = mElements->at(aIndex).evt) != NULL) {
	if (oldEvent->marked())
	    oldEvent->owner()->delUpdatedEvent(aExec, oldEvent);
	delete oldEvent;
    }

    mElements->at(aIndex).evt = aEvt;
    return aIndex;
}

int CBaseObject::eventPut(CExecutor* aExec, string aFieldName, CEvent* aEvt)
{
    CField* fld = mType->field(aFieldName);

    if (!fld) {
	ERRFMT("BaseObject::eventPut(%s) No field called [%s] found.", 
	       type()->cname(), aFieldName.c_str());
	exit(1);
    }
    return eventPut(aExec, fld->index(), aEvt);
}


//////////////////////////////////////////////////////////////////////////////
//
// CExecutable : CBaseObject
//
//////////////////////////////////////////////////////////////////////////////

CExecutable::CExecutable(CExecutor* aExec, CBaseType *aType) :
    CBaseObject(aExec, aType, sizeof(UData), aType->fieldCount()),
    mFlags(0),
    mDisconnect(this)
{
    setFlags(ExecuteOnEventUpdate);
    // We initialize cycle count to one less than current executor
    // so it's not misstaken for a executed object
    mCycle = aExec->cycle()-1;
    
    mUpdatedEvents = NULL; // Initailize list of updated events
    mQLink = NULL;         // Queue link
    mStructureChanged = NULL;       // May point to a "special" event field
    // stop constructor from schedule the object when
    // fields are setup.
    aExec->activate(this);
}

CExecutable::CExecutable(CExecutor* aExec, CBaseType *aType, 
			 size_t elemSize, size_t n) :
    CBaseObject(aExec, aType, elemSize, n),
    mFlags(0),
    mDisconnect(this)
{
    setFlags(ExecuteOnEventUpdate);
    // We initialize cycle count to one less than current executor
    // so it's not misstaken for a executed object
    mCycle = aExec->cycle()-1;

    mUpdatedEvents = NULL; // Initailize list of updated events
    mQLink = NULL;         // Queue link
    mStructureChanged = NULL;       // May point to a "special" event field
    // stop constructor from schedule the object when
    // fields are setup.
    aExec->activate(this);
}

CExecutable::~CExecutable()
{
#ifdef DEBUG
/***
    if (mUpdatedEvents)
	m1BreakHere(__FILE__, __LINE__, "events are hanging around");
***/
#endif
    if (mStructureChanged)
	delete mStructureChanged;
}

void CExecutable::disconnectOutput(CExecutor* aExec)
{
    size_t n = size();
    int i;
    
    for (i = 0; i < (int) n; i++) {
	CType* t = typeAt(i);
	// FIXME handle array of events ?
	if ((t->typeTag()) == M1TYPE_EVENT) {
	    CEventType* et = (CEventType*) t;
	    if (et->isOutput()) {
		CEvent* evt = eventAt(i);
		// disconnect subscribers!
		if (evt) evt->disconnect(); 
	    }
	}
    }
}

void CExecutable::execute(CExecutor* aExec)
{
    // FIXME: not used right now - add abstract Executable!
    if (mDisconnect.updated() && mDisconnect.value())
	disconnectOutput(aExec);
}

//
// CExecutable::disconnect
//
//   Disconnect event target from event source
//
bool CExecutable::disconnect(int evtPos)
{
    CType* dst_e  = type()->typeAt(evtPos);

    if (dst_e == NULL) {
	fprintf(stderr,"CExecutable::disconnect: target is not defined\n");
	return false;
    }
    DBGFMT("CExecutable: disconnected %s:%s",
	   type()->cname(), ((CBaseType*)type())->fieldName(evtPos));
    eventAt(evtPos)->deleteFromSource();
    return true;
}

//
// CExecutable::disconnect
//
//   Disconnect event target from event source
//
bool CExecutable::disconnect(string dstField)
{
    int dst_pos;

    if ((dst_pos = indexAt(dstField)) < 0) {
	fprintf(stderr, "CExecutable:disconnect: target %s not found in %s\n",
		dstField.c_str(), type()->cname());
	return false;
    }
    return disconnect(dst_pos);
}

bool CExecutable::connect(int evtPos,CObject* src,int srcPos)
{
    if ((src == NULL) && (srcPos == 0))
	return disconnect(evtPos);
    else {
	CType* srcType = src->type();
	CType* dst_e  = type()->typeAt(evtPos);
	CType* src_e  = srcType->typeAt(srcPos);

	if (dst_e == NULL) {
	    fprintf(stderr,"CExecutable::connect: target is not defined\n");
	    return false;
	}
	if (src_e == NULL) {
	    fprintf(stderr, "CExecutable::connect: source is not defined\n");
	    return false;
	}

	if (!isAType(CEventType*, dst_e) || 
	    !isAType(CEventType*, src_e) ||
	    !((CEventType*) dst_e)->isInput() ||
	    !((CEventType*) src_e)->isOutput() ||
	    (((CEventType*) dst_e)->baseType() != 
	     ((CEventType*) src_e)->baseType())) {
	    fprintf(stderr, "CExecutable::connect: "
		    "target %s:%s and source %s:%s are not compatible\n",
		    type()->cname(), 
		    ((CBaseType*)type())->fieldName(evtPos),
		    src->type()->cname(),
		    ((CBaseType*)(src->type()))->fieldName(srcPos));
	    return false;
	}

	DBGFMT("CExcutable: connect %s:%s with %s:%s",
	       type()->cname(), 
	       ((CBaseType*)type())->fieldName(evtPos),
	       src->type()->cname(),
	       ((CBaseType*)(src->type()))->fieldName(srcPos));

	src->eventAt(srcPos)->addSubscriber(eventAt(evtPos));
	return true;
    }
}

bool CExecutable::connect(string dstField, CObject* src, string srcField)
{
    int dst_pos, src_pos;

    if ((dst_pos = indexAt(dstField)) < 0) {
	fprintf(stderr, "CExecutable:connect: target %s not found in %s\n",
		dstField.c_str(), type()->cname());
	return false;
    }
    if ((src_pos = src->indexAt(srcField)) < 0) {
	fprintf(stderr, "CExecutable:connect: source %s not found in %s\n",
		srcField.c_str(), src->type()->cname());
	return false;
    }
    return connect(dst_pos, src, src_pos);
}

void CExecutable::propagateAllUpdated(CExecutor* aExec) 
{
    CEvent* ptr = mUpdatedEvents;
    while(ptr) {
	CEvent* nptr = ptr->getNextUpdated();  // protect from side-effect
	ptr->propagate(aExec);
	ptr = nptr;
    }
}

// Since we allow queue's we push all updated events in beginng
// so that clearAllUpdated events will not clear it.
void CExecutable::addUpdatedEvent(CExecutor* aExec, CEvent *aUpdated)
{
#ifdef DEBUG
    for (CEvent* ptr = mUpdatedEvents; ptr; ptr = ptr->getNextUpdated()) {
	if (ptr == aUpdated) 
	    m1BreakHere(__FILE__, __LINE__, (char*) "already updated");
    }
#endif
    aUpdated->setMarked();                     // mark event as in list
    aUpdated->setNextUpdated(mUpdatedEvents);  // link in first
    mUpdatedEvents = aUpdated;
}

// Locate and delete event from updated list
void CExecutable::delUpdatedEvent(CExecutor* aExec, CEvent *aUpdated)
{
    CEvent** pptr = &mUpdatedEvents;
    CEvent*  evt;

    while((evt = *pptr)) {
	if (evt == aUpdated) {
	    *pptr = evt->getNextUpdated();   // unlink
	    evt->clearMarked();              // ordered before clearUpdated!
	    evt->setNextUpdated(NULL);       // clear
	    evt->clearUpdated(aExec);        // may affect EventQueue!!
	    return;
	}
	pptr = evt->getNextUpdatedAddr();       // keep
    }
}

void CExecutable::clrScheduledLater(void)
{
    CEvent*  evt;

    clrFlags(ExecutableInNextQueue);

    // Scan event and clear later flag
    for (evt = mUpdatedEvents; evt != NULL; evt = evt->getNextUpdated())
	evt->clearLater();
}

// Clear list while traverse it from begin to end,
// this implelemtation allows new elements to appear in 
// the beginning of the list while working it's way forward.

void CExecutable::clearAllUpdated(CExecutor* aExec, CEvent* aUntil) 
{
    CEvent** pptr = &mUpdatedEvents;
    CEvent*  evt;

    while((evt = *pptr) && (evt != aUntil)) {
	if (evt->later()) {
#ifdef DEBUG_EVT
	    cerr << "clearAllUpdate: keep " << evt->debugName() << "\n";
#endif
	    // evt->clearLater();
	    pptr = evt->getNextUpdatedAddr();       // keep
	}
	else {
#ifdef DEBUG_EVT
	    cerr << "clearAllUpdate: clear " << evt->debugName() << "\n";
#endif
#ifdef DEBUG
	    evt->tracePropagationOff(); // Turn off propagation debug output.
#endif
	    *pptr = evt->getNextUpdated();   // unlink
	    evt->clearMarked();              // ordered before clearUpdated!
	    evt->setNextUpdated(NULL);       // clear
	    evt->clearUpdated(aExec);        // may affect EventQueue!!
	}
    }
}

//
// Mark events on the updated list as updated
//
void CExecutable::setAllUpdated(CExecutor* aExec) 
{
    CEvent*  evt = mUpdatedEvents;

    while(evt) {
	evt->setUpdated(aExec);
#ifdef DEBUG_EVT
	    cerr << "setAllUpdate: set " << evt->debugName() << "\n";
#endif
	evt = evt->getNextUpdated();
    }
}

void CExecutable::useStructureChanged(void)
{
    if (!mStructureChanged)
	mStructureChanged = new EventBool(this);
}

void CExecutable::setStructureChanged(CExecutor* aExec)
{
    if (mStructureChanged)
	mStructureChanged->assign(aExec, UTrue(), false, TRIGGER_AUTO);
}

void CExecutable::clearStructureChanged(CExecutor* aExec)
{
    if (mStructureChanged) {
	mStructureChanged->cancel(aExec);
    }
}


void CExecutable::stop(CRtExecutor* aExec)
{
    CExecutor* exec = (CExecutor*)aExec;
    type()->destruct(exec, this);
    exec->removeComponent(this);
    clearAllUpdated(exec);
    if (!updatedEventEmpty()) {  // We may have later object
	fprintf(stdout, "clear updated events twice\n");
	clearAllUpdated(exec);
    }
    CBaseObject::stop(aExec);
}


//
//  CField
//
CField::CField(int aStorage, string aName, CType *aType, 
	       const char* aDocumentation, UData aConst, int* aIndexPtr)
{
    if ((aStorage & (Q_PUBLIC|Q_PRIVATE|Q_EXTERN)) == 0)
	aStorage |= Q_PUBLIC;
    mStorage = aStorage;
    mName = aName;
    mType  = m1RetainType(aType);
    mIndex = -1;
    mIndexPtr = aIndexPtr;  // where to update index when mIndex is set
    mStackPos = -1;  // set by linter for local variables
    mDocumentation = aDocumentation;
    mConst.o = NULL; // Must nil here to use setConstant
    setConstant(aConst);
}

CField::CField(const CField &aSource) 
{
    mStorage       = aSource.mStorage;
    mName          = aSource.mName;
    mType          = m1RetainType(aSource.mType);
    mIndex         = aSource.mIndex;
    mIndexPtr      = aSource.mIndexPtr;
    mStackPos      = aSource.mStackPos;
    mDocumentation = aSource.mDocumentation;
    mConst.o       = NULL; // Must nil here to use setConstant
    setConstant(aSource.mConst);
}

CField::~CField(void)
{
    if (mConst.o && mType)
	mType->releaseObject(mConst);
    m1ReleaseType(mType);
    // NOTE: the mDocumentation field is assumed to be a constant string!
}

int CField::mark(Mark_t aMark)
{
    if (mConst.o && mType) {
	return m1MarkAndCheck(aMark, CRuntime, mType) +
	    mType->markObject(mConst, aMark);
    }
    else {
	return m1MarkAndCheck(aMark, CRuntime, mType);
    }
}

int CField::setStorage(int aStorage)
{
    if (mStorage != aStorage) {
	if ((aStorage & (Q_PUBLIC|Q_PRIVATE|Q_EXTERN)) == 0)
	    aStorage |= Q_PUBLIC;
	mStorage = aStorage;
    }
    return mStorage;
}

CType* CField::setType(CType* aType)
{
    m1SetRetainType(&mType, aType);
    return aType;
}

//////////////////////////////////////////////////////////////////////////////
//  CArgs - object constructor arguments
//////////////////////////////////////////////////////////////////////////////

int CArgs::mark(Mark_t aMark) 
{
    return m1MarkAndCheck(aMark, CRuntime, mCreator, mGlobal);
}

//////////////////////////////////////////////////////////////////////////////
//  CType  - Base type
//////////////////////////////////////////////////////////////////////////////

CType::~CType() 
{ 
    STAT_DEC(m1_stat_live_types);
}

string CType::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "Type:%s #%lu 0x%lx", 
	    mName.c_str(), refCount(), (unsigned long) this);
    return string(ndata);
}

//////////////////////////////////////////////////////////////////////////////
//  CObject - Base object
//////////////////////////////////////////////////////////////////////////////

string CObject::debugName(void)
{
    char ndata[256];
    sprintf(ndata, "Object:%s #%lu 0x%lx", 
	    type()->cname(), refCount(), (unsigned long) this);
    return string(ndata);
}

//////////////////////////////////////////////////////////////////////////////
//  Error
//////////////////////////////////////////////////////////////////////////////

CErrorType::CErrorType(void) : CType("error")
{
    m1TypeRegister("error", this); 
}


//////////////////////////////////////////////////////////////////////////////
//  Void
//////////////////////////////////////////////////////////////////////////////

CVoidType::CVoidType(void) : CType("void")
{
    m1TypeRegister("void", this); 
}


//////////////////////////////////////////////////////////////////////////////
//  Any
//////////////////////////////////////////////////////////////////////////////

CAnyType::CAnyType(void) : CType("any")
{
    m1TypeRegister("any", this); 
}

//////////////////////////////////////////////////////////////////////////////
//  Numeric
//////////////////////////////////////////////////////////////////////////////

CNumericType::CNumericType(string aTypeName) : CType(aTypeName)
{
    m1TypeRegister(aTypeName, this);
}

//////////////////////////////////////////////////////////////////////////////
//  Byte  - 0-255
//////////////////////////////////////////////////////////////////////////////

UData CByteType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueByte(aExec->current());
    else
	r.evt = new EventByte(aExec->current());
    return r;
}

void CByteType::print(ostream* os, UData a)
{
    *os << (unsigned int) a.b;
}

//////////////////////////////////////////////////////////////////////////////
//  Char - -128-127
//////////////////////////////////////////////////////////////////////////////
UData CCharType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueChar(aExec->current());
    else
	r.evt = new EventChar(aExec->current());
    return r;
}

void CCharType::print(ostream* os, UData a)
{
    switch(a.c) {
    case '\e': *os << "'\\e'"; break;
    case '\a': *os << "'\\a'"; break;
    case '\b': *os << "'\\b'"; break;
    case '\f': *os << "'\\f'"; break;
    case '\n': *os << "'\\n'"; break;
    case '\r': *os << "'\\r'"; break;
    case '\v': *os << "'\\v'"; break;
    case '\\': *os << "'\\\\'"; break;
    case '\'': *os << "'\\''"; break;
    default:
	if (isprint(a.c)) {
	    char s[4] = {'\'', a.c, '\'', 0};
	    *os << s;
	}
	else {
	    char s[16];
	    sprintf(s, "'\\%03o'", a.c);
	    *os << s;
	}
    }
}

//////////////////////////////////////////////////////////////////////////////
//  Bool - [false-true]
//////////////////////////////////////////////////////////////////////////////

UData CBoolType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueBool(aExec->current());	
    else
	r.evt = new EventBool(aExec->current());
    return r;
}

void CBoolType::print(ostream* os, UData a)
{
    if (a.b)
	*os << "true";
    else
	*os << "false";
}

//////////////////////////////////////////////////////////////////////////////
//  Signed - Signed numbers (32 bit)
//////////////////////////////////////////////////////////////////////////////

UData CSignedType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueSigned(aExec->current());
    else
	r.evt = new EventSigned(aExec->current());
    return r;
}

void CSignedType::print(ostream* os, UData a)
{
    *os << a.i;
}

//////////////////////////////////////////////////////////////////////////////
//  Unsigned - Unsigned numbers (32 bit)
//////////////////////////////////////////////////////////////////////////////

UData CUnsignedType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueUnsigned(aExec->current());	
    else
	r.evt = new EventUnsigned(aExec->current());
    return r;
}

void CUnsignedType::print(ostream* os, UData a)
{
    *os << a.u;
}

//////////////////////////////////////////////////////////////////////////////
//  Float - Floating point numbers (32 bit)
//////////////////////////////////////////////////////////////////////////////

UData CFloatType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueFloat(aExec->current());
    else
	r.evt = new EventFloat(aExec->current());
    return r;
}

void CFloatType::print(ostream* os, UData a)
{
    *os << a.f;
}

//////////////////////////////////////////////////////////////////////////////
//  Time - 32 bit unsigned number
//////////////////////////////////////////////////////////////////////////////

UData CTimeType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueTime(aExec->current());
    else
	r.evt = new EventTime(aExec->current());
    return r;
}

void CTimeType::print(ostream* os, UData a)
{
    *os << a.tm;
}

//////////////////////////////////////////////////////////////////////////////
//  Enum - enumeration
//////////////////////////////////////////////////////////////////////////////

void CEnumerationType::addEnumeration(string aName, int aValue) 
{
    mEnumerations.push_back(CEnumeration(aName, aValue));
}

void CEnumerationType::addEnumerations(CEnumeration* aEnums,size_t aEnumCount) 
{
    for (int i = 0; i < (int) aEnumCount; i++)
	addEnumeration(aEnums[i].name(), aEnums[i].value());
}

bool CEnumerationType::enumName(int aValue, string& aName) 
{
    CEnumerationVector::iterator i = mEnumerations.begin();
    while(i != mEnumerations.end()) {
	if (i->value() == aValue) {
	    aName = i->name();
	    return true;
	}
	i++;
    }
    return false;
}

bool CEnumerationType::enumValue(string aName, int& rValue) 
{
    CEnumerationVector::iterator i = mEnumerations.begin();
    while(i != mEnumerations.end()) {
	if (i->name() == aName) {
	    rValue = i->value();
	    return true;
	}
	i++;
    }
    return false;
}

void CEnumerationType::print(ostream* os) 
{ 
    CEnumerationVector::iterator i = mEnumerations.begin();
    *os << "enum " << name() << " {";
    if (i != mEnumerations.end()) {
	*os << " " << i->name() << "=" << i->value();
	i++;
	while(i != mEnumerations.end()) {
	    *os << "," << i->name() << "=" << i->value();
	    i++;
	}
    }
    *os << "}";
}

void CEnumerationType::print(ostream* os, UData a) 
{
    string nm;
    if (enumName(a.i, nm))
	*os << nm;
    else
	*os << a.i;
}

//////////////////////////////////////////////////////////////////////////////
//  Array - object array
//////////////////////////////////////////////////////////////////////////////

CArrayType::CArrayType(string eTypeName, size_t n)
{
    string name;

    // 
    // Locate the element type that the array will host elements of.
    //
    mElemType = m1TypeLookup(eTypeName);

    //
    // Construct a new array type consisting of n eTypeName elements.
    //

    // Construct a correct string for the new array type.
    name = addDimension(eTypeName, n);
    setName(name);

    m1TypeRegister(name, this);
    mArraySize = n;
}

CArrayType::CArrayType(CType* aElemType, size_t n)
{
    string name;

    name = addDimension(aElemType->name(), n);

    setName(name);
    m1TypeRegister(name, this);
    mArraySize = n;
    mElemType = aElemType;
}

int CArrayType::mark(Mark_t aMark)
{
    return CBaseType::mark(aMark) +
	m1Mark(mElemType, aMark);
}

UData CArrayType::produceEvent(CExecutor* aExec, bool aQueued)
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueObject<CArray*>(aExec->current());	
    else
	r.evt = new EventObject<CArray*>(aExec->current());
    return r;
}

CArrayType* CArrayType::create(CType* aType, size_t n)
{
    string name;
    CType* t;

    name = addDimension(aType->name(), n);
    if ((t = m1TypeLookup(name)) == NULL)
	return m1New(CArrayType, aType, n);
    return dynamic_cast<CArrayType*>(t);
}

void CArrayType::init(CExecutor* aExec, CBaseObject* obj)
{
    int i;
    int n = (int) obj->size();

    DBGFMT("%s::init(%p)",cname(),obj);

    if (isAType(CArrayType*, mElemType) && ((CArrayType*) mElemType)->arraySize() > 0) {
	// only contstruct fixed size array objects (e.g float[3][3])
	for (i = 0; i < n; i++)
	    obj->put(aExec, i, mElemType->produce(aExec));
    }
    else if (mElemType == string_type()) {
	for (i = 0; i < n; i++) {
	    UData s = mElemType->produce(aExec);
	    m1RetainString(s.str);
	    obj->putw(i, s);
	}
    }
    else if (isAType(CEventType*, mElemType)) {
	for (i = 0; i < n; i++)
	    obj->eventPut(aExec, i, (mElemType->produce(aExec)).evt);
    }
    else {
	for (i = 0; i < n; i++)
	    obj->put(aExec, i, nil);
    }
}

void CArrayType::construct(CExecutor* aExec, CBaseObject* obj)
{
    // Not used ...??
}

// Clear mElements by type
void CArrayType::destruct(CExecutor* aExec, CBaseObject* aObject)
{
    CArray* aArray = (CArray*) aObject;
    int n = mArraySize ? mArraySize : aArray->size();
    int i = 0;
    CType* ti = mElemType;

    DBGFMT("%s::destruct(%p)",cname(), aObject);

    while(n--) {
	if (ti->typeTag() == M1TYPE_EVENT)
	    aArray->eventPut(aExec, i, NULL);
	else 
	    aArray->put(aExec, i, nil);
	i++;
    }
}

// This producer is needed to produce default arrays etc 
UData CArrayType::produce(CExecutor* aExec, CBaseType*, CArgs*)
{
    UData r;
    r.arr = m1New(CArray,aExec,this,mElemType->sizeOf(),mArraySize);
    return r;
}

// This producer is called when explicit size is used on dynamic arrays
UData CArrayType::produceArray(CExecutor* aExec, size_t size)
{
    UData r;
    if (mArraySize == 0)
	r.arr = m1New(CArray,aExec, this, mElemType->sizeOf(), size);
    else
	r.arr = m1New(CArray,aExec, this, mElemType->sizeOf(), mArraySize);
    return r;
}

int CArrayType::compare(UData a, UData b)
{
    int i;
    int cmp;
    if (a.arr->size() < b.arr->size()) return -1;
    if (a.arr->size() > b.arr->size()) return  1;
    for (i = 0; i < (int)a.arr->size(); i++) {
	if ((cmp = mElemType->compare(a.arr->at(i), b.arr->at(i))) != 0)
	    return cmp;
    }
    return 0;
}

void CArrayType::shallowCopy(CExecutor* aExec, UData src, UData* dst)
{
    CArray* s = src.arr;
    CArray* d = (*dst).arr;

    if ((s == NULL) && (d == NULL))
	return;
    if (s == NULL) {
	m1ReleaseArray(d);
	*dst = nil;
	return;
    }

    if (d == NULL) {
	*dst = produceArray(aExec, s->size());
	d = (*dst).arr;
	m1RetainArray(d);
    }

    if (d->size() < s->size())
	d->resize(s->size());

    if ((arraySize() > 0) && isAType(CArrayType*, mElemType)) {
	int i;
	for (i = 0; i < (int) s->size(); i++)
	    mElemType->shallowCopy(aExec,s->base()->at(i), &d->base()->at(i));
    }
    else {
	int i;
	for (i = 0; i < (int) s->size(); i++)
	    d->put(aExec, i, s->at(i));	
    }
}

void CArrayType::deepCopy(CExecutor* aExec, UData src, UData* dst)
{
    CArray* s = src.arr;
    CArray* d = (*dst).arr;
    int i;
    
    if ((s == NULL) && (d == NULL))
	return;
    if (s == NULL) {
	m1ReleaseArray(d);
	*dst = nil;
	return;
    }

    if (d == NULL) {
	*dst = produceArray(aExec, s->size());
	d = (*dst).arr;
	m1RetainArray(d);
    }

    if (d->size() < s->size())
	d->resize(s->size());

    for (i = 0; i < (int) s->size(); i++)
	mElemType->deepCopy(aExec, s->base()->at(i), &d->base()->at(i));
}

//
// Format arrays as:
//   { v1, v2, ... vn }
//
void CArrayType::print(ostream* os, UData a)
{
    *os << "{";
    if ((a.o != NULL) && (a.arr->size() > 0)) {
	int i;
	*os << " ";
	mElemType->print(os, a.arr->at((int) 0));
	for (i=1; i<(int)a.arr->size(); i++) {
	    *os << ", ";
	    mElemType->print(os, a.arr->at(i));
	}
    }
    *os << " }";
}

// Print as signed[7]   (e.g element type)
void CArrayType::print(ostream* os)
{
    mElemType->print(os);
    if (arraySize() == 0)
	*os << "[]";
    else
	*os << "[" << arraySize() << "]";
}

//////////////////////////////////////////////////////////////////////////////
//  CArray - methods
//////////////////////////////////////////////////////////////////////////////
void CArray::stop(CRtExecutor* aExec)
{
    CExecutor* exec = (CExecutor*)aExec;
    type()->destruct(exec, this);
    CBaseObject::stop(aExec);
}

void CArray::rotate(int aShift)
{
    UDataVector* vec = base();
    int sz = (int) size();

    if ((sz > 1) && (aShift != 0)) {
	if (aShift < 0) {
	    /* rotate left (from index 1 -> 0) */
	    aShift = (-aShift) % sz;
	    while(aShift--) {
		int i;
		UData save = vec->at(0);
		for (i=0; i<sz-1; i++)
		    vec->at(i) = vec->at(i+1);
		vec->at(sz-1) = save;
	    }
	}
	else {
	    /* rotate right (from index 0 -> 1) */
	    aShift = aShift % sz;
	    while(aShift--) {
		int i;
		UData save = vec->at(sz-1);
		for (i=sz-1; i>0; i--)
		    vec->at(i) = vec->at(i-1);
		vec->at(0) = save;
	    }
	}
    }
}

void CArray::swap(int i, int j)
{
    UDataVector* vec = base();
    int n = ((int) size());

    if ((i >= 0) && (i < n) && (j >= 0) && (j < n)) {
	UData tmp = vec->at(i);
	vec->at(i) = vec->at(j);
	vec->at(j) = tmp;
    }
    // FIXME throw error!
}

void CArray::reverse(void)
{
    UDataVector* vec = base();
    int i  = 0;
    int j = ((int) size())-1;

    while(i < j) {
	UData tmp = vec->at(i);
	vec->at(i) = vec->at(j);
	vec->at(j) = tmp;
	i++;
	j--;
    }
}

//
// Append element from aArray to the array
// NOTE aArray may be this!
//
void CArray::append(CExecutor* aExec, UData aArray)
{
    CArray* a   = aArray.arr;
    int     asz = a->size();   // MUST save here since this may == a!
    int i;
    int j = size();

    resize(size() + asz);
    for (i = 0; i < asz; i++, j++)
	put(aExec, j, a->at(i), TRIGGER_AUTO);
}


void CArray::elementAppend(CExecutor* aExec, UData aElement)
{
    int i = size();
    base()->insert(base()->end(), nil);  // will resize
    put(aExec, i, aElement, TRIGGER_AUTO);
}

void CArray::elementPrepend(CExecutor* aExec, UData aElement)
{
    base()->insert(base()->begin(), nil);  // will resize
    put(aExec, 0, aElement, TRIGGER_AUTO);
}

int CArray::erase(CExecutor* aExec, UData aArray)
{
    CArray* a = aArray.arr;
    int   asz = a->size();
    bool nErased = 0;
    int i;

    if (this == a) { // special case for erase
	for (i = 0; i < asz; i++)
	    // nuke since resize will not (yet)
	    put(aExec, i, nil, TRIGGER_AUTO); 
	resize(0);  
	return asz;
    }
    else {
	for (i = 0; i < asz; i++)
	    if (elementErase(aExec, a->at(i)))
		nErased++;
    }
    return nErased;
}

//
// Find the first instance of aElement in the array 
// (using pointer / primitive compare) and erase the poistion.
//
bool CArray::elementErase(CExecutor* aExec, UData aElement)
{
    vector<UData>::iterator iter = base()->begin();
    for (int i = 0; i < (int) size(); i++, iter++) {
	if (aElement.u == at(i).u) {
	    put(aExec, i, nil, TRIGGER_AUTO);
	    base()->erase(iter);
	    return true;
	}
    }
    return false;
}

//////////////////////////////////////////////////////////////////////////////
//  String - string of characters (0 terminated)
//////////////////////////////////////////////////////////////////////////////

CStringType::CStringType() : CType("string")
{
    m1TypeRegister("string", this);
}

UData CStringType::produce(CExecutor* aExec,CBaseType*, CArgs*)
{
    UData r;
    r.str = m1New(CString);
    return r;
}

UData CStringType::produceEvent(CExecutor* aExec, bool aQueued)
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueString(aExec->current());
    else
	r.evt = new EventString(aExec->current());
    return r;
}

int CStringType::compare(UData a, char* bp, size_t bn)
{
    return a.str->compare(bp, bn);
}

int CStringType::compare(UData a, string str)
{
    return a.str->compare(str);
}

int CStringType::compare(UData a, UData b)
{
    return a.str->compare(b.str);
}

void CStringType::elementInit(CExecutor* aExec, UDataVector *vec, int index)
{
    UData value = string_type()->produce(aExec, NULL,NULL);
    vec->at(index) = value;
    m1RetainObject(value.str);
}

void CStringType::elementPut(CExecutor* aExec, UDataVector *vec, int index,
			     UData value, Trig_t trig) 
{
    CString* str = vec->at(index).str;
    if ((str != NULL) && (value.str != NULL))
	str->set(value.str->str());
    else {
	m1RetainObject(value.str);
	vec->at(index) = value;
	m1ReleaseObject(str);
    }
}

void CStringType::elementCopy(CExecutor* aExec, UDataVector *vec, int index, 
			      UData value, Trig_t trig) 
{
    CString* str = vec->at(index).str;
    if (str == NULL) {
	m1RetainObject(value.str);
	vec->at(index) = value;
    }
    else if (value.str == NULL)
	str->set("");
    else
	str->set(value.str->str());
}


void CStringType::shallowCopy(CExecutor* aExec, UData src, UData* dst)
{
    CString* s = src.str;
    CString* d = (*dst).str;

    if ((s == NULL) && (d == NULL)) 
	return;

    if (s == NULL)
	d->set("");
    else if (d == NULL) {
	d = m1New(CString, s->c_str());
	m1RetainString(d);
	(*dst).str = d;
    }
    else {
	d->set(s->str());
    }
}

void CStringType::deepCopy(CExecutor* aExec, UData src, UData* dst)
{
    shallowCopy(aExec, src, dst);
}
 
//
// Format string:
//   "abcd"
//
void CStringType::print(ostream* os, UData a)
{
    if (a.str == NULL)
	*os << "(null)";
    else {
	string s = a.str->str();
	string::iterator i = s.begin();
	int c;
	*os << "\"";
	for (i=s.begin(); i != s.end(); i++) {
	    switch(c=*i) {
	    case '\e': *os << "\\e"; break;
	    case '\a': *os << "\\a"; break;
	    case '\b': *os << "\\b"; break;
	    case '\f': *os << "\\f"; break;
	    case '\n': *os << "\\n"; break;
	    case '\r': *os << "\\r"; break;
	    case '\t': *os << "\\t"; break;
	    case '\v': *os << "\\v"; break;
	    case '\'': *os << "\\'"; break;
	    case '\\': *os << "\\"; break;
	    case '"':  *os << "\\\""; break;
	    default:
		if (isprint(c)) {
		    char s[2] = {(char) c,0};
		    *os << s;
		}
		else {
		    ios_base::fmtflags reset = 
			os->setf( ios_base::oct, ios_base::basefield);
		    *os << "\\'" << c << "'";
		    os->flags(reset);
		}
		break;
	    }
	}
    }
    *os << "\"";
}

//////////////////////////////////////////////////////////////////////////////
// Event -   input/output types
//////////////////////////////////////////////////////////////////////////////

CEventType::CEventType(CType* aType, bool aQueued, int aDirection)
{
    string typeName;

    mDirection = aDirection & E_INOUT;

    switch(mDirection) {
    case E_INPUT: typeName = aQueued?"input event queue ":"input event "; break;
    case E_OUTPUT: typeName = "output event "; break;
    case E_INOUT: typeName = aQueued?"event queue ": "event "; break;
    default: break;
    }
    typeName += aType->name();
    mBaseType = m1RetainType(aType);
    mQueued = aQueued;
    setName(typeName);
    m1TypeRegister(typeName, this);
}

CEventType::CEventType(string aName, bool aQueued, int aDirection)
{
    string typeName;

    mDirection = aDirection & E_INOUT;
    mBaseType = m1TypeLookup(aName);

    switch(mDirection) {
    case E_INPUT:  typeName=aQueued ?"input event queue ":"input event "; break;
    case E_OUTPUT: typeName="output event "; break;
    case E_INOUT:  typeName=aQueued ? "event queue ": "event "; break;
    default: break;
    }
    typeName += aName;
    setName(typeName);
    mQueued = aQueued;
    m1TypeRegister(typeName, this);
}


CType* CEventType::createType(CType* aType, bool aQueued, int aDirection)
{
    string eName;
    CType* t;

    switch(aDirection & E_INOUT) {
    case E_INPUT:  
	eName=(aQueued ? "input event queue ": "input event "); 
	break;
    case E_OUTPUT: 
       eName="output event "; 
	break;
    case E_INOUT:
	eName=(aQueued ? "event queue ": "event ");
	break;
    default: break;
    }

    t = m1TypeFmtLookup((char*)"%s%s", eName.c_str(), aType->cname());
    if (t == NULL)
	return m1New(CEventType, aType, aQueued, aDirection);
    return t;
}

CType* CEventType::create(CType* aType, int aDirection)
{
    return createType(aType, false, aDirection);
}


CType* CEventType::createQueue(CType* aType, int aDirection)
{
    return createType(aType, true, aDirection);
}

string CEventType::typeSignature(void) 
{
    string baseSig = mBaseType->typeSignature();
    switch(mDirection) {
    case E_INPUT: return mQueued ? baseSig + "R" : baseSig + "I";
    case E_OUTPUT:return baseSig + "O";
    case E_INOUT: return mQueued ? baseSig + "Q" : baseSig + "E";
    default: return baseSig;
    }
}

string CEventType::typeName(void) 
{
    string baseName = mBaseType->typeName();
    switch(mDirection) {
    case E_INPUT: return (mQueued ? "input event queue" : "input event ") +
	    baseName;
    case E_OUTPUT: return "output event " + baseName;
    case E_INOUT: return   (mQueued ? "event queue" : "event ") +
	    baseName;
    default: return baseName;
    }
}

void CEventType::elementInit(CExecutor* aExec,UDataVector *vec, int index)
{
    UData value = mBaseType->produceEvent(aExec, mQueued);
    vec->at(index) = value;
    value.evt->setIndex(index); // For debugging etc
    m1RetainObject(value.o);
}

UData CEventType::elementAt(UDataVector *vec, int index)
{
    CEvent* evt = vec->at(index).evt;
    return evt->uvalue();
}

void CEventType::elementPut(CExecutor* aExec,UDataVector *vec,int index,
			     UData value,Trig_t trig) 
{
    (vec->at(index).evt)->assign(aExec,value,true,trig);
}

void CEventType::elementCopy(CExecutor* aExec, UDataVector *vec,int index,
			     UData value,Trig_t trig) 
{
    (vec->at(index).evt)->assign(aExec,value,true,trig);
}


void CEventType::print(ostream* os, UData a)
{
    mBaseType->print(os, a);
}

void CEventType::print(ostream* os)
{
    switch(mDirection & E_INOUT) {
    case E_INPUT:  
	*os << (mQueued ? "input event queue ":"input event ");
	break;
    case E_OUTPUT:
	*os << "output event "; 
	break;
    case E_INOUT:
	*os << (mQueued ? "event queue ":"event "); 
	break;
    default: 
	break;
    }
    mBaseType->print(os);
}

//////////////////////////////////////////////////////////////////////////////
// CEvent
//////////////////////////////////////////////////////////////////////////////


int CEvent::mark(Mark_t aMark) 
{
    if (mSource)
	m1Mark(mSource->mOwner, aMark);
    return 0;
}

void CEvent::setUpdated(CExecutor*)
{
    mUpdate |= UpdateStatusYes;
}

// Clear both updated and Assigned(mask)
void CEvent::clearUpdated(CExecutor*)
{
    mUpdate &= ~(UpdateStatusYes|UpdateStatusAssigned);
}

// Check if we are already on the owner updated list
bool CEvent::marked(void) 
{
    return ((mUpdate & UpdateStatusMark) != 0);
}

// Mark the event as beeing on the owner updated list
void CEvent::setMarked(void)
{
    mUpdate |= UpdateStatusMark;
}

void CEvent::clearMarked(void)
{
    mUpdate &= ~UpdateStatusMark;
}

// Check if event is marked to stay on the updated list
bool CEvent::later(void) 
{
    return ((mUpdate & UpdateStatusLater)!= 0);
}

void CEvent::setLater(void)
{
    mUpdate |= UpdateStatusLater;
}

void CEvent::clearLater(void)
{
    mUpdate &= ~UpdateStatusLater;
}

// Clear update / assignments and remove owner from update list
void CEvent::cancel(CExecutor* aExec)
{
    if (marked()) {
	if (mOwner)
	    mOwner->delUpdatedEvent(aExec, this);
	else
	    mUpdate = UpdateStatusNo;
    }
}


//
// Print event ObjectClass.FieldName[UML] 
//
string CEvent::debugName(void)
{
    string s;
    if (mOwner) {
	CBaseType* bt = (CBaseType*) mOwner->type();
	s = bt->name() + ".";
	if (mIndex >= 0)
	    s += bt->field(mIndex)->name();
	else
	    s += "no_index";
    }
    else
	s = "no_owner.no_index";
    s += "[";
    if (mUpdate & UpdateStatusYes)
	s += "U";
    if (mUpdate & UpdateStatusMark)
	s += "M";
    if (mUpdate & UpdateStatusLater)
	s += "L";
    if (mUpdate & UpdateStatusLocal)
	s += "l";
    if (mUpdate & UpdateStatusRemote)
	s += "r";
    if (mUpdate & UpdateStatusPropagate)
	s += "p";
    s += "]";
    return s;
}

void CEvent::printValue(ostream* os)
{
    if (mOwner) {
	CBaseType* bt = (CBaseType*) mOwner->type();
	if (mIndex >= 0)
	    bt->field(mIndex)->type()->print(os, mValue);
	else
	    *os << "no-index";
    }
    else
	*os << "no-owner";
}


UData CEvent::assign(CExecutor* aExec, UData aNewValue,
		     bool aSetSender, Trig_t aTrig)
{
    // Use system current executor if not set (FIXME)
    if (!aExec) aExec = m1_system().executor();
#ifdef DEBUG
    if (aSetSender)
	setSender(aExec->eventSourceFile(), aExec->eventSourceLine());
#endif
    setValue(aNewValue);
    if (mOwner) {
	switch(aTrig) {
	case TRIGGER_NO:
	    break;
	case TRIGGER_YES:
	    if (!mOwner->executeNever()) {
		if (!marked())
		    mOwner->addUpdatedEvent(aExec, this);
		if (!mOwner->executeEverySweep())
		    mUpdate |= aExec->schedule(mOwner);
	    }
	    break;
	case TRIGGER_PROPAGATE:
	    mUpdate |= UpdateStatusPropagate;
	    /* do the TRIGGER_AUTO case */
	case TRIGGER_AUTO:
	    if (!mOwner->executeNever()) {
		if (!marked())
		    mOwner->addUpdatedEvent(aExec, this);
		if ((aExec->current() != mOwner) && 
		    !mOwner->executeEverySweep())
		    mUpdate |= aExec->schedule(mOwner);
	    }
	    break;
	}
    }

    // Updated is postponed until execute
    if (aExec->current() == mOwner)
	mUpdate |= UpdateStatusLocal;
    else
	mUpdate |= UpdateStatusRemote;

#ifdef DEBUG_EVT
    cerr << "assigned: " << debugName() << " = [";
    printValue(&cerr);
    cerr << "]\n";
#endif
    return aNewValue;
}

// Propagate value 1-step (to direct subscribers)
void CEvent::propagate(CExecutor* aExec)
{
#ifdef DEBUG
    if (tracePropagation()) 
	cerr << "propagated: " << debugName() << "\n";
#endif

    for (CEvent* ptr = mSubscribers; ptr; ptr = ptr->mNext) {
#ifdef DEBUG
	ptr->setSender(senderFile(), senderLine());
	if (tracePropagation()) {
	    cerr << "  propagating to: " << ptr->debugName() << "\n";
	    ptr->tracePropagationOn();
	}
#endif
	ptr->assign(aExec,mValue,false,TRIGGER_PROPAGATE);
    }
}

// Disconnect all subscribers
void CEvent::disconnect(void) 
{
    CEvent* ptr = mSubscribers;
    while(ptr) {
	CEvent* nptr = ptr->mNext;
	ptr->setSource(NULL);
	ptr->mNext = NULL;
	ptr = nptr;
    }
    mSubscribers = NULL;
}

void CEvent::deleteSubscriber(CEvent* aEvt)
{
    CEvent** pevt = &mSubscribers;
	
    while(*pevt && (*pevt != aEvt))
	pevt = &(*pevt)->mNext;
    if (*pevt) {
	aEvt->setSource(NULL);
	*pevt = aEvt->mNext;
	aEvt->mNext = NULL;
    }
}

void CEvent::addSubscriber(CEvent* aEvt) 
{
    if (aEvt->source() != this) {
	aEvt->deleteFromSource();     // delete from old source
	aEvt->setSource(this);        // set new source
	aEvt->mNext = mSubscribers;
	mSubscribers = aEvt;
	aEvt->setValue(mValue); // a value change should? trigger
    }
}


void CEvent::setSource(CEvent* aSource) 
{
    if (aSource) m1Retain(CExecutable, aSource->owner());
    if (mSource) m1Release(CExecutable, mSource->owner());
    mSource = aSource;
}

//
// Count number of subsribers (debugging only)
//
size_t CEvent::subscriberCount(void)
{
    size_t n = 0;
    CEvent* ptr = mSubscribers;
    while(ptr) {
	n++;
	ptr = ptr->mNext;
    }
    return n;
}

//////////////////////////////////////////////////////////////////////////////
// CEventQueue
//////////////////////////////////////////////////////////////////////////////

UData CEventQueue::assign(CExecutor* aExec, UData aValue, 
			  bool aSetSender, Trig_t aTrig) 
{
    if (!assigned()) {
	return CEvent::assign(aExec,aValue,aSetSender,aTrig);
    }
    else {
	QItem qi;
	if (mQueue.size() >= mMaxSize) {
	    switch(mPolicy) {
	    case EventQueuePolicyDropFront:
		// Do not enter queue - warning?
		return aValue;
	    case EventQueuePolicyDropRear:
		releaseValue(mQueue.front().mValue);
		mQueue.pop();
		break;
	    case EventQueuePolicyStore:
	    case EventQueuePolicyBlock:
		break;
	    }
	}
	qi.mTime = m1_timeStamp();
	qi.mValue = aValue;
#ifdef DEBUG
	if (aExec != 0) {
	    qi.mSenderFile = aExec->eventSourceFile();
	    qi.mSenderLine = aExec->eventSourceLine();
	}
#endif
	retainValue(aValue);   // retain value in queue
	mQueue.push(qi);
	return aValue;
    }
}

void CEventQueue::clearUpdated(CExecutor* aExec) 
{
    CEvent::clearUpdated(aExec);
    if (!mQueue.empty()) {
	UData value = mQueue.front().mValue;
#ifdef DEBUG
	setSender(mQueue.front().mSenderFile,
		  mQueue.front().mSenderLine);
#endif
	CEvent::assign(aExec, value, false, TRIGGER_YES);
	releaseValue(value);  // release the queue reference (after assign!!!)
	mQueue.pop();
    }
}



//////////////////////////////////////////////////////////////////////////////
// CBaseType
//////////////////////////////////////////////////////////////////////////////

CBaseType::CBaseType(string aName) :
    CType(aName), 
    mFieldOffset(0),
    mFieldCount(0),
    mParent(NULL),
    mSubTypes(NULL),
    mGlobalType(NULL),
    mTypeLevel(-1),
    mSingleton(NULL)
{
    m1TypeRegister(aName, this);
}

CBaseType::CBaseType() : 
    CType(), 
    mFieldOffset(0),
    mFieldCount(0),
    mParent(NULL),
    mSubTypes(NULL),
    mGlobalType(NULL),
    mTypeLevel(-1),
    mSingleton(NULL)
{
}

CBaseType::~CBaseType() 
{
    int i;
    m1ReleaseType(mParent);
    m1ReleaseBaseType(mSubTypes);
    m1ReleaseBaseType(mGlobalType);
    m1ReleaseBaseObject(mSingleton);
    for (i = 0; i < (int) mFields.size(); i++)
	m1Release(CField, mFields[i]);
}

int CBaseType::mark(Mark_t aMark)
{
    int marked = 0;
    if (aMark != mMark) {
	int i;
	marked += CType::mark(aMark);
	marked += m1Mark(mParent, aMark);
	marked += m1Mark(mSubTypes, aMark);
	marked += m1Mark(mGlobalType, aMark);
	marked += m1Mark(mSingleton, aMark);
	// Scan trough all fields
	for (i = 0; i < (int)mFields.size(); i++)
	    marked += mFields[i]->mark(aMark);
    }
    return marked;
}

// Setup singleton reference
void CBaseType::setSingleton(CBaseObject* aObject)
{
    m1SetRetain(CBaseObject, &mSingleton, aObject);
}

void CBaseType::elementPut(CExecutor* aExec, UDataVector *vec, int index, 
			   UData value, Trig_t trig) 
{
    CObject* t = vec->at(index).o;
    m1RetainObject(value.o);
    vec->at(index) = value;
    m1ReleaseObject(t);
}

void CBaseType::elementCopy(CExecutor* aExec, UDataVector *vec, int index,
			    UData value, Trig_t trig) 
{
    shallowCopy(aExec, value, &vec->at(index));
}


// Initialize mElements by type
void CBaseType::init(CExecutor* aExec, CBaseObject* aObject)
{
    int i = fieldTypeOffset();
    int n = fieldTypeCount();

    DBGFMT_EVAL("CBaseType::%s::init(%p)",
		cname(), aObject);
    if (mParent) mParent->init(aExec, aObject);

    while(n--) {
	CType* ti = typeAt(i);

	DBGFMT_EVAL("%s::init: field %s index=%d", 
		    cname(), fieldName(i), i);
	switch(ti->typeTag()) {
	case M1TYPE_OBJECT:
	case M1TYPE_BYTE:
	case M1TYPE_CHAR:
	case M1TYPE_BOOL:
	case M1TYPE_SIGNED:
	case M1TYPE_UNSIGNED:
	case M1TYPE_FLOAT:
	    aObject->putw(i, nil);
	    break;

	case M1TYPE_ARRAY: {
	    UData array = ti->produce(aExec);
	    m1RetainObject(array.o);
	    aObject->putw(i, array);
	    break;
	}

	case M1TYPE_STRING: {
	    UData s = ti->produce(aExec);
	    m1RetainString(s.str);
	    aObject->putw(i, s);
	    break;
	}

	case M1TYPE_EVENT: {
	    CEventType* tie = (CEventType*) ti;
	    UData e = ti->produceEvent(aExec, tie->isQueued());
	    aObject->putw(i, e);
	    e.evt->setIndex(i);
	    break;
	}

	default:
	    DBGFMT_WARN("can not initialize field %s", fieldName(i));
	    break;
	}
	i++;
    }
}

// Setup default values
void CBaseType::defaults(CExecutor* aExec, CBaseObject* aObject)
{
    if (mParent) mParent->defaults(aExec, aObject);
}

// Run field setup (after defaults)
void CBaseType::construct(CExecutor* aExec, CBaseObject* aObject)
{
    if (mParent) mParent->construct(aExec, aObject);
}

// Clear mElements by type
void CBaseType::destruct(CExecutor* aExec, CBaseObject* aObject)
{
    int i = fieldTypeOffset();
    int n = fieldTypeCount();

    DBGFMT("CBaseType::destruct(%p)", aObject);

    while(n--) {
	CType* ti = typeAt(i);
	DBGFMT_EVAL("%s::destruct: field %s index=%d",cname(),fieldName(i),i);
	switch(ti->typeTag()) {
	case M1TYPE_EVENT:
	    aObject->eventPut(aExec, i, NULL);
	    break;
	case M1TYPE_STRING:
	case M1TYPE_OBJECT:
	case M1TYPE_ARRAY:
	    // FIXME: combine this
	    ti->releaseObject(aObject->at(i));
	    aObject->putw(i, nil);
	    break;
	default:
	    break;
	}
	i++;
    }
    if (mParent) mParent->destruct(aExec, aObject);
}

//
// Search for field by name MUST search backwards to get
// fields from subtypes before parent types
//
CField *CBaseType::field(string aFieldName)
{
    int i;

    for (i = ((int) mFields.size())-1; i >= 0; i--) {
	if (mFields[i]->name() == aFieldName)
	    return mFields[i];
    }
    return NULL;
}


CType *CBaseType::fieldType(string aFieldName)
{
    CField *res = field(aFieldName);

    if (res)
	return res->type();
    return NULL;
}

int CBaseType::indexAt(string aFieldName) 
{
    CField *res = field(aFieldName);
    if (res)
	return res->index();
    return -1;
}

CField* CBaseType::addField(CField* aField) 
{
    size_t nextIndex = mFields.size();
    setupFields(aField, 1);
    // return address to the stored field (not aField!)
    return field(nextIndex);
}

CField* CBaseType::updateField(CField* aField) 
{
    CField* fld;
    if ((fld = field(aField->name())) != NULL) {
	fld->setType(aField->type());
	fld->setStorage(aField->storage());
	return fld;
    }
    return addField(aField);
}

void CBaseType::addSubType(int aStorage, string aName, CType* aType)
{
    if (!mSubTypes) {
	mSubTypes = m1New(CBaseType);
	m1RetainBaseType(mSubTypes);
    }
    // addField MUST run before setScope !!! FIXME (ugly)
    mSubTypes->addField(aStorage, aName, aType);
    if (aType)
	aType->setScope(this);
}

void CBaseType::addSubType(CType* aType)
{
    addSubType(Q_PUBLIC, aType->name(), aType);
}

void CBaseType::updateSubType(int aStorage, string aName, CType* aType)
{
    if (!mSubTypes) {
	mSubTypes = m1New(CBaseType);
	m1RetainBaseType(mSubTypes);
    }
    mSubTypes->updateField(aStorage, aName, aType);
    if (aType)
	aType->setScope(this);
}

void CBaseType::updateSubType(CType* aType)
{
    updateSubType(Q_PUBLIC, aType->name(), aType);
}


UData CBaseType::produceEvent(CExecutor* aExec, bool aQueued)
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueObject<CBaseObject*>(aExec->current());
    else
	r.evt = new EventObject<CBaseObject*>(aExec->current());
    return r;
}

void CBaseType::shallowCopy(CExecutor* aExec, UData src, UData* dst)
{
    CObject* s = src.o;
    CObject* d = (*dst).o;
    int i;

    if ((s == NULL) && (d == NULL))
	return;
    if (s == NULL) {
	m1ReleaseObject(d);
	*dst = nil;
	return;
    }

    if (d == NULL) {
	*dst = produce(aExec, this);
	d = (*dst).o;
	m1RetainObject(d);
    }
    for (i = 0; i < (int)mFields.size(); i++)
	d->put(aExec, i, s->at(i));
}

void CBaseType::deepCopy(CExecutor* aExec, UData src, UData* dst)
{
    CObject* s = src.o;
    CObject* d = (*dst).o;

    if ((s == NULL) && (d == NULL))
	return;
    if (s == NULL) {
	m1ReleaseObject(d);
	*dst = nil;
	return;
    }

    if (d == NULL) {
	*dst = produce(aExec, this);
	d = (*dst).o;
	m1RetainObject(d);
    }
    if (isAType(CBaseObject*, s) && isAType(CBaseObject*, d)) {
	int i;
	for (i = 0; i < (int)mFields.size(); i++)
	    mFields[i]->type()->deepCopy(aExec,
					 ((CBaseObject*)s)->base()->at(i), 
					 &((CBaseObject*)d)->base()->at(i));
    }
    else {
	int i;
	for (i = 0; i < (int)mFields.size(); i++)
	    d->put(aExec, i, s->at(i));
    }
}


int CBaseType::compare(UData a, UData b)
{
    int cmp;
    int i;

    for (i = 0; i < (int)mFields.size(); i++) {
	if ((cmp = mFields[i]->type()->compare(a.o->at(i),b.o->at(i))) != 0)
	    return cmp;
    }
    return 0;
}

//!
// Add fields incrementally by resizing the mFields vector
//
void CBaseType::setupFields(CField *aFields, size_t aFieldCount)
{
    int i,j;

    // Check for null values as present in NULL_FIELD
    if ((aFieldCount == 1) && (aFields[0].type() == NULL)) {
	mFieldCount = 0;
	return;
    }

    // Save field count as current type fieldCount
    mFieldOffset = mFields.size();   // start of fields
    mFieldCount = aFieldCount;       // number of new fields

    // FIXME: remove constant fields - they should NOT allocate space
    i = mFields.size();
    for (j=0; j < (int)aFieldCount; j++) {
	CField* fld = new CField(aFields[j]);
	mFields.push_back(fld);   // add new slot
	m1Retain(CField, fld);    // retain in vector
	mFields[i]->setIndex(i);  // index in overall (derived) class
	DBGFMT_LINT("%s: Loaded field [%s] type [%s]"
		    " Will be stored in mElement[%d]", 
		    cname(), 
		    mFields[i]->cname(), 
		    mFields[i]->type()->cname(), 
		    i);
	i++;
    }
}

//
// Copy field to target base type
//
void CBaseType::copyFields(CBaseType* aTarget, CType* (*aTranslateType)(CType*))
{
    int i;

    DBGFMT_LINT("CBaseType::copyFields from=%s to=%s", 
	   cname(), aTarget->cname());
    for (i = 0; i < (int) mFields.size(); i++) {
	CField* fld = mFields[i];
	aTarget->mFields.push_back(fld);
	m1Retain(CField, fld);
	if (aTranslateType) {
	    CField* fld = aTarget->field(i);
	    fprintf(stderr, "FIXME: copy field!!!\n");
	    fld->setType(aTranslateType(fld->type()));
	}
    }
    // After a copy we move the offset after all other fields
    aTarget->setFieldTypeOffset(mFields.size());
    aTarget->setFieldTypeCount(0);
}

//
// Load object field declaration
//
u_int8_t* CBaseType::loadFields(u_int8_t* ptr,u_int8_t* ptr_end)
{
    int fieldCount;
    int    i, j;

    fieldCount = VM_GET_U32(ptr);
    ptr += 4;

    j  = 0;
    i  = mFields.size();
    mFieldOffset = i;         // this is where local fields start
    mFieldCount = fieldCount; // this many fields will be defined

    while((ptr < ptr_end) && (j < fieldCount)) {
	CField* fld;
	CType* fieldType;
	string fieldName;
	string typeSig;
	int storage;

	if (ptr + sizeof(unsigned int)+1 > ptr_end) return NULL;
	storage = *ptr++;

	if ((ptr=load_string(ptr, ptr_end, &fieldName))==NULL)
	    return NULL;

	if ((ptr=load_string(ptr, ptr_end, &typeSig))==NULL)
	    return NULL;
	fieldType = typeFromSignature(typeSig);
	fld = new CField(storage,fieldName,fieldType);
	mFields.push_back(fld);
	m1Retain(CField, fld);
	mFields[i]->setIndex(i);

	DBGFMT("CBaseType::loadFields(%s): "
	       "Loaded field [%s] type [%s] at mElements[%d]", 
	       cname(), fieldName.c_str(), mFields[i]->type()->cname(), 
	       i);
	i++;
	j++;
    }
    return ptr;
}

//
// Format object as:
//   Name { a=1, b=2, ... , z=3 }
//   Name {}
//

//////////////////////////////////////////////////////////////////////////////
//  printElement
//////////////////////////////////////////////////////////////////////////////
void printElement(ostream* os, CType* type, CObject* object, int ix)
{
    if (isAType(CEventType*, type)) {
	*os << "<-";
	type->print(os, object->at(ix));
    }
    else {
	*os << "=";
	type->print(os, object->at(ix));
    }
}


void CBaseType::print(ostream* os, UData a)
{
    unsigned int n = mFields.size();

    *os << name() <<  " {";
    if ((a.o != NULL) && (n > 0)) {
	unsigned int i;
	*os << " " << mFields[0]->name();
	printElement(os, mFields[0]->type(), a.o, 0);
	for (i=1; i<n; i++) {
	    *os << ", " << mFields[i]->name();
	    printElement(os, mFields[i]->type(), a.o, i);
	}
    }
    *os << " }";
}

// print as [type|library] <name> { (<field> ;) * }
void CBaseType::print(ostream* os)
{
    unsigned int i;

    if (isLibrary()) 
	*os << "library ";
    else if (isApplication())
	*os << "application ";
    else
	*os << "type ";
    
    *os << name() << "{ ";
    for (i=0; i<mFields.size(); i++) {
	mFields[i]->type()->print(os);
	*os << mFields[i]->name() << "; ";
    }
    *os << "}";
}

//////////////////////////////////////////////////////////////////////////////
//
// CExecutableType
//
//////////////////////////////////////////////////////////////////////////////

void CExecutableType::execute(CExecutor* aExec, CExecutable* aObject)
{
    CType* pt;

    DBGFMT_EVAL("CExecutableType::execute() %s", cname());

    aObject->execute(aExec);

    if ((pt = parentType()) && pt->isExecutableType())
	((CExecutableType*) pt)->execute(aExec, aObject);
}

void CExecutableType::destruct(CExecutor* aExec, CBaseObject* aObject)
{
    DBGFMT("CExecutableType::destruct(%p)", aObject);
    CBaseType::destruct(aExec, aObject);
}

//////////////////////////////////////////////////////////////////////////////
// CBindingType
//////////////////////////////////////////////////////////////////////////////

UData CBindingType::produce(CExecutor* aExec, CBaseType*, CArgs* args) 
{
    ExecutorState state;
    UData r;

    // Since CBindingType is now executable we must save/restore 
    aExec->save(&state);

    r.o = m1New(CBindingObject, aExec, this, fieldCount());
    if (args) args->run(aExec, r.o);  // future extension ?

    aExec->restore(&state);
    return r;
}

void CBindingType::init(CExecutor* aExec, CBaseObject* aObj)
{
    DBGFMT("%s::init(%p)", cname(), aObj);

    CExecutableType::init(aExec, aObj);  // init me
}


void CBindingType::construct(CExecutor* aExec, CBaseObject* aObj)
{
    DBGFMT("%s::construct(%p)", cname(), aObj);

    CExecutableType::construct(aExec, aObj);  // construct me
}


void CBindingType::defaults(CExecutor* aExec, CBaseObject* aObj)
{
    DBGFMT("%s::defaults(%p)", cname(), aObj);

    CExecutableType::defaults(aExec, aObj);  // default me
}

void CBindingType::destruct(CExecutor* aExec, CBaseObject* aObj)
{
    DBGFMT("CBindingType::destruct(%p)", aObj);
    CExecutableType::destruct(aExec, aObj);
}

//////////////////////////////////////////////////////////////////////////////
// CBindingObject
//////////////////////////////////////////////////////////////////////////////
void CBindingObject::stop(CRtExecutor* aExec)
{
    CExecutor* exec = (CExecutor*) aExec;
    type()->destruct(exec, this);
    CExecutable::stop(aExec);
}

//////////////////////////////////////////////////////////////////////////////
// CString
//////////////////////////////////////////////////////////////////////////////

string CString::debugName(void)
{
    char ndata[512];
    sprintf(ndata, "String: [%s] #%lu 0x%lx", 
	    mString.c_str(),refCount(),(unsigned long) this);
    return string(ndata);
}

//////////////////////////////////////////////////////////////////////////////
// VmObjectType
//////////////////////////////////////////////////////////////////////////////

VmObjectType::VmObjectType(string aName,Definition_t aDef) : CExecutableType(aName) 
{
    mDefinitionType = aDef;
    mScope = NULL;
    mConstructorFrameSize = 0;
    mDestructorFrameSize = 0;
}

VmObjectType::VmObjectType(Definition_t aDef) : CExecutableType() 
{
    mDefinitionType = aDef;
    mScope = NULL;
    mConstructorFrameSize = 0;
    mDestructorFrameSize = 0;
}

VmObjectType::~VmObjectType() 
{
    m1ReleaseType(mScope);
}

int VmObjectType::mark(Mark_t aMark)
{
    if (mMark == aMark) 
	return 0;
    else {
	VmObjectType* aSingleton = singleton();
	return CExecutableType::mark(aMark) +
	    m1Mark(mScope, aMark) +
	    m1Mark(aSingleton, aMark);
    }
}

UData VmObjectType::produce(CExecutor* aExec,CBaseType* aBase, CArgs* args)
{
    ExecutorState state;
    UData r;

    aExec->save(&state);

    r.o = m1New(VmObject, aExec, aBase ? aBase : this);
    if (args) args->run(aExec, r.o);

    aExec->restore(&state);
    return r;
}

void VmObjectType::setScope(CType* aScope)
{
    m1SetRetainType(&mScope, aScope);
}

string VmObjectType::scopeName(void) 
{
    CType* st = mScope;
    string scopeName = "";

    while(st != NULL) {
	string tmpName = st->name();
	if (tmpName == "_FileScope")
	    return name();  // Just return the type name
	else if (tmpName == "_Contexts")
	    return ":"+scopeName+":"+name();
	else if (scopeName == "")
	    scopeName = tmpName;
	else
	    scopeName = tmpName + ":" + scopeName;
	st = st->scope();
    }
    return scopeName;
}

// Check if the type respresents a library
bool VmObjectType::isLibrary(void)
{
    return (mDefinitionType == T_LIBRARY);
}

// Check if the type respresents an application
bool VmObjectType::isApplication(void)
{
    return (mDefinitionType == T_APPLICATION);
}

//////////////////////////////////////////////////////////////////////////////
// VmObject
//////////////////////////////////////////////////////////////////////////////
VmObject::VmObject(CExecutor* aExec, CBaseType* aType) : 
    CExecutable(aExec, aType)
{
    setFlags(ExecuteOnEventUpdate);
}

VmObject::~VmObject()
{
    if (type()->isApplication())
	m1_applicationList.remove(this);
}

//////////////////////////////////////////////////////////////////////////////
// VmInterfaceType
//////////////////////////////////////////////////////////////////////////////

VmInterfaceType::VmInterfaceType(string aName,Definition_t aDef) :
    VmObjectType(aName,aDef) 
{
}

VmInterfaceType::~VmInterfaceType() 
{
}

// setup a link to derived classes, needed for resolve
void VmInterfaceType::setChild(CType* aChildType)
{
    if (isAType(CBaseType*, aChildType))
	// FIXME: check duplicates?
	derivedTypes.push_back((CBaseType*)aChildType);
}

UData VmInterfaceType::produce(CExecutor* aExec,CBaseType* aBase, CArgs* args)
{
    fprintf(stderr, "Can not create objects from interface %s\n",
	    name().c_str());
    exit(1);
    return nil;
}

void VmInterfaceType::execute(CExecutor* aExec, CExecutable *aObject)
{
    fprintf(stderr, "Can not execute objects of interface %s\n",
	    name().c_str());
    exit(1);
}

void VmInterfaceType::print(ostream* os, UData a)
{
    print(os);
}

void VmInterfaceType::print(ostream* os)
{
    *os << "interface ";
    VmObjectType::print(os);
}



//////////////////////////////////////////////////////////////////////////////
// VmMachineType
//////////////////////////////////////////////////////////////////////////////

VmMachineType::VmMachineType(string aName,Definition_t aDef) : VmObjectType(aName,aDef)
{
    exec(NULL, NULL, true);

    mPreScript   = NULL;
    mPostScript  = NULL;
    mConstructor = NULL;
    mDestructor  = NULL;
    mDefaults    = NULL;
    S_size      = 0;
    R_size      = 0;
    N_size      = 0;
}

VmMachineType::VmMachineType(Definition_t aDef) : VmObjectType(aDef)
{
    exec(NULL, NULL, true);

    mPreScript   = NULL;
    mPostScript  = NULL;
    mConstructor = NULL;
    mDestructor  = NULL;
    mDefaults    = NULL;
    S_size      = 0;
    R_size      = 0;
    N_size      = 0;
}

VmMachineType::~VmMachineType()
{
    int i;

    if (mPreScript != NULL)  delete [] mPreScript;
    if (mPostScript != NULL)  delete [] mPostScript;
    if (mConstructor != NULL)  delete [] mConstructor;
    if (mDestructor != NULL)  delete [] mDestructor;
    if (mDefaults != NULL)  delete [] mDefaults;

    for (i = 0; i < (int) cnst.size(); i++)
	cnst[i].o->releaseThis();
}

void VmMachineType::init(CExecutor* aExec, CBaseObject* obj)
{
    DBGFMT("%s::init(%p)", cname(), obj);
    
    VmObjectType::init(aExec, obj);  // initialize me
}

void VmMachineType::defaults(CExecutor* aExec, CBaseObject* obj)
{
    CBaseObject* savedGlobal;
    DBGFMT("%s::defaults(%p)", cname(), obj);

    VmObjectType::defaults(aExec, obj);
    aExec->activate_global(global(), &savedGlobal);
    exec(aExec, mDefaults, false);
    aExec->restore_global(savedGlobal);
}

void VmMachineType::construct(CExecutor* aExec, CBaseObject* obj)
{
    CBaseObject* savedGlobal;
    DBGFMT("%s::construct(%p)", cname(), obj);
    
    VmObjectType::construct(aExec, obj);  // construct me

    // run machine contructor code
    aExec->activate_global(global(), &savedGlobal);
    exec(aExec, mConstructor, false);
    aExec->addComponent((CExecutable*)obj);
    aExec->restore_global(savedGlobal);
}

void VmMachineType::destruct(CExecutor* aExec,CBaseObject* obj)
{
    ExecutorState state;
    DBGFMT_EVAL("VmMachineType::%s::destruct(%p)", cname(), obj);

    aExec->save(&state);
    if (mDestructor) {
	CBaseObject* savedGlobal;
	aExec->activate((CExecutable*) obj);
	aExec->activate_global(global(), &savedGlobal);
	exec(aExec, mDestructor, false);
    }
    VmObjectType::destruct(aExec, obj);
    aExec->restore(&state);    
}


UData VmMachineType::produce(CExecutor* aExec, CBaseType* aBase, CArgs* args)
{
    ExecutorState state;
    CBaseType* produceType = this;
    CBaseType* initType = aBase ? aBase : this;
    UData r;
    CEvent* clearUntil;

    aExec->save(&state);

    // Find the base type to use when producing the object 
    while(!produceType->isVmType())
	produceType = (CBaseType*) produceType->parentType();

    r = produceType->produce(aExec, initType, NULL);
    aExec->activate((CExecutable*) r.o);
    initType->init(aExec, (CBaseObject*) r.o);
    clearUntil = ((CExecutable*)r.o)->updatedEvents();
    initType->defaults(aExec, (CBaseObject*) r.o);
    ((CExecutable*) r.o)->clearAllUpdated(aExec, clearUntil);
    if (args) args->run(aExec, r.o); // run constructor args if present
    initType->construct(aExec, (CBaseObject*) r.o);
    if (mDefinitionType == T_APPLICATION) {
	m1_applicationList.push_back((VmObject*) r.o);
	r.o->retainThis();
    }
    aExec->restore(&state);
    return r;
}

void VmMachineType::execute(CExecutor* aExec, CExecutable *aObject)
{
    CType* pt;
    CBaseObject* savedGlobal;
    DBGFMT("VmMachineType::execute() %s", cname());

    aExec->activate_global(global(), &savedGlobal);
    exec(aExec, mPreScript, false);

    if ((pt = parentType()) && pt->isExecutableType())
	((CExecutableType*) pt)->execute(aExec, aObject);

    exec(aExec, mPostScript, false);
    aExec->restore_global(savedGlobal);
}

string codeIndent(int indent)
{
    string s = "";
    while(indent--)
	s += ' ';
    return s;
}


void codeBody(VmMachineType* aMachine, ostream* os, string prefix, int indent)
{
    Instruction* ptr;
    Instruction* base;
    CBaseType* subt;
    int i;
    string blanks0 = codeIndent(indent);
    string blanks1 = codeIndent(indent+4);
    string blanks2 = codeIndent(indent+8);

    *os << blanks0 << prefix << " " << aMachine->name();
    if (aMachine->parentType() != NULL)
	*os << " : " << aMachine->parentName();
    *os << " {\n";

    *os << blanks1 << "VALUE-STACK-SIZE: " << 
	aMachine->valueStackSize() << ";\n";
    *os << blanks1 << "RETURN-STACK-SIZE: " << 
	aMachine->returnStackSize() << ";\n";

    for (i = 0; i < (int)aMachine->fieldCount(); i++) {
	if (i >= aMachine->fieldTypeOffset()) {  // only display added members
	    CField* fld = aMachine->field(i);
	    *os << blanks1 << "MEMBER: " <<
		formatStorageClass(fld->storage()) << ": ";
	    *os << fld->type()->typeSignature() << ":" << fld->name() << ";\n";
	}
    }
    
    if ((subt = aMachine->subTypes()) != NULL) {
	for (int i = 0; i < (int)subt->fieldCount(); i++) {
	    CField* fld = subt->field(i);
	    CType* fld_type = fld->type();
	    if (isAType(VmMachineType*, fld_type))
		codeBody((VmMachineType*)fld_type, os, "type", indent+4);
	}
    }

    *os << blanks1 << "+SCRIPT {\n";
    if ((base = ptr = aMachine->preScript()) != NULL) {
	while((i = m1VmDisAssemble(os, blanks2, base, ptr)) > 0)
	    ptr += i;
    }
    *os << blanks1 << "}\n";

    *os << blanks1 << "-SCRIPT {\n";
    if ((base = ptr = aMachine->postScript()) != NULL) {
	while((i = m1VmDisAssemble(os, blanks2, base, ptr)) > 0)
	    ptr += i;
    }
    *os << blanks1 << "}\n";
    
    *os << blanks1 << "DEFAULTS {\n";
    if ((base = ptr = aMachine->defaults()) != NULL) {
	while((i = m1VmDisAssemble(os, blanks2, base, ptr)) > 0)
	    ptr += i;
    }    
    *os << blanks1 << "}\n";

    *os << blanks1 << "CONSTRUCTOR {\n";
    if ((base = ptr = aMachine->constructor()) != NULL) {
	while((i = m1VmDisAssemble(os, blanks2, base, ptr)) > 0)
	    ptr += i;
    }
    *os << blanks1 << "}\n";
    *os << blanks0 << "}\n";
}
//
// Print disassembled code onto the output stream
//
void VmMachineType::printCode(ostream* os)
{
    codeBody(this, os, "library", 0);
}


void VmMachineArgs::run(CExecutor* aExec,CObject* obj)
{
    ExecutorState state;
    CBaseObject* savedGlobal;

    aExec->save(&state);
    // Switch to creator to get context right!!!
    aExec->activate(creator());
    aExec->activate_global(global(), &savedGlobal);

    // Run the BEGIN code after the NEWo instruction
    mArgs[1] = obj;   // this is the PUSH op data to push object on stack!!!
    ((VmMachineType*)(creator()->type()))->exec(aExec, mArgs, false);

    aExec->restore(&state);
}

//
// Generate byte code onto the output stream
//
void VmMachineType::writeCode(ostream* os)
{

}


#ifdef DEBUG
#define NEXT(I) do { \
	if (S >= S_end) { \
	    fprintf(stderr, "m1vm: stack overflow\n"); \
	    exit(1); \
	} \
	if (M1DBG_IS_SET(M1DBG_EXEC|M1DBG_PRNT)) {	\
	    char buf[8];				\
	    sprintf(buf, "%04d:", I - I0);		\
	    cerr << buf;				\
	}						\
	goto *(*I++);					\
    } while(0)
#else
#define NEXT(I) goto *(*I++)
#endif

#define SWAPOUT do { \
	aExec->setValueStackPtr(S);	\
	aExec->setReturnStackPtr(R);	\
    } while(0)
    

void VmMachineType::exec(CExecutor* aExec, Instruction* I0, bool init)
{
    __label__ PUSHu, PUSHi, PUSHf, PUSHs;
    __label__ DROP, SWAP, OVER, DUP;
    __label__ BEGIN, END, BREAK, RETURN, IGET, IPUT;
    __label__ NYI, UNDEF;
    __label__ ADDi, ADDu, ADDf, ADDs;
    __label__ SUBi, SUBu, SUBf;
    __label__ NEGi, NEGu, NEGf;
    __label__ MULi, MULu, MULf;
    __label__ DIVi, DIVu, DIVf;
    __label__ REMi, REMu;
    __label__ NOTi, ANDi, ORi;
    __label__ BANDu, BORu, BXORu, BNOTu, BSLu, BSLi, BSRu, BSRi;
    __label__ NEWo, NEWe, NEWa, INEWa;
    __label__ DEL, THIS, SCOPE, UPDATED, ACTIVATE, SAVE, RESTORE, CALL, CNCT;
    __label__ PUTu, PUTi, PUTf, PUTb, PUTc, PUTt, PUTs, PUTo, COPY;
    __label__ GETu, GETi, GETf, GETb, GETc, GETt, GETs, GETo, GETe;
    __label__ FPUTu, FPUTi, FPUTf, FPUTb, FPUTc, FPUTt, FPUTs, FPUTo;
    __label__ FGETu, FGETi, FGETf, FGETb, FGETc, FGETt, FGETs, FGETo, FGETe;
    __label__ CMPi, CMPu, CMPf, CMPo;
    __label__ LTz, LTEz, GTz, GTEz, EQz, NEQz;
    __label__ JUMPlt, JUMPle, JUMPeq, JUMPne, JUMPi, JUMPv;
    __label__ MULPUTi, DIVPUTi, ADDPUTi, SUBPUTi, REMPUTi;
    __label__ BANDPUTi, BORPUTi, BXORPUTi, BSLPUTi, BSRPUTi;
    __label__ MULPUTu, DIVPUTu, ADDPUTu, SUBPUTu, REMPUTu;
    __label__ BANDPUTu, BORPUTu, BXORPUTu, BSLPUTu, BSRPUTu;
    __label__ MULPUTb, DIVPUTb, ADDPUTb, SUBPUTb, REMPUTb;
    __label__ BANDPUTb, BORPUTb, BXORPUTb, BSLPUTb, BSRPUTb;
    __label__ MULPUTc, DIVPUTc, ADDPUTc, SUBPUTc, REMPUTc;
    __label__ BANDPUTc, BORPUTc, BXORPUTc, BSLPUTc, BSRPUTc;
    __label__ MULPUTt, DIVPUTt, ADDPUTt, SUBPUTt, REMPUTt;
    __label__ BANDPUTt, BORPUTt, BXORPUTt, BSLPUTt, BSRPUTt;
    __label__ MULPUTf, DIVPUTf, ADDPUTf, SUBPUTf;
    __label__ ADDPUTs;
    __label__ INCi,INCu,INCb,INCc,INCt,INCf;
    __label__ DECi,DECu,DECb,DECc,DECt,DECf;

    static bool need_init = true;
    UData* S;    // Top of stack
    UData* R;    // Top of return stack
    UData* S_end;
    UData* R_begin;
    UData* R_end;
    CObject* T;    // Temorary ref
    CExecutable* self;
    register Instruction* I = I0;

    if (init) goto init;
    if (I == NULL) return;

    self = aExec->current();

    S       = aExec->valueStackPtr();
    S_end   = aExec->valueStackEnd();

    R       = aExec->returnStackPtr();
    R_end   = aExec->returnStackEnd();
    R_begin = aExec->returnStackBegin();

    if (S + valueStackSize() > S_end) {
	ERRFMT("Value stack to small need is %d", valueStackSize());
	exit(1);
    }

    if (R + returnStackSize() > R_end) {
	ERRFMT("Return stack to small need is %d", returnStackSize());
	exit(1);
    }

    DBGFMT("VmMachineType::execute %s self=%p", cname(), self);
    NEXT(I);

    // Stack do not have reference count objects
    // the reason is that VmMachineType will not be rescheduled
    // in the middle of executing code. Must change if
    // we ever suspend execution in script.

////////////////////////////////////////////////////////////////
// Primitive stack operations
////////////////////////////////////////////////////////////////

PUSHu:
    INSTRUCTION_BEGIN(PUSHu);
    S[0].u = (unsigned int)*I;
    I++;
    S++;
    INSTRUCTION_END(PUSHu);
    NEXT(I);

PUSHi:
    INSTRUCTION_BEGIN(PUSHi);
    S[0].i = (int)*I;
    S++;
    I++;
    INSTRUCTION_END(PUSHi);
    NEXT(I);

PUSHf:
    INSTRUCTION_BEGIN(PUSHf);
    S[0].f = *((float*)I);
    S++;
    I++;
    INSTRUCTION_END(PUSHf);
    NEXT(I);

PUSHs:
    INSTRUCTION_BEGIN(PUSHs);
    S[0].str = (CString*)*I;
    I++;
    S++;
    INSTRUCTION_END(PUSHs);
    NEXT(I);

NIL:
    INSTRUCTION_BEGIN(NIL);
    S[0].u = 0;
    S++;
    INSTRUCTION_END(NIL);
    NEXT(I);    

DROP:
    INSTRUCTION_BEGIN(DROP);
    S--; 
    INSTRUCTION_END(DROP);
    NEXT(I);

DUP:
    INSTRUCTION_BEGIN(DUP);
    S[0] = S[-1];
    S++;
    INSTRUCTION_END(DUP);
    NEXT(I);    

OVER:
    INSTRUCTION_BEGIN(OVER);
    S[0] = S[-2];
    S++;
    INSTRUCTION_END(OVER);
    NEXT(I);    

SWAP: {
    UData tmp;
    INSTRUCTION_BEGIN(SWAP);
    tmp  = S[-2];
    S[-2] = S[-1];
    S[-1] = tmp;
    INSTRUCTION_END(SWAP);
    NEXT(I);
    }

////////////////////////////////////////////////////////////////
// Return stack operations
////////////////////////////////////////////////////////////////

BEGIN:
    INSTRUCTION_BEGIN(BEGIN);
    R[0].ud   = S-1;   // save stack pointer
    // set contiue and break address to absolute value
    R[1].addr = I[1] ? (I+(int)I[1]) : R[-3].addr;  // set continue address
    R[2].addr = I[0] ? (I+(int)I[0]) : R[-2].addr;  // set break address
    R[3]      = S[-1]; // store return value
    I += 2;
    S--;
    R += 4;
    INSTRUCTION_END(BEGIN);
    NEXT(I);

END:
    INSTRUCTION_BEGIN(END);
    R -= 4;
    if (R == R_begin)
	goto swapout;
    // FIXME: reset stack pointer to original ?
    INSTRUCTION_END(END);
    NEXT(I);

BREAK:
    INSTRUCTION_BEGIN(BREAK);
    I = R[-2].addr;
    INSTRUCTION_END(BREAK);
    NEXT(I);

CONTINUE:
    INSTRUCTION_BEGIN(CONTINUE);
    I = R[-3].addr;
    INSTRUCTION_END(CONTINUE);
    NEXT(I);

RETURN:
    INSTRUCTION_BEGIN(RETURN);
    INSTRUCTION_END(RETURN);
    goto swapout;

IGET:
    INSTRUCTION_BEGIN(IGET);
    S[0] = R[-1];
    S++;
    INSTRUCTION_END(IGET);
    NEXT(I);

IPUT:
    INSTRUCTION_BEGIN(IPUT);
    R[-1] = S[-1];
    S--;
    INSTRUCTION_END(IPUT);
    NEXT(I);

////////////////////////////////////////////////////////////////
// Binary operators
////////////////////////////////////////////////////////////////
ADDi:
    INSTRUCTION_BEGIN(ADDi);
    EXEC_BINARY(+, i);
    INSTRUCTION_END(ADDi);
    NEXT(I);
ADDu:
    INSTRUCTION_BEGIN(ADDu);
    EXEC_BINARY(+, u);
    INSTRUCTION_END(ADDu);
    NEXT(I);
ADDf:
    INSTRUCTION_BEGIN(ADDf);
    EXEC_BINARY(+, f);
    INSTRUCTION_END(ADDf);
    NEXT(I);
ADDs:
    INSTRUCTION_BEGIN(ADDs);
    S[-2].str = m1New(CString, S[-2].str->str() + S[-1].str->str());
    S--;
    INSTRUCTION_END(ADDs);
    NEXT(I);
SUBi:
    INSTRUCTION_BEGIN(SUBi);
    EXEC_BINARY(-, i);
    INSTRUCTION_END(SUBi);
    NEXT(I);
SUBu: 
    INSTRUCTION_BEGIN(SUBu);
    EXEC_BINARY(-, u);
    INSTRUCTION_END(SUBu);
    NEXT(I);
SUBf:
    INSTRUCTION_BEGIN(SUBf);
    EXEC_BINARY(-, f);
    INSTRUCTION_END(SUBf);
    NEXT(I);

MULi:
    INSTRUCTION_BEGIN(MULi);
    EXEC_BINARY(*, i);
    INSTRUCTION_END(MULi);
    NEXT(I);

MULu:
    INSTRUCTION_BEGIN(MULu);
    EXEC_BINARY(*, u);
    INSTRUCTION_END(MULu);
    NEXT(I);

MULf:
    INSTRUCTION_BEGIN(MULf);
    EXEC_BINARY(*, f);
    INSTRUCTION_END(MULf);
    NEXT(I);

DIVi:
    INSTRUCTION_BEGIN(DIVi);
    EXEC_BINARY(/, i);
    INSTRUCTION_END(DIVi);
    NEXT(I);

DIVu:
    INSTRUCTION_BEGIN(DIVu);
    EXEC_BINARY(/, u);
    INSTRUCTION_END(DIVu);
    NEXT(I);

DIVf:
    INSTRUCTION_BEGIN(DIVf);
    EXEC_BINARY(/, f);
    INSTRUCTION_END(DIVf);
    NEXT(I);

REMi:
    INSTRUCTION_BEGIN(REMi);
    EXEC_BINARY(%, i);
    INSTRUCTION_END(REMi);
    NEXT(I);

REMu:
    INSTRUCTION_BEGIN(REMu);
    EXEC_BINARY(%, u);
    INSTRUCTION_END(REMu);
    NEXT(I);

ANDi:
    INSTRUCTION_BEGIN(ANDi);
    REXEC_BINARY(&&, i);
    INSTRUCTION_END(ANDi);
    NEXT(I);

ORi:
    INSTRUCTION_BEGIN(ORi);
    REXEC_BINARY(||, i);
    INSTRUCTION_END(ORi);
    NEXT(I);

BANDu:
    INSTRUCTION_BEGIN(BANDu);
    EXEC_BINARY(&, u);
    INSTRUCTION_END(BANDu);
    NEXT(I);

BORu: 
    INSTRUCTION_BEGIN(BORu);
    EXEC_BINARY(|, u);
    INSTRUCTION_END(BORu);
    NEXT(I);

BXORu:
    INSTRUCTION_BEGIN(BXORu);
    EXEC_BINARY(^, u);
    INSTRUCTION_END(BXORu);
    NEXT(I);

BSLu:
    INSTRUCTION_BEGIN(BSLu);
    IEXEC_BINARY(<<, u);
    INSTRUCTION_END(BSLu);
    NEXT(I);

BSLi:
    INSTRUCTION_BEGIN(BSLi);
    IEXEC_BINARY(<<, i);
    INSTRUCTION_END(BSLi);
    NEXT(I);

BSRu:
    INSTRUCTION_BEGIN(BSRu);
    IEXEC_BINARY(>>, u);
    INSTRUCTION_END(BSRu);
    NEXT(I);

BSRi:
    INSTRUCTION_BEGIN(BSRi);
    IEXEC_BINARY(>>, i);
    INSTRUCTION_END(BSRi);
    NEXT(I);

////////////////////////////////////////////////////////////////
// Unary operators
////////////////////////////////////////////////////////////////
NEGi: 
    INSTRUCTION_BEGIN(NEGi);
    EXEC_UNARY(-, i);
    INSTRUCTION_END(NEGi);
    NEXT(I);

NEGu:
    INSTRUCTION_BEGIN(NEGu);
    EXEC_UNARY(-, u);
    INSTRUCTION_END(NEGu);
    NEXT(I);

NEGf:
    INSTRUCTION_BEGIN(NEGf);
    EXEC_UNARY(-, f);
    INSTRUCTION_END(NEGf);
    NEXT(I);

NOTi:
    INSTRUCTION_BEGIN(NOTi);
    EXEC_UNARY(!, i);
    INSTRUCTION_END(NOTi);
    NEXT(I);

BNOTu:
    INSTRUCTION_BEGIN(BNOTu);
    EXEC_UNARY(~, u);
    INSTRUCTION_END(BNOTu);
    NEXT(I);

ADDPUTi:
    INSTRUCTION_BEGIN(ADDPUTi);
    EXEC_UPDATE(+, i, i);
    INSTRUCTION_END(ADDPUTi);
    NEXT(I);

ADDPUTu:
    INSTRUCTION_BEGIN(ADDPUTu);
    EXEC_UPDATE(+, u, u);
    INSTRUCTION_END(ADDPUTu);
    NEXT(I);

ADDPUTb:
    INSTRUCTION_BEGIN(ADDPUTb);
    EXEC_UPDATE(+, b, u);
    INSTRUCTION_END(ADDPUTb);
    NEXT(I);

ADDPUTc:
    INSTRUCTION_BEGIN(ADDPUTc);
    EXEC_UPDATE(+, c, i);
    INSTRUCTION_END(ADDPUTc);
    NEXT(I);

ADDPUTt:
    INSTRUCTION_BEGIN(ADDPUTt);
    EXEC_UPDATE(+, t, i);
    INSTRUCTION_END(ADDPUTt);
    NEXT(I);

ADDPUTf:
    INSTRUCTION_BEGIN(ADDPUTf);
    EXEC_UPDATE(+, f, f);
    INSTRUCTION_END(ADDPUTf);
    NEXT(I);

ADDPUTs:
    INSTRUCTION_BEGIN(ADDPUTs);
    T = S[-3].o;
    S[-3].str = m1New(CString, T->at(S[-2].i).str->str() + S[-1].str->str());
    T->put(aExec, S[-2].i, S[-3]);
    S -= 2;
    INSTRUCTION_END(ADDPUTs);
    NEXT(I);

SUBPUTi: // object index value
    INSTRUCTION_BEGIN(SUBPUTi);
    EXEC_UPDATE(-, i, i);
    INSTRUCTION_END(SUBPUTi);
    NEXT(I);

SUBPUTu:
    INSTRUCTION_BEGIN(SUBPUTu);
    EXEC_UPDATE(-, u, u);
    INSTRUCTION_END(SUBPUTu);
    NEXT(I);

SUBPUTb:
    INSTRUCTION_BEGIN(SUBPUTb);
    EXEC_UPDATE(-, b, u);
    INSTRUCTION_END(SUBPUTb);
    NEXT(I);

SUBPUTc:
    INSTRUCTION_BEGIN(SUBPUTc);
    EXEC_UPDATE(-, c, i);
    INSTRUCTION_END(SUBPUTc);
    NEXT(I);

SUBPUTt:
    INSTRUCTION_BEGIN(SUBPUTt);
    EXEC_UPDATE(-, t, i);
    INSTRUCTION_END(SUBPUTt);
    NEXT(I);

SUBPUTf:
    INSTRUCTION_BEGIN(SUBPUTf);
    EXEC_UPDATE(-, f, f);
    INSTRUCTION_END(SUBPUTf);
    NEXT(I);

MULPUTi:
    INSTRUCTION_BEGIN(MULPUTi);
    EXEC_UPDATE(*, i, i);
    INSTRUCTION_END(MULPUTi);
    NEXT(I);

MULPUTu:
    INSTRUCTION_BEGIN(MULPUTu);
    EXEC_UPDATE(*, u, u);
    INSTRUCTION_END(MULPUTu);
    NEXT(I);

MULPUTb:
    INSTRUCTION_BEGIN(MULPUTb);
    EXEC_UPDATE(*, b, u);
    INSTRUCTION_END(MULPUTb);
    NEXT(I);

MULPUTc:
    INSTRUCTION_BEGIN(MULPUTc);
    EXEC_UPDATE(*, c, i);
    INSTRUCTION_END(MULPUTc);
    NEXT(I);

MULPUTt:
    INSTRUCTION_BEGIN(MULPUTt);
    EXEC_UPDATE(*, t, i);
    INSTRUCTION_END(MULPUTt);
    NEXT(I);

MULPUTf:
    INSTRUCTION_BEGIN(MULPUTf);
    EXEC_UPDATE(*, f, f);
    INSTRUCTION_END(MULPUTf);
    NEXT(I);

DIVPUTi:
    INSTRUCTION_BEGIN(DIVPUTi);
    EXEC_UPDATE(/, i, i);
    INSTRUCTION_END(DIVPUTi);
    NEXT(I);

DIVPUTu:
    INSTRUCTION_BEGIN(DIVPUTu);
    EXEC_UPDATE(/, u, u);
    INSTRUCTION_END(DIVPUTu);
    NEXT(I);

DIVPUTb:
    INSTRUCTION_BEGIN(DIVPUTb);
    EXEC_UPDATE(/, b, u);
    INSTRUCTION_END(DIVPUTb);
    NEXT(I);

DIVPUTc:
    INSTRUCTION_BEGIN(DIVPUTc);
    EXEC_UPDATE(/, c, i);
    INSTRUCTION_END(DIVPUTc);
    NEXT(I);

DIVPUTt:
    INSTRUCTION_BEGIN(DIVPUTt);
    EXEC_UPDATE(/, t, i);
    INSTRUCTION_END(DIVPUTt);
    NEXT(I);

DIVPUTf:
    INSTRUCTION_BEGIN(DIVPUTf);
    EXEC_UPDATE(/, f, f);
    INSTRUCTION_END(DIVPUTf);
    NEXT(I);

REMPUTi:
    INSTRUCTION_BEGIN(REMPUTi);
    EXEC_UPDATE(%, i, i);
    INSTRUCTION_END(REMPUTi);
    NEXT(I);

REMPUTu:
    INSTRUCTION_BEGIN(REMPUTu);
    EXEC_UPDATE(%, u, u);
    INSTRUCTION_END(REMPUTu);
    NEXT(I);

REMPUTb:
    INSTRUCTION_BEGIN(REMPUTb);
    EXEC_UPDATE(%, b, u);
    INSTRUCTION_END(REMPUTb);
    NEXT(I);

REMPUTc:
    INSTRUCTION_BEGIN(REMPUTc);
    EXEC_UPDATE(%, c, i);
    INSTRUCTION_END(REMPUTc);
    NEXT(I);

REMPUTt:
    INSTRUCTION_BEGIN(REMPUTt);
    EXEC_UPDATE(%, t, i);
    INSTRUCTION_END(REMPUTt);
    NEXT(I);

BORPUTi:
    INSTRUCTION_BEGIN(BORPUTi);
    EXEC_UPDATE(|, i, i);
    INSTRUCTION_END(BORPUTi);
    NEXT(I);

BORPUTu:
    INSTRUCTION_BEGIN(BORPUTu);
    EXEC_UPDATE(|, u, u);
    INSTRUCTION_END(BORPUTu);
    NEXT(I);

BORPUTb:
    INSTRUCTION_BEGIN(BORPUTb);
    EXEC_UPDATE(|, b, u);
    INSTRUCTION_END(BORPUTb);
    NEXT(I);

BORPUTc:
    INSTRUCTION_BEGIN(BORPUTc);
    EXEC_UPDATE(|, c, i);
    INSTRUCTION_END(BORPUTc);
    NEXT(I);

BORPUTt:
    INSTRUCTION_BEGIN(BORPUTt);
    EXEC_UPDATE(|, t, i);
    INSTRUCTION_END(BORPUTt);
    NEXT(I);

BXORPUTi:
    INSTRUCTION_BEGIN(BXORPUTi);
    EXEC_UPDATE(^, i, i);
    INSTRUCTION_END(BXORPUTi);
    NEXT(I);

BXORPUTu:
    INSTRUCTION_BEGIN(BXORPUTu);
    EXEC_UPDATE(^, u, u);
    INSTRUCTION_END(BXORPUTu);
    NEXT(I);

BXORPUTb:
    INSTRUCTION_BEGIN(BXORPUTb);
    EXEC_UPDATE(^, b, u);
    INSTRUCTION_END(BXORPUTb);
    NEXT(I);

BXORPUTc:
    INSTRUCTION_BEGIN(BXORPUTc);
    EXEC_UPDATE(^, c, i);
    INSTRUCTION_END(BXORPUTc);
    NEXT(I);

BXORPUTt:
    INSTRUCTION_BEGIN(BXORPUTt);
    EXEC_UPDATE(^, t, i);
    INSTRUCTION_END(BXORPUTt);
    NEXT(I);

BANDPUTi:
    INSTRUCTION_BEGIN(BANDPUTi);
    EXEC_UPDATE(&, i, i);
    INSTRUCTION_END(BANDPUTi);
    NEXT(I);

BANDPUTu:
    INSTRUCTION_BEGIN(BANDPUTu);
    EXEC_UPDATE(&, u, u);
    INSTRUCTION_END(BANDPUTu);
    NEXT(I);

BANDPUTb:
    INSTRUCTION_BEGIN(BANDPUTb);
    EXEC_UPDATE(&, b, u);
    INSTRUCTION_END(BANDPUTb);
    NEXT(I);

BANDPUTc:
    INSTRUCTION_BEGIN(BANDPUTc);
    EXEC_UPDATE(&, c, i);
    INSTRUCTION_END(BANDPUTc);
    NEXT(I);

BANDPUTt:
    INSTRUCTION_BEGIN(BANDPUTt);
    EXEC_UPDATE(&, t, i);
    INSTRUCTION_END(BANDPUTt);
    NEXT(I);

BSLPUTi:
    INSTRUCTION_BEGIN(BSLPUTi);
    IEXEC_UPDATE(<<, i, i);
    INSTRUCTION_END(BSLPUTi);
    NEXT(I);

BSLPUTu:
    INSTRUCTION_BEGIN(BSLPUTu);
    IEXEC_UPDATE(<<, u, u);
    INSTRUCTION_END(BSLPUTu);
    NEXT(I);

BSLPUTb:
    INSTRUCTION_BEGIN(BSLPUTb);
    IEXEC_UPDATE(<<, b, u);
    INSTRUCTION_END(BSLPUTb);
    NEXT(I);

BSLPUTc:
    INSTRUCTION_BEGIN(BSLPUTc);
    IEXEC_UPDATE(<<, c, i);
    INSTRUCTION_END(BSLPUTc);
    NEXT(I);

BSLPUTt:
    INSTRUCTION_BEGIN(BSLPUTt);
    IEXEC_UPDATE(<<, t, i);
    INSTRUCTION_END(BSLPUTt);
    NEXT(I);

BSRPUTi:
    INSTRUCTION_BEGIN(BSRPUTi);
    IEXEC_UPDATE(>>, i, i);
    INSTRUCTION_END(BSRPUTi);
    NEXT(I);

BSRPUTu:
    INSTRUCTION_BEGIN(BSRPUTu);
    IEXEC_UPDATE(>>, u, u);
    INSTRUCTION_END(BSRPUTu);
    NEXT(I);

BSRPUTb:
    INSTRUCTION_BEGIN(BSRPUTb);
    IEXEC_UPDATE(>>, b, u);
    INSTRUCTION_END(BSRPUTb);
    NEXT(I);

BSRPUTc:
    INSTRUCTION_BEGIN(BSRPUTc);
    IEXEC_UPDATE(>>, c, i);
    INSTRUCTION_END(BSRPUTc);
    NEXT(I);

BSRPUTt:
    INSTRUCTION_BEGIN(BSRPUTt);
    IEXEC_UPDATE(>>, t, i);
    INSTRUCTION_END(BSRPUTt);
    NEXT(I);

INCi:
    INSTRUCTION_BEGIN(INCi);
    EXEC_ADD(1, i, i);
    INSTRUCTION_END(INCi);
    NEXT(I);

INCu:
    INSTRUCTION_BEGIN(INCu);
    EXEC_ADD(1, u, u);
    INSTRUCTION_END(INCu);
    NEXT(I);

INCb:
    INSTRUCTION_BEGIN(INCb);
    EXEC_ADD(1, b, u);
    INSTRUCTION_END(INCb);
    NEXT(I);

INCc:
    INSTRUCTION_BEGIN(INCc);
    EXEC_ADD(1, c, i);
    INSTRUCTION_END(INCc);
    NEXT(I);

INCt:
    INSTRUCTION_BEGIN(INCt);
    EXEC_ADD(1, t, i);
    INSTRUCTION_END(INCt);
    NEXT(I);

INCf:
    INSTRUCTION_BEGIN(INCf);
    EXEC_ADD(1.0, f, f);
    INSTRUCTION_END(INCf);
    NEXT(I);

DECi:
    INSTRUCTION_BEGIN(DECi);
    EXEC_ADD(-1, i, i);
    INSTRUCTION_END(DECi);
    NEXT(I);

DECu:
    INSTRUCTION_BEGIN(DECu);
    EXEC_ADD(-1, u, u);
    INSTRUCTION_END(DECu);
    NEXT(I);

DECb:
    INSTRUCTION_BEGIN(DECb);
    EXEC_ADD(-1, b, u);
    INSTRUCTION_END(DECb);
    NEXT(I);

DECc:
    INSTRUCTION_BEGIN(DECc);
    EXEC_ADD(-1, c, i);
    INSTRUCTION_END(DECc);
    NEXT(I);

DECt:
    INSTRUCTION_BEGIN(DECt);
    EXEC_ADD(-1, t, i);
    INSTRUCTION_END(DECt);
    NEXT(I);

DECf:
    INSTRUCTION_BEGIN(DECf);
    EXEC_ADD(-1.0, f, f);
    INSTRUCTION_END(DECf);
    NEXT(I);


NEWo: {
    VmMachineArgs* args;
    CBaseType* t = ((CBaseType*)I[0]);
    INSTRUCTION_BEGIN(NEWo);
    INSTRUCTION_NL;
    // Next instruction MUST be a BEGIN, that will execute the args
    args = new VmMachineArgs(aExec->current(), aExec->global(),
			     I + 2);
    // Increament stack here to allow start function to do calls
    S++;
    // Save R and S since produce may be calling machine
    SWAPOUT;
    S[-1] = t->produce(aExec, t, args);
    if (isAType(CExecutable*, S[-1].o))
	((CExecutable*)S[-1].o)->start(aExec);
    I = I+(int)I[1]+1;
    INSTRUCTION_END(NEWo);
    NEXT(I);
    }

NEWe: // FIXME: needed? add queue flag
    INSTRUCTION_BEGIN(NEWe);
    S[0] = ((CType*)*I)->produceEvent(aExec, false); 
    I++; 
    S++;
    INSTRUCTION_END(NEWe);
    NEXT(I);

NEWa:
    INSTRUCTION_BEGIN(NEWa);
    SWAPOUT;
    S[0] = ((CType*)*I)->produceArray(aExec, (size_t)I[1]); 
    S++;
    I += 2; 
    INSTRUCTION_END(NEWa);
    NEXT(I);

INEWa: /* create array from stack elements */
    INSTRUCTION_BEGIN(INEWa);
    SWAPOUT;
    {
	unsigned int n = (size_t)I[1];
	UData d = ((CType*)*I)->produceArray(aExec, n);
	int i;
	for (i=0; i < (int) n; i++) // loop from S[-n] to  S[-1]
	    d.o->put(aExec, i, S[i-n]);
	S -= n;
	S[0] = d;
	S++;
    }
    I += 2;
    INSTRUCTION_END(INEWa);
    NEXT(I);

DEL:
    INSTRUCTION_BEGIN(DEL);
    S[-1].o->releaseThis();
    S--; 
    INSTRUCTION_END(DEL);
    NEXT(I);

FDELo:
    INSTRUCTION_BEGIN(FDELo);
    self->put(aExec, (unsigned int)*I, nil);
    INSTRUCTION_END(FDELo);
    I++;
    NEXT(I);

FDELe:
    INSTRUCTION_BEGIN(FDELe);
    self->eventPut(aExec, (int)*I, NULL);
    INSTRUCTION_END(FDELe);
    I++;
    NEXT(I);

CNCT:
    INSTRUCTION_BEGIN(CNCT);
    if (S[-1].evt == NULL)
	S[-2].evt->deleteFromSource();
    else
	S[-1].evt->addSubscriber(S[-2].evt);
    S[-2] = S[-1];  // source is kept on stack
    S--;
    INSTRUCTION_END(CNCT);
    NEXT(I);

GETu:
    INSTRUCTION_BEGIN(GETu);
    T = S[-2].o;
    S[-2] = T->at(S[-1].i);
    S--;
    INSTRUCTION_END(GETu);
    NEXT(I);

GETi:
    INSTRUCTION_BEGIN(GETi);
    T = S[-2].o;
    S[-2] = T->at(S[-1].i);
    S--;
    INSTRUCTION_END(GETi);
    NEXT(I);

GETf:
    INSTRUCTION_BEGIN(GETf);
    T = S[-2].o;
    S[-2] = T->at(S[-1].i);
    S--;
    INSTRUCTION_END(GETf);
    NEXT(I);

GETb:
    INSTRUCTION_BEGIN(GETb);
    T = S[-2].o;
    S[-2].u = T->at(S[-1].i).b;
    S--;
    INSTRUCTION_END(GETb);
    NEXT(I);

GETc:
    INSTRUCTION_BEGIN(GETc);
    T = S[-2].o;
    S[-2].i = T->at(S[-1].i).c;
    S--;
    INSTRUCTION_END(GETc);
    NEXT(I);

GETt:
    INSTRUCTION_BEGIN(GETt);
    T = S[-2].o;
    S[-2].i = T->at(S[-1].i).t;
    S--;
    INSTRUCTION_END(GETt);
    NEXT(I);

GETs:
    INSTRUCTION_BEGIN(GETs);
    T = S[-2].o;
    S[-2] = T->at(S[-1].i);
    S--;
    INSTRUCTION_END(GETs);
    NEXT(I);

GETo:
    INSTRUCTION_BEGIN(GETo);
    T = S[-2].o;
    S[-2] = T->at(S[-1].i);
    S--;
    INSTRUCTION_END(GETo);
    NEXT(I);

GETe:
    INSTRUCTION_BEGIN(GETe);
    if ((T = S[-2].o) == NULL)
	S[-2].evt = NULL;
    else
	S[-2].evt = T->eventAt(S[-1].i);
    S--;
    INSTRUCTION_END(GETe);
    NEXT(I);

FGETu:
    INSTRUCTION_BEGIN(FGETu);
    S[0] = self->at((int)*I);
    S++;
    INSTRUCTION_END(FGETu);
    I++;
    NEXT(I);

FGETi:
    INSTRUCTION_BEGIN(FGETi);
    S[0] = self->at((int)*I);
    S++;
    INSTRUCTION_END(FGETi);
    I++; 
    NEXT(I);

FGETf:
    INSTRUCTION_BEGIN(FGETf);
    S[0] = self->at((int)*I);
    S++;
    INSTRUCTION_END(FGETf);
    I++; 
    NEXT(I);

FGETb:
    INSTRUCTION_BEGIN(FGETb);
    S[0].u = self->at((int)*I).b;
    S++;
    INSTRUCTION_END(FGETb);
    I++; 
    NEXT(I);

FGETc:
    INSTRUCTION_BEGIN(FGETc);
    S[0].i = self->at((int)*I).c;
    S++;
    INSTRUCTION_END(FGETc);
    I++; 
    NEXT(I);

FGETt:
    INSTRUCTION_BEGIN(FGETt);
    S[0].i = self->at((int)*I).t;
    S++;
    INSTRUCTION_END(FGETt);
    I++; 
    NEXT(I);

FGETs:
    INSTRUCTION_BEGIN(FGETs);
    S[0] = self->at((int)*I);
    S++;
    INSTRUCTION_END(FGETs);
    I++;
    NEXT(I);

FGETo:
    INSTRUCTION_BEGIN(FGETo);
    S[0] = self->at((int)*I);
    S++;
    INSTRUCTION_END(FGETo);
    I++;
    NEXT(I);

FGETe:
    INSTRUCTION_BEGIN(FGETe);
    S[0].evt = self->eventAt((int)*I);
    S++;
    INSTRUCTION_END(FGETe);
    I++;
    NEXT(I);

PUTw:
    INSTRUCTION_BEGIN(PUTw);
    S[-3].o->putw(S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTw);
    NEXT(I);

PUTu:
    INSTRUCTION_BEGIN(PUTu);
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTu);
    NEXT(I);

PUTi:
    INSTRUCTION_BEGIN(PUTi);
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTi);
    NEXT(I);

PUTf:
    INSTRUCTION_BEGIN(PUTf);
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTf);
    NEXT(I);

PUTb:
    INSTRUCTION_BEGIN(PUTb);
    S[-1].b = S[-1].u; // convert to byte
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTb);
    NEXT(I);

PUTc:
    INSTRUCTION_BEGIN(PUTc);
    S[-1].c = S[-1].i; // convert to char
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;    
    INSTRUCTION_END(PUTc);
    NEXT(I);

PUTt:
    INSTRUCTION_BEGIN(PUTt);
    S[-1].t = S[-1].i; // convert to bool
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;        
    INSTRUCTION_END(PUTt);
    NEXT(I);

PUTs:
    INSTRUCTION_BEGIN(PUTs);
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTs);
    NEXT(I);

PUTo:
    INSTRUCTION_BEGIN(PUTo);
    S[-3].o->put(aExec, S[-2].i, S[-1]);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(PUTo);
    NEXT(I);

COPY:
    INSTRUCTION_BEGIN(COPY);
    fprintf(stderr, "FIXME: load target type for COPY!\n");
    S[-3].o->copy(aExec, any_type(), S[-2].i, S[-1], TRIGGER_AUTO);
    S[-3] = S[-1];
    S -=2;
    INSTRUCTION_END(COPY);
    NEXT(I);

FPUTw:
    INSTRUCTION_BEGIN(FPUTw);
    unsigned_type()->elementPut(aExec, self->base(), (int)*I, S[-1]);
    S--; 
    INSTRUCTION_END(FPUTw);
    I++;
    NEXT(I);

FPUTu:
    INSTRUCTION_BEGIN(FPUTu);
    self->put(aExec, (int)*I, S[-1]);
    INSTRUCTION_END(FPUTu);
    I++; 
    NEXT(I);

FPUTi:
    INSTRUCTION_BEGIN(FPUTi);
    self->put(aExec, (int)*I, S[-1]);
    INSTRUCTION_END(FPUTi);
    I++; 
    NEXT(I);

FPUTf:
    INSTRUCTION_BEGIN(FPUTf);
    self->put(aExec, (int)*I, S[-1]);
    INSTRUCTION_END(FPUTf);
    I++; 
    NEXT(I);

FPUTb:
    INSTRUCTION_BEGIN(FPUTb);
    self->put(aExec, (int)*I, UByte(S[-1].u));
    INSTRUCTION_END(FPUTb);
    I++; 
    NEXT(I);

FPUTc:
    INSTRUCTION_BEGIN(FPUTc);
    self->put(aExec, (int)*I, UChar(S[-1].i));
    INSTRUCTION_END(FPUTc);
    I++;
    NEXT(I);

FPUTt:
    INSTRUCTION_BEGIN(FPUTt);
    self->put(aExec, (int)*I, UBool(S[-1].i));
    INSTRUCTION_END(FPUTt);
    I++; 
    NEXT(I);

FPUTs:
    INSTRUCTION_BEGIN(FPUTs);
    self->put(aExec, (int)*I, S[-1]);
    INSTRUCTION_END(FPUTs);
    I++; 
    NEXT(I);

FPUTo:
    INSTRUCTION_BEGIN(FPUTo);
    self->put(aExec, (int)*I, S[-1]);
    INSTRUCTION_END(FPUTo);
    I++; 
    NEXT(I);

THIS:
    INSTRUCTION_BEGIN(THIS);
    S[0].o = self;
    S++;
    INSTRUCTION_END(THIS);
    NEXT(I);

SCOPE:
    INSTRUCTION_BEGIN(SCOPE);
    S[0].o = aExec->scope((ActivationType_t)((int)*I));
    S++;
    INSTRUCTION_END(SCOPE);
    I++;
    NEXT(I);

UPDATED:
    INSTRUCTION_BEGIN(UPDATED);
    S[-1].i = S[-1].evt->updated();
    INSTRUCTION_END(UPDATED);
    NEXT(I);

ACTIVATE:
    INSTRUCTION_BEGIN(ACTIVATE);
    aExec->activate((CBaseObject*)S[-1].o);
    S--;
    INSTRUCTION_END(ACTIVATE);
    NEXT(I);
    
SAVE:
    INSTRUCTION_BEGIN(SAVE);
    aExec->save((ExecutorState*)R); // Yes it's ugly ;-) 
    R += (sizeof(ExecutorState)/sizeof(UData));
    INSTRUCTION_END(SAVE);
    NEXT(I);

RESTORE:
    INSTRUCTION_BEGIN(RESTORE);
    R -= (sizeof(ExecutorState)/sizeof(UData));
    aExec->restore((ExecutorState*)R);
    INSTRUCTION_END(RESTORE);
    NEXT(I);

CALL: {
    int n = (int)I[1];
    INSTRUCTION_BEGIN(CALL);
    // Swapout R,S since function may create objects etc.
    SWAPOUT;
    S[-n] = ((UBuiltin*)I[0])->func(aExec, S - n);
    S -= (n-1);
    INSTRUCTION_END(CALL);
    I += 2;
    NEXT(I);
    }

CMPi:
    INSTRUCTION_BEGIN(CMPi);
    S[-2].i = signed_type()->compare(S[-2],S[-1]); 
    S--;
    INSTRUCTION_END(CMPi);
    NEXT(I);
CMPu:
    INSTRUCTION_BEGIN(CMPu);
    S[-2].i = unsigned_type()->compare(S[-2],S[-1]); 
    S--;
    INSTRUCTION_END(CMPu);
    NEXT(I);
CMPf:
    INSTRUCTION_BEGIN(CMPf);
    S[-2].i = float_type()->compare(S[-2],S[-1]); 
    S--; 
    INSTRUCTION_END(CMPf);
    NEXT(I);

CMPo:
    INSTRUCTION_BEGIN(CMPo);
    T = S[-2].o;
    if (T == S[-1].o)   S[-2].i = 0;
    else if (T == NULL) S[-2].i = -1;
    else if (S[-1].o == NULL) S[-2].i = -1;
    else  S[-2].i = T->type()->compare(S[-2], S[-1]);
    S--; 
    INSTRUCTION_END(CMPo);
    NEXT(I);

LTz:
    INSTRUCTION_BEGIN(LTz);
    S[-1].i = (S[-1].i < 0);
    INSTRUCTION_END(LTz);
    NEXT(I);    

LTEz:
    INSTRUCTION_BEGIN(LTEz);
    S[-1].i = (S[-1].i <= 0);
    INSTRUCTION_END(LTEz);
    NEXT(I);    

GTz:
    INSTRUCTION_BEGIN(GTz);
    S[-1].i = (S[-1].i > 0);
    INSTRUCTION_END(GTz);
    NEXT(I);    

GTEz:
    INSTRUCTION_BEGIN(GTEz);
    S[-1].i = (S[-1].i >= 0);
    INSTRUCTION_END(GTEz);
    NEXT(I);    

EQz:
    INSTRUCTION_BEGIN(EQz);
    S[-1].i = (S[-1].i == 0);
    INSTRUCTION_END(EQz);
    NEXT(I);    

NEQz:
    INSTRUCTION_BEGIN(NEQz);
    S[-1].i = (S[-1].i != 0);
    INSTRUCTION_END(NEQz);
    NEXT(I);    

JUMPlt:
    INSTRUCTION_BEGIN(JUMPlt);
    if (S[-1].i < 0)
	I = I+(int)*I;
    else
	I++;
    S--;
    INSTRUCTION_END(JUMPlt);
    NEXT(I);

JUMPle:
    INSTRUCTION_BEGIN(JUMPle);
    if (S[-1].i <= 0)
	I = I+(int)*I;
    else
	I++;
    S--;
    INSTRUCTION_END(JUMPle);
    NEXT(I);

JUMPeq:
    INSTRUCTION_BEGIN(JUMPeq);
    if (S[-1].i == 0)
	I = I+(int)*I;
    else
	I++;
    S--;
    INSTRUCTION_END(JUMPeq);
    NEXT(I);

JUMPne:
    INSTRUCTION_BEGIN(JUMPne);
    if (S[-1].i != 0)
	I = I+(int)*I;
    else
	I++;
    S--;
    INSTRUCTION_END(JUMPne);
    NEXT(I);

JUMP:
    INSTRUCTION_BEGIN(JUMP);
    I = I+(int)*I;
    INSTRUCTION_END(JUMP);
    NEXT(I);

JUMPi:
    INSTRUCTION_BEGIN(JUMPi);
    {
	unsigned int  n = (unsigned int)*I++;
	unsigned int  offs = 0;

	if ((S[-1].i >= 0) && (S[-1].u < n))
	    offs = (int)I[S[-1].i];
	I = I+n+offs;
    }
    S--;
    INSTRUCTION_END(JUMPi);
    NEXT(I);

JUMPv: 
    INSTRUCTION_BEGIN(JUMPv);
    {
	unsigned int  n = (unsigned int)*I++;
        // table v32 ... v32
	int* vbase = (int*)I;
        // table offs32 ... offs32
	int* obase = (int*)vbase + n;
	unsigned int  lim;
	int  offs = 0;

	for (lim = n; lim != 0; lim >>= 1) {
	    int* p = vbase + (lim>>1);

	    if (S[-1].i == *p) {
		offs = *(obase + (lim>>1));
		break;
	    }
	    if (S[-1].i > *p) {
		vbase = p + 1;
		lim--;
	    }
	}
	I = I+2*n+offs;
    }
    S--;
    INSTRUCTION_END(JUMPv);
    NEXT(I);

CVTiu:
    INSTRUCTION_BEGIN(CVTiu);
    S[-1].u = S[-1].i;
    INSTRUCTION_END(CVTiu);
    NEXT(I);

CVTif:
    INSTRUCTION_BEGIN(CVTif);
    S[-1].f = (float) S[-1].i;
    INSTRUCTION_END(CVTif);
    NEXT(I);

CVTui:
    INSTRUCTION_BEGIN(CVTui);
    S[-1].i = S[-1].u;
    INSTRUCTION_END(CVTui);
    NEXT(I);

CVTuf:
    INSTRUCTION_BEGIN(CVTuf);
    S[-1].f = (float) S[-1].u;
    INSTRUCTION_END(CVTuf);
    NEXT(I);

CVTfi:
    INSTRUCTION_BEGIN(CVTfi);
    S[-1].i = (int) S[-1].f;
    INSTRUCTION_END(CVTfi);
    NEXT(I);

CVTfu:
    INSTRUCTION_BEGIN(CVTfu);
    S[-1].u = (unsigned int) S[-1].f;
    INSTRUCTION_END(CVTfu);
    NEXT(I);

UNDEF:
    m1BreakHere(__FILE__, __LINE__, (char*)"operation not defined");
    throw M1StatusError;

NYI:
    m1BreakHere(__FILE__, __LINE__, (char*)"operation not implemented");
    throw M1StatusError;

swapout:
    SWAPOUT;
    return;

init:
    // FIXME! Make thread safe
    if (!need_init) return;
    need_init = false;
    DBGFMT("VmMachineType::Execute init");

    // initialize all codes to not implemented
    {
	int i;
	for (i = 0; i < op_LAST; i++) 
	    vm_instruction[i].instr = &&NYI;
    }

    INIT_INSTRUCTION(UNDEF, 0, "",  0,  0, "", "");
    vm_instruction[0].instr = NULL;

    /* primitive stack operations */
    INIT_INSTRUCTION(PUSHu,  1, "u",  0, 1, "", "u");
    INIT_INSTRUCTION(PUSHi,  1, "i",  0, 1, "", "i");
    INIT_INSTRUCTION(PUSHf,  1, "f",  0, 1, "", "f");
    INIT_INSTRUCTION(PUSHs, 1, "s",  0, 1, "", "s");
    INIT_INSTRUCTION(DROP,  0, "",   1, 0, "u", "");
    INIT_INSTRUCTION(DUP,   0, "",   0, 2, "u", "uu");
    INIT_INSTRUCTION(OVER,  0, "",   2, 3, "uu", "uuu");
    INIT_INSTRUCTION(SWAP,  0, "",   2, 2, "uu", "uu");
    INIT_INSTRUCTION(NIL,   0, "",   0, 1, "", "u");

    /* return stack operations */
    INIT_INSTRUCTION(BEGIN,    2, "LL",  1, 0, "u", "");
    INIT_INSTRUCTION(END,      0, "",    0, 0, "", "");
    INIT_INSTRUCTION(BREAK,    0, "",    0, 0, "", "");
    INIT_INSTRUCTION(CONTINUE, 0, "",    0, 0, "", "");
    INIT_INSTRUCTION(RETURN,   0, "",    0, 0, "", "");
    INIT_INSTRUCTION(IGET,     0, "",    0, 1, "", "u");
    INIT_INSTRUCTION(IPUT,     0, "",    1, 0, "u", "");

    INIT_INSTRUCTION(ADDi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(ADDu,  0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(ADDf,  0, "", 2, 1, "ff", "f");
    INIT_INSTRUCTION(ADDs,  0, "", 2, 1, "ss", "s");
    INIT_INSTRUCTION(SUBi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(SUBu,  0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(SUBf,  0, "", 2, 1, "ff", "f");
    INIT_INSTRUCTION(NEGi,  0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(NEGu,  0, "", 1, 1, "u", "u");
    INIT_INSTRUCTION(NEGf,  0, "", 1, 1, "f", "f");
    INIT_INSTRUCTION(MULi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(MULu,  0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(MULf,  0, "", 2, 1, "ff", "f");
    INIT_INSTRUCTION(DIVi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(DIVu,  0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(DIVf,  0, "", 2, 1, "ff", "f");
    INIT_INSTRUCTION(REMi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(REMu,  0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(NOTi,  0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(ANDi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(ORi,   0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(BANDu, 0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(BORu,  0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(BXORu, 0, "", 2, 1, "uu", "u");
    INIT_INSTRUCTION(BNOTu, 0, "", 1, 1, "u", "u");
    INIT_INSTRUCTION(BSLu,  0, "", 2, 1, "ui", "u");
    INIT_INSTRUCTION(BSLi,  0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(BSRu,  0, "", 2, 1, "ui", "u");
    INIT_INSTRUCTION(BSRi,  0, "", 2, 1, "ii", "i");

    INIT_INSTRUCTION(MULPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(DIVPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(ADDPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(SUBPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(REMPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BANDPUTi, 0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BORPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BXORPUTi, 0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BSLPUTi,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BSRPUTi,  0, "", 3, 1, "oii", "i");

    INIT_INSTRUCTION(MULPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(DIVPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(ADDPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(SUBPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(REMPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BANDPUTu, 0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BORPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BXORPUTu, 0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BSLPUTu,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BSRPUTu,  0, "", 3, 1, "oiu", "u");

    INIT_INSTRUCTION(MULPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(DIVPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(ADDPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(SUBPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(REMPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BANDPUTb, 0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BORPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BXORPUTb, 0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BSLPUTb,  0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(BSRPUTb,  0, "", 3, 1, "oiu", "u");

    INIT_INSTRUCTION(MULPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(DIVPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(ADDPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(SUBPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(REMPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BANDPUTc, 0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BORPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BXORPUTc, 0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BSLPUTc,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BSRPUTc,  0, "", 3, 1, "oii", "i");

    INIT_INSTRUCTION(MULPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(DIVPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(ADDPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(SUBPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(REMPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BANDPUTt, 0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BORPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BXORPUTt, 0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BSLPUTt,  0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(BSRPUTt,  0, "", 3, 1, "oii", "i");

    INIT_INSTRUCTION(MULPUTf,  0, "", 3, 1, "oif", "f");
    INIT_INSTRUCTION(DIVPUTf,  0, "", 3, 1, "oif", "f");
    INIT_INSTRUCTION(ADDPUTf,  0, "", 3, 1, "oif", "f");
    INIT_INSTRUCTION(SUBPUTf,  0, "", 3, 1, "oif", "f");

    INIT_INSTRUCTION(ADDPUTs,  0, "", 3, 1, "ois", "s");

    INIT_INSTRUCTION(INCi,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(INCu,     0, "", 2, 1, "oi", "u");
    INIT_INSTRUCTION(INCb,     0, "", 2, 1, "oi", "u");
    INIT_INSTRUCTION(INCc,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(INCt,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(INCf,     0, "", 2, 1, "oi", "f");

    INIT_INSTRUCTION(DECi,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(DECu,     0, "", 2, 1, "oi", "u");
    INIT_INSTRUCTION(DECb,     0, "", 2, 1, "oi", "u");
    INIT_INSTRUCTION(DECc,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(DECt,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(DECf,     0, "", 2, 1, "oi", "f");

    INIT_INSTRUCTION(NEWo,     2, "TL", 0, 1, "", "o");
    INIT_INSTRUCTION(NEWe,     1, "T",  0, 1, "", "e");
    INIT_INSTRUCTION(NEWa,     2, "Tu", 0, 1, "", "o");
    INIT_INSTRUCTION(INEWa,    2, "Tn", 0, 1, "n*u", "o");

    INIT_INSTRUCTION(DEL,      0, "", 1, 0, "o", "");
    INIT_INSTRUCTION(FDELo,    0, "", 0, 0, "", "");
    INIT_INSTRUCTION(FDELe,    0, "", 0, 0, "", "");
    INIT_INSTRUCTION(CNCT,     0, "", 2, 0, "ee", "e");

    INIT_INSTRUCTION(GETu,     0, "", 2, 1, "oi", "u");
    INIT_INSTRUCTION(GETi,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(GETf,     0, "", 2, 1, "oi", "f");
    INIT_INSTRUCTION(GETb,     0, "", 2, 1, "oi", "u");
    INIT_INSTRUCTION(GETc,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(GETt,     0, "", 2, 1, "oi", "i");
    INIT_INSTRUCTION(GETs,     0, "", 2, 1, "oi", "s");
    INIT_INSTRUCTION(GETo,     0, "", 2, 1, "oi", "o");
    INIT_INSTRUCTION(GETe,     0, "", 2, 1, "oi", "e");

    INIT_INSTRUCTION(PUTu,     0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(PUTi,     0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(PUTf,     0, "", 3, 1, "oif", "f");
    INIT_INSTRUCTION(PUTb,     0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(PUTc,     0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(PUTt,     0, "", 3, 1, "oii", "i");
    INIT_INSTRUCTION(PUTw,     0, "", 3, 1, "oiu", "u");
    INIT_INSTRUCTION(PUTs,     0, "", 3, 1, "ois", "s");
    INIT_INSTRUCTION(PUTo,     0, "", 3, 1, "oio", "o");
    INIT_INSTRUCTION(COPY,     0, "", 3, 1, "oio", "o");

    INIT_INSTRUCTION(ACTIVATE, 0, "",   1, 0, "o", "");
    INIT_INSTRUCTION(SAVE,     0, "",   0, 0, "", "");
    INIT_INSTRUCTION(RESTORE,  0, "",   0, 0, "", "");
    INIT_INSTRUCTION(THIS,     0, "",  0, 1, "", "o");
    INIT_INSTRUCTION(SCOPE,    1, "i",   0, 1, "", "o");
    INIT_INSTRUCTION(UPDATED,  0, "",   1, 1, "e", "i");
    INIT_INSTRUCTION(CALL,     2, "Fn", 0, 1, "n*u", "u");

    INIT_INSTRUCTION(CMPi,     0, "", 2, 1, "ii", "i");
    INIT_INSTRUCTION(CMPu,     0, "", 2, 1, "uu", "i");
    INIT_INSTRUCTION(CMPf,     0, "", 2, 1, "ff", "i");
    INIT_INSTRUCTION(CMPo,     0, "", 2, 1, "oo", "i");

    INIT_INSTRUCTION(JUMPlt,   1, "L", 1, 0, "i", "");
    INIT_INSTRUCTION(JUMPle,   1, "L", 1, 0, "i", "");
    INIT_INSTRUCTION(JUMPeq,   1, "L", 1, 0, "i", "");
    INIT_INSTRUCTION(JUMPne,   1, "L", 1, 0, "i", "");
    INIT_INSTRUCTION(JUMP,     1, "L", 0, 0, "", "");
    INIT_INSTRUCTION(JUMPi,    1, "n*L", 1, 0, "i", "");
    INIT_INSTRUCTION(JUMPv,    1, "n*i*L", 1, 0, "i", "");

    INIT_INSTRUCTION(FGETu,    1, "i", 0, 1, "", "u");
    INIT_INSTRUCTION(FGETi,    1, "i", 0, 1, "", "i");
    INIT_INSTRUCTION(FGETf,    1, "i", 0, 1, "", "f");
    INIT_INSTRUCTION(FGETb,    1, "i", 0, 1, "", "u");
    INIT_INSTRUCTION(FGETc,    1, "i", 0, 1, "", "i");
    INIT_INSTRUCTION(FGETt,    1, "i", 0, 1, "", "i");
    INIT_INSTRUCTION(FGETo,    1, "i", 0, 1, "", "o");
    INIT_INSTRUCTION(FGETs,    1, "i", 0, 1, "", "s");
    INIT_INSTRUCTION(FGETe,    1, "i", 0, 1, "", "e");

    INIT_INSTRUCTION(FPUTu,    1, "i", 1, 1, "u", "u");
    INIT_INSTRUCTION(FPUTi,    1, "i", 1, 1, "i", "i");
    INIT_INSTRUCTION(FPUTf,    1, "i", 1, 1, "f", "f");
    INIT_INSTRUCTION(FPUTb,    1, "i", 1, 1, "u", "u");
    INIT_INSTRUCTION(FPUTc,    1, "i", 1, 1, "i", "i");
    INIT_INSTRUCTION(FPUTt,    1, "i", 1, 1, "i", "i");
    INIT_INSTRUCTION(FPUTo,    1, "i", 1, 1, "o", "o");
    INIT_INSTRUCTION(FPUTs,    1, "i", 1, 1, "s", "s");
    INIT_INSTRUCTION(FPUTw,    1, "i", 1, 1, "u", "u");

    INIT_INSTRUCTION(CVTiu,    0, "", 1, 1, "i", "u");
    INIT_INSTRUCTION(CVTif,    0, "", 1, 1, "i", "f");
    INIT_INSTRUCTION(CVTui,    0, "", 1, 1, "u", "i");
    INIT_INSTRUCTION(CVTuf,    0, "", 1, 1, "u", "f");
    INIT_INSTRUCTION(CVTfi,    0, "", 1, 1, "f", "i");
    INIT_INSTRUCTION(CVTfu,    0, "", 1, 1, "f", "u");

    INIT_INSTRUCTION(LTz,      0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(LTEz,     0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(GTz,      0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(GTEz,     0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(EQz,      0, "", 1, 1, "i", "i");
    INIT_INSTRUCTION(NEQz,     0, "", 1, 1, "i", "i");


}


//
// Allocate and load code into code area
//
u_int8_t* VmMachineType::loadPreScript(u_int8_t* ptr,u_int8_t* ptr_end)
{
    return loadCode((char*)"Script", ptr, ptr_end, &mPreScript);
}

u_int8_t* VmMachineType::loadPostScript(u_int8_t* ptr,u_int8_t* ptr_end)
{
    return loadCode((char*)"Script", ptr, ptr_end, &mPostScript);
}

u_int8_t* VmMachineType::loadConstructor(u_int8_t* ptr,u_int8_t* ptr_end)
{
    return loadCode((char*)"Construct", ptr, ptr_end, &mConstructor);
}

u_int8_t* VmMachineType::loadDestructor(u_int8_t* ptr,u_int8_t* ptr_end)
{
    return loadCode((char*)"Destruct", ptr, ptr_end, &mDestructor);
}

u_int8_t* VmMachineType::loadDefaults(u_int8_t* ptr,u_int8_t* ptr_end)
{
    return loadCode((char*)"Defaults", ptr, ptr_end, &mDefaults);
}


u_int8_t* VmMachineType::loadCode(char* execName,
				    u_int8_t* ptr,u_int8_t* ptr_end,
				    Instruction** code)
{
    u_int8_t* ptr0 = ptr;
    size_t I_size = 0;
    size_t S_max = S_size;
    size_t S_cur = S_size;
    size_t R_max = R_size;
    size_t R_cur = R_size;
    size_t N_cur = N_size;
    size_t n;
    Instruction* I;

    DBGFMT("Loading: %s:%s", cname(), execName);

    // Scan byte code and calculate instruction area
    // Initial stack area and constants
    while (ptr < ptr_end) {
	int op = *ptr++;
	DBGFMT("Opcode: %d S_cur=%d", op, (int)S_cur);
	S_cur += (vm_instruction[op].produce - vm_instruction[op].consume);
	I_size += (1 + vm_instruction[op].args);
	switch(op) {
	case op_PUSHu:
	case op_PUSHi:
	case op_PUSHf:
	case op_FPUTu:  
	case op_FPUTw: 
	case op_FPUTo:
	case op_FGETu: 
	case op_FGETo:
	case op_JUMPlt: 
	case op_JUMPle:
	case op_JUMPeq:
	case op_SCOPE:
	case op_FDELo:
	case op_FDELe:
	case op_JUMP:
	    ptr += 4;
	    break;

	    // op <string:n:8>   consume:0 produce: 1
	case op_PUSHs:
	    N_size++;
	    ptr = ptr+(1+ptr[0]);
	    break;

	    // op <string:n:8> <unsigned:32>  consume:0 produce:1
	case op_NEWa:
	    ptr = ptr+(1+ptr[0])+4;
	    break;

	    // op <string:n:8> <unsigned:32> consume:n produce:1
	case op_INEWa:
	    ptr = ptr+(1+ptr[0]);
	    n = VM_GET_U32(ptr);
	    ptr += 4;
	    S_cur += n;
	    break;

	    // op <string:n:8>  produce:1
	case op_NEWe:
	    ptr = ptr + (1+ptr[0]);
	    break;
	    // op <string:n:8> <signed:32> 
	case op_NEWo:
	    ptr = ptr + (1+ptr[0]) + 4;
	    break;

	    // op <unsigned:32> ( unsigned:32 ) * n  consume:0 produce:0
	case op_JUMPi: {
	    int n;
	    S_cur--;
	    ptr++;
	    n = VM_GET_U32(ptr);
	    ptr += 4;
	    I_size += (2+n);
	    ptr += n*4;
	    break;
	}

	    // op <unsigned:32> ( unsigned:32 ) * n ( unsigned:32 ) * n
	    //     consume:0 produce:0
	case op_JUMPv: {
	    int n;
	    S_cur--;
	    ptr++;
	    n = VM_GET_U32(ptr);
	    ptr += 4;
	    I_size += (2+2*n);
	    ptr += 2*n*4;
	    break;
	}

	case op_BEGIN:  // label-break label-continue 
	    ptr += 8;
	    R_cur += 4;
	    break;

	case op_END:
	    R_cur -= 4;
	    break;

	default:
	    // no argument opcode end up here
	    break;
	}
	if (S_cur > S_max) S_max = S_cur;
	if (R_cur > R_max) R_max = R_cur;
    }

    S_size = S_max;
    R_size = R_cur;

    I = *code = new Instruction [I_size + 1];
    cnst.resize(N_size);

    ptr = ptr0;

    while(ptr < ptr_end) {
	switch(*ptr) {
	    // op <string255>
	case op_PUSHs:
	    *I++ = vm_instruction[*ptr++].instr;
	    n = *ptr++;
	    *I++ = (void*) addStringConstant(string((char*)ptr, n));
	    ptr = ptr + n;
	    break;

	case op_INEWa:
	case op_NEWa: { // op <element-class> <unsigned32>
	    string aName;  // Array class name <ename>[%d]

	    *I++ = vm_instruction[*ptr++].instr;

	    ptr=load_string(ptr, ptr_end, &aName);
	    n = VM_GET_U32(ptr);
	    ptr += 4;
	    aName = addDimension(aName,n);
	    *I++ = (void*) m1TypeLookup(aName);
	    *I++ = (void*) n;
	    break;
	}

	case op_NEWo: { // op <class> <label:32>
	    string tName;
	    *I++ = vm_instruction[*ptr++].instr;

	    ptr=load_string(ptr, ptr_end, &tName);
	    *I++ = (void*) m1TypeLookup(tName);
	    n = VM_GET_U32(ptr);
	    ptr += 4;
	    *I++ = (void*) n;
	    break;
	}

	case op_NEWe: { // op <class>
	    string tName;
	    *I++ = vm_instruction[*ptr++].instr;

	    ptr=load_string(ptr, ptr_end, &tName);
	    *I++ = (void*) m1TypeLookup(tName);
	    break;
	}

	    // op <signed:32>
	case op_SCOPE:
	case op_FDELo:
	case op_FDELe:
	case op_PUSHu:
	case op_PUSHi:
	case op_PUSHf:
	case op_FPUTu: case op_FPUTw: case op_FPUTo:
	case op_FGETu: case op_FGETo:
	case op_JUMPlt: case op_JUMPle: case op_JUMPeq: case op_JUMPne:
	case op_JUMP: {
	    *I++ = vm_instruction[*ptr++].instr;
	    *I++ =(void*)VM_GET_U32(ptr);
	    ptr += 4;
	    break;
	}

	    // op <N=u32> <offs1> ..<offsN>
	case op_JUMPi: {
	    *I++ = vm_instruction[*ptr++].instr;
	    n = VM_GET_U32(ptr);
	    *I++ = (void*) n;
	    ptr += sizeof(unsigned int);
	    while(n--) {
		*I++ = (void*) VM_GET_U32(ptr);
		ptr += sizeof(unsigned int);
	    }
	    break;
	}
	    // op <N=u32> <offs1> ..<offsN> <u1> ... <uN>
	case op_JUMPv: {
	    *I++ = vm_instruction[*ptr++].instr;
	    n = VM_GET_U32(ptr);
	    *I++ = (void*)n;
	    ptr += sizeof(unsigned int);
	    n *= 2;
	    while(n--) {
		*I++ = (void*) VM_GET_U32(ptr);
		ptr += sizeof(unsigned int);
	    }
	    break;
	}

	default:
	    *I++ = vm_instruction[*ptr].instr;
	    ptr++;
	    break;
	    // END  section terminator & op code

	}
    }

    // add an implicit end
    *I++ = vm_instruction[op_END].instr;

    N_size = N_cur;
    return ptr;
}
