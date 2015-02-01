//
// FILE FORMAT definition 
//

#ifndef __M1VM_FORMAT_HH__
#define __M1VM_FORMAT_HH__

//
// FILE FORMAT
//   VM_TAG_MAGIC
//
// ONE OR MORE OF THE FOLLOWING:
//
//   VM_TAG_TYPE <name255>
//   <n-fields:32>
//   <pos:32> <q-field:8> <name255> <type255>
//   ...
//   <pos:32> <q-field:8> <name255> <type255>
//
//   [ VM_TAG_CODE
//     VM_TAG_CONSTRUCTOR
//     <init-byte-code> 
//     END ]
//
//   [ VM_TAG_CODE
//     VM_TAG_EXECUTE
//     <executel-byte-code 
//     END]
//

typedef u_int32_t VmTag;

#define VM_U32_BE(a,b,c,d) (((a) << 24)|((b)<<16)|((c)<<8)|(d))
#define VM_U32_LE(a,b,c,d) (((d) << 24)|((c)<<16)|((b)<<8)|(a))


// FILE SEGEMENTS
#if BYTE_ORDER == BIG_ENDIAN
#define TAG_MAKE(a,b,c,d) VM_U32_BE(a,b,c,d)
#else
#define TAG_MAKE(a,b,c,d) VM_U32_LE(a,b,c,d)
#endif

// VM_TAG_xxx  are put in the file
#define VM_TAG_MAGIC 'M','1','V','M'
#define VM_TAG_TYPE  'T','Y','P','E'
#define VM_TAG_CODE  'C','O','D','E'

// tag_xxx  are used to match in code
static const VmTag tag_MAGIC = TAG_MAKE('M','1','V','M');
static const VmTag tag_TYPE  = TAG_MAKE('T','Y','P','E');
static const VmTag tag_CODE  = TAG_MAKE('C','O','D','E');

// TYPE DEFINITIONS 
#define VM_TAG_TYPE_OBJECT      'T','O','B','J'
#define VM_TAG_TYPE_DEF         'T','D','E','F'
#define VM_TAG_TYPE_DECL        'T','D','C','L'

static const VmTag tag_TYPE_OBJECT = TAG_MAKE('T','O','B','J');
static const VmTag tag_TYPE_DEF    = TAG_MAKE('T','D','E','F');
static const VmTag tag_TYPE_DECL   = TAG_MAKE('T','D','C','L');

#define VM_TAG_TYPE_ARRAY       'T','A','R','R'
#define VM_TAG_TYPE_NAME        'T','N','A','M'
#define VM_TAG_TYPE_EVT         'T','E','V','T'

static const VmTag tag_TYPE_ARRAY  = TAG_MAKE('T','A','R','R');
static const VmTag tag_TYPE_NAME   = TAG_MAKE('T','N','A','M');
static const VmTag tag_TYPE_EVT    = TAG_MAKE('T','E','V','T');
// static const VmTag tag_TYPE_OBJECT = TAG_MAKE('T','O','B','J');


// CODE SEGMENTS
#define VM_TAG_SCRIPT      'S','R','P','T'
#define VM_TAG_CONSTRUCTOR 'C','O','N','S'
#define VM_TAG_DEFAULTS    'D','F','L','T'

static const VmTag tag_PRESCRIPT   = TAG_MAKE('+','S','C','R');
static const VmTag tag_POSTSCRIPT  = TAG_MAKE('-','S','C','R');
static const VmTag tag_CONSTRUCTOR = TAG_MAKE('C','O','N','S');
static const VmTag tag_DESTRUCTOR  = TAG_MAKE('D','E','S','T');
static const VmTag tag_DEFAULTS    = TAG_MAKE('D','F','L','T');

#endif

