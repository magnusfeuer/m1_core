// Test function.

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/types.h>

#include "m1vm.hh"
#include "m1vm_opcodes.hh"
#include "m1vm_format.hh"
#include "epic.h"

#define TYPE_NAME_POINT   7, '(','P','o','i','n','t',')'
#define TYPE_NAME_RECT    6, '(','R','e','c','t',')'
#define TYPE_NAME_PIXEL   7, '(','P','i','x','e','l',')'

#define TYPE_NAME_FLOAT_2  4, 'f','[','2',']'
#define TYPE_NAME_POINT_2  10, '(','P','o','i','n','t',')','[','2',']'
#define TYPE_NAME_BYTE_4   4, 'b','[','4',']'

#define TYPE_NAME_EVENT_FLOAT 2, 'f','E'

#define TYPE_NAME_TUnsigned 9, 'T','U','n','s','i','g','n','e','d'
#define TYPE_NAME_TSigned   7, 'T','S','i','g','n','e','d'
#define TYPE_NAME_TFloat    6, 'T','F','l','o','a','t'
#define TYPE_NAME_TString   7, 'T','S','t','r','i','n','g'
#define TYPE_NAME_TObject   7, 'T','O','b','j','e','c','t'
#define TYPE_NAME_TSymbol   7, 'T','S','y','m','b','o','l'
#define TYPE_NAME_TBArray   7, 'T','B','A','r','r','a','y'
#define TYPE_NAME_TWArray   7, 'T','W','A','r','r','a','y'
#define TYPE_NAME_TOArray   7, 'T','O','A','r','r','a','y'
#define TYPE_NAME_TMArray   7, 'T','M','A','r','r','a','y'
#define TYPE_NAME_TJump     5, 'T','J','u','m','p'


// Check if condition on stack == 0 then print "OK" otherwise print "ERROR"
#define VM_VERIFY \
    op_JUMPeq, VM_UNSIGNED32(4),		\
	VM_STRING(5, 'E','R','R','O','R'),	\
	op_JUMP, VM_UNSIGNED32(2),		\
	VM_STRING(2, 'O','K'),			\
	op_PRINTs, op_POPs, VM_PRINT_NL


// Load if not defined | unresolved
CType* LoadResolve(char* typeName, u_int8_t* code, size_t code_len)
{
    CType* t;

    if ((t=m1TypeLookup(typeName)) == NULL) {
	m1Load(code, code_len);
	if ((t=m1TypeLookup(typeName)) == NULL)
	    return NULL;
    }
    return t;
}

void Release(char* name)
{
    CType* t;

    if ((t = m1TypeLookup(name)) != NULL)
	m1TypeUnRegister(t);
}


/* Scalar type tests ( arithmetic etc) */

// 1. TestUnsigned
CType* tUnsigned(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,                // The magic
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TUnsigned,
	VM_UNSIGNED32(0),            // Number of object FIELDS

	VM_TAG_CODE, VM_TAG_SCRIPT,
	// 7+13 = 20
	VM_UNSIGNED(7), VM_UNSIGNED(13), op_ADDu,
	VM_UNSIGNED(20), op_CMPu,
	VM_VERIFY,

	// 13-7 = 6
	VM_UNSIGNED(13), VM_UNSIGNED(7), op_SUBu,
	VM_UNSIGNED(6), op_CMPu,
	VM_VERIFY,

	// 13*7 = 91
	VM_UNSIGNED(13), VM_UNSIGNED(7), op_MULu,  
	VM_UNSIGNED(91), op_CMPu,
	VM_VERIFY,

	// 91 / 7  = 13
	VM_UNSIGNED(91), VM_UNSIGNED(7), op_DIVu,
	VM_UNSIGNED(13), op_CMPu,
	VM_VERIFY,

	// 91 % 6  = 1
	VM_UNSIGNED(91), VM_UNSIGNED(6), op_REMu,
	VM_UNSIGNED(1), op_CMPu,
	VM_VERIFY,

	// 10 << 5 == 320
	VM_UNSIGNED(10), VM_SIGNED(5), op_BSLu,
	VM_UNSIGNED(320), op_CMPu,
	VM_VERIFY,

	// 101 >> 5 == 3
	VM_UNSIGNED(101), VM_SIGNED(5), op_BSRu,
	VM_UNSIGNED(3), op_CMPu,
	VM_VERIFY,

	// 10 & 3 == 2
	VM_UNSIGNED(10), VM_UNSIGNED(3), op_BANDu,
	VM_UNSIGNED(2), op_CMPu,
	VM_VERIFY,

	// 10 | 3 == 11
	VM_UNSIGNED(10), VM_UNSIGNED(3), op_BORu,
	VM_UNSIGNED(11), op_CMPu,
	VM_VERIFY,

	// 10 ^ 3 == 9
	VM_UNSIGNED(10), VM_UNSIGNED(3), op_BXORu,
	VM_UNSIGNED(9), op_CMPu,
	VM_VERIFY,

	// ~10 == 4294967285
	VM_UNSIGNED(10), op_BNOTu,
	VM_UNSIGNED(4294967285U), op_CMPu,
	VM_VERIFY,

    	op_END
    };
    return LoadResolve("TUnsigned", Code, sizeof(Code));
}


// 2. TSigned
CType* tSigned(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,                // The magic
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TSigned,
	VM_UNSIGNED32(0),            // Number of object FIELDS

	VM_TAG_CODE, VM_TAG_SCRIPT,
	// -7+13 = 6
	VM_SIGNED(-7), VM_SIGNED(13), op_ADDi,
	VM_SIGNED(6), op_CMPi,
	VM_VERIFY,

	// 7-13 = -6
	VM_SIGNED(7), VM_SIGNED(13), op_SUBi,
	VM_UNSIGNED(-6), op_CMPi,
	VM_VERIFY,

	// 13*-7 = -91
	VM_SIGNED(13), VM_SIGNED(-7), op_MULi,  
	VM_SIGNED(-91), op_CMPi,
	VM_VERIFY,

	// -91 / -7  = 13
	VM_SIGNED(-91), VM_SIGNED(-7), op_DIVi,
	VM_SIGNED(13), op_CMPi,
	VM_VERIFY,

	// 91 % 6  = 1
	VM_SIGNED(91), VM_SIGNED(6), op_REMi,
	VM_SIGNED(1), op_CMPi,
	VM_VERIFY,

	// -10 << 5 == -320
	VM_SIGNED(-10), VM_SIGNED(5), op_BSLu, // Yes BSLu is correct.
	VM_SIGNED(-320), op_CMPi,
	VM_VERIFY,

	// -101 >> 5 == -4
	VM_SIGNED(-101), VM_SIGNED(5), op_BSRi,
	VM_SIGNED(-4), op_CMPi,
	VM_VERIFY,

    	op_END
    };
    return LoadResolve("TSigned", Code, sizeof(Code));
}


// 3. TestFloat
CType* tFloat(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,                // The magic
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TFloat,
	VM_UNSIGNED32(0),            // Number of object FIELDS

	VM_TAG_CODE, VM_TAG_SCRIPT,

	// 7.0 + 13.2 = 20.2
	VM_FLOAT(7.0), VM_FLOAT(13.2), op_ADDf,
	VM_FLOAT(20.2), op_CMPf,
	VM_VERIFY,

	// 7.0 - 13.2 = -6.2
	VM_FLOAT(7.0), VM_FLOAT(13.2), op_SUBf,
	VM_FLOAT(-6.2), op_CMPf,
	VM_VERIFY,

	// 7.0*13.2 = 92.4
	VM_FLOAT(7.0), VM_FLOAT(13.2), op_MULf,
	VM_FLOAT(92.4), op_CMPf,
	VM_VERIFY,

	// 39.0/6.0 = 6.5
	VM_FLOAT(39.0), VM_FLOAT(6.0), op_DIVf,
	VM_FLOAT(6.5), op_CMPf,
	VM_VERIFY,

    	op_END
    };
    return LoadResolve("TFloat", Code, sizeof(Code));
}

// TString
CType* tString(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,                // The magic
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TString,
	VM_UNSIGNED32(0),            // Number of object FIELDS

	VM_TAG_CODE, VM_TAG_SCRIPT,
	VM_STRING(6, 'h','e','l','l','o',' '),
	VM_STRING(6, 'w','o','r','l','d','\n'),
	op_ADDs, op_PRINTs,  op_POPs,

	op_END
    };
    return LoadResolve("TString", Code, sizeof(Code));
}

//
// Octet Array test
//
// type TBArray {
//    (0) byte    a[3];
//    (1) char    b[3];
//    (2) boolean c[3];
//
//    script {
//       a[0] = 1; a[2] = 3;
//       b[1] = 'B'; b[2] = 'C';
//       c[0] = true;
//    }
// }

CType* tBArray(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,         // The magic

	// TYPE DECLARATIONS
	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_BYTE, VM_UNSIGNED32(3),  // byte[3]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_CHAR, VM_UNSIGNED32(3), // char[3]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_BOOL, VM_UNSIGNED32(3), // boolean[3]

        // OBJECT DEFINITION
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT, 
	TYPE_NAME_TBArray,
	VM_UNSIGNED32(3),     // Number of object FIELDS
	Q_PUBLIC, 1, 'a',  7,'b','y','t','e','[','3',']',
	Q_PUBLIC, 1, 'b',  7,'c','h','a','r','[','3',']',
	Q_PUBLIC, 1, 'c',  7,'b','o','o','l','[','3',']',

	// CODE DEFINITION AREA
	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	VM_INIT_BYTE_ARRAY(0,3),                  // a=byte[3]
	VM_INIT_CHAR_ARRAY(1,3),                  // b=char[3]
	VM_INIT_BOOL_ARRAY(2,3),                  // c=boolean[3]
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,
	// Set a[0]=1;
	VM_UNSIGNED(1),VM_FIELD_OBJECT_GET(0),VM_UNSIGNED(0),op_PUTu,
	VM_UNSIGNED(3),VM_FIELD_OBJECT_GET(0),VM_UNSIGNED(2),op_PUTu,

	VM_SIGNED('B'),VM_FIELD_OBJECT_GET(1),VM_UNSIGNED(1),op_PUTu,
	VM_SIGNED('C'),VM_FIELD_OBJECT_GET(1),VM_UNSIGNED(2),op_PUTu,

	VM_TRUE,VM_FIELD_OBJECT_GET(2),VM_UNSIGNED(0),op_PUTu,

	op_SELF, op_PRINTo, VM_PRINT_NL, op_POPo,

	op_END };
    return LoadResolve("TBArray", Code, sizeof(Code));
}

//
// Word Array test
//
// type TWArray {
//    (0) unsigned a[3];
//    (1) signed   b[3];
//    (2) float    c[3];
//
//    script {
//       a[0] = 99; a[1] = 999; a[2] = 9999;
//       b[0] = -100; b[1] = -1000; c[2] = -10000;
//       c[0] = 1.0; c[1]=2.0; c[2]=3.0;
//    }
// }

CType* tWArray(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,         // The magic

	// TYPE DECLARATIONS
	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_UNSIGNED, VM_UNSIGNED32(3),

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_SIGNED, VM_UNSIGNED32(3),

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_FLOAT, VM_UNSIGNED32(3),

        // OBJECT DEFINITION
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TWArray,
	VM_UNSIGNED32(3),     // Number of object FIELDS
	Q_PUBLIC, 1, 'a',  11,'u','n','s','i','g','n','e','d','[','3',']',
	Q_PUBLIC, 1, 'b',  9,'s','i','g','n','e','d','[','3',']',
	Q_PUBLIC, 1, 'c',  8,'f','l','o','a','t','[','3',']', 

	// CODE DEFINITION AREA
	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	VM_INIT_UNSIGNED_ARRAY(0,3),           // a=unsigned[3]
	VM_INIT_SIGNED_ARRAY(1,3),             // b=signed[3]
	VM_INIT_FLOAT_ARRAY(2,3),              // c=float[3]
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,
        // a[0] = 99; a[1] = 999; a[2] = 9999;
	VM_UNSIGNED(99),VM_FIELD_OBJECT_GET(0),VM_UNSIGNED(0),op_PUTu,
	VM_UNSIGNED(999),VM_FIELD_OBJECT_GET(0),VM_UNSIGNED(1),op_PUTu,
	VM_UNSIGNED(9999),VM_FIELD_OBJECT_GET(0),VM_UNSIGNED(2),op_PUTu,
        // b[0] = -100; b[1] = -1000; c[2] = -10000;
	VM_SIGNED(-100),VM_FIELD_OBJECT_GET(1),VM_UNSIGNED(0),op_PUTi,
	VM_SIGNED(-1000),VM_FIELD_OBJECT_GET(1),VM_UNSIGNED(1),op_PUTi,
	VM_SIGNED(-10000),VM_FIELD_OBJECT_GET(1),VM_UNSIGNED(2),op_PUTi,

        // c[0] = 1.0; c[1]=2.0; c[2]=3.0;
	VM_FLOAT(1.0),VM_FIELD_OBJECT_GET(2),VM_UNSIGNED(0),op_PUTf,
	VM_FLOAT(2.0),VM_FIELD_OBJECT_GET(2),VM_UNSIGNED(1),op_PUTf,
	VM_FLOAT(3.0),VM_FIELD_OBJECT_GET(2),VM_UNSIGNED(2),op_PUTf,

	op_SELF, op_PRINTo, op_POPo,

	op_END };

    return LoadResolve("TWArray", Code, sizeof(Code));
}

//
// Object test
//
// type TObject {
//    (0) public unsigned a = 1;
//    (1) public float    b = 2.23;
//    (2) public signed   c = -2;
//    (3) public char     d = 'A'
//

CType* tObject(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,         // The magic
        // OBJECT DEFINITION
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT, 
	TYPE_NAME_TObject,

	VM_UNSIGNED32(4),     // Number of object FIELDS
	Q_PUBLIC, 1, 'a', VM_TYPE_UNSIGNED,
	Q_PUBLIC, 1, 'b', VM_TYPE_FLOAT,
	Q_PUBLIC, 1, 'c', VM_TYPE_SIGNED,
	Q_PUBLIC, 1, 'd', VM_TYPE_CHAR,

	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	VM_INIT_UNSIGNED(0, 1),     // a=1
	VM_INIT_FLOAT(1, 2.23),     // b=2.23
	VM_INIT_SIGNED(2, -2),      // c=-2
	VM_INIT_CHAR(3, 'A'),       // d='A'
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,
	op_SELF, op_PRINTo, op_POPo,

	op_END };

    return LoadResolve("TObject", Code, sizeof(Code));
}



//
// Symbol test
//
// type TSymbol {
//    (0) unsigned a = 1;
//    (1) event float b = 0.1;
//    (2) event float c = 0.2;
//

CType* tSymbol(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,         // The magic

	// Declare event float type
	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_EVT, VM_TYPE_FLOAT,

        // OBJECT DEFINITION 
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TSymbol,

	VM_UNSIGNED32(3),     // Number of object FIELDS
	Q_PUBLIC, 1, 'a', VM_TYPE_UNSIGNED,
	Q_PUBLIC, 1, 'b', TYPE_NAME_EVENT_FLOAT,
	Q_PUBLIC, 1, 'c', TYPE_NAME_EVENT_FLOAT,

	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	VM_INIT_UNSIGNED(0, 1),    // a=1
	op_NEWe, TYPE_NAME_EVENT_FLOAT, VM_FIELD_PUTw(1), // b=event float
	op_NEWe, TYPE_NAME_EVENT_FLOAT, VM_FIELD_PUTw(2), // c=event float
	VM_INIT_FLOAT(1, 0.1),     // b=0.1
	VM_INIT_FLOAT(2, 0.2),     // c=0.2
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,
	op_SELF, op_PRINTo, op_POPo,

	op_END };

    return LoadResolve("TSymbol", Code, sizeof(Code));
}

//
// Object Array test
//
// type Pixel byte[4];
//
// type TArray {
//    (0) float  a[3];
//    (1) Rect   b;
//    (2) Rect   c[3]
//    (3) Point* d[3]
//    (4) Pixel  e
//
//    script {
//       a[5] = 3.14;
//       d[1] = Point { x=1, y=2 };
//       e[0] = 127; e[1] = 255; e[2]=0; e[3]=255;
//    }
// }

CType* tOArray(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,         // The magic

	// TYPE DECLARATIONS
	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_FLOAT, VM_UNSIGNED32(3),  // float[3]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, TYPE_NAME_RECT, VM_UNSIGNED32(3),  // Rect[3]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, TYPE_NAME_POINT, VM_UNSIGNED32(3),  // Point[3]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_BYTE, VM_UNSIGNED32(4),    // byte[4]

	// TYPE DEFINITIONS
	VM_TAG_TYPE, VM_TAG_TYPE_DEF,
	TYPE_NAME_PIXEL, TYPE_NAME_BYTE_4,

        // OBJECT DEFINITION
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT, 
	TYPE_NAME_TOArray,
	VM_UNSIGNED32(5),     // Number of object FIELDS
	Q_PRIVATE, 1, 'a', 8, 'f','l','o','a','t','[','3',']',
	Q_PRIVATE, 1, 'b', 4, 'R','e','c','t',
	Q_PRIVATE, 1, 'c', 7, 'R','e','c','t','[','3',']',
	Q_PRIVATE, 1, 'd', 8, 'P','o','i','n','t','[','3',']',
	Q_PRIVATE, 1, 'e', 5, 'P','i','x','e','l',
	
	// CODE DEFINITION AREA
	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	VM_INIT_FLOAT_ARRAY(0,3),                          // a=float[3]
	VM_INIT_OBJECT(1, 4,'R','e','c','t'),              // b=Rect{}
	VM_INIT_OBJECT_ARRAY(2, 3, 4,'R','e','c','t'),     // c=Rect[3]
	VM_INIT_OBJECT_ARRAY(3, 3, 5,'P','o','i','n','t'), // d=Point[3]
	VM_INIT_OBJECT(4, 5,'P','i','x','e','l'),          // e=Pixel
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,
	// Set a[2]=3.14

/*
	VM_FLOAT(3.14),
	VM_FIELD_OBJECT_GET(0),     // Get float[3] array
	VM_UNSIGNED(2),
	op_PUTf,


	// Set d[1] = Point { x=1, y=2 }
	VM_OBJECT(5,'P','o','i','n','t'),   // Point --
	VM_FIELD_OBJECT_GET(3),             // Point d[] --
	VM_UNSIGNED(1),                     // Point d[] 1 --
	op_PUTo,                            // --   ( d[1] = Point )

	VM_SIGNED(1),VM_FIELD_OBJECT_GET(3),VM_UNSIGNED(1),  // 1 d[] 1 --
	op_GETo,VM_UNSIGNED(0),                              // 1 d[1] 0 --
	op_PUTu,                                             // -- Point.x = 1

	VM_SIGNED(2),VM_FIELD_OBJECT_GET(3),VM_UNSIGNED(1),  // 2 d[] 1 --
	op_GETo,VM_UNSIGNED(1),                              // 2 d[1] 1 --
	op_PUTu,                                             // -- Point.y = 1
*/
	op_SELF, op_PRINTo, op_POPo,

	op_END };

    return LoadResolve("TOArray", Code, sizeof(Code));
}

//
//  Multi dimensional Array test
//
// type TArray {
//    (0) float  a[3][2];
//    (1) Point  b[3][2];
//    (2) Point* c[3][2];
//    (3) Pixel  d[2]
//
//    script {
//       a[1][1] = 1.0;
//       b[1][1] = Point { x=1, y=1 }
//       c[1][1] = Point { x=2, y=2 }
//       d[0][1] = 255;
//       d[1][0] = 255;
//    }
// }

CType* tMArray(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,         // The magic

	// TYPE DECLARATIONS
	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_FLOAT, VM_UNSIGNED32(2),     // float[2]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, TYPE_NAME_FLOAT_2, VM_UNSIGNED32(3), // float[3][2]
	
	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, TYPE_NAME_POINT, VM_UNSIGNED32(2),  // Point[2]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, TYPE_NAME_POINT_2, VM_UNSIGNED32(3),  // Point[3][2]

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, VM_TYPE_BYTE, VM_UNSIGNED32(4),    // byte[4]

	// TYPE DEFINITIONS
	VM_TAG_TYPE, VM_TAG_TYPE_DEF,
	TYPE_NAME_PIXEL, TYPE_NAME_BYTE_4,

	VM_TAG_TYPE, VM_TAG_TYPE_DECL,
	VM_TAG_TYPE_ARRAY, TYPE_NAME_PIXEL, VM_UNSIGNED32(2),    // Pixel[2]


        // OBJECT DEFINITION
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TMArray,
	VM_UNSIGNED32(3),     // Number of object FIELDS
	Q_PRIVATE, 1, 'a', 7, 'f','[','3',']','[','2',']',
	Q_PRIVATE, 1, 'b', 13, '(','P','o','i','n','t',')','[','3',']','[','2',']',
	Q_PRIVATE, 1, 'c', 10, '(','P','i','x','e','l',')','[','2',']',
	
	// CONSTRUCTOR
	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	// a=float[3][2]
	op_NEWa,TYPE_NAME_FLOAT_2, VM_UNSIGNED32(3), VM_FIELD_PUTw(0),
	// b=Point[3][2]
	op_NEWa,TYPE_NAME_POINT_2, VM_UNSIGNED32(3), VM_FIELD_PUTw(1),
	// c=Pixel[2]
	op_NEWa,TYPE_NAME_PIXEL, VM_UNSIGNED32(2), VM_FIELD_PUTw(2),
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,
	// Set a[1][1] = 1.0
	VM_FLOAT(1.0),
	VM_FIELD_OBJECT_GET(0),         // Get float[3][2] array
	VM_UNSIGNED(1),	op_GETo,        // Get float[2] array
	VM_UNSIGNED(1),
	op_PUTf,

	op_SELF, op_PRINTo, op_POPo,

	op_END };
    return LoadResolve("TMArray", Code, sizeof(Code));
}


// TJump -- Jumps
CType* tJump(void)
{
    u_int8_t Code[] = {
	VM_TAG_MAGIC,                // The magic
	VM_TAG_TYPE, VM_TAG_TYPE_OBJECT,
	TYPE_NAME_TJump,
	VM_UNSIGNED32(1),            // Number of object FIELDS
	Q_PUBLIC, 6, 's','e','l','e','c','t', VM_TYPE_UNSIGNED,

	VM_TAG_CODE,
	VM_TAG_CONSTRUCTOR,
	VM_INIT_UNSIGNED(0, 0),     // select=0
	op_END,

	VM_TAG_CODE, VM_TAG_SCRIPT,

	// switch 0-3
	VM_FIELD_GET(0),           // -- select
	op_JUMPi, VM_UNSIGNED32(4), 
	VM_UNSIGNED32(4),VM_UNSIGNED32(8),VM_UNSIGNED32(12),VM_UNSIGNED32(16),
	VM_STRING(1, 'X'), op_JUMP,VM_UNSIGNED32(14),
	VM_STRING(1, '0'), op_JUMP,VM_UNSIGNED32(10),
	VM_STRING(1, '1'), op_JUMP,VM_UNSIGNED32(6),
	VM_STRING(1, '2'), op_JUMP,VM_UNSIGNED32(2),
	VM_STRING(1, '3'),
	op_PRINTs, op_POPs, VM_PRINT_NL,
	
	// switch 10,20,30,40
 	VM_FIELD_GET(0),           // -- select
 	op_JUMPv, VM_UNSIGNED32(4),
	// Value array MUST be sorted (compiler will fix this of course)
 	VM_UNSIGNED32(10),VM_UNSIGNED32(20),VM_UNSIGNED32(30),VM_UNSIGNED32(40),
 	VM_UNSIGNED32(4),VM_UNSIGNED32(8),VM_UNSIGNED32(12),VM_UNSIGNED32(16),
 	VM_STRING(1, 'X'),    op_JUMP,VM_UNSIGNED32(14),
 	VM_STRING(2,'1','0'), op_JUMP,VM_UNSIGNED32(10),
 	VM_STRING(2,'2','0'), op_JUMP,VM_UNSIGNED32(6),
 	VM_STRING(2,'3','0'), op_JUMP,VM_UNSIGNED32(2),
 	VM_STRING(2,'4','0'),
 	op_PRINTs, op_POPs, VM_PRINT_NL,
     	op_END
    };
    return LoadResolve("TJump", Code, sizeof(Code));
}


#define CASE(i) (1 << (i-1))

void run_test(CType* t)
{
    UData object = t->produce(NULL);
    CExecutable* test = (CExecutable*) object.o;

    test->retain();
    test->execute(0);
    test->release();
}

void test(unsigned long tcase) 
{
    if (tcase & CASE(1)) {
	fprintf(stderr, "Unsigned test\n");
	run_test(tUnsigned());
    }

    if (tcase & CASE(2)) {
	fprintf(stderr, "Signed test\n");
	run_test(tSigned());
    }

    if (tcase & CASE(3)) {
	fprintf(stderr, "Float test\n");
	run_test(tFloat());
    }

    if (tcase & CASE(4)) {
	fprintf(stderr, "String test\n");
	run_test(tString());
    }

    if (tcase & CASE(5)) {
	fprintf(stderr, "Object test\n");
	run_test(tObject());
    }

    if (tcase & CASE(6)) {
	fprintf(stderr, "Byte array test\n");
	run_test(tBArray());
    }

    if (tcase & CASE(7)) {
	fprintf(stderr, "Word array test\n");
	run_test(tWArray());
    }

    if (tcase & CASE(8)) {
	fprintf(stderr, "Object array test\n");
	run_test(tOArray());
    }

    if (tcase & CASE(9)) {
	fprintf(stderr, "Multi dimensional array test\n");
	run_test(tMArray());
    }

    if (tcase & CASE(10)) {
	CType* t;
	CExecutable* test;
	UData object;
	int i;

	fprintf(stderr, "Jump test\n");
	t = tJump();
	object = t->produce(NULL);
	object.o->retainThis();
	test = (CExecutable*) object.o;
	for (i = 0; i < 42; i++) {
	    UData d;
	    d.u = i;
	    // Use API to set field value by name */
	    test->put("select", d);
	    test->execute(0);
	}
	test->release();
    }

    if (tcase & CASE(11)) {
	CType* t;
	CExecutable* test1;
	CExecutable* test2;
	UData object;
	UData d;
	
	t = tSymbol();
	object = t->produce(NULL);
	object.o->retainThis();
	test1 = (CExecutable*) object.o;

	object = t->produce(NULL);
	object.o->retainThis();
	test2 = (CExecutable*) object.o;

	test1->connect("b", test2, "c");
	d.f = 0.3;
	test2->put("c", d);

	test1->execute(0);
	test2->execute(0);
	test1->release();
	test2->release();
    }

    Release("TUnsigned");
    Release("TSigned");
    Release("TFloat");
    Release("TString");
    Release("TBArray");
    Release("TArray");
    Release("TObject");
    Release("TJump");
    Release("TSymbol");
}

void usage()
{
    fprintf(stderr, "m1t: usage: m1t [-L path] [-d] case ...\n");
    exit(1);
}


int main(int argc, char** argv)
{
    char* lib_path[100];
    int il = 0;
    int c;
    int n;
    unsigned long tcase = 0;

    cout.setf(ios::unitbuf);

    while((c = getopt(argc, argv, "L:d")) != -1) {
	switch (c) {
	case 'd':
	    m1_debug = 1;
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
    epic_init(EPIC_SIMD_AUTO);

    if (argc == 0)
	tcase = 0xffffffff;
    else {
	int i;
	for (i = 0; i < argc; i++) 
	    tcase |= (1 << (atoi(argv[i])-1));
    }
    test(tcase);
    n = m1DeleteZeroObjects();
    fprintf(stderr, "Cleaned up %d objects\n", n);
#ifdef DEBUG
    m1_dump_stat(&cerr);
#endif    
}
