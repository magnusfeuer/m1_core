#ifndef __M1VM_OPCODES_HH__
#define __M1VM_OPCODES_HH__

#define op_UNDEF  0    // ---
#define op_PUSHu  1    // -- u
#define op_DROP   2    // u --
#define op_DUP    3    // u -- u u
#define op_ADDi   4    // i1 i2 -- (i1 + i2)
#define op_ADDu   5    // u1 u2 -- (u1 + u2)
#define op_ADDf   6    // f1 f2 -- (f1 + f2)
#define op_ADDs   7    // s1 s2 -- (s1 + s2)
#define op_SUBi   8    // i1 i2 -- (i1 - i2)
#define op_SUBu   9    // u1 u2 -- (u1 - u2)
#define op_SUBf   10   // f1 f2 -- (f1 - f1)
#define op_NEGi   11   // i -- -i
#define op_NEGf   12   // f -- -f
#define op_MULi   13    // i1 i2 -- (i1 * i2)
#define op_MULu   14    // u1 u2 -- (u1 * u2)
#define op_MULf   15    // f1 f2 -- (f1 * f2)
#define op_DIVi   16    // i1 i2 -- (i1 / i2)
#define op_DIVu   17    // u1 u2 -- (u1 / u2)
#define op_DIVf   18    // f1 f2 -- (f1 / f2)
#define op_REMi   19    // i1 i2 -- (i1 % i2)
#define op_REMu   20    // u1 u2 -- (u1 % u2)
#define op_NOTi   21    // i -- !i
#define op_ANDi   22    // i1 i2 -- (i1 && i2)
#define op_ORi    23    // i1 i2 -- (i1 || i2)
#define op_BANDu  24    // u1 u2 -- (u1 & u2)
#define op_BORu   25    // u1 u2 -- (u1 | u2)
#define op_BXORu  26    // u1 u2 -- (u1 ^ u2)
#define op_BNOTu  27    // u -- ~u
#define op_BSLu   28    // u1 i2 --  (u1 << i2)
#define op_BSRu   29    // u1 i2 --  (u1 >> i2)
#define op_BSRi   30    // i1 i2 --  (i1 >> i2)
#define op_PUSHs  31    // s -- 
#define op_BSLi   32    // i1 i2 --  (i1 << i2)

#define op_NEWo   33    //   -- new <class>
#define op_NEWa   34    //   -- new (array) <class> <size:32>
#define op_INEWa  36    //   -- new (array) <class> <size:32>
#define op_NEWe   37    //   -- new event <type> (symbol)

#define op_FDELo  38   // --
#define op_DEL    39   // obj   --
#define op_CNCT   40   // target source --

#define op_PUSHf  41   // -- f
#define op_PUSHi  42   // -- i

#define op_ACTIVATE 43   // obj --
#define op_SAVE     44   // --
#define op_RESTORE  45   // --
#define op_THIS     46   // -- obj
#define op_SCOPE    47   // -- obj     (  m1_scope(param) )
#define op_UPDATED  48   // evt -- i   (  check if event is updated )

#define op_CMPi     49   // i1 i2 -- Sign(i1-i2)
#define op_CMPu     50   // u1 u2 -- Sign(u1-u2)
#define op_CMPf     51   // f1 f2 -- Sign(f1-f2)
#define op_CMPo     52   // o1 o2 -- Compare(o1,o2)

#define op_JUMPlt   53   // i --    (jump +<offs> if i<0)
#define op_JUMPle   54   // i --    (jump +<offs> if i<=0)
#define op_JUMPeq   55   // i --    (jump +<offs> if i==0)
#define op_JUMPne   56   // i --    (jump +<offs> if i!=0)

#define op_JUMP     57   // --      (jump +<offs>)
#define op_JUMPi    58   // i --    (jump +<offs1> ... +<offsn)
#define op_JUMPv    59   // u --    (jump <u1>..<un> +<offs1> ... +<offsn)

#define op_PUTw     60   // u a ix --   ( a[ix]=u )

#define op_FDELe    61   // --       ( delete event )
#define op_NIL      62   // -- 0     (  0 )

#define op_MULPUTi  63  /* *= */
#define op_DIVPUTi  64  /* /= */
#define op_ADDPUTi  65  /* += */
#define op_SUBPUTi  66  /* -= */
#define op_REMPUTi  67  /* %= */
#define op_BANDPUTi 68  /* &= */
#define op_BORPUTi  69  /* |= */
#define op_BXORPUTi 70  /* ^= */
#define op_BSLPUTi  71  /* <<= */
#define op_BSRPUTi  72  /* >>= */

#define op_MULPUTu  73  /* *= */
#define op_DIVPUTu  74  /* /= */
#define op_ADDPUTu  75  /* += */
#define op_SUBPUTu  76  /* -= */
#define op_REMPUTu  77  /* %= */
#define op_BANDPUTu 78  /* &= */
#define op_BORPUTu  79  /* |= */
#define op_BXORPUTu 80  /* ^= */
#define op_BSLPUTu  81  /* <<= */
#define op_BSRPUTu  82  /* >>= */

#define op_MULPUTb  83  /* *= */
#define op_DIVPUTb  84  /* /= */
#define op_ADDPUTb  85  /* += */
#define op_SUBPUTb  86  /* -= */
#define op_REMPUTb  87  /* %= */
#define op_BANDPUTb 88  /* &= */
#define op_BORPUTb  89  /* |= */
#define op_BXORPUTb 90  /* ^= */
#define op_BSLPUTb  91  /* <<= */
#define op_BSRPUTb  92  /* >>= */

#define op_MULPUTc  93  /* *= */
#define op_DIVPUTc  94  /* /= */
#define op_ADDPUTc  95  /* += */
#define op_SUBPUTc  96  /* -= */
#define op_REMPUTc  97  /* %= */
#define op_BANDPUTc 98  /* &= */
#define op_BORPUTc  99  /* |= */
#define op_BXORPUTc 100  /* ^= */
#define op_BSLPUTc  101  /* <<= */
#define op_BSRPUTc  102  /* >>= */

#define op_MULPUTt  103  /* *= */
#define op_DIVPUTt  104  /* /= */
#define op_ADDPUTt  105  /* += */
#define op_SUBPUTt  106  /* -= */
#define op_REMPUTt  107  /* %= */
#define op_BANDPUTt 108  /* &= */
#define op_BORPUTt  109  /* |= */
#define op_BXORPUTt 110  /* ^= */
#define op_BSLPUTt  111  /* <<= */
#define op_BSRPUTt  112  /* >>= */

#define op_MULPUTf  113  /* *= */
#define op_DIVPUTf  114  /* /= */
#define op_ADDPUTf  115  /* += */
#define op_SUBPUTf  116  /* -= */
#define op_ADDPUTs  117  /* += */
#define op_NEGu     118

#define op_INCi     119
#define op_INCu     120
#define op_INCb     121
#define op_INCc     122
#define op_INCt     123
#define op_INCf     124

#define op_DECi     125
#define op_DECu     126
#define op_DECb     127
#define op_DECc     128
#define op_DECt     129
#define op_DECf     130

#define op_GETu     131    // obj ix   -- obj->elem[ix].u 
#define op_GETi     132    // obj ix   -- obj->elem[ix].i
#define op_GETf     133    // obj ix   -- obj->elem[ix].f
#define op_GETb     134    // obj ix   -- obj->octet[ix].b => u
#define op_GETc     135    // obj ix   -- obj->octet[ix].c => i
#define op_GETt     136    // obj ix   -- obj->octet[ix].t => i
#define op_GETo     137    // obj ix   -- obj->elem[ix].o
#define op_GETs     138    // obj ix   -- obj->elem[ix].s
#define op_GETe     139    // obj ix   -- obj->elem[ix].evt

#define op_PUTu     140    // o ix u --    ( o->elem[ix]=u )
#define op_PUTi     141    // o ix i --    ( o->elem[ix]=i )
#define op_PUTf     142    // o ix f --    ( o->elem[ix]=f )
#define op_PUTb     143    // o ix u --    ( o->elem[ix].b=u )
#define op_PUTc     144    // o ix i --    ( o->elem[ix].c=i )
#define op_PUTt     145    // o ix i --    ( o->elem[ix].t=i )
#define op_PUTo     146    // o ix obj --  ( o->elem[ix]=obj )
#define op_PUTs     147    // o ix str --  ( o->elem[ix]=str)

#define op_CVTiu    148    // i -- u
#define op_CVTif    149    // i -- f
#define op_CVTui    150    // u -- i
#define op_CVTuf    151    // u -- f
#define op_CVTfi    152    // f -- i
#define op_CVTfu    153    // f -- u

#define op_LTz      154    // <0  
#define op_LTEz     155    // <=0 
#define op_GTz      156    // >0  
#define op_GTEz     157    // >=0 
#define op_EQz      158    // ==0
#define op_NEQz     159    // !=0 

#define op_SWAP     160    // x y -- y x
#define op_OVER     161    // x y -- x y x
#define op_COPY     162    // :=  

#define op_ADDa     163    // array + array
#define op_ADDae    164    // array + element
#define op_ADDea    165    // element + array
#define op_SUBa     166    // array - array
#define op_SUBae    167    // array - element

#define op_ADDPUTa  168    // array += array
#define op_ADDPUTae 169    // array += element
#define op_SUBPUTa  170    // array -= array
#define op_SUBPUTae 171    // array -= element

#define op_ADDsc    172    // string + char
#define op_ADDcs    173    // char + string
#define op_SUBs     174    // string - string
#define op_SUBsc    175    // string - char
#define op_ADDPUTsc 176    // string += element
#define op_SUBPUTs  177    // string -= element
#define op_SUBPUTsc 178    // string -= string

// FREE 163 - 199 = 38

// OPT:  op_THIS, op_PUSHu, u32(ix), op_GETx
#define op_FGETu    200   // -- u   ( this->elem[ix].u )
#define op_FGETi    201   // -- i   ( this->elem[ix].i )
#define op_FGETf    202   // -- f   ( this->elem[ix].f )
#define op_FGETb    203   // -- u   ( this->elem[ix].b )
#define op_FGETc    204   // -- i   ( this->elem[ix].c )
#define op_FGETt    205   // -- i   ( this->elem[ix].t )
#define op_FGETo    207   // -- o   ( this->elem[ix].o )
#define op_FGETs    208   // -- s   ( this->elem[<arg>] )
#define op_FGETe    209   // -- evt ( this->elem[<arg>] )

// OPT:  op_THIS, op_PUSHu, u32(ix), op_PUTx 
#define op_FPUTu    210   // u   --    ( this->elem[<arg>].u=u )
#define op_FPUTi    211   // i   --    ( this->elem[<arg>].i=i )
#define op_FPUTf    212   // f   --    ( this->elem[<arg>].f=f )
#define op_FPUTb    213   // u   --    ( this->elem[<arg>].b=u )
#define op_FPUTc    214   // i   --    ( this->elem[<arg>].c=i )
#define op_FPUTt    215   // i   --    ( this->elem[<arg>].t=i )
#define op_FPUTo    216   // o   --    ( this->elem[<arg>].o=o )
#define op_FPUTs    217   // s   --    ( this->elem[<arg>].s=s )
#define op_FPUTw    218   // u   --    ( this->elem[<arg>].u=u )

// FREE 219 - 247 = 29

#define op_IPUT     248   // u   --    ( R[-1] = u )
#define op_IGET     249   // -- u      ( u = R[-1] )
#define op_CALL     250
#define op_RETURN   251  // I = <end>    ( RETURN from script )
#define op_CONTINUE 252  // I = R[-3]    ( jump to continue address )
#define op_BREAK    253  // I = R[-2]    ( jump to break address )
#define op_BEGIN    254  // BLOCK begin  ( push return stack )
#define op_END      255  // BLOCK end    ( pop return stack )
#define op_LAST     256

/* SYNTHETIC opcodes use before lint/compile */
#define op_ADD    256   /* + */
#define op_SUB    257   /* - */
#define op_NEG    258   /* - */
#define op_MUL    259   /* * */
#define op_DIV    260   /* / */
#define op_REM    261   /* % */
#define op_NOT    262   /* ! */
#define op_AND    263   /* && */
#define op_OR     264   /* || */
#define op_BAND   265   /* & */
#define op_BOR    266   /* | */
#define op_BXOR   267   /* ^ */
#define op_BNOT   268   /* ~ */
#define op_BSL    269   /* << */
#define op_BSR    270   /* >> */

#define op_LT     271   /* <  */
#define op_LTE    272   /* <= */
#define op_GT     273   /* >  */
#define op_GTE    274   /* >= */
#define op_EQ     275   /* == */
#define op_NEQ    276   /* != */
#define op_PUT    277   /*  = */
#define op_MULPUT 278   /* *= */
#define op_DIVPUT 279   /* /= */
#define op_ADDPUT 280   /* += */
#define op_SUBPUT 281   /* -= */
#define op_REMPUT 282   /* %= */
#define op_BANDPUT 283  /* &= */
#define op_BORPUT  284  /* |= */
#define op_BXORPUT 285   /* ^= */
#define op_BSLPUT 286   /* <<= */
#define op_BSRPUT 287   /* >>= */

#define op_TRIGGER  288   /* *x */
#define op_SUPRESS 289    /* **x */

#define op_FLD    291   /* obj . field */
#define op_ELEM   292   /* obj [ index ] */
#define op_RANGE  293   /* i .. j */
#define op_SEQ    294   /* expr , expr */
#define op_PSTINC 295  /* expr ++ */
#define op_PREINC 296  /* ++ expr */
#define op_PSTDEC 297  /* expr -- */
#define op_PREDEC 298  /* -- expr */
#define op_POS    299  /* + expr  */
#define op_ELEMS  300  /* obj [ index : index [ : index ]] */
/* 300-307 deleted */

#define op_EQL    308   /* =:= */
#define op_NQL    309   /* =!= */
#define op_CVT    320
#define op_CAST   321
#define op_OBJ    323
#define op_ARR    324
#define op_ID     325
#define op_BID    326
#define op_IX     327
#define op_COND   328  /* ? : */
#define op_PROG2  329  /* (pre); (value); (post) */

#define op_CNSTb  330
#define op_CNSTc  331
#define op_CNSTt  332
#define op_CNSTi  333
#define op_CNSTu  334
#define op_CNSTf  335
#define op_CNSTs  336
#define op_CNSTn  337

#define op_VAR    338   /* local variable */
#define op_INIT   344   /* = temporary op for constant init */
//
// Helpful Construction macros
//
#define VM_UNSIGNED32(x) \
        (((x) >> 24) & 0xff),			\
	(((x) >> 16) & 0xff),			\
	(((x) >> 8) & 0xff),			\
	((x) & 0xff)

#define VM_UNSIGNED64(x) \
        (((x) >> 56) & 0xff),			\
        (((x) >> 48) & 0xff),			\
        (((x) >> 40) & 0xff),			\
        (((x) >> 32) & 0xff),			\
        (((x) >> 24) & 0xff),			\
	(((x) >> 16) & 0xff),			\
	(((x) >> 8) & 0xff),			\
	((x) & 0xff)

#define VM_SIGNED32(x) \
        (((x) >> 24) & 0xff),			\
	(((x) >> 16) & 0xff),			\
	(((x) >> 8) & 0xff),			\
	((x) & 0xff)

#define VM_SIGNED64(x) \
        (((x) >> 56) & 0xff),			\
        (((x) >> 48) & 0xff),			\
        (((x) >> 40) & 0xff),			\
        (((x) >> 32) & 0xff),			\
        (((x) >> 24) & 0xff),			\
	(((x) >> 16) & 0xff),			\
	(((x) >> 8) & 0xff),			\
	((x) & 0xff)

static inline u_int32_t iFLOAT32(float x)
{
    union { float f32; u_int32_t u32; } u;
    u.f32 = x;
    return u.u32;
}

static inline u_int64_t iFLOAT64(double x)
{
    union { double f64; u_int64_t u64; } u;
    u.f64 = x;
    return u.u64;
}

#define VM_FLOAT32(x) VM_UNSIGNED32(iFLOAT32((x)))
#define VM_FLOAT64(x) VM_UNSIGNED64(iFLOAT64((x)))


#define VM_STRING(n, ...) op_PUSHs, (n), __VA_ARGS__
#define VM_FLOAT(f)  op_PUSHf, VM_FLOAT32(f)
#define VM_SIGNED(i) op_PUSHu, VM_SIGNED32(i)
#define VM_UNSIGNED(u) op_PUSHu, VM_UNSIGNED32(u)
#define VM_TRUE  op_PUSHi, VM_UNSIGNED32(1)
#define VM_FALSE op_PUSHi, VM_UNSIGNED32(0)
#define VM_NIL   op_PUSHu, VM_UNSIGNED32(0)

#define VM_TYPE_BYTE             1, 'b'
#define VM_TYPE_CHAR             1, 'c'
#define VM_TYPE_BOOL             1, 't'
#define VM_TYPE_SIGNED           1, 'i'
#define VM_TYPE_UNSIGNED         1, 'u'
#define VM_TYPE_FLOAT            1, 'f'
#define VM_TYPE_STRING           1, 's'


#define VM_FIELD_OBJECT_GET(offs) \
    op_SELF, VM_UNSIGNED(offs), op_GETo

#define VM_FIELD_GET(offs) \
    op_SELF, VM_UNSIGNED(offs), op_GETu

#define VM_FIELD_OBJECT_PUT(offs) \
    op_SELF, VM_UNSIGNED(offs), op_GETo

#define VM_FIELD_PUTw(offs) \
    op_SELF, VM_UNSIGNED(offs), op_PUTw

#define VM_FIELD_PUTu(offs) \
    op_SELF, VM_UNSIGNED(offs), op_PUTu

// v -- (update array field at index)
#define VM_FIELD_ARRAY_PUT(offs, index) \
    VM_FIELD_GET(offs), VM_UNSIGNED(index), op_PUTu

// obj -- (update object array / array ref at index)
#define VM_FIELD_OBJECT_ARRAY_PUT(offs, index) \
    VM_FIELD_OBJECT_GET(offs), VM_UNSIGNED(index), op_PUTo

#define VM_PRINT_NL  VM_STRING(2,'\r','\n'), op_PRINTs, op_POPs

#define VM_INIT_UNSIGNED(pos,value) \
    VM_UNSIGNED(value), VM_FIELD_PUTu(pos)

#define VM_INIT_SIGNED(pos,value) \
    VM_UNSIGNED(value), VM_FIELD_PUTu(pos)

#define VM_INIT_FLOAT(pos,value) \
    VM_FLOAT(value), VM_FIELD_PUTu(pos)

#define VM_INIT_CHAR(pos,value) \
    VM_SIGNED(value), VM_FIELD_PUTw(pos)

#define VM_OBJECT(n,...) \
    op_NEWo, (n), __VA_ARGS__

// NOTE VM_FIELD_PUT! NOT VM_FIELD_OBJECT_PUT (init is special)
#define VM_INIT_OBJECT(pos,...) \
    op_NEWo, __VA_ARGS__, VM_FIELD_PUTw(pos)

#define VM_INIT_BYTE_ARRAY(pos,size) \
    op_NEWa, VM_TYPE_BYTE, VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)

#define VM_INIT_BOOL_ARRAY(pos,size) \
    op_NEWa, VM_TYPE_BOOL, VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)

#define VM_INIT_CHAR_ARRAY(pos,size) \
    op_NEWa,VM_TYPE_CHAR, VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)

#define VM_INIT_SIGNED_ARRAY(pos,size) \
    op_NEWa,VM_TYPE_SIGNED, VM_UNSIGNED32(size),VM_FIELD_PUTw(pos)

#define VM_INIT_UNSIGNED_ARRAY(pos,size) \
    op_NEWa,VM_TYPE_UNSIGNED,VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)

#define VM_INIT_FLOAT_ARRAY(pos,size) \
    op_NEWa,VM_TYPE_FLOAT, VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)

#define VM_INIT_STRING_ARRAY(pos,size) \
    op_NEWa, VM_TYPE_STRING, VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)

#define VM_INIT_OBJECT_ARRAY(pos,size,...)  \
    op_NEWa,__VA_ARGS__, VM_UNSIGNED32(size), VM_FIELD_PUTw(pos)


// Use PUTu to avoid reference count in constructor
#define VM_INIT_NIL(pos) \
    VM_NIL, VM_FIELD_PUTw(pos)

// Release the Field at position pos
// FDELETE (as opposed to DELETE) will also nuke the position to NIL
#define VM_FINAL(pos) \
    op_FDELo, VM_UNSIGNED32(pos)

#define VM_FINALe(pos) \
    op_FDELe, VM_UNSIGNED32(pos)


#endif
