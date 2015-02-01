%%
%% EPIC pixmaps
%%

-define(EPIC_SERVER, epic_serv).
-define(EPIC_PORT,   epic_port).
-define(EPIC_REG,    epic_reg).

-define(REPLY_OK,    1).
-define(REPLY_ERROR, 2).
-define(REPLY_EVENT, 3).

-define(BOOLEAN,        0).
-define(UINT8,          1).
-define(UINT16,         2).
-define(UINT32,         3).
-define(UINT64,         4).
-define(STRING1,        5).
-define(LIST,           6).
-define(LIST_END,       7).
-define(TUPLE,          8).
-define(TUPLE_END,      9).
-define(ATOM,           10).
-define(BINARY,         11).
-define(INT8,           12).
-define(INT16,          13).
-define(INT32,          14).
-define(INT64,          15).
-define(FLOAT32,        16).
-define(FLOAT64,        17).
-define(STRING4,        18).

-define(EEVENT_KEY_PRESS,        16#0001).
-define(EEVENT_KEY_RELEASE,      16#0002).
-define(EEVENT_POINTER_MOTION,   16#0004).
-define(EEVENT_BUTTON_PRESS,     16#0008).
-define(EEVENT_BUTTON_RELEASE,   16#0010).
-define(EEVENT_FOCUS,            16#0060).
-define(EEVENT_FOCUS_IN,         16#0020).
-define(EEVENT_FOCUS_OUT,        16#0040).

%%
%% Commands
%%
-define(EPIC_NOOP,               16#0001).

-define(EBACKEND_CREATE,         16#1000).
-define(EBACKEND_DESTROY,        16#1001).
-define(EBACKEND_LIST,           16#1011).

-define(EWINDOW_CREATE,          16#2000).
-define(EWINDOW_DESTROY,         16#2001).
-define(EWINDOW_ATTACH,          16#2002).
-define(EWINDOW_DETACH,          16#2003).


-define(EPIXMAP_CREATE,          16#3000).
-define(EPIXMAP_DESTROY,         16#3001).
-define(EPIXMAP_ATTACH,          16#3002).
-define(EPIXMAP_DETACH,          16#3003).
-define(EPIXMAP_FILL,            16#3011).
-define(EPIXMAP_COPY,            16#3012).
-define(EPIXMAP_DRAW,            16#3013).
-define(EPIXMAP_PUT_PIXEL,       16#3014).
-define(EPIXMAP_GET_PIXEL,       16#3015).
-define(EPIXMAP_COPY_AREA,       16#3016).
-define(EPIXMAP_DRAW_RECTANGLE,  16#3017).
-define(EPIXMAP_DRAW_LINE,       16#3019).
-define(EPIXMAP_DRAW_ELLIPSE,    16#301C).
-define(EPIXMAP_PUT_PIXELS,      16#301E).
-define(EPIXMAP_SCROLL,          16#301F).
-define(EPIXMAP_TEX_LINE,        16#3020).
-define(EPIXMAP_DRAW_TRIANGLE,   16#3022).
-define(EPIXMAP_DRAW_POINT,      16#3025).

-define(EBITMAP_CREATE,          16#4000).
-define(EBITMAP_DESTROY,         16#4001).
-define(EBITMAP_FILL,            16#4011).
-define(EBITMAP_COPY,            16#4012).
-define(EBITMAP_DRAW,            16#4013).
-define(EBITMAP_PUT_BIT,         16#4014).
-define(EBITMAP_GET_BIT,         16#4015).
-define(EBITMAP_COPY_AREA,       16#4016).
-define(EBITMAP_DRAW_RECTANGLE,  16#4017).
-define(EBITMAP_DRAW_LINE,       16#4019).
-define(EBITMAP_DRAW_CIRCLE,     16#401A).
-define(EBITMAP_DRAW_ELLIPSE,    16#401C).
-define(EBITMAP_PUT_BITS,        16#401E).
-define(EBITMAP_SCROLL,          16#401F).

-define(EGC_CREATE,              16#5000).
-define(EGC_DESTROY,             16#5001).
-define(EGC_COPY,                16#5002).

-define(EGC_SET,                 16#5012).
-define(EGC_GET,                 16#5013).

-define(EDICT_CREATE,            16#6000).
-define(EDICT_DESTROY,           16#6001).
-define(EDICT_COPY,              16#6002).

-define(EDICT_SET,               16#6010).
-define(EDICT_UNSET,             16#6011).
-define(EDICT_GET,               16#6012).


-define(EPIXEL_RGB4,  0).
-define(EPIXEL_RGB5,  1).
-define(EPIXEL_RGB8,  2).
-define(EPIXEL_RGB10, 3).
-define(EPIXEL_RGB12, 4).
-define(EPIXEL_RGB16, 5).
-define(EPIXEL_RGB332,6).
-define(EPIXEL_RGB232,7).
-define(EPIXEL_RGB565,8).
-define(EPIXEL_YUV8,  9).
-define(EPIXEL_ALPHA, 10).
-define(EPIXEL_GRAY,  11).
-define(EPIXEL_RED,   12).
-define(EPIXEL_GREEN, 13).
-define(EPIXEL_BLUE,  14).

-define(EPIXEL_FMT(Fmt,Bgr,Alpha,AlphaFirst,Little,BytesPerPixel),
	(((Fmt) bsl 12) bor ((Bgr) bsl 9) bor ((Alpha) bsl 8) bor 
	 ((AlphaFirst)bsl 7) bor ((Little) bsl 6) bor ((BytesPerPixel)-1))).

-define(EPIXEL_BE_FMT(Fmt,Bgr,Alpha,AlphaFirst,BytesPerPixel),
	?EPIXEL_FMT(Fmt,Bgr,Alpha,AlphaFirst,0,BytesPerPixel)).

-define(EPIXEL_LE_FMT(Fmt,Bgr,Alpha,AlphaFirst,BytesPerPixel),
	?EPIXEL_FMT(Fmt,Bgr,Alpha,AlphaFirst,1,BytesPerPixel)).

%% 64 bit 
-define( EPIXEL_TYPE_R16G16B16A16, ?EPIXEL_BE_FMT(?EPIXEL_RGB16,0,1,0,64)).
-define( EPIXEL_TYPE_R16G16B16X16, ?EPIXEL_BE_FMT(?EPIXEL_RGB16,0,0,0,64)).
-define( EPIXEL_TYPE_A16R16G16B16, ?EPIXEL_BE_FMT(?EPIXEL_RGB16,0,1,1,64)).
-define( EPIXEL_TYPE_X16R16G16B16, ?EPIXEL_BE_FMT(?EPIXEL_RGB16,0,0,1,64)).

%% 48 bit 
-define( EPIXEL_TYPE_R16G16B16, ?EPIXEL_BE_FMT(?EPIXEL_RGB16,0,0,0,48)).

%% 32 bit
-define( EPIXEL_TYPE_R8G8B8A8, ?EPIXEL_BE_FMT(?EPIXEL_RGB8,0,1,0,32)).
-define( EPIXEL_TYPE_R8G8B8X8, ?EPIXEL_BE_FMT(?EPIXEL_RGB8,0,0,0,32)).
-define( EPIXEL_TYPE_A8R8G8B8, ?EPIXEL_BE_FMT(?EPIXEL_RGB8,0,1,1,32)).
-define( EPIXEL_TYPE_X8R8G8B8, ?EPIXEL_BE_FMT(?EPIXEL_RGB8,0,0,1,32)).
-define( EPIXEL_TYPE_L16A16,   ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,1,0,32)).
-define( EPIXEL_TYPE_A16L16,   ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,1,1,32)).

%% 24 bit
-define( EPIXEL_TYPE_R8G8B8,   ?EPIXEL_BE_FMT(?EPIXEL_RGB8,0,0,0,24)).

%% 16 bit
-define( EPIXEL_TYPE_R5G5B5A1, ?EPIXEL_BE_FMT(?EPIXEL_RGB5,0,1,0,16)).
-define( EPIXEL_TYPE_A1R5G5B5, ?EPIXEL_BE_FMT(?EPIXEL_RGB5,0,1,1,16)).
-define( EPIXEL_TYPE_R5G5B5X1, ?EPIXEL_BE_FMT(?EPIXEL_RGB5,0,0,0,16)).
-define( EPIXEL_TYPE_X1R5G5B5, ?EPIXEL_BE_FMT(?EPIXEL_RGB5,0,0,1,16)).
-define( EPIXEL_TYPE_R5G6B5,   ?EPIXEL_BE_FMT(?EPIXEL_RGB565,0,0,0,16)).
-define( EPIXEL_TYPE_L8A8,     ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,1,0,16)).
-define( EPIXEL_TYPE_A8L8,     ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,1,1,16)).
-define( EPIXEL_TYPE_L16,      ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,0,0,16)).

%% 8 bit
-define( EPIXEL_TYPE_R2G3B2A1, ?EPIXEL_BE_FMT(?EPIXEL_RGB232,0,1,0,8)).
-define( EPIXEL_TYPE_A1R2G3B2, ?EPIXEL_BE_FMT(?EPIXEL_RGB232,0,1,1,8)).
-define( EPIXEL_TYPE_R2G3B2X1, ?EPIXEL_BE_FMT(?EPIXEL_RGB232,0,0,0,8)).
-define( EPIXEL_TYPE_X1R2G3B2, ?EPIXEL_BE_FMT(?EPIXEL_RGB232,0,0,1,8)).
-define( EPIXEL_TYPE_R3G3B2,   ?EPIXEL_BE_FMT(?EPIXEL_RGB332,0,0,0,8)).
-define( EPIXEL_TYPE_L8,       ?EPIXEL_BE_FMT(?EPIXEL_GRAY,  0,0,0,8)).
-define( EPIXEL_TYPE_A8,       ?EPIXEL_BE_FMT(?EPIXEL_ALPHA, 0,1,0,8)).
-define( EPIXEL_TYPE_R8,       ?EPIXEL_BE_FMT(?EPIXEL_RED, 0,1,0,8)).
-define( EPIXEL_TYPE_G8,       ?EPIXEL_BE_FMT(?EPIXEL_GREEN, 0,1,0,8)).
-define( EPIXEL_TYPE_B8,       ?EPIXEL_BE_FMT(?EPIXEL_BLUE, 0,1,0,8)).

%% 4, 2, 1 bit
-define( EPIXEL_TYPE_L4,       ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,0,0,4)).
-define( EPIXEL_TYPE_A4,       ?EPIXEL_BE_FMT(?EPIXEL_ALPHA,0,0,0,4)).
-define( EPIXEL_TYPE_L2,       ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,0,0,2)).
-define( EPIXEL_TYPE_L1,       ?EPIXEL_BE_FMT(?EPIXEL_GRAY,0,0,0,1)).


%% ALIASES 
-define( EPIXEL_TYPE_ALPHA,  ?EPIXEL_TYPE_A8).
-define( EPIXEL_TYPE_232,    ?EPIXEL_TYPE_R2G3B2).

-define( EPIXEL_TYPE_565_LE, ?EPIXEL_LE_FMT(?EPIXEL_RGB565,0,0,0,16)).
-define( EPIXEL_TYPE_565_BE, ?EPIXEL_TYPE_R5G6B5).

-define( EPIXEL_TYPE_RGB,    ?EPIXEL_TYPE_R8G8B8).
-define( EPIXEL_TYPE_BGR,    ?EPIXEL_TYPE_FORMAT(?EPIXEL_COLOR_BGR,0,0,0,24)).

-define( EPIXEL_TYPE_RGBA,   ?EPIXEL_TYPE_R8G8B8A8).
-define( EPIXEL_TYPE_ARGB,   ?EPIXEL_TYPE_A8R8G8B8).
-define( EPIXEL_TYPE_BGRA,   ?EPIXEL_TYPE_FORMAT(?EPIXEL_COLOR_BGR,1,0,0,32)).
-define( EPIXEL_TYPE_ABGR,   ?EPIXEL_TYPE_FORMAT(?EPIXEL_COLOR_BGR,1,1,0,32)).

%% GC SELECTOR
-define(EGC_FOREGROUND_COLOR,   1).
-define(EGC_BACKGROUND_COLOR,   2).

-define(EGC_FILL_STYLE,         10).
-define(EGC_FILL_COLOR,         11).
-define(EGC_FILL_TEXTURE,       12).

-define(EGC_LINE_STYLE,         20).
-define(EGC_LINE_JOIN_STYLE,    21).
-define(EGC_LINE_CAP_STYLE,     22).
-define(EGC_LINE_WIDTH,         23).
-define(EGC_LINE_TEXTURE,       24).

-define(EGC_BORDER_STYLE,       30).
-define(EGC_BORDER_JOIN_STYLE,  31).
-define(EGC_BORDER_CAP_STYLE,   32).
-define(EGC_BORDER_WIDTH,       33).
-define(EGC_BORDER_TEXTURE,     34).
-define(EGC_BORDER_COLOR,       35).

%% GENERAL FLAGS
-define(EFLAG_NONE,                16#00000000).
-define(EFLAG_SOLID,               16#00000001).
-define(EFLAG_BLEND,               16#00000002).
-define(EFLAG_AALIAS,              16#00000004).
-define(EFLAG_TEXTURED,            16#00000008).

%% FILL FLAGS
-define(EPIC_FILL_STYLE_NONE,      EFLAG_NONE).
-define(EPIC_FILL_STYLE_SOLID,     EFLAG_SOLID).
-define(EPIC_FILL_STYLE_BLEND,     EFLAG_BLEND).
-define(EPIC_FILL_STYLE_TEXTURED,  EFLAG_TEXTURED).

%% LINE FLAGS
-define(EPIC_LINE_STYLE_SOLID,     EFLAG_SOLID).
-define(EPIC_LINE_STYLE_BLEND,     EFLAG_BLEND).
-define(EPIC_LINE_STYLE_AALIAS,    EFLAG_AALIAS).
-define(EPIC_LINE_STYLE_TEXTURED,  EFLAG_TEXTURED).
-define(EPIC_LINE_STYLE_DASHED,    16#00000100).
-define(EPIC_LINE_STYLE_NFIRST,    16#00001000).
-define(EPIC_LINE_STYLE_NLAST,     16#00002000).

%% BORDER FLAGS
-define(EPIC_BORDER_STYLE_SOLID,   EFLAG_SOLID).
-define(EPIC_BORDER_STYLE_BLEND,   EFLAG_BLEND).
-define(EPIC_BORDER_STYLE_AALIAS,  EFLAG_AALIAS).
-define(EPIC_BORDER_STYLE_TEXTURED,EFLAG_TEXTURED).
-define(EPIC_BORDER_STYLE_DASHED,  16#00010000).
-define(EPIC_BORDER_STYLE_NBORDER1,16#00100000).
-define(EPIC_BORDER_STYLE_NBORDER2,16#00200000).
-define(EPIC_BORDER_STYLE_NBORDER3,16#00400000).
-define(EPIC_BORDER_STYLE_NBORDER4,16#00800000).
-define(EPIC_BORDER_STYLE_NBORDER, 16#00F00000).

-define(EPIC_CAP_STYLE_NONE,       0).
-define(EPIC_CAP_STYLE_BUTT,       1).
-define(EPIC_CAP_STYLE_ROUND,      2).
-define(EPIC_CAP_STYLE_PROJECTING ,3).

-define(EPIC_JOIN_STYLE_MITER,  0).
-define(EPIC_JOIN_STYLE_ROUND,  1).
-define(EPIC_JOIN_STYLE_BEVEL,  2).
