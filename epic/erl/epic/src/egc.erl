%%% File    : egc.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPic GC
%%% Created : 24 Oct 2006 by Tony Rogvall <tony@PBook.local>

-module(egc).

-include("epic_int.hrl").

-export([create/0, destroy/1, copy/1]).
-export([push/0, pop/0, current/0]).
-export([draw/1]).

-export([set_background_color/2,set_background_color/1,
	 set_foreground_color/2,set_foreground_color/1,
	 set_fill_style/2,	 set_fill_style/1,
	 set_fill_color/2,	 set_fill_color/1,
	 set_fill_texture/2,	 set_fill_texture/1,

	 set_line_style/2,	 set_line_style/1,
	 set_line_width/2,	 set_line_width/1,
	 set_line_join_style/2,	 set_line_join_style/1,
	 set_line_cap_style/2,	 set_line_cap_style/1,
	 set_line_texture/2,	 set_line_texture/1,

	 set_border_style/2,	 set_border_style/1,
	 set_border_color/2,	 set_border_color/1,
	 set_border_width/2,	 set_border_width/1,
	 set_border_join_style/2,	 set_border_join_style/1,
	 set_border_cap_style/2,	 set_border_cap_style/1,
	 set_border_texture/2,	 set_border_texture/1
	]).

-import(epixmap,[pixel/1]).
       

create() ->
    epic_drv:create(?EGC_CREATE, ?EGC_DESTROY, <<>>).

destroy(Gc) ->
    epic_drv:destroy(?EGC_DESTROY, Gc).

copy(Gc) ->
    epic_drv:copy(?EGC_COPY, ?EGC_DESTROY, Gc).

draw(Fun) ->
    %%  Should implement copy on write!
    push(),
    %% Do NOT pop inside fun, unless intensional ;-)
    Res = (catch Fun()),
    pop(),
    Res.

current() ->
    case get('EPIC_GC') of
	undefined ->
	    {ok,Gc} = egc:create(),	    
	    put('EPIC_GC', Gc),
	    put('EPIC_GC_STACK', []),
	    Gc;
	Gc -> Gc
    end.

push() ->
    case get('EPIC_GC') of
	undefined ->
	    {ok,Gc} = egc:create(),
	    put('EPIC_GC', Gc),
	    put('EPIC_GC_STACK', []),
	    ok;
	Gc ->
	    {ok,GcCopy} = egc:copy(Gc),
	    case get('EPIC_GC_STACK') of
		undefined ->
		    put('EPIC_GC_STACK', [Gc]);
		Stack ->
		    put('EPIC_GC_STACK', [Gc|Stack])
	    end,
	    put('EPIC_GC', GcCopy),
	    ok
    end.

pop() ->
    case get('EPIC_GC_STACK') of
	[Gc|Stack] ->
	    Current = get('EPIC_GC'),
	    egc:destroy(Current),
	    put('EPIC_GC', Gc),
	    put('EPIC_GC_STACK', Stack),
	    ok;
	[] ->
	    ok
    end.

%%
%% General colors
%%
set_background_color(Color) ->
    set_background_color(current(),Color).
set_background_color(Gc,Color) ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_BACKGROUND_COLOR,(pixel(Color)):32>>).

set_foreground_color(Color) ->
    set_foreground_color(current(),Color).
set_foreground_color(Gc,Color) ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_FOREGROUND_COLOR, (pixel(Color)):32>>).

%%
%% FILL style/color/texture
%%
set_fill_style(Style) ->
    set_fill_style(current(),Style).
set_fill_style(Gc,Style) ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_FILL_STYLE,(efill_style(Style)):32>>).

set_fill_color(Color) ->
    set_fill_color(current(),Color).
set_fill_color(Gc,Color) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_FILL_COLOR,(pixel(Color)):32>>).

set_fill_texture(Texture) ->
    set_fill_texture(current(),Texture).
set_fill_texture(Gc,Texture) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_FILL_TEXTURE,Texture:32>>).
    

    

%%
%% BORDER style/color/join/cap/texture
%%
set_border_style(Style) ->
    set_border_style(current(), Style).
set_border_style(Gc,Style) ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_BORDER_STYLE,(eborder_style(Style)):32>>).

set_border_color(Color) ->
    set_border_color(current(),Color).
set_border_color(Gc,Color) ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_BORDER_COLOR, (pixel(Color)):32>>).

set_border_width(Width) ->
    set_border_width(current(), Width).
set_border_width(Gc,Width) when is_integer(Width), Width >= 0  ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_BORDER_WIDTH,Width:32>>).


set_border_join_style(Style) ->
    set_border_join_style(current(),Style).
set_border_join_style(Gc,Style) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_BORDER_JOIN_STYLE,(ejoin_style(Style)):32>>).

set_border_cap_style(Style) ->
    set_border_cap_style(current(),Style).
set_border_cap_style(Gc,Style) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_BORDER_CAP_STYLE,(ecap_style(Style)):32>>).

set_border_texture(Texture) ->
    set_border_texture(current(),Texture).
set_border_texture(Gc,Texture) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_BORDER_TEXTURE,Texture:32>>).

%%
%% LINE style/join/cap/texture
%%


set_line_style(Style) ->
    set_line_style(current(), Style).
set_line_style(Gc,Style) ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_LINE_STYLE,(eline_style(Style)):32>>).

set_line_width(Width) ->
    set_line_width(current(),Width).
set_line_width(Gc,Width) when is_integer(Width), Width >= 0 ->
    epic_drv:call(?EGC_SET, 
		  <<Gc:32,?EGC_LINE_WIDTH, Width:32>>).

set_line_join_style(Style) ->
    set_line_join_style(current(),Style).
set_line_join_style(Gc,Style) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_LINE_JOIN_STYLE,(ejoin_style(Style)):32>>).

set_line_cap_style(Style) ->
    set_line_cap_style(current(),Style).
set_line_cap_style(Gc,Style) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_LINE_CAP_STYLE,(ecap_style(Style)):32>>).

set_line_texture(Texture) ->
    set_line_texture(current(),Texture).
set_line_texture(Gc,Texture) ->
    epic_drv:call(?EGC_SET,
		  <<Gc:32,?EGC_LINE_TEXTURE,Texture:32>>).


%%
%% Encode 
%%
ejoin_style(miter) -> ?EPIC_JOIN_STYLE_MITER;
ejoin_style(round) -> ?EPIC_JOIN_STYLE_ROUND;
ejoin_style(bevel) -> ?EPIC_JOIN_STYLE_BEVEL.

ecap_style(none) -> ?EPIC_CAP_STYLE_NONE;
ecap_style(butt) -> ?EPIC_CAP_STYLE_BUTT;
ecap_style(round) -> ?EPIC_CAP_STYLE_ROUND;
ecap_style(projecting) -> ?EPIC_CAP_STYLE_PROJECTING.


efill_style(Flags) -> estyle(Flags).

eline_style(dashed) -> ?EPIC_LINE_STYLE_DASHED;
eline_style(nfirst) -> ?EPIC_LINE_STYLE_NFIRST;
eline_style(nlast) -> ?EPIC_LINE_STYLE_NLAST;
eline_style(Flag) when atom(Flag) -> estyle(Flag);
eline_style([F|Fs]) ->
    eline_style(F) bor eline_style(Fs);
eline_style([]) -> 0.


eborder_style(dashed)   -> ?EPIC_BORDER_STYLE_DASHED;
eborder_style(nborder1) -> ?EPIC_BORDER_STYLE_NBORDER1;
eborder_style(nborder2) -> ?EPIC_BORDER_STYLE_NBORDER2;
eborder_style(nborder3) -> ?EPIC_BORDER_STYLE_NBORDER3;
eborder_style(nborder4) -> ?EPIC_BORDER_STYLE_NBORDER4;
eborder_style(nborder)  -> ?EPIC_BORDER_STYLE_NBORDER;
eborder_style(Flag) when atom(Flag) -> estyle(Flag);
eborder_style([F|Fs]) ->
    eborder_style(F) bor eborder_style(Fs);
eborder_style([]) -> 0.

estyle(solid) -> ?EFLAG_SOLID;
estyle(blend) -> ?EFLAG_BLEND;
estyle(aalias) -> ?EFLAG_AALIAS;
estyle(textured) -> ?EFLAG_TEXTURED.
