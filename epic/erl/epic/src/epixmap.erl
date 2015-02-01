%%% File    : epixmap.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Pixmap interface
%%% Created : 26 Sep 2006 by Tony Rogvall <tony@PBook.local>

-module(epixmap).

-export([create/2, destroy/1, attach/1, attach/2, detach/1]).
-export([fill/2, copy/2, copy_area/8, draw/8]).
-export([scroll/6, scroll_up/5, scroll_down/5, scroll_left/5, scroll_right/5]).
-export([pixel/1, put_pixel/4, put_pixels/6, get_pixel/3]).
-export([draw_point/3, draw_point/4,
	 draw_line/5, draw_line/6,
	 draw_rectangle/5, draw_rectangle/6, 
	 draw_ellipse/5, draw_ellipse/6,
	 tex_line/9,tex_line/10]).


-include("epic_int.hrl").

create(Width, Height) ->
    create(Width, Height, ?EPIXEL_TYPE_A8R8G8B8).
create(Width, Height,PixelType) ->
    epic_drv:create(?EPIXMAP_CREATE, ?EPIXMAP_DESTROY, 
		    <<Width:32, Height:32, (pixel_type(PixelType)):32>>).

destroy(Pix) ->
    epic_drv:destroy(?EPIXMAP_DESTROY, Pix).

attach(Pix) ->
    attach(Pix, ebackend:default()).

attach(Pix, Be) ->
    epic_drv:attach(?EPIXMAP_ATTACH, Pix, Be).

detach(Pix) ->
    epic_drv:detach(?EPIXMAP_DETACH, Pix).

fill(Pix, Pixel) ->
    epic_drv:call(?EPIXMAP_FILL, <<Pix:32, 
				  (pixel(Pixel)):32>>).
copy(Src, Dst) ->
    epic_drv:call(?EPIXMAP_COPY, <<Src:32, Dst:32>>).

copy_area(SrcPix, DstPix, XSrc, YSrc, XDst, YDst, Width, Height) ->
    epic_drv:call(?EPIXMAP_COPY_AREA, 
		  <<SrcPix:32, DstPix:32,
		   XSrc:32, YSrc:32, XDst:32, YDst:32, 
		   Width:32, Height:32>>).

scroll_up(SrcPix, DstPix, Amount, Rotate, Pixel) when Amount >= 0->
    scroll(SrcPix, DstPix, 0, Amount, Rotate, Pixel).

scroll_down(SrcPix, DstPix, Amount, Rotate, Pixel) when Amount >= 0 ->
    scroll(SrcPix, DstPix, 0, -Amount, Rotate, Pixel).

scroll_left(SrcPix, DstPix, Amount, Rotate, Pixel) when Amount >= 0->
    scroll(SrcPix, DstPix, -Amount, 0, Rotate, Pixel).

scroll_right(SrcPix, DstPix, Amount, Rotate, Pixel) when Amount >= 0 ->
    scroll(SrcPix, DstPix, Amount, 0, Rotate, Pixel).

scroll(SrcPix, DstPix, Horizontal, Vertical, Rotate, Pixel) ->
       epic_drv:call(?EPIXMAP_SCROLL,
		     <<SrcPix:32, DstPix:32,
		      Horizontal:32, Vertical:32, Rotate:32, Pixel:32>>).

draw(Pix, Win, XSrc, YSrc, XDst, YDst, Width, Height) ->
    epic_drv:call(?EPIXMAP_DRAW,
		  <<Pix:32, Win:32,
		   XSrc:32, YSrc:32, XDst:32, YDst:32, 
		   Width:32, Height:32>>).

put_pixel(Pix, X, Y, Pixel) ->
    epic_drv:call(?EPIXMAP_PUT_PIXEL,
		  <<Pix:32, X:32, Y:32,
		   (pixel(Pixel)):32>>).

%% Pixels is either a binary or a list of pixel-values
put_pixels(Pix, X, Y, Width, Height, Pixels) ->
    epic_drv:call(?EPIXMAP_PUT_PIXELS, 
		  <<Pix:32, X:32, Y:32,
		   Width:32, Height:32,
		   (pixels(Pixels))/binary>>).

get_pixel(Pix, X, Y) ->
    epic_drv:call(?EPIXMAP_GET_PIXEL, <<Pix:32, X:32, Y:32>>).

draw_point(Pix, X, Y) ->
    draw_point(Pix, egc:current(), X, Y).

draw_point(Pix,Gc,X,Y) ->
    epic_drv:call(?EPIXMAP_DRAW_POINT, <<Pix:32,Gc:32,X:32,Y:32>>).
    
draw_rectangle(Pix, X, Y, Width, Height) ->
    draw_rectangle(Pix, egc:current(), X, Y, Width, Height).

draw_rectangle(Pix, Gc, X, Y, Width, Height) ->
    epic_drv:call(?EPIXMAP_DRAW_RECTANGLE, 
		  <<Pix:32, Gc:32, X:32, Y:32, Width:32, Height:32>>).

draw_line(Pix, X0, Y0, X1, Y1) ->
    draw_line(Pix, egc:current(), X0, Y0, X1, Y1).

draw_line(Pix, Gc, X0, Y0, X1, Y1) ->
    epic_drv:call(?EPIXMAP_DRAW_LINE,
		  <<Pix:32, Gc:32, X0:32, Y0:32, X1:32, Y1:32>>).

draw_ellipse(Pix, X, Y, A, B) ->
    draw_ellipse(Pix, egc:current(), X, Y, A, B).

draw_ellipse(Pix, Gc, X, Y, A, B) ->
    epic_drv:call(?EPIXMAP_DRAW_ELLIPSE,
		  <<Pix:32, Gc:32,  X:32, Y:32, A:32, B:32>>).


tex_line(Pix, X0, Y0, X1, Y1, Tex, TX0, TX1, Ty) ->
    tex_line(Pix, egc:current(), X0, Y0, X1, Y1, Tex, TX0, TX1, Ty).
tex_line(Pix, Gc, X0, Y0, X1, Y1, Tex, TX0, TX1, Ty) ->
    epic_drv:call(?EPIXMAP_TEX_LINE,
		  <<Pix:32, Gc:32, X0:32, Y0:32, X1:32, Y1:32, 
		   Tex:32, TX0:32, TX1:32, Ty:32/float>>).


-define(ARGB(A,R,G,B),
	(((A) bsl 24) + ((R) bsl 16) + ((G) bsl 8) + (B))).

pixel({A,R,G,B}) -> ?ARGB(A,R,G,B);
pixel({R,G,B})   -> ?ARGB(255,R,G,B);
pixel(ARGB)      -> ARGB.

pixels(Binary) when binary(Binary) ->
    Binary;
pixels(Pixels) -> 
    pixels(Pixels, []).

pixels([{A,R,G,B}|Ps], Acc) ->
    pixels(Ps, [(<<A,R,G,B>>) | Acc]);
pixels([ARGB|Ps], Acc) when is_integer(ARGB) ->
    pixels(Ps, [<<ARGB:32>> | Acc]);
pixels([PixelRow|Ps], Acc) when is_list(PixelRow) ->
    pixels(Ps, [pixels(PixelRow) | Acc]);
pixels([PixelRow|Ps], Acc) when is_binary(PixelRow) ->
    pixels(Ps, [PixelRow | Acc]);
pixels([], Acc) ->
    list_to_binary(lists:reverse(Acc)).


pixel_type(Type) when is_integer(Type) ->
    Type;

pixel_type(r16g16b16a16) -> ?EPIXEL_TYPE_R16G16B16A16;
pixel_type(r16g16b16x16) -> ?EPIXEL_TYPE_R16G16B16X16;
pixel_type(a16r16g16b16) -> ?EPIXEL_TYPE_A16R16G16B16;
pixel_type(x16r16g16b16) -> ?EPIXEL_TYPE_X16R16G16B16;
pixel_type(r16g16b16) -> ?EPIXEL_TYPE_R16G16B16;
pixel_type(r8g8b8a8) -> ?EPIXEL_TYPE_R8G8B8A8;
pixel_type(r8g8b8x8) -> ?EPIXEL_TYPE_R8G8B8X8;
pixel_type(a8r8g8b8) -> ?EPIXEL_TYPE_A8R8G8B8;
pixel_type(x8r8g8b8) -> ?EPIXEL_TYPE_X8R8G8B8;
pixel_type(l16a16) -> ?EPIXEL_TYPE_L16A16;
pixel_type(a16l16) -> ?EPIXEL_TYPE_A16L16;
pixel_type(r8g8b8) -> ?EPIXEL_TYPE_R8G8B8;
pixel_type(r5g5b5a1) -> ?EPIXEL_TYPE_R5G5B5A1;
pixel_type(a1r5g5b5) -> ?EPIXEL_TYPE_A1R5G5B5;
pixel_type(r5g5b5x1) -> ?EPIXEL_TYPE_R5G5B5X1;
pixel_type(x1r5g5b5) -> ?EPIXEL_TYPE_X1R5G5B5;
pixel_type(r5g6b5) -> ?EPIXEL_TYPE_R5G6B5;
pixel_type(l8a8) -> ?EPIXEL_TYPE_L8A8;
pixel_type(a8l8) -> ?EPIXEL_TYPE_A8L8;
pixel_type(l16) -> ?EPIXEL_TYPE_L16;
pixel_type(r2g3b2a1) -> ?EPIXEL_TYPE_R2G3B2A1;
pixel_type(a1r2g3b2) -> ?EPIXEL_TYPE_A1R2G3B2;
pixel_type(r2g3b2x1) -> ?EPIXEL_TYPE_R2G3B2X1;
pixel_type(x1r2g3b2) -> ?EPIXEL_TYPE_X1R2G3B2;
pixel_type(r3g3b2) -> ?EPIXEL_TYPE_R3G3B2;
pixel_type(l8) -> ?EPIXEL_TYPE_L8;
pixel_type(a8) -> ?EPIXEL_TYPE_A8;
pixel_type(l4) -> ?EPIXEL_TYPE_L4;
pixel_type(l2) -> ?EPIXEL_TYPE_L2;
pixel_type(l1) -> ?EPIXEL_TYPE_L1;
pixel_type(gray8a8)   -> ?EPIXEL_TYPE_A8L8;      %% alias
pixel_type(gray16)    -> ?EPIXEL_TYPE_L16;       %% alias
pixel_type(gray8)     -> ?EPIXEL_TYPE_L8;        %% alias
pixel_type(Color) ->
    erlang:fault({colorspec,Color}).
