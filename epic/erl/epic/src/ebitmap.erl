%%% File    : ebitmap.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EBitmap 
%%% Created : 27 Aug 2006 by Tony Rogvall <tony@PBook.local>
-module(ebitmap).


-export([create/2, destroy/1]).
-export([fill/2, copy/2, copy_area/8, draw/10]).
-export([scroll/6, scroll_up/5, scroll_down/5, scroll_left/5, scroll_right/5]).
-export([put_bit/4, put_bits/6, get_bit/3]).
-export([draw_line/6,
	 draw_rectangle/6,
	 draw_ellipse/6]).

-include("epic_int.hrl").

create(Width, Height) ->
    epic_drv:create(?EBITMAP_CREATE, ?EBITMAP_DESTROY, 
		    <<Width:32, Height:32>>).

destroy(Pic) ->
    epic_drv:destroy(?EBITMAP_DESTROY, Pic).

fill(Pic, Pattern) ->
    epic_drv:call(?EBITMAP_FILL, <<(Pic):32,(Pattern):8>>).

copy(Src, Dst) ->
    epic_drv:call(?EBITMAP_COPY, <<Src:32, Dst:32>>).

copy_area(Src, Dst, XSrc, YSrc, XDst, YDst, Width, Height) ->
    epic_drv:call(?EBITMAP_COPY_AREA,
		  <<Src:32, Dst:32,
		   XSrc:32, YSrc:32, XDst:32, YDst:32, 
		   Width:32, Height:32>>).

scroll_up(Src, Dst, Amount, Rotate, Pixel) when Amount >= 0->
    scroll(Src, Dst, 0, Amount, Rotate, Pixel).

scroll_down(Src, Dst, Amount, Rotate, Pixel) when Amount >= 0 ->
    scroll(Src, Dst, 0, -Amount, Rotate, Pixel).

scroll_left(Src, Dst, Amount, Rotate, Pixel) when Amount >= 0->
    scroll(Src, Dst, -Amount, 0, Rotate, Pixel).

scroll_right(Src, Dst, Amount, Rotate, Pixel) when Amount >= 0 ->
    scroll(Src, Dst, Amount, 0, Rotate, Pixel).

scroll(Src, Dst, Horizontal, Vertical, Rotate, Pixel) ->
       epic_drv:call(?EBITMAP_SCROLL,
		     <<Src:32, Dst:32,
		      Horizontal:32, Vertical:32, Rotate:32, Pixel:32>>).

draw(SrcBmp, DstPix, XSrc, YSrc, XDst, YDst, Width, Height,Fg,Bg) ->
    epic_drv:call(?EBITMAP_DRAW,
		  <<SrcBmp:32, DstPix:32,
		   XSrc:32, YSrc:32, XDst:32, YDst:32, 
		   Width:32, Height:32, (pixel(Fg)):32, (pixel(Bg)):32 >>).


put_bit(Bmp, X, Y, Bit) ->
    epic_drv:call(?EBITMAP_PUT_BIT,
		  <<Bmp:32, X:32, Y:32, Bit:8>>).

%% Pixels is either a binary or a list of pixel-values
put_bits(Bmp, X, Y, Width, Height, Bits) ->
    epic_drv:call(?EBITMAP_PUT_BITS, 
		  <<Bmp:32, X:32, Y:32,
		   Width:32, Height:32,
		   (list_to_binary([Bits]))/binary>>).

get_bit(Bmp, X, Y) ->
    epic_drv:call(?EBITMAP_GET_BIT, <<Bmp:32, X:32, Y:32>>).

    
draw_rectangle(Bmp, X, Y, Width, Height, Pixel) ->
    epic_drv:call(?EBITMAP_DRAW_RECTANGLE, 
		  <<Bmp:32, X:32, Y:32, Width:32, Height:32,
		   (pixel(Pixel)):32>>).

draw_line(Bmp, X0, Y0, X1, Y1, Bit) ->
    epic_drv:call(?EBITMAP_DRAW_LINE,
		  <<Bmp:32, X0:32, Y0:32, X1:32, Y1:32, Bit:8>>).

draw_ellipse(Bmp, X, Y, A, B, Bit) ->
    epic_drv:call(?EBITMAP_DRAW_ELLIPSE,
		  <<Bmp:32, X:32, Y:32, A:32, B:32, Bit:8>>).

-define(ARGB(A,R,G,B),
	(((A) bsl 24) + ((R) bsl 16) + ((G) bsl 8) + (B))).

pixel({A,R,G,B}) -> ?ARGB(A,R,G,B);
pixel({R,G,B}) -> ?ARGB(0,R,G,B);
pixel(ARGB) -> ARGB.
