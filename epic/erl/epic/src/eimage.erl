%%% File    : eimage.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Image file interface
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(eimage).

-compile(export_all).

-include_lib("erl_img/include/erl_img.hrl").

-import(lists, [foldl/3, reverse/1, foreach/2, map/2]).

%%
%% Load an image file into one or more epic Pixmaps
%%
file(File) ->
    case erl_img:load(File) of
	{ok,Image} ->
	    make_image(Image);
	Error ->
	    Error
    end.

show(File) ->
    show(File,[copy]).

show(File,Transform) ->
    spawn(?MODULE, show1, [File, Transform]).

show1(File, Transform) ->
    case erl_img:load(File) of
	{ok,Image} ->
	    Width = Image#erl_image.width,
	    Height = Image#erl_image.height,
	    Diag = trunc(math:sqrt(Width*Width + Height*Height)+4.5),
	    BgW = Diag,
	    BgH = Diag,
	    X = Diag div 2,
	    Y = Diag div 2,
	    {ok,Win} = ewindow:create(50, 50, BgW, BgH),
	    {ok,Bg}  = epixmap:create(BgW, BgH),
	    ewindow:attach(Win),
	    epixmap:attach(Bg),
	    [{_Delay,EPic}|_] = make_image(Image),
	    transform_loop(Transform, EPic, X, Y, Width, Height, 
			   Bg, BgW, BgH, Win);
	Error ->
	    io:format("Image load error: ~p\n", [Error])
    end.
	    
transform_loop(Transform, EPic, X, Y, Width, Height, Bg, BgW, BgH, Win) ->
    epixmap:fill(Bg, {255,0,0,0}),
    transform(Transform, EPic, X, Y, Width, Height, Bg),
    epixmap:draw(Bg, Win, 0, 0, 0, 0, BgW, BgH),
    case next_transform(Transform) of
	[] ->
	    receive
		{eevent,Win,close} ->
		    epixmap:destroy(Bg),
		    epixmap:destroy(EPic),
		    ewindow:destroy(Win)
	    end;
	Transform1 ->
	    receive after 10 -> ok end,
	    transform_loop(Transform1, EPic, X, Y, Width, Height, 
			   Bg, BgW, BgH, Win)
    end.

next_transform([{spin,From,From}|T]) -> T;
next_transform([{spin,From,To}|T]) when From < To -> [{spin,From+1,To}|T];
next_transform([{rotate,_A}|T]) -> T;
next_transform([copy|T]) -> T;
next_transform([]) -> [].

transform([], _EPic, _X, _Y, _Width, _Height, _Bg) ->
    ok;
transform([copy|_], EPic, X, Y, Width, Height, Bg) ->
    epixmap:copy_area(EPic, Bg, 0, 0, 
		   X - (Width div 2), 
		   Y - (Height div 2),
		   Width, Height);
transform([{spin,Angle,_EndAngle}|T],EPic, X, Y, Width, Height, Bg)  ->
    transform([{rotate,Angle}|T],EPic, X, Y, Width, Height, Bg);
transform([{rotate,Angle}|_], EPic, X, Y, Width, Height, Bg) ->
    XOffs = (Width div 2),
    X0 =  -XOffs,
    X1 = X0 + Width - 1,
    YOffs = (Height div 2),
    Y0 = -YOffs,
    Y1 = Y0 + Height - 1,
    Ai = trunc(Angle),
    Ad = (Ai rem 360) + (Angle - Ai),
    Ar = Ad*(2*math:pi() / 360),
    CosA = math:cos(Ar),
    SinA = math:sin(Ar),
    rotate(EPic, X0, Y0, X1, Y1, X, Y, XOffs, YOffs, CosA, SinA, Bg).


rotate(_EPic, _X0, Y0, _X1, Y1, _Xc, _Yc, _XOffs, _YOffs, _CosA, _SinA, _Bg) when Y0>Y1->
    ok;
rotate(EPic, X0, Y0, X1, Y1, Xc, Yc, XOffs, YOffs, CosA, SinA, Bg) ->
    Px = trunc(X0*CosA - Y0*SinA + Xc),
    Py = trunc(Y0*CosA + X0*SinA + Yc),

    Qx = trunc(X1*CosA - Y0*SinA + Xc),
    Qy = trunc(Y0*CosA + X1*SinA + Yc),
    Tx0 = X0 + XOffs,
    Tx1 = X1 + XOffs,
    Ty = float(Y0+YOffs),
    %% io:format("Ty = ~w\n", [Ty]),
    epixmap:tex_line(Bg, Px, Py, Qx, Qy, EPic, Tx0, Tx1, Ty),
    rotate(EPic, X0, Y0+1, X1, Y1, Xc, Yc, XOffs, YOffs, CosA, SinA, Bg).


%%
%% generate a list of images
%%
make_image(Image) ->
    Map0 = map_colors(Image#erl_image.palette),
    map(
      fun(Pixmap) ->
	      {ok,EPic} = epixmap:create(Pixmap#erl_pixmap.width,
				      Pixmap#erl_pixmap.height),
	      Pal = Pixmap#erl_pixmap.palette,
	      Map = case Pixmap#erl_pixmap.palette of
			undefined -> Map0;
			Pal -> map_colors(Pal)
		    end,
	      Transparent = get_attribute('Transparent',Pixmap,undefined),
	      Background = get_attribute('Background', Pixmap, 0),
	      DelayTime  = get_attribute('DelayTime', Pixmap, 0),
	      
	      make_pixmap(Pixmap, EPic, Image#erl_image.order,
			  Map, Transparent, Background),
	      {DelayTime, EPic}
      end, Image#erl_image.pixmaps).


get_attribute(Key, Pixmap, Default) ->
    case lists:keysearch(Key, 1, Pixmap#erl_pixmap.attributes) of
	false -> Default;
	{value,{_,V}} -> V
    end.


-define(ARGB(A,R,G,B),
	(((A) bsl 24) + ((R) bsl 16) + ((G) bsl 8) + (B))).

%% create a color lookup  table
map_colors(undefined) ->
    {};
map_colors(Colors) ->
    map_colors(Colors, []).
    
map_colors([{R,G,B}| Cs], Acc) ->
    %% map from erl_img 16 bit RGB to 8 bit RGB
    Pixel = ?ARGB(255,R,G,B),
    map_colors(Cs, [Pixel|Acc]);
map_colors([Spec | Cs], Acc) ->
    Pixel = color_parse(Spec),
    map_colors(Cs, [Pixel|Acc]);
map_colors([], Acc) ->
    list_to_tuple(reverse(Acc)).

%% FIXME add color name parsing
color_parse(_) ->
    io:format("FIXME: color parse...\n", []),
    ?ARGB(255,0,0,0).

%%
%% Load image (FIXME)
%%
make_pixmap(Pixmap, EPic, Order, Map, Transparent, _Background) ->
    #erl_pixmap { top = Y0, left = X0, width = W, height = _H } = Pixmap,
    foreach(
      fun({Y,Row}) ->
	      Pixels = make_row(Pixmap#erl_pixmap.format,
				0, W, Row, Map, Transparent, []),
	      case Order of
		  left_to_right ->
		      epixmap:put_pixels(EPic, X0, Y+Y0, W, 1, reverse(Pixels));
		  right_ro_left ->
		      epixmap:put_pixels(EPic, X0, Y+Y0, W, 1, Pixels)
	      end
      end, Pixmap#erl_pixmap.pixels),
    EPic.

make_row(_Format, Xmax, Xmax, _Row, _Map, _Transparent, Acc) ->
    Acc;
make_row(Format, X, Xmax, Row, Map, Transparent, Acc) ->
    make_row(Format, X+1, Xmax, Row, Map, Transparent,
	     [make_pixel(Format, X, Row, Map, Transparent)|Acc]).

%% This should NOT be needed any more, since epic should have conversions
%% for most pixel types.
make_pixel(Type, X, Row, Map, Transparent) ->
    case Type of
	gray4 ->
	    Offs = X*4,
	    Pad  = (X band 1)*4,
	    <<_:Offs,G:4,_:Pad,_/binary>> = Row,
	    V = G*16,
	    ?ARGB(255,V,V,V);
	gray8 ->
	    Offs = X,
	    <<_:Offs/binary,G,_/binary>> = Row,
	    ?ARGB(255,G,G,G);
	b8g8r8 ->
	    Offs = 3*X,
	    <<_:Offs/binary,B,G,R,_/binary>> = Row,
	    ?ARGB(255,R,G,B);
	b8g8r8a8 ->
	    Offs = 4*X,
	    <<_:Offs/binary,B,G,R,A,_/binary>> = Row,
	    ?ARGB(A,R,G,B);
	r8g8b8 ->
	    Offs = 3*X,
	    <<_:Offs/binary,R:8,G:8,B:8,_/binary>> = Row,
	    ?ARGB(255,R,G,B);
	r8g8b8a8 ->
	    Offs = 4*X,
	    <<_:Offs/binary,R:8,G:8,B:8,A:8,_/binary>> = Row,
	    ?ARGB(A,R,G,B);
	palette4 ->
	    Offs = X*4,
	    Pad  = (X band 1)*4,
	    <<_:Offs,I:4,_:Pad,_/binary>> = Row,
	    if I == Transparent ->
		    ?ARGB(255,0,0,0);   %% speical draw transparent!
	       true ->
		    element(I+1, Map)
	    end;
	palette8 ->
	    Offs = X,
	    <<_:Offs/binary,I,_/binary>> = Row,
	    if I == Transparent ->
		    ?ARGB(255,0,0,0);   %% speical draw transparent!
	       true ->
		    element(I+1, Map)
	    end
    end.



