%%% File    : efont.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Font loader
%%% Created : 26 Sep 2006 by Tony Rogvall <tony@PBook.local>

-module(efont).

-compile(export_all).

-include("../include/epic.hrl").
-import(lists, [map/2, foreach/2]).

%% Convert font to efnt format
create_efnt(Name) ->
    Ext = filename:extension(Name),
    Base = filename:basename(Name, Ext),
    create_efnt(Name, Base++".efnt").

create_efnt(Name, File) ->
    case load_file(Name) of
	{ok,{Mod,F}} ->
	    Mod:write_efnt_file(F, File);
	Error ->
	    Error
    end.


load_file(Name) ->
    FontPath = filename:join(code:priv_dir(epic),"fonts"),
    case filename:extension(Name) of
	".bdf" ->
	    Path=[".",
		  filename:join(FontPath, "bdf"),
		  filename:join(FontPath, "efont-unicode-bdf-0.4.2")],
	    epic_bdf:read_font_path(Path, Name);
	".ttf" ->
	    Path=[".",
		  filename:join(FontPath, "ttf")],
	    epic_ttf:read_font_path(Path, Name)
    end.

load_font(Name) ->
    case load_file(Name) of
	{ok,{Mod,FontData}} ->
	    Mod:make_font(FontData);
	Error ->
	    Error
    end.

dump_char(Char, Font) ->
    case ets:lookup(Font#efont.glyphs, Char) of
	[] ->
	    {error, not_found};
	[Glyph] ->
	    #eglyph { bmp_offs = BmpOffs,
		      width=Width, height=Height,
		      xoffs=_XOffs, yoffs=_Yoffs } = Glyph,
	    Bitmap = Font#efont.bitmap,
	    io:format("Glyph: ~p\n", [Glyph]),
	    io:format("+~s+\n", [lists:duplicate(Width, $-)]),
	    foreach(
	      fun(Y) ->
		      Row = map(
			      fun(X) -> 
				      case ebitmap:get_bit(Bitmap,BmpOffs+X,Y) of
					  {ok,1} -> $o;
					  {ok,0} -> $\s
				      end
			      end, lists:seq(0,Width-1)),
		      io:format("|~s|\n", [Row])
	      end, lists:seq(0, Height-1)),
	    io:format("+~s+\n", [lists:duplicate(Width, $-)])
    end.


draw_char(Char, X, Y, Pixmap, Font, Fg, Bg) ->
    case ets:lookup(Font#efont.glyphs, Char) of
	[] ->
	    %% FIXME draw ? char
	    X + 10;
	[#eglyph { bmp_offs = BmpOffs,
		   width=Width, height=Height,
		   xoffs=XOffs, yoffs=YOffs,
		   dwx=Dwx }] ->
	    ebitmap:draw(Font#efont.bitmap,
			 Pixmap,
			 BmpOffs, 0,
			 X+XOffs,
			 Y-YOffs-Height,
			 Width, Height, Fg, Bg),
	    X + Dwx
    end.

draw_string(String, X, Y, Pixmap, Font) ->
    draw_string(String, X, Y, Pixmap, Font, {255,0,0,0}, {0,255,255,255}).

draw_string([H|T], X, Y, Pixmap, Font, Fg, Bg) ->
    X1 = draw_char(H, X, Y, Pixmap, Font, Fg, Bg),
    draw_string(T, X1, Y, Pixmap, Font, Fg, Bg);
draw_string([], X, _Y, _Pixmap, _Font, _Fg, _Bg) ->
    X.

