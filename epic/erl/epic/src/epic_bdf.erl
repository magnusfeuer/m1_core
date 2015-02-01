%%% File    : epic_bdf.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : BDF font format parser
%%% Created : 17 Sep 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_bdf).

-compile(export_all).

-import(lists, [reverse/1, map/2, filter/2]).

-include("../include/epic.hrl").
-include("epic_int.hrl").


-record(bdf_font,
	{
	  font,             %% Font name
	  size,             %% {PointSize,Xres,Yres}
	  bits_per_pixel=1, %% 1,2,4 or 8
	  boundingbox,      %% {FFBx,FFBy,Xoff,Yoff}
	  properties,       %% #bdf_properties
	  metricsset = 0,
	  swidth,           %% {swx0, swy0}
	  dwidth,           %% {dwx0, dwy0}
	  swidth1,          %% {swx1, swy1}
	  dwidth1,          %% {dwx1, dwy1}
	  vvector,          %% {xoff, yoff}
	  glyphs            %% [#bdf_glyph]
	 }).

-record(bdf_properties,
	{
	  foundry,
	  family_name,
	  weight_name,
	  slant,
	  setwidth_name,
	  add_style_name,
	  pixel_size,
	  point_size,
	  resolution_x,
	  resolution_y,
	  spacing,
	  average_width,
	  charset_registry,
	  charset_encoding,
	  cap_height,
	  x_height,
	  font_ascent,
	  font_descent,
	  face_name,
	  default_char,
	  relative_setwidth,
	  relative_weight,
	  charset_collections,
	  full_name
	 }).

-record(bdf_glyph,
	{
	  name,      %% glyph name
	  encoding,  %% index...
	  swidth,    %% {swx0, swy0}
	  dwidth,    %% {dwx0, dwy0}
	  swidth1,   %% {swx1, swy1}
	  dwidth1,   %% {dwx1, dwy1}
	  vvector,   %% {xoff, yoff}
	  bbx,       %% {BBw,BBh,BBxoff,BByoff}
	  bitmap     %% binary
	 }).

read_font_path([Dir|Ds], Name) ->
    case read_font_file(filename:join(Dir, Name)) of
	{error,enoent} ->
	    read_font_path(Ds, Name);
	Res -> Res
    end;
read_font_path([], _Name) ->
    {error, enoent}.

read_font_file(File) ->
    Res = epic_csvfile:fold(File,
			    [{quote,$\"},{space,trim},{fc,$\s}], %% "
			      fun bdf/2, undefined),
    if is_record(Res, bdf_font) -> 
	    {ok,{?MODULE, Res}};
       true ->
	    Res
    end.

%% make string lower case 
tolower([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs]) ->
    [C|tolower(Cs)];
tolower([]) ->
    [].


%%
%% Generate ONE bitmap for all characters in the font
%% Also a ets table is generated with encoding information
%% for each glyph like offset in bitmap etc
%%
make_font(F) ->
    P = F#bdf_font.properties,
    {_FFBx,FFBy,_Xoff,Yoff} = F#bdf_font.boundingbox,
    BWidth = lists:sum(map(fun(#bdf_glyph { bbx={BBw,_,_,_}}) -> 
				   BBw
			   end, F#bdf_font.glyphs)),
    BHeight = lists:max(map(fun(#bdf_glyph { bbx={_,BBh,_,_}}) -> 
				    BBh
			    end, F#bdf_font.glyphs)),
    GlyphInfo = ets:new(glyphinfo, [public,{keypos,#eglyph.encoding}]),
    io:format("Font bitmap: ~w x ~w\n", [BWidth, BHeight]),
    io:format("       bbox: ~w\n", [F#bdf_font.boundingbox]),
    io:format("       name: ~s\n", [P#bdf_properties.family_name]),
    io:format("       size: ~w\n", [P#bdf_properties.pixel_size]), 
    {ok,Bitmap} = ebitmap:create(BWidth, BHeight),
    ebitmap:fill(Bitmap, 0),
    lists:foldl(
      fun(G,X) ->
	      #bdf_glyph { name=Name,
			   encoding=Encoding,
			   bitmap=Bits,
			   bbx={BBw,BBh,BBxoff,BByoff},
			   dwidth={Dwx0,Dwy0} } = G,
%%	      io:format("Glyph name: ~s\n", [Name]),
%%	      io:format("  encoding: ~w\n", [Encoding]),
%%	      io:format("       bbx: ~w\n", [{BBw,BBh,BBxoff,BByoff}]),
%%	      io:format("      X:~w, W:~w, H:~w\n", [X,BBw,BBh]),
	      ebitmap:put_bits(Bitmap, X, 0, BBw, BBh, Bits),
	      %% FIXME: unicode map all characters!!!
	      Ix = if Encoding == -1 -> Name; true -> Encoding end,
	      ets:insert(GlyphInfo,
			 #eglyph { encoding = Ix,
				   bmp_offs = X,
				   width    = BBw,
				   height   = BBh,
				   xoffs    = BBxoff,
				   yoffs    = BByoff,
				   dwx      = Dwx0,
				   dwy      = Dwy0 }),
	      X+BBw
      end, 0, F#bdf_font.glyphs),
    {ok,#efont { name=P#bdf_properties.family_name,
		 size=P#bdf_properties.pixel_size,
		 descent=-Yoff, 
		 ascent=FFBy+Yoff,
		 bitmap=Bitmap,
		 glyphs=GlyphInfo}}.


%% debug
bdf0(Line, St) ->
    io:format("~p\n", [Line]),
    bdf(Line,St).

bdf(["COMMENT"|_], St) -> 
    St;
bdf(["CONTENTVERSION",_IntVersion], St) -> 
    %% Upgrade information
    St;
bdf(["STARTFONT",_Version], undefined) ->  
    %% Version is the fileformat used
    #bdf_font { };
%% Font data
bdf(Data, F) when record(F,bdf_font) ->
    case Data of
	["FONT",Name] ->    %% String
	    F#bdf_font { font = Name};
	["SIZE"|Values] ->  %% {PointSize, Xres, Yres [,Bits_Per_Pixel]}
	    Vs = size_tuple(Values),
	    case Vs of
		{PointSize,XRes,YRes,BitsPerPixel} ->
		    F#bdf_font { size = {PointSize,XRes,YRes},
				 bits_per_pixel = BitsPerPixel };
		_ ->
		    F#bdf_font { size = Vs }
	    end;
	["BITS_PER_PIXEL", N] -> %% Version 2.3
	    F#bdf_font { bits_per_pixel = list_to_integer(N)};
	["FONTBOUNDINGBOX"|Values] -> %% {FBBx FBBy Xoff Yoff}
	    F#bdf_font { boundingbox = size_tuple(Values) };
	["STARTPROPERTIES", _N] ->  %% N = number of properties
	    [#bdf_properties{} | F];
	["SWIDTH"|Values] -> %% {swx0, swy0}
	    F#bdf_font { swidth=size_tuple(Values)};
	["DWIDTH"|Values] -> %% {dwx0, dwy0}
	    F#bdf_font { dwidth=size_tuple(Values)};
	["SWIDTH1"|Values] -> %% {swx1, swy1}
	    F#bdf_font { swidth1=size_tuple(Values)};
	["DWIDTH1"|Values] -> %% {dwx1, dwy1}
	    F#bdf_font { dwidth1=size_tuple(Values)};
	["VVECTOR"|Values] -> %% {xoff,yoff}
	    F#bdf_font { vvector=size_tuple(Values)};
	["CHARS",_N] ->  %% N = nglyphs
	    [[] | F];
	["METRICSSET",N] -> %% Writing direction
	    F#bdf_font { metricsset = list_to_integer(N)};
	_ ->
	    io:format("Ignore: ~p\n", [Data]),
	    F
    end;
bdf(Data, [C|St]) when record(C,bdf_glyph) ->
    case Data of
	["ENCODING", Code] ->
	    [C#bdf_glyph { encoding=list_to_integer(Code)}|St];
	["SWIDTH"|Values] ->
	    [C#bdf_glyph { swidth=size_tuple(Values)}|St];
	["DWIDTH"|Values] ->
	    [C#bdf_glyph { dwidth=size_tuple(Values)}|St];
	["SWIDTH1"|Values] ->
	    [C#bdf_glyph { swidth1=size_tuple(Values)}|St];
	["DWIDTH1"|Values] ->
	    [C#bdf_glyph { dwidth1=size_tuple(Values)}|St];
	["VVECTOR"|Values] -> %% {xoff,yoff}
	    [C#bdf_glyph { vvector=size_tuple(Values)}|St];
	["BBX"|Values] ->    %% {BBw,BBh,BBxoff,BByoff}
	    [C#bdf_glyph { bbx=size_tuple(Values)}|St];
	["ENDCHAR"|_] ->
	    [Chars|St1] = St,
	    [[C|Chars]|St1];
	["BITMAP"|_] ->
	    [[],C|St];
	["ATTRIBUTES"|_] ->  %% 4-DIGIT hex code (spec?)
	    [C|St];
	_ ->
	    io:format("Ignore: ~p\n", [Data]),
	    [C|St]
    end;
bdf(Data, [Bits,C|St]) when list(Bits),record(C,bdf_glyph) ->
    case Data of
	["ENDCHAR"] ->
	    [Chars|St1] = St,
	    [[C#bdf_glyph { bitmap=list_to_binary(reverse(Bits))}|Chars]|St1];
	[Hex] ->
	    BitRow = erlang:list_to_integer(Hex, 16),
	    Sz = length(Hex)*4,
	    [[<<BitRow:Sz>>|Bits],C|St]
    end;
	
bdf(Data, [Chars|F]) when list(Chars),record(F,bdf_font) ->
    case Data of
	["STARTCHAR",Name] ->
	    [#bdf_glyph{name=Name},Chars|F];
	["ENDFONT"] ->
	    F#bdf_font { glyphs = reverse(Chars)};
	_ ->
	    io:format("Ignore: ~p\n", [Data]),
	    [Chars|F]
    end;
bdf(Data, [P|F]) when record(P,bdf_properties) ->
    case Data of
	["FOUNDRY", Name] ->
	    [P#bdf_properties{foundry=Name} | F];
	["FAMILY_NAME", Name] ->
	    [P#bdf_properties{family_name=Name} | F];
	["WEIGHT_NAME", Name] ->
	    [P#bdf_properties{weight_name=Name} | F];
	["SLANT",Slant] ->
	    [P#bdf_properties{slant=Slant} | F];
	["SETWIDTH_NAME",Name] ->
	    [P#bdf_properties{setwidth_name=Name} | F];
	["ADD_STYLE_NAME",Name] ->
	    [P#bdf_properties{add_style_name=Name} | F];
	["PIXEL_SIZE", Size] ->
	    [P#bdf_properties{pixel_size=list_to_integer(Size)} | F];
	["POINT_SIZE", Size] ->
	    [P#bdf_properties{point_size=list_to_integer(Size)} | F];
	["RESOLUTION_X", Res] ->
	    [P#bdf_properties{resolution_x=list_to_integer(Res)} | F];
	["RESOLUTION_Y", Res] ->
	    [P#bdf_properties{resolution_y=list_to_integer(Res)} | F];
	["SPACING",Spacing] ->
	    [P#bdf_properties{spacing=Spacing} | F];	    
	["AVERAGE_WIDTH",W] ->
	    [P#bdf_properties{average_width=list_to_integer(W)} | F];
	["CHARSET_REGISTRY",Name] ->
	    [P#bdf_properties{charset_registry=Name} | F];
	["CHARSET_ENCODING",Code] ->
	    [P#bdf_properties{charset_encoding=Code} | F];
	["CAP_HEIGHT",CapH] ->
	    [P#bdf_properties{cap_height=list_to_integer(CapH)} | F];
	["X_HEIGHT",XH] ->
	    [P#bdf_properties{x_height=list_to_integer(XH)} | F];
	["FONT_ASCENT",Ascent] ->
	    [P#bdf_properties{font_ascent=list_to_integer(Ascent)} | F];
	["FONT_DESCENT",Descent] ->
	    [P#bdf_properties{font_descent=list_to_integer(Descent)} | F];
	["FACE_NAME",Name] ->
	    [P#bdf_properties{face_name=Name} | F];
	["DEFAULT_CHAR",Char] ->
	    [P#bdf_properties{default_char=list_to_integer(Char)} | F];
	["RELATIVE_SETWIDTH",Width] ->
	    [P#bdf_properties{relative_setwidth=list_to_integer(Width)} | F];
	["RELATIVE_WEIGHT",Weight] ->
	    [P#bdf_properties{relative_weight=list_to_integer(Weight)} | F];
	["CHARSET_COLLECTIONS",Colls] ->
	    [P#bdf_properties{charset_collections=Colls} | F];
	["FULL_NAME", Name] ->
	    [P#bdf_properties{full_name=Name} | F];
	["ENDPROPERTIES"] ->
	    F#bdf_font { properties = P };
	_ ->
	    io:format("Ignore: ~p\n", [Data]),
	    [P|F]
    end.

size_tuple(Values) ->
    list_to_tuple(map(fun(V) -> list_to_integer(V) end, Values)).


%%
%% Generate a EPIC eft file from bdf font data
%%
%% The efnt file format (all integers are little endian)
%%
write_efnt_file(F, File) ->
    P = F#bdf_font.properties,
    %% FIXME: add glyphs without encoding?
    BGs = filter(
	    fun(G) ->
		    if G#bdf_glyph.encoding < 0 ->
			    io:format("Glyph '~s' has no encoding, removed\n",
				      [G#bdf_glyph.name]),
			    false;
		       true ->
			    true
		    end
	    end,  F#bdf_font.glyphs),
    %% Generate glyph data table
    Gs0 = map(fun(G) ->
		      %% FIXME: map encoding to unicode space
		      %%  this will work for ascii, iso8859-1 utf
		      %%  but NOT for other encodings
		      {G#bdf_glyph.name,G#bdf_glyph.encoding,efnt_glyph(G)}
	      end, BGs),
    %% Sort according to encoding to get tables right.
    Gs = lists:keysort(2, Gs0),

    %% Generate the Font info structure
    FontInfo = efnt_info(F),

    %% Generate string table (init with ? to allow offset=0 to mean undefined
    {_Offs, S0} = efnt_string_table_add("?", {0,[]}),
    {FoundryOffset,S1} = efnt_string_table_add(P#bdf_properties.foundry, S0),
    {FamilyOffset,S2} = efnt_string_table_add(P#bdf_properties.family_name, S1),
    %% Add all glyph names (ignore names starting with U+ not useful..)

    {S3,Gs1} = efnt_glyph_string_table(Gs, S2),
    StringTableStart = 0,
    StringTableData  = efnt_string_table_data(S3),
    StringTableLen   = size(StringTableData),

    OffsetTableStart = StringTableLen,
    {StartCode,EndCode,RelOffsetTable} = efnt_glyph_offset_table(Gs1),
    OffsetTableLen  = efnt_offset_table_size(RelOffsetTable),
    OffsetTable = efnt_offset_table_add(RelOffsetTable, 
					StringTableLen + OffsetTableLen),
    OffsetTableData = efnt_offset_table_data(OffsetTable),
    OffsetTableLen  = size(OffsetTableData),  %% Assertion!!!

    GlyphTableStart = OffsetTableStart + OffsetTableLen,
    GlyphTableData  = efnt_glyph_table_data(Gs1),
    GlyphTableLen   = size(GlyphTableData),

    FontFileData = 
	<<
	 ?EFNT_MAGIC/binary,
	 FoundryOffset:32/unsigned-little,
	 FamilyOffset:32/unsigned-little,
	 FontInfo/binary,
	 StartCode:32/unsigned-little,        %% Encoding Start
	 EndCode:32/unsigned-little,          %% Encoding Stop
	 ($?):32/unsigned-little,             %% Encoding Default
	 StringTableStart:32/unsigned-little, %% Offset to string table
	 StringTableLen:32/unsigned-little,   %% Length of string table
	 OffsetTableStart:32/unsigned-little,
	 OffsetTableLen:32/unsigned-little,
	 GlyphTableStart:32/unsigned-little,
	 GlyphTableLen:32/unsigned-little,
	 %% NOTE! add pad to 16 byte aligment if needed!
	 StringTableData/binary,
	 OffsetTableData/binary,
	 GlyphTableData/binary 
	 >>,
     file:write_file(File, FontFileData).

%% WEIGHT: Medium | Bold 
%% SLANT:  I | R

efnt_info(F) ->
    P = F#bdf_font.properties,
    {_FFBx,FFBy,_Xoff,Yoff} = F#bdf_font.boundingbox,
    Weight = 
	case tolower(P#bdf_properties.weight_name) of
	    "medium" -> ?EFONT_WEIGHT_MEDIUM;
	    "bold"   -> ?EFONT_WEIGHT_BOLD;
	    "demibold" -> ?EFONT_WEIGHT_DEMIBOLD;
	    X0 ->
		io:format("Weight ~s unknown\n", [X0]),
		?EFONT_WEIGHT_NONE
	end,
    Slant = 
	case tolower(P#bdf_properties.slant) of
	    "r" -> ?EFONT_SLANT_ROMAN;
	    "i" -> ?EFONT_SLANT_ITALIC;
	    "o" -> ?EFONT_SLANT_OBLIQUE;
	    "ri" -> ?EFONT_SLANT_REVERSE_ITALIC;
	    "ro" -> ?EFONT_SLANT_REVERSE_OBLIQUE;
	    "ot" -> ?EFONT_SLANT_OTHER;
	    X1 ->
		io:format("SLANT ~s unknown\n", [X1]),
		?EFONT_SLANT_NONE
	end,
    Width = 
	case tolower(P#bdf_properties.setwidth_name) of
	    "normal" -> ?EFONT_WIDTH_NORMAL;
	    "condensed" -> ?EFONT_WIDTH_CONDENSED;
	    "narrow" -> ?EFONT_WIDTH_NARROW;
	    "double wide" -> ?EFONT_WIDTH_DOUBLE_WIDE;
	    X2 ->
		io:format("SETWIDTH_NAME ~s unknown\n", [X2]),
		?EFONT_WIDTH_NONE
	end,
    Style = 
	case tolower(P#bdf_properties.add_style_name) of
	    "serif" -> ?EFONT_STYLE_SERIF;
	    "sans serif" -> ?EFONT_STYLE_SANS_SERIF;
	    "informal" -> ?EFONT_STYLE_INFORMAL;
	    "decorated" -> ?EFONT_STYLE_DECORATED;
	    X3 ->
		io:format("ADD_STYLE_NAME ~s unknown\n", [X3]),
		?EFONT_STYLE_NONE
	end,

    Spacing = 
	case tolower(P#bdf_properties.spacing) of
	    "proportional" -> ?EFONT_SPACING_PROPORTIONAL;
	    "monospaced" -> ?EFONT_SPACING_MONOSPACED;
	    "charcell" -> ?EFONT_SPACING_CHAR_CELL;
	    X4 ->
		io:format("SPACING ~s unknown\n", [X4]),
		?EFONT_SPACING_NONE
	end,
    PixelType =
	?EPIXEL_TYPE_A8,
    PixelSize = 
	case P#bdf_properties.pixel_size of
	    undefined -> 0;
	    Y1 -> Y1
	end,
    PointSize = 
	case P#bdf_properties.point_size of
	    undefined -> 0;
	    Y2 -> Y2
	end,
    ResX = 
	case P#bdf_properties.resolution_x of
	    undefined -> 0;
	    Y3 -> Y3
	end,
    ResY = 
	case P#bdf_properties.resolution_y of
	    undefined -> 0;
	    Y4 -> Y4
	end,
    <<
     Weight:32/unsigned-little,
     Slant:32/unsigned-little,
     Width:32/unsigned-little,
     Style:32/unsigned-little,
     Spacing:32/unsigned-little,
     PixelType:32/unsigned-little,       %% Type of pixels used
     PixelSize:32/unsigned-little,       %% Font pixel size
     PointSize:32/unsigned-little,       %% Font point size in decipoint
     ResX:32/unsigned-little,            %% Font device horizontal Resolution decipoint
     ResY:32/unsigned-little,            %% Font device vertical Resolution decipoint
     (-Yoff):32/unsigned-little,         %% Descent
     (FFBy+Yoff):32/unsigned-little      %% Ascent 
     %% NOTE! add pad to 16 byte aligment if needed!
     >>.


%%
%% Pad a binary to N byte boundry
%% 
efnt_table_pad(Table, Align) ->
    efnt_table_pad(Table, Align, 0).
    
efnt_table_pad(Table, Align, Fill) when binary(Table) ->
    N = size(Table),
    case (Align - (N rem Align)) rem Align of
	0 ->
	    Table;
	Pad ->
	    PadBin = list_to_binary(lists:duplicate(Pad, Fill)),
	    <<Table/binary, PadBin/binary>>
    end;
efnt_table_pad(Table, Align, Fill) when list(Table) ->
    N = length(Table),
    case (Align - (N rem Align)) rem Align of
	0 ->
	    Table;
	Pad ->
	    Table ++ lists:duplicate(Pad, Fill)
    end.

%%
%% Add a string to the string table
%%	 
efnt_string_table_add(String, {Size, Data}) ->
    io:format("Adding string: '~s'\n", [String]),
    %% trunate string to 255 chars start with length and end with 0
    Length = length(String),
    Entry = if Length > 255 ->
		    [255,string:substr(String, 255),0];
	       true ->
		    [Length,String,0]
	    end,
    EntrySize = if Length > 255 -> 257;
		   true -> Length + 2
		end,
    {Size, {Size+EntrySize,[Data,Entry]}}.

efnt_string_table_size({Size, _}) -> 
    Size.

efnt_string_table_data({_Size,Data}) ->
    efnt_table_pad(list_to_binary(Data), 16).

%%
%% Add all glyph "names" to the string table and
%% update the glyph data with the name offset
%%
efnt_glyph_string_table(Gs, StringTable) ->
    efnt_glyph_string_table(Gs, StringTable, []).

efnt_glyph_string_table([{Name, Code, Data}|Gs], StringTable, GlyphTable) ->
    {NameOffset,StringTable1}
	= case Name of
	      undefined -> {0, StringTable};
	      "U+"++_   -> {0, StringTable};
	      _ -> efnt_string_table_add(Name, StringTable)
	  end,
    %% Add glyph name to data
    Data1 = <<NameOffset:32/little-unsigned, Data/binary>>,
    io:format("Glyph: ~s name offset = ~w\n", [Name, NameOffset]),
    efnt_glyph_string_table(Gs, StringTable1,
			   [{NameOffset,Code,Data1}|GlyphTable]);
efnt_glyph_string_table([], StringTable, GlyphTable) ->
    {StringTable, reverse(GlyphTable)}.

%%
%% Generate the glyph (relative) offset table.
%% Each offset is an unsigned 32 bit offset in Little endian.
%% A -1 (or less than zero) mark a not used slot.
%%
efnt_glyph_offset_table(Gs=[{_NameOffset,StartCode,_Data}|_]) ->
    {EndCode,NextOffset,OffsetTable} = 
	efnt_glyph_offset_table(Gs, StartCode-1, 0, []),
    %% Pad offset table to align = 4 (of 32 bit data = 16 bit alignment)
    {StartCode,EndCode,efnt_table_pad(OffsetTable,4,empty)}.

efnt_glyph_offset_table([{NameOffset,Code,Data}|Gs],
			PrevCode,NextOffset,OffsetTable) ->
    %% io:format("Code = ~w, PrevCode = ~w\n", [Code,PrevCode]),
    Gap = (Code-PrevCode)-1,
    %% Fill in gaps in the encoding space with atom empty
    OffsetTable1 = lists:duplicate(Gap, empty) ++ OffsetTable,
    efnt_glyph_offset_table(Gs, Code, NextOffset+size(Data),
			    [NextOffset | OffsetTable1]);
efnt_glyph_offset_table([],PrevCode,NextOffset,OffsetTable) ->
    {PrevCode,NextOffset,reverse(OffsetTable)}.

efnt_offset_table_size(OffsetTable) ->
    length(OffsetTable)*4.

efnt_offset_table_data(OffsetTable) ->
    list_to_binary(
      map(fun(empty) -> <<0:32/unsigned-little>>;
	     (Offset) -> <<Offset:32/unsigned-little>>
	  end, OffsetTable)).

%% Update offset entries with offset 'Add' (except for zeros)
%% normally makeing relative offset table absolute.
efnt_offset_table_add(OffsetTable, Add) ->
    map(fun(empty) -> empty;
	   (Offs) -> Offs + Add 
	end, OffsetTable).


efnt_glyph_table_data(Gs) ->    
    list_to_binary(map(fun({_NameOffs,_Code,Data}) -> Data end, Gs)).

%%
%% Convert a bdf_glyph into a binary epic_glyph (little endian)
%%
%%  The glyph header is 16 bytes (when name offset is added)
%%  The pixel data MUST be padded to 16 bytes boundry, this
%%  will keep the table SSE2 data aligned when possible
%%
efnt_glyph(#bdf_glyph { name=Name,
			encoding=Encoding,
			bitmap=Bits,
			bbx={BBw,BBh,BBxoff,BByoff},
			dwidth={Dwx0,Dwy0} }) ->
    io:format("GlyphBits: '~s' ~w x ~w bits = ~w\n", 
	      [Name, BBw, BBh, Bits]),
    Pixels0 = efnt_pixels_data(BBw, BBh, Bits),
    Pixels  = efnt_table_pad(Pixels0, 16),
    %% 16 Byte header
    <<
     %% NameOffset:32/unsigned-little is patched on later!
     BBw:16/unsigned-little,
     BBh:16/unsigned-little,
     BBxoff:16/signed-little,
     BByoff:16/signed-little,
     Dwx0:16/signed-little,
     Dwy0:16/signed-little,
     Pixels/binary >>.

%% Generate pixel A8 data and pad to 16 byte alignment
efnt_pixels_data(Width,Height,Bits) ->
    efnt_pixels_data(Width,Height,0,Bits,[]).

%% Scan each row and convert to target pixel type
%% Note we want each row to be a mutiple of 16 bytes
%% This make it easy to scan each row with SSE2/ALTIVEC
%% The font is normally mmap'ed so it should be ok....
efnt_pixels_data(Width,0,_Offset,_Bits,Acc) ->
    list_to_binary(reverse(Acc));
efnt_pixels_data(Width,I,Offset,Bits,Acc) ->
    Row = efnt_pixel_row(Width, Offset, Bits, []),
    ARow = efnt_table_pad(Row, 16, 0),  %% align to 16 bytes!
    efnt_pixels_data(Width,I-1,Offset + ((Width+7) div 8)*8,Bits,[ARow|Acc]).

%% Convert a row of bits into target alpha data
efnt_pixel_row(0, _Offset, _Bits, Acc) ->
    reverse(Acc);
efnt_pixel_row(I, Offset, Bits, Acc) ->
    Pad = (8 - ((Offset+1) band 7)) band 7,
    %% io:format("Offset = ~w, Pad = ~w\n", [Offset, Pad]),
    <<_:Offset, Bit:1, _:Pad, _/binary>> = Bits,
    efnt_pixel_row(I-1, Offset+1, Bits, [Bit*255|Acc]).
			      


	
