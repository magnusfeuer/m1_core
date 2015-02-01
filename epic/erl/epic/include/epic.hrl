%%
%% Font structures
%%
-define(EFNT_MAGIC, <<$E,$F,$N,$T>>).

-define(EFONT_WEIGHT_NONE,     0).
-define(EFONT_WEIGHT_MEDIUM,   1).
-define(EFONT_WEIGHT_BOLD,     2).
-define(EFONT_WEIGHT_DEMIBOLD, 3).

-define(EFONT_SLANT_NONE,            0).
-define(EFONT_SLANT_ROMAN,           1).
-define(EFONT_SLANT_ITALIC,          2).
-define(EFONT_SLANT_OBLIQUE,         3).
-define(EFONT_SLANT_REVERSE_ITALIC,  4).
-define(EFONT_SLANT_REVERSE_OBLIQUE, 5).
-define(EFONT_SLANT_OTHER,           6).

-define(EFONT_WIDTH_NONE,         0).
-define(EFONT_WIDTH_NORMAL,       1).
-define(EFONT_WIDTH_CONDENSED,    2).
-define(EFONT_WIDTH_NARROW,       3).
-define(EFONT_WIDTH_DOUBLE_WIDE,  4).

-define(EFONT_STYLE_NONE,       0).
-define(EFONT_STYLE_SERIF,      1).
-define(EFONT_STYLE_SANS_SERIF, 2).
-define(EFONT_STYLE_INFORMAL,   3).
-define(EFONT_STYLE_DECORATED,  4).

-define(EFONT_SPACING_NONE,          0).
-define(EFONT_SPACING_PROPORTIONAL,  1).
-define(EFONT_SPACING_MONOSPACED,    2).
-define(EFONT_SPACING_CHAR_CELL,     3).


-record(eglyph,
	{
	  encoding,   %% the encoding value 
	  bmp_offs,   %% xoffset in bitmap
	  width,      %% width of bits 
	  height,     %% height of bits
	  xoffs,      %% x offset from origin
	  yoffs,      %% y offset from origin
	  dwx,        %% x step
	  dwy         %% y step
	 }).

-record(efont,
	{
	  name,      %% font name
	  size,      %% Point size
	  descent,
	  ascent,
	  bitmap,    %% ebitmap with all bits
	  glyphs     %% ets table with #eglyph
	 }).
