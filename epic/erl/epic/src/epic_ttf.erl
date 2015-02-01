%%% File    : epic_ttf.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Read True Type Fonts
%%% Created : 10 Sep 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_ttf).

-compile(export_all).

-record(ttf_header,
	{ vsn,
	  numTables,
	  searchRange,
	  entrySelector,
	  rangeShift
	 }).

-record(ttf_ent,
	{
	  tag,
	  checksum,
	  offset,
	  length,
	  data }).


open(File) ->
    file:open(File, [read,binary]).

close(Fd) ->
    file:close(Fd).

scan_file(File) ->
    case open(File) of
	{ok,Fd} ->
	    {ok,Hdr} = read_header(Fd),
	    io:format("Hdr=~p\n", [Hdr]),
	    scan_directory(Fd, Hdr#ttf_header.numTables,
			   fun(Ent,_St) ->
				   io:format("Ent=~p\n", [Ent])
			   end, []),
	    close(Fd);
	Error -> Error
    end.

scan_directory(_Fd,0,_Fun,Acc) ->
    Acc;
scan_directory(Fd,I,Fun,Acc) ->
    case read_ent(Fd) of
	{ok,Ent} ->
	    Acc1 = Fun(Ent,Acc),
	    scan_directory(Fd, I-1, Fun, Acc1);
	eof -> {error,truncated};
	Error -> Error
    end.

read_header(Fd) ->
    case file:read(Fd, 12) of
	{ok,<<VsnHi:16,VsnLo:16,
	      NumTables:16,
	      SearchRange:16,
	      EntrySelector:16,
	      RangeShift:16>>} ->
	    {ok, #ttf_header { vsn = {VsnHi,VsnLo},
			       numTables = NumTables,
			       searchRange = SearchRange,
			       entrySelector = EntrySelector,
			       rangeShift = RangeShift }};
	{ok,_}  ->
	    {error, truncated};
	Error -> Error
    end.

read_ent(Fd) ->
    case file:read(Fd, 16) of
	{ok, <<Tag:4/binary, CheckSum:32,
	      Offset:32, Length:32>>} ->
	    {ok,#ttf_ent { tag = binary_to_list(Tag),
			   checksum = CheckSum,
			   offset   = Offset,
			   length   = Length }};
	{ok, _} ->
	    {error, truncated};
	Error ->
	    Error
    end.
	     

checksum(Bin, Length) ->	     
    checksum(Bin, 0, Length, 0).

checksum(_Bin, _Offset, I, CSum) when I =< 4 ->
    CSum band 16#ffffffff;
checksum(Bin, Offset, I, CSum) ->
    <<_:Offset/binary,C:32,_/binary>> = Bin,
    checksum(Bin, Offset+4, I-4, CSum+C).
