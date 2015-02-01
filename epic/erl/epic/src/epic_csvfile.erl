%%% File    : epic_csvfile.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Read csv files
%%% Created : 17 Sep 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_csvfile).

-export([fold/3, fold/4]).
-export([line/1, line/2]).
-export([options/1, options/2]).

-import(lists, [reverse/1]).

-define(CHUNK_SIZE, 1024).

-record(opt,
	{
	  line = 1,
	  file = "",      %% file name if used
	  comment,        %% comment character
	  fc = $;,        %% field delimiter
	  quote,          %% quote character
	  xquote,         %% extra quote character
	  qnl = false,    %% allow new line in quotes
	  nl = $\n,       %% new line char
	  space           %% field white space handling 
	 }).

-define(char(X), (((X) >= 0) and ((X) =< 255))).

%% create a options record
options(Opts) ->
    options(Opts, #opt {}).

options([{Key,Val}|Options],Opt) ->
    case Key of
	line when integer(Val) ->
	    options(Options, Opt#opt { line = Val });
	comment when ?char(Val) ->
	    options(Options, Opt#opt { comment = Val });
	fc when ?char(Val) ->
	    options(Options, Opt#opt { fc = Val });
	quote when ?char(Val) ->
	    options(Options, Opt#opt { quote = Val });
	xquote when ?char(Val) ->
	    options(Options, Opt#opt { xquote = Val });
	qnl when Val == true ->
	    options(Options, Opt#opt { qnl = true });
	qnl when Val == false ->
	    options(Options, Opt#opt { qnl = false });
	nl when ?char(Val) ->
	    options(Options, Opt#opt { nl = Val });
	space when Val == keep ->
	    options(Options, Opt#opt { space = keep });
	space when Val == trim ->
	    options(Options, Opt#opt { space = trim });
	space when Val == normalize ->
	    options(Options, Opt#opt { space = normalize });
	_ ->
	    erlang:fault(badarg)
    end;
options([],Opt) ->
    Opt.
    
%%
%% Load a XLS/CSV (:;,... separated) 
%%
fold(File,Fun,St) ->
    fold1(File,#opt { },Fun,St).

fold(File,Opts,Fun,St) when list(Opts) ->
    fold1(File, options(Opts), Fun, St);
fold(File,Opt,Fun,St) when record(Opt, opt) ->
    fold1(File, Opt, Fun, St).

fold1(File, Opt, Fun, St) ->
    case file:open(File, [read]) of
	{ok,Fd} ->
	    Res = (catch foldfd(Fd,[],Opt#opt { file = File}, Fun, 
				Opt#opt.line, St)),
	    file:close(Fd),
	    case Res of
		{'EXIT',Reason} ->
		    exit(Reason);
		St1 -> St1
	    end;
	Error -> Error
    end.

foldfd(Fd, Cs, Opt, Fun, Ln, St) ->
    case scan_tokens(Fd,Cs,Ln,[],Opt) of
	eof ->
	    St;
	{[],Cs1,Ln1} ->
	    foldfd(Fd, Cs1, Opt, Fun, Ln1, St);
	{Ts,Cs1,Ln1} ->
	    case is_empty(Ts) of
		true ->
		    foldfd(Fd,Cs1, Opt, Fun, Ln1, St);
		false ->
		    case catch Fun(Ts, St) of
			{'EXIT',Reason} ->
			    io:format("~s:~w: crash ~p\n >~p\n",
				      [Opt#opt.file,Ln,Reason,Ts]),
			    exit(Reason);
			St1 ->
			    foldfd(Fd,Cs1,Opt,Fun,Ln1,St1)
		    end
	    end
    end.

line(Data) ->
    line1(Data, #opt {}).

line(Data, Opts) when list(Opts) ->
    line1(Data, options(Opts));
line(Data, Opt) when record(Opt, opt) ->
    line1(Data, Opt).

line1(LineData, Opt) ->
    case scan_tokens(string,LineData,Opt#opt.line,[],Opt) of
	eof ->
	    [];
	{Ts,_,_} -> Ts
    end.
    

scan_tokens(Fd,Cs,Ln,Ts,Opt) ->
    scan_tokens(Fd,Cs, Ln, [], Ts, Opt).

scan_tokens(Fd,[C|Cs],Ln,Acc,Ts,Opt) ->
    if C == Opt#opt.fc ->
	    scan_tokens(Fd,Cs,Ln,[token(Acc,Opt#opt.space)|Ts], Opt);
       C == Opt#opt.nl ->
	    {reverse([token(Acc,Opt#opt.space)|Ts]),Cs,Ln+1};
       C == Opt#opt.comment ->
	    Cs1 = skip_line(Fd, Cs, Opt),
	    {reverse([token(Acc,Opt#opt.space)|Ts]),Cs1,Ln};
       C == Opt#opt.quote ->
	    scan_quote(Fd,Cs,Ln,C,Acc,Ts,Opt);
       C == Opt#opt.xquote ->
	    scan_quote(Fd,Cs,Ln,C,Acc,Ts,Opt);
       true ->
	    scan_tokens(Fd,Cs,Ln,[C|Acc],Ts,Opt)
    end;
scan_tokens(Fd,[],Ln,Acc,Ts,Opt) ->
    case more(Fd) of
	eof ->
	    if Acc == [], Ts == [] ->
		    eof;
	       true ->
		    {reverse([token(Acc,Opt#opt.space)|Ts]),[],Ln}
	    end;
	Cs ->
	    scan_tokens(Fd,Cs,Ln,Acc,Ts,Opt)
    end.


scan_quote(Fd,[C|Cs],Ln,Q,Acc,Ts,Opt) ->
    if C == Q ->
	    scan_tokens(Fd,Cs,Ln,Acc,Ts,Opt);
       C == Opt#opt.nl ->
	    if Opt#opt.qnl == true ->
		    scan_quote(Fd,Cs,Ln+1,Q,[C|Acc],Ts,Opt);
	       true ->
		    {reverse([token(Acc,Opt#opt.space)|Ts]),Cs,Ln}
	    end;
       true ->
	    scan_quote(Fd,Cs,Ln,Q,[C|Acc],Ts,Opt)
    end;
scan_quote(Fd,[],Ln,Q,Acc,Ts,Opt) ->
    case more(Fd) of
	eof ->
	    if Acc == [], Ts == [] ->
		    eof;
	       true ->
		    {reverse([token(Acc,Opt#opt.space)|Ts]),[],Ln}
	    end;
	Data ->
	    scan_quote(Fd,Data,Ln,Q,Acc,Ts,Opt)
    end.

skip_line(Fd, [C|Cs], Opt) ->
    if C == Opt#opt.nl -> Cs;
       true -> skip_line(Fd, Cs, Opt)
    end;
skip_line(Fd, [], Opt) -> 
    case more(Fd) of
	eof -> [];
	Cs ->  skip_line(Fd, Cs,Opt)
    end.
	
more(string) ->
    eof;
more(Fd) ->
    case file:read(Fd, ?CHUNK_SIZE) of
	{ok,Data} ->
	    Data;
	eof -> 
	    eof
    end.

    
    
	    

is_empty([""|Ts]) -> is_empty(Ts);
is_empty([_|_]) -> false;
is_empty([]) -> true.

token(Rev,undefined) ->
    token(Rev, trim);
token(Rev,keep) -> %% keep blanks
    reverse(Rev);
token(Rev,trim) -> %% trim of blanks
    trim(reverse(trim(Rev)));
token([],normalize) -> %% trim blanks but leave a blank if none empty
    [];
token(Rev,normalize) -> 
    case reverse(trim(Rev)) of
	[] -> " ";
	Tok -> trim(Tok)
    end.

trim([$\s|Cs]) -> trim(Cs);
trim([$\t|Cs]) -> trim(Cs);
trim([$\n|Cs]) -> trim(Cs);
trim([$\r|Cs]) -> trim(Cs);
trim(Cs) -> Cs.


	    
		      
		       
	    
	    

    
    


		
	    
    
	    



