%%% File    : epic_bench.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : Benchmark primitive operations
%%% Created :  4 Sep 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_bench).

-compile(export_all).
    
%%
%% Noop (check the lower limit for call interface)
%% PPC 1.5Ghz  ~ 1.8M calls/sec
%%
noop() ->
    call(noop, [1000000]).

noop(N) ->
    S0 = stat(),
    noop_loop(N),
    S1 = stat(),
    stat_print("noop","", S1, S0, N, [{calls,1}]).

noop_loop(0) -> ok;
noop_loop(I) ->
    epic:noop(),
    noop_loop(I-1).

%%
%% Nop, check erlang simple call. 
%% PPC 1.5GHz  ~ 5.7 M calls/sec
%%
nop() ->
    call(nop, [1000000]).

nop(N) ->
    S0 = stat(),
    nop_loop(N),
    S1 = stat(),
    stat_print("nop","", S1, S0, N,[{calls,1}]).

nop_loop(0) -> ok;
nop_loop(I) ->
    epic:nop(),
    nop_loop(I-1).



%%
%% Benchmark the simplest operation, put_pixel
%% 1. messure call interface
%% 2. messure blend function
%%
%% return Pixel/sec
put_pixel() ->
     put_pixel(1000000).

put_pixel(N) ->
    {ok,Pic} = epixmap:create(10, 10),
    call(put_pixel, [Pic, N, 0]),
    call(put_pixel, [Pic, N, 0]),
    call(put_pixel, [Pic, N, {255,100,100,100}]),
    call(put_pixel, [Pic, N, 16#40646464]),
    call(put_pixel, [Pic, N, {64,100,100,100}]),
    epixmap:destroy(Pic),
    ok.

put_pixel(Pic, N, Pixel) ->
    S0 = stat(),
    put_pixel_loop(Pic, N, Pixel),
    S1 = stat(),
    stat_print("put_pixel",Pixel, S1, S0, N, [{pixels,1}]).

put_pixel_loop(_Pic, 0, _Pixel) -> ok;
put_pixel_loop(Pic, I, Pixel) ->
    epixmap:put_pixel(Pic, 3, 5, Pixel),
    put_pixel_loop(Pic, I-1, Pixel).

%%
%% Benchmark line drawing
%%
%% return Lines/sec
draw_line() ->
    draw_line(100000).

draw_line(N) ->
    {ok,Pic} = epixmap:create(100, 100),
    call(draw_line, [Pic, N, 0]),
    call(draw_line, [Pic, N, 0]),
    call(draw_line, [Pic, N, {255,100,100,100}]),
    call(draw_line, [Pic, N, 16#40646464]),
    call(draw_line, [Pic, N, {50,100,100,100}]),
    epixmap:destroy(Pic),
    ok.

draw_line(Pic, N, Pixel) ->
    S0 = stat(),
    draw_line_loop(Pic, N, Pixel),
    S1 = stat(),
    stat_print("draw", Pixel, S1, S0, N, [{lines,1}]).


%% draw a line of length 100
draw_line_loop(_Pic, 0, _Pixel) -> ok;
draw_line_loop(Pic, I, Pixel) ->
    epixmap:draw_line(Pic, 0, 0, 91, 42, Pixel),
    draw_line_loop(Pic, I-1, Pixel).

%%
%% Fill area becnhmark
%%
fill(N) ->
    W = 100,
    H = 100,
    NoBlendPixel = 16#FF404040,
    BlendPixel   = 16#40404040,
    {ok,Pic} = epixmap:create(W, H), %% W*H pixels
    call(run, [put_pixel, N, Pic, W, H, NoBlendPixel]),
    call(run, [draw_line, N, Pic, W, H, NoBlendPixel]),
    call(run, [fill_rect, N, Pic, W, H, NoBlendPixel]),
    call(run, [put_pixel, N, Pic, W, H, BlendPixel]),
    call(run, [draw_line, N, Pic, W, H, BlendPixel]),
    call(run, [fill_rect, N, Pic, W, H, BlendPixel]),
    epixmap:destroy(Pic).


run(F, N, Pic, W, H, Pixel) ->
    S0 = stat(),
    fill_loop(F, N, Pic, W, H, 0),
    S1 = stat(),
    stat_print(F, Pixel, S1, S0, N, [{pixels,W*H},{fill,1}]).
    		  
fill_loop(_F, 0, _Pic, _W, _H, _P) -> 
    ok;
fill_loop(F, I, Pic, W, H, P) ->
    case F of
	put_pixel -> put_pixel_fill(Pic,W,H,P);
	draw_line -> draw_line_fill(Pic,W,H,P);
	fill_rect -> fill_rect_fill(Pic,W,H,P)
    end,
    fill_loop(F,I-1, Pic, W, H, P).

%%
%% fill are with put_pixel
%%
put_pixel_fill(Pic, W, H, P) ->
    put_pixel_fill(Pic, 0, W, H-1, P).

put_pixel_fill(_Pic, W, W, -1, _P) -> 
    ok;
put_pixel_fill(Pic, W, W, Y, P) -> 
    put_pixel_fill(Pic, 0, W, Y-1, P);
put_pixel_fill(Pic, X, W,  Y, P) ->
    epixmap:put_pixel(Pic, X, Y, P),
    put_pixel_fill(Pic, X+1, W, Y, P).

%%
%% Fill are with draw_line
%%
draw_line_fill(Pic, W, H, P) ->
    draw_line_fill(Pic, 0, W-1, H-1, P).

draw_line_fill(_Pic, _X0, _X1, -1, _P) ->
    ok;
draw_line_fill(Pic, X0, X1, Y, P) ->
    epixmap:draw_line(Pic, X0, Y, X1, Y, P),
    draw_line_fill(Pic, X0, X1, Y-1, P).

%%
%% Fill are with fill_rect
%%
fill_rect_fill(Pix, W, H, P) ->
    epixmap:fill_rectangle(Pix, 0, 0, W-1, H-1, P).

%% spawn and call
call(F, Args) ->
     SELF = self(),
     Pid = spawn(fun() -> 
     		 case catch apply(?MODULE, F, Args) of
		    {'EXIT', Error} ->
		    	     io:format("~s: crash ~p\n", [F, Error]),
			     SELF ! {self(), {error, crash}};
	            Res ->
		    	     SELF ! {self(),Res}
	         end
                 end),
     receive
        {Pid, Res} -> Res
     end.

     

%% display info
stat_print(Label,Case,Stat1,Stat0, N, FmtList) ->
     {Diff, {GCDiff,_}} = stat_delta(Stat1,Stat0),
     PerSec = 1000000*(N / Diff),

     io:format("~s: case=~p, n=~w, gc=~w",[Label,Case,N,GCDiff]),
     lists:foreach(
         fun({Lbl,Scale}) ->
	    io:format(" ~s/s=~w",
	    		[Lbl,trunc(Scale*PerSec)])
	 end, FmtList),
     io:nl().

%% sample info
stat() ->
    {now(), statistics(garbage_collection)}.

stat_delta({T1, GC1}, {T0,GC0}) ->
    {now_diff(T1,T0), gc_diff(GC1,GC0)}.

%% 
gc_diff({N1,R1,_}, {N0,R0,_}) -> {N1-N0,R1-R0}.


%% time diff in us
now_diff({M,S1,U1},{M,S0,U0}) ->
    (S1-S0)*1000000 + (U1-U0);
now_diff({M1,S1,U1},{M0,S0,U0}) ->
    1000000*((M1-M0)*1000000 + (S1-S0)) + (U1-U0).


	





    
     
    

