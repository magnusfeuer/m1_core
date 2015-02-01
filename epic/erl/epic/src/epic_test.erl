%%% File    : epic_test.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPic ev event tester
%%% Created : 29 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_test).

-compile(export_all).

run() ->
    run([key_press, key_release]).

run(EventMask) ->
    {ok,Win} = ewindow:create(50, 50, 100, 100, EventMask),
    ewindow:attach(Win),
    loop(Win).

loop(Win) ->
    receive
	{eevent, Win, Event} ->
	    event(Win,Event);
	Other ->
	    io:format("GOT: win=~p,  ~p\n", [Win,Other]),
	    loop(Win)
    end.

event(Win, Event) ->
    case Event of
	close ->
	    io:format("CLOSE\n", []),
	    ewindow:destroy(Win),
	    ok;	    
	{key_press, Sym, Mod, Code} ->
	    io:format("KEY PRESS: ~w  mod=~w, code=~w\n", [Sym,Mod,Code]),
	    if Sym == $q ->
		    ewindow:destroy(Win),
		    ok;
	       true ->
		    loop(Win)
	    end;
	{key_release, Sym, Mod, Code} ->
	    io:format("KEY RELEASE: ~w  mod=~w, code=~w\n", [Sym,Mod,Code]),
	    loop(Win);
	{button_press, Button, Where} ->
	    io:format("BUTTON PRESS: ~w where=~w\n", [Button, Where]),
	    loop(Win);
	{button_release, Button, Where} ->
	    io:format("BUTTON RELEASE: ~w where=~w\n", [Button, Where]),
	    loop(Win);
	{motion, Button, Where} ->
	    io:format("MOTION: ~w where=~w\n", [Button, Where]),
	    loop(Win)
    end.

text_demo() ->
    text_demo("timBI18.bdf").

text_demo(FontName) ->
    {ok,Win} = ewindow:create(50, 50, 200, 100, []),
    ewindow:attach(Win),
    {ok,Pic} = epixmap:create(200,100),
    epixmap:fill(Pic, {255,192,192,192}),
    epixmap:attach(Pic),
    {ok,Font} = efont:load_font(FontName),
    efont:draw_string("\"Hello World.\"", 10, 50, Pic, Font),
    epixmap:draw(Pic, Win, 0, 0, 0, 0, 200, 100),
    receive
	{eevent, Win, close} ->
	    io:format("CLOSE\n", []),
	    ewindow:destroy(Pic),
	    ewindow:destroy(Win),
	    ok
    end.
    

line_demo() ->
    {ok,Win} = ewindow:create(50, 50, 200, 100, []),
    ewindow:attach(Win),
    {ok,Pic} = epixmap:create(200,100),
    epixmap:attach(Pic),
    {ok,Gc} = egc:create(),
    draw_rot_loop(Pic,Win,Gc,0,5000),
    epixmap:draw(Pic, Win, 0, 0, 0, 0, 200, 100),
    receive
	{eevent, Win, close} ->
	    io:format("CLOSE\n", []),
	    ewindow:destroy(Pic),
	    ewindow:destroy(Win),
	    ok
    end.

draw_rot_loop(_Pic, _Win, _Gc, _A, 0) ->
    ok;
draw_rot_loop(Pic, Win, Gc, A, I) ->
    Fg = (A rem 200) + 40,
    Bg = (A rem 200),
    epixmap:fill(Pic, {255,192,Fg,Fg}),
    egc:set_foreground_color(Gc, {255,Bg,Bg,Bg}),
    draw_rot(Pic,Gc,A rem 360),
    epixmap:draw(Pic, Win, 0, 0, 0, 0, 200, 100),
    draw_rot_loop(Pic,Win,Gc,A+1,I-1).


draw_rot(Pic,Gc) ->
    draw_rot(Pic,Gc,0).

draw_rot(Pic,Gc,A) ->
    draw_rot(Pic,Gc,A,360+A,10).

draw_rot(_Pic,_Gc,A,Max,_Step) when A >= Max ->
    ok;
draw_rot(Pic,Gc,A, Max,Step) ->
    Ar = A*math:pi()/180,
    X = math:cos(Ar),
    Y = math:sin(Ar),
    egc:set_line_style(Gc, solid),
    epixmap:draw_line(Pic,Gc, 50, 50, 
		      trunc(50*X+50), trunc(50*Y+50)),
    egc:set_line_style(Gc, aalias),
    epixmap:draw_line(Pic, Gc, 150, 50, 
		    trunc(50*X+150), trunc(50*Y+50)),
    draw_rot(Pic,Gc,A+Step,Max,Step).

    
    


	
	

    



