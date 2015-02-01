%%% File    : epic_test.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPic ev event tester
%%% Created : 29 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_ev).


run() ->
    {ok,Win} = ewindow:create(50, 50, 100, 100),
    ewindow:attach(Win),
    loop(Win).

loop(Win) ->
    receive
	{eevent, Win, Event} ->
	    event(Win,Event)
    end.

event(Win, Event) ->
    case Event of
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


	
	

    



