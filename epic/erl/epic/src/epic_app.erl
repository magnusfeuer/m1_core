%%% File    : epic_app.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : 
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_app).

-behaviour(application).
-export([start/2,stop/1]).

%% start
start(_Type, _StartArgs) ->
    epic_sup:start_link().

%% stop FIXME
stop(_State) ->
  ok.
