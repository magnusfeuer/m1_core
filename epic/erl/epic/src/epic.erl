%%% File    : epic.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPic start/stop wrapper
%%% Created : 21 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epic).

-export([start/0, stop/0]).
-export([noop/0, nop/0]).

-include("epic_int.hrl").

start() ->  application:start(?MODULE).
stop()  ->  application:stop(?MODULE).
    
noop() ->
    epic_drv:call(?EPIC_NOOP, []).

nop() ->
    ok.

