%%% File    : edict.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPic Dict
%%% Created : 24 Oct 2006 by Tony Rogvall <tony@PBook.local>

-module(edict).

-include("epic_int.hrl").

-export([create/0, destroy/1, copy/1]).
-export([set/3, unset/2, get/2]).
       
create() ->
    epic_drv:create(?EDICT_CREATE, ?EDICT_DESTROY, <<>>).

destroy(Dict) ->
    epic_drv:destroy(?EDICT_DESTROY, Dict).

copy(Dict) ->
    epic_drv:copy(?EDICT_COPY, ?EDICT_DESTROY, Dict).


set(Dict, Key, Value) ->
    K = epic_drv:encode(Key),
    V = epic_drv:encode(Value),
    epic_drv:call(?EDICT_SET, <<Dict:32, K/binary, V/binary>>).

unset(Dict, Key) ->
    K = epic_drv:encode(Key),    
    epic_drv:call(?EDICT_UNSET, <<Dict:32, K/binary>>).


get(Dict, Key) ->
    K = epic_drv:encode(Key),
    epic_drv:call(?EDICT_GET, <<Dict:32, K/binary>>).

    


