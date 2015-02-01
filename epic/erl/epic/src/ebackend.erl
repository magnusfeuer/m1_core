%%% File    : ebackend.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPIC window interface
%%% Created : 27 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(ebackend).

-export([create/0, create/1, create/2, destroy/1, list/0, default/0]).

-include("epic_int.hrl").

create() ->
    create("").

create(Name) ->
    create(Name,[]).

create(Name,KeyVals) ->
    Dict = if KeyVals == [] -> 
		   0;
	      true ->
		   {ok,D} = edict:create(),
		   lists:foreach(fun({K,V}) -> edict:set(D,K,V) end, KeyVals),
		   D
	   end,
    Res = epic_drv:create(?EBACKEND_CREATE, ?EBACKEND_DESTROY,
			  <<Dict:32,(list_to_binary(Name))/binary>>),
    edict:destroy(Dict),
    Res.

destroy(Backend) ->
    epic_drv:destroy(?EBACKEND_DESTROY, Backend).

list() ->
    epic_drv:call(?EBACKEND_LIST, <<>>).

default() ->
    case ets:lookup(?EPIC_REG, default_backend) of
	[{_,Be}] -> Be;
	[] -> undefined
    end.
