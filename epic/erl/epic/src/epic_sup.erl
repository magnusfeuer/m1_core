%%% File    : epic_sup.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : 
%%% Created : 28 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(epic_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    EPicDrv = {epic_drv, {epic_drv, start_link, []},
	     permanent, 5000, worker, [epic_drv]},
    {ok,{{one_for_all,0,300}, [EPicDrv]}}.

