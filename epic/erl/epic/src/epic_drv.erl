%%%-------------------------------------------------------------------
%%% File    : epic_drv.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPIC driver interface
%%%
%%% Created : 27 Aug 2006 by Tony Rogvall <tony@PBook.local>
%%%-------------------------------------------------------------------
-module(epic_drv).

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).

-export([create/3, destroy/2, copy/3, attach/3, detach/2, call/2]).
-export([encode/1, decode/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("epic_int.hrl").

-ifdef(debug).
-define(dbg(F,A), io:format((F)++"\n",(A))).
-define(dbg_hard(F,A), ok).
-else.
-define(dbg(F,A), ok).
-define(dbg_hard(F,A), ok).
-endif.


-record(state,
	{
	  port,
	  tab
	 }).
%%
%% Table will contain all resource mappings
%%
%%  -   Monitor -> Handle x CodeDestroy   (used DOWN / EXIT )
%%  -   Handle  -> Mon                    (destroy)
%%


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?EPIC_SERVER}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?EPIC_SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?EPIC_SERVER, stop).

create(Code, CodeDestroy, Args) ->
    case call(Code, Args) of
	{ok,Handle} ->
	    gen_server:cast(?EPIC_SERVER, {monitor,self(),Handle,CodeDestroy}),
	    {ok, Handle};
	Error -> Error
    end.

copy(Code, CodeDestroy, OrigHandle) ->
    case call(Code, <<OrigHandle:32>>) of
	{ok,Handle} ->
	    gen_server:cast(?EPIC_SERVER, {monitor,self(),Handle,CodeDestroy}),
	    {ok, Handle};
	Error -> Error
    end.

destroy(Code, Handle) ->
    case call(Code, <<Handle:32>>) of
	ok ->
	    gen_server:cast(?EPIC_SERVER, {unmonitor,self(),Handle}),
	    ok;
	Error ->
	    Error
    end.

attach(_Code, _Handle, undefined) ->
    {error, einval};
attach(Code, Handle, BeHandle) ->
    case call(Code, <<Handle:32, BeHandle:32>>) of
	ok ->
	    Pid = self(),
	    ets:insert(?EPIC_REG, {Handle,Pid}),
	    ets:insert(?EPIC_REG, {Pid,Handle}),
	    ok;
	Error -> 
	    Error
    end.

detach(Code, Handle) ->
    Pid = self(),
    case ets:lookup(?EPIC_REG, Handle) of
	[{Pid,_}|_] ->
	    ets:delete_object(?EPIC_REG, {Handle,Pid}),
	    ets:delete_object(?EPIC_REG, {Pid,Handle}), 
	    call(Code, <<Handle:32>>);
	[_,{Pid,_}|_] ->
	    ets:delete_object(?EPIC_REG, {Handle,Pid}),
	    ets:delete_object(?EPIC_REG, {Pid,Handle}),
	    call(Code, <<Handle:32>>);
	_ ->
	    ok
    end.


call(Code, Args) ->
    case erlang:port_control(?EPIC_PORT, Code, Args) of
	[?REPLY_OK] ->
	    ok;
	[?REPLY_OK,?UINT32,A,B,C,D] ->
	    {ok, (A bsl 24)+(B bsl 16)+(C bsl 8)+D};
	[?REPLY_OK|Data] ->
	    {value,Result} = decode(list_to_binary(Data)),
	    {ok,Result};
	[?REPLY_ERROR|Data] ->
	    {value,Result} = decode(list_to_binary(Data)),
	    {error, Result}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    case erl_ddll:load_driver(code:priv_dir(epic), "epic") of
	ok ->
	    Port = erlang:open_port({spawn, "epic"}, [binary]),
	    register(?EPIC_PORT, Port),
	    Tab  = ets:new(?EPIC_REG, [named_table, public, bag]),
	    %% open "default" backend (fixme when native is working)
	    case ebackend:list() of
		{ok,[]} ->
		    io:format("Warning: no backends found\n", []);
		{ok,[Default|_]} ->
		    case ebackend:create(Default) of
			{ok,Backend} ->
			    ets:insert(?EPIC_REG, {default_backend,Backend});
			Error ->
			    io:format("Warning: backend error: ~p\n", [Error])
		    end
	    end,
	    {ok, #state{ port = Port, tab  = Tab }};
	Error  ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({monitor,Pid,Handle,CodeDestroy}, State) ->
    if Pid == self() ->  
	    %% this will happend when we create the default backend
	    {noreply, State};
       true ->
	    Mon = start_monitor(Pid),
	    ets:insert(?EPIC_REG, {Mon, Handle, CodeDestroy}),
	    ets:insert(?EPIC_REG, {Handle, Mon}),
	    {noreply, State}
    end;
handle_cast({unmonitor,Pid,Handle}, State) ->
    case ets:lookup(?EPIC_REG, Handle) of
	[{_,Mon}|_] when is_reference(Mon) ->
	    ets:delete(?EPIC_REG, Handle),
	    ets:delete(?EPIC_REG, Mon),
	    ets:delete_object(?EPIC_REG, {Pid,Handle}),
	    stop_monitor(Mon),
	    {noreply,State};
	[_,{_,Mon}|_] when is_reference(Mon) ->
	    ets:delete(?EPIC_REG, Handle),
	    ets:delete(?EPIC_REG, Mon),
	    ets:delete_object(?EPIC_REG, {Pid,Handle}),
	    stop_monitor(Mon),
	    {noreply,State};	    
	[] ->
	    {noreply,State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({Port,{data,Data}}, State) when Port == State#state.port ->
    ?dbg_hard("Port: data=~p", [Data]),
    case Data of
	<<?REPLY_OK, CmdId:32, ReplyData/binary>> ->
	    ?dbg("Got: OK cmdid=~w, data=~p", [CmdId,ReplyData]),
	    State1 = case decode(ReplyData) of
			 false ->
			     epic_reply(CmdId, ok, State);
			 {value,Decoded} ->
			     epic_reply(CmdId, {ok,Decoded}, State)
		     end,
	    {noreply, State1};
	<<?REPLY_ERROR, CmdId:32, ReplyData/binary>> ->
	    ?dbg("Got: ERROR cmdid=~w, data=~p", [CmdId,ReplyData]),
	    State1 = case decode(ReplyData) of
			 false ->
			     epic_reply(CmdId, error, State);
			 {value,Decoded} ->
			     epic_reply(CmdId, {error,Decoded}, State)
		     end,			 
	    {noreply, State1};
	<<?REPLY_EVENT, EventId:32, EventData/binary>> ->
	    ?dbg("Got: EVENT evtid=~w, data=~p", [EventId,EventData]),
	    case ets:lookup(?EPIC_REG, EventId) of
		[{_,Pid}|_] when is_pid(Pid) ->
		    epic_event(Pid, EventData, State);
		[_, {_,Pid}|_] when is_pid(Pid) ->
		    epic_event(Pid, EventData, State);
		_ ->
		    ?dbg("no receipient for evtid=~p data=~p", 
			 [EventId, decode(EventData)]),
		    {noreply, State}
	    end;
	_ ->
	    ?dbg("got bad info data ~p\n", [Data]),
	    {noreply, State}
    end;
handle_info({Port,eof}, State) when Port == State#state.port ->
    ?dbg("epic_drv closed",[]),
    erlang:port_close(Port),
    {stop, closed, State};
handle_info({'DOWN',Mon,process,Pid,_Reason}, State) ->
    case ets:lookup(?EPIC_REG, Mon) of
	[{_,Handle,CodeDestroy}] ->
	    call(CodeDestroy, <<Handle:32>>),
	    ets:delete(?EPIC_REG, Mon),
	    ets:delete(?EPIC_REG, Handle),
	    ets:delete_object(?EPIC_REG, {Pid,Handle}),
	    {noreply, State};
	[] ->
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

epic_event(Pid, EventData, State) ->	    
    case decode(EventData) of
	false ->
	    ?dbg("bad event data ~p", [EventData]),
	    {noreply, State};
	{value, _E={eevent,Win,destroyed}} -> 
	    %% cleanup window structure
	    ?dbg("event ~p", [_E]),
	    case ets:lookup(?EPIC_REG, Win) of
		[{_,Mon}|_] when is_reference(Mon) ->
		    ets:delete(?EPIC_REG, Win),
		    ets:delete(?EPIC_REG, Mon),
		    stop_monitor(Mon),
		    {noreply,State};
		[_,{_,Mon}|_] when is_reference(Mon) ->
		    ets:delete(?EPIC_REG, Win),
		    ets:delete(?EPIC_REG, Mon),
		    stop_monitor(Mon),
		    {noreply,State};
		[] ->
		    {noreply,State}
	    end;
	{value, E} ->
	    ?dbg("event ~p", [E]),
	    Pid ! E,
	    {noreply, State}
    end.

epic_reply(_CmdID, _Reply, State) ->
    ?dbg("got reply: ~w  ~p", [_CmdID, _Reply]),
    {noreply, State}.

start_monitor(Pid) ->
    erlang:monitor(process, Pid).

%% stop and flush
stop_monitor(undefined) -> 
    ok;
stop_monitor(Ref) ->
    erlang:demonitor(Ref),
    receive
	{'DOWN',Ref,_,_Pid,_Reason} ->
	    ok
    after 0 ->
	    ok
    end.

%% Encode/Decode reply data from driver
%% encode into Data format
encode(Term) ->
    list_to_binary([enc(Term)]).

enc(true) ->   
    <<?BOOLEAN, 1>>;
enc(false) ->  
    <<?BOOLEAN, 0>>;
enc(X) when atom(X) ->
    Val = atom_to_list(X),
    [<<?ATOM, (length(Val)):8>>, Val];
enc(X) when integer(X) ->
    if X =< 16#ffffffff -> <<?UINT32, X:32>> ;
       X >= -16#8000000 -> <<?INT32, X:32>> ;
       X > 0 -> <<?UINT64, X:64>>;
       true -> (<<?INT64, X:64>>)
    end;
enc(X) when float(X) -> 
    <<?FLOAT64, X:64/float>>;
enc(X) when is_list(X) ->
    case is_string(X) of
        true ->
            Len = length(X),
            if Len =< 255 ->
                    [<<?STRING1, Len:8>>, X];
               true ->
                    [<<?STRING4, Len:32>>, X]
            end;
        false ->
            [?LIST, lists:map(fun(E) -> enc(E) end, X), ?LIST_END]
    end;
enc(X) when is_tuple(X) ->
    [?TUPLE, lists:map(fun(E) -> enc(E) end, tuple_to_list(X)), ?TUPLE_END];
enc(X) when is_binary(X) ->
    [<<?BINARY,(size(X)):32>>, X].
     
is_string([X|Xs]) when X >= 0, X =< 255 -> is_string(Xs);
is_string([]) -> true;
is_string(_) -> false.


%% decode reply data
decode(<<>>) -> false;
decode(Data) -> {value, decode(Data, [])}.
    
decode(<<>>, [Hd]) -> Hd;
decode(Data, Stack) ->
    case Data of
        <<?LIST, Rest/binary>> -> 
            ?dbg_hard("LIST",[]),
            decode(Rest, [list|Stack]);
        <<?TUPLE, Rest/binary>> ->
            ?dbg_hard("TUPLE",[]),
            decode(Rest, [tuple|Stack]);
        <<?BOOLEAN, B, Rest/binary>> -> 
            ?dbg_hard("BOOLEAN:~w",[B]),
            decode(Rest, [B =/= 0 | Stack]);
        <<?UINT8, I:8, Rest/binary>> -> 
            ?dbg_hard("UINT8:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?UINT16, I:16, Rest/binary>> ->
            ?dbg_hard("UINT16:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?UINT32, I:32, Rest/binary>> ->
            ?dbg_hard("UINT32:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?UINT64, I:64, Rest/binary>> ->
            ?dbg_hard("UINT64:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?INT8, I:8/signed, Rest/binary>> ->
            ?dbg_hard("INT8:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?INT16, I:16/signed, Rest/binary>> ->
            ?dbg_hard("INT16:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?INT32, I:32/signed, Rest/binary>> -> 
            ?dbg_hard("INT32:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?INT64, I:64/signed, Rest/binary>> ->
            ?dbg_hard("INT64:~w",[I]),
            decode(Rest, [I|Stack]);
        <<?FLOAT32, F:32/float, Rest/binary>> ->
            ?dbg_hard("FLOAT32:~w",[F]),
            decode(Rest, [F|Stack]);
        <<?FLOAT64, F:64/float, Rest/binary>> -> 
            ?dbg_hard("FLOAT64:~w",[F]),
            decode(Rest, [F|Stack]);
        <<?STRING1, Len:8, String:Len/binary, Rest/binary>> -> 
            ?dbg_hard("STRING1: len=~w, ~w",[Len,String]),
            decode(Rest, [binary_to_list(String)  | Stack]);
        <<?STRING4, Len:32, String:Len/binary, Rest/binary>> ->
            ?dbg_hard("STRING4: len=~w, ~w",[Len,String]),
            decode(Rest, [binary_to_list(String)  | Stack]);
        <<?BINARY, Len:32, Bin:Len/binary, Rest/binary>> -> 
            ?dbg_hard("BINARY: len=~w, ~w",[Len,Bin]),
            decode(Rest, [Bin | Stack]);
       <<?ATOM, Len:8, Atom:Len/binary, Rest/binary>> -> 
            ?dbg_hard("ATOM: len=~w, ~w",[Len,Atom]),
            decode(Rest, [list_to_atom(binary_to_list(Atom)) | Stack]);
        <<?LIST_END, Rest/binary>> ->
            ?dbg_hard("LIST_END",[]),
            {L,[_|Stack1]} = lists:splitwith(fun(X) -> X =/= list end, Stack),
            decode(Rest, [lists:reverse(L) | Stack1]);
        <<?TUPLE_END, Rest/binary>> ->
            ?dbg_hard("TUPLE_END",[]),
            {L,[_|Stack1]}=lists:splitwith(fun(X) -> X =/= tuple end, Stack),
            decode(Rest, [list_to_tuple(lists:reverse(L)) | Stack1])
    end.
