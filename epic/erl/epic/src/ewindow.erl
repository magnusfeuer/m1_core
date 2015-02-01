%%% File    : ewindow.erl
%%% Author  : Tony Rogvall <tony@PBook.local>
%%% Description : EPIC window interface
%%% Created : 27 Aug 2006 by Tony Rogvall <tony@PBook.local>

-module(ewindow).

-export([create/4,create/5,destroy/1, attach/1, attach/2, detach/1]).

-include("epic_int.hrl").

create(X, Y, Width, Height) ->
    create(X, Y, Width, Height, []).

create(X, Y, Width, Height, Mask) ->
    M = make_mask(Mask),
    epic_drv:create(?EWINDOW_CREATE, ?EWINDOW_DESTROY,
		    <<X:32, Y:32, Width:32, Height:32, M:16>>).
    
make_mask(Mask) ->
    make_mask(Mask, 0).
    
make_mask([],M) -> M;
make_mask([E | Es], M) ->
    case E of
	key_press      -> make_mask(Es, M bor ?EEVENT_KEY_PRESS);
	key_release    -> make_mask(Es, M bor ?EEVENT_KEY_RELEASE);
	motion         -> make_mask(Es, M bor ?EEVENT_POINTER_MOTION);
	button_press   -> make_mask(Es, M bor ?EEVENT_BUTTON_PRESS);
	button_release -> make_mask(Es, M bor ?EEVENT_BUTTON_RELEASE);
	focus_in       -> make_mask(Es, M bor ?EEVENT_FOCUS_IN);
	focus_out      -> make_mask(Es, M bor ?EEVENT_FOCUS_OUT);
	focus          -> make_mask(Es, M bor ?EEVENT_FOCUS)
    end.


destroy(Win) ->
    epic_drv:destroy(?EWINDOW_DESTROY, Win).

attach(Win) ->
    attach(Win, ebackend:default()).

attach(Win, Backend) ->
    epic_drv:attach(?EWINDOW_ATTACH, Win, Backend).

detach(Win) ->
    epic_drv:detach(?EWINDOW_DETACH, Win).
