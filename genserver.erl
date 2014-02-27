-module(genserver).
-export([start/3, request/2, request/3, update/2]).
-include_lib("./defs.hrl").

start(Name, State, F) ->
    Pid = spawn(fun() -> loop(State, F) end),
    register(Name, Pid),
    Pid.

loop(State, F) ->
    receive
	{request, From, Ref, Data} ->
	    case catch(F(State, Data)) of
		{'EXIT', Reason} ->
		    From!{exit, Ref, Reason},
		    loop(State, F);
		{R, NewState} ->
		    From!{result, Ref, R},
		    loop(NewState, F)
	    end;
	{update, From, Ref, NewF} ->
	    From ! {ok, Ref},
	    loop(State, NewF);
	stop ->
	    true
    end.

request(Pid, Data) ->
    request(Pid, Data, 3000).

request(Pid, Data, Timeout) ->
    Ref = make_ref(),
    Pid!{request, self(), Ref, Data},
    receive
	{result, Ref, Result} ->
	    Result;
	{exit, Ref, Reason} ->
	    exit(Reason)
    after Timeout ->
	    exit("Timeout")
    end.

update(Pid, Fun) ->
    Ref = make_ref(),
    Pid!{update, self(), Ref, Fun},
    receive
	{ok, Ref} ->
	    ok
    end.
