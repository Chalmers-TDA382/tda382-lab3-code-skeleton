-module(helper).
-export([start/3, request/2, request/3, requestAsync/2, timeSince/1]).

%% Spawn a process and register it with a given atom
%% Function F should have arity 1
start(Name, State, F) ->
    Pid = spawn(fun() -> apply(F, [State]) end),
    register(Name, Pid),
    Pid.

%% Send a request to a Pid and wait for a response
request(Pid, Data) ->
    request(Pid, Data, 3000).

%% Send a request to a Pid and wait for a response
%% With a specified timeout
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

%% Send a request to a Pid without waiting
requestAsync(Pid, Data) ->
    Ref = make_ref(),
    Pid!{request, self(), Ref, Data}.

%% Returns time since TimeStamp in milliseconds (ms)
timeSince(TimeStamp) ->
    timer:now_diff(now(), TimeStamp) / 1000.
