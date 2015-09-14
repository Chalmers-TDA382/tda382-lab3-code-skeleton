-module(genserver).
-export([start/3, request/2, request/3, requestAsync/2, update/2, timeSince/1, maybeWait/0]).

%% Spawn a process and register it with a given atom
%% Function F should have arity 1
start(Atom, State, F) ->
  Pid = spawn(fun() -> loop(State, F) end),
  catch(unregister(Atom)),
  register(Atom, Pid),
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

update(Pid, Fun) ->
  Ref = make_ref(),
  Pid!{update, self(), Ref, Fun},
  receive
    {ok, Ref} ->
      ok
  end.

%% Returns time since TimeStamp in milliseconds (ms)
timeSince(TimeStamp) ->
  timer:now_diff(now(), TimeStamp) / 1000.

%% If process sleepy exists, ask her if we should sleep
maybeWait() ->
  case whereis(sleepy) of
    undefined -> ok ;
    Pid ->
      Pid ! {hi, self()},
      receive
        {wait, N} -> timer:sleep(N) ;
        {go} -> ok
      after 100 ->
        ok
      end
  end.
