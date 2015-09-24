-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{}.

%% ---------------------------------------------------------------------------

loop(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {Response, St}.
