-module(server).
-export([main/1, initial_state/1]).
-include_lib("./defs.hrl").

main(State) ->
    % TODO: Receive message, handle it, and loop
    not_implemented.

initial_state(ServerName) ->
    #server_st{}.
