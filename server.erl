-module(server).
-export([loop/2, initial_state/1]).

-include_lib("./defs.hrl").

loop(St, _Msg) -> 
    {ok, St}. 


initial_state(_Server) ->
    #server_st{}.
