% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    ServerAtom = list_to_atom(Server),
    catch(unregister(ServerAtom)),
    helper:start(list_to_atom(Server), server:initial_state(Server), fun server:main/1).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().
