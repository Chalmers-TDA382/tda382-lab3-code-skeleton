-module(cchat).

-export([start/0,server/0,gui_interface/0,start2/0]).
-include_lib("./defs.hrl").

server() ->
    Server = "shire",
    catch(unregister(list_to_atom(Server))),
    genserver:start(list_to_atom(Server), server:initial_state(Server), 
                    fun server:loop/2).

gui_interface() ->
    gui:start().
    
start() ->
    %% Starting a local server
    %% Starting the GUI, which starts the client
    server(),
    gui_interface().


start2() ->
    %% Starting a local server
    %% Starting the GUI, which starts the client
    server(),
    spawn(fun gui_interface/0), 
    spawn(fun gui_interface/0). 
