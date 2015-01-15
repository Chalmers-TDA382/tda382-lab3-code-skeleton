-module(client).
-export([main/1, initial_state/2]).
-include_lib("./defs.hrl").

%% Receive messages from GUI and handle them accordingly
main(State) ->
    receive
        {request, From, Ref, Request} ->
            {Response, NextState} = loop(State, Request),
            From ! {result, Ref, Response},
            main(NextState)
    end.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName }.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, _Server}) ->
    {ok, St} ;

%% Disconnect from server
loop(St, disconnect) ->
     {ok, St} ;

% Join channel
loop(St, {join, _Channel}) ->
    {ok, St} ;

%% Leave channel
loop(St, {leave, _Channel}) ->
     {ok, St} ;

% Sending messages
loop(St, {msg_from_GUI, _Channel, _Msg}) ->
     {ok, St} ;

%% Get current nick
loop(St, whoami) ->
    {"TODO", St} ;

%% Change nick
loop(St, {nick, _Nick}) ->
    {ok, St} ;

%% Incoming message
loop(St = #cl_st { gui = GUIName }, _MsgFromClient) ->
    {incoming_msg, Channel, Name, Msg} = _MsgFromClient,
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
