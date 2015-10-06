-module(test_remote).
-include_lib("./defs.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-define(SERVER,"server").
-define(SERVERATOM, list_to_atom(?SERVER)).
-define(SERVERNODENAME, 'srv').
-define(HOST, '127.0.0.1').
-define(SERVERNODE, 'srv@127.0.0.1').

% --- Re-use helpers from test_client ----------------------------------------

putStrLn(S) -> test_client:putStrLn(S).
putStrLn(S1, S2) -> test_client:putStrLn(S1, S2).
red(S) -> test_client:red(S).
green(S) -> test_client:green(S).
blue(S) -> test_client:blue(S).
gray(S) -> test_client:gray(S).
purple(S) -> test_client:purple(S).

assert(Message, Condition) -> test_client:assert(Message, Condition).
assert_ok(Message, X) -> test_client:assert_ok(Message, X).
assert_error(Result, Atom) -> test_client:assert_error(Result, Atom).
assert_error(Message, Result, Atom) -> test_client:assert_error(Message, Result, Atom).

% It is assumed that is called at the beginning of each test case (only)
init(Name) ->
    ?assert(compile:file(server) =:= {ok,server}),
    putStrLn(blue("\n# Test: "++Name)),
    InitState = server:initial_state(?SERVER),
    Result1 = slave:start(?HOST, ?SERVERNODENAME, "-setcookie dchat"),
    assert_ok("start server node "++atom_to_list(?SERVERNODE), element(1,Result1)),
    Result2 = remote_start(?SERVERNODE, [?SERVERATOM, InitState, fun server:loop/2]),
    assert("server startup", is_pid(Result2)).

% Start a new client
new_client() ->
    Nick = test_client:find_unique_name("user_"),
    new_client(Nick).

% Start a new client with a given nick
new_client(Nick) ->
    GUIName = test_client:find_unique_name("gui_"),
    new_client(Nick, GUIName).

% Start a new client with a given nick and GUI name
new_client(Nick, GUIName) ->
    ClientName = test_client:find_unique_name("client_"),
    ClientAtom = list_to_atom(ClientName),
    Result = slave:start(?HOST, ClientAtom, "-setcookie dchat"),
    assert_ok("start client node "++ClientName++"@"++atom_to_list(?HOST), element(1,Result)),
    ClientNode = element(2,Result),

    InitState = client:initial_state(Nick, GUIName),
    Result2 = remote_start(ClientNode, [ClientAtom, InitState, fun client:loop/2]),
    assert("client startup "++ClientName, is_pid(Result2)),

    {Nick, ClientAtom, ClientNode}.

% Start a new client and connect to server
new_client_connect() ->
    {Nick, ClientAtom, ClientNode} = new_client(),
    Result = test_client:request({ClientAtom, ClientNode}, {connect, {?SERVER, atom_to_list(?SERVERNODE)}}),
    assert_ok(atom_to_list(ClientNode)++" connects to server as "++Nick, Result),
    {Nick, ClientAtom, ClientNode}.

% Start a new client and connect to server
new_client_connect(GUI) ->
    case GUI of
        true ->
            Nick = test_client:find_unique_name("user_"),
            GUIName = test_client:find_unique_name("gui_"),
            {Nick, ClientAtom, ClientNode} = new_client(Nick, GUIName),
            Result = test_client:request({ClientAtom, ClientNode}, {connect, {?SERVER, atom_to_list(?SERVERNODE)}}),
            assert_ok(atom_to_list(ClientNode)++" connects to server as "++Nick, Result),
            new_gui(ClientNode, GUIName),
            {Nick, ClientAtom, ClientNode}
            ;
        _ -> new_client_connect()
    end.

% Start genserver server on remote node and wait for it
remote_start(Node, Args) ->
  ParentPid = self(),
  spawn(Node, fun() ->
    Pid = apply(genserver, start, Args),
    ParentPid ! {spawned, Pid}
  end),
  receive
    {spawned, Pid} -> Pid
  end.

% Start new GUI and register it as Name
new_gui(Node, GUIName) ->
  Me = self(),
  spawn(Node, fun() ->
    Pid = apply(dummy_gui, start_link, [GUIName, Me]),
    Me ! {spawned, Pid}
  end),
  receive
    {spawned, Pid} -> Pid
  end.

% --- Distributed tests ------------------------------------------------------

one_channel_test() ->
    init("one_channel"),
    Channel = test_client:new_channel(),

    % Client 1
    {Nick1, ClientAtom1, ClientNode1} = new_client_connect(),
    Client1 = {ClientAtom1, ClientNode1},
    test_client:join_channel(Client1, Channel),

    % Client 2 with dummy GUI
    {_Nick2, ClientAtom2, ClientNode2} = new_client_connect(true),
    Client2 = {ClientAtom2, ClientNode2},
    test_client:join_channel(Client2, Channel),

    % Client 1 writes to to channel
    Message = test_client:find_unique_name("message_"),
    test_client:send_message(Client1, Channel, Message),

    % Client 2 receives
    test_client:receive_message(Channel, Nick1, Message),

    % Client 2 leaves, 1 resends, no receive
    test_client:leave_channel(Client2, Channel),
    test_client:send_message(Client1, Channel, Message),
    test_client:no_more_messages(),

    ok.
