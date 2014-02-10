-module(test_client).
-include_lib("./defs.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-define(SERVER,"server").
-define(SERVERATOM, list_to_atom(?SERVER)).
-define(MAX, 100000).

% --- Helpers ----------------------------------------------------------------

% It is assumed that is called at the beginning of each test case (only)
% init() ->
%     init("Test").
init(Name) ->
    ?assert(compile:file(server) =:= {ok,server}),
    putStrLn(blue("\n# Test: "++Name)),
    catch(unregister(?SERVERATOM)),
    InitState = server:initial_state(?SERVER),
    Result = genserver:start(?SERVERATOM, InitState, fun server:server_loop/2),
    assert("server startup", is_pid(Result)).

% Start new GUI and register it as Name
new_gui(Name) ->
    {ok, Pid} = dummy_gui:start_link(Name,self()),
    Pid.

find_unique_name(Prefix) ->
    MStr = integer_to_list(random:uniform(?MAX)),
    Name = Prefix++MStr,
    case whereis(list_to_atom(Name)) of
        undefined -> Name ;
        _         -> find_unique_name(Prefix)
    end.

% Start a new client
new_client() ->
    Nick = find_unique_name("user_"),
    new_client(Nick).

% Start a new client with a given nick
new_client(Nick) ->
    GUIName = find_unique_name("gui_"),
    new_client(Nick, GUIName).

% Start a new client with a given nick and GUI name
new_client(Nick, GUIName) ->
    ClientName = find_unique_name("client_"),
    ClientAtom = list_to_atom(ClientName),
    Pid = genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:loop/2),
    {Pid, Nick, ClientAtom}.

% Start a new client and connect to server
new_client_connect() ->
    {Pid, Nick, ClientAtom} = new_client(),
    Result = genserver:request(ClientAtom, {connect, ?SERVER}),
    assert(atom_to_list(ClientAtom)++" connects to server as "++Nick, Result =:= ok),
    {Pid, Nick, ClientAtom}.

% Start a new client and connect to server
new_client_connect(GUI) ->
    case GUI of
        true ->
            Nick = find_unique_name("user_"),
            GUIName = find_unique_name("gui_"),
            new_gui(GUIName),
            {Pid, Nick, ClientAtom} = new_client(Nick, GUIName),
            Result = genserver:request(ClientAtom,{connect, ?SERVER}),
            assert(atom_to_list(ClientAtom)++" connects to server as "++Nick, Result =:= ok),
            {Pid, Nick, ClientAtom}
            ;
        _ -> new_client_connect()
    end.

% Connect and assert it succeeded
connect(ClientAtom) ->
    Result = genserver:request(ClientAtom,{connect, ?SERVER}),
    assert(atom_to_list(ClientAtom)++" connects to server", Result =:= ok).

% Join a channel and assert it succeeded
join_channel(ClientAtom, Channel) ->
    Result = genserver:request(ClientAtom,{join,Channel}),
    assert(atom_to_list(ClientAtom)++" joins "++Channel, Result =:= ok).

% Leave a channel and assert it succeeded
leave_channel(ClientAtom, Channel) ->
    Result = genserver:request(ClientAtom,{leave,Channel}),
    assert(atom_to_list(ClientAtom)++" leaves "++Channel, Result =:= ok).

% Disconnect and assert it succeeded
disconnect(ClientAtom) ->
    Result = genserver:request(ClientAtom,disconnect),
    assert(atom_to_list(ClientAtom)++" disconnects from server", Result =:= ok).

% Send a message and assert it succeeded
send_message(ClientAtom, Channel, Message) ->
    Result = genserver:request(ClientAtom, {msg_from_GUI,Channel,Message}),
    assert(atom_to_list(ClientAtom)++" sends message on "++Channel, Result =:= ok).

% Change nick and assert it succeeded
change_nick(ClientAtom, Nick) ->
    Result = genserver:request(ClientAtom, {nick,Nick}),
    assert(atom_to_list(ClientAtom)++" changes nick to "++Nick, Result =:= ok).

% Receive a message from dummy GUI
receive_message(Channel, Nick, Message) ->
    receive
        {msg_to_GUI, From, Msg} ->
            assert("channel matches", From =:= Channel),
            assert("message matches", Msg =:= Nick++"> "++Message)
    after
        500 ->
            putStrLn(red("nothing received")),
            ?assert(false)
    end.

% Make sure thare are no pending messages from dummy GUI
no_more_messages() ->
    receive
        {msg_to_GUI, _From, _Msg} ->
            putStrLn(red("there are unreceived messages")),
            ?assert(false)
    after
        500 ->
            assert("no more messages", true)
    end.

% Get a new channel name
new_channel() ->
    find_unique_name("#channel_").

% Positive assertion, with error message
assert(Message, Condition) ->
    Pfx = Message++": ",
    case (catch(?assert(Condition))) of
        {'EXIT', Ex} -> putStrLn(Pfx++red("Fail")), throw(Ex) ;
        _            -> putStrLn(Pfx++green("Ok"))
    end.

% Assert for particular error message
assert_error(Result, Atom) ->
    ?assert((element(1,Result) =:= error) and (element(2,Result) =:= Atom)).
assert_error(Message, Result, Atom) ->
    Pfx = Message++" fails: ",
    case (catch(assert_error(Result, Atom))) of
        {'EXIT', Ex} -> putStrLn(Pfx++red("Passes")), throw(Ex) ;
        _            -> putStrLn(Pfx++green("Ok"))
    end.

% --- Output -----------------------------------------------------------------

% dump(S) ->
%     ?debugFmt("~p",[S]).

% Turn output off/on
output_off() ->
    F = fun () -> receive _ -> ok end end,
    catch(register(output_off, spawn(F))).
output_on() ->
    case whereis(output_off) of
        undefined -> ok ;
        Pid -> Pid ! die ,
               catch(unregister(output_off)),
               ok
    end.

putStrLn(S) ->
    case whereis(output_off) of
        undefined -> io:fwrite(user, <<"~s~n">>, [S]) ;
        _ -> ok
    end.

putStrLn(S1, S2) ->
    putStrLn(io_lib:format(S1++"~n", S2)).

colour(Num,S) ->
    "\033["++Num++"m"++S++"\033[0m".
red(S) ->
    colour("31",S).
green(S) ->
    colour("32",S).
blue(S) ->
    colour("34",S).
gray(S) ->
    colour("37",S).
purple(S) ->
    colour("35",S).

% --- Good unit tests --------------------------------------------------------

% A normal sequence of events
normal_one_user_test() ->
    init("normal_one_user"),
    Channel = new_channel(),
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),
    send_message(ClientAtom1, Channel, "hello"),
    send_message(ClientAtom1, Channel, "goodbye"),
    leave_channel(ClientAtom1, Channel),
    disconnect(ClientAtom1).

% One user writes, the other receives
write_receive_test() ->
    init("write_receive"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2 with dummy GUI
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(true),
    join_channel(ClientAtom2, Channel),

    % Client 1 writes to to channel
    Message = find_unique_name("message_"),
    send_message(ClientAtom1, Channel, Message),

    % Client 2 receives
    receive_message(Channel, Nick1, Message),

    % Client 2 leaves, 1 resends, no receive
    leave_channel(ClientAtom2, Channel),
    send_message(ClientAtom1, Channel, Message),
    no_more_messages(),

    ok.

% Write/receive with multiple channels
%
%  User1    User2    User3
%   | \     / | \     / |
%   |  Chan1  |  Chan2  |
%    \        |        /
%     `---- Chan3 ----`
write_receive_2_test() ->
    init("write_receive_2"),
    Channel1 = new_channel(),
    Channel2 = new_channel(),
    Channel3 = new_channel(),

    % Client 1 -> Channel 1, 3
    {_Pid1, Nick1, ClientAtom1} = new_client_connect(true),
    join_channel(ClientAtom1, Channel1),
    join_channel(ClientAtom1, Channel3),

    % Client 2 -> Channel 1, 2, 3
    {_Pid2, Nick2, ClientAtom2} = new_client_connect(true),
    join_channel(ClientAtom2, Channel1),
    join_channel(ClientAtom2, Channel2),
    join_channel(ClientAtom2, Channel3),

    % Client 3 -> Channel 2, 3
    {_Pid3, Nick3, ClientAtom3} = new_client_connect(true),
    join_channel(ClientAtom3, Channel2),
    join_channel(ClientAtom3, Channel3),

    % Client 1 writes to channel 1
    % Receive from Client 2
    Message1 = find_unique_name("message_"),
    send_message(ClientAtom1, Channel1, Message1),
    receive_message(Channel1, Nick1, Message1),

    % Client 3 writes to channel 2
    % Receive from Client 2
    Message2 = find_unique_name("message_"),
    send_message(ClientAtom3, Channel2, Message2),
    receive_message(Channel2, Nick3, Message2),

    % Client 2 writes to channel 3
    % Receive from Client 1 and 2
    Message3 = find_unique_name("message_"),
    send_message(ClientAtom2, Channel3, Message3),
    receive_message(Channel3, Nick2, Message3),
    receive_message(Channel3, Nick2, Message3),

    no_more_messages(),
    ok.

% Changing nick
% Moved to Lab 4
% change_nick_test() ->
%     init("change_nick"),
%     Channel = new_channel(),

%     % Client 1
%     {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
%     join_channel(ClientAtom1, Channel),

%     % Client 2
%     {_Pid2, _Nick2, ClientAtom2} = new_client_connect(true),
%     join_channel(ClientAtom2, Channel),

%     % Change nick of 1
%     NewNick = find_unique_name("user_"),
%     change_nick(ClientAtom1, NewNick),

%     % Client 1 writes to channel
%     % Make sure prompt in 2 reflects correct name
%     Message = find_unique_name("message_"),
%     send_message(ClientAtom1, Channel, Message),
%     receive_message(Channel, NewNick, Message),

%     % no_more_messages(),
%     ok.

% --- Bad unit tests ---------------------------------------------------------

% Connecting to incorrect server
connect_wrong_server_test() ->
    init("connect_wrong_server"),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = genserver:request(ClientAtom, {connect, "mordor"}),
    assert_error("connecting to server mordor", Result, server_not_reached).

% Logging in with a name that is taken
connect_registered_nick_test() ->
    init("connect_registered_nick"),

    % Client 1
    {_Pid1, Nick1, _ClientAtom1} = new_client_connect(),

    % Client 2, set nick to client1's
    {_Pid2, _Nick2, ClientAtom2} = new_client(Nick1),
    Result = genserver:request(ClientAtom2, {connect, ?SERVER}),
    assert_error(atom_to_list(ClientAtom2)++" connecting as "++_Nick2, Result, user_already_connected).

% Disconnect when not connected
disconnect_not_connected_test() ->
    init("disconnect_not_connected"),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = genserver:request(ClientAtom, disconnect),
    assert_error("disconnecting when not connected", Result, user_not_connected).

% Disconnect when still in channels
disconnect_leave_channels_first_test() ->
    init("disconnect_leave_channels_first"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client_connect(),
    join_channel(ClientAtom, Channel),

    Result2 = genserver:request(ClientAtom, disconnect),
    assert_error(atom_to_list(ClientAtom)++" disconnects without leaving "++Channel, Result2, leave_channels_first).

% Joining already joined
join_already_joined_test() ->
    init("join_already_joined"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client_connect(),
    join_channel(ClientAtom, Channel),

    Result2 = genserver:request(ClientAtom,{join,Channel}),
    assert_error(atom_to_list(ClientAtom)++" joins "++Channel, Result2, user_already_joined).

% Writing when not joined
write_not_joined_test() ->
    init("write_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(),
    Result = genserver:request(ClientAtom2,{msg_from_GUI,Channel,"hi"}),
    assert_error(atom_to_list(ClientAtom2)++" writing to "++Channel, Result, user_not_joined).

% Leaving when not joined
leave_not_joined_test() ->
    init("leave_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(),
    Result2 = genserver:request(ClientAtom2,{leave,Channel}),
    assert_error(atom_to_list(ClientAtom2)++" leaving "++Channel, Result2, user_not_joined).

% Trying to take a nick which is taken
% Moved to Lab 4
% nick_taken_test() ->
%     init("nick_taken"),
%     Channel = new_channel(),

%     % Client 1
%     {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
%     join_channel(ClientAtom1, Channel),

%     % Client 2
%     {_Pid2, Nick2, ClientAtom2} = new_client_connect(),
%     join_channel(ClientAtom2, Channel),

%     % Change nick of 1 to 2
%     Result = genserver:request(ClientAtom1,{nick,Nick2}),
%     assert_error(atom_to_list(ClientAtom1)++" changing nick to "++Nick2, Result, nick_taken).

% --- Performance unit tests -------------------------------------------------

% many_users_one_channel_test_() ->
%     {timeout, 60, [{test_client,many_users_one_channel}]}.

many_users_one_channel() ->
    init("many_users_one_channel"),
    Channel = new_channel(),
    ParentPid = self(),
    X = 500, % number of users
    F = fun (I) ->
                fun () ->
                        Is = lists:flatten(io_lib:format("~p", [I])),
                        % {Pid, Nick, ClientAtom} = new_client("user_"++I),
                        Nick = "user_perf1_"++Is,
                        ClientName = "client_perf1_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf1_"++Is,
                        new_gui(GUIName),
                        genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:loop/2),
                        connect(ClientAtom),
                        join_channel(ClientAtom, Channel),
                        send_message(ClientAtom, Channel, "message_"++Is++"_1"),
                        send_message(ClientAtom, Channel, "message_"++Is++"_2"),
                        leave_channel(ClientAtom, Channel),
                        disconnect(ClientAtom),
                        ParentPid ! ready,
                        ok
                end
        end,
    Seq = lists:seq(1, X),
    Spawn = fun (I) -> spawn_link(F(I)) end,
    Recv  = fun (_) -> receive ready -> ok end end,
    putStrLn("spawning ~p clients, each connecting to 1 channel...", [X]),
    output_off(),
    lists:map(Spawn, Seq),
    {Time, _Value} = timer:tc(fun () -> lists:map(Recv, Seq) end),
    output_on(),
    putStrLn(red("time elapsed: ~p ms"), [Time/1000]),
    ok.

% many_users_many_channels_test_() ->
%     {timeout, 60, [{test_client,many_users_many_channels}]}.

many_users_many_channels() ->
    init("many_users_many_channels"),
    ParentPid = self(),
    Chans = 200, % channels
    Users = 300, % users (join all channels!)
    ChansSeq = lists:seq(1, Chans),
    UsersSeq = lists:seq(1, Users),
    F = fun (I) ->
                fun () ->
                        Is = lists:flatten(io_lib:format("~p", [I])),
                        Nick = "user_perf2_"++Is,
                        ClientName = "client_perf2_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf2_"++Is,
                        new_gui(GUIName),
                        genserver:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:loop/2),
                        connect(ClientAtom),
                        G = fun(Ch_Ix) ->
                                    Ch_Ixs = lists:flatten(io_lib:format("~p", [Ch_Ix])),
                                    Channel = "#channel_"++Ch_Ixs,
                                    join_channel(ClientAtom, Channel),
                                    send_message(ClientAtom, Channel, "message_"++Is++"_1"),
                                    send_message(ClientAtom, Channel, "message_"++Is++"_2"),
                                    leave_channel(ClientAtom, Channel),
                                    ok
                            end,
                        lists:foreach(G, ChansSeq),
                        disconnect(ClientAtom),
                        ParentPid ! ready,
                        ok
                end
        end,
    Spawn = fun (I) -> spawn_link(F(I)) end,
    Recv  = fun (_) -> receive ready -> ok end end,
    putStrLn("spawning ~p clients, each connecting to ~p channels...", [Users, Chans]),
    output_off(),
    lists:map(Spawn, UsersSeq),
    {Time, _Value} = timer:tc(fun () -> lists:map(Recv, UsersSeq) end),
    output_on(),
    putStrLn(red("time elapsed: ~p ms"), [Time/1000]),
    ok.
