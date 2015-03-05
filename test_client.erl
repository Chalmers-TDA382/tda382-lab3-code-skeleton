-module(test_client).
-include_lib("./defs.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-define(SERVER,"server"). % forces students not to hardcode "shire"
-define(SERVERATOM, list_to_atom(?SERVER)).
-define(MAX, 100000).

-define(PERF_1_USERS, 500).
-define(PERF_2_USERS, 150).
-define(PERF_2_CHANS, 100).
-define(PERF_2_MSGS, 5).

% --- Output -----------------------------------------------------------------

% Turn output off/on
is_output_off() ->
    get(output) == off.
output_off() ->
    put(output,off).
output_on() ->
    put(output,on).

putStrLn(S) ->
    case get(output) of
        off -> ok ;
        _   -> io:fwrite(user, <<"~s~n">>, [S])
    end.

putStrLn(S1, S2) ->
    putStrLn(io_lib:format(S1++"~n", S2)).
sprintf(S1, S2) ->
    lists:flatten(io_lib:format(S1, S2)).

% To turn off colours, just use this function:
% colour(Num,S) -> S.
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

% --- Helpers: assertions ----------------------------------------------------

% Positive assertion, with error message
assert(Message, Condition) ->
    Pfx = Message++": ",
    case (catch(?assert(Condition))) of
        {'EXIT', _Ex} ->
            Msg = Pfx++red("Fail"),
            case get(output) of
                off -> throw(Msg) ;
                _   -> putStrLn(Msg), throw("Test failed")
            end ;
        _ -> putStrLn(Pfx++green("Ok"))
    end.
assert(Message, X, Y) ->
    Pfx = Message++": ",
    case (catch(?assertEqual(Y, X))) of
        {'EXIT', _Ex} ->
            Msg = Pfx++red("Fail")++
                  sprintf("~nExpected: ~p~n     Got: ~p~n", [Y,X]),
            case get(output) of
                off -> throw(Msg) ;
                _   -> putStrLn(Msg), throw("Test failed")
            end ;
        _ -> putStrLn(Pfx++green("Ok"))
    end.
assert_ok(X) ->
    ?assertEqual(ok, X).
assert_ok(Message, X) ->
    assert(Message, X, ok).

% Assert for particular error message
assert_error(Result, Atom) ->
    ?assertMatch({error, Atom, _}, Result).
assert_error(Message, Result, Atom) ->
    Pfx = Message++" fails: ",
    case (catch(assert_error(Result, Atom))) of
        {'EXIT', _Ex} ->
            Msg = Pfx++red("Passes")++
                  sprintf("~nExpected: {error,~p,_}~n     Got: ~p~n", [Atom,Result]),
            case get(output) of
                off -> throw(Msg) ;
                _   -> putStrLn(Msg), throw("Test failed")
            end ;
        _ -> putStrLn(Pfx++green("Ok"))
    end.

% --- Helpers ----------------------------------------------------------------

% Our own version of helper:request without a timeout
request(Pid, Data) ->
    Ref = make_ref(),
    Pid!{request, self(), Ref, Data},
    receive
	{result, Ref, Result} ->
	    Result;
	{exit, Ref, Reason} ->
	    exit(Reason)
    end.

% Generic to_string
to_string({_Atom,Node}) ->
    atom_to_list(Node);
to_string(X) ->
    io_lib:format("~p", [X]).

% It is assumed that is called at the beginning of each test case (only)
init(Name) ->
    ?assert(compile:file(server) =:= {ok,server}),
    putStrLn(blue("\n# Test: "++Name)),
    catch(unregister(?SERVERATOM)),
    InitState = server:initial_state(?SERVER),
    Pid = spawn_link(fun() -> server:main(InitState) end),
    register(?SERVERATOM, Pid),
    assert("server startup", is_pid(Pid)).

% Start new GUI and register it as Name
new_gui(Name) ->
    catch(unregister(list_to_atom(Name))),
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
    Pid = helper:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:main/1),
    {Pid, Nick, ClientAtom}.

% Start a new client and connect to server
new_client_connect() ->
    {Pid, Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom, {connect, ?SERVER}),
    assert_ok(to_string(ClientAtom)++" connects to server as "++Nick, Result),
    {Pid, Nick, ClientAtom}.

% Start a new client and connect to server
new_client_connect(GUI) ->
    case GUI of
        true ->
            Nick = find_unique_name("user_"),
            GUIName = find_unique_name("gui_"),
            new_gui(GUIName),
            {Pid, Nick, ClientAtom} = new_client(Nick, GUIName),
            Result = request(ClientAtom,{connect, ?SERVER}),
            assert_ok(to_string(ClientAtom)++" connects to server as "++Nick, Result),
            {Pid, Nick, ClientAtom}
            ;
        _ -> new_client_connect()
    end.

% Connect and assert it succeeded
connect(ClientAtom) ->
    Result = request(ClientAtom,{connect, ?SERVER}),
    assert_ok(to_string(ClientAtom)++" connects to server", Result).

% Join a channel and assert it succeeded
join_channel(ClientAtom, Channel) ->
    Result = request(ClientAtom,{join,Channel}),
    assert_ok(to_string(ClientAtom)++" joins "++Channel, Result).

% Leave a channel and assert it succeeded
leave_channel(ClientAtom, Channel) ->
    Result = request(ClientAtom,{leave,Channel}),
    assert_ok(to_string(ClientAtom)++" leaves "++Channel, Result).

% Disconnect and assert it succeeded
disconnect(ClientAtom) ->
    Result = request(ClientAtom,disconnect),
    assert_ok(to_string(ClientAtom)++" disconnects from server", Result).

% Send ping
send_ping(ClientAtom, Nick) ->
    Result = request(ClientAtom, {ping,Nick}),
    assert_ok(to_string(ClientAtom)++" pings "++Nick, Result).

% Send a message from GUI to client and assert it succeeded
send_message(ClientAtom, Channel, Message) ->
    Result = request(ClientAtom, {msg_from_GUI,Channel,Message}),
    assert_ok(to_string(ClientAtom)++" sends message on "++Channel, Result).

% Receive a message from dummy GUI
receive_message(Channel, Nick, Message) ->
    receive
        {msg_to_GUI, From, Msg} ->
            assert("channel matches", From, Channel),
            assert("message matches", Msg, Nick++"> "++Message)
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
change_nick_test_DISABLED() ->
    init("change_nick"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(true),
    join_channel(ClientAtom2, Channel),

    % Change nick of 1
    NewNick = find_unique_name("user_"),
    Result = request(ClientAtom1, {nick,NewNick}),
    assert_ok(to_string(ClientAtom1)++" changes nick to "++NewNick, Result),

    % Client 1 writes to channel
    % Make sure prompt in 2 reflects correct name
    Message = find_unique_name("message_"),
    send_message(ClientAtom1, Channel, Message),
    receive_message(Channel, NewNick, Message),

    ok.

% Combined test for changing nick
change_nick_combined_test() ->
    init("change_nick_combined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, Nick2, ClientAtom2} = new_client_connect(true),
    join_channel(ClientAtom2, Channel),

    % Change nick of 1 to something unique
    NewNick = find_unique_name("user_"),
    Result = request(ClientAtom1, {nick,NewNick}),
    case Result of
        % Client supports online nick change
        ok ->
            % Client 1 writes to channel
            % Make sure prompt in 2 reflects correct name
            Message = find_unique_name("message_"),
            send_message(ClientAtom1, Channel, Message),
            receive_message(Channel, NewNick, Message),

            % Change nick of 1 to 2
            Result2 = request(ClientAtom1,{nick,Nick2}),
            assert_error(to_string(ClientAtom1)++" changing nick to "++Nick2, Result2, nick_taken) ;

        % Client doesn't support online nick change
        {error, user_already_connected, _} -> ok
    end.

% Ping test (not run automatically)
ping() ->
    init("ping"),

    % Client 1
    {_Pid1, _Nick1, _ClientAtom1} = new_client_connect(true),

    % Client 2
    {_Pid2, _Nick2, _ClientAtom2} = new_client_connect(),

    % Send ping from 1 to 2
    send_ping(_ClientAtom1, _Nick2),

    % Make sure pong is received
    % Don't check message format, since students may change it
    receive
        {msg_to_SYSTEM, _Msg} ->
            assert_ok(to_string(_ClientAtom1)++" receives pong",ok),
            putStrLn(green(_Msg))
    after
        1000 ->
            putStrLn(red("nothing received after 1000ms")),
            ?assert(false)
    end.

% --- Bad unit tests ---------------------------------------------------------

% Connecting to incorrect server
connect_wrong_server_test() ->
    init("connect_wrong_server"),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom, {connect, "mordor"}),
    assert_error("connecting to server mordor", Result, server_not_reached).

% Logging in with a name that is taken
connect_registered_nick_test() ->
    init("connect_registered_nick"),

    % Client 1
    {_Pid1, Nick1, _ClientAtom1} = new_client_connect(),

    % Client 2, set nick to client1's
    {_Pid2, _Nick2, ClientAtom2} = new_client(Nick1),
    Result = request(ClientAtom2, {connect, ?SERVER}),
    assert_error(to_string(ClientAtom2)++" connecting as "++_Nick2, Result, user_already_connected).

% Disconnect when not connected
disconnect_not_connected_test() ->
    init("disconnect_not_connected"),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom, disconnect),
    assert_error("disconnecting when not connected", Result, user_not_connected).

% Disconnect when still in channels
disconnect_leave_channels_first_test() ->
    init("disconnect_leave_channels_first"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client_connect(),
    join_channel(ClientAtom, Channel),

    Result2 = request(ClientAtom, disconnect),
    assert_error(to_string(ClientAtom)++" disconnects without leaving "++Channel, Result2, leave_channels_first).

% Joining already joined
join_already_joined_test() ->
    init("join_already_joined"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client_connect(),
    join_channel(ClientAtom, Channel),

    Result2 = request(ClientAtom,{join,Channel}),
    assert_error(to_string(ClientAtom)++" joins "++Channel, Result2, user_already_joined).

% Writing when not joined
write_not_joined_test() ->
    init("write_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(),
    Result = request(ClientAtom2,{msg_from_GUI,Channel,"hi"}),
    assert_error(to_string(ClientAtom2)++" writing to "++Channel, Result, user_not_joined).

% Leaving when not joined
leave_not_joined_test() ->
    init("leave_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_connect(),
    Result2 = request(ClientAtom2,{leave,Channel}),
    assert_error(to_string(ClientAtom2)++" leaving "++Channel, Result2, user_not_joined).

% Trying to take a nick which is taken
nick_taken_test_DISABLED() ->
    init("nick_taken"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client_connect(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, Nick2, ClientAtom2} = new_client_connect(),
    join_channel(ClientAtom2, Channel),

    % Change nick of 1 to 2
    Result = request(ClientAtom1,{nick,Nick2}),
    assert_error(to_string(ClientAtom1)++" changing nick to "++Nick2, Result, nick_taken).

% --- Performance unit tests -------------------------------------------------

% many_users_one_channel_test_() ->
%     {timeout, 60, [{test_client,many_users_one_channel}]}.

% Tests that broadcasting is concurrent
many_users_one_channel() ->
    init("many_users_one_channel"),
    Channel = new_channel(),
    ParentPid = self(),
    F = fun (I) ->
                fun () ->
                    try
                        output_off(),
                        Is = lists:flatten(io_lib:format("~p", [I])),
                        % {Pid, Nick, ClientAtom} = new_client("user_"++I),
                        Nick = "user_perf1_"++Is,
                        ClientName = "client_perf1_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf1_"++Is,
                        new_gui(GUIName),
                        helper:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:main/1),
                        T1 = now(),
                        connect(ClientAtom),
                        join_channel(ClientAtom, Channel),
                        send_message(ClientAtom, Channel, "message_"++Is++"_1"),
                        send_message(ClientAtom, Channel, "message_"++Is++"_2"),
                        leave_channel(ClientAtom, Channel),
                        disconnect(ClientAtom),
                        T2 = now(),
                        ParentPid ! {ready, timer:now_diff(T2, T1)}
                    catch Ex ->
                        ParentPid ! {failed, Ex}
                    end
                end
        end,
    Seq = lists:seq(1, ?PERF_1_USERS),
    Spawn = fun (I) -> spawn(F(I)) end,
    Recv  = fun (_) ->
                    receive
                        {ready, Time} -> Time ;
                        {failed, Ex} -> putStrLn(Ex),
                                        throw("")
                    end
            end,
    putStrLn("spawning ~p clients, each connecting to 1 channel...", [?PERF_1_USERS]),
    spawn(fun() -> lists:foreach(Spawn, Seq) end),
    Times = lists:map(Recv, Seq),
    summary(Times).

% many_users_many_channels_test_() ->
%     {timeout, 60, [{test_client,many_users_many_channels}]}.

% Tests that channels are implemented concurrently
many_users_many_channels() ->
    init("many_users_many_channels"),
    ParentPid = self(),
    ChansSeq = lists:seq(1, ?PERF_2_CHANS),
    UsersSeq = lists:seq(1, ?PERF_2_USERS),
    MsgsSeq  = lists:seq(1, ?PERF_2_MSGS),
    F = fun (I) ->
                fun () ->
                    try
                        output_off(),
                        Is = lists:flatten(integer_to_list(I)),
                        Nick = "user_perf2_"++Is,
                        ClientName = "client_perf2_"++Is,
                        ClientAtom = list_to_atom(ClientName),
                        GUIName = "gui_perf2_"++Is,
                        new_gui(GUIName),
                        helper:start(ClientAtom, client:initial_state(Nick, GUIName), fun client:main/1),
                        T1 = now(),
                        connect(ClientAtom),
                        G = fun(Ch_Ix) ->
                                    Ch_Ixs = lists:flatten(io_lib:format("~p", [Ch_Ix])),
                                    Channel = "#channel_"++Ch_Ixs,
                                    join_channel(ClientAtom, Channel),
                                    Send = fun (I2) ->
                                                   Is2 = lists:flatten(io_lib:format("~p", [I2])),
                                                   send_message(ClientAtom, Channel, "message_"++Is++"_"++Is2)
                                           end,
                                    lists:map(Send, MsgsSeq),
                                    leave_channel(ClientAtom, Channel),
                                    ok
                            end,
                        lists:foreach(G, ChansSeq),
                        disconnect(ClientAtom),
                        T2 = now(),
                        ParentPid ! {ready, timer:now_diff(T2, T1)}
                    catch Ex ->
                        ParentPid ! {failed, Ex}
                    end
                end
        end,
    Spawn = fun (I) -> spawn(F(I)) end,
    Recv  = fun (_) ->
                    receive
                        {ready, Time} -> Time ;
                        {failed, Ex} -> putStrLn(Ex),
                                        throw("")
                    end
            end,
    putStrLn("spawning ~p clients, each connecting to ~p channels...", [?PERF_2_USERS, ?PERF_2_CHANS]),
    spawn(fun() -> lists:foreach(Spawn, UsersSeq) end),
    Times = lists:map(Recv, UsersSeq),
    summary(Times).

summary(MTimes) ->
    Times = lists:map(fun(X) -> X/1000 end, MTimes),
    Tot = lists:sum(Times),
    Avg = Tot / length(Times),
    Med = lists:nth(length(Times) div 2, lists:sort(Times)),
    putStrLn(red("Time elapsed: ~wms average / ~wms median"), [round(Avg), round(Med)]).
