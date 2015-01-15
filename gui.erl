-module(gui).

-export([start/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

%% File with records definitions to be used as state for the client
-include_lib("./defs.hrl").

-define(SYSTEM, 0).
-define(CMDLINE, 1).
-define(NOTEBOOK, 2).
-define(MAX_CONNECTIONS, 100000).

-define(SYSTEM_NAME, "System").


% This record defines the structure of the
% client process.
%
% It contains the fields:
%
% parent: it stores the top window (used with operations
%         related to display widgets in the GUI.
% gui: it stores the name the GUI process.
% client: it stores the name of the client process.
-record(state,
	{
	  parent,
          client,
          gui,
          clientid % unique part of name, as integer
	 }).

start() ->
    Server = wx:new(),
    wx_object:start_link(?MODULE, Server, []).


init(Server) ->
    wx:batch(fun () ->
                     do_init(Server) end ).

do_init(Server) ->

    % It creates a unique name for the client and gui processes
    {ClientName, ClientID} = find_unique_name("client_", ?MAX_CONNECTIONS),
    {GUIName,_ }           = find_unique_name("gui_", ?MAX_CONNECTIONS),

    % If any of the name choosen above are taken at this point, everything crashes!
    register(to_atom(GUIName), self()),
    helper:start(to_atom(ClientName), client:initial_state("user01", GUIName), fun client:main/1),

    %% Starting GUI
    Frame = wxFrame:new(Server, -1, "Chat", []),
    Parent = Frame,
    Panel = wxPanel:new(Parent, []),


    %% Widgets: command line and system tab
    Cmd  = wxTextCtrl:new(Panel, -1, [{value, ""}, {style, ?wxTE_PROCESS_ENTER}]),
    label(ClientID, ?CMDLINE, Cmd),

    Ntbk = wxAuiNotebook:new(Panel,[{style,?wxAUI_NB_DEFAULT_STYLE}]),
    label(ClientID, ?NOTEBOOK, Ntbk),

    Tab = create_tab(ClientName, ?SYSTEM_NAME, "Welcome to CCHAT v. 0.2"),
    label(ClientID, ?SYSTEM, Tab),

    %% Sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Ntbk, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxSizer:addSpacer(MainSizer,10),
    wxSizer:add(MainSizer, Cmd, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Frame),

    focus(with_label(ClientName, ?CMDLINE)),

    wxTextCtrl:connect(Cmd, command_text_enter),

    wxAuiNotebook:connect(Ntbk, command_auinotebook_button),

    trace(["Client:", ClientName]),
    trace(["GUI:", GUIName]),

    {Panel, #state{parent=Panel, client=ClientName, clientid=ClientID, gui=GUIName}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{ event = #wxCommand{type = command_text_enter, cmdString = Item} },
             St = #state{ parent = Panel, client = ClientName, clientid = ClientID }) ->

    clear_text(with_label(ClientName, ?CMDLINE)),
    Cmd = lexgrm:parse_cmd(Item),
    trace(["Command:", Cmd]),
    case Cmd  of

         %% Connecting to the server (Specifying the remote machine)
         {connect_remote, Server, Machine} ->
            write_channel(with_label(ClientName, ?SYSTEM), "* "++"Trying to connect to "++Server++" in machine "++Machine++"..."),
            Result = catch_fatal (ClientName, Panel, fun () -> request(ClientName, {connect, {Server, Machine}}) end ),
            case Result of
                 ok     -> write_channel(with_label(ClientName, ?SYSTEM), "+ Connected!") ;
                 error  -> ok
            end ;

         %% Connecting to the server
         {connect, Server} ->
            write_channel(with_label(ClientName, ?SYSTEM), "* "++"Trying to connect to "++Server++"..."),
            Result = catch_fatal (ClientName, Panel, fun () -> request(ClientName, {connect, Server}) end ),
            case Result of
                 ok     -> write_channel(with_label(ClientName, ?SYSTEM), "+ Connected!") ;
                 error  -> ok
            end ;

         %% Disconnect from the server
         disconnect ->
            Result = catch_fatal (ClientName, Panel, fun () -> request(ClientName, disconnect) end ),
            case Result of
                 ok    -> write_channel(with_label(ClientName, ?SYSTEM), "+ Disconnected") ;
                 error -> ok
            end ;

         %% Joining a new channel
         {join, Channel}   ->
            Result = catch_fatal(ClientName, Panel, fun () -> request(ClientName, {join, Channel}) end ),
            case Result of
                 ok -> write_channel(with_label(ClientName, ?SYSTEM), "+ Joined "++Channel),
                           Tab = create_tab(ClientName, Channel, "* Channel "++Channel),
                           label(ClientID, channel_id(Channel), Tab) ;
                 error  -> ok
            end ;

         % /leave
         leave -> Channel = active_channel(with_label(ClientName,?NOTEBOOK)),
                  leave_channel(ClientName, Panel, Channel) ;

         % /leave #channel
         {leave, Channel} -> leave_channel(ClientName, Panel, Channel) ;

         %% Sending a message
         {msg, String}     ->
            Channel = active_channel(with_label(ClientName,?NOTEBOOK)),
            case Channel of
                 ?SYSTEM_NAME ->  write_channel(with_label(ClientName, ?SYSTEM), "- "++"Command not recognized: "++String) ;
                 _        -> Result = catch_fatal(ClientName, Panel,
                                                  fun () -> request(ClientName, {msg_from_GUI, Channel, String}) end ),
                             case Result of
                                 ok    -> write_channel(with_label(ClientName, channel_id(Channel)), String) ;
                                 error -> ok
                             end
            end ;

         %% Who am I?
         whoami -> Result = catch_fatal(ClientName, Panel, fun () -> request(ClientName, whoami)  end),
                   case Result of
                        error -> ok ;
                        Nick  -> write_channel(with_label(ClientName, ?SYSTEM), "* "++"You are "++Nick)
                   end ;

         %% Change nickname
         {nick, Nick} -> Result = catch_fatal(ClientName, Panel, fun () -> request(ClientName,{nick, Nick}) end ),
                         case Result of
                              ok    -> write_channel( with_label(ClientName, ?SYSTEM),
                                                      "* "++"You are known now as "++Nick) ;
                              error -> ok
                         end ;

         %% Ping someone
         {ping, Nick} -> Result = catch_fatal(ClientName, Panel, fun () -> request(ClientName,{ping, Nick}) end ),
                         case Result of
                              ok    -> write_channel( with_label(ClientName, ?SYSTEM),
                                                      "* "++"Ping "++Nick) ;
                              error -> ok
                         end ;

         %% The given command was wrong
         {ignore, Line}    ->
            write_channel(with_label(ClientName, ?SYSTEM), "- "++"Command not recognized"),
            write_channel(with_label(ClientName, ?SYSTEM), Line)
    end,
    focus(with_label(ClientName, ?CMDLINE)),
    {noreply, St} ;

% Clicking the X on a tab
handle_event(#wx{ event = #wxAuiNotebook{type = command_auinotebook_button, selection = TabPos} },
             St = #state{ parent = Panel, client = ClientName }) ->
    Ntbk    = typed_search(with_label(ClientName, ?NOTEBOOK), wxAuiNotebook),
    Channel = wxAuiNotebook:getPageText(Ntbk,TabPos),
    leave_channel(ClientName, Panel, Channel),
    {noreply, St} ;

handle_event(WX = #wx{}, State = #state{}) ->
    io:format("#wx: ~p~n",[WX]),
    io:format("#state: ~p~n",[State]),
    {noreply, State}.

%% Callbacks handled as normal gen_server callbacks (not used)
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State};

%% Here, the GUI receives a message from the client process!
handle_call({msg_to_GUI, Channel, Msg}, _From, State = #state{ client = ClientName }) ->
    write_channel( with_label(ClientName, channel_id(Channel)), Msg),
    {reply, ok, State} ;

%% Here, the GUI receives a message to the system tab
handle_call({msg_to_SYSTEM, Msg}, _From, State = #state{ client = ClientName }) ->
    write_channel(with_label(ClientName, ?SYSTEM), "* "++Msg),
    {reply, ok, State} ;

handle_call(_Msg, _From, State) ->
    {reply, {error,nyi}, State}.

handle_cast(_Msg, State) ->
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.


%% Auxiliary functions

%% Finding an unique name
find_unique_name(Prefix,N) ->
    Num = random:uniform(N),
    MStr = integer_to_list(Num),
    Name = Prefix++MStr,
    case whereis(to_atom(Name)) of
        undefined -> {Name, Num} ;
        _         -> find_unique_name(Prefix,N)
    end.

%% Debugging
trace(Args) ->
    io:format("~n~s"++lists:flatten(lists:duplicate(length(Args)-1,"~p~n")),Args).

%% GUI
clear_text(ID) ->
    CmdLine = typed_search(ID, wxTextCtrl),
    wxTextCtrl:setValue(CmdLine, "").

fatal_dialog(Parent, Error) ->
    StrError = lists:flatten(io_lib:format("~p",[Error])),
    Msg = "Something went very wrong!\n\n" ++ StrError,
    WW = wxMessageDialog:new(Parent, Msg,
                             [{style, ?wxSTAY_ON_TOP bor ?wxICON_ERROR bor ?wxOK}]),
    wxDialog:showModal(WW),
    exit(StrError).

create_tab(ClientName, Title, Init) ->
    Ntbk = typed_search(with_label(ClientName, ?NOTEBOOK), wxAuiNotebook),
    NtbkPanel = wxPanel:new(Ntbk, []),
    Msgs = wxTextCtrl:new(NtbkPanel, -1,
                           [{value, Init},
	  		    {style, ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(Msgs, false),
    wxTextCtrl:setInsertionPointEnd(Msgs),
    NtbkSizer  = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(NtbkSizer, 10),
    wxSizer:add(NtbkSizer, Msgs, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxPanel:setSizer(NtbkPanel, NtbkSizer),
    wxAuiNotebook:addPage(Ntbk,NtbkPanel,Title),
    wxWindow:setFocus(NtbkPanel),
    Msgs.

active_channel(ID) ->
    Ntbk = typed_search(ID, wxAuiNotebook),
    PageNumber = wxAuiNotebook:getSelection(Ntbk),
    Title = wxAuiNotebook:getPageText(Ntbk, PageNumber),
    Title.

close_tab(NotebookID, TabName) ->
    Ntbk = typed_search(NotebookID, wxAuiNotebook),
    Max  = wxAuiNotebook:getPageCount(Ntbk),
    Tabs = [ {wxAuiNotebook:getPageText(Ntbk,N), N} || N <- lists:seq(0,Max-1) ],
    {_, PageNumber} = lists:keyfind(TabName, 1, Tabs),
    Page = wxAuiNotebook:getPage(Ntbk,PageNumber),
    wxWindow:destroyChildren(Page),
    wxAuiNotebook:removePage(Ntbk, PageNumber).

write_channel(ID, String) ->
    DMesg = typed_search(ID,wxTextCtrl),
    wxTextCtrl:writeText(DMesg, "\n"++String).

%% Labels
focus(ID) ->
     W = wxWindow:findWindowById(ID),
     wxWindow:setFocus(W).

typed_search(ID, no_cast) ->
    Result = wxWindow:findWindowById(ID),
    % io:format("Looking for label ~p and found ~p~n",[ID,Result]),
    Result ;

typed_search(ID, Cast) ->
    Result = wxWindow:findWindowById(ID),
    {F1, F2, _, F3} = Result,
    % io:format("Looking for label ~p and found ~p~n",[ID,{F1,F2,Cast,F3}]),
    {F1,F2,Cast,F3}.

label(ClientID, ID, Widget) ->
    Label = join_ids(ClientID, ID),
    _Result = wxWindow:setId(Widget, Label),
    % io:format("Setting label ~p to widget ~p, result ~p~n",[Label,Widget, Result]),
    ok.

% with_label("client_123", 9) = 1239
with_label(ClientName, Id) ->
    CIds = lists:sublist(ClientName,8,5),
    S = lists:flatten(io_lib:format("~s~p", [CIds, Id])),
    {N, _} = string:to_integer(S),
    N.

% join_ids(123,456) = 123456
join_ids(ClientId, Id) ->
    S = lists:flatten(io_lib:format("~p~p", [ClientId, Id])),
    {N, _} = string:to_integer(S),
    N.

% Concats ASCII codes for each character into a mega integer
% channel_id("AAA") = 656565
channel_id(ChannelName) ->
    S = lists:foldl(fun(S, Acc) -> io_lib:format("~p", [S])++Acc end, "", ChannelName),
    list_to_integer(lists:flatten(S)).

% client_id("client_1234") = 1234
% client_id(ClientName) ->
%     {N, _} = string:to_integer(lists:sublist(ClientName,8,5)),
%     N.

%% Requests
request(ClientName, Msg) ->
    helper:request(to_atom(ClientName), Msg, 100000). % must be greater than default timeout

%% Errors
catch_fatal(ClientName, Panel, Cmd) ->
    case catch( Cmd() ) of
        {'EXIT',Reason} -> fatal_dialog(Panel, Reason) ;
        {error, _, Msg} -> write_channel(with_label(ClientName, ?SYSTEM), "- Error: "++Msg),
                           error ;
        Result          -> Result
    end.


to_atom(String) ->
    list_to_atom(String).


%% Leave a channel
leave_channel(ClientName, Panel, Channel) ->
    case Channel of
         ?SYSTEM_NAME -> write_channel(with_label(ClientName, ?SYSTEM), "- "++"Cannot leave System tab") ;
         Channel -> Result = catch_fatal(ClientName, Panel,
                                         fun () -> request(ClientName, {leave, Channel}) end ),
                    case Result of
                         ok    -> close_tab(with_label(ClientName, ?NOTEBOOK), Channel),
                                  write_channel(with_label(ClientName, ?SYSTEM), "* "++"Left "++Channel) ;
                         error -> ok
                    end
    end.
