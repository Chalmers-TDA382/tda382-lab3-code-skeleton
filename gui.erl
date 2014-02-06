-module(gui).

-export([start/0, init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").

%% File with records definitions to be used as state for the client
-include_lib("./defs.hrl").

-define(SYSTEM, "System").
-define(CMDLINE, "cmdline").
-define(NOTEBOOK, "notebook").
-define(MAX_CONNECTIONS, 100000).


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
          gui
	 }).


start() ->
    Server = wx:new(),
    wx_object:start_link(?MODULE, Server, []).


init(Server) ->
    wx:batch(fun () ->
                     do_init(Server) end ).

do_init(Server) ->
    % It creates a unique name for the client and gui processes
    ClientName = find_unique_name("client_", ?MAX_CONNECTIONS),
    GUIName    = find_unique_name("gui_", ?MAX_CONNECTIONS),

    % If any of the name choosen above are taken at this point, everything crashes!
    register(to_atom(GUIName), self()),
    genserver:start(to_atom(ClientName), client:initial_state("user01", GUIName),
                    fun client:loop/2),

    %% Starting GUI
    Frame = wxFrame:new(Server, -1, "Chat", []),
    Parent = Frame,
    Panel = wxPanel:new(Parent, []),

    %% Widgets: command line and system tab
    Cmd  = wxTextCtrl:new(Panel, -1, [{value, ""},
 				     {style, ?wxTE_PROCESS_ENTER}]),
    label(ClientName, Cmd, ?CMDLINE),
    Ntbk = wxAuiNotebook:new(Panel,[{style,?wxAUI_NB_DEFAULT_STYLE}]),
    label(ClientName, Ntbk, ?NOTEBOOK),
    Tab = create_tab(ClientName, ?SYSTEM, "Welcome to CCHAT v. 0.1\n"),
    label(ClientName, Tab, ?SYSTEM),

    %% Sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Ntbk, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxSizer:addSpacer(MainSizer,10),
    wxSizer:add(MainSizer, Cmd, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, MainSizer),

    wxFrame:show(Frame),

    focus(with_label(ClientName, ?CMDLINE)),

    wxTextCtrl:connect(Cmd, command_text_enter),
    %wxAuiNotebook:connect(Ntbk, command_auinotebook_page_close),
    wxAuiNotebook:connect(Ntbk, command_auinotebook_button),

    trace(["Client:", ClientName]),
    trace(["GUI:", GUIName]),

    {Panel, #state{parent=Panel, client=ClientName, gui=GUIName}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{ event = #wxCommand{type = command_text_enter, cmdString = Item} },
             St = #state{ parent = Panel, client = ClientName }) ->

    clear_text(with_label(ClientName, ?CMDLINE)),
    Cmd = lexgrm:parse_cmd(Item),
    trace(["Command:", Cmd]),
    case Cmd  of

         %% Connecting to the server
         {connect, Server} ->
            write_channel(with_label(ClientName, ?SYSTEM), "* "++"Trying to connect to "++Server++"..."),
            Result = catch_fatal (ClientName, Panel, fun () -> request(ClientName, {connect,Server}) end ),
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
                           label(ClientName, Tab, Channel) ;
                 error  -> ok
            end ;

         leave -> Channel = active_channel(with_label(ClientName,?NOTEBOOK)),
                  leave_channel(ClientName, Panel, Channel) ;

         {leave, Channel} -> leave_channel(ClientName, Panel, Channel) ;

         %% Sending a message
         {msg, String}     ->
            Channel = active_channel(with_label(ClientName,?NOTEBOOK)),
            case Channel of
                 ?SYSTEM ->  write_channel(with_label(ClientName, ?SYSTEM), "- "++"Command not recognized"),
                             write_channel(with_label(ClientName, ?SYSTEM), String) ;
                 _        -> Result = catch_fatal(ClientName, Panel,
                                                  fun () -> request(ClientName, {msg_from_GUI, Channel, String}) end ),
                             case Result of
                                 ok    -> write_channel(with_label(ClientName, Channel), String) ;
                                 error -> ok
                             end
            end ;

         %% Who I am
         whoiam -> Result = catch_fatal(ClientName, Panel, fun () -> request(ClientName, whoiam)  end),
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

         %% The given command was wrong
         {ignore, Line}    ->
            write_channel(with_label(ClientName, ?SYSTEM), "- "++"Command not recognized"),
            write_channel(with_label(ClientName, ?SYSTEM), Line)
    end,
    focus(with_label(ClientName, ?CMDLINE)),
    {noreply, St} ;


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
    write_channel( with_label(ClientName, Channel), Msg),
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
    MStr = integer_to_list(random:uniform(N)),
    Name = Prefix++MStr,
    case whereis(to_atom(Name)) of
        undefined -> Name ;
        _         -> find_unique_name(Prefix,N)
    end.


%% Debugging
trace(Args) ->
    io:format("~n~s"++lists:flatten(lists:duplicate(length(Args)-1,"~p")),Args).

%% GUI
clear_text(Label) ->
    CmdLine = typed_search(Label, wxTextCtrl),
    Length = wxTextCtrl:getLineLength(CmdLine, 0),
    wxTextCtrl:remove(CmdLine, 0, Length).


fatal_dialog(Parent, Error) ->
    StrError = lists:flatten(io_lib:format("~p",[Error])),
    Msg = "Something went very wrong!\n\n" ++ StrError,
    WW = wxMessageDialog:new(Parent, Msg,
                             [{style, ?wxSTAY_ON_TOP bor ?wxICON_ERROR bor ?wxOK}]),
    wxDialog:showModal(WW),
    exit(StrError).

create_tab(Client, Title, Init) ->
    Ntbk = typed_search(with_label(Client, ?NOTEBOOK), wxAuiNotebook),
    NtbkPanel = wxPanel:new(Ntbk, []),
    Msgs = wxTextCtrl:new(NtbkPanel, -1,
                           [{value, Init},
	  		    {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    wxTextCtrl:setEditable(Msgs, false),
    wxTextCtrl:setInsertionPointEnd(Msgs),
    NtbkSizer  = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:addSpacer(NtbkSizer, 10),
    wxSizer:add(NtbkSizer, Msgs, [{flag, ?wxEXPAND}, {proportion,1}]),
    wxPanel:setSizer(NtbkPanel, NtbkSizer),
    wxAuiNotebook:addPage(Ntbk,NtbkPanel,Title),
    wxPanel:setFocusIgnoringChildren(NtbkPanel),
    Msgs.

active_channel(Label) ->
    Ntbk = typed_search(Label, wxAuiNotebook),
    PageNumber = wxAuiNotebook:getSelection(Ntbk),
    Title = wxAuiNotebook:getPageText(Ntbk, PageNumber),
    Title.

close_tab(Label, TabName) ->
    Ntbk = typed_search(Label, wxAuiNotebook),
    Max  = wxAuiNotebook:getPageCount(Ntbk),
    Tabs = [ {wxAuiNotebook:getPageText(Ntbk,N), N} || N <- lists:seq(0,Max-1) ],
    {_, PageNumber} = lists:keyfind(TabName, 1, Tabs),
    wxAuiNotebook:removePage(Ntbk, PageNumber).


write_channel(Label, String) ->
    DMesg = typed_search(Label,wxTextCtrl),
    wxTextCtrl:writeText(DMesg, "\n"++String).


%% Labels
focus(Label) ->
     W = wxWindow:findWindowByLabel(Label),
     wxWindow:setFocus(W).

typed_search(Label, no_cast) ->
    wxWindow:findWindowByLabel(Label) ;

typed_search(Label, Cast) ->
    {F1,F2,_,F3} = wxWindow:findWindowByLabel(Label),
    {F1,F2,Cast,F3}.

label(Client, Widget, Label) ->
    wxControl:setLabel(Widget, with_label(Client, Label)).

with_label(Client, Label) ->
    Client++Label.

%% Requests
request(ClientName, Msg) ->
    genserver:request(to_atom(ClientName), Msg).


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
         ?SYSTEM ->  write_channel(with_label(ClientName, ?SYSTEM), "- "++"Cannot leave a channel"),
                     write_channel(with_label(ClientName, ?SYSTEM), "- "++"(be on a channel tab first)") ;
         Channel -> Result = catch_fatal(ClientName, Panel,
                                         fun () -> request(ClientName, {leave, Channel}) end ),
                    case Result of
                         ok    -> close_tab(with_label(ClientName,?NOTEBOOK), Channel),
                                  write_channel(with_label(ClientName, ?SYSTEM), "* "++"Left "++Channel) ;
                         error -> ok
                    end
    end.
