-module(dummy_gui).
-behaviour(gen_server).
-compile(export_all).

% Mirror is the Pid of the process we send all our msgs to
init(Mirror) ->
    {ok, Mirror}.

start_link(Name, Mirror) ->
    gen_server:start_link({local,list_to_atom(Name)}, dummy_gui, Mirror, []).

% Send all messages to Mirror process
handle_call(Call, _From, Mirror) ->
    Mirror ! Call,
    {reply, {}, Mirror}.

handle_info(Info, Mirror) ->
    Mirror ! Info,
    {noreply, Mirror}.

handle_cast(Cast, Mirror) ->
    Mirror ! Cast,
    {noreply, Mirror}.

terminate(_, _State) ->
    ok.
