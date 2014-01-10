-module(ch).
-export([channel_loop/2]).

-include_lib("./defs.hrl").


channel_loop(St = #ch_st{name = Channel}, {join, User, Pid}) ->
         case user_name(St, Pid) of 
              user_not_joined  -> {ok, add_user(St,User,Pid) } ;
              _                -> { {error, user_already_joined, "already joined to "++Channel}, St } 
         end ;

channel_loop(St,{write_ch, From, Msg}) ->
         Name = user_name(St, From),  %% Possible bottleneck
         case Name of 
              user_not_joined -> { {error, user_not_joined, "no write in this channel, user not joined"}, St } ;
              _               -> broadcast_ch(Name, From, Msg, St)
    end;

channel_loop(St,{leave, Pid}) ->
         Name = user_name(St, Pid), 
         case Name of 
              user_not_joined -> { {error, user_not_joined, "cannot leave a channel which I have not joined!"}, St } ;
              _               -> NewSt = remove_user(St, Pid), 
                                 {ok, NewSt} 
         end ;

channel_loop(St, debug_ch) ->
    {St, St}.


%% Auxiliary functions
add_user(St, User, Pid) ->
    NewPids = lists:keydelete(Pid, 2, St#ch_st.pids), %% In case the user is joining but has a different Pid
    St#ch_st{ pids= [ {User, Pid} | NewPids ] }.

remove_user(St, Pid) ->
    NewPids = lists:keydelete(Pid, 2, St#ch_st.pids),
    St#ch_st{ pids= NewPids }.

% Send a msg to the clients (list of Pids)
broadcast_ch(Name, Pid, Msg, St = #ch_st{ name = Channel, pids = Members} ) ->
    Others = lists:keydelete(Pid, 2, Members),
    lists:foreach(fun({_,PidU}) ->
                           spawn(fun () -> genserver:request(PidU, {income_msg, Name, Channel, Msg}) end)
                  end, Others), 
    {ok, St}.

% Gives the user Pid
user_name(St, Pid) ->    
    case lists:keyfind(Pid, 2, St#ch_st.pids) of 
        false -> user_not_joined ;
        {Name, Pid} -> Name
    end.
             
