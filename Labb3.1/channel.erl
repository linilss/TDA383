-module(channel).
-export([handle/2, initial_state/1]).

-include_lib("./defs.hrl"). 

initial_state(Channel) -> 
	#channel_st{channel = Channel, user = []}. 

% User connect
handle(St, {join, Nick, Pid}) ->
	NewState = St#channel_st{user = [{Nick, Pid} | St#channel_st.user]},
	case lists:keymember(Nick, 1, St#channel_st.user) of
		true ->
			{reply, user_already_exists, St};
		false ->
			{reply, ok, NewState}
	end;

% User disconnect
handle(St, {leave, Nick, Pid}) ->
	NewState = St#channel_st{user = lists:delete({Nick, Pid}, St#channel_st.user)},
	case lists:member({Nick, Pid}, St#channel_st.user) of
		true ->
			{reply, ok, NewState};
		false ->
			{reply, user_not_existing, St}
	end;

% Send message to all clients
handle(St, {send, Pid, Nick, Msg}) ->
	io:fwrite("~p~n", [St#channel_st.user]),
	case lists:member({Nick, Pid}, St#channel_st.user) of 
		true ->
			spawn(fun() -> lists:foreach(fun({_, Pid}) -> 
				genserver:request(Pid, {incoming_msg, St#channel_st.channel, Nick, Msg}) end,
				lists:keydelete(Pid, 2, St#channel_st.user)) end),
			{reply, ok, St};
		false ->
			{reply, user_not_existing, St}
	end.
