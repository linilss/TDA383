-module(channel).
-export([handle/2, initial_state/1]).

-include_lib("./defs.hrl"). 

initial_state(Channel) -> 
	#channel_st{channel = Channel, user = []}. 

% User connect
handle(St, {join, Pid}) ->
	NewState = St#channel_st{user = [Pid | St#channel_st.user]},
	case lists:member(Pid, St#channel_st.user) of
		true ->
			{reply, user_already_joined, St};
		false ->
			{reply, ok, NewState}
	end;

% User disconnect
handle(St, {leave, Pid}) ->
	NewState = St#channel_st{user = lists:delete(Pid, St#channel_st.user)},
	case lists:member(Pid, St#channel_st.user) of
		true ->
			{reply, ok, NewState};
		false ->
			{reply, user_not_existing, St}
	end;

% Send message to all clients
handle(St, {send, Pid, Nick, Msg}) ->
	case lists:member(Pid, St#channel_st.user) of 
		true ->
			[spawn(fun() -> genserver:request(N, {St#channel_st.channel, Nick, Msg}) end) || N <- St#channel_st.user, N /= Pid],
			{reply, ok, St};
		false ->
			{reply, user_not_existing, St}
	end.
	

