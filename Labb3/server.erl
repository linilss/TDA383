-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
    #server_st{name=ServerName, users=[], channel=[]}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.
handle(St, {connect, Server, {Nick, Pid}}) -> 
	NewState = St#server_st{name = Server, users=[{Nick,Pid} |St#server_st.users]},
	io:fwrite("Conn ~n"),
	{reply, ok, NewState};

handle(St, {disconnect, {Nick, Pid}}) ->
	io:fwrite("Dis ~n"),
	NewState = St#server_st{users = lists:delete({Nick, Pid}, St#server_st.users)},
	{reply, disconnect, NewState};

handle(St, {join, Channel, {Nick, Pid}}) ->
	io:fwrite("Join ~n"),
    NewState = St#server_st{channel = [[Channel] | St#server_st.channel],
    	users=[{Nick,Pid} | St#server_st.users]},
	{reply, join, NewState};

handle(St, {leave, Channel, {Nick, Pid}}) ->
	io:fwrite("leave.....111s"),
	NewState = St#server_st{users=lists:delete({Nick,Pid}, St#server_st.users),
	 	channel = lists:delete(Channel, St#server_st.channel)},
	{reply, leave, NewState};

handle(St, {msg_from_GUI, GUIName, Chatroom, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Chatroom, Msg}),
	{reply, msg_from_GUI, St};

handle(St, {whoami}) ->
	{reply, whoami, St};

handle(St, {nick, NickNew, {NickOld, Pid}}) ->
	NewState = St#server_st{users=
		[{NickNew,Pid} | lists:delete({NickOld,Pid}, St#server_st.users)]},
	{reply, nick, NewState};


handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.
