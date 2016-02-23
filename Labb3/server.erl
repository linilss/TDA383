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
handle(St, {connect, {Nick, Pid}}) -> 
	NewState = St#server_st{users = [{Nick, Pid} | St#server_st.users]},
	{reply, ok, NewState};

handle(St, {disconnect, Nick}) ->
	{reply, disconnect, St};

handle(St, {join, Nick}) ->
	{reply, join, St};

handle(St, {leave}) ->
	{reply, leave, St};

handle(St, {msg_from_GUI}) ->
	{reply, msg_from_GUI, St};

handle(St, {whoami}) ->
	{reply, whoami, St};

handle(St, {nick}) ->
	{reply, nick, St};


handle(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {reply, Response, St}.
