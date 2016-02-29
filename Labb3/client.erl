-module(client).
-export([handle/2, initial_state/2, user/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {    
        gui = GUIName, 
        nick = Nick,
        server = none,
        channel = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
    Pid = self(),

    Data = {connect, St#client_st.server,Pid},
    io:fwrite("Client is sending: ~p~n", [Data]),
    ServerAtom = list_to_atom(Server),
    Response = genserver:request(ServerAtom, {connect, Server, user(St)}),
    io:fwrite("Client received: ~p~n", [ServerAtom]),
    {reply, ok, St#client_st{server = ServerAtom}} ;

%% Disconnect from server
handle(St, disconnect) ->

    Response = genserver:request(St#client_st.server, {disconnect, user(St)}),
    io:fwrite("Client recieved ~p ~n", [Response]),
    {reply, ok, St#client_st{server = none}};

% Join channel
handle(St, {join, Channel}) ->
    ServerAtom = St#client_st.server,
    Response = genserver:request(ServerAtom, {join, Channel, user(St)}),
    NewState = St#client_st{channel = [[Channel] | St#client_st.channel]},
    {reply, ok, NewState};

%% Leave channel
handle(St, {leave, Channel}) ->
    ServerAtom = St#client_st.server,
    Response = genserver:request(ServerAtom, {leave, Channel, user(St)}),
    NewState = St#client_st{channel = lists:delete(Channel, St#client_st.channel)},
    {reply, ok, NewState} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    ServerAtom = St#client_st.server,
    Data = {msg_from_GUI, St#client_st.gui, Channel, St#client_st.nick++"> "++Msg},
    Response = genserver:request(ServerAtom, Data),
    {reply, ok, St} ;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};

%% Change nick
% Fix dis shit! 
% Kraschar om vi inte är anslutna till server
handle(St, {nick, Nick}) ->
    ServerAtom = St#client_st.server,
    Response = genserver:request(ServerAtom, {nick, Nick, user(St)}),
    NewState = St#client_st{nick = Nick},
    {reply, ok, NewState};

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.

user(St) ->
    {St#client_st.nick, self()}.