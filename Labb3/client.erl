-module(client).
-export([handle/2, initial_state/2]).
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
    Response = genserver:request(ServerAtom, Data),
    io:fwrite("Client received: ~p~n", [Response]),
    {reply, ok, St#client_st{server = ServerAtom}} ;

%% Disconnect from server
handle(St, disconnect) ->
    Pid = self(),
    Data = {disconnect, St#client_st.nick, Pid},
    {reply, ok, St#client_st{server = none}} ;

% Join channel
handle(St, {join, Channel}) ->
    Pid = self(),
    Data = {join, St#client_st.gui, Channel },

    {reply, ok, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
    Pid = self(),
    Data = {leave, St#client_st.gui, Pid},
    {reply, ok, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    Data = {msg_from_GUI, St#client_st.channel, Msg},
    {reply, ok, St} ;

%% Get current nick
handle(St, whoami) ->
    {St#client_st.nick, St};

%% Change nick
handle(St, {nick, Nick}) ->
    S = 
    Data = {nick, St#client_st.nick, Nick},
    {reply, ok, St} ;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
