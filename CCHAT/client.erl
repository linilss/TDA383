-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {gui = GUIName,
    			nick = Nick,
    			server = user_not_connected,
    			channel = []}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
	NewState = St#client_st{server = Server},
	Pid = self(),
	case catch(genserver:request(list_to_atom(Server), {connect, St#client_st.nick, Pid})) of
		ok ->
	    	{reply, ok, NewState};
	    already_connected ->
	    	{reply, {error, user_already_connected, "The user is already connected!"}, St};
	    {'EXIT', _} ->
	    	{reply, {error, server_not_reached, "Server is not reachable!"}, St}
    end;

%% Disconnect from server
handle(St, disconnect) ->
	NewState = St#client_st{server = user_not_connected},
	Pid = self(),
    case St#client_st.channel of
    	[] ->
    		case St#client_st.server of
    			user_not_connected ->
    				{reply, {error, user_not_connected, "You are not connected to any server!"}, St};
    			_ ->
    				case catch(genserver:request(list_to_atom(St#client_st.server), {disconnect, St#client_st.nick, Pid})) of
    					ok ->
    						{reply, ok, NewState};
    					{'EXIT', _} ->
    						{reply, {error, server_not_reached, "Could not disconnect from the server!"}, St}
    				end
    		end;
    	_ ->
    		{reply, {error, leave_channels_first, "You need to leave all channels first!"}, St}
    end;

% Join channel
handle(St, {join, Channel}) ->
	NewState = St#client_st{channel = [Channel | St#client_st.channel]},
	Pid = self(),
	case lists:member(Channel, St#client_st.channel) of
		false ->
			genserver:request(list_to_atom(St#client_st.server), {join, St#client_st.nick, Pid, Channel}),
			{reply, ok, NewState};
		true ->
			{reply, {error, user_already_joined, "User already in channel!"}, St}
	end;

%% Leave channel
handle(St, {leave, Channel}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
    % {reply, ok, St} ;
    {reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
    % {reply, "nick", St} ;
    {reply, St#client_st.nick, St};

%% Change nick
handle(St, {nick, Nick}) ->
	NewState = St#client_st{nick = Nick},
	case St#client_st.server of
		user_not_connected ->
			{reply, ok, NewState};
		_ ->
			{reply, {error, user_already_connected, atom_to_list(user_already_connected)}, St}
		end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
