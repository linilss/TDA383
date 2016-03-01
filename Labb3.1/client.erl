-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st {gui = GUIName,
    			nick = Nick,
    			server = not_connected,
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
	ServerAtom = list_to_atom(Server),
	Data = {connect, St#client_st.nick, self()},
	case catch(genserver:request(ServerAtom, Data)) of
		ok ->
	    	{reply, ok, NewState};
	    already_connected ->
	    	{reply, {error, user_already_connected, "The user is already connected!"}, St};
	    {'EXIT', _} ->
	    	{reply, {error, server_not_reached, "Server is not reachable!"}, St}
    end;

%% Disconnect from server
handle(St, disconnect) ->
	NewState = St#client_st{server = not_connected},
	Data = {disconnect, St#client_st.nick, self()},
    case St#client_st.channel of
    	[] ->
    		case St#client_st.server of
    			not_connected ->
    				{reply, {error, user_not_connected, "You are not connected to any server!"}, St};
    			_ ->
    				ServerAtom = list_to_atom(St#client_st.server),
    				case catch(genserver:request(ServerAtom, Data)) of
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
	ChannelAtom =list_to_atom(Channel),
	SData = {join, Channel},
	CData = {join, St#client_st.nick, self()},
	case St#client_st.server of
		not_connected ->
			{reply, {error, user_not_connected, "You are not connected to any server!"}, St};
		_ ->
			ServerAtom = list_to_atom(St#client_st.server),
			case catch(genserver:request(ServerAtom, SData)) of
				join ->
					case catch(genserver:request(ChannelAtom, CData)) of	
						user_already_exists ->
							{reply, {error, user_already_joined, "User is already connected to channel!"}, St};
						ok ->
							{reply, ok, NewState}
					end;
				{'EXIT', _} ->
					{reply, {error, server_not_reached, "Server is not reachable!"}, St}
			end
	end;			

%% Leave channel
handle(St, {leave, Channel}) ->
    NewState = St#client_st{channel = lists:delete(Channel, St#client_st.channel)},
    ChannelAtom = list_to_atom(Channel),
    Data = {leave, St#client_st.nick, self()},
    case catch(genserver:request(ChannelAtom, Data)) of
    	ok ->
    		{reply, ok, NewState};
    	user_not_existing ->
    		{reply, {error, user_not_joined, "User not joined to channel!"}, St}
    end;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
	ChannelAtom = list_to_atom(Channel),
	Data = {send, self(), St#client_st.nick, Msg},
	case catch(genserver:request(ChannelAtom, Data)) of
		ok ->
			{reply, ok, St};
		user_not_existing ->
			{reply, {error, user_not_joined, "User not joined to channel!"}, St}
	end;

%% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
	NewState = St#client_st{nick = Nick},
	case St#client_st.server of
		not_connected ->
			{reply, ok, NewState};
		_ ->
			{reply, {error, user_already_connected, "Can't change nick while connected to a server!"}, St}
		end;

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {reply, ok, St}.
