-module(chatroom).

-include_lib("defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{
				  	name = ChatName, %% the name (string) of the chatroom.
		  			registrations = maps:new(), %% ClientPID => ClientNick
					history = [] %% chat history since the beginning of that chatroom.
	}),
    ok.

loop(State) ->
    NewState =
	receive
	    %% Server tells this chatroom to register a client
	    {_ServerPID, Ref, register, ClientPID, ClientNick} ->
		do_register(State, Ref, ClientPID, ClientNick);
		
	    %% Server tells this chatroom to unregister a client
	    {_ServerPID, _Ref, unregister, ClientPID} ->
		do_unregister(State, ClientPID);
		
	    %% Server tells this chatroom to update the nickname for a certain client
	    {_ServerPID, _Ref, update_nick, ClientPID, NewNick} ->
		do_update_nick(State, ClientPID, NewNick);
		
	    %% Client sends a new message to the chatroom, and the chatroom must
	    %% propegate to other registered clients
	    {ClientPID, Ref, message, Message} ->
		do_propegate_message(State, Ref, ClientPID, Message);
	    {TEST_PID, get_state} ->
		TEST_PID!{get_state, State},
		loop(State)
end,
    loop(NewState).

%% This function should register a new client to this chatroom
do_register(State, Ref, ClientPID, ClientNick) ->
	%%update its local record of registered clients
	Registrations = State#chat_st.registrations,
	Registrations2 = maps:put(ClientPID, ClientNick, Registrations),
    State2 = #chat_st{
				  	name = State#chat_st.name,
		  			registrations = Registrations2, %% ClientPID => ClientNick
					history = State#chat_st.history},
	ClientPID!{self(), Ref, connect, State2#chat_st.history},
	State2.

%% This function should unregister a client from this chatroom
do_unregister(State, ClientPID) ->
    Registrations = State#chat_st.registrations,
	Registrations2 = maps:remove(ClientPID, Registrations),
    State2 = #chat_st{
				  	name = State#chat_st.name,
		  			registrations = Registrations2, %% ClientPID => ClientNick
					history = State#chat_st.history},
	State2.

%% This function should update the nickname of specified client.
update_history(false,History,_NewNick) ->
	History;
update_history({Client_nickname,Message},History,NewNick) ->
	New_History = lists:keyreplace(Client_nickname, 1, History, {NewNick,Message}),
	update_history(lists:keyfind(Client_nickname, 1, History),New_History,NewNick).
do_update_nick(State, ClientPID, NewNick) ->
	Registrations = State#chat_st.registrations,
	Client_nickname = maps:get(ClientPID, Registrations),
	Registrations2 = maps:put(ClientPID, NewNick, Registrations),
	History =  State#chat_st.history,
	New_History = update_history(lists:keyfind(Client_nickname, 1, History),History,NewNick),
	State2 = #chat_st{
		  	name = State#chat_st.name,
			registrations = Registrations2, %% ClientPID => ClientNick
			history = New_History},
	State2.

%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
send_message(To_PID, Nick, ClientPID, Ref, CliNick, State, Message) ->
	if
		To_PID /= ClientPID ->
			To_PID!{request,self(),Ref,{incoming_msg,CliNick,State#chat_st.name,Message}};
		true -> []
	end,
	Nick.
do_propegate_message(State, Ref, ClientPID, Message) ->
    %%io:format("chatroom:do_propegate_message(...): IMPLEMENT ME~n"),
	%% send back to the sending client
	ClientPID!{self(), Ref, ack_msg},
	%% send a message to each receiving client
	Registrations = State#chat_st.registrations,
	CliNick =  maps:get(ClientPID, Registrations),
	maps:map(fun(K,V) -> send_message(K, V, ClientPID, Ref, CliNick, State, Message) end, Registrations),
	%%append the new message
	New_History = State#chat_st.history ++ [{CliNick,Message}],
	
    #chat_st{
		  	name = State#chat_st.name,
			registrations = Registrations, %% ClientPID => ClientNick
			history = New_History}.