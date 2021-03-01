-module(chatroom).
%% Yakov Kazinets and Nicholas Szegheo
%% I pledge my honor that I have abided by the Stevens Honor System.
-include_lib("./defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{name = ChatName,
		  registrations = maps:new(), history = []}),
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
    Local_Regs1 = State#chat_st.registrations,
	Local_Regs2 = maps:put(ClientPID, ClientNick, Local_Regs1),

	State_2 = #chat_st{name = State#chat_st.name, registrations = Local_Regs2, history = State#chat_st.history},

	ClientPID!{self(), Ref, connect, State_2#chat_st.history},

	State_2.

%% This function should unregister a client from this chatroom
do_unregister(State, ClientPID) ->
    Local_Regs1 = State#chat_st.registrations,
	Local_Regs2 = maps:remove(ClientPID, Local_Regs1),
	State_2 = #chat_st{name = State#chat_st.name, registrations = Local_Regs2, history = State#chat_st.history},
	State_2.

%% This function is a helper function to do_update_nick
update_nick_helper(false, Previous_Logs, _NewNick) ->
	Previous_Logs;
update_nick_helper({Nick_of_client1, Message}, Previous_Logs, NewNick) ->
	New_Logs = lists:keyreplace(Nick_of_client1, 1, Previous_Logs, {NewNick, Message}),

	update_nick_helper(lists:keyfind(Nick_of_client1, 1, Previous_Logs), 
					    			 New_Logs,
									 NewNick).


%% This function should update the nickname of specified client.
do_update_nick(State, ClientPID, NewNick) ->
	Local_Regs1 = State#chat_st.registrations,
	Local_Regs2 = maps:get(ClientPID, Local_Regs1),

	Nick_of_client1 = maps:get(ClientPID, Local_Regs1),
	Previous_Logs = State#chat_st.history,
	New_Logs = update_nick_helper(lists:keyfind(Nick_of_client1, 1, Previous_Logs),
												Previous_Logs,
												NewNick),
	
	State_2 = #chat_st{name = State#chat_st.name, registrations = Local_Regs2, history = New_Logs},
	State_2.
	
%% This function is a helper function for propegate message

prop_helper(X, Y, ClientPID, Ref, Nick_of_client2, State, Message) ->
	if
		X /= ClientPID ->
			X!{request, self(), Ref, 
				{incoming_msg, Nick_of_client2, State#chat_st.name, Message}};
		true ->
			[]
	end,
	Y.


%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
do_propegate_message(State, Ref, ClientPID, Message) ->
    ClientPID!{self(), Ref, ack_msg},

	Local_Regs1 = State#chat_st.registrations,
	Nick_of_client2 = maps:get(ClientPID, Local_Regs1),
	maps:map(fun(X,Y) ->
				prop_helper(X, Y, ClientPID, Ref, Nick_of_client2, State, Message) end,
							Local_Regs1),
	New_Logs = State#chat_st.history ++ [{Nick_of_client2, Message}],

	#chat_st{name = State#chat_st.name, registrations = Local_Regs1, history = New_Logs}.
