-module(server).
%% Yakov Kazinets and Nicholas Szegheo
%% I pledge my honor that I have abided by the Stevens Honor System.
-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    Chatroomchecker1 = State#serv_st.chatrooms,
	Local_Regs1 = State#serv_st.registrations,
	Chatroom_already_exists = maps:is_key(ChatName, Chatroomchecker1),
	if 
	   Chatroom_already_exists ->
		CroomPID = element(2, maps:find(ChatName, Chatroomchecker1)),
		Chatroomchecker2 = Chatroomchecker1,
		Local_Regs2 = Local_Regs1,
		io:format("This chatroom already exists~n");
	   true ->
		   CroomPID = spawn(chatroom, start_chatroom, [ChatName]),
		   Chatroomchecker2 = maps:put(ChatName, CroomPID, Chatroomchecker1),
		   Local_Regs2 = maps:put(ChatName, [], Local_Regs1)
	end,

	Nicknames = State#serv_st.nicks,
	Nick_of_client = maps:get(ClientPID, Nicknames),
	CroomPID!{self(), Ref, register, ClientPID, Nick_of_client},
	PIDs_of_clients1 = maps:get(ChatName, Local_Regs2),
	PIDs_of_clients2 = PIDs_of_clients1 ++ [ClientPID],
	Local_Regs3 = maps:put(ChatName, PIDs_of_clients2, Local_Regs2),
	#serv_st{nicks = Nicknames, registrations= Local_Regs3, chatrooms = Chatroomchecker2}.


%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    Chatroomchecker1 = State#serv_st.chatrooms,
	CroomPID = maps:get(ChatName, Chatroomchecker1),
	Local_Regs1 = State#serv_st.registrations,
	PIDs_of_clients1 = maps:get(ChatName, Local_Regs1),
	PIDs_of_clients2 = lists:delete(ClientPID, PIDs_of_clients1),
	Local_Regs2 = maps:put(ChatName, PIDs_of_clients2, Local_Regs1),
	CroomPID!{self(), Ref, unregister, ClientPID},
	ClientPID!{self(), Ref, ack_leave},
	#serv_st{nicks = State#serv_st.nicks, registrations = Local_Regs2, chatrooms = State#serv_st.chatrooms}.

%% This function is a helper function for do_new_nick

rename_help(ChatName, PIDs_of_clients, ClientPID, Chatroomchecker, NewNick, Ref) ->
	Is_PID_member = lists:member(ClientPID, PIDs_of_clients),
	if Is_PID_member ->
		PID_of_c = maps:get(ChatName, Chatroomchecker),
		PID_of_c!{self(), Ref, update_nick, ClientPID, NewNick}
	end,
	PIDs_of_clients.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    All_nicks_in_use = maps:values(State#serv_st.nicks),
	Is_Nick_Old = lists:member(NewNick, All_nicks_in_use),
	if 
	   Is_Nick_Old ->
		ClientPID!{self(), Ref, err_nick_used},
		State;
	   not Is_Nick_Old ->
		   Nicknames1 = State#serv_st.nicks,
		   Nicknames2 = maps:put(ClientPID, NewNick, Nicknames1),
		   Chatroomchecker1 = State#serv_st.chatrooms,
		   maps:map(fun(X,Y) ->
			   			rename_help(X, Y, ClientPID, Chatroomchecker1, NewNick, Ref) end,
						   				State#serv_st.registrations),
		   ClientPID!{self(), Ref, nick_valid},
		   #serv_st{nicks = Nicknames2, registrations = State#serv_st.registrations, chatrooms = State#serv_st.chatrooms}
	end.

%% These functions are helper functions for do_client_quit

quit_croom(ChatName, PID_of_c, ClientPID, Ref, State) ->
	Local_Regs = State#serv_st.registrations,
	PIDs_of_clients = maps:get(ChatName, Local_Regs),
	Is_PID_member = lists:member(ClientPID, PIDs_of_clients),
	if Is_PID_member ->
		PID_of_c!{self(), Ref, unregister, ClientPID};
	   true ->
		   []
	end,
	PID_of_c.


quit_client(_ChatName, PIDs_of_clients, ClientPID) ->
	Is_PID_member = lists:member(ClientPID, PIDs_of_clients),
	if Is_PID_member ->
		lists:delete(ClientPID, PIDs_of_clients);
	   true ->
		   PIDs_of_clients
	end.



%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    Nicknames1 = State#serv_st.nicks,
	Nicknames2 = maps:remove(ClientPID, Nicknames1),
	maps:map(fun(X,Y) ->
				quit_croom(X, Y, ClientPID, Ref, State) end,
							State#serv_st.chatrooms),
	Local_Regs = maps:map(fun(X,Y) ->
							  quit_client(X, Y, ClientPID) end,
							  State#serv_st.registrations),
	ClientPID!{self(), Ref, ack_quit},
	#serv_st{nicks = Nicknames2, registrations = Local_Regs, chatrooms = State#serv_st.chatrooms}.						  
