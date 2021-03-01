-module(server).

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
    %io:format("server:do_join(...): IMPLEMENT ME~n"),
    %State.

	%check if the chatroom exists
	Chatrooms=State#serv_st.chatrooms,
	Registrations = State#serv_st.registrations,
	Chatroom_exists = maps:is_key(ChatName, Chatrooms),
	
	if
		Chatroom_exists -> %% chatroom exists
			ChatroomPid=element(2, maps:find(ChatName, Chatrooms)),
			Chatrooms2=Chatrooms,
			Registrations2=Registrations,
			io:format("The chatroom exists ~n");
		true -> %% not exists
			ChatroomPid = spawn(chatroom,start_chatroom,[ChatName]),
			Chatrooms2 = maps:put(ChatName, ChatroomPid, Chatrooms),
			Registrations2 = maps:put(ChatName, [], Registrations)
	end,
	
	%%look up the client's nickname
	Nicks=State#serv_st.nicks,
	ClientNick = maps:get(ClientPID, Nicks),

	%% tell the chatroom that the client is joining the chatroom.
	ChatroomPid!{self(), Ref, register, ClientPID, ClientNick},
	
	%% update its record of chatroom registrations		
	Client_pids = maps:get(ChatName, Registrations2),
	Client_pids2 = Client_pids ++ [ClientPID], 
	
	Registrations3 = maps:put(ChatName, Client_pids2, Registrations2),
	
	#serv_st{
			 nicks = Nicks,
			 registrations = Registrations3,
			 chatrooms = Chatrooms2}.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	%%look up the chatroom's PID
	Chatrooms = State#serv_st.chatrooms,
	ChatroomPid= maps:get(ChatName, Chatrooms),
	
	%%remove the client
	Registrations = State#serv_st.registrations,
	Client_pids = maps:get(ChatName, Registrations),
	Client_pids2 = lists:delete(ClientPID, Client_pids),
	Registrations2 = maps:put(ChatName, Client_pids2, Registrations),

	%%send the message to chatroom
	ChatroomPid!{self(), Ref, unregister, ClientPID},
	
	%%send the message to client
	ClientPID!{self(), Ref, ack_leave},
	
	#serv_st{
			 nicks = State#serv_st.nicks,
			 registrations = Registrations2,
			 chatrooms = State#serv_st.chatrooms}.
	
    %io:format("server:do_leave(...): IMPLEMENT ME~n"),
    %State.


rename_help(ChatName,Cl_PIDS,ClientPID,Chatrooms,NewNick,Ref) ->
	Flag = lists:member(ClientPID, Cl_PIDS),
	if
		Flag->
			ChatPID = maps:get(ChatName, Chatrooms),
			ChatPID!{self(), Ref, update_nick, ClientPID, NewNick}
	end,
	Cl_PIDS.
%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    %%io:format("server:do_new_nick(...): IMPLEMENT ME~n"),
	%% check if is already used
	All_Value = maps:values(State#serv_st.nicks),
	Flag = lists:member(NewNick, All_Value),
	if
		Flag -> %% already be used
			ClientPID!{self(), Ref, err_nick_used},
			State;
		
		not Flag -> %% update
			Nicks = State#serv_st.nicks,
			Nicks2 = maps:put(ClientPID, NewNick, Nicks),
			Chatrooms = State#serv_st.chatrooms,
			maps:map(fun(K,V)-> rename_help(K,V,ClientPID,Chatrooms,NewNick,Ref) end, State#serv_st.registrations),
			ClientPID!{self(),Ref,ok_nick},
			
			#serv_st{
			 nicks = Nicks2,
			 registrations = State#serv_st.registrations,
			 chatrooms = State#serv_st.chatrooms}
	end.
 
%% executes client quit protocol from server perspective
quit_leave(ChatName, ChatPID, ClientPID, Ref, State) ->
	Registrations = State#serv_st.registrations,
	Client_pids = maps:get(ChatName, Registrations),
	Flag = lists:member(ClientPID, Client_pids),
	if
		Flag ->
			ChatPID!{self(), Ref, unregister, ClientPID};
		true ->
			[]
	end,
	ChatPID.
remove_client(_ChatName, ClientPIDs, ClientPID) ->
	Flag = lists:member(ClientPID, ClientPIDs),
	if
		Flag ->
			lists:delete(ClientPID, ClientPIDs);
		true ->
			ClientPIDs
	end.
do_client_quit(State, Ref, ClientPID) ->
    %%io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
	%%Remove client from nicknames
	Nicks = State#serv_st.nicks,
	Nicks2 = maps:remove(ClientPID, Nicks),
	%% tell each chatroom
	maps:map(fun(K,V) -> quit_leave(K, V, ClientPID, Ref, State) end, State#serv_st.chatrooms),
	%%Remove client
	Registrations = maps:map(fun(K,V) -> remove_client(K, V, ClientPID) end, State#serv_st.registrations),
	%%send message to client
	ClientPID!{self(), Ref, ack_quit},
	#serv_st{
			 nicks = Nicks2,
			 registrations = Registrations,
			 chatrooms = State#serv_st.chatrooms}.