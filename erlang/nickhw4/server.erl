%% Nick Guo and Miles Rosenberg
%% I pledge my honor that I have abided by the Stevens Honor System

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
		case maps:is_key(ChatName, State#serv_st.chatrooms) of
			false -> C = spawn(chatroom, start_chatroom, [ChatName]),
				New1 = State#serv_st{chatrooms = maps:put(ChatName,C,State#serv_st.chatrooms)},
				ChatID = C,
				ClientNick = maps:get(ClientPID, New1#serv_st.nicks),
				ChatID!{self(), Ref, register, ClientPID, ClientNick},
				New2 = New1#serv_st{registrations = maps:put(ChatName,[], New1#serv_st.registrations)},
				Reg = maps:get(ChatName, New2#serv_st.registrations),
				New2#serv_st{registrations = maps:put(ChatName, [ClientPID] ++ Reg, New2#serv_st.registrations)};
			true -> ChatID = maps:get(ChatName, State#serv_st.chatrooms),
				ClientNick = maps:get(ClientPID, State#serv_st.nicks),
				ChatID!{self(), Ref, register, ClientPID, ClientNick},
				Reg = maps:get(ChatName, State#serv_st.registrations),
				State#serv_st{registrations = maps:put(ChatName, [ClientPID] ++ Reg, State#serv_st.registrations)}
		end.
		% case maps:get(ClientPID, State#serv_st.nicks) of
		% 	NickName -> ClientNick = NickName;
		% 	_ -> io:format("Client not exists in this server~n")
		% end,

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	ChatID = maps:get(ChatName, State#serv_st.chatrooms),
	RemCl = maps:get(ChatName, State#serv_st.registrations),
	ChatID!{self(), Ref, unregister, ClientPID},
	ClientPID!{self(), Ref, ack_leave},
	State#serv_st{registrations = maps:put(ChatName, RemCl -- [ClientPID], State#serv_st.registrations)}.



%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	Nicknames = maps:values(State#serv_st.nicks),
	case lists:member(NewNick, Nicknames) of
		true -> ClientPID!{self(), Ref, err_nick_used},
				State;
		false -> 
			New1 = State#serv_st{nicks = maps:put(ClientPID, NewNick, State#serv_st.nicks)},
			RelChatName = maps:keys(maps:filter(
					fun(_K,V) ->
						lists:member(ClientPID, V)
					end,New1#serv_st.registrations)),
			_RelChatID = lists:map(
				fun(D) ->
					maps:get(D,New1#serv_st.chatrooms)!{self(), Ref, update_nick,ClientPID, NewNick}
				end,RelChatName),
			ClientPID!{self(), Ref, ok_nick},
			New1
	end.
					

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	New1 = State#serv_st{nicks = maps:remove(ClientPID, State#serv_st.nicks)},
	RelChatName = maps:keys(maps:filter(
					fun(_K,V) ->
						lists:member(ClientPID, V)
					end,New1#serv_st.registrations)),
	_RelChatID = lists:map(
				fun(D) ->
					maps:get(D,New1#serv_st.chatrooms)!{self(), Ref, unregister, ClientPID}
				end,RelChatName),
	RemoveC = maps:map( 
			fun(_K,V) ->
				case lists:member(ClientPID, V) of
					true -> V -- [ClientPID];
					false -> V 
				end
			end, New1#serv_st.registrations),
	ClientPID!{self(), Ref, ack_quit},
	New1#serv_st{registrations = RemoveC}.
