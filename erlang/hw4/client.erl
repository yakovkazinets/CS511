-module(client).
%% Yakov Kazinets and Nicholas Szegheo
%% I pledge my honor that I have abided by the Stevens Honor System.
-export([main/1, initial_state/2]).

-include_lib("./defs.hrl").

-spec main(_InitialState) -> _.
-spec listen(_State) -> _.
-spec initial_state(_Nick, _GuiName) -> _InitialClientState.
-spec loop(_State, _Request, _Ref) -> _.
-spec do_join(_State, _Ref, _ChatName) -> _.
-spec do_leave(_State, _Ref, _ChatName) -> _.
-spec do_new_nick(_State, _Ref, _NewNick) -> _.
-spec do_new_incoming_msg(_State, _Ref, _SenderNick, _ChatName, _Message) -> _.

%% Receive messages from GUI and handle them accordingly
%% All handling can be done in loop(...)
main(InitialState) ->
    %% The client tells the server it is connecting with its initial nickname.
    %% This nickname is guaranteed unique system-wide as long as you do not assign a client
    %% the nickname in the form "user[number]" manually such that a new client happens
    %% to generate the same random number as you assigned to your client.
    whereis(server)!{self(), connect, InitialState#cl_st.nick},
    %% if running test suite, tell test suite that client is up
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{client_up, self()}
    end,
    %% Begins listening
    listen(InitialState).

%% This method handles all incoming messages from either the GUI or the
%% chatrooms that are not directly tied to an ongoing request cycle.
listen(State) ->
    receive
        {request, From, Ref, Request} ->
	    %% the loop method will return a response as well as an updated
	    %% state to pass along to the next cycle
            {Response, NextState} = loop(State, Request, Ref),
	    case Response of
		{dummy_target, Resp} ->
		    io:format("Use this for whatever you would like~n"),
		    From!{result, self(), Ref, {dummy_target, Resp}},
		    listen(NextState);
		%% if shutdown is received, terminate
		shutdown ->
		    ok_shutdown;
		%% if ok_msg_received, then we don't need to reply to sender.
		ok_msg_received ->
		    listen(NextState);
		%% otherwise, reply to sender with response
		_ ->
		    From!{result, self(), Ref, Response},
		    listen(NextState)
	    end
    end.

%% This function just initializes the default state of a client.
%% This should only be used by the GUI. Do not change it, as the
%% GUI code we provide depends on it.
initial_state(Nick, GUIName) ->
    #cl_st { gui = GUIName, nick = Nick, con_ch = maps:new() }.

%% ------------------------------------------
%% loop handles each kind of request from GUI
%% ------------------------------------------
loop(State, Request, Ref) ->
    case Request of
	%% GUI requests to join a chatroom with name ChatName
	{join, ChatName} ->
	    do_join(State, Ref, ChatName);

	%% GUI requests to leave a chatroom with name ChatName
	{leave, ChatName} ->
	    do_leave(State, Ref, ChatName);

	%% GUI requests to send an outgoing message Message to chatroom ChatName
	{outgoing_msg, ChatName, Message} ->
	    do_msg_send(State, Ref, ChatName, Message);

	%% GUI requests the nickname of client
	whoami ->
	    {{dummy_target, dummy_response}, State}; %% <-------------------------issues, where is it???

	%% GUI requests to update nickname to Nick
	{nick, Nick} ->
            do_new_nick(State, Ref, Nick);

	%% GUI requesting to quit completely
	quit ->
	    do_quit(State, Ref);

	%% Chatroom with name ChatName has sent an incoming message Message
	%% from sender with nickname SenderNick
	{incoming_msg, SenderNick, ChatName, Message} ->
	    do_new_incoming_msg(State, Ref, SenderNick, ChatName, Message);

	{get_state} ->
	    {{get_state, State}, State};

	%% Somehow reached a state where we have an unhandled request.
	%% Without bugs, this should never be reached.
	_ ->
	    io:format("Client: Unhandled Request: ~w~n", [Request]),
	    {unhandled_request, State}
    end.

%% executes `/join` protocol from client perspective
do_join(State, Ref, ChatName) ->
    Chatroomchecker = maps:is_key(ChatName, State#cl_st.con_ch),
	if 
	 	Chatroomchecker ->
			{err,State};
	   	true ->
		   whereis(server)!{self(), Ref, join, ChatName},
		   receive
			   {ChatroomPid, Ref, connect, History} ->
				   Connect_chat1 = State#cl_st.con_ch,
				   Connect_chat2 = maps:put(ChatName, ChatroomPid, Connect_chat1),
				   State_2 = #cl_st{gui = State#cl_st.gui, nick = State#cl_st.nick, con_ch = Connect_chat2},
				   {History, State_2}
		   end
	end.


%% executes `/leave` protocol from client perspective
do_leave(State, Ref, ChatName) ->
    Chatroomchecker = maps:is_key(ChatName,State#cl_st.con_ch),
	if 
		not Chatroomchecker ->
			{err, State};
		Chatroomchecker ->
			whereis(server)!{self(), Ref, leave, ChatName},
			receive
				{_Server, Ref, ack_leave} -> %%<-----------------------------------------
					Connect_chat1 = State#cl_st.con_ch,
				   	Connect_chat2 = maps:remove(ChatName, Connect_chat1),
					State_2 = #cl_st{gui = State#cl_st.gui, nick = State#cl_st.nick, con_ch = Connect_chat2},
					{ok, State_2}
			end
	end.

%% executes `/nick` protocol from client perspective
do_new_nick(State, Ref, NewNick) ->
    Current_Nick = State#cl_st.nick,
	Is_Nick_Old = Current_Nick == NewNick,
	if 
		Is_Nick_Old -> %% This means they are the same
			{err_same, State};
	   true ->
		   whereis(server)!{self(), Ref, nick, NewNick},
		   receive
		   		{_Server, Ref, err_nick_used} ->
				   {err_same, State};
				{_Server, Ref, ok_nick} ->
					State_2 = #cl_st{gui = State#cl_st.gui, nick = NewNick, con_ch = State#cl_st.con_ch},
					{ok_nick, State_2}
		   end
	end.

%% executes send message protocol from client perspective
do_msg_send(State, Ref, ChatName, Message) ->
    Connect_chat1 = State#cl_st.con_ch,
	Msg_PID = maps:get(ChatName, Connect_chat1),
	Msg_PID!{self(), Ref, message, Message},
	receive
		{Msg_PID, Ref, ack_msg} ->
			{{msg_sent, State#cl_st.nick},
			State}
	end.

%% executes new incoming message protocol from client perspective
do_new_incoming_msg(State, _Ref, CliNick, ChatName, Msg) ->
    %% pass message along to gui
    gen_server:call(list_to_atom(State#cl_st.gui), {msg_to_GUI, ChatName, CliNick, Msg}),
    {ok_msg_received, State}.

%% executes quit protocol from client perspective
do_quit(State, Ref) ->
    whereis(server)!{self(), Ref, quit},
	receive
		{_Server, Ref, ack_quit} ->
			{shutdown, State}
	end.
