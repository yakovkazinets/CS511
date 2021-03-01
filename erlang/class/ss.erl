-module(ss).
-compile(export_all).

start() ->
    S = spawn(?MODULE ,server ,[]),
    [ spawn(?MODULE,client,[S]) || _ <- lists:seq(1,100000)].

client(S) -> 
    S!{start,self()}, 
    S!{add,"h",self()},
    S!{add,"e",self()}, 
    S!{add,"l",self()},
    S!{add,"l",self()}, 
    S!{add,"o",self()}, 
    S!{done,self()}, 
    receive
	{S,Str} ->
	    io:format("Done: ~p~s~n",[self(),Str])
    end.


server () ->
    receive
        {start, Client_PID} ->
            server_loop(Client_PID, "")
    end.

server_loop(C_PID, State) ->
    receive
        {add, S, C_PID} ->
            server_loop(C_PID, State++S);
        {done,C_PID} ->
            C_PID!{self(), State},
            server()
    end.