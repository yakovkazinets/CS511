-module(fs).
-compile(nowarn_export_all).
-compile(export_all).

fact(0) ->
    1;
fact(N)->
    N*fact(N-1).

start(N) ->
    S = spawn(?MODULE, server, []),
    [ spawn(?MODULE, client, [S]) || _ <- lists:seq (1,N)].

server() ->
    receive
        {From, req,N}->
            From!{self(),repl,fact(N)},
            server()
    end.

clinet(S) ->
    N = rand:uniform(20),
    S!{self(), req, N},
    receive
        {S, repl, F} ->
            io:format("The factorial of ~w is ~w~n", [N,F])
    end.
