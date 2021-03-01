-module(sem).
-compile(nowarn_export_all).
-compile(export_all).

start(Permits) ->
    S = spawn(?MODULE,semaphore,[Permits]),
    %% pass the pid of server to clients and
    %% spawn clients
    spawn(?MODULE,client1,[S]),
    spawn(?MODULE,client2,[S]),
    ok.

  %%  acquire(semaphore) -> 
    %%        semaphore!{acquire, self()};
   %%     release(semaphore) ->
    %%        semaphore!{release, self()}.



semaphore(0) ->
    receive
        {_From,release} ->
            semaphore(1)
    end;
semaphore(N) when N>0 ->
    receive
        {_From, release} ->
            semaphore(N+1);
        {From, acquire} ->
            From ! {self(), ok}, 
            semaphore(N-1)
        
    end.

acquire(S) ->
    S!{self(), acquire},
    receive
        {S, ok} ->
            ok
    end.

release(S) ->
    S!{self(), release}.

%% Make sure that ab is always printed after cd
client1(S) -> 
    acquire(S),
    io:format("a~n"),
    io:format("b~n").

client2(S) -> 
    io:format("c~n"),
    io:format("d~n"),
    release(S).