-module(es).
-compile(export_all).

start()->
    spawn(?MODULE, server_loop, [])

server_loop()->
    recieve
        {From, Msg} ->
                From!{Msg},
                server_loop();
    end.