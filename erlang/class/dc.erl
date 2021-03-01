
% I pledge my honor that I have abided by the Stevens Honor System.
% Yakov Kazinets and Alexander Heifler
-module(dc).
-compile(export_all).

% {dropOffOverall} Sent by employee
% {From,dryCleanItem} Sent by dry cleaning machine
% {From,pickUpOverall} Send by employee
% Here is the stub you must complete. You module must be called dc.erl.
dryCleaner(Clean, Dirty) -> % % Clean , Dirty are counters
    receive
        {From, pickupOverall} when Clean > 0->
            From!{doneDropping},
            dryCleaner(Clean-1,Dirty);
        {From, dryCleanItem} when Dirty > 0->
            From!{doneDryClean},
            dryCleaner(Clean+1,Dirty-1);
        {dropOffOverall} ->
            dryCleaner(Clean, Dirty+1)
    end.
employee(DC)  -> % drop off overall , then pick up a clean one ( if none % is available , wait ) , and end
    DC!{self(), dryCleanItem},
    receive
        
        {doneDropping} ->
            ok

    end.
dryCleanMachine( DC ) -> % dry clean item ( if none are available , wait ) ,
% then sleep for 1000 milliseconds and repeat
    DC!{self(), dryCleanItem},
    receive
        {doneDryClean} ->
            dryCleanMachine:sleep(1000),
            dryCleanMachine(DC)
            
    end.
start(E,M)->
    DC = spawn(?MODULE ,dryCleaner ,[0,0]),
    [spawn(?MODULE ,employee ,[DC]) || _ <- lists:seq(1,E)],
    [spawn(?MODULE ,dryCleanMachine ,[DC]) || _ <- lists:seq(1,M)].
