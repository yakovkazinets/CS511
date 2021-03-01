-module(q6a).
-compile(export_all).

dryCleaner ( Clean , Dirty ) -> % % Clean , Dirty are counters
    receive
        
employee ( DC ) ->  % drop off overall , then pick up a clean one ( if none
                    % is available , wait ) , and end
    todo .
dryCleanMachine ( DC ) -> % dry clean item ( if none are available , wait ) ,
                          % then sleep for 1000 milliseconds and repeat
    todo .
start (E , M ) ->
    DC =spawn(? MODULE , dryCleaner ,[0 ,0]) ,
    [ spawn(? MODULE , employee ,[DC]) || _ <- lists : seq (1 , E ) ],
    [ spawn(? MODULE , dryCleanMachine ,[DC]) || _ <- lists : seq (1 , M ) ].
