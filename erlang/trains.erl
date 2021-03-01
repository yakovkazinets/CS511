%% Yakov Kazinets
%% I pledge my honor that I have abided by the Stevens Honor System.
%% December 9, 2020
start () ->
    register (loading_machine, spawn(fun loadingMachine /0)) ,
    register (control_center, spawn(?MODULE , controlCenterLoop ,[1,1])) ,
    [spawn (?MODULE,passengerTrain ,[0]) || _ <- lists:seq (1,10) ],
    [spawn (?MODULE,passengerTrain ,[1]) || _ <- lists:seq (1,10) ],
    [spawn (?MODULE,freightTrain ,[0]) || _ <- lists:seq (1,5) ],
    [spawn (?MODULE,freightTrain ,[1]) || _ <- lists:seq (1,5) ].

passengerTrain ( Direction ) ->
    acquireTrack (Direction) ,
    timer : sleep (rand : uniform (1000)) , 
    releaseTrack (Direction).

freightTrain ( _Direction ) ->
    acquireTrack (0) ,
    acquireTrack (1) ,
    waitForLoadingMachine () ,
    releaseTrack (0) ,
    releaseTrack (1).

loadingMachine () ->
    receive
        {From , permToProcess } ->
            timer : sleep ( rand : uniform (1000)) ,
            From !{ doneProcessing },
            loadingMachine ()
    end.  

%% activate loading machine and then wait for it to finish
waitForLoadingMachine (N) -> %%TODO --------------------------------------------------------------
    N!{self(), permToProcess},
    receive
        {doneProcessing} ->
            ok
    end.



releaseTrack (N) ->
    whereis(control_center)!{ self(), release, N}.


acquireTrack (N) -> %% TODO -----------------------------------------------------------------------
    whereis(control_center)!{self(), acquire, N}.




%% used by acquireTrack and releaseTrack
%% S0 is 0 ( track 0 has been acquired ) or 1 ( track 0 is free )
%% S1 is 0 ( track 1 has been acquired ) or 1 ( track 1 is free )
%% understands two types of messages :
%% {From , acquire ,N} -- acquire track N
%% {From , release ,N} -- release track N
controlCenterLoop (S0 , S1) -> %% TODO --------------------------------------------------------------
    receive
        {From, acquireTrack, N}->
            case N of 
                S0 -> controlCenterLoop (0, S1);
                S1 -> controlCenterLoop (S0, 0)
            end;
        {From, releaseTrack, N}->
            case N of
                S0 -> controlCenterLoop (1, S1);
                S1 -> controlCenterLoop (S0, 1)
            end,
    end.
