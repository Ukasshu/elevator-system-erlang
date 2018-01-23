-module (elevator_system).
-compile (export_all).
-import (panel, [print/1]).
-import (panel, [printxy/1]).


start(Amount, [LowestFloor, HighestFloor]) when is_integer(Amount)
                                        andalso is_integer(LowestFloor)
                                        andalso is_integer(HighestFloor)
                                        andalso LowestFloor =< 0
                                        andalso LowestFloor < HighestFloor
                                           ->
    MainPID = spawn_link(?MODULE, managing_init, [Amount, [LowestFloor, HighestFloor]]), 
    put(managing_process, MainPID).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
loop() ->loop().

managing_init(Amount, FloorRange) when is_integer(Amount)
                                         andalso is_list(FloorRange)
                                            ->  % wydaje mi sie ze mozna wyniesc ja wyzej do procesu glownego
    DrawingPID = spawn_link(?MODULE, drawing_init, [Amount, FloorRange]),
    PIDs = generate_elevators(Amount, 1, FloorRange, DrawingPID, []),
    put(elevators, PIDs),
    put(drawing, DrawingPID),
    put(amount, Amount),
    put(range, FloorRange),
    %io:write(get(elevators)),
    lists:foreach(fun(X) -> X!{manager, self()}, io:fwrite("sent")  end ,PIDs),
    get_all_elevators_ready(Amount),
    
    RandomPID = spawn_link(?MODULE, random_floor_init, [self(), FloorRange]),
    put(random, RandomPID),

    managing_loop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO: dodac obsluge podzialu zadan/ mowienia windzie gdzie jechac
managing_loop()  -> 
    receive
        {random, Floor, Dest} ->
            lists:foreach(fun(X) -> X!{getQueueLength, self()} end, get(elevators)),
            get_lengths_of_queues(Floor, Dest, 0, get(amount), []),
            managing_loop();
        {get, ok} -> ok;
        the_end -> managing_end()
    end.

get_lengths_of_queues(Floor, Dest, X, X, L ) when is_integer(Floor)
                                          andalso is_integer(Dest)
                                             ->
    choose_elevator(Floor, Dest, L, 0);
get_lengths_of_queues(Floor, Dest, Got, Amount, L ) when is_integer(Floor)
                                          andalso is_integer(Dest)
                                             ->
    receive
        {length, Length, PID} -> get_lengths_of_queues(Floor, Dest, Got+1, Amount, L++[{Length, PID}])
    end.

choose_elevator(Floor, Dest, [H | T], 0) -> choose_elevator(Floor, Dest, T, H);
choose_elevator(Floor, Dest, [], {_, PID}) when Dest > Floor -> PID!{updateQueue,{Floor,1,Dest}};
choose_elevator(Floor, Dest, [], {_, PID}) when Dest < Floor -> PID!{updateQueue,{Floor,-1,Dest}};
choose_elevator(Floor, Dest, [{LQ, PIDQ} | T], {L, _}) when LQ < L -> choose_elevator(Floor, Dest, T, {LQ, PIDQ});
choose_elevator(Floor, Dest, [{LQ, _} | T], {L, PID}) when LQ >= L -> choose_elevator(Floor, Dest, T, {L, PID}).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
managing_end() -> 
    lists:foreach(fun (X) -> X!the_end end, get(elevators)),
    get(drawing)!the_end,
    get(random)!the_end.

elevator_init(OrdinalNumber, Lowest, Highest, ManagingPID, DrawingPID) when Lowest < Highest
                                                                    andalso is_integer(OrdinalNumber)
                                                                    andalso is_integer(Lowest)
                                                                    andalso is_integer(Highest)
                                                                       -> 
    CyclePID  = spawn_link(?MODULE, elevator_cycle_init, [self(), DrawingPID, OrdinalNumber]),
    put(cycle_pid, CyclePID),
    put(manager_pid, ManagingPID),
    put(ord, OrdinalNumber),
    put(range, [Lowest, Highest]),
    ManagingPID!{ready, self()},
    elevator_loop(0, 0, [], true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
elevator_loop(Position, Direction, [ {HeadQueueFloor, HeadQueueDir, HeadQueueDst} | TailQueue ], IsIdle) when is_integer(Position)
                                andalso is_integer(Direction)
                                   -> 
    receive
        {updateState, NewState, Dir} -> 
            if 
                (NewState == HeadQueueFloor) and (HeadQueueDir =/= 0) -> 
                    get(cycle_pid)!{open, NewState, HeadQueueDir, HeadQueueDst},
                    self()!{updateQueue},
                    elevator_loop(NewState, HeadQueueDir, TailQueue, false);
                (NewState == HeadQueueFloor) and (HeadQueueDir == 0) ->
                    NDir = next_floor_dir(NewState, TailQueue),
                    if 
                        NDir == 0 ->
                            get(cycle_pid)!{idle},
                            elevator_loop(NewState, NDir, TailQueue, true);
                        true ->
                            get(cycle_pid)!{move, NewState, NDir, HeadQueueDst},
                            elevator_loop(NewState, NDir, TailQueue, false)
                    end;
                (NewState =/= HeadQueueFloor) ->
                    get(cycle_pid)!{move, HeadQueueDir},
                    elevator_loop(NewState, Dir, [ {HeadQueueFloor, HeadQueueDir, HeadQueueDst} | TailQueue ], false)
            end;
        {updateQueue, {NewFloor, NewDir, NewDst}} -> 
            if
                IsIdle->
                    get(cycle_pid)!{move, Position, NewDir, NewFloor}
            end,
            elevator_loop(Position, Direction, update_queue([ {HeadQueueFloor, HeadQueueDir, HeadQueueDst} | TailQueue ], {NewFloor, NewDir, NewDst}, Direction, Position), false);
        {getQueueLength, ManagerPID} -> 
            ManagerPID!{length, length([ {HeadQueueFloor, HeadQueueDir, HeadQueueDst} | TailQueue ]), self()};
        the_end -> elevator_end()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
elevator_end() -> 
    get(cycle_pid)!the_end.

elevator_cycle_init(ElevatorPID, DrawingPID, OrdinalNumber) when is_integer(OrdinalNumber)
                                                            -> 
    put(elevator_pid, ElevatorPID),
    put(drawing_pid, DrawingPID),
    put(ord, OrdinalNumber),
    elevator_cycle_loop_idle(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO : podzielić na stany windy - otwarta; w ruchu; idle
elevator_cycle_loop_idle(Pos) when is_integer(Pos)
                                   -> 
    receive
        {move, Pos, Dir, Dst} ->
            elevator_cycle_loop_move(Pos, Dir, Dst);
        {open, Pos, Dir, Dst} ->
            elevator_cycle_loop_open(Pos, Dir, Dst)
    end.

elevator_cycle_loop_move(Pos, Dir, Dst) when is_integer(Dir)         
                                     andalso is_integer(Pos)
                                        ->
    receive
    after
        3000 ->
                get(elevator_pid)!{updateState, Pos + Dir, Dir},
                get(drawing_pid)!{updated, get(ord), Pos, Pos+Dir}             
    end,
    receive
        {idle} -> elevator_cycle_loop_idle(Pos+Dir);
        {move, NPos, NDir, NDst } -> elevator_cycle_loop_move(NPos, NDir, NDst);
        {open, NPos, NDir, NDst} -> elevator_cycle_loop_open(NPos, Dir, NDst)
    end.

elevator_cycle_loop_open(Pos, Dir, Dst) when is_integer(Dir)
                                           andalso is_integer(Dst)
                                           andalso is_integer(Pos)
                                              ->
    receive
    after
        3000 -> 
            elevator_cycle_loop_move(Pos ,Dir, Dst) %trzeba chyba jakos od nowa policzyc Dest
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
elevator_cycle_end() -> ok.

drawing_init(Amount, Range) when is_integer(Amount)
                         andalso is_list(Range)
                            -> 
    put(amount, Amount),
    put(range, Range),
    draw_all_initially().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
drawing_loop() -> 
    receive
        {updated, Ord, OldPos, NewPos} -> 
            draw_update(Ord, OldPos, NewPos),
            drawing_loop();
        the_end -> 
            drawing_end()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% @TODO:
drawing_end() -> ok.


random_floor_init(ManagerPID, Range) when is_list(Range)
                                     ->
    io:write(xDDD), 
    put(manager_pid, ManagerPID),
    put(range, range),
    random_floor_loop().

random_floor_loop() -> 
    receive
        the_end ->
            random_floor_end()
    after
        3500 ->
            get(manager_pid)!{random, 5, 6},
            random_floor_loop() % trzeba ogarnac jak wybierac pietra z zakresow
    end.

random_floor_end() -> ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DODATKOWE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_elevators_ready(0) -> ok;
get_all_elevators_ready(Amount) when is_integer(Amount)
                                ->
    receive
        {ready, PID} -> get_all_elevators_ready(Amount-1)
    end.


generate_elevators(0, _, _, _, PIDs) when is_list(PIDs) -> 
    PIDs;
generate_elevators(Amount, Ord, Range, DrawingPID, PIDs) when is_integer(Amount)
                                                      andalso is_integer(Ord)
                                                      andalso is_list(Range)
                                                      andalso is_list(PIDs)
                                                         ->
    generate_elevators(Amount-1, Ord+1, Range, DrawingPID, PIDs ++ [spawn_link(?MODULE, elevator_init, [Ord] ++ Range ++ [self(), DrawingPID] )]).

draw_all_initially() ->
    print({clear}),
    Amount = get(amount),
    GroundFloorWidth = 6*Amount+5,
    [Low, High] = get(range),
    lists:foreach( fun(X) -> 
                      print({printxy, X, 6+High, w})    
                   end, lists:seq(1, GroundFloorWidth)),
    lists:foreach( fun(X) -> 
                        lists:foreach(fun(Y) ->
                                            print({printxy, 6*X-1, Y, 'X'})
                                        end, lists:seq(6, 6+High+abs(Low)))
                   end, lists:seq(1, Amount)),
    lists:foreach( fun(X) -> 
                        print({printxy, 6*X-1, 6+High, 'O'})
                   end, lists:seq(1, Amount)),
    print({gotoxy, 1, 6+High+abs(Low)+5}).

draw_update(Ord, OldPos, NewPos) when is_integer(Ord)
                              andalso is_integer(OldPos)
                              andalso is_integer(NewPos)
                                 ->
    [Low, High] = get(range),
    print({printxy, 6*Ord-1, 6+High-OldPos, 'X'}),
    print({printxy, 6*Ord-1, 6+High-NewPos, 'O'}),
    print({gotoxy, 1, 6+High+abs(Low)+5}).


next_floor_dir(_, []) -> 0;
next_floor_dir(Pos, [{Fl,_,_} | _]) when Pos < Fl -> -1;
next_floor_dir(Pos, [{Fl,_,_} | _]) when Pos > Fl -> 1.

update_queue([], {NFl, NDir, NDst}, _, _) when is_integer(NFl)
                                     andalso is_integer(NDir)
                                     andalso is_integer(NDst)
                                      -> 
    [{NFl, NDir, NDst}];
update_queue([ {HQFl, HQDir, HQDst} | TailQueue ], {NewFloor, NewDir, NewDst}, Dir, Pos) when is_integer(HQFl)
                                                                            andalso is_integer(HQDir)
                                                                            andalso is_integer(HQDst)
                                                                            andalso is_integer(NewFloor)
                                                                            andalso is_integer(NewDir)
                                                                            andalso is_integer(NewDst)
                                                                            ->
    if 
        ((NewDir-Pos)*(HQFl-Pos) < 0) and (NewDir == Dir) ->
            [{NewFloor, NewDir, NewDst}, {HQFl, HQDir, HQDst} | TailQueue];
        true ->
            [ {HQFl, HQDir, HQDst}] ++ update_queue(TailQueue, {NewFloor, NewDir, NewDst}, next_floor_dir(HQFl, TailQueue), HQFl)
    end.