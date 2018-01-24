-module (elevator_system_v2).
-compile(export_all).
-import (panel,[print/1]).
-import (panel,[printxy/1]).

start(Amount, [LowestFloor, HighestFloor]) when is_integer(Amount)
                                        andalso is_integer(LowestFloor)
                                        andalso is_integer(HighestFloor)
                                        andalso LowestFloor =< 0
                                        andalso Amount > 0
                                        andalso LowestFloor < HighestFloor ->
    ManagingPID = spawn_link(?MODULE, managing_init, [Amount, [LowestFloor, HighestFloor]]),
    put(managing_process, ManagingPID),
    loop().

loop() -> loop().

managing_init(Amount, FloorRange) when is_integer(Amount)
                               andalso is_list(FloorRange)
                               andalso Amount > 0 ->
    DrawingPID = spawn_link(?MODULE, drawing_init, [Amount, FloorRange]),
    PIDs = generate_elevators(Amount, 1, FloorRange, DrawingPID, []),
    put(elevators_processes, PIDs),
    put(drawing_process, DrawingPID),
    put(amount, Amount),
    put(range, FloorRange),
    lists:foreach(fun(X) ->
                    X!{manager, self()}
                  end, PIDs),
    get_elevators_ready(Amount),
    RandomPID = spawn_link(?MODULE, random_init, [self(), FloorRange]),
    put(random_process, RandomPID),
    managing_loop().

managing_loop() ->
    receive
        {random, NewFloor, NewDest} ->
            lists:foreach(fun(X) ->
                            X!{getQueueLength, self()}
                          end, get(elevators_processes)),
            Lengths = get_lengths_of_queues(get(amount), []),
            choose_and_send_update(NewFloor, NewDest, Lengths, 0),
            managing_loop();
        the_end -> managing_end()
    end.

managing_end() ->
    lists:foreach(fun(X) ->
                      X!the_end
                  end, get(elevators_processes)),
    get(drawing)!the_end,
    get(random)!the_end.

drawing_init(Amount, FloorRange) ->
    draw_all_initially(Amount, FloorRange),
    put(amount, Amount),
    put(range, FloorRange),
    drawing_loop().

drawing_loop() ->
    receive
        {updated, Ord, Old, New} ->
            draw_update(Ord, Old, New),
            drawing_loop();
        the_end -> drawing_end()
    end.

drawing_end() -> ok.

random_init(ManagingPID, [Low, High]) when is_integer(Low)
                                   andalso is_integer(High) ->
    put(low, Low),
    put(high, High),
    put(managing_process, ManagingPID),
    random_loop().

random_loop() ->
    receive
        the_end -> 
            random_end()
    after
        3500 ->
            io:fwrite("3500~n"),
            get(managing_process)!{random, 5, 6},
            random_loop()
    end.

random_end() -> ok.

elevator_init(Ord, FloorRange, ManagingPID, DrawingPID)->
    put(ordinal, Ord),
    put(range, FloorRange),
    put(managing_process, ManagingPID),
    put(drawing_process, DrawingPID),
    CyclePID = spawn_link(?MODULE, cycle_init, [self(), DrawingPID, Ord]),
    put(cycle_process, CyclePID),
    ManagingPID!ready,
    elevator_loop(0, 0, []).

elevator_loop(Pos, Dir, Queue) -> 
    receive
        the_end ->
            get(cycle_process)!the_end;
        {updateQueue, NewFloor, NewDest} ->
            NewQueue = update_queue(Queue, Pos, NewFloor, quick_dir(NewFloor, NewDest), NewDest),
            if 
                Dir == 0 ->
                    get(cycle_process)!{move, Pos, quick_dir(Pos, NewFloor)}
            end,
            elevator_loop(Pos,Dir, NewQueue);
        {getQueueLength, ManagingPID} ->
            ManagingPID!{length, self(), length(Queue)},
            elevator_loop(Pos, Dir, Queue);
        {updateState, NPos, Dir} ->
            [{HPos, HDir, HDst} | Tail] = Queue,
            if 
                HDst ==(-1000) ->
                    if
                        NPos =/= HPos ->
                            get(cycle_process)!{move, NPos, Dir},
                            elevator_loop(NPos, Dir, Queue);
                        NPos == HPos ->
                            NDir = calculate_dir(NPos, Tail),
                            if 
                                NDir == 0 ->
                                    get(cycle_process)!{idle},
                                    elevator_loop(NPos, NDir, Tail);
                                true ->
                                    get(cycle_process)!{move, NPos, NDir},
                                    elevator_loop(NPos, NDir, Tail)
                            end
                    end;
                true ->
                    if
                        NPos =/= HPos ->
                            get(cycle_process)!{move, NPos, Dir},
                            elevator_loop(NPos, Dir, Queue);
                        NPos == HPos ->
                            NewQueue = update_queue(Tail, NPos, HDst, Dir, -1000),
                            NDir = calculate_dir(NPos, NewQueue),
                            get(cycle_process)!{move, NPos, HDir},
                            elevator_loop(NPos, NDir, NewQueue)
                            
                    end
            end

    end.

elevator_end() -> 
    get(cycle_process)!the_end.

cycle_init(ElevatorPID, DrawingPID, Ord)->
    put(elevator_process, ElevatorPID),
    put(drawing_process, DrawingPID),
    put(ordinal, Ord),
    cycle_loop_idle(0).

cycle_loop_idle(Pos) when is_integer(Pos) -> 
    receive
        the_end ->
            cycle_end();
        {move, Pos, Dir} ->
            io:fwrite("mam~n"),
            cycle_loop_move(Pos, Dir)
    end.

cycle_loop_move(Pos, Dir) when is_integer(Pos)
                       andalso is_integer(Dir) -> 
    receive
        the_end ->
            cycle_end()
    after
        1500 ->
            get(drawing_process)!{updated, get(ordinal), Pos, Pos+Dir},
            get(elevator_process)!{updateState, Pos+Dir, Dir}
    end,
    receive
        the_end ->
            cycle_end();
        {idle} -> 
            cycle_loop_idle(Pos+Dir);
        {move, NPos, NDir} when NPos == Pos+Dir
                        andalso Dir == NDir ->
            cycle_loop_move(Pos+Dir, Dir);
        {open, NPos, NDir} when NPos == Pos+Dir ->
            cycle_loop_open(NPos, NDir)
    end.

cycle_loop_open(Pos, Dir) when is_integer(Pos)
                       andalso is_integer(Dir) -> 
    receive
        the_end ->
            cycle_end()
    after
        3000 ->
            cycle_loop_move(Pos, Dir)
    end.

cycle_end() -> ok.
%%%%%%%%%%%%%%%%%%%%%%%            %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% POMOCNICZE %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%            %%%%%%%%%%%%%%%%%%%%%%%

get_elevators_ready(0) -> ok;
get_elevators_ready(Amount) when is_integer(Amount) ->
    receive
        ready -> get_elevators_ready(Amount-1)
    end.

generate_elevators(X, Y, _, _, PIDs) when is_list(PIDs)
                                    andalso is_integer(X)
                                    andalso Y == X+1 -> 
    PIDs;
generate_elevators(Amount, Ord, FloorRange, DrawingPID, PIDs) when is_integer(Amount)
                                                           andalso is_integer(Ord) 
                                                           andalso is_list(FloorRange)
                                                           andalso is_list(PIDs) ->
    NewPID = spawn_link(?MODULE, elevator_init, [Ord, FloorRange, self(), DrawingPID]),
    generate_elevators(Amount, Ord+1, FloorRange, DrawingPID, PIDs++[NewPID]).

get_lengths_of_queues(0, Lenghts) -> Lenghts;
get_lengths_of_queues(Amount,Lenghts) -> 
    receive
        {length, PID, Len} ->
            get_lengths_of_queues(Amount-1, Lenghts++[{PID, Len}])
    end.

choose_and_send_update(NewFloor, NewDest, [{HPID, HLen} | Tail], 0) -> choose_and_send_update(NewFloor, NewDest, Tail, {HPID, HLen});
choose_and_send_update(NewFloor, NewDest, [{HPID, HLen} | Tail], {_, CurLen}) when HLen < CurLen -> choose_and_send_update(NewFloor, NewDest, Tail, {HPID, HLen});
choose_and_send_update(NewFloor, NewDest, [{_, HLen} | Tail], {CurPID, CurLen}) when HLen >= CurLen -> choose_and_send_update(NewFloor, NewDest, Tail, {CurPID, CurLen});
choose_and_send_update(NewFloor, NewDest, [], {CurPID, _}) -> CurPID!{updateQueue, NewFloor, NewDest}.

draw_all_initially(Amount, [Low, High])  when is_integer(Amount)
                                      andalso is_integer(Low)
                                      andalso is_integer(High) ->
    print({clear}),
    print({gotoxy, 5,3}),
    io:fwrite("Symulacja systemu wind sprzezonych"),
    lists:foreach(fun(X) -> 
                      print({printxy, X, 6+High, w}) 
                  end, lists:seq(1, 6*Amount+5)),
    lists:foreach(fun(X) ->
                      lists:foreach(fun(Y) ->
                                        print({printxy, 6*X-1, Y, 'X'})
                                    end, lists:seq(6, 6+High+abs(Low)))
                  end, lists:seq(1, Amount)),
    lists:foreach(fun(X) ->
                      print({printxy, 6*X-1, 6+High, 'O'})
                  end, lists:seq(1, Amount)),
    print({gotoxy, 1, 6+High+abs(Low)+5}).

draw_update(Ord,Old,New) -> 
    [Low, High] = get(range),
    print({printxy, 6*Ord-1, 6+High-Old, 'X'}),
    print({printxy, 6*Ord-1, 6+High-New, 'O'}),
    print({gotoxy, 1, 6+High+abs(Low)+5}). 

calculate_dir(_, []) -> 0;
calculate_dir(NPos,[{HPos, _, _} | _]) when NPos < HPos -> -1;
calculate_dir(NPos,[{HPos, _, _} | _]) when NPos > HPos -> 1.

quick_dir(Floor, Dest) when Dest > Floor -> 1;
quick_dir(Floor, Dest) when Dest < Floor -> -1.

update_queue([], Pos, Floor, Dir, NDst) when (Floor-Pos)*Dir > 0 -> [{Floor, Dir, NDst}];
update_queue([], Pos, Floor, Dir, NDst) when (Floor-Pos)*Dir < 0 -> [{Floor, (-Dir), NDst}];
update_queue([{HPos, HDir, HDst} | Tail], Pos, Floor, Dir, NDst) when (Floor-Pos)*(Floor-HPos) < 0 
                                                              andalso Dir*(Floor-Pos) > 0 -> 
    [{Floor, Dir, NDst}, {HPos, HDir, HDst} | Tail];
update_queue([{Floor, Dir, NDst} | Tail], _, Floor, Dir, NDst) -> [{Floor, Dir, NDst} | Tail];
update_queue([{HPos, HDir, HDst} | Tail], _, Floor, Dir, NDst) -> [{HPos, HDir, HDst}] ++ update_queue(Tail, HPos, Floor, Dir, NDst).