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
	MainPID = spawn_link(?MODULE, managing_init, [Amount, [LowestFloor, HighestFloor], true]), 
	put(managing_process, MainPID),
	loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
loop() ->loop().

managing_init(Amount, FloorRange, RandomOn) when is_integer(Amount)
										 andalso is_list(FloorRange)
										 andalso is_boolean(RandomOn)
											->  % wydaje mi sie ze mozna wyniesc ja wyzej do procesu glownego
	DrawingPID = spawn_link(?MODULE, drawing_init, [Amount, FloorRange]),
	PIDs = generate_elevators(Amount, 1, FloorRange, DrawingPID, []),
	put(elevators, PIDs),
	put(amount, Amount),
	put(range, FloorRange),
	io:write(get(elevators)),
	lists:foreach(fun(X) -> X!{manager, self()}, io:fwrite("sent")	end ,PIDs),
	get_all_elevators_ready(Amount),
	if
		RandomOn ->
			io:write(si),
			RandomPID = spawn_link(?MODULE, random_floor_init, [self(), FloorRange])
	end,
	managing_loop([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO: dodac obsluge podzialu zadan/ mowienia windzie gdzie jechac
managing_loop(List) when is_list(List) -> 
	receive
		{random, Floor, Direction} ->
			io:write(Floor),
 			managing_loop(List ++ [{Floor, Direction}] );
 		{get, ok} -> ok;
 		the_end -> managing_end()
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
managing_end() -> 
	"send to all the_end".

elevator_init(OrdinalNumber, Lowest, Highest, ManagingPID, DrawingPID) when Lowest < Highest
																	andalso is_integer(OrdinalNumber)
																	andalso is_integer(Lowest)
																	andalso is_integer(Highest)
                                                                       -> 
	CyclePID  = spawn_link(?MODULE, elevator_cycle_init, [self(), DrawingPID, OrdinalNumber]),
	put(cycle_pid, CyclePID),
	put(manager_pid, ManagingPID),
	ManagingPID!{ready, self()},
	elevator_loop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
elevator_loop(Position, Direction) -> 
	receive
		{updateState, Position, Direction}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
elevator_end() -> 
	get(cycle_pid)!the_end.

elevator_cycle_init(ElevatorPID, DrawingPID, OrdinalNumber) -> 
	put(elevator_pid, ElevatorPID),
	put(drawing_pid, DrawingPID),
	put(ord, OrdinalNumber),
	elevator_cycle_loop_idle().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO : podzielić na stany windy - otwarta; w ruchu; idle
elevator_cycle_loop_idle(Position) -> 
	receive
		{move, Dir} ->
			elevator_cycle_loop_move(Dir, Dest);
		{open, Dir} ->
			ok
	end.

elevator_cycle_loop_move(Dir, Dest, Position) ->
	receive
	after
		3000 ->
			if 
				Dest =/= Position + Dir ->
					get(elevator_pid)!{updateState, Position + Dir},
					get(drawing_pid)!{updated, get(ord), Position, Position+Dir},
					elevator_cycle_loop_move(Dir, Dest, Position + Dir);
				Dest == Position + Dir ->
					get(elevator_pid)!{updateState, Position + Dir},
					get(drawing_pid)!{updated, get(ord), Position, Position+Dir},
					elevator_cycle_loop_open(Dir, Dest, Position + Dir)
			end
	end.

elevator_cycle_loop_open(Dir, Dest, Position) ->
	receive
	after
		3000 -> 
			elevator_cycle_loop_move(Dir, Dest, Position) %trzeba chyba jakos od nowa policzyc
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
elevator_cycle_end() -> ok.

drawing_init(Amount, Range) -> 
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%TODO
drawing_end() -> ok.


random_floor_init(ManagerPID, Range) ->
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
			get(manager_pid)!{random, 5, d},
			random_floor_loop() % trzeba ogarnac jak wybierac pietra z zakresow
	end.

random_floor_end() -> io:fwrite("random_floor_end").

get_all_elevators_ready(0) -> ok;
get_all_elevators_ready(Amount) ->
	receive
		{ready, PID} -> get_all_elevators_ready(Amount-1)
	end.


generate_elevators(0, _, _, _, PIDs) when is_list(PIDs) -> 
	PIDs;
generate_elevators(Amount, Ord, Range, DrawingPID, PIDs) when is_integer(Amount)
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

draw_update(Ord, OldPos, NewPos) ->
	[Low, High] = get(range),
	print({printxy, 6*Ord-1, 6+High-OldPos, 'X'}),
	print({printxy, 6*Ord-1, 6+High-NewPos, 'O'}).
