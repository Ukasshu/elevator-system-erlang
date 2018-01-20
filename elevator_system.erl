-module(elevator_system).
-compile(export_all).


start(Amount, FloorRanges) when is_integer(Amount)
						   andalso is_list(FloorRanges)
						   andalso length(FloorRanges) == Amount 
						   %andalso ((lists:flatlength(FloorRanges)) == (2 * Amount)) 
						   %- mysle ze trzeba ten warunek jakos przemyslec zeby sprawdzac czy lista ma poprawny format
						   ->
	MainPID = spawn_link(?MODULE, managing_init, [Amount, FloorRanges]), 
	MainPID!xD,
	put(managing_process, MainPID),
	{x}.

	

managing_init(Amount, FloorRanges) ->  % wydaje mi sie ze mozna wyniesc ja wyzej do procesu glownego
	PIDs = generate_elevators(Amount, FloorRanges, []),
	put(elevators, PIDs),
	put(amount, Amount),
	put(ranges, FloorRanges),
	io:write(get(elevators)),
	lists:foreach(fun(X) -> X!{manager, self()}, io:fwrite("sent")	end ,PIDs),
	get_all_elevators_ready(Amount),
	managing_loop().




managing_loop() -> ok.

managing_end() -> ok.

elevator_init(Lowest, Highest) when Lowest < Highest -> 
	CyclePID  = spawn_link(?MODULE, elevator_cycle_init, [self()]).

elevator_loop() -> ok.

elevator_end() -> ok.

elevator_cycle_init(ElevatorPID) -> ok.

elevator_cycle_loop() -> ok.

elevator_cycle_end() -> ok.

drawing_init() -> ok.

drawing_loop() -> ok.

drawing_end() -> ok.

random_floor_init() -> ok.

random_floor_loop() -> ok.

random_floor_end() -> ok.

get_all_elevators_ready(Amount) ->
	receive
		{ready, PID} -> get_all_elevators_ready(Amount-1)
	end.


generate_elevators(0, [], PIDs) when is_list(PIDs) -> 
	PIDs;
generate_elevators(Amount, [FirstRange | RestOfRanges], PIDs) when is_integer(Amount)
										andalso is_list(FirstRange)
										andalso is_list(RestOfRanges) % bez sensu chyba bo w zapisie [H|T] T to zawsze lista 
										andalso is_list(PIDs)
										->
	generate_elevators(Amount-1, RestOfRanges, PIDs ++ [spawn_link(?MODULE, elevator_init, FirstRange)]).