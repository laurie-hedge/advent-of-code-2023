-module(part1).
-export([start/0]).

start() ->
	[Time_Line_Str, Distance_Line_Str] = read_file_lines("day6/input.txt"),
	Times = extract_number_list(Time_Line_Str),
	Distances = extract_number_list(Distance_Line_Str),
	Result = process_races(lists:zip(Times, Distances)),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

extract_number_list(Line) ->
	Parts = string:tokens(Line, " "),
	Numeric_Parts = lists:sublist(Parts, 2, length(Parts)),
	[Number || {Number, _} <- [string:to_integer(Part) || Part <- Numeric_Parts]].

process_races([]) -> 1;
process_races([{Race_Time, Current_Record} | Races]) ->
	number_of_ways(Race_Time, Current_Record, Race_Time) * process_races(Races).

number_of_ways(_, _, 0) -> 0;
number_of_ways(Race_Time, Current_Record, Time_Holding_Button) ->
	Time_Remaining = Race_Time - Time_Holding_Button,
	Distance = Time_Remaining * Time_Holding_Button,
	if
		Distance > Current_Record -> 1;
		true -> 0
	end + number_of_ways(Race_Time, Current_Record, Time_Holding_Button - 1).
