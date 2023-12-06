-module(part2).
-export([start/0]).

start() ->
	[Time_Line_Str, Distance_Line_Str] = read_file_lines("day6/input.txt"),
	Time = extract_number(Time_Line_Str),
	Distance = extract_number(Distance_Line_Str),
	Result = number_of_ways(Time, Distance, Time),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

extract_number(Line) ->
	Parts = string:tokens(Line, " "),
	Numeric_Part = lists:flatten(lists:sublist(Parts, 2, length(Parts))),
	{Number, _} = string:to_integer(Numeric_Part),
	Number.

number_of_ways(_, _, 0) -> 0;
number_of_ways(Race_Time, Current_Record, Time_Holding_Button) ->
	Time_Remaining = Race_Time - Time_Holding_Button,
	Distance = Time_Remaining * Time_Holding_Button,
	if
		Distance > Current_Record -> 1;
		true -> 0
	end + number_of_ways(Race_Time, Current_Record, Time_Holding_Button - 1).
