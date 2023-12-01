-module(part1).
-export([start/0]).

start() ->
	Lines = read_file_lines("day1/input.txt"),
	Result = process_lines(Lines),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines(Lines) ->
	process_lines(Lines, 0).
process_lines([], Running_Total) ->
	Running_Total;
process_lines([Line|Lines], Running_Total) ->
	{match, Matches} = re:run(Line, "[0-9]", [global]),
	Match_List = lists:flatten(Matches),
	{First_Index, 1} = lists:nth(1, Match_List),
	{Last_Index, 1} = lists:nth(length(Match_List), Match_List),
	Digit_Str = [lists:nth(First_Index + 1, Line), lists:nth(Last_Index + 1, Line)],
	{Digit, _} = string:to_integer(Digit_Str),
	process_lines(Lines, Running_Total + Digit).
