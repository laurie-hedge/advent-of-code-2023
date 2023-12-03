-module(part1).
-export([start/0]).

start() ->
	Input_Lines = [[$.] ++ Line ++ [$.] || Line <- read_file_lines("day3/input.txt")],
	Empty_Line = [$. || _ <- lists:nth(1, Input_Lines)],
	Lines = [Empty_Line] ++ Input_Lines ++ [Empty_Line],
	Result = process_lines(Lines),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines(Lines) ->
	process_lines(Lines, 0).
process_lines([_, _], Running_Total) ->
	Running_Total;
process_lines([Above, Current, Below | Rest], Running_Total) ->
	{match, Matches} = re:run(Current, "[0-9]+", [global]),
	Match_List = lists:flatten(Matches),
	New_Running_Total = process_matches(Match_List, Above, Current, Below, Running_Total),
	process_lines([Current, Below | Rest], New_Running_Total).

process_matches([], _, _, _, Running_Total) ->
	Running_Total;
process_matches([{Match_Index, Match_Size} | Rest], Above, Current, Below, Running_Total) ->
	Part_Number_Str = string:substr(Current, Match_Index + 1, Match_Size),
	{Part_Number, _} = string:to_integer(Part_Number_Str),
	Char_Before = string:substr(Current, Match_Index, 1),
	Char_After = string:substr(Current, Match_Index + Match_Size + 1, 1),
	Chars_Above = string:substr(Above, Match_Index, Match_Size + 2),
	Chars_Below = string:substr(Below, Match_Index, Match_Size + 2),
	Adjacent_Characters = Char_Before ++ Char_After ++ Chars_Above ++ Chars_Below,
	Num_Adjacent_Symbols = length([Char || Char <- Adjacent_Characters, is_symbol(Char)]),
	New_Running_Total = if
		Num_Adjacent_Symbols > 0 -> Running_Total + Part_Number;
		true -> Running_Total
	end,
	process_matches(Rest, Above, Current, Below, New_Running_Total).

is_symbol(Char) ->
	case re:run([Char], "[0-9.]", [global]) of
		nomatch -> true;
		_ -> false
	end.
