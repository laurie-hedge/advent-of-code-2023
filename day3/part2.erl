-module(part2).
-export([start/0]).

start() ->
	Input_Lines = [[$.] ++ Line ++ [$.] || Line <- read_file_lines("day3/input.txt")],
	Empty_Line = [$. || _ <- lists:nth(1, Input_Lines)],
	Lines = [Empty_Line] ++ Input_Lines ++ [Empty_Line],
	Line_Matches = [make_line_matches(Line) || Line <- Lines],
	Result = process_lines(Line_Matches),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

make_line_matches(Line) ->
	{
		Line,
		case re:run(Line, "[0-9]+", [global]) of
			{match, Digit_Matches} -> lists:flatten(Digit_Matches);
			nomatch -> []
		end,
		case re:run(Line, "\\*", [global]) of
			{match, Gear_Matches} -> lists:flatten(Gear_Matches);
			nomatch -> []
		end
	}.

process_lines(Lines) ->
	process_lines(Lines, 0).
process_lines([_, _], Running_Total) ->
	Running_Total;
process_lines([Above, Current, Below | Rest], Running_Total) ->
	{_, _, Gear_Matches} = Current,
	New_Running_Total = process_gears(Gear_Matches, Above, Current, Below, Running_Total),
	process_lines([Current, Below | Rest], New_Running_Total).

process_gears([], _, _, _, Running_Total) ->
	Running_Total;
process_gears([{Match_Index, 1} | Rest], Above, Current, Below, Running_Total) ->
	Adjacent_Parts = find_adjacent_parts(Match_Index, Above) ++
	                 find_adjacent_parts(Match_Index, Below) ++
	                 find_adjacent_parts(Match_Index, Current),
	New_Running_Total = case Adjacent_Parts of
		[First_Part_Number, Second_Part_Number] -> Running_Total + (First_Part_Number * Second_Part_Number);
		_ -> Running_Total
	end,
	process_gears(Rest, Above, Current, Below, New_Running_Total).

find_adjacent_parts(Gear_Index, {Line, Part_Matches, Gear_Matches}) ->
	Overlapping_Matches = find_adjacent_matches(Gear_Index, Part_Matches),
	[get_part_number(Match, Line) || Match <- Overlapping_Matches].

find_adjacent_matches(_, []) ->
	[];
find_adjacent_matches(Gear_Index, [{Match_Index, Match_Size} | Rest]) ->
	if
		Gear_Index >= Match_Index - 1 andalso Gear_Index =< (Match_Index + Match_Size) ->
			[{Match_Index, Match_Size}] ++ find_adjacent_matches(Gear_Index, Rest);
		true -> find_adjacent_matches(Gear_Index, Rest)
	end.

get_part_number({Match_Index, Match_Size}, Line) ->
	{Part_Number, _} = string:to_integer(string:substr(Line, Match_Index + 1, Match_Size)),
	Part_Number.
