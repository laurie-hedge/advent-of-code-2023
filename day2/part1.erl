-module(part1).
-export([start/0]).

start() ->
	Lines = read_file_lines("day2/input.txt"),
	Result = process_lines(Lines),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines(Lines) ->
	process_lines(Lines, 0).
process_lines([], Sum_Of_Game_Ids) ->
	Sum_Of_Game_Ids;
process_lines([Line|Lines], Sum_Of_Game_Ids) ->
	[Game_Part, Results_Part_Str] = string:tokens(Line, ":"),
	["Game", Game_Id_Str] = string:tokens(Game_Part, " "),
	{Game_Id, _} = string:to_integer(Game_Id_Str),
	Results_Parts = string:tokens(Results_Part_Str, " ,;"),
	process_lines(Lines, new_sum_of_game_ids(Sum_Of_Game_Ids, Game_Id, get_max_cubes_by_colour(Results_Parts))).

get_max_cubes_by_colour(Results_Parts) ->
	get_max_cubes_by_colour(Results_Parts, {0, 0, 0}).
get_max_cubes_by_colour([], Current_Max) ->
	Current_Max;
get_max_cubes_by_colour([Num_Cubes_Str, Cube_Colour|Rest], Current_Max) ->
	{Num_Cubes, _} = string:to_integer(Num_Cubes_Str),
	{Max_Red, Max_Green, Max_Blue} = Current_Max,
	New_Max = case Cube_Colour of
		"red" -> { lists:max([Max_Red, Num_Cubes]), Max_Green, Max_Blue };
		"green" -> { Max_Red, lists:max([Max_Green, Num_Cubes]), Max_Blue };
		"blue" -> { Max_Red, Max_Green, lists:max([Max_Blue, Num_Cubes]) }
	end,
	get_max_cubes_by_colour(Rest, New_Max).

new_sum_of_game_ids(Sum_Of_Game_Ids, Game_Id, {Max_Red, Max_Green, Max_Blue}) ->
	Red_Threshold = 12,
	Green_Threshold = 13,
	Blue_Threshold = 14,
	Possible_Result = Max_Red =< Red_Threshold andalso Max_Green =< Green_Threshold andalso Max_Blue =< Blue_Threshold,
	if
		Possible_Result -> Sum_Of_Game_Ids + Game_Id;
		true -> Sum_Of_Game_Ids
	end.
