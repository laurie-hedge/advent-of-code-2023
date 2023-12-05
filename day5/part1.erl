-module(part1).
-export([start/0]).

start() ->
	Lines = read_file_lines("day5/input.txt"),
	Almanac = process_lines(Lines),
	Seed_Locations = get_seed_locations(Almanac),
	io:format("Result: ~w~n", [lists:min(Seed_Locations)]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines(Lines) ->
	process_lines(Lines, dict:new(), "").
process_lines([], Almanac, _) ->
	Almanac;
process_lines([Line | Lines], Almanac, Current_Map_Name) ->
	Is_Seeds_Line = string:str(Line, "seeds:") > 0,
	Is_Map_Line = string:str(Line, "map:") > 0,
	if
		Is_Seeds_Line ->
			Seed_List = [Seed_Id || {Seed_Id, _} <- [string:to_integer(Seed_String) || Seed_String <- string:tokens(lists:nth(2, string:tokens(Line, ":")), " ")]],
			process_lines(Lines, dict:store(seeds, Seed_List, Almanac), Current_Map_Name);
		Is_Map_Line ->
			New_Map_Name = lists:nth(1, string:tokens(Line, " ")),
			process_lines(Lines, dict:update(maps, fun (Old) -> Old ++ [New_Map_Name] end, [New_Map_Name], Almanac), New_Map_Name);
		true ->
			New_Mapping = [Mapping_Number || {Mapping_Number, _} <- [string:to_integer(Mapping_Str) || Mapping_Str <- string:tokens(Line, " ")]],
			process_lines(Lines, dict:update(Current_Map_Name, fun (Old) -> Old ++ [New_Mapping] end, [New_Mapping], Almanac), Current_Map_Name)
	end.

get_seed_locations(Almanac) ->
	get_seed_locations(Almanac, dict:fetch(seeds, Almanac)).
get_seed_locations(Almanac, []) -> [];
get_seed_locations(Almanac, [Seed | Seeds]) ->
	[map_seed_to_location(Seed, Almanac)] ++ get_seed_locations(Almanac, Seeds).

map_seed_to_location(Seed, Almanac) ->
	lists:foldl(fun (Map_Name, Input_Id) -> translate_id(Input_Id, dict:fetch(Map_Name, Almanac)) end, Seed, dict:fetch(maps, Almanac)).

translate_id(Input_Id, []) -> Input_Id;
translate_id(Input_Id, [Mapping | Mappings]) ->
	[Dest_Start, Source_Start, Size] = Mapping,
	if
		Input_Id >= Source_Start andalso Input_Id < (Source_Start + Size) ->
			Input_Id - Source_Start + Dest_Start;
		true ->
			translate_id(Input_Id, Mappings)
	end.
