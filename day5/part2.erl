-module(part2).
-export([start/0]).

start() ->
	Lines = read_file_lines("day5/input.txt"),
	Almanac = process_lines(Lines),
	Result = get_min_seed_location(Almanac),
	io:format("Result: ~w~n", [Result]).

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
			Seed_Ranges = build_seed_ranges(string:tokens(lists:nth(2, string:tokens(Line, ":")), " ")),
			process_lines(Lines, dict:store(seeds, Seed_Ranges, Almanac), Current_Map_Name);
		Is_Map_Line ->
			New_Map_Name = lists:nth(1, string:tokens(Line, " ")),
			process_lines(Lines, dict:update(maps, fun (Old) -> Old ++ [New_Map_Name] end, [New_Map_Name], Almanac), New_Map_Name);
		true ->
			New_Mapping = [Mapping_Number || {Mapping_Number, _} <- [string:to_integer(Mapping_Str) || Mapping_Str <- string:tokens(Line, " ")]],
			process_lines(Lines, dict:update(Current_Map_Name, fun (Old) -> Old ++ [New_Mapping] end, [New_Mapping], Almanac), Current_Map_Name)
	end.

build_seed_ranges([]) -> [];
build_seed_ranges([Start_Id_Str, Size_Str | Rest]) ->
	{Start_Id, _} = string:to_integer(Start_Id_Str),
	{Size, _} = string:to_integer(Size_Str),
	[{Start_Id, Size}] ++ build_seed_ranges(Rest).

get_min_seed_location(Almanac) ->
	min_location_for_seed_ranges(Almanac, dict:fetch(seeds, Almanac)).

min_location_for_seed_ranges(Almanac, [Seed_Range]) ->
	find_range_min(Almanac, Seed_Range);
min_location_for_seed_ranges(Almanac, [Seed_Range | Seeds]) ->
	lists:min([find_range_min(Almanac, Seed_Range), min_location_for_seed_ranges(Almanac, Seeds)]).

find_range_min(Almanac, Seed_Range) ->
	find_range_min(Almanac, Seed_Range, dict:fetch(maps, Almanac)).
find_range_min(_, {Seed_Range_Start, _}, []) -> Seed_Range_Start;
find_range_min(Almanac, Seed_Range, [Map | Maps]) ->
	{Translated_Range, Input_Ranges_Remaining} = translate_range(Almanac, Seed_Range, Map),
	case Input_Ranges_Remaining of
		[] -> find_range_min(Almanac, Translated_Range, Maps);
		[Range] -> 
			lists:min([find_range_min(Almanac, Translated_Range, Maps), find_range_min(Almanac, Range, [Map | Maps])]);
		[Range1, Range2] -> lists:min([find_range_min(Almanac, Translated_Range, Maps), find_range_min(Almanac, Range1, [Map | Maps]), find_range_min(Almanac, Range2, [Map | Maps])])
	end.

translate_range(Almanac, Seed_Range, Map) ->
	translate_range(Seed_Range, dict:fetch(Map, Almanac)).
translate_range(Seed_Range, []) -> {Seed_Range, []};
translate_range(Seed_Range, [Mapping | Mappings]) ->
	[Dest_Start, Source_Start, Size] = Mapping,
	case intersection(Seed_Range, {Source_Start, Size}) of
		none ->
			translate_range(Seed_Range, Mappings);
		Intersection_Range ->
			{Intersection_Range_Start, Intersection_Range_Size} = Intersection_Range,
			{{Intersection_Range_Start - Source_Start + Dest_Start, Intersection_Range_Size}, difference(Seed_Range, Intersection_Range)}
	end.

intersection({Start1, Size1}, {Start2, Size2}) ->
	End1 = Start1 + Size1,
	End2 = Start2 + Size2,
	if
		End1 =< Start2 orelse Start1 >= End2 -> none;
		true ->
			Start = lists:max([Start1, Start2]),
			End = lists:min([End1, End2]),
			{ Start, End - Start }
	end.

difference({Start1, Size1}, {Start2, Size2}) ->
	End1 = Start1 + Size1,
	End2 = Start2 + Size2,
	if
		End1 =< Start2 orelse Start1 >= End2 -> [{Start1, Size1}];
		Start1 >= Start2 andalso End1 =< End2 -> [];
		Start1 < Start2 andalso End1 =< End2 -> [{Start1, Start2 - Start1}];
		Start1 >= Start2 andalso End1 > End2 -> [{End2, End1 - End2}];
		Start1 < Start2 andalso End1 > End2 -> [{Start1, Start2 - Start1}, {End2, End1 - End2}]
	end.
