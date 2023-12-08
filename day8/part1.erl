-module(part1).
-export([start/0]).

start() ->
	[Directions | Map_Lines] = read_file_lines("day8/input.txt"),
	Map = parse_map(Map_Lines),
	Num_Steps = steps_to_zzz(Directions, Map),
	io:format("Result: ~w~n", [Num_Steps]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

parse_map(Lines) ->
	parse_map(Lines, digraph:new()).
parse_map([], Map) -> Map;
parse_map([Line | Lines], Map) ->
	[Vertex, Left, Right] = string:tokens(Line, " =(),"),
	digraph:add_vertex(Map, Vertex),
	digraph:add_vertex(Map, Left),
	digraph:add_vertex(Map, Right),
	digraph:add_edge(Map, Vertex, Left, "L"),
	digraph:add_edge(Map, Vertex, Right, "R"),
	parse_map(Lines, Map).

steps_to_zzz(Directions, Map) ->
	traverse_map("AAA", Directions, Directions, Map, 0).

traverse_map("ZZZ", _, _, _, Step_Count) -> Step_Count;
traverse_map(Current_Node, [], All_Directions, Map, Step_Count) ->
	traverse_map(Current_Node, All_Directions, All_Directions, Map, Step_Count);
traverse_map(Current_Node, [Direction | Rest], All_Directions, Map, Step_Count) ->
	Edges = [digraph:edge(Map, Edge) || Edge <- digraph:out_edges(Map, Current_Node)],
	{_, _, Next_Node, _} = lists:keyfind([Direction], 4, Edges),
	traverse_map(Next_Node, Rest, All_Directions, Map, Step_Count + 1).
