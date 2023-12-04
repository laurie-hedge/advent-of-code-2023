-module(part2).
-export([start/0]).

start() ->
	Lines = read_file_lines("day4/input.txt"),
	Result = process_lines(Lines),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines(Lines) ->
	process_lines(Lines, dict:new()).
process_lines([], _) ->
	0;
process_lines([Line | Lines], Copies_Dictionary) ->
	[Card_Str, Winning_Numbers_Str, Card_Numbers_Str] = string:tokens(Line, ":|"),
	["Card", Card_Id_Str] = string:tokens(Card_Str, " "),
	{Card_Id, _} = string:to_integer(Card_Id_Str),
	Num_Copies = case dict:is_key(Card_Id, Copies_Dictionary) of
		true -> dict:fetch(Card_Id, Copies_Dictionary) + 1;
		false -> 1
	end,
	Winning_Numbers = [Winning_Number || {Winning_Number, _} <- [string:to_integer(Winning_Number_Str) || Winning_Number_Str <- string:tokens(Winning_Numbers_Str, " ")]],
	Card_Numbers = [Card_Number || {Card_Number, _} <- [string:to_integer(Card_Number_Str) || Card_Number_Str <- string:tokens(Card_Numbers_Str, " ")]],
	Not_Matched_Numbers = Card_Numbers -- Winning_Numbers,
	Num_Matched_Numbers = length(Card_Numbers -- Not_Matched_Numbers),
	Updated_Dictionary = update_copies_dictionary(Card_Id, Num_Matched_Numbers, Num_Copies, Copies_Dictionary),
	Num_Copies + process_lines(Lines, Updated_Dictionary).

update_copies_dictionary(_, 0, _, Copies_Dictionary) ->
	Copies_Dictionary;
update_copies_dictionary(Card_Id, Num_Matched_Numbers, Num_Copies, Copies_Dictionary) ->
	Updated_Card_Id = Card_Id + 1,
	Updated_Dictionary = dict:update_counter(Updated_Card_Id, Num_Copies, Copies_Dictionary),
	update_copies_dictionary(Updated_Card_Id, Num_Matched_Numbers - 1, Num_Copies, Updated_Dictionary).
