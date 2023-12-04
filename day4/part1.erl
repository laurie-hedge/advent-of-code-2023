-module(part1).
-export([start/0]).

start() ->
	Lines = read_file_lines("day4/input.txt"),
	Result = process_lines(Lines),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines([]) ->
	0;
process_lines([Line | Lines]) ->
	[_, Winning_Numbers_Str, Card_Numbers_Str] = string:tokens(Line, ":|"),
	Winning_Numbers = [Winning_Number || {Winning_Number, _} <- [string:to_integer(Winning_Number_Str) || Winning_Number_Str <- string:tokens(Winning_Numbers_Str, " ")]],
	Card_Numbers = [Card_Number || {Card_Number, _} <- [string:to_integer(Card_Number_Str) || Card_Number_Str <- string:tokens(Card_Numbers_Str, " ")]],
	Not_Matched_Numbers = Card_Numbers -- Winning_Numbers,
	Matched_Numbers = Card_Numbers -- Not_Matched_Numbers,
	card_score(length(Matched_Numbers)) + process_lines(Lines).

card_score(0) -> 0;
card_score(1) -> 1;
card_score(Num_Matches_Remaining) ->
	card_score(Num_Matches_Remaining - 1) * 2.
