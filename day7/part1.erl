-module(part1).
-export([start/0]).

start() ->
	Lines = read_file_lines("day7/input.txt"),
	Hands = process_lines(Lines),
	Sorted_Hands = lists:sort(fun (Hand1, Hand2) -> compare_hands(Hand1, Hand2) end, Hands),
	Result = score_hands(Sorted_Hands, 1),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines([]) -> [];
process_lines([Line | Lines]) ->
	[Hand, Bid_Str] = string:tokens(Line, " "),
	{Bid, _} = string:to_integer(Bid_Str),
	[{Hand, Bid}] ++ process_lines(Lines).

compare_hands({Hand1_Cards, _}, {Hand2_Cards, _}) ->
	Hand1_Type = hand_type(Hand1_Cards),
	Hand2_Type = hand_type(Hand2_Cards),
	if
		Hand1_Type < Hand2_Type -> true;
		Hand1_Type > Hand2_Type -> false;
		Hand1_Type == Hand2_Type -> compare_card_strengths(lists:zip(Hand1_Cards, Hand2_Cards))
	end.

hand_type(Hand_Cards) ->
	Card_Counts = lists:sort(count_cards(Hand_Cards)),
	case Card_Counts of
		[5] -> 6;
		[1, 4] -> 5;
		[2, 3] -> 4;
		[1, 1, 3] -> 3;
		[1, 2, 2] -> 2;
		[1, 1, 1, 2] -> 1;
		[1, 1, 1, 1, 1] -> 0
	end.

count_cards(Hand_Cards) ->
	Card_Count_Dict = count_cards(Hand_Cards, dict:new()),
	[dict:fetch(Card, Card_Count_Dict) || Card <- dict:fetch_keys(Card_Count_Dict)].
count_cards([], Card_Count_Dict) -> Card_Count_Dict;
count_cards([Card | Cards], Card_Count_Dict) ->
	count_cards(Cards, dict:update_counter(Card, 1, Card_Count_Dict)).

compare_card_strengths([]) -> true;
compare_card_strengths([{Card1, Card2} | Cards]) ->
	Card1_Value = card_value(Card1),
	Card2_Value = card_value(Card2),
	if
		Card1_Value < Card2_Value -> true;
		Card1_Value > Card2_Value -> false;
		Card1_Value == Card2_Value -> compare_card_strengths(Cards)
	end.

card_value(Card) ->
	case Card of
		$A -> 12;
		$K -> 11;
		$Q -> 10;
		$J -> 9;
		$T -> 8;
		Char -> Char - $2
	end.

score_hands([], _) -> 0;
score_hands([{_, Bid} | Hands], Rank) ->
	(Rank * Bid) + score_hands(Hands, Rank + 1).
