-module(part2).
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
	Card_Counts = count_cards(Hand_Cards),
	case Card_Counts of
		[5] -> 6;
		[4, 1] -> 5;
		[3, 2] -> 4;
		[3, 1, 1] -> 3;
		[2, 2, 1] -> 2;
		[2, 1, 1, 1] -> 1;
		[1, 1, 1, 1, 1] -> 0
	end.

count_cards(Hand_Cards) ->
	Card_Count_Dict = count_cards(Hand_Cards, dict:new()),
	{Updated_Card_Count_Dict, Num_Jokers} = extract_joker(Card_Count_Dict),
	case lists:reverse(lists:sort([dict:fetch(Card, Updated_Card_Count_Dict) || Card <- dict:fetch_keys(Updated_Card_Count_Dict)])) of
		[Highest | Rest] -> [Highest + Num_Jokers | Rest];
		[] -> [Num_Jokers]
	end.

count_cards([], Card_Count_Dict) -> Card_Count_Dict;
count_cards([Card | Cards], Card_Count_Dict) ->
	count_cards(Cards, dict:update_counter(Card, 1, Card_Count_Dict)).

extract_joker(Card_Count_Dict) ->
	case dict:is_key($J, Card_Count_Dict) of
		true -> {dict:erase($J, Card_Count_Dict), dict:fetch($J, Card_Count_Dict)};
		false -> {Card_Count_Dict, 0}
	end.

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
		$J -> -1;
		$T -> 8;
		Char -> Char - $2
	end.

score_hands([], _) -> 0;
score_hands([{_, Bid} | Hands], Rank) ->
	(Rank * Bid) + score_hands(Hands, Rank + 1).
