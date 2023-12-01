-module(part2).
-export([start/0]).

start() ->
	Lines = read_file_lines("day1/input.txt"),
	Result = process_lines(Lines),
	io:format("Result: ~w~n", [Result]).

read_file_lines(File_Name) ->
	{ok, Data} = file:read_file(File_Name),
	Chars = lists:flatten(binary_to_list(Data)),
	string:tokens(Chars, "\n").

process_lines(Lines) ->
	process_lines(Lines, 0).
process_lines([], Running_Total) ->
	Running_Total;
process_lines([Line|Lines], Running_Total) ->
	Regex_Digit_Strings = "one|two|three|four|five|six|seven|eight|nine",
	Regex_String_Forwards = "[1-9]|" ++ Regex_Digit_Strings,
	Regex_String_Backwards = "[1-9]|" ++ string:reverse(Regex_Digit_Strings),
	Rev_Line = string:reverse(Line),
	{match, [{First_Index, First_Size}|_]} = re:run(Line, Regex_String_Forwards),
	{match, [{Last_Index, Last_Size}|_]} = re:run(Rev_Line, Regex_String_Backwards),
	First_Digit_Str = string:substr(Line, First_Index + 1, First_Size),
	Last_Digit_Str = string:reverse(string:substr(Rev_Line, Last_Index + 1, Last_Size)),
	Digit_Str = string_to_digit(First_Digit_Str) ++ string_to_digit(Last_Digit_Str),
	{Digit, _} = string:to_integer(Digit_Str),
	process_lines(Lines, Running_Total + Digit).

string_to_digit(String) ->
	Len = length(String),
	case String of
		Digit when Len == 1 -> Digit;
		"one" -> "1";
		"two" -> "2";
		"three" -> "3";
		"four" -> "4";
		"five" -> "5";
		"six" -> "6";
		"seven" -> "7";
		"eight" -> "8";
		"nine" -> "9"
	end.
