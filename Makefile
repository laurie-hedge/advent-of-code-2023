part1.beam: day${DAY}/part1.erl
	erlc day${DAY}/part1.erl

part2.beam: day${DAY}/part2.erl
	erlc day${DAY}/part2.erl

.PHONY: part1
part1: part1.beam
	erl -noshell -s part1 start -s init stop

.PHONY: part2
part2: part2.beam
	erl -noshell -s part2 start -s init stop

.PHONY: clean
clean:
	rm -f *.beam *.dump
