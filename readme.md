# Advent of Code 2023 #

## Dependencies ##

[Erlang/OTP](https://www.erlang.org/downloads)

Make

## Build and Run ##

### Overview ###

First cd into advent-of-code-2023. Next set the environment variable DAY to the number of the day, e.g. 1, 2 etc. Finally run make, specifying the target as part1 or part2. Make will both compile and run the program.

### Examples ###

Bash
```
cd advent-of-code-2023
export DAY=1
make part1
make part2
```

Powershell
```
cd advent-of-code-2023
$Env:DAY=1
make part1
make part2
```

Cmd
```
cd advent-of-code-2023
set DAY=1
make part1
make part2
```

## Notes ##

As of version 26, Erlang/OTP has a known issue breaking the Cmd terminal after running, so Bash on Linux or Powershell on Windows are suggested until this issue is resolved.
