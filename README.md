# Advent of Code 2022

Solutions for ❄️[Advent of Code 2022]❄️ in [Haskell]

| Su   | Mo   | Tu   | We   | Th   | Fr   | Sa   |
| ---- | ---- | ---- | ---- | ---- | ---- | ---- |
|      |      |      |      | 1 ⭐ | 2 ⭐ | 3    |
| 4    | 5    | 6    | 7    | 8    | 9    | 10   |
| 11   | 12   | 13   | 14   | 15   | 16   | 17   |
| 18   | 19   | 20   | 21   | 22   | 23   | 24   |
| 25   | 26   | 27   | 28   | 29   | 30   | 31   |

## Help

### Using stack directly

This project uses the [`hspec`][hspec] test runner as its entry point,
so the solutions are written as tests. Pass `--help` to the executable
with `stack run` or `stack run advent2022` to see options available for
running and filtering tests:

```console
$ stack run -- --help

Usage: advent2022 [OPTION]...

OPTIONS
              --help              display this help and exit
              --ignore-dot-hspec  do not read options from ~/.hspec and .hspec
  -m PATTERN  --match=PATTERN     only run examples that match given PATTERN
              --skip=PATTERN      skip examples that match given PATTERN

RUNNER OPTIONS
        --[no-]dry-run          pretend that everything passed; don't verify
                                anything
...etc.
```

### Using make

Alternatively, use `make solve` to run all solutions or
`make run day=N part=N` to run a specific solution. Run `make` without
a target to see the other available targets:

```console
$ make
Usage
solve                 Test all solutions
run                   Test a solution for one day by setting [day=N] [part=N]
opt                   Build optimized
build                 Build unoptimized
update                Update compiler, dependencies, and tools
update.stack          Set up the compiler and project dependencies
update.tools          Install additional tooling
clean                 Clean project
```

[Advent of Code 2022]: https://adventofcode.com/2022
[Haskell]: https://www.haskell.org
[hspec]: https://hspec.github.io
