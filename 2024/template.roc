app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
}

import pf.Stdout

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        # TODO: set correct date
        day: 0,
        part1: part1,
        part2: part2,
    }

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    Ok ""

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
