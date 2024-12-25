app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
    aocUtil: "../roc-aoc-utils/main.roc",
}

import pf.Stdout
import aocUtil.AoCUtils

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        day: 19,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

Towel : List U8
Pattern : List U8

getTowelCombination : Pattern, List Towel -> Result (List Towel) [NoCombinationFound]
getTowelCombination = \pattern, towels ->
    getValidCandidates : Pattern -> List Towel
    getValidCandidates = \nextPattern ->
        towels
        |> List.keepIf \towel ->
            numberOfStripes = List.len towel
            towel == List.sublist nextPattern { start: 0, len: numberOfStripes }

    getTowelCombinationRecursive : Pattern -> Result (List Towel) [NoCombinationFound]
    getTowelCombinationRecursive = \remainingPattern ->
        if List.len remainingPattern == 0 then
            Ok []
        else
            possibleCandidates = getValidCandidates remainingPattern
            possibleCandidates
            |> List.walkUntil (Err NoCombinationFound) \_, next ->
                recursiveResult = getTowelCombinationRecursive (remainingPattern |> List.dropFirst (List.len next))
                when recursiveResult is
                    Ok list -> Break (Ok (List.prepend list next))
                    Err _ -> Continue (Err NoCombinationFound)

    getTowelCombinationRecursive pattern

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    (towelString, patternsInput) =
        input
        |> Str.trim
        |> Str.splitOn "\n\n"
        |> AoCUtils.listToTuple

    towelOptions = towelString |> Str.splitOn "," |> List.map Str.trim |> List.map Str.toUtf8

    patterns = patternsInput |> Str.toUtf8 |> List.splitOn '\n'

    possibleDesigns =
        patterns
        |> List.dropIf \x ->
            Result.isErr (getTowelCombination x towelOptions)

    Ok (List.len possibleDesigns |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
