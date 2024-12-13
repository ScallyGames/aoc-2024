app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
    aocUtils: "../roc-aoc-utils/main.roc",
    linearAlgebra: "https://github.com/Hasnep/roc-linear-algebra/releases/download/v0.2.0/r1qHVwbBCy0fed0Ox3lqpiFOkPJQHCpJfGzRXZJWV8c.tar.br",
}

import pf.Stdout
import linearAlgebra.Vector2

import aoc.AoC {
    stdoutLine: Stdout.line,
}
import aocUtils.AoCUtils

main =
    AoC.solve! {
        year: 2024,
        day: 13,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

Machine : { a : Vector2.Vector2, b : Vector2.Vector2, prizeLocation : Vector2.Vector2 }

mapMachine : Str -> Result Machine [InvalidNumberOfLines]
mapMachine = \machineDescription ->
    entries = machineDescription |> Str.splitOn "\n"
    when entries is
        [aString, bString, prize] ->
            a =
                aString
                |> Str.replaceEach "Button A: X" ""
                |> Str.replaceEach " Y" ""
                |> Str.splitOn ","
                |> List.map Str.toF64
                |> List.keepOks \x -> x
                |> Vector2.fromList
                |> AoCUtils.unwrap
            b =
                bString
                |> Str.replaceEach "Button B: X" ""
                |> Str.replaceEach " Y" ""
                |> Str.splitOn ","
                |> List.map Str.toF64
                |> List.keepOks \x -> x
                |> Vector2.fromList
                |> AoCUtils.unwrap
            prizeLocation =
                prize
                |> Str.replaceEach "Prize: X=" ""
                |> Str.replaceEach " Y=" ""
                |> Str.splitOn ","
                |> List.map Str.toF64
                |> List.keepOks \x -> x
                |> Vector2.fromList
                |> AoCUtils.unwrap
            Ok { a, b, prizeLocation }

        _ -> Err InvalidNumberOfLines

getLowestCost : Machine -> Result U64 [PrizeUnobtainable]
getLowestCost = \machine ->
    aPressCost = 3
    bPressCost = 1
    buttonPressCosts = (aPressCost, bPressCost)
    cost =
        List.range { start: At 0, end: At 100 }
        |> List.walk (Err PrizeUnobtainable) \minCost, aPresses ->
            List.range { start: At 0, end: At 100 }
            |> List.walk minCost \minCostInner, bPresses ->
                clawLocation = Vector2.add (Vector2.elementwiseMul (Num.toF64 aPresses, Num.toF64 aPresses) machine.a) (Vector2.elementwiseMul (Num.toF64 bPresses, Num.toF64 bPresses) machine.b)
                permutationCost = Vector2.dot buttonPressCosts (aPresses, bPresses)
                if Vector2.isApproxEq clawLocation machine.prizeLocation {} then
                    when minCostInner is
                        Ok currentMinCost -> Ok (Num.min currentMinCost permutationCost)
                        Err _ -> Ok permutationCost
                else
                    minCostInner

    cost |> Result.map Num.round

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    machines =
        input
        |> Str.trim
        |> Str.splitOn "\n\n"
        |> List.map mapMachine
        |> List.keepOks \x -> x

    totalCost =
        machines
        |> List.map getLowestCost
        |> List.keepOks \x -> x
        |> List.sum

    Ok (totalCost |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
