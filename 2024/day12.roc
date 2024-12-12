app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        day: 12,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

unwrap : Result a _ -> a
unwrap = \x ->
    when x is
        Ok a -> a
        Err _ -> crash "This should not ever happen"

floodFillRegion : Array2D U8, { region : { size : U64, perimiter : U64 }, visited : Array2D Bool }, Index2D, U8 -> { region : { size : U64, perimiter : U64 }, visited : Array2D Bool }
floodFillRegion = \field, state, currentLocation, fieldType ->
    if !(Array2D.hasIndex field currentLocation) then
        state
    else if Array2D.get state.visited currentLocation |> unwrap then
        state
    else
        neighborsOfSameType =
            [
                Index2D.adjacentTo currentLocation (Array2D.shape field) PrevRow SameCol,
                Index2D.adjacentTo currentLocation (Array2D.shape field) NextRow SameCol,
                Index2D.adjacentTo currentLocation (Array2D.shape field) SameRow NextCol,
                Index2D.adjacentTo currentLocation (Array2D.shape field) SameRow PrevCol,
            ]
            |> List.keepOks \x -> x
            |> List.keepIf \x -> (Array2D.get field x |> unwrap) == fieldType

        areaIncrease = 1
        perimiterIncrease = 4 - (List.len neighborsOfSameType)
        newVisited = Array2D.set state.visited currentLocation Bool.true
        newState = {
            region: {
                size: state.region.size + areaIncrease,
                perimiter: state.region.perimiter + perimiterIncrease,
            },
            visited: newVisited,
        }

        neighborsOfSameType
        |> List.walk
            newState
            \accumulatingState, neighborLocation -> floodFillRegion field accumulatingState neighborLocation fieldType

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    fieldGrid = input |> Str.trim |> Str.toUtf8 |> List.splitOn '\n' |> Array2D.fromLists (FitLongest 0)

    { regions } =
        fieldGrid
        |> Array2D.walk
            {
                regions: [],
                visited: Array2D.repeat Bool.false (Array2D.shape fieldGrid),
            }
            { direction: Forwards }
            \state, currentField, currentLocation ->
                if state.visited |> Array2D.get currentLocation |> unwrap then
                    state
                else
                    { region, visited: newVisited } = floodFillRegion fieldGrid { region: { size: 0, perimiter: 0 }, visited: state.visited } currentLocation currentField
                    {
                        regions: List.append state.regions region,
                        visited: newVisited,
                    }

    totalCost =
        regions
        |> List.map \region -> region.size * region.perimiter
        |> List.sum

    Ok (totalCost |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
