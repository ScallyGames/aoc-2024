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

OutsideDirection := [Up, Down, Left, Right] implements [Eq, Hash, Inspect]
FencePart : (Index2D, OutsideDirection)

AccumulatingState : { region : { size : U64, perimeterParts : List FencePart }, visited : Array2D Bool }

floodFillRegion : Array2D U8, AccumulatingState, Index2D, U8 -> AccumulatingState
floodFillRegion = \field, state, currentLocation, fieldType ->
    if !(Array2D.hasIndex field currentLocation) then
        state
    else if Array2D.get state.visited currentLocation |> unwrap then
        state
    else
        neighborsOfSameType =
            [
                (@OutsideDirection Up, Index2D.adjacentTo currentLocation (Array2D.shape field) PrevRow SameCol),
                (@OutsideDirection Down, Index2D.adjacentTo currentLocation (Array2D.shape field) NextRow SameCol),
                (@OutsideDirection Right, Index2D.adjacentTo currentLocation (Array2D.shape field) SameRow NextCol),
                (@OutsideDirection Left, Index2D.adjacentTo currentLocation (Array2D.shape field) SameRow PrevCol),
            ]
            |> List.keepOks \x ->
                when x is
                    (a, Ok b) -> Ok (a, b)
                    _ -> Err OutOfBounds
            |> List.keepIf \x -> (Array2D.get field x.1 |> unwrap) == fieldType

        areaIncrease = 1
        perimeterParts : List FencePart
        perimeterParts =
            [
                (currentLocation, @OutsideDirection Up),
                (currentLocation, @OutsideDirection Down),
                (currentLocation, @OutsideDirection Right),
                (currentLocation, @OutsideDirection Left),
            ]
            |> List.dropIf \x -> List.any neighborsOfSameType \y -> y.0 == x.1

        newVisited = Array2D.set state.visited currentLocation Bool.true

        newState : AccumulatingState
        newState = {
            region: {
                size: state.region.size + areaIncrease,
                perimeterParts: List.concat state.region.perimeterParts perimeterParts,
            },
            visited: newVisited,
        }

        neighborsOfSameType
        |> List.walk
            newState
            \accumulatingState, neighborLocation -> floodFillRegion field accumulatingState neighborLocation.1 fieldType

countFenceSegments : List FencePart -> U64
countFenceSegments = \fencePartList ->
    initialDictionary : Dict OutsideDirection (List FencePart)
    initialDictionary = Dict.empty {}
    fencesByDirection =
        fencePartList
        |> List.walk initialDictionary \state, fencePart ->
            addToListAtKey : Result (List a) [Missing] -> Result (List a) [Missing]
            addToListAtKey = \current ->
                when current is
                    Ok list -> Ok (List.append list fencePart)
                    Err Missing -> Ok [fencePart]
            currentKey : OutsideDirection
            currentKey = fencePart.1
            Dict.update state currentKey addToListAtKey

    fencesByDirection
    |> Dict.map \direction, fenceListInDireciton ->
        when direction is
            @OutsideDirection Up ->
                sortedInDirection =
                    fenceListInDireciton
                    |> List.sortWith \a, b ->
                        Num.compare (a.0.row * 5128652167 + a.0.col) (b.0.row * 5128652167 + b.0.col)
                tuples = List.map2 sortedInDirection (List.dropFirst sortedInDirection 1) \a, b -> (a, b)
                tuples
                |> List.walk 1 \state, x ->
                    if x.1.0.row == x.0.0.row && x.1.0.col == x.0.0.col + 1 then
                        state
                    else
                        state + 1

            @OutsideDirection Down ->
                sortedInDirection =
                    fenceListInDireciton
                    |> List.sortWith \a, b ->
                        Num.compare (a.0.row * 5128652167 + a.0.col) (b.0.row * 5128652167 + b.0.col)
                tuples = List.map2 sortedInDirection (List.dropFirst sortedInDirection 1) \a, b -> (a, b)
                tuples
                |> List.walk 1 \state, x ->
                    if x.1.0.row == x.0.0.row && x.1.0.col == x.0.0.col + 1 then
                        state
                    else
                        state + 1

            @OutsideDirection Left ->
                sortedInDirection =
                    fenceListInDireciton
                    |> List.sortWith \a, b ->
                        Num.compare (a.0.col * 5128652167 + a.0.row) (b.0.col * 5128652167 + b.0.row)
                tuples = List.map2 sortedInDirection (List.dropFirst sortedInDirection 1) \a, b -> (a, b)
                tuples
                |> List.walk 1 \state, x ->
                    if x.1.0.col == x.0.0.col && x.1.0.row == x.0.0.row + 1 then
                        state
                    else
                        state + 1

            @OutsideDirection Right ->
                sortedInDirection =
                    fenceListInDireciton
                    |> List.sortWith \a, b ->
                        Num.compare (a.0.col * 5128652167 + a.0.row) (b.0.col * 5128652167 + b.0.row)
                tuples = List.map2 sortedInDirection (List.dropFirst sortedInDirection 1) \a, b -> (a, b)
                tuples
                |> List.walk 1 \state, x ->
                    if x.1.0.col == x.0.0.col && x.1.0.row == x.0.0.row + 1 then
                        state
                    else
                        state + 1
    |> Dict.values
    |> List.sum

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
                    initialState : AccumulatingState
                    initialState = { region: { size: 0, perimeterParts: [] }, visited: state.visited }
                    { region, visited: newVisited } = floodFillRegion fieldGrid initialState currentLocation currentField
                    {
                        regions: List.append state.regions region,
                        visited: newVisited,
                    }

    totalCost =
        regions
        |> List.map \region -> region.size * (List.len region.perimeterParts)
        |> List.sum

    Ok (totalCost |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
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
                    initialState : AccumulatingState
                    initialState = { region: { size: 0, perimeterParts: [] }, visited: state.visited }
                    { region, visited: newVisited } = floodFillRegion fieldGrid initialState currentLocation currentField
                    {
                        regions: List.append state.regions region,
                        visited: newVisited,
                    }

    totalCost =
        regions
        |> List.map \region ->
            fenceSegments = countFenceSegments region.perimeterParts
            region.size * fenceSegments
        |> List.sum

    Ok (totalCost |> Num.toStr)
