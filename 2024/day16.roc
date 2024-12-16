app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
    aocUtils: "../roc-aoc-utils/main.roc",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import aocUtils.AoCUtils

import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]
import array2d.Shape2D exposing [Shape2D]

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        day: 16,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

MazeContent : [Empty, Wall, Start, End]
Direction : [Up, Down, Left, Right]

charToMazeContent : U8 -> MazeContent
charToMazeContent = \x ->
    when x is
        '#' -> Wall
        '.' -> Empty
        'S' -> Start
        'E' -> End
        _ -> crash "Unexpected char in input"

directionTupleToDirection = \direction ->
    when direction is
        (SameCol, PrevRow) -> Up
        (NextCol, SameRow) -> Right
        (SameCol, NextRow) -> Down
        (PrevCol, SameRow) -> Left
        _ -> crash "Unexpected direction"

directionToDirectionTuple = \direction ->
    when direction is
        Up -> (SameCol, PrevRow)
        Right -> (NextCol, SameRow)
        Down -> (SameCol, NextRow)
        Left -> (PrevCol, SameRow)

moveStraight : Index2D, Direction, Shape2D -> Result (Index2D, Direction) [PathBlocked]
moveStraight = \currentLocation, currentDirection, shape ->
    directionTuple = currentDirection |> directionToDirectionTuple
    Index2D.adjacentTo currentLocation shape directionTuple.1 directionTuple.0
    |> Result.mapErr \_ -> PathBlocked
    |> Result.map \newLocation -> (newLocation, currentDirection)

rotateLeft : Index2D, Direction, Shape2D -> Result (Index2D, Direction) []
rotateLeft = \currentLocation, currentDirection, _ ->
    when currentDirection is
        Up -> Ok (currentLocation, Left)
        Right -> Ok (currentLocation, Up)
        Down -> Ok (currentLocation, Right)
        Left -> Ok (currentLocation, Down)

rotateRight : Index2D, Direction, Shape2D -> Result (Index2D, Direction) []
rotateRight = \currentLocation, currentDirection, _ ->
    when currentDirection is
        Up -> Ok (currentLocation, Right)
        Right -> Ok (currentLocation, Down)
        Down -> Ok (currentLocation, Left)
        Left -> Ok (currentLocation, Up)

# basically a recursive implementation of a BFS that unwraps the while loop and priority queue into a recursive function call
getLowestCostToGoal : Array2D MazeContent, List (Index2D, Direction, U64), Set (Index2D, Direction) -> Result U64 [NoPathFound]
getLowestCostToGoal = \maze, open, visited ->
    if List.len open == 0 then
        Err NoPathFound
    else
        currentState = open |> List.sortWith (\a, b -> Num.compare a.2 b.2) |> List.get 0 |> AoCUtils.unwrap

        remainingOpen = List.dropIf open \x -> x == currentState

        if Array2D.get maze currentState.0 |> AoCUtils.unwrap == End then
            Ok currentState.2
        else if Set.contains visited (currentState.0, currentState.1) then
            getLowestCostToGoal maze remainingOpen visited
        else
            possibleActions = [
                (moveStraight, 1),
                (rotateLeft, 1000),
                (rotateRight, 1000),
            ]

            validActions =
                List.map possibleActions \action ->
                    Result.map (action.0 currentState.0 currentState.1 (Array2D.shape maze)) \x -> (x.0, x.1, action.1)
                |> List.keepOks \x -> x
                |> List.dropIf \(nextLocation, _) -> (Array2D.get maze nextLocation |> AoCUtils.unwrap) == Wall

            newOpen =
                validActions
                |> List.map \(nextLocation, nextDirection, nextCost) -> (nextLocation, nextDirection, currentState.2 + nextCost)
                |> List.concat remainingOpen

            getLowestCostToGoal maze newOpen (Set.insert visited (currentState.0, currentState.1))

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    maze =
        input
        |> Str.trim
        |> Str.toUtf8
        |> List.splitOn '\n'
        |> Array2D.fromLists (FitLongest 0)
        |> Array2D.map charToMazeContent

    startLocation = (Array2D.findFirstIndex maze \x -> x == Start) |> AoCUtils.unwrap

    lowestCost = getLowestCostToGoal maze [(startLocation, Right, 0)] (Set.empty {})

    lowestCost |> Result.map \x -> Num.toStr x

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
