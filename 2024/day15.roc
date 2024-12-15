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

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        day: 15,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

MazeObject : [Wall, Robot, Box, Empty]
DirectionInstruction : ([SameCol, PrevCol, NextCol], [SameRow, PrevRow, NextRow])

charToMazeObject : U8 -> MazeObject
charToMazeObject = \x ->
    when x is
        '@' -> Robot
        '#' -> Wall
        'O' -> Box
        '.' -> Empty
        _ -> crash "Unexpected character in maze setup"

mazeObjectToChar : MazeObject -> U8
mazeObjectToChar = \x ->
    when x is
        Robot -> '@'
        Wall -> '#'
        Box -> 'O'
        Empty -> '.'

charToDirection : U8 -> DirectionInstruction
charToDirection = \x ->
    when x is
        '^' -> (SameCol, PrevRow)
        'v' -> (SameCol, NextRow)
        '<' -> (PrevCol, SameRow)
        '>' -> (NextCol, SameRow)
        _ -> crash "Unexpected direction character"

invertDirection : DirectionInstruction -> DirectionInstruction
invertDirection = \x ->
    when x is
        (SameCol, PrevRow) -> (SameCol, NextRow)
        (SameCol, NextRow) -> (SameCol, PrevRow)
        (PrevCol, SameRow) -> (NextCol, SameRow)
        (NextCol, SameRow) -> (PrevCol, SameRow)
        _ -> crash "This direction should not be needed"

firstFreeLocationInDirection : Array2D MazeObject, Index2D, DirectionInstruction -> Result Index2D [FullyBlocked]
firstFreeLocationInDirection = \maze, currentLocation, direction ->
    when Array2D.get maze currentLocation |> AoCUtils.unwrap is
        Empty -> Ok currentLocation
        Wall -> Err FullyBlocked
        _ ->
            nextLocationCandidate = Index2D.adjacentTo currentLocation (Array2D.shape maze) direction.1 direction.0
            nextLocationCandidate
            |> Result.mapErr \_ -> FullyBlocked
            |> Result.try \nextLocation ->
                firstFreeLocationInDirection maze nextLocation direction

shiftFrom : Array2D MazeObject, Index2D, DirectionInstruction -> Array2D MazeObject
shiftFrom = \maze, currentLocation, direction ->
    when Array2D.get maze currentLocation |> AoCUtils.unwrap is
        Robot -> Array2D.set maze currentLocation Empty
        Wall -> crash "Shouldn't have called shiftFrom with these arguments"
        _ ->
            nextIndex = Index2D.adjacentTo currentLocation (Array2D.shape maze) direction.1 direction.0 |> AoCUtils.unwrap
            newMaze = Array2D.set maze currentLocation (Array2D.get maze nextIndex |> AoCUtils.unwrap)
            shiftFrom newMaze nextIndex direction

applyMoveInstruction : Array2D MazeObject, DirectionInstruction -> Array2D MazeObject
applyMoveInstruction = \currentMaze, direction ->
    robotLocation = (Array2D.findFirstIndex currentMaze \x -> x == Robot) |> AoCUtils.unwrap
    freeTargetLocationResult = firstFreeLocationInDirection currentMaze robotLocation direction
    when freeTargetLocationResult is
        Ok freeTargetLocation ->
            shiftFrom currentMaze freeTargetLocation (invertDirection direction)

        Err FullyBlocked -> currentMaze

getGpsCoordinate : MazeObject, Index2D -> U64
getGpsCoordinate = \content, index ->
    if content == Box then
        index.row * 100 + index.col
    else
        0

# convertMazeToString : Array2D MazeObject -> Str
# convertMazeToString = \maze ->
#    maze |> Array2D.map mazeObjectToChar |> Array2D.toLists |> List.map Str.fromUtf8 |> List.keepOks AoCUtils.identity |> Str.joinWith "\n"

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    (mazeSetup, directionInputs) =
        input
        |> Str.trim
        |> Str.splitOn "\n\n"
        |> AoCUtils.listToTuple

    maze =
        mazeSetup
        |> Str.toUtf8
        |> List.splitOn '\n'
        |> Array2D.fromLists (FitLongest 0)
        |> Array2D.map charToMazeObject

    directionInstructions =
        directionInputs
        |> Str.replaceEach "\n" ""
        |> Str.toUtf8
        |> List.map charToDirection

    mazeAfterWalking =
        directionInstructions
        |> List.walk maze applyMoveInstruction

    result = mazeAfterWalking |> Array2D.mapWithIndex getGpsCoordinate |> Array2D.toList |> List.sum

    Ok (result |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
