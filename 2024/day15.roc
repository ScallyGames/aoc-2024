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

MazeObject : [Wall, Robot, Box, Empty, WideBoxLeft, WideBoxRight]
DirectionInstruction : ([SameCol, PrevCol, NextCol], [SameRow, PrevRow, NextRow])

charToMazeObject : U8 -> MazeObject
charToMazeObject = \x ->
    when x is
        '@' -> Robot
        '#' -> Wall
        'O' -> Box
        '.' -> Empty
        _ -> crash "Unexpected character in maze setup"

# mazeObjectToChar : MazeObject -> U8
# mazeObjectToChar = \x ->
#    when x is
#        Robot -> '@'
#        Wall -> '#'
#        Box -> 'O'
#        Empty -> '.'
#        WideBoxLeft -> '['
#        WideBoxRight -> ']'

charToDirection : U8 -> DirectionInstruction
charToDirection = \x ->
    when x is
        '^' -> (SameCol, PrevRow)
        'v' -> (SameCol, NextRow)
        '<' -> (PrevCol, SameRow)
        '>' -> (NextCol, SameRow)
        _ -> crash "Unexpected direction character"

widen : MazeObject -> List MazeObject
widen = \x ->
    when x is
        Robot -> [Robot, Empty]
        Wall -> [Wall, Wall]
        Box -> [WideBoxLeft, WideBoxRight]
        Empty -> [Empty, Empty]
        _ -> crash "Cannot widen this element"

getGpsCoordinate : MazeObject, Index2D -> U64
getGpsCoordinate = \content, index ->
    if content == Box || content == WideBoxLeft then
        index.row * 100 + index.col
    else
        0

push : Array2D MazeObject, Index2D, DirectionInstruction, Bool -> Result (Array2D MazeObject) [FullyBlocked]
push = \maze, currentLocation, direction, isAttachedCall ->
    currentValue = Array2D.get maze currentLocation |> AoCUtils.unwrap
    when currentValue is
        Wall -> Err FullyBlocked
        Empty -> Ok maze
        _ ->
            nextIndexCandidate =
                Index2D.adjacentTo currentLocation (Array2D.shape maze) direction.1 direction.0
                |> Result.mapErr \_ -> FullyBlocked

            nextResult = nextIndexCandidate |> Result.try \nextIndex -> push maze nextIndex direction Bool.false

            ownResult = Result.map2 nextIndexCandidate nextResult \nextIndex, newMap ->
                Array2D.swap newMap nextIndex currentLocation

            if (currentValue != WideBoxLeft && currentValue != WideBoxRight) || isAttachedCall || direction.1 == SameRow then
                ownResult
            else
                offsetDirection =
                    when currentValue is
                        WideBoxLeft -> NextCol
                        WideBoxRight -> PrevCol
                        _ -> crash "This should be prevented by if before"

                attachedIndex = Index2D.adjacentTo currentLocation (Array2D.shape maze) SameRow offsetDirection |> AoCUtils.unwrap
                ownResult |> Result.try \newMap -> push newMap attachedIndex direction Bool.true

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
        |> List.map \row -> List.map row charToMazeObject
        |> Array2D.fromLists (FitLongest Wall)

    directionInstructions =
        directionInputs
        |> Str.replaceEach "\n" ""
        |> Str.toUtf8
        |> List.map charToDirection

    mazeAfterWalking =
        directionInstructions
        |> List.walk maze \currentMaze, direction ->
            robotLocation = (Array2D.findFirstIndex currentMaze \x -> x == Robot) |> AoCUtils.unwrap
            when push currentMaze robotLocation direction Bool.false is
                Ok newMaze -> newMaze
                Err FullyBlocked -> currentMaze

    result = mazeAfterWalking |> Array2D.mapWithIndex getGpsCoordinate |> Array2D.toList |> List.sum

    Ok (result |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    (mazeSetup, directionInputs) =
        input
        |> Str.trim
        |> Str.splitOn "\n\n"
        |> AoCUtils.listToTuple

    maze =
        mazeSetup
        |> Str.toUtf8
        |> List.splitOn '\n'
        |> List.map \row -> List.joinMap row \char -> widen (charToMazeObject char)
        |> Array2D.fromLists (FitLongest Wall)

    directionInstructions =
        directionInputs
        |> Str.replaceEach "\n" ""
        |> Str.toUtf8
        |> List.map charToDirection

    mazeAfterWalking =
        directionInstructions
        |> List.walk maze \currentMaze, direction ->
            robotLocation = (Array2D.findFirstIndex currentMaze \x -> x == Robot) |> AoCUtils.unwrap
            when push currentMaze robotLocation direction Bool.false is
                Ok newMaze -> newMaze
                Err FullyBlocked -> currentMaze

    result = mazeAfterWalking |> Array2D.mapWithIndex getGpsCoordinate |> Array2D.toList |> List.sum

    Ok (result |> Num.toStr)
