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

isTest = Bool.false

main =
    AoC.solve! {
        year: 2024,
        day: 18,
        part1: part1,
        part2: part2,
        useTestInput: isTest,
    }

MemoryType : [Safe, Corrupted]

getShortestPath : Array2D MemoryType, Index2D, List (Index2D, List Index2D), Set Index2D -> Result (List Index2D) [NoPathFound]
getShortestPath = \memorySpace, goalLocation, open, visited ->
    if List.len open == 0 then
        Err NoPathFound
    else
        currentState =
            open
            |> List.sortWith (\a, b -> Num.compare (List.len a.1) (List.len b.1))
            |> List.get 0
            |> AoCUtils.unwrap
        
        remainingOpen = List.dropIf open \x -> x == currentState
        currentPath = List.append currentState.1 currentState.0
        
        
        if currentState.0 == goalLocation then
           Ok (currentPath)
        else if Set.contains visited currentState.0 then
           getShortestPath memorySpace goalLocation remainingOpen visited
        else
            newSpots : List (Index2D, List Index2D)
            newSpots =
                [
                    (SameCol, PrevRow),
                    (NextCol, SameRow),
                    (SameCol, NextRow),
                    (PrevCol, SameRow),
                ]
                |> List.map \x -> Index2D.adjacentTo currentState.0 (Array2D.shape memorySpace) x.1 x.0
                |> List.keepOks AoCUtils.identity
                |> List.keepIf \x -> Array2D.get memorySpace x |> AoCUtils.unwrap != Corrupted
                |> List.map \x -> (x, currentPath)
            newOpen = List.concat remainingOpen newSpots
            getShortestPath memorySpace goalLocation newOpen (Set.insert visited currentState.0)

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    inputParsed : List Index2D
    inputParsed =
        input
        |> Str.trim
        |> Str.splitOn "\n"
        |> List.map \line ->
            Str.splitOn line ","
            |> List.map (\number -> Str.toU64 number |> AoCUtils.unwrap)
            |> AoCUtils.listToTuple
            |> \x -> { col: x.0, row: x.1 }

    memorySize = if isTest then { cols: 7, rows: 7 } else { cols: 71, rows: 71 }

    numberOfDrops = if isTest then 12 else 1024

    goalLocation : Index2D
    goalLocation = { row: memorySize.rows - 1, col: memorySize.cols - 1 }

    memory : Array2D MemoryType
    memory =
        Array2D.repeat Safe memorySize



    memoryAfterDrops =
        inputParsed
        |> List.takeFirst numberOfDrops
        |> List.walk memory \currentMemory, newBlock ->
            Array2D.set currentMemory newBlock Corrupted

    shortestPath = getShortestPath memoryAfterDrops goalLocation [({ row: 0, col: 0 }, [])] (Set.empty {}) |> AoCUtils.unwrap

    outputArrayBase = Array2D.map
        memoryAfterDrops
        (\x ->
            when x is
                Safe -> "."
                Corrupted -> "#"
        )

    outputArray = shortestPath
        |> List.walk outputArrayBase \state, newBlock ->
            Array2D.set state newBlock "O"

    dbgOutput =
        outputArray
        |> Array2D.joinWith "" "\n"

    dbg (dbgOutput |> Str.withPrefix "\n")


    Ok (((List.len shortestPath) - 1) |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    inputParsed : List Index2D
    inputParsed =
        input
        |> Str.trim
        |> Str.splitOn "\n"
        |> List.map \line ->
            Str.splitOn line ","
            |> List.map (\number -> Str.toU64 number |> AoCUtils.unwrap)
            |> AoCUtils.listToTuple
            |> \x -> { col: x.0, row: x.1 }

    memorySize = if isTest then { cols: 7, rows: 7 } else { cols: 71, rows: 71 }

    goalLocation : Index2D
    goalLocation = { row: memorySize.rows - 1, col: memorySize.cols - 1 }

    memory : Array2D MemoryType
    memory =
        Array2D.repeat Safe memorySize


    
    WalkState : (Array2D MemoryType, Set Index2D, Result Index2D [Nothing])

    walkUntilBlocked : WalkState, Index2D, U64 -> [Continue WalkState, Break WalkState]
    walkUntilBlocked = \(currentMemory, shortestPathSoFar, _), newBlock, index ->
        newMemory = Array2D.set currentMemory newBlock Corrupted

        if (Set.len shortestPathSoFar) == 0 || Set.contains shortestPathSoFar newBlock then
            shortestPathResult = getShortestPath newMemory goalLocation [({ row: 0, col: 0 }, [])] (Set.empty {})
            
            when shortestPathResult is
                Ok path -> Continue (newMemory, (path |> Set.fromList), Err Nothing)
                Err NoPathFound -> 
                    dbg index
                    Break (newMemory, shortestPathSoFar, Ok newBlock)
        else
            Continue (newMemory, shortestPathSoFar, Err Nothing)

    firstByteThatFailedResult = 
        inputParsed
        |> List.walkWithIndexUntil (memory, (Set.empty {}), Err Nothing) walkUntilBlocked

    firstByteThatFailed = firstByteThatFailedResult.2 |> AoCUtils.unwrap

    Ok ("$(firstByteThatFailed.col |> Num.toStr),$(firstByteThatFailed.row |> Num.toStr)")
