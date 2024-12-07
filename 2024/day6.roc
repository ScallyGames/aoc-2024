app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import pf.Stdout
import pf.Http
import pf.Dir
import pf.File
import pf.Path
import pf.Env
import pf.Arg

import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]

getExecutableFolder = 
    args = Arg.list! {} # {} is necessary as a temporary workaround
    exePath = List.get args 0 |> Result.withDefault ""
    exePathPrefixed = Str.concat "./" exePath
    { before: exePathFolder } = Str.splitLast exePathPrefixed "/" |> Result.withDefault { before: exePathPrefixed, after: "" }
    cwd = Env.cwd! |> Path.display
    Task.ok (Str.joinWith [cwd, exePathFolder] "/")

    ## This should be sufficient but is currently broken with `roc run`
    ## see https://github.com/roc-lang/basic-cli/issues/265
    # exePath = Env.exePath!
    # exePathStr = Path.display exePath
    # dbg exePathStr


leftPad = \input, padding, numChars ->
    paddingSequence = Str.repeat padding numChars
    padded = Str.concat paddingSequence input
    Str.toUtf8 padded |> List.takeLast numChars |> Str.fromUtf8

getSessionToken = 
    home = Env.var! "HOME"
    (Str.joinWith [home, ".aoc_session"] "/")
        |> File.readUtf8!


getFromNetwork = \year, day ->

    sessionToken = getSessionToken!
        |> Str.trim

    header = Http.header "Cookie" "session=$(sessionToken)"

    request = { Http.defaultRequest & url: "https://adventofcode.com/$(Num.toStr year)/day/$( Num.toStr day)/input" , headers: [header] }

    result = Http.send request
        |> Task.result!

    responseText = Result.try result Http.handleStringResponse

    when responseText is
        Ok inputText -> 
            Task.ok inputText
        Err err -> Task.err err

# convert errors to false
fileExists = \filePath ->
    result = File.isFile filePath |> Task.result!
    when result is
        Ok val -> Task.ok val
        Err _ -> Task.ok Bool.false

getInput = \year, day ->
    executableFolder = getExecutableFolder!

    padded = (leftPad (Num.toStr day) "0" 2) |> Result.withDefault ""
    filename = Str.joinWith ["input", padded, ".txt"] ""
    #filename = Str.joinWith ["testInput", padded, ".txt"] ""
    inputPath = Str.joinWith [executableFolder, "inputs"] "/"
    filePath = Str.joinWith [inputPath, filename] "/"

    Dir.createAll! inputPath
    
    if fileExists! filePath then
        File.readUtf8! filePath
    else
        inputText = getFromNetwork! year day
        File.writeUtf8! filePath inputText
        Task.ok inputText


sendAnswer = \year, day, part, solution ->
    sessionToken = getSessionToken!
        |> Str.trim

    cookieHeader = Http.header "Cookie" "session=$(sessionToken)"
    contentTypeHeader = Http.header "Content-Type" "application/x-www-form-urlencoded"

    body = "level=$(Num.toStr part)&answer=$(solution)"

    request = { Http.defaultRequest & 
        url: "https://adventofcode.com/$(Num.toStr year)/day/$( Num.toStr day)/answer" , 
        headers: [cookieHeader, contentTypeHeader], 
        method: Post, 
        body: Str.toUtf8 body
    }

    result = Http.send request
        |> Task.result!
    
    responseText = Result.try result Http.handleStringResponse
    dbg responseText

    Task.ok {}

main =
    year = 2024
    day = 6

    input = getInput! year day


    part1Result = part1 input
    part1Task = 
        when part1Result is 
            Ok str -> 
                Stdout.line "Part 1: $(str)"
                # sendAnswer! year day 1 str
            Err _ -> Stdout.line "Something went wrong in part 1"
    part1Task!


    part2Result = part2 input
    part2Task = 
        when part2Result is 
            Ok str -> Stdout.line "Part 2: $(str)"
            Err _ -> Stdout.line "Something went wrong in part 2"
    part2Task!

resultsInALoop := Array2D [Obstacle, Empty], Index2D, (I64, I64), List (Index2D, (I64, I64)) -> Bool
resultsInALoop = \map, currentPosition, currentDirection, visited ->
    paramState = (currentPosition, currentDirection)
    if List.contains visited paramState then
        Bool.true
    else
        if !(Array2D.hasIndex map currentPosition) then
            Bool.false
        else
            nextPositionCandidate = { 
                col: Num.toU64 ((Num.toI64 currentPosition.col) + currentDirection.0), 
                row: Num.toU64 ((Num.toI64 currentPosition.row) + currentDirection.1),
            }
            (nextPosition, nextDirection) = 
                if !(Array2D.hasIndex map nextPositionCandidate) then
                    (nextPositionCandidate, currentDirection)
                else if Array2D.get map nextPositionCandidate == Ok Obstacle then
                    (currentPosition, (rotateRightMath currentDirection))
                else
                    (nextPositionCandidate, currentDirection)
            newVisited = 
                if currentDirection == nextDirection then
                    visited
                else
                    (List.append visited paramState)
            resultsInALoop map nextPosition nextDirection newVisited

walkPath := Array2D [Obstacle, Empty], Index2D, (I64, I64) -> Array2D Bool
walkPath = \map, currentPosition, currentDirection ->
    if !(Array2D.hasIndex map currentPosition) then
        Array2D.repeat Bool.false (Array2D.shape map)
    else
        nextPositionCandidate = { 
            col: Num.toU64 ((Num.toI64 currentPosition.col) + currentDirection.0), 
            row: Num.toU64 ((Num.toI64 currentPosition.row) + currentDirection.1),
        }
        (nextPosition, nextDirection) = 
            if !(Array2D.hasIndex map nextPositionCandidate) then
                (nextPositionCandidate, currentDirection)
            else if Array2D.get map nextPositionCandidate == Ok Obstacle then
                (currentPosition, (rotateRightMath currentDirection))
            else
                (nextPositionCandidate, currentDirection)
        resultMap = walkPath map nextPosition nextDirection
        Array2D.set resultMap currentPosition Bool.true


rotateRightPattern := (I64, I64) -> (I64, I64)
rotateRightPattern = \direction ->
    when direction is
        (0, -1) -> (1, 0)
        (1, 0) -> (0, 1)
        (0, 1) -> (-1, 0)
        (-1, 0) -> (0, -1)
        _ -> (0, 0)

rotateRightMath := (I64, I64) -> (I64, I64)
rotateRightMath = \direction ->
    (-direction.1, direction.0)

expect
    initialDirection = (0, -1)
    expectedDirection = (1, 0)
    actualDirection = rotateRightMath initialDirection

    actualDirection == expectedDirection


expect
    initialDirection = (1, 0)
    expectedDirection = (0, 1)
    actualDirection = rotateRightMath initialDirection

    actualDirection == expectedDirection


expect
    initialDirection = (0, 1)
    expectedDirection = (-1, 0)
    actualDirection = rotateRightMath initialDirection

    actualDirection == expectedDirection


expect
    initialDirection = (-1, 0)
    expectedDirection = (0, -1)
    actualDirection = rotateRightMath initialDirection

    actualDirection == expectedDirection


## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input -> 
    obstacleSign = 0x23 # "#"
    startSign =    0x5E # "^"

    characterMap = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \x -> Str.toUtf8 x
        |> Array2D.fromLists (FitLongest 0)

    map = characterMap |> Array2D.map \x -> if x == obstacleSign then Obstacle else Empty

    startPositionMatch = characterMap |> Array2D.findFirstIndex \x -> x == startSign
    when startPositionMatch is
        Ok startPosition ->
            walkResult = walkPath map startPosition (0, -1)
            # dbg walkResult |> Array2D.map (\x -> if x then "X" else ".") |> Array2D.toLists
            spotsVisited = walkResult |> Array2D.countIf \x -> x
            Ok (Num.toStr spotsVisited)
        Err _ -> crash "Couldn't find start position"

part2 : Str -> Result Str _
part2 = \input ->     
    obstacleSign = 0x23 # "#"
    startSign =    0x5E # "^"

    characterMap = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \x -> Str.toUtf8 x
        |> Array2D.fromLists (FitLongest 0)

    map = characterMap |> Array2D.map \x -> if x == obstacleSign then Obstacle else Empty

    startPositionMatch = characterMap |> Array2D.findFirstIndex \x -> x == startSign
    when startPositionMatch is
        Ok startPosition ->

            possibleNewObstaclePositions =  map |> Array2D.mapWithIndex \value, index ->
                # dbg index
                if index == startPosition then
                    Bool.false
                else if value == Obstacle then  
                    Bool.false
                else
                    resultsInALoop (Array2D.set map index Obstacle) startPosition (0, -1) []

            numberOfObstacleOptions = possibleNewObstaclePositions |> Array2D.countIf \x -> x

            Ok (Num.toStr numberOfObstacleOptions)
        Err _ -> crash "Couldn't find start position"

