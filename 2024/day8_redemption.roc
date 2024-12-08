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

CellEntry a : [Antenna a, Empty, AntiNode]

getIndexesOfMatchingAntenna := Array2D (CellEntry U8), CellEntry U8 -> List Index2D
getIndexesOfMatchingAntenna = \map, antenna ->
    antennaIndexesMapped := Array2D (Result Index2D [NotCorrectFrequency])
    antennaIndexesMapped = Array2D.mapWithIndex map \cell, index ->
        if cell == antenna then
            Ok index
        else 
            Err NotCorrectFrequency

    antennaIndexes = antennaIndexesMapped |> Array2D.toList |> List.keepOks \x -> x
    antennaIndexes

vectorBetween := Index2D, Index2D -> { row: I64, col: I64 }
vectorBetween = \fromAntenna, toAntenna ->
    { 
        col: Num.toI64 toAntenna.col - Num.toI64 fromAntenna.col, 
        row: Num.toI64 toAntenna.row - Num.toI64 fromAntenna.row,
    }

unique := List a -> List a where a implements Hash & Eq
unique = \list -> list |> Set.fromList |> Set.toList

flatten := List (List a) -> List a
flatten = \nestedList ->
    nestedList |> List.walk [] \a, b -> List.concat a b

getDistinctPairs := List a -> List (a, a) where a implements Eq
getDistinctPairs = \list ->
    (list |> List.map \v1 ->
        list |> List.map \v2 ->
            if v1 == v2 then
                Err SameEntry
            else
                Ok (v1, v2)
    ) 
    |> flatten
    |> List.keepOks \x -> x


main =
    year = 2024
    day = 8

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
    
## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input -> 
    emptyCharacter = "." |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0
    antiNodeCharacter = "#" |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0

    parsedMap := Array2D (CellEntry U8)
    parsedMap = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> Array2D.fromLists (FitLongest 0)
        |> Array2D.map \x ->
            if x == emptyCharacter then
                Empty
            else if x == antiNodeCharacter then
                AntiNode
            else
                Antenna x
    
    antennaFrequencies := List (CellEntry U8)
    antennaFrequencies = parsedMap
        |> Array2D.toList
        |> List.keepIf \x ->
            when x is
                Antenna _ -> Bool.true
                _ -> Bool.false
        |> unique

    allAntennaPairs = antennaFrequencies
        |> List.map \antenna -> 
            antennaCoordinates = getIndexesOfMatchingAntenna parsedMap antenna
            
            getDistinctPairs antennaCoordinates
        |> flatten

    antiNodeCoordinates = allAntennaPairs |> List.map \pair ->
        distance = vectorBetween pair.0 pair.1
        antiNodeCoordinate = {
            row: Num.toU64Checked (Num.toI64 pair.1.row + distance.row),
            col: Num.toU64Checked (Num.toI64 pair.1.col + distance.col),
        }
        when antiNodeCoordinate is
            { row: Ok row, col: Ok col } -> Ok { row, col }
            _ -> Err OutOfBounds

    count = antiNodeCoordinates
        |> List.keepOks \x -> x
        |> unique
        |> List.keepIf \x -> Array2D.hasIndex parsedMap x
        |> List.len
    
    Ok (Num.toStr count)

part2 : Str -> Result Str _
part2 = \input -> 
    emptyCharacter = "." |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0
    antiNodeCharacter = "#" |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0

    parsedMap := Array2D (CellEntry U8)
    parsedMap = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> Array2D.fromLists (FitLongest 0)
        |> Array2D.map \x ->
            if x == emptyCharacter then
                Empty
            else if x == antiNodeCharacter then
                AntiNode
            else
                Antenna x

    antennaFrequencies := List (CellEntry U8)
    antennaFrequencies = parsedMap
        |> Array2D.toList
        |> List.keepIf \x ->
            when x is
                Antenna _ -> Bool.true
                _ -> Bool.false
        |> unique

    allAntennaPairs = antennaFrequencies
        |> List.map \antenna -> 
            antennaCoordinates = getIndexesOfMatchingAntenna parsedMap antenna
            
            getDistinctPairs antennaCoordinates
        |> flatten

    getAntiNodes := Array2D (CellEntry U8), Index2D, { row: I64, col: I64 } -> List Index2D
    getAntiNodes = \map, currentIndex, offsetDistance ->
        nextAntiNodeCoordinate = {
            row: Num.toU64Checked (Num.toI64 currentIndex.row + offsetDistance.row),
            col: Num.toU64Checked (Num.toI64 currentIndex.col + offsetDistance.col),
        }
        when nextAntiNodeCoordinate is
            { row: Ok row, col: Ok col } ->
                index = { row, col } 
                if Array2D.hasIndex map index then
                    List.append (getAntiNodes map index offsetDistance) {
                        row: Num.toU64 index.row,
                        col: Num.toU64 index.col,
                    }
                else
                    [] # out of bounds -> end recursion
            _ -> [] # out of bounds - end recursion

    antiNodeCoordinates = allAntennaPairs 
        |> List.map \pair ->
            distance = vectorBetween pair.0 pair.1
            getAntiNodes parsedMap pair.0 distance
        |> flatten
        |> unique

    count = antiNodeCoordinates |> List.len

    Ok (Num.toStr count)