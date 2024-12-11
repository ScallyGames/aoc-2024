app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
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

    sessionToken =
        getSessionToken!
        |> Str.trim

    header = Http.header "Cookie" "session=$(sessionToken)"

    request = { Http.defaultRequest & url: "https://adventofcode.com/$(Num.toStr year)/day/$(Num.toStr day)/input", headers: [header] }

    result =
        Http.send request
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
    # filename = Str.joinWith ["testInput", padded, ".txt"] ""
    inputPath = Str.joinWith [executableFolder, "inputs"] "/"
    filePath = Str.joinWith [inputPath, filename] "/"

    Dir.createAll! inputPath

    if fileExists! filePath then
        File.readUtf8! filePath
    else
        inputText = getFromNetwork! year day
        File.writeUtf8! inputText filePath
        Task.ok inputText

sendAnswer = \year, day, part, solution ->
    sessionToken =
        getSessionToken!
        |> Str.trim

    cookieHeader = Http.header "Cookie" "session=$(sessionToken)"
    contentTypeHeader = Http.header "Content-Type" "application/x-www-form-urlencoded"

    body = "level=$(Num.toStr part)&answer=$(solution)"

    request = { Http.defaultRequest &
        url: "https://adventofcode.com/$(Num.toStr year)/day/$(Num.toStr day)/answer",
        headers: [cookieHeader, contentTypeHeader],
        method: Post,
        body: Str.toUtf8 body,
    }

    result =
        Http.send request
        |> Task.result!

    responseText = Result.try result Http.handleStringResponse
    dbg responseText
    Task.ok {}

main =
    year = 2024
    day = 10

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

digitStringToNumbers : Str -> List U8
digitStringToNumbers = \line ->
    line
    |> Str.toUtf8
    |> List.map
        (\x -> Str.fromUtf8 [x]
            |> Result.try Str.toU8
        )
    |> List.keepOks \x -> x

getTrailEnds : Array2D U8, Index2D -> List Index2D
getTrailEnds = \map, currentLocation ->
    currentHeight = map |> Array2D.get currentLocation |> Result.withDefault 9
    if currentHeight == 9 then
        [currentLocation]
    else
        possibleNextLocations =
            Index2D.allAdjacentTo currentLocation (Array2D.shape map)
            |> List.keepIf \x -> x.row == currentLocation.row || x.col == currentLocation.col
            |> List.keepIf \possibleLocation ->
                heightAtLocation = Array2D.get map possibleLocation |> Result.withDefault 0
                heightAtLocation == currentHeight + 1

        possibleNextLocations |> List.joinMap \possibleLocation -> getTrailEnds map possibleLocation

unique : List a -> List a where a implements Hash & Eq
unique = \list -> list |> Set.fromList |> Set.toList

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    inputMap =
        input
        |> Str.trim
        |> Str.splitOn "\n"
        |> List.map digitStringToNumbers
        |> Array2D.fromLists (FitLongest 0)

    scores =
        inputMap
        |> Array2D.mapWithIndex \value, index ->
            if value == 0 then
                getTrailEnds inputMap index
                |> unique
                |> List.len
            else
                0

    totalScore = scores |> Array2D.toList |> List.sum

    Ok (Num.toStr totalScore)

part2 : Str -> Result Str _
part2 = \input ->
    inputMap =
        input
        |> Str.trim
        |> Str.splitOn "\n"
        |> List.map digitStringToNumbers
        |> Array2D.fromLists (FitLongest 0)

    scores =
        inputMap
        |> Array2D.mapWithIndex \value, index ->
            if value == 0 then
                getTrailEnds inputMap index
                |> List.len
            else
                0

    totalScore = scores |> Array2D.toList |> List.sum

    Ok (Num.toStr totalScore)