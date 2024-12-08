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
    dotCharacter = "." |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0

    parsedMap = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> Array2D.fromLists (FitLongest 0)
    
    count = parsedMap
        |> Array2D.mapWithIndex \_, index ->
            checkIfInfluenced := a, U8, Index2D -> [Break Bool, Continue a]
            checkIfInfluenced = \_, element, index2 ->
                if element == dotCharacter then
                    Continue Bool.false
                else if index == index2 then
                    Continue Bool.false
                else
                    difference = {
                        row: (Num.toI64 index2.row) - (Num.toI64 index.row),
                        col: (Num.toI64 index2.col) - (Num.toI64 index.col),
                    }
                    mirroredIndex = {
                        row: Num.toU64 ((Num.toI64 index2.row) + difference.row),
                        col: Num.toU64 ((Num.toI64 index2.col) + difference.col),
                    }
                    when Array2D.get parsedMap mirroredIndex is
                        Ok v -> 
                            if v == element then
                                Break Bool.true
                            else
                                Continue Bool.false
                        Err _ ->
                            Continue Bool.false
                            
            isInfluenced = Array2D.walkUntil parsedMap Bool.false { direction: Forwards } checkIfInfluenced
            
            if isInfluenced then
                1
            else
                0
        |> Array2D.toList
        |> List.sum
    Ok (Num.toStr count)

part2 : Str -> Result Str _
part2 = \input -> 
    dotCharacter = "." |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0

    parsedMap = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.toUtf8
        |> Array2D.fromLists (FitLongest 0)

    typesOfAntenna = parsedMap |> Array2D.toList |> Set.fromList |> Set.toList |> List.dropIf \x -> x == dotCharacter

    count = parsedMap
        |> Array2D.mapWithIndex \_, index ->
            isAntinode = List.any typesOfAntenna \type ->
                antennaLocationResults = Array2D.mapWithIndex parsedMap \val, index2 ->
                    if val == type then
                        Ok index2
                    else
                        Err ""
                antennaLocations = antennaLocationResults
                    |> Array2D.toList
                    |> List.keepOks \x -> x

                antennaLocations |> List.walkUntil Bool.false \_, antenna1Location ->
                    foundAny = antennaLocations |> List.walkUntil Bool.false \_, antenna2Location ->
                        if antenna1Location == antenna2Location then
                            Continue Bool.false
                        else
                            distanceBetweenAntenna = {
                                row: (Num.toI64 antenna2Location.row) - (Num.toI64 antenna1Location.row),
                                col: (Num.toI64 antenna2Location.col) - (Num.toI64 antenna1Location.col),
                            }
                            distanceToCheckSpot = {
                                row: (Num.toI64 antenna2Location.row) - (Num.toI64 index.row),
                                col: (Num.toI64 antenna2Location.col) - (Num.toI64 index.col),
                            }

                            range = List.range { start: After -50, end: Before 50 }

                            hasIntegerFactor = range
                                |> List.walkUntil Bool.false \_, factor ->
                                    if 
                                        distanceBetweenAntenna.row * factor == distanceToCheckSpot.row &&
                                        distanceBetweenAntenna.col * factor == distanceToCheckSpot.col
                                    then
                                        Break Bool.true
                                    else
                                        Continue Bool.false
                            if hasIntegerFactor then
                                Break Bool.true
                            else
                                Continue Bool.false

                    if foundAny then
                        Break Bool.true
                    else
                        Continue Bool.false
                
            if isAntinode then
                1
            else
                0
        |> Array2D.toList
        |> List.sum
    Ok (Num.toStr count)

