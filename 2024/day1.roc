app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
}

import pf.Stdout
import pf.Http
import pf.Dir
import pf.File
import pf.Path
import pf.Env
import pf.Arg

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
    inputPath = Str.joinWith [executableFolder, "inputs"] "/"
    filePath = Str.joinWith [inputPath, filename] "/"

    Dir.createAll! inputPath
    
    if fileExists! filePath then
        File.readUtf8! filePath
    else
        inputText = getFromNetwork! year day
        File.writeUtf8! filePath inputText
        Task.ok inputText

main =
    year = 2024
    day = 1

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
    # split in lines
    lines = input
        |> Str.trim 
        |> Str.split "\n"
    # split on whitespace
    # convert to integers
    lineValues = lines 
        |> List.map \line -> 
            splitLine = Str.split line "   "
            when splitLine is
                [a, b] -> (Str.toI32 a |> Result.withDefault 0, Str.toI32 b |> Result.withDefault 0)
                _ -> (0, 0)


    # sort each list individually
    leftValues = lineValues |> List.map \line -> line.0
    sortedLeftValues = List.sortAsc leftValues

    rightValues = lineValues |> List.map \line -> line.1
    sortedRightValues = List.sortAsc rightValues

    # calculate differces
    differences = List.map2 sortedLeftValues sortedRightValues \a, b -> Num.absDiff a b

    # sum reduction operation
    sum = List.sum differences

    Ok (Num.toStr sum)

part2 : Str -> Result Str _
part2 = \input -> 
    # split in lines
    lines = input
        |> Str.trim 
        |> Str.split "\n"
    # split on whitespace
    # convert to integers
    lineValues = lines 
        |> List.map \line -> 
            splitLine = Str.split line "   "
            when splitLine is
                [a, b] -> (Str.toI32 a |> Result.withDefault 0, Str.toI32 b |> Result.withDefault 0)
                _ -> (0, 0)


    # sort each list individually
    leftValues = lineValues |> List.map \line -> line.0
    rightValues = lineValues |> List.map \line -> line.1
    

    # naively count amounts for each entry
    similarityScores = leftValues 
        |> List.map \leftValue -> 
            (Num.toU64 leftValue) * (rightValues |> List.countIf \rightValue -> leftValue == rightValue)
    

    # sum reduction operation
    sum = List.sum similarityScores
    Ok (Num.toStr sum)


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