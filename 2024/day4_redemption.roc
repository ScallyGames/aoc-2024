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
    day = 4

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

matrixGet := List List U8, Int s, Int s -> Result U8 [OutOfBounds]
matrixGet = \matrix, column, row ->
    if column < 0 || row < 0 then
        Err OutOfBounds
    else
        rowList = List.get? matrix (Num.toU64 row)
        value = List.get rowList (Num.toU64 column)
        value

countWordMatches := List (List U8), Int s, Int s, List U8 -> U64
countWordMatches = \matrix, column, row, searchWord ->
    # We could check offset 0 here if it's X and fast exit if not
    # Then we could rework wordCheckRange to start: At 1
    directions = [
        ( 1i64,  0i64),
        (-1i64,  0i64),
        ( 0i64,  1i64),
        ( 0i64, -1i64),
        ( 1i64,  1i64),
        ( 1i64, -1i64),
        (-1i64,  1i64),
        (-1i64, -1i64),
    ]

    wordCheckRange = List.range { start: At 0, end: Before (List.len searchWord) } |> List.map Num.toI64


    directions |> List.countIf \direction ->
        wordCheckRange |> List.all \offset ->
            expected = List.get searchWord (Num.toU64 offset) |> Result.withDefault 0
            columnIndex = (Num.toI64 column) + direction.0 * offset
            rowIndex = (Num.toI64 row) + direction.1 * offset
            found = matrixGet matrix columnIndex rowIndex
            
            when found is
                Ok character -> expected == character
                Err _ -> Bool.false

countWordMatchesX = \matrix, column, row, searchWord ->
    # We could check offset 0 here if it's A and fast exit if not
    directions = [
        ( 1i64,  1i64),
        ( 1i64, -1i64),
        (-1i64,  1i64),
        (-1i64, -1i64),
    ]

    wordCheckRange = List.range { start: At 0, end: Before (List.len searchWord) } |> List.map Num.toI64

    halfWordDistance = Num.floor ((Num.toFrac (List.len searchWord)) / 2)

    numberOfMatches = directions |> List.countIf \direction ->
        wordCheckRange |> List.all \offset ->
            expected = List.get searchWord (Num.toU64 offset) |> Result.withDefault 0
            columnIndex = (Num.toI64 column) - direction.0 * halfWordDistance + direction.0 * offset
            rowIndex = (Num.toI64 row) - direction.1 * halfWordDistance + direction.1 * offset
            found = matrixGet matrix columnIndex rowIndex
            
            when found is
                Ok character -> expected == character
                Err _ -> Bool.false

    if numberOfMatches == 2 then    
        1
    else
        0


flatten := List (List a) -> List a
flatten = \nestedList ->
    nestedList |> List.walk [] \a, b -> List.concat a b
                

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input -> 
    searchWord = "XMAS" |> Str.toUtf8

    inputAsArray = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \x -> Str.toUtf8 x

    counts = inputAsArray
        |> List.mapWithIndex 
            \row, rowIndex ->
                List.mapWithIndex row \_, columnIndex ->
                    countWordMatches inputAsArray columnIndex rowIndex searchWord


    Ok (Num.toStr (counts |> flatten |> List.sum))

part2 : Str -> Result Str _
part2 = \input -> 
    searchWord = "MAS" |> Str.toUtf8

    inputAsArray = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \x -> Str.toUtf8 x

    counts = inputAsArray
        |> List.mapWithIndex 
            \row, rowIndex ->
                List.mapWithIndex row \_, columnIndex ->
                    countWordMatchesX inputAsArray columnIndex rowIndex searchWord


    Ok (Num.toStr (counts |> flatten |> List.sum))
