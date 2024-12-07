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

countMatches = \list, searchWord ->
    searchWordLength = List.len searchWord
    countResult = List.walk list { slidingBuffer: List.repeat 0 searchWordLength, count: 0 } \state, nextChar -> 
        slidingBuffer = List.append state.slidingBuffer nextChar
            |> List.takeLast searchWordLength
        newState = { state & slidingBuffer }
        if slidingBuffer == searchWord then 
            { newState & count: state.count + 1 }
        else
            newState
    countResult.count

matrixGet := List List U8, Int s, Int s -> Result U8 [OutOfBounds]
matrixGet = \matrix, column, row ->
    if column < 0 || row < 0 then
        Err OutOfBounds
    else
        rowList = List.get? matrix (Num.toU64 row)
        value = List.get rowList (Num.toU64 column)
        value

transpose = \matrix ->
    columnCount = List.get matrix 0 |> Result.withDefault [] |> List.len
    rowCount = List.len matrix
    
    newRowRange = List.range { start: At 0, end: Before columnCount }
    newColumnRange = List.range { start: At 0, end: Before rowCount }

    newRowRange 
        |> List.map \newRowIndex ->
            newColumnRange
                |> List.map \newColumnIndex ->
                    matrix |> matrixGet newRowIndex newColumnIndex |> Result.withDefault 0

expect
    testMatrix = [
        [1, 2, 3],
        [4, 5, 6],
    ]

    expectedResult = [
        [1, 4],
        [2, 5],
        [3, 6],
    ]

    actualResult = transpose testMatrix

    actualResult == expectedResult
    

diagonalizePositive = \matrix ->
    columnCount = List.get matrix 0 |> Result.withDefault [] |> List.len |> Num.toI64
    rowCount = List.len matrix |> Num.toI64
    
    offsetRange = List.range { start: After -rowCount, end: Before columnCount }
    newColumnRange = List.range { start: At 0, end: Before rowCount }

    offsetRange
        |> List.map \offset ->
            newColumnRange
                |> List.map \newColumnIndex ->
                    matrixGet matrix newColumnIndex (offset + newColumnIndex) |> Result.withDefault 0

expect
    testMatrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
    ]

    expectedResult = [
        [0, 0, 3],
        [0, 2, 6],
        [1, 5, 9],
        [4, 8, 0],
        [7, 0, 0],
    ]

    actualResult = diagonalizePositive testMatrix

    actualResult == expectedResult


diagonalizeNegative = \matrix ->
    columnCount = List.get matrix 0 |> Result.withDefault [] |> List.len |> Num.toI64
    rowCount = List.len matrix |> Num.toI64
    
    offsetRange = List.range { start: After -rowCount, end: Before columnCount }
    newColumnRange = List.range { start: At 0, end: Before rowCount }

    offsetRange
        |> List.map \offset ->
            newColumnRange
                |> List.map \newColumnIndex ->
                    matrixGet matrix newColumnIndex (offset + (columnCount - 1 - newColumnIndex)) |> Result.withDefault 0

expect
    testMatrix = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
    ]

    expectedResult = [
        [1, 0, 0],
        [4, 2, 0],
        [7, 5, 3],
        [0, 8, 6],
        [0, 0, 9],
    ]

    actualResult = diagonalizeNegative testMatrix

    actualResult == expectedResult

convertToUtf8 := List (List Str) -> List (List U8)
convertToUtf8 = \matrix ->
    matrix |> List.map \line ->
        line |> List.map \cell -> Str.toUtf8 cell |> List.get 0 |> Result.withDefault 0

flatten := List (List a) -> List a
flatten = \nestedList ->
    nestedList |> List.walk [] \a, b -> List.concat a b

get3x3AtPosition = \matrix, col, row ->
    [
        [(matrixGet matrix (col + 0) (row + 0) |> Result.withDefault 0), (matrixGet matrix (col + 1) (row + 0) |> Result.withDefault 0), (matrixGet matrix (col + 2) (row + 0) |> Result.withDefault 0)],
        [(matrixGet matrix (col + 0) (row + 1) |> Result.withDefault 0), (matrixGet matrix (col + 1) (row + 1) |> Result.withDefault 0), (matrixGet matrix (col + 2) (row + 1) |> Result.withDefault 0)],
        [(matrixGet matrix (col + 0) (row + 2) |> Result.withDefault 0), (matrixGet matrix (col + 1) (row + 2) |> Result.withDefault 0), (matrixGet matrix (col + 2) (row + 2) |> Result.withDefault 0)],
    ] |> flatten

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input -> 
    searchWord = "XMAS" |> Str.toUtf8

    inputAsArray = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \x -> Str.toUtf8 x

    
    horizontalCounts = inputAsArray
        |> List.map \line ->
            countMatches line searchWord
        |> List.sum
    

    horizontalReverseCounts = inputAsArray
        |> List.map \line ->
            countMatches (List.reverse line) searchWord
        |> List.sum

    verticalCounts = inputAsArray
        |> transpose
        |> List.map \line ->
            countMatches line searchWord
        |> List.sum

    verticalReverseCounts = inputAsArray
        |> transpose
        |> List.map \line ->
            countMatches (List.reverse line) searchWord
        |> List.sum

    positiveDiagonalCounts = inputAsArray
        |> diagonalizePositive
        |> List.map \line ->
            countMatches line searchWord
        |> List.sum


    positiveDiagonalReverseCounts = inputAsArray
        |> diagonalizePositive
        |> List.map \line ->
            countMatches (List.reverse line) searchWord
        |> List.sum

    negativeDiagonalCounts = inputAsArray
        |> diagonalizeNegative
        |> List.map \line ->
            countMatches line searchWord
        |> List.sum

    negativeDiagonalReverseCounts = inputAsArray
        |> diagonalizeNegative
        |> List.map \line ->
            countMatches (List.reverse line) searchWord
        |> List.sum


    Ok (Num.toStr (
        horizontalCounts + 
        horizontalReverseCounts + 
        verticalCounts + 
        verticalReverseCounts + 
        positiveDiagonalCounts + 
        positiveDiagonalReverseCounts +
        negativeDiagonalCounts +
        negativeDiagonalReverseCounts
    ))

part2 : Str -> Result Str _
part2 = \input -> 

    inputAsArray = input
        |> Str.trim
        |> Str.split "\n"
        |> List.map \x -> Str.toUtf8 x

    wildcard = "*" |> Str.toUtf8 |> List.get 0 |> Result.withDefault 0

    orthogonalPattern = ([
        ["*", "M", "*"],
        ["M", "A", "S"],
        ["*", "S", "*"],
    ]) |> convertToUtf8

    diagonalPattern = [
        ["M", "*", "S"],
        ["*", "A", "*"],
        ["M", "*", "S"],
    ] |> convertToUtf8

    patterns = [
        # (orthogonalPattern |> flatten),
        # (orthogonalPattern |> List.map (\x -> List.reverse x) |> flatten),
        # (orthogonalPattern |> List.reverse |> flatten),
        # (orthogonalPattern |> List.map (\x -> List.reverse x) |> List.reverse |> flatten),
        (diagonalPattern |> flatten),
        (diagonalPattern |> List.map (\x -> List.reverse x) |> flatten),
        (diagonalPattern |> transpose |> flatten),
        (diagonalPattern |> transpose |> List.reverse |> flatten),
    ]

    
    # convertToStringList = \pattern ->
    #     pattern |> List.map \row -> Str.fromUtf8 row |> Result.withDefault ""

    # patternsAsStrings = patterns |> List.map convertToStringList |> List.map \pattern -> "\n$(Str.joinWith pattern "\n")\n"

    # tmp = patternsAsStrings
    #     |> List.map \x ->
    #         dbg x
    #         x

    matches = inputAsArray
        |> List.mapWithIndex (
            \row, rowIndex ->
                List.mapWithIndex row \_, columnIndex ->
                    patternSection = get3x3AtPosition inputAsArray columnIndex rowIndex
                    List.any patterns \pattern ->
                        (List.map2 pattern patternSection \a, b -> a == wildcard || a == b)
                            |> List.all \x -> x
        )

    

    count = matches |> flatten |> List.countIf \x -> x         


    Ok (Num.toStr count)

