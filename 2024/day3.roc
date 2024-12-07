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
    day = 3

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

walkUntilCharacterMatches = \characterToMatch ->
    \state, nextCharacter -> 
        if nextCharacter == characterToMatch then
            Break state
        else
            Continue { accumulatedCharacters: (List.append state.accumulatedCharacters nextCharacter) }

takeCharactersUntilCharacter := List U8, U64, U8 -> List U8
takeCharactersUntilCharacter = \stringAsUtf8, startIndex, stopCharacter ->
    result = stringAsUtf8 |> List.walkFromUntil startIndex { accumulatedCharacters: [] } (walkUntilCharacterMatches stopCharacter)
    result.accumulatedCharacters

matchesPattern = \input, pattern ->
    (List.takeLast input (List.len pattern)) == pattern


## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input -> 
    mulPattern = Str.toUtf8 "mul("

    inputUtf8 = Str.toUtf8 input

    parsed = inputUtf8
        |> List.walkWithIndex { slidingWindow: List.repeat 0u8 7, sum: 0, enabled: 0 } \state, nextChar, index ->
            slidingWindow = List.append state.slidingWindow nextChar
                |> List.takeLast 7
            
            if matchesPattern slidingWindow mulPattern then
                firstNumberString = takeCharactersUntilCharacter inputUtf8 (index + 1) (Str.toUtf8 "," |> List.get 0 |> Result.withDefault 0)
                secondNumberString = takeCharactersUntilCharacter inputUtf8 (index + 1 + List.len firstNumberString + 1) (Str.toUtf8 ")" |> List.get 0 |> Result.withDefault 0)
                
                firstNumber = Str.toU32 (Str.fromUtf8 firstNumberString |> Result.withDefault "")
                secondNumber = Str.toU32 (Str.fromUtf8 secondNumberString |> Result.withDefault "")
                
                product = (firstNumber |> Result.withDefault 0) * (secondNumber |> Result.withDefault 0)

                { state & slidingWindow, sum: state.sum + product }
            else
                { state & slidingWindow }
    
    Ok (Num.toStr parsed.sum)

part2 : Str -> Result Str _
part2 = \input ->
    mulPattern = Str.toUtf8 "mul("
    doPattern = Str.toUtf8 "do()"
    dontPattern = Str.toUtf8 "don't()"

    maxPatternLength = List.max([List.len mulPattern, List.len doPattern, List.len dontPattern]) |> Result.withDefault 0

    inputUtf8 = Str.toUtf8 input

    parsed = inputUtf8
        |> List.walkWithIndex { slidingWindow: List.repeat 0u8 maxPatternLength, sum: 0, enabled: Bool.true } \state, nextChar, index ->
            slidingWindow = List.append state.slidingWindow nextChar
                |> List.takeLast maxPatternLength

            newState = { state & slidingWindow }
            
            if matchesPattern slidingWindow mulPattern then
                if state.enabled then
                    firstNumberString = takeCharactersUntilCharacter inputUtf8 (index + 1) (Str.toUtf8 "," |> List.get 0 |> Result.withDefault 0)
                    secondNumberString = takeCharactersUntilCharacter inputUtf8 (index + 1 + List.len firstNumberString + 1) (Str.toUtf8 ")" |> List.get 0 |> Result.withDefault 0)
                    
                    firstNumber = Str.toU32 (Str.fromUtf8 firstNumberString |> Result.withDefault "")
                    secondNumber = Str.toU32 (Str.fromUtf8 secondNumberString |> Result.withDefault "")
                    
                    product = (firstNumber |> Result.withDefault 0) * (secondNumber |> Result.withDefault 0)

                    { newState & sum: state.sum + product }
                else
                    newState
            else if matchesPattern slidingWindow doPattern then
                { newState & enabled: Bool.true }
            else if matchesPattern slidingWindow dontPattern then
                { newState & enabled: Bool.false }
            else
                newState

    Ok (Num.toStr parsed.sum)

