module { stdoutLine } -> [Solution, solve]

import pf.Http
import pf.Dir
import pf.File
import pf.Path
import pf.Env
import pf.Arg

Solution err : {
    year : U64,
    day : U64,
    part1 : Str -> Result Str [SomeErr]err,
    part2 : Str -> Result Str [SomeErr]err,
    useTestInput : Bool,
} where err implements Inspect

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

getInput = \year, day, useTestInput ->
    executableFolder = getExecutableFolder!

    padded = (leftPad (Num.toStr day) "0" 2) |> Result.withDefault ""
    filename =
        if useTestInput then
            Str.joinWith ["testInput", padded, ".txt"] ""
        else
            Str.joinWith ["input", padded, ".txt"] ""
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

solve : Solution err -> Task {} _
solve = \{ year, day, part1, part2, useTestInput } ->
    input = getInput! year day useTestInput

    part1Result = part1 input
    part1Task =
        when part1Result is
            Ok str ->
                stdoutLine "Part 1: $(str)"

            # sendAnswer! year day 1 str
            Err _ -> stdoutLine "Something went wrong in part 1"

    stdoutLine! "Running Part 1"
    part1Task!

    part2Result = part2 input
    part2Task =
        when part2Result is
            Ok str -> stdoutLine "Part 2: $(str)"
            Err _ -> stdoutLine "Something went wrong in part 2"

    stdoutLine! "Running Part 2"
    part2Task!
