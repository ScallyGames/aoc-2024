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

# listToTuple := List t -> _ [InvalidListSize]
listToTuple = \list ->
    when (list |> List.get 0, list |> List.get 1) is
        (Ok a, Ok b) -> Ok (a, b)
        _ -> Err InvalidListSize

insert : List a, a, U64 -> List a
insert = \list, item, index -> 
    List.append (List.takeFirst list index) item
        |> List.concat (List.dropFirst list index)

expect
    previous = []
    itemToAdd = 5
    indexToAdd = 0

    expected = [5]
    actual = insert previous itemToAdd indexToAdd

    actual == expected


expect
    previous = [1, 2, 3]
    itemToAdd = 5
    indexToAdd = 0

    expected = [5, 1, 2, 3]
    actual = insert previous itemToAdd indexToAdd

    actual == expected

expect
    previous = [1, 2, 3]
    itemToAdd = 5
    indexToAdd = 1

    expected = [1, 5, 2, 3]
    actual = insert previous itemToAdd indexToAdd

    actual == expected

expect
    previous = [1, 2, 3]
    itemToAdd = 5
    indexToAdd = List.len previous

    expected = [1, 2, 3, 5]
    actual = insert previous itemToAdd indexToAdd

    actual == expected


integerDivision := Int a, Int a -> Int a
integerDivision = \a, b ->
    Num.floor ((Num.toFrac a) / (Num.toFrac b))

getCenterItem := List (Num a) -> Num a
getCenterItem = \list ->
    List.get list (integerDivision (List.len list) 2) |> Result.withDefault 0

getApplyingRules = \listOfPages, orderingRules ->
    orderingRules
        |> List.keepIf \rule -> 
            (List.contains listOfPages rule.0) &&
            (List.contains listOfPages rule.1)

hasValidOrdering = \listOfPages, orderingRules ->
    applyingRules = getApplyingRules listOfPages orderingRules
    List.all applyingRules (\rule -> matchesRule rule listOfPages)

fixOrdering := List U32, List (U32, U32) -> List U32
fixOrdering = \listOfPages, orderingRules ->
    applyingRules = getApplyingRules listOfPages orderingRules
    listOfPages 
        |> List.walk [] \state, nextItem ->
            firstPossibleIndex = applyingRules 
                |> List.keepIf \rule -> rule.1 == nextItem && (List.contains state rule.0)
                |> List.map (\rule -> (List.findFirstIndex state (\x -> x == rule.0) |> Result.withDefault 0) + 1)
                |> List.max
                |> Result.withDefault 0

            insert state nextItem firstPossibleIndex

fixOrderingAlternative := List U32, List (U32, U32) -> List U32
fixOrderingAlternative = \listOfPages, orderingRules ->
    applyingRules = getApplyingRules listOfPages orderingRules
    
    if listOfPages == [] then
        []
    else
        lastElement = (List.findFirst listOfPages \element -> List.all applyingRules \rule -> rule.0 != element) |> Result.withDefault 0
        remainingElements = List.dropIf listOfPages \x -> x == lastElement
        List.append (fixOrderingAlternative remainingElements applyingRules) lastElement


matchesRule := (U32, U32), List U32 -> Bool
matchesRule = \rule, listOfPages ->
    indexOfFirstRulePage = List.findFirstIndex listOfPages \x -> x == rule.0
    indexOfSecondRulePage = List.findFirstIndex listOfPages \x -> x == rule.1
    when (indexOfFirstRulePage, indexOfSecondRulePage) is
        (Ok first, Ok second) -> first < second
        _ -> crash "This rule should not have been checked"

parseInput = \input ->
    (ordering, updates) = Str.split input "\n\n" 
        |> listToTuple 
        |> Result.withDefault ("", "")

    orderingRules = ordering
        |> Str.trim
        |> Str.split "\n"
        |> List.map (
            \x -> Str.split x "|" 
                |> List.map (\v -> Str.toU32 v |> Result.withDefault 0)
                |> listToTuple
        )
        |> List.keepOks \x -> x


    updatePages = updates 
        |> Str.trim
        |> Str.split "\n"
        |> List.map 
            \x -> Str.split x "," 
                |> List.map Str.toU32
                |> List.keepOks \l -> l

    (orderingRules, updatePages)

main =
    year = 2024
    day = 5

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
    (orderingRules, updatePages) = parseInput input

    result = updatePages
        |> List.keepIf \listOfPages -> listOfPages |> hasValidOrdering orderingRules
        |> List.map getCenterItem
        |> List.sum

    Ok (Num.toStr result)

part2 : Str -> Result Str _
part2 = \input -> 
    (orderingRules, updatePages) = parseInput input

    result = updatePages
        |> List.dropIf \listOfPages -> listOfPages |> hasValidOrdering orderingRules
        |> List.map \listOfPages -> fixOrdering listOfPages orderingRules
        |> List.map getCenterItem
        |> List.sum


    resultAlternative = updatePages
        |> List.dropIf \listOfPages -> listOfPages |> hasValidOrdering orderingRules
        |> List.map \listOfPages -> fixOrderingAlternative listOfPages orderingRules
        |> List.map getCenterItem
        |> List.sum


    Ok ("Method A: $(Num.toStr result); Method B: $(Num.toStr resultAlternative)")

