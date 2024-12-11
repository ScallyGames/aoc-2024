app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
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
    day = 9

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

BlockSize := U64 implements [Eq, Hash, Inspect]
FileId := U64 implements [Eq, Hash, Inspect]

Entry : [FileEntry BlockSize FileId, BlankEntry BlockSize]
MemoryBlock : [FileBlock FileId, BlankBlock]

entryToFileEntry : U8, U64 -> Entry
entryToFileEntry = \x, i ->
    numberOfRepeats = @BlockSize (Num.toU64 x)
    if Num.isEven i then
        fileId = @FileId (Num.divTrunc i 2)
        FileEntry numberOfRepeats fileId
    else
        BlankEntry numberOfRepeats

entryToMemoryBlocks : Entry -> List MemoryBlock
entryToMemoryBlocks = \entry ->
    when entry is
        FileEntry (@BlockSize size) id -> List.repeat (FileBlock id) size
        BlankEntry (@BlockSize size) -> List.repeat BlankBlock size

parseInputToFiles : Str -> List Entry
parseInputToFiles = \input ->
    input
    |> Str.toUtf8
    |> List.map \x -> Str.toU8 (try Str.fromUtf8 [x])
    |> List.keepOks \x -> x
    |> List.mapWithIndex entryToFileEntry

defragmentBlocks : List MemoryBlock -> List MemoryBlock
defragmentBlocks = \memory ->
    firstBlank = List.findFirstIndex memory \x -> x == BlankBlock
    lastNonBlank = List.findLastIndex memory \x -> x != BlankBlock

    when (firstBlank, lastNonBlank) is
        (Ok blankIndex, Ok nonBlankIndex) ->
            if blankIndex < nonBlankIndex then
                defragmentBlocks (List.swap memory blankIndex nonBlankIndex)
            else
                memory

        _ -> memory

fileWithIdLowerThan : FileId -> (Entry -> Bool)
fileWithIdLowerThan = \@FileId highestId ->
    \x ->
        when x is
            FileEntry _ (@FileId id) -> id < highestId
            _ -> Bool.false

findSwappablePair : List Entry, FileId -> Result (U64, U64) _
findSwappablePair = \entries, highestId ->
    List.findLastIndex entries (fileWithIdLowerThan highestId)
    |> Result.try \fileId ->
        List.get entries fileId |> Result.try \file -> Ok { fileId, file }
    |> Result.try \fileFindResult ->
        when fileFindResult.file is
            FileEntry (@BlockSize fileSize) _ ->
                blankIndexResult = List.findFirstIndex entries \x ->
                    when x is
                        BlankEntry (@BlockSize blankSize) -> blankSize >= fileSize
                        _ -> Bool.false
                when blankIndexResult is
                    Ok blankIndex -> Ok (fileFindResult.fileId, blankIndex)
                    _ -> Err SomethingWentWrong

            _ -> Err SomethingWentWrong

defragmentFiles : List Entry, FileId -> List Entry
defragmentFiles = \entries, highestId ->
    indexesResult = findSwappablePair entries highestId
    swapEntriesResult =
        indexesResult
        |> Result.try \indexes ->
            Result.map2 (List.get entries indexes.0) (List.get entries indexes.1) \a, b -> (a, b)
    swapParticipants = Result.map2 indexesResult swapEntriesResult \indexes, swapParts -> ((indexes.0, swapParts.0), (indexes.1, swapParts.1))
    when swapParticipants is
        Ok ((fileIndex, FileEntry (@BlockSize fileSize) fileId), (blankIndex, BlankEntry (@BlockSize blankSize))) ->
            if blankIndex < fileIndex then
                newList =
                    if fileSize < blankSize then
                        entries
                        |> List.map \x -> [x]
                        |> List.set fileIndex [BlankEntry (@BlockSize fileSize)]
                        |> List.set blankIndex [FileEntry (@BlockSize fileSize) fileId, BlankEntry (@BlockSize (blankSize - fileSize))]
                        |> List.join
                    else
                        entries |> List.swap fileIndex blankIndex
                defragmentFiles newList fileId
            else
                (@FileId highestIdInt) = highestId
                if highestIdInt == 0 then
                    entries
                else
                    defragmentFiles entries (@FileId (highestIdInt - 1))

        _ ->
            (@FileId highestIdInt) = highestId
            if highestIdInt == 0 then
                entries
            else
                defragmentFiles entries (@FileId (highestIdInt - 1))

# memoryToString : List MemoryBlock -> Str
# memoryToString = \memory ->
#    memory
#    |> List.map \x ->
#        when x is
#            FileBlock (@FileId fileId) -> Num.toStr fileId
#            BlankBlock -> "."
#    |> Str.joinWith ""

checksumValue : [FileBlock FileId, BlankBlock], U64 -> U64
checksumValue = \value, index ->
    when value is
        FileBlock (@FileId id) -> id * index
        BlankBlock -> 0

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    initialMemory =
        parseInputToFiles input
        |> List.map entryToMemoryBlocks
        |> List.join

    defragmented = defragmentBlocks initialMemory

    checksum = defragmented |> List.mapWithIndex checksumValue |> List.sum
    Ok (Num.toStr checksum)

part2 : Str -> Result Str _
part2 = \input ->
    initialFiles = parseInputToFiles input

    defragmented = defragmentFiles initialFiles (@FileId Num.maxU64)

    checksum =
        defragmented
        |> List.map entryToMemoryBlocks
        |> List.join
        |> List.mapWithIndex checksumValue
        |> List.sum
    Ok (Num.toStr checksum)
