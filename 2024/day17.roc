app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
    aocUtils: "../roc-aoc-utils/main.roc",
}

import pf.Stdout
import aocUtils.AoCUtils

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        day: 17,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

parseProgram : Str -> ((U64, U64, U64), List U8)
parseProgram = \input ->
    programParams =
        input
        |> Str.trim
        |> Str.replaceEach "Register A: " ""
        |> Str.replaceEach "Register B: " ""
        |> Str.replaceEach "Register C: " ""
        |> Str.replaceEach "\nProgram: " ""
        |> Str.splitOn "\n"
    registerAInitial = programParams |> List.get 0 |> AoCUtils.unwrap |> Str.toU64 |> AoCUtils.unwrap
    registerBInitial = programParams |> List.get 1 |> AoCUtils.unwrap |> Str.toU64 |> AoCUtils.unwrap
    registerCInitial = programParams |> List.get 2 |> AoCUtils.unwrap |> Str.toU64 |> AoCUtils.unwrap
    instructions = programParams |> List.get 3 |> AoCUtils.unwrap |> Str.splitOn "," |> List.map Str.toU8 |> List.keepOks AoCUtils.identity
    ((registerAInitial, registerBInitial, registerCInitial), instructions)

getComboOperand : (U64, U64, U64), List U8, U64 -> U64
getComboOperand = \memory, instructions, operandIndexInInstructions ->
    operandValueInInstructions = List.get instructions operandIndexInInstructions |> AoCUtils.unwrap
    when operandValueInInstructions is
        4 -> memory.0
        5 -> memory.1
        6 -> memory.2
        _ -> Num.toU64 operandValueInInstructions

getLiteralOperand : List U8, U64 -> U64
getLiteralOperand = \instructions, operandIndexInInstructions ->
    operandValueInInstructions = List.get instructions operandIndexInInstructions |> AoCUtils.unwrap
    Num.toU64 operandValueInInstructions

runProgram : (U64, U64, U64), List U8, U64, List U64 -> List U64
runProgram = \memory, instructions, instructionPointer, output ->
    currentInstructionResult = List.get instructions instructionPointer
    when currentInstructionResult is
        Ok currentInstruction ->
            operandIndex = instructionPointer + 1
            (newMemory, newOutput) =
                when currentInstruction is
                    0 -> # adv
                        numerator = memory.0
                        denominator = Num.powInt 2 (getComboOperand memory instructions operandIndex)
                        result = Num.divTrunc numerator denominator
                        ((result, memory.1, memory.2), output)

                    1 -> # bxl
                        result = Num.bitwiseXor memory.1 (getLiteralOperand instructions operandIndex)
                        ((memory.0, result, memory.2), output)

                    2 -> # bst
                        operand = getComboOperand memory instructions operandIndex
                        result = Num.bitwiseAnd operand 0b111
                        ((memory.0, result, memory.2), output)

                    3 -> # jnz
                        if memory.0 == 0 then
                            (memory, output)
                        else
                            nextJump = getLiteralOperand instructions operandIndex
                            return runProgram memory instructions nextJump output # fast exit and do recursive call with new jump location

                    4 -> # bxc
                        result = Num.bitwiseXor memory.1 memory.2
                        ((memory.0, result, memory.2), output)

                    5 -> # out
                        operand = getComboOperand memory instructions operandIndex
                        result = Num.bitwiseAnd operand 0b111
                        (memory, List.append output result)

                    6 -> # bdv
                        numerator = memory.0
                        denominator = Num.powInt 2 (getComboOperand memory instructions operandIndex)
                        result = Num.divTrunc numerator denominator
                        ((memory.0, result, memory.2), output)

                    7 -> # cdv
                        numerator = memory.0
                        denominator = Num.powInt 2 (getComboOperand memory instructions operandIndex)
                        result = Num.divTrunc numerator denominator
                        ((memory.0, memory.1, result), output)

                    _ -> crash "Invalid opcode $(currentInstruction |> Num.toStr)"
            runProgram newMemory instructions (instructionPointer + 2) newOutput

        Err _ -> output

runProgramSimplified : U64, List U8 -> List U8
runProgramSimplified = \a, output ->
    b = (Num.bitwiseXor (Num.bitwiseAnd a 0b111) 0b010) |> Num.toU8
    c = (Num.shiftRightZfBy a b) |> Num.toU8
    currentOutput = Num.bitwiseAnd (Num.bitwiseXor (Num.bitwiseXor b c) 0b111) 0b111
    nextA = Num.shiftRightZfBy a 3
    newOutput = List.append output currentOutput

    if nextA != 0 then
        runProgramSimplified nextA newOutput
    else
        newOutput

numberToBitString : U64 -> Str
numberToBitString = \number ->
    if number == 0 then
        ""
    else
        currentDigit = Num.bitwiseAnd number 1 |> Num.toStr
        Str.concat (numberToBitString (Num.shiftRightZfBy number 1)) currentDigit

checkQuine : U64, List U8, List U8 -> (Bool, List U8)
checkQuine = \a, instructions, output ->
    if a == 0 then
        if (instructions == output) then
            (Bool.true, output)
        else
            (Bool.false, [])
    else
        b = (Num.bitwiseXor (Num.bitwiseAnd a 0b111) 0b010) |> Num.toU8
        c = (Num.shiftRightZfBy a b) |> Num.toU8
        currentOutput = Num.bitwiseAnd (Num.bitwiseXor (Num.bitwiseXor b c) 0b111) 0b111

        nextA = Num.shiftRightZfBy a 3
        newOutput = List.append output currentOutput

        checkQuine nextA instructions newOutput

findValidInitializerValueV2 : List U8, U64, List U64 -> U64
findValidInitializerValueV2 = \instructions, currentLength, valuesToCheck ->
    validSubsetInitializers =
        valuesToCheck
        |> List.keepIf \initializerValue ->
            currentInstructionSubset = List.dropFirst instructions (List.len instructions - currentLength)
            isQuine = checkQuine initializerValue currentInstructionSubset []
            isQuine.0

    if currentLength == (List.len instructions) then
        validSubsetInitializers |> List.first |> AoCUtils.unwrap
    else
        newValuesToCheck =
            validSubsetInitializers
            |> List.joinMap \x ->
                base = Num.shiftLeftBy x 3
                [
                    base + 0,
                    base + 1,
                    base + 2,
                    base + 3,
                    base + 4,
                    base + 5,
                    base + 6,
                    base + 7,
                ]

        findValidInitializerValueV2 instructions (currentLength + 1) newValuesToCheck

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    (initialMemory, instructions) = parseProgram input

    output = runProgram initialMemory instructions 0 []

    Ok (Str.joinWith (List.map output Num.toStr) ",")

part2 : Str -> Result Str _
part2 = \input ->
    (_, instructions) = parseProgram input

    initializerValue = findValidInitializerValueV2 instructions 1 (List.range { start: At 0, end: At (Num.powInt 2 10) })

    Ok (initializerValue |> Num.toStr)
