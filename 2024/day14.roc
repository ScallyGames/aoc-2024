app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br",
    aoc: "../roc-aoc-scaffolding/main.roc",
    aocUtils: "../roc-aoc-utils/main.roc",
    linearAlgebra: "../roc-linear-algebra/src/main.roc",
}

import pf.Stdout
import linearAlgebra.Vector2
import aocUtils.AoCUtils

import aoc.AoC {
    stdoutLine: Stdout.line,
}

main =
    AoC.solve! {
        year: 2024,
        day: 14,
        part1: part1,
        part2: part2,
        useTestInput: Bool.false,
    }

Robot : { position : Vector2.Vector2, velocity : Vector2.Vector2 }

parseRobot : Str -> Robot
parseRobot = \robotDescription ->
    params =
        robotDescription
        |> Str.splitOn " "
        |> List.map \x ->
            x
            |> Str.replaceEach "p=" ""
            |> Str.replaceEach "v=" ""
            |> Str.splitOn ","
            |> List.map Str.toF64
            |> List.keepOks \v -> v
            |> AoCUtils.listToTuple

    when params is
        [position, velocity] -> { position, velocity }
        _ -> crash "Malformed input"

getRobotLocationAfterSeconds : Robot, F64 -> Vector2.Vector2
getRobotLocationAfterSeconds = \robot, seconds ->
    Vector2.add robot.position (Vector2.mulScalar robot.velocity seconds)

getRobotQuadrant : (Int a, Int a), (Int a, Int a) -> (Int a, Int a)
getRobotQuadrant = \robotPosition, roomSize ->
    midpoint = (roomSize.0 // 2, roomSize.1 // 2)
    cellX = AoCUtils.sign (robotPosition.0 - midpoint.0)
    cellY = AoCUtils.sign (robotPosition.1 - midpoint.1)
    (cellX, cellY)

## Implement your part1 and part2 solutions here
part1 : Str -> Result Str _
part1 = \input ->
    width = 101
    height = 103
    roomSize = (width, height)

    robots =
        input
        |> Str.trim
        |> Str.splitOn "\n"
        |> List.map parseRobot

    robotPositionsAfter100 =
        robots
        |> List.map \x ->
            floatLocation = getRobotLocationAfterSeconds x 100
            (AoCUtils.posRem (Num.round floatLocation.0) width, AoCUtils.posRem (Num.round floatLocation.1) height)

    robotQuadrantsAfter100 =
        robotPositionsAfter100
        |> List.map \x -> getRobotQuadrant x roomSize
        |> List.dropIf \x -> x.0 == 0 || x.1 == 0

    quadrantCounts =
        robotQuadrantsAfter100
        |> AoCUtils.groupBy \x -> x
        |> Dict.values
        |> List.map List.len

    result = quadrantCounts |> List.product

    Ok (result |> Num.toStr)

part2 : Str -> Result Str _
part2 = \input ->
    Ok ""
