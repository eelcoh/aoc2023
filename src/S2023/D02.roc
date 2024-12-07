module [solution]

import AoC
import Cubes exposing [Cubes]
import "2023-02.txt" as input : Str

solution : AoC.Solution
solution = { year: 2023, day: 2, title: "Cubes Conundrum", part1, part2 }

games : List Game
games =
    lines input
    |> List.map parseLine

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->

    games
    |> List.keepIf gameIsValid
    |> List.map .id
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    games
    |> List.map .cubes # -> List (List Cubes)
    |> List.map (\cl -> List.walk cl Cubes.empty Cubes.max) # -> List Cubes
    |> List.map Cubes.power # -> List U64
    |> List.sum # -> U64
    |> Num.toStr # -> Str
    |> Ok

Game :
{
    id : U64,
    cubes : List Cubes,
}

emptyGame : Game
emptyGame = {
    id: 0,
    cubes: [],
}

gameIsValid = \game ->
    List.map game.cubes Cubes.isValid
    |> all

parseLine : Str -> Game
parseLine = \inputLine ->
    parts = Str.splitOn inputLine ": "

    when parts is
        [gameStr, cubesStr, ..] ->
            { id: parseGameStr gameStr, cubes: parseCubesStr cubesStr }

        _ -> emptyGame

parseGameStr : Str -> U64
parseGameStr = \gameStr ->
    parts = Str.splitOn gameStr " "

    when parts is
        ["Game", n] ->
            Str.trim n
            |> Str.toU64
            |> Result.withDefault 0

        ["Game", n, ..] ->
            Str.trim n
            |> Str.toU64
            |> Result.withDefault 0

        _ -> 0

parseCubesStr : Str -> List Cubes
parseCubesStr = \cubesStr ->
    Str.splitOn cubesStr ";"
    |> List.map parseCubesStr2

parseCubesStr2 : Str -> Cubes
parseCubesStr2 = \cubesStr ->

    # parseVal = Str, (U64 -> Cubes) -> Cubes
    parseVal = \val, fnCubes ->

        Str.trim val
        |> Str.toU64
        |> Result.map fnCubes

    getColorValue = \colorStr ->
        when Str.splitOn colorStr " " is
            [val, "red"] ->
                parseVal val Cubes.red

            [val, "green"] ->
                parseVal val Cubes.green

            [val, "blue"] ->
                parseVal val Cubes.blue

            [..] ->
                Ok Cubes.empty

    Str.splitOn cubesStr ","
    |> List.map Str.trim
    |> List.keepOks getColorValue
    |> List.walk Cubes.empty Cubes.add

testGames =
    lines testset
    |> List.map parseLine
    |> List.keepIf gameIsValid
    |> List.map .id

# helpers
all : List Bool -> Bool
all = \bools ->
    bools
    |> List.walk Bool.true Bool.and

lines = \str ->
    Str.splitOn str "\n"

cubesListToStr : List Cubes -> Str
cubesListToStr = \cubesList ->
    List.map cubesList Cubes.toStr
    |> Str.joinWith " - "

# tests
expect testGames == [1, 2, 5]

testset =
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """
