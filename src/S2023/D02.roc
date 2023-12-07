interface S2023.D02
    exposes [solution]
    imports
    
        [ AoC
        , Cubes.{ Cubes } 
        # , "2023-02.txt" as input : Str
        # , parser.Core.{ Parser, many, oneOf, map }
        # , parser.String.{ parseStr, string, codeunit, anyCodeunit }
        # , pf.Stdout
        ,
        ] 

solution : AoC.Solution
solution = { year: 2023, day: 2, title: "Cubes Conundrum", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 
    dbg "hello"

    lines testset 
        |> List.map parseLine
        # |> List.keepIf gameIsValid
        # |> List.map .id
        # |> List.map Num.toStr
        |> List.map .cubes
        |> List.map cubesListToStr
        |> Str.joinWith "\n"
        |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented




Game : 
    { id : U64
    , cubes : List Cubes
    }


    

emptyGame : Game 
emptyGame = 
    { id : 0
    , cubes : []
    }

    

gameIsValid = \game -> 
    List.map game.cubes Cubes.isValid
        |> all

        

parseLine : Str -> Game
parseLine = \inputLine ->
    parts = Str.split inputLine ": " 
    
    when parts is 
        [gameStr, cubesStr, ..] -> 
            {id: (parseGameStr gameStr), cubes : (parseCubesStr cubesStr)}
        _ -> emptyGame


parseGameStr : Str -> U64
parseGameStr = \gameStr ->
    parts = Str.split gameStr " " 

    when parts is 
        ["game", n] -> 
            Str.trim n
            |> Str.toU64 
            |> Result.withDefault 0
        ["game", n, ..] -> 
            Str.trim n
            |> Str.toU64 
            |> Result.withDefault 0
        _ -> 0


parseCubesStr : Str -> List Cubes
parseCubesStr = \cubesStr ->
    Str.split cubesStr ";"
        |> List.map parseCubesStr2


parseCubesStr2 : Str -> Cubes
parseCubesStr2 = \cubesStr ->

    # parseVal = Str, (U64 -> Cubes) -> Cubes
    parseVal = \val, fnCubes -> 

        Str.trim val
        |> Str.toU64 
        |> Result.map fnCubes

    getColorValue = \colorStr ->
        when Str.split colorStr " " is
            ["red", val] -> 
                parseVal val Cubes.red

            ["green", val] ->
                parseVal val Cubes.green 
                
            ["blue", val] -> 
                parseVal val Cubes.blue

            [..] -> Ok Cubes.empty

    Str.split cubesStr ","
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
    Str.split str "\n"


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