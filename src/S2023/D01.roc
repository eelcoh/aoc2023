interface S2023.D01
    exposes [solution]
    imports 
        [ "2023-01.txt" as input : Str
        , AoC
        , parser.Core.{ Parser, many, oneOf, map }
        , parser.String.{ parseStr, codeunit, anyCodeunit }
        ,
        ]

solution : AoC.Solution
solution = { year: 2023, day: 1, title: "Trebuchet?!", part1, part2 }

inputs = parse input
Digit : [One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Zero, NoDigit]


part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 
    inputs
        |> List.sum
        |> Num.toStr
        |> Ok


part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented

parse : Str -> List Nat
parse = \inputString ->
    Str.split inputString "\n" 
        |> List.keepOks makeNat

parse2 : Str -> List Nat
parse2 = \inputString ->
    Str.split inputString "\n" 
        |> List.keepOks parse2Helper
        |> List.keepOks makeNatFromList

parse2Helper : Str -> Result (List Nat) [ParsingFailure Str, ParsingIncomplete Str]
parse2Helper = \inputString ->
    many digitParser
        |> parseStr inputString
        |> Result.map (\dl -> List.keepOks dl digitToNatRes)

       
digitToNatRes : Digit -> Result Nat [NaN]
digitToNatRes = \d -> 
    when d is 
        One -> Ok 1
        Two -> Ok 2
        Three -> Ok 3
        Four -> Ok 4
        Five -> Ok 5
        Six -> Ok 6
        Seven -> Ok 7
        Eight -> Ok 8
        Nine -> Ok 9
        Zero -> Ok 0
        NoDigit -> Err NaN

makeNat : Str -> Result Nat [NoNat]
makeNat = \str ->
    nats = 
        Str.graphemes str        
        |> List.keepOks Str.toNat
  
    makeNatFromList nats

makeNatFromList : List Nat -> Result Nat [NoNat]
makeNatFromList = \nats -> 
    when nats is 
        [f, .. , l] -> Ok (10*f + l)
        [f] -> Ok (10*f + f)
        [] -> Err NoNat

digitParser : Parser (List U8) Digit
digitParser =
    oneOf [
        codeunit '1' |> map \_ -> One,
        codeunit '2' |> map \_ -> Two,
        codeunit '3' |> map \_ -> Three,
        codeunit '4' |> map \_ -> Four,
        codeunit '5' |> map \_ -> Five,
        codeunit '6' |> map \_ -> Six,
        codeunit '7' |> map \_ -> Seven,
        codeunit '8' |> map \_ -> Eight,
        codeunit '9' |> map \_ -> Nine,
        codeunit '0' |> map \_ -> Zero,
        anyCodeunit |> map \_ -> NoDigit,
    ]


expect parse examplePt1 == [12, 38, 15, 77]
expect parse2 examplePt1 == [12, 38, 15, 77]
expect parse examplePt2 == [ 29, 83, 13, 24, 42, 14, 76]

examplePt1 : Str
examplePt1 =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """

examplePt2 : Str
examplePt2 =
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """