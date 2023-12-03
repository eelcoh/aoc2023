interface S2023.D01
    exposes [solution]
    imports 
        [ "2023-01.txt" as input : Str
        , AoC
        , parser.Core.{ Parser, many, oneOf, map }
        , parser.String.{ parseStr, string, codeunit, anyCodeunit }
        ,
        ]

solution : AoC.Solution
solution = { year: 2023, day: 1, title: "Trebuchet?!", part1, part2 }

Digit : [One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Zero, NoDigit]


part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 
    parse1 input
        |> List.sum
        |> Num.toStr
        |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> 
    parse2 input
        |> List.sum
        |> Num.toStr
        |> Ok


#Part 1

parse1 : Str -> List Nat
parse1 = \inputString ->
    Str.split inputString "\n" 
        |> List.keepOks parse1Helper
        |> List.keepOks makeNatFromList


parse1Helper : Str -> Result (List Nat) [ParsingFailure Str, ParsingIncomplete Str]
parse1Helper = \inputString ->
    many digitParser1
        |> parseStr inputString
        |> Result.map (\dl -> List.keepOks dl digitToNatRes)


# Part 1 parser (naive)

digitParser1 : Parser (List U8) Digit
digitParser1 =
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


# Part 2

parse2 : Str -> List Nat
parse2 = \inputString ->
    Str.split inputString "\n" 
        |> List.keepOks parseTwice

parseTwice : Str -> Result Nat [ListWasEmpty, ParsingFailure Str, ParsingIncomplete Str]
parseTwice = \inputString -> 
    (parse2Helper digitParser inputString, parse2Helper digitParserReverse (reverseStr inputString) )
        |> \(f, l) -> joinNats f l


joinNats : Result Nat [ListWasEmpty, ParsingFailure Str, ParsingIncomplete Str], Result Nat [ListWasEmpty, ParsingFailure Str, ParsingIncomplete Str] -> Result Nat [ListWasEmpty, ParsingFailure Str, ParsingIncomplete Str]   
joinNats = \r1, r2 -> 
    when (r1, r2) is
        (Ok f, Ok l) -> Ok (10*f + l)
        (Err x, _) -> Err x
        (_, Err x) -> Err x


parse2Helper : Parser (List U8) Digit, Str -> Result Nat [ListWasEmpty, ParsingFailure Str, ParsingIncomplete Str]
parse2Helper = \parser, inputString ->
    many parser
        |> parseStr inputString
        |> Result.map (\dl -> List.keepOks dl digitToNatRes)
        |> Result.try List.first


makeNatFromList : List Nat -> Result Nat [NoNat]
makeNatFromList = \nats -> 
    when nats is 
        [f, .. , l] -> Ok (10*f + l)
        [f] -> Ok (10*f + f)
        [] -> Err NoNat


# Parsers for Part 2

digitParser : Parser (List U8) Digit
digitParser =
    oneOf [
        codeunit '1' |> map \_ -> One,
        string "one" |> map \_ -> One,
        codeunit '2' |> map \_ -> Two,
        string "two" |> map \_ -> Two,
        codeunit '3' |> map \_ -> Three,
        string "three" |> map \_ -> Three,
        codeunit '4' |> map \_ -> Four,
        string "four" |> map \_ -> Four,
        codeunit '5' |> map \_ -> Five,
        string "five" |> map \_ -> Five,
        codeunit '6' |> map \_ -> Six,
        string "six" |> map \_ -> Six,
        codeunit '7' |> map \_ -> Seven,
        string "seven" |> map \_ -> Seven,
        codeunit '8' |> map \_ -> Eight,
        string "eight" |> map \_ -> Eight,
        codeunit '9' |> map \_ -> Nine,
        string "nine" |> map \_ -> Nine,
        codeunit '0' |> map \_ -> Zero,
        string "zero" |> map \_ -> Zero,
        anyCodeunit |> map \_ -> NoDigit,
    ]

# Nasty, the input contains things like oneight
# which when parsed returns 1 but then disregards the 8.
# When found at the beginning, then this is ok, but at 
# the end of the line, it should take the 8, not the 1.
# Using the parser was already a slow solution, now it is
# twice as slow.

digitParserReverse : Parser (List U8) Digit
digitParserReverse =
    oneOf [
        codeunit '1' |> map \_ -> One,
        string "eno" |> map \_ -> One,
        codeunit '2' |> map \_ -> Two,
        string "owt" |> map \_ -> Two,
        codeunit '3' |> map \_ -> Three,
        string "eerht" |> map \_ -> Three,
        codeunit '4' |> map \_ -> Four,
        string "ruof" |> map \_ -> Four,
        codeunit '5' |> map \_ -> Five,
        string "evif" |> map \_ -> Five,
        codeunit '6' |> map \_ -> Six,
        string "xis" |> map \_ -> Six,
        codeunit '7' |> map \_ -> Seven,
        string "neves" |> map \_ -> Seven,
        codeunit '8' |> map \_ -> Eight,
        string "thgie" |> map \_ -> Eight,
        codeunit '9' |> map \_ -> Nine,
        string "enin" |> map \_ -> Nine,
        codeunit '0' |> map \_ -> Zero,
        string "orez" |> map \_ -> Zero,
        anyCodeunit |> map \_ -> NoDigit,
    ]

# helpers

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

reverseStr : Str -> Str
reverseStr = \s -> 
    Str.graphemes s
    |> List.reverse
    |> Str.joinWith ""

# tests

expect parse1 examplePt1 == [12, 38, 15, 77]
expect parse2 examplePt1 == [12, 38, 15, 77]
expect parse2 examplePt2 == [ 29, 83, 13, 24, 42, 14, 76, 55, 18]

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
    5ffour295
    oneight
    """