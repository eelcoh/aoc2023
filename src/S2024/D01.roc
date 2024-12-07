module [solution]

import "2024-01.txt" as input : Str
import AoC
import parser.Parser exposing [Parser, many, oneOf, map]
import parser.String exposing [parseStr, string, codeunit, anyCodeunit]
import Modules.Tuples exposing [Tuple, first, second]
import Modules.ListExtras exposing [unzip, zip, gatherEquals]

solution : AoC.Solution
solution = { year: 2024, day: 1, title: "Historian Hysteria", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    parse input
    |> unzip
    |> sortAndZip
    |> subtract
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    parse input
    |> multiply
    |> Num.toStr
    |> Ok

# |> Num.toStr
# |> Ok

# parse1 : Str -> List (Tuple I32 I32)
# parse1 = \inputString ->
#    Str.splitOn inputString "\n"
#    |> List.map toNumbers
#    |> List.keepOks toPairs

parse : Str -> List (Tuple U64 U64)
parse = \inputString ->
    Str.splitOn inputString "\n"
    |> List.map toNumbers
    |> List.keepOks toPairs

# toNumbers : Str -> Result (List I32) [ParsingFailure Str, ParsingIncomplete Str]

toNumbers = \s ->
    Str.splitOn s " "
    |> List.keepOks Str.toU64

toPairs = \l ->
    when l is
        [a, b, ..] ->
            Ok (Pair a b)

        [a, b] ->
            Ok (Pair a b)

        _ -> Err "oeps"

sortAndZip = \pair ->
    Pair (List.sortAsc (first pair)) (List.sortAsc (second pair))
    |> \p -> zip (first p) (second p)

subtract = \pairs ->
    List.map pairs subtractPair
    |> List.map Num.abs

subtractPair = \t ->
    fst =
        Num.toI32 (first t)
    snd =
        Num.toI32 (second t)

    Num.abs (fst - snd)

multiply = \pairs ->
    List.map pairs second
    |> gatherEquals
    |> dbg
    |> List.map (\(a, b) -> (a, List.len b))
    |> Dict.fromList
    |> multiplyAll (List.map pairs first)

multiplyAll = \index, numbers ->
    calc = \n ->
        Dict.get index n
        |> Result.map (\r -> r + 1)
        |> Result.withDefault 0
        |> \r -> r * n

    List.map numbers calc
    |> List.sum

# parse1Helper : Str -> Result (List I32) [ParsingFailure Str, ParsingIncomplete Str]
# parse1Helper = \inputString ->
#    many digitParser1
#    |> parseStr inputString
#    |> Result.map (\dl -> List.keepOks dl digitToNatRes)

# part2 : {} -> Result Str [NotImplemented, Error Str]
# part2 = \_ -> Err NotImplemented

testinput =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """
