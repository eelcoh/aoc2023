module [solution]

import "2024-02.txt" as input : Str
import AoC
import parser.Parser exposing [Parser, many, oneOf, map]
import parser.String exposing [parseStr, string, codeunit, anyCodeunit]
import Modules.Tuples exposing [Tuple, first, second]
import Modules.ListExtras exposing [unzip, zip, gatherEquals]

solution : AoC.Solution
solution = { year: 2024, day: 2, title: "Red-Nosed Reports", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    parse input
    |> List.map deltas
    |> dbg
    |> List.keepOks valid
    |> dbg
    |> List.len
    |> Num.toStr
    |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    parse input
    |> List.map deltas
    |> List.len
    |> Num.toStr
    |> Ok

parse : Str -> List (List I32)
parse = \inputString ->
    Str.splitOn inputString "\n"
    |> List.map toNumbers

# toNumbers : Str -> Result (List I32) [ParsingFailure Str, ParsingIncomplete Str]
# toNumbers : Str -> Result (List I32) _
toNumbers = \s ->
    Str.splitOn s " "
    |> List.keepOks Str.toI32

deltas : List I32 -> List I32
deltas = \l ->
    when l is
        [a, b, .. as rest] ->
            List.prepend (deltas (List.prepend rest b)) (a - b)

        _ ->
            []

allPositive = \l ->
    positive = \a ->
        a > 0

    List.all l positive

allNegative = \l ->
    negative = \a ->
        a < 0

    List.all l negative

allSmall = \l ->
    small = \x ->
        a = Num.abs x
        (a >= 1) && (a <= 3)

    List.all l small

allValid = \l ->
    (allPositive l || allNegative l) && allSmall l && (!(List.isEmpty l))

valid = \l ->
    if allValid l then
        Ok l
    else
        Err "not relevant"

testinput =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    2 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """
