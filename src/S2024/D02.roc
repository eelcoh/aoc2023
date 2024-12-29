module [solution]

import "2024-02.txt" as input : Str
import AoC
import parser.Parser exposing [Parser, many, oneOf, map]
import parser.String exposing [parseStr, string, codeunit, anyCodeunit]
import Modules.Value as Value exposing [Value]
import Modules.ListExtras exposing [unzip, zip, gatherEquals, select]
import Bool exposing [and, or]

solution : AoC.Solution
solution = { year: 2024, day: 2, title: "Red-Nosed Reports", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    parse testinput
    |> List.map deltas
    |> List.keepOks valid
    |> List.len
    |> Num.toStr
    |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    parse input
    |> List.map checkReports
    |> dbg
    |> List.dropIf (\l -> List.isEmpty l)
    |> dbg
    |> List.len
    |> Num.toStr
    |> Ok

parse : Str -> List (List I32)
parse = \inputString ->
    Str.splitOn inputString "\n"
    |> List.map toNumbers
    |> List.dropIf List.isEmpty

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
    List.all l isSmall

isFlipped : I32, Value I32 -> Bool
isFlipped = \a, vb ->
    when vb is
        Val b ->
            ((a < 0) && (b > 0)) || ((a > 0) && (b < 0))

        NoValue ->
            Bool.false

isSmall = \x ->
    a = Num.abs x
    (a >= 1) && (a <= 3)

isZero = \x ->
    x == 0

allValid = \l ->
    (allPositive l || allNegative l) && allSmall l && (!(List.isEmpty l))

checkReports : List I32 -> List (List I32)
checkReports = \l ->
    variants l # List a -> List (List a)
    |> List.map deltas # List (List I32) -> List (List I32)
    |> List.keepOks valid # List (List I32)

variants : List a -> List (List a)
variants = \l ->
    snd = \(_, b) ->
        b

    select l
    |> List.map snd

valid : List I32 -> Result (List I32) Str
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
    5 4 2 3 2
    5 4 2 3 1
    8 6 4 4 1
    1 3 6 7 9
    30 32 33 35 38 42 41
    28 30 31 36 39 42
    """

# 7 6 4 2 1: >> -1 -2 -2 -1    Safe without removing any level.
# 1 2 7 8 9: >>  1  5  1  1    Unsafe regardless of which level is removed.
# 9 7 6 2 1: >> -2 -1 -4 -1    Unsafe regardless of which level is removed.
# 1 3 2 4 5: >>  2 -1  2  1    Safe by removing the second level, 3.
# 2 3 2 4 5: >>  1 -1  2  1    Safe by removing the third level, 2.
# 5 4 2 3 2: >>  1 -1  2  1    Safe by removing the third level, 2.
# 5 4 2 3 1: >>  1 -1  2  1    Safe by removing the fourth level, 3.
# 8 6 4 4 1: >> -2 -2  0 -3    Safe by removing the third level, 4.
# 1 3 6 7 9: >>  2  3  1  1    Safe without removing any level.
# 30 32 33 35 38 42 41: >> -2 -1 -3 -3 -4 -1    Safe by removing the sixth level, 42.
