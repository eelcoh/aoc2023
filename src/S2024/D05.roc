module [solution, part1]

import AoC

import "2024-05.txt" as input : Str

import parser.Parser exposing [Parser, keep, skip, const, sepBy]
import parser.String exposing [parseStr, string, digits, codeunit]
import Modules.Util exposing [identity]

solution : AoC.Solution
solution = { year: 2024, day: 5, title: "Print Queue", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    testinput
    |> read
    |> Result.map walkTheLines
    |> Result.map Num.toStr

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    Err NotImplemented

read : Str -> Result (Rules, Lines) [Error Str]
read = \inp ->
    Str.splitOn inp "\n\n"
    |> toRulesAndLines
    |> Result.mapErr (\_ -> Error "yipes!")

Rule : (U64, U64)
Rules : List Rule

Line : List U64
Lines : List Line

#
walkTheLines : (Rules, Lines) -> U64
walkTheLines = \(rs, ls) ->
    walk = \l ->
        walkTheLine l rs (Set.empty {})

    List.keepIf ls walk
    |> List.keepOks middle
    |> List.sum

# solution for part 1: walk through the elements
# of a line. Keep track of the set of numbers
# that are supposed to be listed before this element,
# including all rules, not only the ones with numbers
# that appear in this list.
# Suppose the rules say
# [1] w has to be before x
# [2] x has to be before y
# [3] y has to be before z
# And the list is [z, w], then rule [1] and [3] won't
# tell anything, it is rule [2] that is the most decisive.
# In this example, we start with an empty set, and the `z`.
# We will add all numbers to the set that have to be before
# the `z`. So all rules that that say a number has to be
# before the `z`, but recursively, any number that should
# be before the `y` as well.
walkTheLine : Line, Rules, Set U64 -> Bool
walkTheLine = \l, rs, s ->
    when dbg l is
        [a, .. as rest] ->
            if Set.contains s a then
                Bool.false
            else
                newSet =
                    setFrom a rs
                    |> Set.union s

                x = dbg newSet
                walkTheLine (rest) rs (newSet)

        [] ->
            Bool.true

# setFrom : U64, Rules -> Set U64
# setFrom = \n, rs ->
#    List.keepIf rs (\(a, b) -> b == n)
#    |> List.map (\(a, b) -> a)
#    |> Set.fromList

setFrom : U64, Rules -> Set U64
setFrom = \n, rs ->

    # create set at current level
    befores =
        List.keepIf rs (\(a, b) -> b == n)
        |> List.map (\(a, b) -> a)
        |> Set.fromList

    # reduce function
    expand = \v ->
        setFrom v rs

    # add all second levels
    Set.joinMap (befores) expand
# |> dbg

## Danger! If rules are circular, this will never stop
# setFrom2 : U64, Rules -> Set U64
# setFrom2 = \n, rs ->

#    reduce = \ss, v ->
#        numbers v
#        |> Set.union ss

#    # list the numbers that have to be before `v`
#    befores = \v ->
#        List.keepIf rs (\(a, b) -> b == v)
#        |> List.map (\(a, b) -> a)
#        |> Set.fromList

#    base =
#        befores n

#    curry = \v ->
#        setFrom2 v rs

#    if Set.isEmpty base then
#        base
#    else
#        Set.joinMap base curry

##

toRulesAndLines : List Str -> Result (Rules, Lines) [ParseError]
toRulesAndLines = \ls ->
    when ls is
        [a, b, ..] ->
            Ok (rules a, lines b)

        _ ->
            Err ParseError

rules : Str -> List Rule
rules = \s ->
    s
    |> Str.splitOn "\n"
    |> List.map (\st -> parseStr ruleParser st)
    |> List.keepOks identity

allNumbers : \allrules ->
    List.concatMap allrules (\(a, b) -> [a,b])

lines : Str -> List Line
lines = \s ->
    s
    |> Str.splitOn "\n"
    |> List.map (\st -> parseStr lineParser st)
    |> List.keepOks identity

ruleParser : Parser _ Rule
ruleParser =
    const (\a -> \b -> (a, b))
    |> keep digits
    |> skip (string "|")
    |> keep digits

lineParser : Parser _ Line
lineParser =
    digits |> sepBy (codeunit ',')

middle = \ls ->
    midlen ls (List.len ls)

midlen = \ls, ln ->
    when (ls, ln) is
        ([], _) -> Err EmptyList
        ([a, ..], 1) -> Ok a
        ([a, b, ..], 2) -> Err EvenList
        ([a, .. as xs], _) -> midlen xs (ln - 2)

testinput =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """
