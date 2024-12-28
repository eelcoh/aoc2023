module [solution, part1]

import AoC

import "2024-05.txt" as input : Str

import parser.Parser exposing [Parser, keep, skip, const, sepBy]
import parser.String exposing [parseStr, string, digits, codeunit]
import Modules.Util exposing [identity]
import Modules.ListExtras exposing [unique]

solution : AoC.Solution
solution = { year: 2024, day: 5, title: "Print Queue", part1, part2 }

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
Rules : Dict U64 (Set U64)

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

walkTheLine : Line, Rules, Set U64 -> Bool
walkTheLine = \l, rs, befores ->
    when l is
        [a, .. as rest] ->
            if Set.contains befores a then
                Bool.false
            else
                newSet =
                    Dict.get rs a
                    |> Result.map (\s -> Set.union s befores)
                    |> Result.withDefault befores

                walkTheLine rest rs newSet

        [] ->
            Bool.true

toRulesAndLines : List Str -> Result (Rules, Lines) [ParseError]
toRulesAndLines = \ls ->
    when ls is
        [a, b, ..] ->
            Ok (rules a, lines b)

        _ ->
            Err ParseError

rules : Str -> Rules
rules = \s ->
    rls =
        ruleLines s

    consolidate rls

consolidate : List Rule -> Rules
consolidate = \rls ->

    nrs =
        allNumbers rls

    befores = \v ->
        List.keepIf rls (\(a, b) -> b == v)
        |> List.map (\(a, b) -> a)

    deps = \v ->
        befores v
        |> \l -> (v, Set.fromList l)

    firstpass =
        makeDepsFirstPass rls

    secondpass =
        List.map nrs (\v -> (v, makeDepsSecondPass firstpass v))
        |> Dict.fromList

    secondpass

expect consolidate [(14, 83), (53, 14), (12, 53), (67, 14)] == Dict.fromList [(83, Set.fromList [14, 53, 67, 12]), (14, Set.fromList [53, 12, 67]), (53, Set.fromList [12]), (67, Set.empty {}), (12, Set.empty {})]

makeDepsFirstPass : List Rule -> Rules
makeDepsFirstPass = \rls ->
    d = Dict.empty {}

    # insert :  Dict k v,  k,  v -> Dict k v

    addToDict = \dd, (v, k) ->
        s = Dict.get dd k

        when s is
            Ok ss ->
                Dict.insert dd k (Set.insert ss v)

            _ ->
                Dict.insert dd k (Set.single v)

    # List.walk :  List elem,  state,  (state, elem -> state) -> state
    List.walk rls d addToDict

makeDepsSecondPass : Dict U64 (Set U64), U64 -> Set U64
makeDepsSecondPass = \rls, n ->

    dps : U64 -> Set U64
    dps = \v ->
        d = Dict.get rls v

        when d is
            Ok vs ->
                List.map (Set.toList vs) dps # List (Set U64)
                |> List.walk vs Set.union # List (Set U64)

            _ ->
                Set.empty {}

    dps n

toSet : (a, List b) -> (a, Set b)
toSet = \(a, b) ->
    (a, Set.fromList b)

makeDepsHelper : List (U64, List U64), U64 -> Set U64
makeDepsHelper = \rls, v ->
    List.map rls toSet
    |> Dict.fromList
    |> \s -> makeDepsSecondPass s v

testrules = [(83, [14]), (14, [53, 67]), (53, [12]), (67, []), (12, [])]
expect makeDepsHelper testrules 83 == (Set.fromList [14, 53, 67, 12])
expect makeDepsHelper testrules 14 == (Set.fromList [53, 12, 67])

ruleLines : Str -> List Rule
ruleLines = \s ->
    s
    |> Str.splitOn "\n"
    |> List.map (\st -> parseStr ruleParser st)
    |> List.keepOks identity

allNumbers = \allrules ->
    List.joinMap allrules (\(a, b) -> [a, b])
    |> unique

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

# List.walk :  List elem,  state,  (state, elem -> state) -> state
# Set.walk :  Set k,  state,  (state, k -> state) -> state
# Set.joinMap : Set a, (a -> Set b) -> Set b
# Result.withDefault : Result ok err, ok -> ok
# Result.map : Result a err, (a -> b) -> Result b err
# Result.try : Result a err, (a -> Result b err) -> Result b err
