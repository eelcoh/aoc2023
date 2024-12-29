module [solution, part1]

import AoC

import "2024-05.txt" as input : Str

import parser.Parser exposing [Parser, keep, skip, const, sepBy]
import parser.String exposing [parseStr, string, digits, codeunit]
import Modules.Util exposing [identity]
import Modules.ListExtras exposing [unique]

solution : AoC.Solution
solution = { year: 2024, day: 5, title: "Print Queue", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    input
    |> read
    |> Result.map (\(rls, lns) -> checkLines rls lns Bool.true)
    |> Result.map sumMiddles
    |> Result.map Num.toStr

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    (rls, lns) =
        input
        |> read
        |> Result.withDefault ([], [])

    checkLines rls lns Bool.false
    |> List.map (\ln -> fix ln rls)
    |> sumMiddles
    |> Num.toStr
    |> Ok

parseLines =
    testinput
    |> read
    |> Result.withDefault ([], [])

# expect (parseLines |> (\(a, _) -> Dict.keys a)) == [12]

read : Str -> Result (Rules, Lines) [Error Str]
read = \inp ->
    Str.splitOn inp "\n\n"
    |> toRulesAndLines
    |> Result.mapErr (\_ -> Error "not good!")

Rule : (U64, U64)
Rules : List (U64, Set U64)

Line : List U64
Lines : List Line

#
checkLines : Rules, Lines, Bool -> List (List U64)
checkLines = \rs, ls, keepValids ->
    walk = \l ->
        valid =
            checkLine l rs (Set.empty {})

        if keepValids then
            valid
        else
            !valid

    List.keepIf ls walk

checkLine : Line, Rules, Set U64 -> Bool
checkLine = \l, rs, befores ->
    when l is
        [a, .. as rest] ->
            if Set.contains befores a then
                Bool.false
            else
                newSet =
                    rulesGet rs a
                    |> Result.map (\s -> Set.union s befores)
                    |> Result.withDefault befores

                checkLine rest rs newSet

        [] ->
            Bool.true

sumMiddles : List (List U64) -> U64
sumMiddles = \l ->
    List.keepOks l middle
    |> List.sum

isPredecessor : Rules, U64, U64 -> Bool
isPredecessor = \rls, a, b ->
    rulesGet rls b
    |> Result.map (\s -> Set.contains s a)
    |> Result.withDefault Bool.false

insert = \rls, l, elem ->
    when l is
        [a, .. as rest] ->
            if isPredecessor rls elem a then
                List.prepend l elem
            else
                insert rls rest elem
                |> List.prepend a

        [] ->
            [elem]

fix : List U64, Rules -> List U64
fix = \l, rls ->
    insertFn = \ls, elem ->
        insert rls ls elem

    List.walk l [] insertFn

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

    befores = \v ->
        List.keepIf rls (\(a, b) -> b == v)
        |> List.map (\(a, b) -> a)

    makeDepsFirstPass rls # Rules

makeDepsFirstPass : List Rule -> Rules
makeDepsFirstPass = \rls ->
    d = []

    addToRules = \dd, (v, k) ->
        rulesSet dd k v

    List.walk rls d addToRules

toSet : (a, List b) -> (a, Set b)
toSet = \(a, b) ->
    (a, Set.fromList b)

ruleLines : Str -> List Rule
ruleLines = \s ->
    s
    |> Str.splitOn "\n"
    |> List.map (\st -> parseStr ruleParser st)
    |> List.keepOks identity
    |> List.sortWith sortRules

sortRules : Rule, Rule -> [LT, EQ, GT]
sortRules = \(a1, b1), (a2, b2) ->
    if a1 == a2 then
        Num.compare b1 b2
    else
        Num.compare a1 a2

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

rulesGet : Rules, U64 -> Result (Set U64) [NotFound]
rulesGet = \rls, k ->
    when rls is
        [(a, v), .. as rest] ->
            if a == k then
                Ok v
            else
                rulesGet rest k

        _ ->
            Err NotFound

rulesSet : Rules, U64, U64 -> Rules
rulesSet = \rls, k, v ->
    when rls is
        [(a, s), .. as rest] ->
            if a == k then
                newSet =
                    Set.insert s v

                List.prepend rest (a, newSet)
            else
                newRules =
                    rulesSet rest k v

                List.prepend newRules (a, s)

        [] ->
            [(k, Set.single v)]

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
# UNFORTUNATELY, this ran into stack overflows. Quite
# possibly the input is circular
