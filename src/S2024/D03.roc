module [solution, part1]

import AoC

import "2024-03.txt" as input : Str

import parser.Parser exposing [Parser, many, oneOf, keep, skip, chompWhile, const]
import parser.String exposing [parseStr, string, anyCodeunit, digits]
import Modules.Util exposing [identity]

solution : AoC.Solution
solution = { year: 2024, day: 3, title: "Mull It Over", part1, part2 }

Instruction : [
    Multiply (U64, U64),
    Garbage,
    Do,
    Dont,
]

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    input
    |> parseAll
    |> dbg
    |> List.map process
    |> List.sum
    |> Num.toStr
    |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    input
    |> parseAll
    |> dbg
    |> processAll
    |> Num.toStr
    |> Ok

multiply : Parser _ Instruction
multiply =
    const (\a -> \b -> Multiply (a, b))
    |> skip (string "mul(")
    |> keep digits
    |> skip (string ",")
    |> keep digits
    |> skip (string ")")

garbage : Parser _ Instruction
garbage =
    const Garbage
    |> skip (anyCodeunit)
    |> skip (chompWhile \b -> b != 'm' && b != 'd')

do : Parser _ Instruction
do =
    const Do
    |> skip (string "do()")

dont : Parser _ Instruction
dont =
    const Dont
    |> skip (string "don't()")

instructions : Parser _ (List Instruction)
instructions =
    oneOf [multiply, do, dont, garbage]
    |> many
    |> skip String.anyThing

parseAll : Str -> List Instruction
parseAll = \text ->
    parseStr instructions text
    |> Result.withDefault []

expect parseStr multiply "mul(2,4)" == Ok (Multiply (2, 4))
expect parseStr multiply "mul(223,445)" == Ok (Multiply (223, 445))
expect parseStr multiply "mul[3,7]" |> Result.isErr
expect parseStr multiply "muld(3,7)" |> Result.isErr
expect parseStr multiply "mul (3,7)" |> Result.isErr
expect parseStr multiply "mul( 3,7)" |> Result.isErr
expect parseStr multiply "mul(3, 7)" |> Result.isErr
expect parseStr multiply "mul(3,7 )" |> Result.isErr
expect parseAll "cdcmul(2,4)" == [Garbage, Garbage, Multiply (2, 4)]
expect parseAll "mul(2,4)ddd" == [Multiply (2, 4), Garbage, Garbage, Garbage]
expect parseAll "cdcmul(2,4)ffff" == [Garbage, Garbage, Multiply (2, 4), Garbage]
expect parseAll "cdcmul(2,4)ffdfdmul(4,5)dd" == [Garbage, Garbage, Multiply (2, 4), Garbage, Garbage, Garbage, Multiply (4, 5), Garbage, Garbage]

process : Instruction -> U64
process = \i ->
    when i is
        Multiply (x, y) -> x * y
        _ -> 0

State : [On, Off]

processAll : List Instruction -> U64
processAll = \l ->
    List.walk l (On, 0) go
    |> \(_, v) -> v

go : (State, U64), Instruction -> (State, U64)
go = \s, el ->
    when (s, el) is
        ((_, acc), Do) -> (On, acc)
        ((_, acc), Dont) -> (Off, acc)
        ((onoff, acc), Garbage) -> (onoff, acc)
        ((Off, acc), Multiply (_, _)) -> (Off, acc)
        ((On, acc), Multiply (x, y)) -> (On, (acc + (x * y)))

testinput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
testinput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
