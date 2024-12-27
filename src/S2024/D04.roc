module [solution, part1]

import AoC

import "2024-04.txt" as input : Str

import parser.Parser exposing [Parser, many, oneOf, keep, skip, chompUntil, chompWhile, const]
import parser.String exposing [parseStr, utf8, string, codeunit, anyCodeunit, digits]
import Modules.Util exposing [identity]

solution : AoC.Solution
solution = { year: 2024, day: 4, title: "Ceres Search", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    input
    |> toMatrix
    |> walkThisMatrix X findAllForX
    |> Num.toStr
    |> Ok

part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ ->
    input
    |> toMatrix
    |> walkThisMatrix A findAllForA
    |> Num.toStr
    |> Ok

Matrix a : List (List a)

Letter : [X, M, A, S, Z]

Direction : [Up, Down, Right, Left, UpLeft, UpRight, DownLeft, DownRight]

Diagonal : [UpLeftDownRight, UpRightDownLeft]

walkThisMatrix : Matrix Letter, Letter, (Matrix Letter, (U64, U64) -> U64) -> U64
walkThisMatrix = \m, c, fun ->

    walkY = \l, y ->
        List.mapWithIndex l (\i, x -> walkX i x y)

    walkX = \i, x, y ->
        if i == c then
            fun m (x, y)
        else
            0

    List.mapWithIndex m walkY
    |> List.map List.sum
    |> List.sum

findAllForX : Matrix Letter, (U64, U64) -> U64
findAllForX = \matrix, pos ->

    findForDir = \dir ->
        findXMAS matrix dir X pos

    [Up, Down, Right, Left, UpLeft, UpRight, DownLeft, DownRight]
    |> List.map findForDir
    |> List.keepOks (\x -> x)
    |> List.len

findXMAS : Matrix Letter, Direction, Letter, (U64, U64) -> Result U64 [NoXMas]
findXMAS = \matrix, dir, letterSearched, pos ->
    letter =
        get matrix pos

    rec = \l ->
        if l != letterSearched then
            Err NoXMas
        else
            newPos =
                newPosition dir pos

            findNext = \nextLetter ->
                Result.try newPos (\p -> findXMAS matrix dir nextLetter p)

            when letterSearched is
                S ->
                    Ok 1 # Merry Christmas

                X ->
                    findNext M

                M ->
                    findNext A

                A ->
                    findNext S

                Z ->
                    Err NoXMas

    Result.try letter rec

findAllForA : Matrix Letter, (U64, U64) -> U64
findAllForA = \matrix, pos ->

    upLeftDownRight =
        findDiagonal matrix UpLeftDownRight pos

    topRightdownLeft =
        findDiagonal matrix UpRightDownLeft pos

    isEqual : Letter, Letter -> [EQ, LT, GT]
    isEqual = \a, b ->
        when (a, b) is
            (A, A) -> EQ
            (A, _) -> LT
            (_, A) -> GT
            (Z, Z) -> EQ
            (_, Z) -> LT
            (Z, _) -> GT
            (M, M) -> EQ
            (S, S) -> EQ
            (X, X) -> EQ
            (M, S) -> LT
            (M, X) -> LT
            (S, M) -> GT
            (S, X) -> LT
            (X, M) -> GT
            (X, S) -> GT

    isMAS : List Letter -> Bool
    isMAS = \ls ->
        List.sortWith ls isEqual # List.sortWith ls (\a, b -> (toLetter a) == (toLetter b))
        |> (\l -> l == [A, M, S])

    if
        (isMAS upLeftDownRight) && (isMAS topRightdownLeft)
    then
        1
    else
        0

newPosition : Direction, (U64, U64) -> Result (U64, U64) [OutOfBounds]
newPosition = \dir, (x, y) ->
    when dir is
        Up ->
            if y > 0 then Ok (x, y - 1) else Err OutOfBounds

        Down ->
            Ok (x, y + 1)

        Right ->
            Ok (x + 1, y)

        Left ->
            if x > 0 then Ok (x - 1, y) else Err OutOfBounds

        UpLeft ->
            if x > 0 && y > 0 then Ok (x - 1, y - 1) else Err OutOfBounds

        UpRight ->
            if y > 0 then Ok (x + 1, y - 1) else Err OutOfBounds

        DownLeft ->
            if x > 0 then Ok (x - 1, y + 1) else Err OutOfBounds

        DownRight ->
            Ok (x + 1, y + 1)

findDiagonal : Matrix a, Diagonal, (U64, U64) -> List a
findDiagonal = \matrix, diag, pos ->
    when diag is
        UpLeftDownRight ->
            upLeft =
                getInDirection matrix UpLeft pos
            center =
                get matrix pos
            downRight =
                getInDirection matrix DownRight pos

            List.keepOks [upLeft, center, downRight] identity

        UpRightDownLeft ->
            topRight =
                getInDirection matrix UpRight pos
            center =
                get matrix pos
            downLeft =
                getInDirection matrix DownLeft pos

            List.keepOks [topRight, center, downLeft] identity

getInDirection : Matrix a, Direction, (U64, U64) -> Result a [OutOfBounds]
getInDirection = \matrix, dir, pos ->
    newPos =
        newPosition dir pos

    getItemFromMatrix = \p ->
        get matrix p

    Result.try newPos getItemFromMatrix

#
get : Matrix a, (U64, U64) -> Result a [OutOfBounds]
get = \matrix, (x, y) ->
    List.get matrix y # Result a [OutOfBounds]
    |> Result.try (\r -> List.get r x)

toMatrix : Str -> Matrix Letter
toMatrix = \s ->
    lines = \st ->
        Str.splitOn st "\n"

    lines s
    |> List.map toLetters

toLetters : Str -> List Letter
toLetters = \s ->
    Str.toUtf8 s
    |> List.map toLetter

toLetter : U8 -> Letter
toLetter = \l ->
    when l is
        'X' -> X
        'M' -> M
        'A' -> A
        'S' -> S
        _ -> Z

#

testinput =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """
