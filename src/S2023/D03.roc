interface S2023.D03
    exposes [solution]
    imports 
        [ AoC
        , Modules.Strings.{ lines }
        ,
        ]

solution : AoC.Solution
solution = { year: 2023, day: 3, title: "Rucksack Reorganization", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 
    grid 
    |> gridToString
    |> Str.joinWith "\n"
    |> Ok


part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented

grid = 
    lines testSet
    |> List.map Str.graphemes
    |> List.map vectorise


TokenType :
    [ Number
    , Symbol
    , Space
    , Start
    ]


Vector : 
    [Vector TokenType Nat Nat Str ]


vectorise : List Str -> List Vector
vectorise = \strs -> 
    vectoriser Start 0 0 [] "" strs


vectoriser : TokenType, Nat, Nat, List Vector, Str, List Str -> List Vector
vectoriser = \currentTokenType, start, last, vecs, acum, rest ->

    dbg "+++++"
    dbg acum

    when rest is

        [] -> 
            List.append vecs (Vector currentTokenType start last acum)

        [head, .. as tail] -> 
            dbg acum
            headTokenType = tokenType head
        
            if tokenTypeIsEqual headTokenType currentTokenType
                then 
                    dbg "=="
                    # dbg head
                    dbg acum
                    # dbg rest

                    vectoriser 
                        headTokenType
                        start 
                        (last + 1) 
                        vecs 
                        (Str.concat acum head) 
                        tail
                else
                    dbg "!="
                    # dbg head
                    dbg acum
                    # dbg rest

                    vectoriser 
                        headTokenType
                        (last + 1)  
                        (last + 1) 
                        (List.append vecs (Vector headTokenType start last acum)) 
                        head 
                        tail                    


tokenType : Str -> TokenType
tokenType = \str -> 
    when str is
        "1" -> Number
        "2" -> Number
        "3" -> Number
        "4" -> Number
        "5" -> Number
        "6" -> Number
        "7" -> Number
        "8" -> Number
        "9" -> Number
        "0" -> Number
        "." -> Space
        "@" -> Symbol
        "#" -> Symbol
        "$" -> Symbol
        "%" -> Symbol
        "^" -> Symbol
        "&" -> Symbol
        "*" -> Symbol
        _ -> Start


tokenTypeIsEqual : TokenType, TokenType -> Bool
tokenTypeIsEqual = \t1, t2 ->
    when (t1, t2) is
        (Symbol, Symbol) -> Bool.true
        (Number, Number) -> Bool.true
        (Space, Space) -> Bool.true
        _ -> Bool.false

gridToString : List (List Vector) -> List Str
gridToString = \vecs -> 
    List.map vecs vecsToString

vecsToString : List Vector -> Str
vecsToString = \vecs -> 
    List.map vecs vecToString
    |> Str.joinWith " | "

vecToString : Vector -> Str
vecToString = \(Vector _ _ _ s) -> 
    s

testSet = 
    """
    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..
    """
    