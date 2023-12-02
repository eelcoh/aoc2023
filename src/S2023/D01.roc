interface S2023.D01
    exposes [solution]
    imports ["2023-01.txt" as input : Str, AoC]

solution : AoC.Solution
solution = { year: 2023, day: 1, title: "Trebuchet?!", part1, part2 }

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ -> 
    parse input 
        |> List.sum
        |> Num.toStr
        |> Ok


part2 : {} -> Result Str [NotImplemented, Error Str]
part2 = \_ -> Err NotImplemented

parse : Str -> List Nat
parse = \inputstring ->
    Str.split inputstring "\n" 
        |> List.keepOks makeNat

makeNat : Str -> Result Nat [NoNat]
makeNat = \str ->
    nats = 
        Str.graphemes str        
        |> List.keepOks Str.toNat
  
    when nats is 
        [f, .. , l] -> Ok (10*f + l)
        [f] -> Ok (10*f + f)
        [] -> Err NoNat


expect parse example == [12, 38, 15, 77]

example : Str
example =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """
