module [unzip, zip, gatherEquals]

import Modules.Tuples exposing [Tuple, first, second]

zip : List a, List b -> List (Tuple a b)
zip = \xs, ys ->
    List.map2 xs ys Pair

unzip : List (Tuple a b) -> Tuple (List a) (List b)
unzip = \pairs ->

    step : Tuple (List a) (List b), Tuple a b -> Tuple (List a) (List b)
    step = \pairsx, pair ->
        Pair (List.prepend (first pairsx) (first pair)) (List.prepend (second pairsx) (second pair))

    List.walkBackwards pairs (Pair [] []) step

gatherWith : (a, a -> Bool), List a -> List (a, List a)
gatherWith = \testFn, list ->

    helper : List a, List (a, List a) -> List (a, List a)
    helper = \scattered, gathered ->

        when scattered is
            [] ->
                List.reverse gathered

            [toGather, .. as population] ->
                (gathering, remaining) =
                    testClosure = \x ->
                        testFn toGather x

                    partition testClosure population

                helper remaining (List.prepend gathered (toGather, gathering))

    helper list []

gatherEquals : List a -> List (a, List a) where a implements Eq
gatherEquals = \list ->
    gatherWith (\x, y -> x == y) list

partition : (a -> Bool), List a -> (List a, List a)
partition = \pred, list ->

    step = \(trues, falses), x ->
        if pred x then
            (List.prepend trues x, falses)
        else
            (trues, List.prepend falses x)

    List.walkBackwards list ([], []) step
