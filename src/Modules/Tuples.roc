module [Tuple, first, second]

Tuple a b : [Pair a b]

first : Tuple a b -> a
first = \Pair a _ ->
    a

second : Tuple a b -> b
second = \Pair _ b ->
    b
