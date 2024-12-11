module [Polarity, polarity]

Polarity : [Positive, Negative, Zero]

polarity : Num * -> Polarity
polarity = \n ->
    if n > 0 then
        Positive
    else if n < 0 then
        Negative
    else
        Zero
