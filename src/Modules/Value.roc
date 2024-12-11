module [Value, map]

Value a : [Val a, NoValue]

map : Value a, (a -> b) -> Value b
map = \vv, f ->
    when vv is
        Val v ->
            Val (f v)

        NoValue ->
            NoValue
