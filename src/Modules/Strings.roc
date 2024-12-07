module [lines]

lines : Str -> List Str
lines = \str ->
    Str.splitOn str "\n"
