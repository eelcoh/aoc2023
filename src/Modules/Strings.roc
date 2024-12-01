interface Modules.Strings
    exposes [lines]
    imports 
        [  
        ]

lines : Str -> List Str
lines = \str -> 
    Str.split str "\n"