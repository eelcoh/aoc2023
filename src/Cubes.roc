interface Cubes
    exposes [Cubes, empty, red, green, blue, add, max, toStr, toList, isValid]
    imports 
        [  
        ]


Cubes : 
    { red : U64
    , green: U64
    , blue : U64
    }


constraints : Cubes
constraints = 
    { red : 12
    , green : 13
    , blue : 14 
    }

red : U64 -> Cubes
red = \val -> 
    { red : val
    , green : 0
    , blue : 0 
    }

green : U64 -> Cubes
green = \val -> 
    { red : 0
    , green : val
    , blue : 0 
    }

blue : U64 -> Cubes
blue = \val -> 
    { red : 0
    , green : 0
    , blue : val
    }


empty : Cubes
empty = 
    { red : 0
    , green : 0
    , blue : 0
    }

max : Cubes, Cubes -> Cubes
max = \c1, c2 -> 
    { red : Num.max c1.red c2.red
    , green : Num.max c1.green c2.green
    , blue : Num.max c1.blue c2.blue
    }

isValid : Cubes -> Bool
isValid = \cube -> 
    [ constraints.red >= cube.red
    , constraints.blue >= cube.blue
    , constraints.green >= cube.green
    ]
        |> all
    

add : Cubes, Cubes -> Cubes
add = \c1, c2 -> 
    { red : c1.red + c2.red
    , green : c1.green + c2.green
    , blue : c1.blue + c2.blue
    }


    
toStr : Cubes -> Str
toStr = \cubes -> 
    toList cubes 
    |> List.map Num.toStr
    |> Str.joinWith ":"
    
toList = \cubes -> 
    [ cubes.red, cubes.green, cubes.blue]
    
all : List Bool -> Bool
all = \bools -> 
    bools
    |> List.walk Bool.true Bool.and

