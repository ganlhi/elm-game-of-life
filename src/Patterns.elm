module Patterns exposing (Pattern, all, get)


type alias Pattern =
    { name : String
    , rle : String
    }


all : List Pattern
all =
    [ { name = "Octagon2", rle = "3b2o3b$2bo2bo2b$bo4bob$o6bo$o6bo$bo4bob$2bo2bo2b$3b2o!" }
    , { name = "Blinker", rle = "3o!" }
    , { name = "Glider", rle = "bob$2bo$3o!" }
    , { name = "Time bomb", rle = "bo11b2o$obo4bo6bo$7bo4bo2b$2bo2bo3bo2bo2b$2b2o6bo4b$3bo!" }
    ]


get : String -> Maybe Pattern
get name =
    List.filter (\p -> p.name == name) all |> List.head
