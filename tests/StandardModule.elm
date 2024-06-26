module StandardModule exposing (standard)

-- Do not edit this module directly, it is generated from ../e2e/Standard.elm


standard : String
standard =
    """module Standard exposing (A(..), B)

import Array exposing (Array)
import Dict exposing (Dict)

type A b
    = Rec B
    | Gen b
    | Recursive (A b)

type alias B =
    { list : List Int 
    , array : Array String
    , dict : Dict String Float
    , tuple : (Maybe String, Bool) 
    , anon : {a : Int, b : Int}
    }"""
