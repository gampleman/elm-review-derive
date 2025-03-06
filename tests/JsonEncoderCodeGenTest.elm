module JsonEncoderCodeGenTest exposing (elmJson, suite)

import CodeGenerator.Test exposing (FakeDependency, codeGenTest, codeGenTestFailsWith)
import StandardModule
import Test exposing (Test, describe)


elmJson : FakeDependency
elmJson =
    CodeGenerator.Test.fakeDependency
        { name = "elm/json"
        , dependencies = []
        , modules =
            [ { name = "Json.Encode"
              , values =
                    [ ( "array", "(a -> Json.Encode.Value) -> Array.Array a -> Json.Encode.Value" )
                    , ( "bool", "Basics.Bool -> Json.Encode.Value" )
                    , ( "dict", "(k -> String.String) -> (v -> Json.Encode.Value) -> Dict.Dict k v -> Json.Encode.Value" )
                    , ( "encode", "Basics.Int -> Json.Encode.Value -> String.String" )
                    , ( "float", "Basics.Float -> Json.Encode.Value" )
                    , ( "int", "Basics.Int -> Json.Encode.Value" )
                    , ( "list", "(a -> Json.Encode.Value) -> List.List a -> Json.Encode.Value" )
                    , ( "null", "Json.Encode.Value" )
                    , ( "object", "List.List ( String.String, Json.Encode.Value ) -> Json.Encode.Value" )
                    , ( "set", "(a -> Json.Encode.Value) -> Set.Set a -> Json.Encode.Value" )
                    , ( "string", "String.String -> Json.Encode.Value" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "JsonEncoderTodo"
        [ codeGenTest "Generates a generator for an int"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

generator : Int -> Value
generator =  Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

generator : Int -> Value
generator =
    Json.Encode.int
"""
        , codeGenTest "Generates a generator for an int with an explicit arg"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

generator : Int -> Value
generator a =  Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

generator : Int -> Value
generator a =
    Json.Encode.int a
"""
        , codeGenTest "Generates an encoder for a basic custom type"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type Foo =
    Foo
    
encode : Foo -> Value
encode foo =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type Foo =
    Foo
    
encode : Foo -> Value
encode foo =
    case foo of
        Foo ->
            Json.Encode.string "Foo"
"""
        , codeGenTest "Generates an encoder for an inline record"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

encode : { a : Int, b : String } -> Value
encode ab =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

encode : { a : Int, b : String } -> Value
encode ab =
    Json.Encode.object [ ( "a", Json.Encode.int ab.a ), ( "b", Json.Encode.string ab.b ) ]
"""
        , codeGenTest "Generates an encoder for a declared record"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo =
  { a : Int, b : String }

encode : Foo -> Value
encode foo =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo =
  { a : Int, b : String }

encode : Foo -> Value
encode foo =
    Json.Encode.object [ ( "a", Json.Encode.int foo.a ), ( "b", Json.Encode.string foo.b ) ]
"""
        , codeGenTest "Generates an encoder for an enum"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type Foo =
  A | B

encode : Foo -> Value
encode foo =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type Foo =
  A | B

encode : Foo -> Value
encode foo =
    case foo of
        A ->
            Json.Encode.string "A"

        B ->
            Json.Encode.string "B"
"""
        , codeGenTest "Generates an encoder for a custom type"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type Foo
  = A Int
  | B String

encode : Foo -> Value
encode foo =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type Foo
  = A Int
  | B String

encode : Foo -> Value
encode foo =
    case foo of
        A argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "A" ), ( "0", Json.Encode.int argA ) ]

        B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", Json.Encode.string argA ) ]
"""
        , codeGenTest "Picks up an encoder from another file"
            [ elmJson ]
            []
            [ """module A exposing (A, encode)
import Json.Encode exposing (Value)

type A
  = A Int
 

encode : A -> Value
encode a =
    case a of
        A arg ->
            Json.Encode.int arg
""", """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B A

encode : B -> Value
encode b =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B A

encode : B -> Value
encode b =
    case b of
        B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", A.encode argA ) ]
"""
        , codeGenTest "Picks up a generator from another file with different import notation"
            [ elmJson ]
            []
            [ """module A exposing (A, encode)
import Json.Encode as Encode

type A
  = A Int
 

encode : A -> Encode.Value
encode a =
     case a of
        A arg ->
            Encode.int arg
""", """module B exposing (..)
import Json.Encode as Encode
import A exposing (A)

type B =
    B A

encode : B -> Encode.Value
encode b =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Json.Encode as Encode
import A exposing (A)

type B =
    B A

encode : B -> Encode.Value
encode b =
    case b of
        B argA ->
            Encode.object [ ( "tag", Encode.string "B" ), ( "0", A.encode argA ) ]
"""
        , codeGenTest "Picks up an encoder from another file with generics"
            [ elmJson ]
            []
            [ """module A exposing (A, encode)
import Json.Encode exposing (Value)

type A val
  = A val
 

encode : (val -> Value) -> A val -> Value
encode encodeVal a =
    case a of
        A arg ->
            encodeVal arg
""", """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B (A Int)

encode : B -> Value
encode b =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B (A Int)

encode : B -> Value
encode b =
    case b of
        B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", A.encode Json.Encode.int argA ) ]
"""
        , codeGenTest "Picks up an encoder from another file with generics type alias"
            [ elmJson ]
            []
            [ """module A exposing (A, encode)
import Json.Encode exposing (Value)

type alias A val
  = { a : val }
 

encode : (val -> Value) -> A val -> Value
encode encodeVal a =
    case a of
        A arg ->
            encodeVal arg
""", """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B (A Int)

encode : B Int -> Value
encode b =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B (A Int)

encode : B Int -> Value
encode b =
    case b of
        B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", A.encode Json.Encode.int argA ) ]
"""
        , codeGenTestFailsWith "Doesn't pick up an encoder if not exposed"
            [ elmJson ]
            []
            [ """module A exposing (A)
import Json.Encode exposing (Value)

type A val
  = A val
 

encode : (val -> Value) -> A val -> Value
encode encodeVal a =
    case a of
        A arg ->
            encodeVal arg
""", """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type B =
    B (A Int)

encode : B Int -> Value
encode b =
    Debug.todo ""
""" ]
            """Could not automatically generate a definition for `A`, as we don't know how to implement this type."""
        , codeGenTestFailsWith "Doesn't generate an encoder if type not sufficiently exposed"
            [ elmJson ]
            []
            [ """module A exposing (A, B(..))
import Json.Encode exposing (Value)


type A
  = A Int

type B =
    B String
 
""", """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A, B)

type C =
    C A B

encode : C -> Value
encode c =
    Debug.todo ""
""" ]
            """Could not automatically generate a definition for `A`, as we don't know how to implement this type."""
        , codeGenTest "Adds proper imports as needed"
            [ elmJson ]
            []
            [ """module A exposing (A(..), B(..))
import C

type A = 
    A B C.C

type B = 
    B Int


"""
            , """module C exposing (C(..))

type C =
    C Int

"""
            , """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type Foo =
    B A

encode : Foo -> Value
encode b =
    Debug.todo ""
"""
            ]
            """module B exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

import C

type Foo =
    B A

encode : Foo -> Value
encode b =
    case b of
        B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", encodeA argA ) ]

encodeA : A -> Value
encodeA arg =
    case arg of
        A.A argA argB ->
            Json.Encode.object [ ( "tag", Json.Encode.string "A" ), ( "0", encodeB argA ), ( "1", encodeC argB ) ]

encodeB : A.B -> Value
encodeB arg =
    case arg of
        A.B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", Json.Encode.int argA ) ]

encodeC : C.C -> Value
encodeC arg =
    case arg of
        C.C argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "C" ), ( "0", Json.Encode.int argA ) ]
"""
        , codeGenTestFailsWith "Fails when necessary sub-values don't have sufficient visibility"
            [ elmJson ]
            []
            [ """module A exposing (A(..), B)

type A = 
    A B 

-- Note: Type A is exposed here, but type B is private. 
type B = 
    B Int
"""
            , """module Foo exposing (..)
import Json.Encode exposing (Value)
import A exposing (A)

type Foo =
    B A

encode : Foo -> Value
encode b =
    Debug.todo ""
"""
            ]
            """Could not automatically generate a definition for `A.B`, as we don't know how to implement this type."""
        , codeGenTest "Generates a generator for a non-exposed value"
            [ elmJson ]
            []
            [ """module A exposing (main)
import Json.Encode exposing (Value)


type A =
    A Int

encode : A -> Value
encode =  Debug.todo ""

main =
    Json.Encode.encode 2 (encode (A 2))
""" ]
            """module A exposing (main)
import Json.Encode exposing (Value)


type A =
    A Int

encode : A -> Value
encode arg =
    case arg of
        A argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "A" ), ( "0", Json.Encode.int argA ) ]

main =
    Json.Encode.encode 2 (encode (A 2))
"""
        , codeGenTest "Picks up an encoder from same file with multiple generics"
            [ elmJson ]
            []
            [ """module A exposing (A, encode)
import Json.Encode exposing (Value)

type A
  = A (Result String Int)
 

encode : A val -> Value
encode encodeVal a =
   Debug.todo ""

encodeResult : (err -> Value) -> (ok -> Value) -> Result err ok -> Value
encodeResult err ok val =
    case val of
        Err argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Err" ), ( "0", err argA ) ]

        Ok argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Ok" ), ( "0", ok argA ) ]
""" ]
            """module A exposing (A, encode)
import Json.Encode exposing (Value)

type A
  = A (Result String Int)
 

encode : A val -> Value
encode encodeVal a =
    case a of
        A argA ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "A" ), ( "0", encodeResult Json.Encode.string Json.Encode.int argA ) ]

encodeResult : (err -> Value) -> (ok -> Value) -> Result err ok -> Value
encodeResult err ok val =
    case val of
        Err argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Err" ), ( "0", err argA ) ]

        Ok argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Ok" ), ( "0", ok argA ) ]
"""
        , codeGenTest "Picks up the definition of Result from dependency"
            [ elmJson ]
            []
            [ """module A exposing (A, encode)
import Json.Encode exposing (Value)

type A
  = A (Result String Int)
 

encode : A val -> Value
encode encodeVal a =
   Debug.todo ""
""" ]
            """module A exposing (A, encode)
import Json.Encode exposing (Value)

type A
  = A (Result String Int)
 

encode : A val -> Value
encode encodeVal a =
    case a of
        A argA ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "A" ), ( "0", encodeResult Json.Encode.string Json.Encode.int argA ) ]

encodeResult : (error -> Value) -> (value -> Value) -> Result error value -> Value
encodeResult error value arg =
    case arg of
        Result.Ok argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Ok" ), ( "0", value argA ) ]

        Result.Err argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Err" ), ( "0", error argA ) ]
"""
        , codeGenTest "Generates an encoder for a subtype"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type A
    = A B

type B 
    = B Int

encode : A -> Value
encode =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type A
    = A B

type B 
    = B Int

encode : A -> Value
encode arg =
    case arg of
        A argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "A" ), ( "0", encodeB argA ) ]

encodeB : B -> Value
encodeB arg =
    case arg of
        B argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", Json.Encode.int argA ) ]
"""
        , codeGenTest "recursive"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode as Encode exposing (Value)

type Tree a
    = Node (Tree a) a (Tree a)
    | Empty

encode : (a -> Value) -> Tree a -> Value
encode childEncoder tree =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode as Encode exposing (Value)

type Tree a
    = Node (Tree a) a (Tree a)
    | Empty

encode : (a -> Value) -> Tree a -> Value
encode childEncoder tree =
    case tree of
        Node argA argB argC ->
            Encode.object
                [ ( "tag", Encode.string "Node" )
                , ( "0", encode childEncoder argA )
                , ( "1", childEncoder argB )
                , ( "2", encode childEncoder argC )
                ]

        Empty ->
            Encode.string "Empty"
"""
        , codeGenTest "Generates an encoder for an inline extensible record"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

encode : { x | a : Int, b : String } -> Value
encode ab =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

encode : { x | a : Int, b : String } -> Value
encode ab =
    Json.Encode.object [ ( "a", Json.Encode.int ab.a ), ( "b", Json.Encode.string ab.b ) ]
"""
        , codeGenTest "Generates an encoder for an declared extensible record"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo x =
    { x | a : Int, b : String }

encode : Foo x -> Value
encode ab =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo x =
    { x | a : Int, b : String }

encode : Foo x -> Value
encode ab =
    Json.Encode.object [ ( "a", Json.Encode.int ab.a ), ( "b", Json.Encode.string ab.b ) ]
"""
        , codeGenTest "Generates an encoder for an declared applied extensible record"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo x =
    { x | a : Int, b : String }

encode : Foo { c : Int } -> Value
encode abc =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo x =
    { x | a : Int, b : String }

encode : Foo { c : Int } -> Value
encode abc =
    Json.Encode.object
        [ ( "a", Json.Encode.int abc.a ), ( "b", Json.Encode.string abc.b ), ( "c", Json.Encode.int abc.c ) ]
"""
        , codeGenTest "Generates an encoder for an declared declared applied extensible record"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo x =
    { x | a : Int, b : String }

type alias Bar =
    Foo { c : Int }

encode : Bar -> Value
encode abc =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo x =
    { x | a : Int, b : String }

type alias Bar =
    Foo { c : Int }

encode : Bar -> Value
encode abc =
    Json.Encode.object
        [ ( "a", Json.Encode.int abc.a ), ( "b", Json.Encode.string abc.b ), ( "c", Json.Encode.int abc.c ) ]
"""
        , codeGenTest "Generates an encoder with phantom types"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type Foo x =
    Foo Int

type Always =
    Always

encode : Foo { capabilites | x : Always } -> Value
encode foo =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type Foo x =
    Foo Int

type Always =
    Always

encode : Foo { capabilites | x : Always } -> Value
encode foo =
    case foo of
        Foo argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Foo" ), ( "0", Json.Encode.int argA ) ]
"""
        , codeGenTest "Generates an encoder with a list"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo =
    List Int

encode : Foo -> Value
encode =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)

type alias Foo =
    List Int

encode : Foo -> Value
encode =
    Json.Encode.list Json.Encode.int
"""
        , codeGenTest "Generates an encoder with container types"
            [ elmJson ]
            []
            [ """module A exposing (..)
import Json.Encode exposing (Value)
import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)

type alias Foo =
    { maybe : Maybe Int
    , list: List Int
    , array : Array Int
    , set : Set Int
    , strDict : Dict String String
    , intDict : Dict Int String
    , tuple : (Int, String)
    , triple : (Int, String, Char)
    }

encode : Foo -> Value
encode =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Json.Encode exposing (Value)
import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)

type alias Foo =
    { maybe : Maybe Int
    , list: List Int
    , array : Array Int
    , set : Set Int
    , strDict : Dict String String
    , intDict : Dict Int String
    , tuple : (Int, String)
    , triple : (Int, String, Char)
    }

encode : Foo -> Value
encode rec =
    Json.Encode.object
        [ ( "maybe", encodeMaybe Json.Encode.int rec.maybe )
        , ( "list", Json.Encode.list Json.Encode.int rec.list )
        , ( "array", Json.Encode.array Json.Encode.int rec.array )
        , ( "set", Json.Encode.set Json.Encode.int rec.set )
        , ( "strDict", Json.Encode.dict identity Json.Encode.string rec.strDict )
        , ( "intDict", Json.Encode.dict (Json.Encode.int >> Json.Encode.encode 0) Json.Encode.string rec.intDict )
        , ( "tuple"
          , Json.Encode.list
                identity
                [ Json.Encode.int (Tuple.first rec.tuple), Json.Encode.string (Tuple.second rec.tuple) ]
          )
        , ( "triple"
          , let
                ( first, second, third ) =
                    rec.triple
            in
            Json.Encode.list
                identity
                [ Json.Encode.int first, Json.Encode.string second, Json.Encode.string (String.fromChar third) ]
          )
        ]

encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe a arg =
    case arg of
        Just val ->
            a val

        Nothing ->
            Json.Encode.null
"""
        , codeGenTest "Standard"
            [ elmJson ]
            []
            [ StandardModule.standard
            , """module A exposing (..)
import Standard exposing (..)
import Json.Encode exposing (Value)

encode : A Int -> Value
encode =
    Debug.todo ""
            """
            ]
            """module A exposing (..)
import Standard exposing (..)
import Json.Encode exposing (Value)

encode : A Int -> Value
encode arg =
    case arg of
        Rec argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Rec" ), ( "0", encodeB argA ) ]

        Gen argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Gen" ), ( "0", Json.Encode.int argA ) ]

        Recursive argA ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Recursive" ), ( "0", encode argA ) ]

encodeB : B -> Value
encodeB rec =
    Json.Encode.object
        [ ( "list", Json.Encode.list Json.Encode.int rec.list )
        , ( "array", Json.Encode.array Json.Encode.string rec.array )
        , ( "dict", Json.Encode.dict identity Json.Encode.float rec.dict )
        , ( "tuple"
          , Json.Encode.list
                identity
                [ encodeMaybe Json.Encode.string (Tuple.first rec.tuple), Json.Encode.bool (Tuple.second rec.tuple) ]
          )
        , ( "anon", Json.Encode.object [ ( "a", Json.Encode.int rec.anon.a ), ( "b", Json.Encode.int rec.anon.b ) ] )
        ]

encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe a arg =
    case arg of
        Just val ->
            a val

        Nothing ->
            Json.Encode.null
"""
        ]
