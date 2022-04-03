module JsonEncoderCodeGenTest exposing (..)

import CodeGenerator.Test exposing (codeGenTest)
import Review.Project.Dependency exposing (Dependency)
import Test exposing (Test, describe)


elmJson : Dependency
elmJson =
    CodeGenerator.Test.fakeDependency "elm/json"


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
        A arg0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "A" ), ( "0", Json.Encode.int arg0 ) ]

        B arg0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", Json.Encode.string arg0 ) ]
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
        B arg0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", A.encode arg0 ) ]
"""
        , codeGenTest "Picks up a generator from another file with different import notation"
            [ elmJson ]
            []
            [ """module A exposing (A, generator)
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
        B arg0 ->
            Encode.object [ ( "tag", Encode.string "B" ), ( "0", A.encode arg0 ) ]
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
        A arg0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "A" ), ( "0", encodeB arg0 ) ]

encodeB : B -> Value
encodeB arg =
    case arg of
        B arg0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "B" ), ( "0", Json.Encode.int arg0 ) ]
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
        Node arg0 arg1 arg2 ->
            Encode.object
                [ ( "tag", Encode.string "Node" )
                , ( "0", encode childEncoder arg0 )
                , ( "1", childEncoder arg1 )
                , ( "2", encode childEncoder arg2 )
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
        Foo arg0 ->
            Json.Encode.object [ ( "tag", Json.Encode.string "Foo" ), ( "0", Json.Encode.int arg0 ) ]
"""
        ]
