module RandomCodeGenTest exposing (..)

import Test exposing (Test, describe)
import TestHelper exposing (codeGenTest, codeGenTestWithDependencies)


suite : Test
suite =
    describe "RandomGeneratorTodo"
        [ codeGenTest "Generates a generator for a basic custom type"
            [ """module A exposing (..)
import Random

type Foo =
    Foo
    
generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo =
    Foo
    
generator : Random.Generator Foo
generator =
    Random.constant Foo
"""
        , codeGenTest "Generates a generator for an inline record"
            [ """module A exposing (..)
import Random

generator : Random.Generator { a : Int, b : String }
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

generator : Random.Generator { a : Int, b : String }
generator =
    Random.map2 (\\a b -> { a = a, b = b}) 
        (Random.int Random.minInt Random.maxInt) 
        (Random.uniform "TODO: Define string options" [])
"""
        , codeGenTest "Generates a generator for a declared record"
            [ """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : String }

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : String }

generator : Random.Generator Foo
generator =
    Random.map2 Foo (Random.int Random.minInt Random.maxInt) (Random.uniform "TODO: Define string options" [])
"""
        , codeGenTest "Generates a generator for a declared record with > 5 fields"
            [ """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Random.constant Foo
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
"""
        , codeGenTestWithDependencies "Generates a generator for a declared record with > 5 fields nicer with random-extra"
            [ TestHelper.randomExtra ]
            [ """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random
import Random.Extra
import Random.Int

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Random.constant Foo
        |> Random.andMap Random.Int.anyInt
        |> Random.andMap Random.Int.anyInt
        |> Random.andMap Random.Int.anyInt
        |> Random.andMap Random.Int.anyInt
        |> Random.andMap Random.Int.anyInt
        |> Random.andMap Random.Int.anyInt
"""
        , codeGenTest "Generates a generator for a enum"
            [ """module A exposing (..)
import Random

type Foo =
  A | B

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo =
  A | B

generator : Random.Generator Foo
generator =
    Random.uniform A [ B ]
"""
        , codeGenTest "Generates a generator for a custom type"
            [ """module A exposing (..)
import Random

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Random.uniform
        (Random.map A (Random.int Random.minInt Random.maxInt))
        [ Random.map B (Random.uniform "TODO: Define string options" []) ]
        |> Random.andThen identity
"""
        , codeGenTestWithDependencies "Picks up random-extra for nicer code"
            [ TestHelper.randomExtra ]
            [ """module A exposing (..)
import Random

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random
import Random.Extra
import Random.Int

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Random.Extra.choices (Random.map A Random.Int.anyInt)
        [Random.map B (Random.uniform "TODO: Define string options" [])]
"""
        , codeGenTest "Picks up a generator from another file"
            [ """module A exposing (A, generator)
import Random

type A
  = A Int
 

generator : Random.Generator A
generator =
    Random.map A (Random.int Random.minInt Random.maxInt)
""", """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Random.map B A.generator
"""
        , codeGenTest "Picks up a generator from another file with different import notation"
            [ """module A exposing (A, generator)
import Random as R exposing (Generator)

type A
  = A Int
 

generator : Generator A
generator =
    R.map A (R.int R.minInt R.maxInt)
""", """module B exposing (..)
import Random exposing (Generator)
import A exposing (A)

type B =
    B A

generator : Generator B
generator =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Random exposing (Generator)
import A exposing (A)

type B =
    B A

generator : Generator B
generator =
    Random.map B A.generator
"""
        , codeGenTest "Generates a todo when no generator found"
            [ """module A exposing (A)
import Random

type A
  = A Int
 
""", """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Random.map B (Debug.todo "Insert a `Random.Generator A` here")
"""
        , codeGenTest "Generates a generator for a subtype"
            [ """module A exposing (..)
import Random

type A
    = A B

type B 
    = B Int

generator : Random.Generator A
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type A
    = A B

type B 
    = B Int

generator : Random.Generator A
generator =
    Random.map A randomB

randomB : Random.Generator B
randomB =
    Random.map B (Random.int Random.minInt Random.maxInt)"""
        ]
