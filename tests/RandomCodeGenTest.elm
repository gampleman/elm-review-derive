module RandomCodeGenTest exposing (..)

import Review.Project.Dependency exposing (Dependency)
import Test exposing (Test, describe)
import TestHelper exposing (codeGenTest, codeGenTestFailsWith)


elmRandom : Dependency
elmRandom =
    TestHelper.fakeDependency "elm/random"


randomExtra : Dependency
randomExtra =
    TestHelper.fakeDependency "elm-community/random-extra"


suite : Test
suite =
    describe "RandomGeneratorTodo"
        [ codeGenTest "Generates a generator for a int"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

generator : Random.Generator Int
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

generator : Random.Generator Int
generator =
    Random.int Random.minInt Random.maxInt
"""
        , codeGenTest "Generates a generator for a basic custom type"
            [ elmRandom ]
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
            [ elmRandom ]
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
    Random.map2
        (\\a b -> { a = a, b = b })
        (Random.int Random.minInt Random.maxInt)
        (Random.uniform "TODO: Define string options" [])
"""
        , codeGenTest "Generates a generator for a declared record"
            [ elmRandom ]
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
            [ elmRandom ]
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
        , codeGenTest "Generates a generator for a declared record with > 5 fields nicer with random-extra"
            [ elmRandom, randomExtra ]
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
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
"""
        , codeGenTest "Generates a generator for a enum"
            [ elmRandom ]
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
            [ elmRandom ]
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
        , codeGenTest "Picks up random-extra for nicer code"
            [ elmRandom, randomExtra ]
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
    Random.Extra.choices
        (Random.map A Random.Int.anyInt)
        [ Random.map B (Random.uniform "TODO: Define string options" []) ]
"""
        , codeGenTest "Picks up a generator from another file"
            [ elmRandom ]
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
            [ elmRandom ]
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
        , codeGenTestFailsWith "Fails when no way to generate opaque type"
            [ elmRandom ]
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
            "Could not automatically generate a definition for `A`, as we don't know how to implement this type."
        , codeGenTest "Generates a generator for a subtype"
            [ elmRandom ]
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
    Random.map B (Random.int Random.minInt Random.maxInt)
"""
        , codeGenTest "Applied generic"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator (Foo Int)
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator (Foo Int)
generator =
    Random.map Foo (Random.int Random.minInt Random.maxInt)
"""
        , codeGenTest "full generic 1 arg"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator a -> Random.Generator (Foo a)
generator a =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator a -> Random.Generator (Foo a)
generator a =
    Random.map Foo a
"""
        , codeGenTest "full generic 1 arg with aliasing"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator b -> Random.Generator (Foo b)
generator x =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator b -> Random.Generator (Foo b)
generator x =
    Random.map Foo x
"""
        , codeGenTest "full generic 2 arg"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

type Foo a b
    = Foo a b

generator : Random.Generator a -> Random.Generator b -> Random.Generator (Foo a b)
generator a b =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a b
    = Foo a b

generator : Random.Generator a -> Random.Generator b -> Random.Generator (Foo a b)
generator a b =
    Random.map2 Foo a b
"""
        , codeGenTest "full generic 2 arg aliasing"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

type Foo a b
    = Foo b a

generator : Random.Generator x -> Random.Generator y -> Random.Generator (Foo y x)
generator d s =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a b
    = Foo b a

generator : Random.Generator x -> Random.Generator y -> Random.Generator (Foo y x)
generator d s =
    Random.map2 Foo d s
"""
        , codeGenTest "partial generic"
            [ elmRandom ]
            [ """module A exposing (..)
import Random

type alias Bar x =
    { x : x }

type Foo a
    = Foo (Bar a) (Bar Int)

generator : Random.Generator x -> Random.Generator (Foo x)
generator y =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type alias Bar x =
    { x : x }

type Foo a
    = Foo (Bar a) (Bar Int)

generator : Random.Generator x -> Random.Generator (Foo x)
generator y =
    Random.map2 Foo (randomBar y) (randomBar (Random.int Random.minInt Random.maxInt))

randomBar : Random.Generator x -> Random.Generator (Bar x)
randomBar x =
    Random.map Bar x
"""
        ]
