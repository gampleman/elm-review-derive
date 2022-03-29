module ResolvedType exposing (ResolvedType(..), Reference)

{-| This module is at the heart of the code generation process.
This library works by gathering all the information necessary to build this type,
then the code generator's job is to translate this type into an AST.

@docs ResolvedType, Reference

-}


{-| Represents a fully qualified name in Elm source code.
-}
type alias Reference =
    { modulePath : List String
    , name : String
    }


{-| Represents an Elm type that has been fully resolved:

  - all references are now in absolute module format
  - type variable applications have been applied
  - custom type and type alias information has been added

The constructors are:

  - `GenericType "foo" ref` represents an unfilled type variable `foo`
  - `Opaque ref vars` represents a custom type (or built in type)
  - `Function args return` represents a function type
  - `TypeAliasRecord ref args definition` represents a record where we have a type alias (and therefore a constructor) available
  - `AnonymouseRecord extensionVar definition` represents an anonymous record
  - `CustomType ref args [(constructorRef, arguments)]` represents a reference to an exposed (or accessible) custom type
  - `Tuple args` represents a tuple.

Let's look at some simple examples and see how they would be represented:

    Int --> Opaque { modulePath = ["Basics"], name = "Int" } []

    List Int --> Opaque { modulePath = ["Basics"], name = "List" } [Opaque { modulePath = ["Basics"], name = "Int" } []]

    List a --> Opaque { modulePath = ["Basics"], name = "List" } [ GenericType "a" Nothing ]

    Int -> String  --> Function [ Opaque { modulePath = ["Basics"], name = "Int" } [] ] Opaque { modulePath = ["String"], name = "String" } []

    { foo : Int } --> AnonymousRecord Nothing [ ( "foo",  Opaque { modulePath = ["Basics"], name = "Int" } [] ) ]

    { x | foo : Int } --> AnonymousRecord (Just "x") [ ( "foo",  Opaque { modulePath = ["Basics"], name = "Int" } [] ) ]

    () -> Tuple []

    ( Int, String ) --> Tuple [ Opaque { modulePath = ["Basics"], name = "Int" } [], Opaque { modulePath = ["String"], name = "String" } [] ]

Now for some more complex examples:

    type Foo a =
        Bar a

    Foo a --> CustomType { modulePath = [], name = "Foo" } [ "a" ] [ ( { modulePath = [], name = "Bar" }, [ GenericType "a" Nothing ] ) ]

    Foo Int --> CustomType { modulePath = [], name = "Foo" } [ "a" ] [ ( { modulePath = [], name = "Bar" }, [ GenericType "a" (Just (Opaque { modulePath = ["Basics"], name = "Int" } [] ))] ) ]

Note how in `Foo Int` the `Int` value is replaced in the definition.

    type alias Qux a =
        { foo : a }

    Qux a --> TypeAlias { modulePath = [], name = "Qux" } [ "a" ] [( "foo", GenericType "a" Nothing )]
    Qux Int --> TypeAliasRecord { modulePath = [], name = "Qux" } [ "a" ] [( "foo", GenericType "a" (Just (Opaque { modulePath = ["Basics"], name = "Int" } [] )))]

-}
type ResolvedType
    = GenericType String (Maybe ResolvedType)
    | Opaque Reference (List ResolvedType)
    | Function (List ResolvedType) ResolvedType
    | TypeAlias Reference (List String) ResolvedType
    | AnonymousRecord (Maybe String) (List ( String, ResolvedType ))
    | CustomType Reference (List String) (List ( Reference, List ResolvedType ))
    | Tuple (List ResolvedType)
