module CodeGenerator exposing
    ( CodeGenerator, define
    , Definition, ifUserHasDependency
    , int, float, string, char, list
    , succeed, map, mapN
    , customType
    , custom
    , combiner, dict, maybe, pipeline, triple, tuple, unit
    )

{-| This module let's you define (or change) type-oriented principled code generators.

By type oriented we mean generators that are driven by a type definition provided by the user.

By principled we mean that the generated code will for the foremost follow compositional patterns to be able to express (almost) any type.

@docs CodeGenerator, define


### Defining code generators

@docs Definition, ifUserHasDependency


### Primitives

@docs int, float, string, char, list


### Combining values

@docs succeed, map, mapN, pipeline\


### Dealing with custom types

@docs customType


### Going crazy

@docs custom

-}

import Elm.CodeGen as CG
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import GenericTodo exposing (CodeGenerator(..), Condition(..), Resolver, ResolverImpl(..))
import ResolvedType exposing (ResolvedType)
import TypePattern exposing (TypePattern)


{-| Represents a code generator configuration.
-}
type alias CodeGenerator =
    GenericTodo.CodeGenerator


{-| Create a code generator. This requires the following pieces:

  - a unique id. This id can be used to extend the generator later.
  - a dependency name which specifies what this generator deals with. This generator will only be active if the user has the dependency installed.
  - a search function that is used to figure out which type the function should work on.
  - a function that generates names if the generator needs to make an auxiliary definition
  - a list of Definitions that determine how code is actually generated. Note that later definitions will override previous ones.

The search function should return `Nothing` if the type annotation is not of interest. It should return a `Just childTypeAnnotation` if this generator wants to handle this type.

For example, if we were to build a generator for `Random.Generator someType` values (i.e. `Typed (Node _ ( [ "Random" ], "Generator" )) [ Node _ someType ]` in elm-syntax parlance), then this search function should return `Just someType`:

    searchFunction : TypeAnnotation -> Maybe TypeAnnotation
    searchFunction annotation =
        case annotation of
            Typed (Node _ ( [ "Random" ], "Generator" )) [ Node _ child ] ->
                Just child

            _ ->
                Nothing

Also note that you will always get module names normalized, i.e. you will always see `( [ "Random" ], "Generator" )` even if the user has `import Random as Foo exposing (Generator)`, so no need to worry about that.

-}
define : String -> String -> TypePattern -> (String -> String) -> List Definition -> CodeGenerator
define id dependency searchPattern makeName definitions =
    List.foldl
        (\(Definition resolver) thing ->
            { thing | resolvers = resolver :: thing.resolvers }
        )
        { id = id
        , searchPattern = searchPattern
        , resolvers = []
        , condition = Dependencies [ dependency ]
        , makeName = makeName
        }
        definitions
        |> Generic


{-| Definitions are a way to to generate and compose small snippets of code to handle specific situations that might occur in an Elm type.
Fundamentally you can think of all the definitions put together as forming a rather sophisticated function `ResolvedType -> Expression`, however this library will handle a large number of gotcha's for you, so it's more convenient to define the function piece-meal.
-}
type Definition
    = Definition Resolver


{-| Apply this definition conditionally if the user has this specific dependency installed (can be chained). Intended for things like json-pipeline or random-extra.
-}
ifUserHasDependency : String -> Definition -> Definition
ifUserHasDependency dependency (Definition resolver) =
    Definition
        { resolver
            | condition =
                case resolver.condition of
                    Always ->
                        Dependencies [ dependency ]

                    Dependencies existing ->
                        Dependencies (dependency :: existing)
        }


simpleDef : ResolverImpl -> Definition
simpleDef impl =
    Definition { implementation = impl, condition = Always }



-- Primitives


{-| Handle an `Int` type.
-}
int : Expression -> Definition
int =
    Just >> always >> always >> PrimitiveResolver { modulePath = [ "Basics" ], name = "Int" } >> simpleDef


{-| Handle a `Float` type.
-}
float : Expression -> Definition
float =
    Just >> always >> always >> PrimitiveResolver { modulePath = [ "Basics" ], name = "Float" } >> simpleDef


{-| Handle a `String` type.
-}
string : Expression -> Definition
string =
    Just >> always >> always >> PrimitiveResolver { modulePath = [ "String" ], name = "String" } >> simpleDef


{-| Handle a `Char` type.
-}
char : Expression -> Definition
char =
    Just >> always >> always >> PrimitiveResolver { modulePath = [ "Char" ], name = "Char" } >> simpleDef


unit : Expression -> Definition
unit =
    Just >> always >> always >> PrimitiveResolver { modulePath = [ "Basics" ], name = "()" } >> simpleDef


arg1Primitive : List String -> String -> (Expression -> Expression) -> Definition
arg1Primitive modPath name fn =
    PrimitiveResolver { modulePath = modPath, name = name }
        (\_ args ->
            case args of
                [ arg ] ->
                    Just (fn arg)

                _ ->
                    Nothing
        )
        |> simpleDef


{-| Handle a `List a` type. You will be given code that handles the `a` subtype.
-}
list : (Expression -> Expression) -> Definition
list =
    arg1Primitive [ "List" ] "List"


{-| Handle a `Maybe a` type. You will be given code that handles the `a` subtype.
-}
maybe : (Expression -> Expression) -> Definition
maybe =
    arg1Primitive [ "Maybe" ] "Maybe"


arg2Primitive : List String -> String -> (Expression -> Expression -> Expression) -> Definition
arg2Primitive modPath name fn =
    PrimitiveResolver { modulePath = modPath, name = name }
        (\_ args ->
            case args of
                [ arg0, arg1 ] ->
                    Just (fn arg0 arg1)

                _ ->
                    Nothing
        )
        |> simpleDef


dict : (Expression -> Expression -> Expression) -> Definition
dict =
    arg2Primitive [ "Dict" ] "Dict"



-- Combiners


{-| Wrap a value in the type. This is called different things in different libraries (i.e. `List.singleton`, `Random.constant`, etc.)
-}
succeed : (Expression -> Expression) -> Definition
succeed fn =
    Combiner
        (\_ exp args ->
            if List.isEmpty args then
                Just (fn exp)

            else
                Nothing
        )
        |> simpleDef


{-| Transform a value inside a type. You will be handed the arguments.
-}
map : (Expression -> Expression -> Expression) -> Definition
map fn =
    Combiner
        (\_ exp args ->
            if List.length args == 1 then
                List.head args |> Maybe.map (fn exp)

            else
                Nothing
        )
        |> simpleDef


{-| A convenient way to specify `map2`, `map3`, `map4`, etc.

The first argument specifies up to what number of arguments you want to specify the `mapN`.

The first argument in the callback is the standard name, so for 3 arguments you will get `"map3"`.

-}
mapN : Int -> (String -> Expression -> List Expression -> Expression) -> Definition
mapN max fn =
    Combiner
        (\_ exp args ->
            let
                n =
                    List.length args
            in
            if n <= max && n > 1 then
                Just (fn ("map" ++ String.fromInt n) exp args)

            else
                Nothing
        )
        |> simpleDef


{-| Deal with any number of arguments using applicative style. The first argument is like for succeed, the second is a partially applied `andMap`.
-}
pipeline : (Expression -> Expression) -> (Expression -> Expression) -> Definition
pipeline init cont =
    combiner
        (\_ exp args ->
            Just (CG.pipe (init exp) (List.map cont args))
        )


{-| Deals with 2-tuples (i.e. pairs). No need to implement if you have `map2`, as it will automatically be used.
-}
tuple : (Expression -> Expression -> Expression) -> Definition
tuple fn =
    combiner
        (\t _ args ->
            case t of
                ResolvedType.Tuple _ ->
                    case args of
                        [ arg0, arg1 ] ->
                            Just (fn arg0 arg1)

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )


{-| Deals with 3-tuples (i.e. triples). No need to implement if you have `map3`, as it will automatically be used.
-}
triple : (Expression -> Expression -> Expression -> Expression) -> Definition
triple fn =
    combiner
        (\t _ args ->
            case t of
                ResolvedType.Tuple _ ->
                    case args of
                        [ arg0, arg1, arg2 ] ->
                            Just (fn arg0 arg1 arg2)

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )


{-| If map, mapN, succeed, pipeline don't work for you, this is a more custom way to combine these.
-}
combiner : (ResolvedType -> Expression -> List Expression -> Maybe Expression) -> Definition
combiner fn =
    Combiner
        fn
        |> simpleDef



-- Custom types


{-| Deal with custom types. You will get a list of `( constructorName, expressionThatGeneratesTheTypeWithThatConstructor )`.

The challenge is to work out which of the branches should be chosen. You can solve that with a `andThen`, or the library might have a different mechanism for disjunctions.

-}
customType : (List ( ResolvedType.Reference, List ResolvedType ) -> List ( String, Expression ) -> Expression) -> Definition
customType fn =
    CustomTypeResolver fn |> simpleDef


{-| This allows you complete freedom in generating expressions, however it also doesn't give you much help.

The recommendation here is to use the normal definitions and only use this for exceptional cases.

-}
custom : (ResolvedType -> Maybe Expression) -> Definition
custom fn =
    UniversalResolver fn |> simpleDef
