module TypePattern exposing (TypePattern(..))

{-|

@docs TypePattern

-}

-- TODO: Figure out how to handle generics. In some cases we may want to have them behave like wildcards, i.e. say for the  error type in a serializer


{-| A type pattern represents a query over a type annotation. We use these to identify objects of interest in the users code, namely:

1.  Definitions with `Debug.todo` that we want to turn into code.
2.  Definitions that provide functionality that generated code might want to hook into.

However, patterns can also be run in reverse, i.e. these can also be used to generate type annotations for auxiliary defintions.

The most important constructor here is `Target`, which denotes the type that will drive the code generation process.

`Typed modulePath name arguments` represents a type application, like `Random.Generator target` could be `Typed ["Random"] "Generator" [ Target ]`.

`Function` represents a `->` in a type. These are right-associative so `a -> b -> c` would be `Function (GenericType "a") (Function (GenericType "b") (GenericType "c"))`.

`GenericType` takes a variable name. This name doesn't need to match the name of a type (so `Typed [] "List" (GenericType "A")` **will match** `List x`), but **do** need to match each other. So `Function (GenericType "a") (GenericType "a")` will match `x -> x` but will not match `x -> y`.

-}
type TypePattern
    = Target
    | Typed (List String) String (List TypePattern)
    | Function TypePattern TypePattern
    | GenericType String
    | Tuple (List TypePattern)
    | Record (List ( String, TypePattern ))
