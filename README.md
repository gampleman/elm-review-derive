# elm-review-derive

This elm-review rule is special in that it's not really a rule that checks for mistakes.  
Instead it looks for code like this

```elm
type MyType = A | B | C

myTypeToString : MyType -> String
myTypeToString myType = Debug.todo ""
```

and offers a fix that transforms the code into this

```elm
type MyType = A | B | C

myTypeToString : MyType -> String
myTypeToString myType =
    case myType of
        A -> "A"
        B -> "B"
        C -> "C"
```

It knows to do this because it sees a top level function containing `Debug.todo ""` and `MyType -> String` as the type signature.

## What else can it do?

This rule isn't limited to generating toString functions for custom types though. It can handle the following signatures:

### `Serialize.Codec e MyType`

Required package: [MartinSStewart/elm-serialize]  
Type features supported: **All\***

### `Codec.Codec e MyType`

Required package: [miniBill/elm-codec]  
Type features supported: **All\***

### `Csv.Decode.Decoder MyType`

Required package: [BrianHicks/elm-csv]  
Type features supported: Supports (non-recursive) **custom types and type aliases** as well as most primitives. Doesn't support collection types.

### `String -> Maybe MyType`

Required package: [elm/core]  
Type features supported: **Enums\*\***

### `Fuzz.Fuzzer MyType`

Required package: [elm-explorations/test]  
Type features supported: **All\***

### `Json.Decode.Decoder MyType`

Required package: [elm/json]  
Type features supported: **All\***

### `MyType -> Json.Encode.Value`

Required package: [elm/json]  
Type features supported: **All\***

### `List MyType`

Required package: [elm/core]  
Type features supported: **Enums\*\***

### `Random.Generator MyType`

Required package: [elm/random]  
Type features supported: **All\***  
Optional packages: [elm-community/random-extra] supported - it will use `Random.Extra.anyInt`, `Random.Extra.choices` and `Random.Extra.andThen` if you have the package installed.

### `MyType -> String`

Required package: [elm/core]  
Type features supported: **Enums\*\***

\* All types actually supported by the relevant packages. Generally this means that it won't support function types, nor opaque
types that don't provide a compatible signature (i.e. a `Decoder { foo : Color }` will fail of `Color` is an opaque type,
unless there is a `Decoder Color` available in the project or dependencies).

\*\* By Enum we mean a custom type where none of the constructors take any arguments. (i.e. `type Semaphore = Red | Yellow | Green`).

[MartinSStewart/elm-serialize]: https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/
[miniBill/elm-codec]: https://package.elm-lang.org/packages/miniBill/elm-codec/latest/
[BrianHicks/elm-csv]: https://package.elm-lang.org/packages/BrianHicks/elm-csv/latest/
[elm/core]: https://package.elm-lang.org/packages/elm/core/latest/
[elm-explorations/test]: https://package.elm-lang.org/packages/elm-explorations/test/latest/
[elm/json]: https://package.elm-lang.org/packages/elm/json/latest/
[elm/random]: https://package.elm-lang.org/packages/elm/random/latest/
[elm-community/random-extra]: https://package.elm-lang.org/packages/elm-community/random-extra/latest/

## When will this rule trigger?

It will always flag any `Debug.todo`, but it will only attempt to generate code for top-level defintions with type annotations matching
one of the patterns above.

However, the generator has good support for generics, so you can do things like this:

```elm
import Json.Decode as Decode exposing (Decoder)

type MyTree a
    = Branch (MyTree a) a (MyTree a)
    | Leaf

decoder : Decoder a -> Decoder (MyTree a)
decoder childDecoder =
    Debug.todo ""
```

and you'll get:

```elm
decoder : Decoder a -> Decoder (MyTree a)
decoder childDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\ctor ->
                case ctor of
                    "Branch" ->
                        Decode.map3
                            Branch
                            (Decode.field "0" (decoder childDecoder))
                            (Decode.field "1" childDecoder)
                            (Decode.field "2" (decoder childDecoder))

                    "Leaf" ->
                        Decode.succeed Leaf

                    _ ->
                        Decode.fail "Unrecognized constructor"
            )
```

More formally, for every type variable in the target type, you can provide an argument that provides the same matching pattern for that type variable.
(Multiple arguments can be passed in any order, it is the matching type variable names that matter).

## Great, how do I use this?

You can add it to your review config like any elm-review rule. However, I like to invoke it from the command line when I want it. You can do that with:

```bash
npx elm-review --fix --template gampleman/elm-review-derive/preview
```

If you like it, you can save that command in a convenient shell alias.

```bash
alias elm-derive='npx elm-review --fix --template gampleman/elm-review-derive/preview'
```

## How stable is it? Are there bugs?

It seems to work reasonably well. That said, it is a rather complex piece of code and testing it in anger on large codebases is likely to surface things
the authors haven't quite thought of. As such, we consider it Beta level software.

## But what about some other thing I'd like to generate?

The infrastructure we provide is extensible. It is somewhat involved, so we don't expect every user to need to understand how this works.

Still here? OK, let's see how this works.

Conceptually each generator is composed of two important pieces.

1. The **Type Pattern** describes a pattern that can match type annotations in a user's code base.
   Each Type Pattern has a `Target` in it, which is the user's type you care about and usually some other bits like some concrete wrapper type.
2. The actually code generation function, which can be conceptually understood as a function `ResolvedType -> Expression`.
   `Expression` in this sense comes from [still4m/elm-syntax](https://package.elm-lang.org/packages/stil4m/elm-syntax/latest/).
   `ResolvedType` is what this codebase spends much of its time and effort doing. It represents an actual type (as opposed to a type signature);
   that is:

   - it resolves type aliases to their actual definitions
   - it resolves custom types to include all their constructors and their arguments
   - it normalizes all names to a full module path (so you don't have to worry about how exactly the user imported them)
   - it resolves visibility of all types (so if a type is opaque and in another module, you won't see it's defintion)
   - it applies generic arguments

   The upshot is that you can use a `ResolvedType` as a reliable guide to generate sensible code from.

However, since manually building the entire `ResolvedType -> Expression` function is a fair amount of work with many corner cases, it practice we define
it piece meal. Here is the entire definition of the code generator for `Fuzzer`s:

```elm
module Internal.Builtin.Fuzzer exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import TypePattern exposing (TypePattern(..))


fuzz : String -> CG.Expression
fuzz =
    CG.fqFun [ "Fuzz" ]


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define
        { id = "elm-explorations/test/Fuzz.Fuzzer"
        , dependency = "elm-explorations/test"
        , typePattern = Typed [ "Fuzz" ] "Fuzzer" [ Target ]
        , makeName = \name -> "fuzz" ++ name
        }
        [ CodeGenerator.customType (\_ exps -> CG.apply [ fuzz "oneOf", CG.list (List.map Tuple.second exps) ])
        , CodeGenerator.pipeline (\c -> CG.apply [ fuzz "constant", c ]) (\m -> CG.apply [ fuzz "andMap", m ])
        , CodeGenerator.mapN 8 (\name a bs -> CG.apply (fuzz name :: a :: bs))
        , CodeGenerator.map (\a b -> CG.apply [ fuzz "map", a, b ])
        , CodeGenerator.string (fuzz "string")
        , CodeGenerator.dict (\key val -> CG.apply [ fuzz "map", CG.fqFun [ "Dict" ] "fromList", CG.apply [ fuzz "list", CG.apply [ fuzz "pair", key, val ] ] ])
        , CodeGenerator.float (fuzz "niceFloat")
        , CodeGenerator.tuple (\a b -> CG.apply [ fuzz "pair", a, b ])
        , CodeGenerator.triple (\a b c -> CG.apply [ fuzz "triple", a, b, c ])
        , CodeGenerator.char (fuzz "char")
        , CodeGenerator.lambdaBreaker (\inner -> CG.apply [ fuzz "lazy", CG.lambda [ CG.unitPattern ] inner ])
        ]
```

The meat of it is about 20 lines of code.

The **Type Pattern** is `Typed [ "Fuzz" ] "Fuzzer" [ Target ]`, which can be read as `Fuzz.Fuzzer <TARGET>`.
This will have the effect of searching the whole codebase for things with that type annotations.

Whenever it finds something matching this pattern, it checks the implementation. If the implementation is just a `Debug.todo`,
then it will try to run the generator to autofix is. If it's anything else, then it will resolve the `<TARGET>` and save it.
This means that the generator will pick up existing definitions and will try to use them in appropriate places.

The code generator has some smarts about creating auxiliary defintions. In order to do that, it uses the `makeName` function to name
these auxiliary definitions and the type pattern to create its type definition.

## Contributing

Contributions welcome. To run the tests, please run `e2e/test.sh`, then you can also run `elm-test` as normal.

MIT License

## History

This package was initially conceived and implemented by @MartinSStewart with a slightly different focus (there was some tooling to generate Lamdera migrations for instance) and a more custom architecture.
It was called MartinSStewart/elm-review-todo-it-for-me.

@gampleman then took the project over with some refactoring work on a more general architecture focused on type-oriented code generation. He renamed the project to be slightly less unwieldy to pronounce.
