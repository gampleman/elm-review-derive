# elm-review-todo-it-for-me

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

This rule isn't limited to generating toString functions for custom types though.

Here are some other type signatures it looks for

- `String -> Maybe MyType` generates a fromString function
- `Codec e MyType` generates a MartinSStewart/elm-serialize Codec
- `List MyType` generates a list containing all the custom type variants
- `Random.Generator MyType` generates an elm/random generator (unfinished)

## Notes

To be transformed into actual documentation...

### Function resolution priority

One way to think about the code generation the rule here does is a systematic way of composing other functions into larger units.
In order to do that, it has to get a good idea of what those functions are.

The way we solve this problem is to search your codebase for functions that have the right kind of signature, then use them in broader compositions.
However, it is not uncommon that multiple values with the right signature are found. elm-review-todo-it-for-me will pick one in based on the following ordering:

1. Values defined in the current file
2. Values defined in other project modules that the current file imports.
3. Values defined in dependency modules that the current file imports.
4. Values defined in dependencies.
5. Values defined in other project modules.
6. Values hard-coded in the code generator definition.

(Note that elm-review-todo-it-for-me will not pick up multiple values matching the same type in a single dependency module. This is a heuristic that indicates that
there are multiple ways to solve the same problem, and probably not something that can easily be automated.)

The ordering allows you to keep some control over the generation process by adding and or removing imports and/or local definitions. The other rationale is that values defined in other modules can often be quite domain specific and not necessarily suitable to be reused, whereas things defined in packages are intentionally made to be reused.

Within these categories, generators with fewer generics will be prioritized over those with more (so for instance `Foo (List Int)` will be picked over `Foo a -> Foo (List a)`).
