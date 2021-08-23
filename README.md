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
* `String -> Maybe MyType` generates a fromString function
* `Codec e MyType` generates a MartinSStewart/elm-serialize Codec
* `List MyType` generates a list containing all the custom type variants
* `Random.Generator MyType` generates an elm/random generator (unfinished)