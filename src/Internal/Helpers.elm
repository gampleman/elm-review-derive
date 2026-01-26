module Internal.Helpers exposing (applyBindings, hasDebugTodo, intToLetter, lambda1, node, rangeContains, traverseExpression, traversePattern, traverseTypeAnnotation, writeExpression)

import Dict
import Elm.CodeGen as CG
import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Pretty


writeExpression : Expression -> String
writeExpression =
    Elm.Pretty.prettyExpression >> Pretty.pretty 120


node : a -> Node a
node =
    Node Elm.Syntax.Range.emptyRange


rangeContains : Range -> Range -> Bool
rangeContains inner outer =
    Elm.Syntax.Range.compareLocations inner.start outer.start /= LT && Elm.Syntax.Range.compareLocations inner.end outer.end /= GT


{-| This is a really dumb little helper that prevents variable shadowing in nested lambdas.

It would be nice to have a proper solution that generates nice variable names like arg0, arg1, arg2, etc. and only when necessary.

But for now this will do.

-}
lambda1 : String -> (Expression -> Expression) -> Expression
lambda1 prefix exprBuilder =
    let
        inner =
            exprBuilder (CG.val prefix)

        -- This is real dumb, but it should solve the problem of overlapping scopes, since outer scopes should be longer then inner scopes
        simpleHash =
            writeExpression inner |> String.length |> String.fromInt

        name =
            prefix ++ simpleHash
    in
    CG.lambda [ CG.varPattern name ]
        (applyBindings (Dict.singleton prefix (CG.val name)) inner)


{-| Given an Expression and a dictionary of `names` to `Expression`, it modifies the expression such that
any encountered variable macthing one of the `names` is replaced by the corresponding `Expression`.

One of the gifts of referential transparency!

Given

    a * a

and

    a : (1 + 1)

produces

    (1 + 1) * (1 + 1)

-}
applyBindings : Dict.Dict String Expression -> Expression -> Expression
applyBindings bindings =
    traverseExpression
        (\subexpr foo ->
            case subexpr of
                Expression.FunctionOrValue [] name ->
                    case Dict.get name bindings of
                        Just ((Expression.Application _) as r) ->
                            ( CG.parens r, foo )

                        Just r ->
                            ( r, foo )

                        Nothing ->
                            ( subexpr, foo )

                _ ->
                    ( subexpr, foo )
        )
        ()
        >> Tuple.first


hasDebugTodo : { a | expression : Node Expression } -> Bool
hasDebugTodo declaration =
    case declaration.expression of
        Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
            True

        _ ->
            False



--- Temporary, to be contributed to elm-syntax-dsl:


traverseExpression : (Expression -> a -> ( Expression, a )) -> a -> Expression -> ( Expression, a )
traverseExpression fn initial expression =
    let
        ( newExpression, accumulator ) =
            fn expression initial

        traverseNode (Node n expr) accu =
            traverseExpression fn accu expr
                |> Tuple.mapFirst (Node n)

        traverseList =
            List.foldr
                (\(Node n expr) ( soFar, accu ) ->
                    let
                        ( newExpr, newAccu ) =
                            traverseExpression fn accu expr
                    in
                    ( Node n newExpr :: soFar, newAccu )
                )
                ( [], accumulator )
    in
    case newExpression of
        Application nexprs ->
            traverseList nexprs
                |> Tuple.mapFirst Application

        OperatorApplication op dir (Node ln le) (Node rn re) ->
            let
                ( newLe, accu ) =
                    traverseExpression fn accumulator le

                ( newRe, newAccu ) =
                    traverseExpression fn accu re
            in
            ( OperatorApplication op dir (Node ln newLe) (Node rn newRe), newAccu )

        IfBlock a0 b0 c0 ->
            let
                ( a1, accu1 ) =
                    traverseNode a0 accumulator

                ( b1, accu2 ) =
                    traverseNode b0 accu1

                ( c1, accu3 ) =
                    traverseNode c0 accu2
            in
            ( IfBlock a1 b1 c1, accu3 )

        Negation n ->
            traverseNode n accumulator |> Tuple.mapFirst Negation

        TupledExpression nexprs ->
            traverseList nexprs
                |> Tuple.mapFirst TupledExpression

        ParenthesizedExpression e ->
            traverseNode e accumulator |> Tuple.mapFirst ParenthesizedExpression

        LetExpression letBlockExpr ->
            let
                ( expr, accu0 ) =
                    traverseNode letBlockExpr.expression accumulator

                ( declarations, accu1 ) =
                    List.foldr
                        (\(Node n decl) ( soFar, accu ) ->
                            let
                                ( newDecl, newAccu ) =
                                    case decl of
                                        LetFunction func ->
                                            let
                                                (Node fdn funcDecl) =
                                                    func.declaration
                                            in
                                            traverseNode funcDecl.expression accu
                                                |> Tuple.mapFirst
                                                    (\fde ->
                                                        LetFunction { func | declaration = Node fdn { funcDecl | expression = fde } }
                                                    )

                                        LetDestructuring patterns le ->
                                            traverseNode le accu
                                                |> Tuple.mapFirst (LetDestructuring patterns)
                            in
                            ( Node n newDecl :: soFar, newAccu )
                        )
                        ( [], accu0 )
                        letBlockExpr.declarations
            in
            ( LetExpression { expression = expr, declarations = declarations }, accu1 )

        CaseExpression ce ->
            let
                ( expr, _ ) =
                    traverseNode ce.expression accumulator

                ( cases, accu1 ) =
                    List.foldr
                        (\( name, Node nb be ) ( soFar, accu ) ->
                            let
                                ( newExpr, newAccu ) =
                                    traverseExpression fn accu be
                            in
                            ( ( name, Node nb newExpr ) :: soFar, newAccu )
                        )
                        ( [], accumulator )
                        ce.cases
            in
            ( CaseExpression { expression = expr, cases = cases }, accu1 )

        LambdaExpression lambdaExpr ->
            let
                ( res, accu ) =
                    traverseNode lambdaExpr.expression accumulator
            in
            ( LambdaExpression { lambdaExpr | expression = res }, accu )

        RecordExpr rec ->
            List.foldr
                (\(Node n ( name, Node nb be )) ( soFar, accu ) ->
                    let
                        ( newExpr, newAccu ) =
                            traverseExpression fn accu be
                    in
                    ( Node n ( name, Node nb newExpr ) :: soFar, newAccu )
                )
                ( [], accumulator )
                rec
                |> Tuple.mapFirst RecordExpr

        ListExpr nexprs ->
            traverseList nexprs
                |> Tuple.mapFirst ListExpr

        RecordAccess expr str ->
            traverseNode expr accumulator
                |> Tuple.mapFirst (\r -> RecordAccess r str)

        RecordUpdateExpression rec setters ->
            List.foldr
                (\(Node n ( name, Node nb be )) ( soFar, accu ) ->
                    let
                        ( newExpr, newAccu ) =
                            traverseExpression fn accu be
                    in
                    ( Node n ( name, Node nb newExpr ) :: soFar, newAccu )
                )
                ( [], accumulator )
                setters
                |> Tuple.mapFirst (RecordUpdateExpression rec)

        _ ->
            ( newExpression, accumulator )


traverseTypeAnnotation : (TypeAnnotation -> a -> ( TypeAnnotation, a )) -> a -> TypeAnnotation -> ( TypeAnnotation, a )
traverseTypeAnnotation fn initial annotation =
    let
        ( newAnnotation, accumulator ) =
            fn annotation initial

        defaultMapper fx expr =
            fx expr

        traverseList mapper =
            List.foldr
                (\(Node n expr) ( soFar, accu ) ->
                    let
                        ( newExpr, newAccu ) =
                            mapper
                                (\inExpr ->
                                    traverseTypeAnnotation fn accu inExpr
                                )
                                expr
                    in
                    ( Node n newExpr :: soFar, newAccu )
                )
                ( [], accumulator )
    in
    case newAnnotation of
        Typed ref args ->
            traverseList defaultMapper args
                |> Tuple.mapFirst (Typed ref)

        Tupled args ->
            traverseList defaultMapper args
                |> Tuple.mapFirst Tupled

        Record args ->
            traverseList
                (\fx ( key, Node nv val ) ->
                    Tuple.mapFirst (\res -> ( key, Node nv res )) (fx val)
                )
                args
                |> Tuple.mapFirst Record

        GenericRecord generic (Node n args) ->
            traverseList
                (\fx ( key, Node nv val ) ->
                    Tuple.mapFirst (\res -> ( key, Node nv res )) (fx val)
                )
                args
                |> Tuple.mapFirst (Node n >> GenericRecord generic)

        FunctionTypeAnnotation (Node lr lv0) (Node rr rv0) ->
            let
                ( lv1, accu1 ) =
                    traverseTypeAnnotation fn accumulator lv0

                ( rv1, accu2 ) =
                    traverseTypeAnnotation fn accu1 rv0
            in
            ( FunctionTypeAnnotation (Node lr lv1) (Node rr rv1), accu2 )

        _ ->
            ( newAnnotation, accumulator )


traversePattern : (Pattern -> a -> ( Pattern, a )) -> a -> Pattern -> ( Pattern, a )
traversePattern fn initial pattern =
    let
        ( newPattern, accumulator ) =
            fn pattern initial

        traverseList wrapper list =
            List.foldr
                (\(Node n expr) ( soFar, accu ) ->
                    let
                        ( newExpr, newAccu ) =
                            traversePattern fn accu expr
                    in
                    ( Node n newExpr :: soFar, newAccu )
                )
                ( [], accumulator )
                list
                |> Tuple.mapFirst wrapper
    in
    case newPattern of
        TuplePattern args ->
            traverseList TuplePattern args

        UnConsPattern (Node lr lv0) (Node rr rv0) ->
            let
                ( lv1, accu1 ) =
                    traversePattern fn accumulator lv0

                ( rv1, accu2 ) =
                    traversePattern fn accu1 rv0
            in
            ( UnConsPattern (Node lr lv1) (Node rr rv1), accu2 )

        ListPattern args ->
            traverseList ListPattern args

        NamedPattern ref args ->
            traverseList (NamedPattern ref) args

        AsPattern (Node lr lv) str ->
            traversePattern fn accumulator lv
                |> Tuple.mapFirst (\res -> AsPattern (Node lr res) str)

        ParenthesizedPattern (Node lr lv) ->
            traversePattern fn accumulator lv
                |> Tuple.mapFirst (\res -> ParenthesizedPattern (Node lr res))

        _ ->
            ( newPattern, accumulator )


lettersInAlphabet : number
lettersInAlphabet =
    26


intToLetter : Int -> String
intToLetter int =
    (if int < lettersInAlphabet then
        ""

     else
        String.fromChar (Char.fromCode (65 - 1 + int // lettersInAlphabet))
    )
        ++ String.fromChar (Char.fromCode (65 + modBy lettersInAlphabet int))
