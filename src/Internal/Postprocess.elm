module Internal.Postprocess exposing (expression, writeFunctionDeclaration)

import Dict
import Elm.CodeGen as CG
import Elm.Pretty
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Internal.Helpers as Helpers
import List.Extra
import Pretty


{-| This simplifies expressions to make them easier on the eyes, but still super simple to program:

1.  Transforms `((\foo -> foo + 1) bar)` to `(bar + 1)`
2.  Transforms `(\\foo qux -> bar foo qux)` to `bar`
3.  Transforms `((foo bar) baz)` to `(foo bar baz)`

This means that generators can use the simple composability of explicit lambdas but get nice looking but still correct code.

-}
expression : Expression -> Expression
expression inputExpression =
    let
        etaApplicable args exprs =
            case ( List.reverse exprs, List.Extra.last args ) of
                ( (Node _ (Expression.FunctionOrValue [] name)) :: rest, Just (Node _ (Elm.Syntax.Pattern.VarPattern pattern)) ) ->
                    name
                        == pattern
                        && List.all
                            (\(Node _ v) ->
                                case v of
                                    Expression.FunctionOrValue m2 n2 ->
                                        n2 /= name || m2 /= []

                                    _ ->
                                        False
                            )
                            rest

                _ ->
                    False
    in
    Helpers.traverseExpression
        (\expr oo ->
            ( case expr of
                Expression.Application ((Node _ (Expression.ParenthesizedExpression (Node _ (Expression.Application xs)))) :: ys) ->
                    Expression.Application (xs ++ ys)

                Expression.Application ((Node _ (Expression.LambdaExpression lambda)) :: args) ->
                    if List.length lambda.args == List.length args && List.all isPlainPattern lambda.args then
                        let
                            bindings =
                                List.map2
                                    (\(Node _ p1) (Node _ p2) ->
                                        case p1 of
                                            VarPattern n1 ->
                                                Just ( n1, p2 )

                                            _ ->
                                                Nothing
                                    )
                                    lambda.args
                                    args
                                    |> List.filterMap identity
                                    |> Dict.fromList
                        in
                        Helpers.applyBindings bindings (Node.value lambda.expression)

                    else
                        expr

                Expression.LambdaExpression lambda ->
                    case lambda.expression of
                        Node ln (Expression.Application app) ->
                            if etaApplicable lambda.args app then
                                case ( List.Extra.init app, List.Extra.init lambda.args ) of
                                    ( Just [ Node _ oneThing ], Just [] ) ->
                                        oneThing

                                    ( Just newApp, Just newArgs ) ->
                                        Expression.LambdaExpression
                                            { args = newArgs
                                            , expression = Node ln (Expression.Application newApp)
                                            }

                                    _ ->
                                        expr

                            else
                                expr

                        _ ->
                            expr

                _ ->
                    expr
            , oo
            )
        )
        ()
        inputExpression
        |> Tuple.first


isPlainPattern : Node Pattern -> Bool
isPlainPattern (Node _ arg) =
    case arg of
        VarPattern _ ->
            True

        _ ->
            False


{-| Writes out a function declaration given potential argument namings (provided by the user).

This handles cases where the generator generates lambdas, but the user expects to provide their own argument naming.

So it can promote:

    myFun : Arg -> Res
    myFun myFancyArg =
        Debug.todo ""

    -- and

    \arg -> arg ++ "d"

    -- to

    myFun : Arg -> Res
    myFun myFancyArg =
        myFancyArg ++ "d"

Also it promotes

     myFun : Arg -> Res
     myFun =
         \arg ->  arg ++ d

to

    myFun : Arg -> Res
    myFun arg =
        arg ++ d

-}
writeFunctionDeclaration : List (Node Pattern) -> Function -> String
writeFunctionDeclaration applicableArgs func =
    let
        declaration =
            Node.value func.declaration

        asVals =
            List.filterMap
                (\p ->
                    case p of
                        Node _ (VarPattern n) ->
                            Just (CG.val n)

                        _ ->
                            Nothing
                )
                applicableArgs

        ( newExpr, newArgs ) =
            case expression (Node.value declaration.expression) of
                Expression.LambdaExpression lambda ->
                    if List.isEmpty declaration.arguments then
                        ( lambda.expression, lambda.args )

                    else if List.all isPlainPattern declaration.arguments && List.all isPlainPattern lambda.args then
                        let
                            bindings =
                                List.map2
                                    (\(Node _ p1) (Node _ p2) ->
                                        case ( p1, p2 ) of
                                            ( VarPattern n1, VarPattern n2 ) ->
                                                Just ( n1, n2 )

                                            _ ->
                                                Nothing
                                    )
                                    (List.reverse lambda.args)
                                    (List.reverse applicableArgs)
                                    |> List.filterMap identity
                                    |> Dict.fromList

                            leftoverArgs =
                                List.drop (List.length applicableArgs) (List.reverse lambda.args)
                        in
                        ( Helpers.traverseExpression
                            (\expr oo ->
                                case expr of
                                    Expression.FunctionOrValue [] name ->
                                        case Dict.get name bindings of
                                            Just r ->
                                                ( Expression.FunctionOrValue [] r, oo )

                                            Nothing ->
                                                ( expr, oo )

                                    _ ->
                                        ( expr, oo )
                            )
                            ()
                            (Node.value lambda.expression)
                            |> Tuple.first
                            |> Helpers.node
                        , declaration.arguments ++ leftoverArgs
                        )

                    else
                        ( CG.apply (Node.value declaration.expression :: asVals) |> Helpers.node, declaration.arguments )

                exp ->
                    ( if List.isEmpty applicableArgs then
                        declaration.expression

                      else
                        CG.apply
                            (exp :: asVals)
                            |> expression
                            |> Helpers.node
                    , declaration.arguments
                    )
    in
    { func | declaration = Helpers.node { declaration | expression = newExpr, arguments = newArgs } }
        |> Elm.Pretty.prettyFun
        |> Pretty.pretty 120
