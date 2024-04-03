module Internal.Postprocess exposing (expression, fixNamesAndImportsInExpression, fixNamesAndImportsInFunctionDeclaration, writeFunctionDeclaration)

import Dict
import Elm.CodeGen as CG
import Elm.Pretty
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression as Expression exposing (Expression(..), Function)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import Internal.ExistingImport exposing (ExistingImport)
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



--- Imports


fixNamesAndImportsInExpression : Expression -> List String -> List ExistingImport -> ( Expression, List (List String) )
fixNamesAndImportsInExpression expre currentModule imports =
    Helpers.traverseExpression
        (\expr newImports ->
            case expr of
                FunctionOrValue moduleName name ->
                    case findImport False moduleName name currentModule imports of
                        Just modPath ->
                            ( FunctionOrValue modPath name, newImports )

                        Nothing ->
                            ( expr, moduleName :: newImports )

                CaseExpression ce ->
                    let
                        ( cases, accu1 ) =
                            List.foldr
                                (\( Node pr pattern, ex ) ( soFar, accu ) ->
                                    let
                                        ( newPattern, newImps ) =
                                            fixNamesAndImportsInPattern pattern currentModule imports
                                    in
                                    ( ( Node pr newPattern, ex ) :: soFar, newImps ++ accu )
                                )
                                ( [], [] )
                                ce.cases
                    in
                    ( CaseExpression { ce | cases = cases }, accu1 )

                LambdaExpression lambdaExpr ->
                    let
                        ( args, accu1 ) =
                            List.foldr
                                (\(Node pr pattern) ( soFar, accu ) ->
                                    let
                                        ( newPattern, newImps ) =
                                            fixNamesAndImportsInPattern pattern currentModule imports
                                    in
                                    ( Node pr newPattern :: soFar, newImps ++ accu )
                                )
                                ( [], [] )
                                lambdaExpr.args
                    in
                    ( LambdaExpression { lambdaExpr | args = args }, accu1 )

                _ ->
                    ( expr, newImports )
        )
        []
        expre


fixNamesAndImportsInSignature : Signature -> List String -> List ExistingImport -> ( Signature, List (List String) )
fixNamesAndImportsInSignature signature currentModule imports =
    Helpers.traverseTypeAnnotation
        (\annotation newImports ->
            case annotation of
                Typed (Node refR ( moduleName, name )) args ->
                    case findImport True moduleName name currentModule imports of
                        Just modPath ->
                            ( Typed (Node refR ( modPath, name )) args, newImports )

                        Nothing ->
                            ( annotation, moduleName :: newImports )

                _ ->
                    ( annotation, newImports )
        )
        []
        (Node.value signature.typeAnnotation)
        |> Tuple.mapFirst (\anno -> { signature | typeAnnotation = Helpers.node anno })


fixNamesAndImportsInPattern : Pattern -> List String -> List ExistingImport -> ( Pattern, List (List String) )
fixNamesAndImportsInPattern pattern currentModule imports =
    Helpers.traversePattern
        (\pttrn newImports ->
            case pttrn of
                NamedPattern ref args ->
                    case findImport False ref.moduleName ref.name currentModule imports of
                        Just modPath ->
                            ( NamedPattern { ref | moduleName = modPath } args, newImports )

                        Nothing ->
                            ( pttrn, ref.moduleName :: newImports )

                _ ->
                    ( pttrn, newImports )
        )
        []
        pattern


fixNamesAndImportsInFunctionDeclaration : Function -> List String -> List ExistingImport -> ( Function, List (List String) )
fixNamesAndImportsInFunctionDeclaration fun currentModule imports =
    let
        impl =
            Node.value fun.declaration

        ( newExpression, expressionImports ) =
            fixNamesAndImportsInExpression (Node.value impl.expression) currentModule imports

        ( newSignature, signatureImports ) =
            Maybe.map (\sign -> fixNamesAndImportsInSignature (Node.value sign) currentModule imports |> Tuple.mapFirst (Helpers.node >> Just)) fun.signature |> Maybe.withDefault ( Nothing, [] )

        ( newArguments, argumentsImports ) =
            List.foldr
                (\(Node argr arg) ( soFar, imps ) ->
                    let
                        ( newArg, newImps ) =
                            fixNamesAndImportsInPattern arg currentModule imports
                    in
                    ( Node argr newArg :: soFar, newImps ++ imps )
                )
                ( [], [] )
                impl.arguments
    in
    ( { fun | declaration = Helpers.node { impl | expression = Helpers.node newExpression, arguments = newArguments }, signature = newSignature }, expressionImports ++ signatureImports ++ argumentsImports )


findImport : Bool -> List String -> String -> List String -> List ExistingImport -> Maybe (List String)
findImport isType moduleName name currentModule imports =
    if moduleName == currentModule || moduleName == [] then
        Just []

    else
        let
            helper imps =
                case imps of
                    [] ->
                        Nothing

                    h :: t ->
                        if moduleName == h.moduleName then
                            if exposes isType name h.exposingList then
                                Just []

                            else
                                h.moduleAlias |> Maybe.map List.singleton |> Maybe.withDefault moduleName |> Just

                        else
                            helper t
        in
        helper (imports ++ Internal.ExistingImport.defaults)


exposes : Bool -> String -> Exposing -> Bool
exposes isType s exposure =
    case exposure of
        All _ ->
            True

        Explicit l ->
            List.any
                (\(Node _ value) ->
                    case value of
                        FunctionExpose fun ->
                            not isType && fun == s

                        TypeExpose { name } ->
                            isType && name == s

                        TypeOrAliasExpose name ->
                            isType && name == s

                        _ ->
                            False
                )
                l
