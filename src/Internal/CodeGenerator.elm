module Internal.CodeGenerator exposing (..)

import Dict
import Elm.CodeGen as CG
import Elm.Syntax.Expression as Expression exposing (Expression(..), Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Internal.ExistingImport exposing (ExistingImport)
import Internal.Helpers as Helpers
import Internal.ResolvedType as ResolvedType
import Internal.TypePattern as TypePattern
import List.Extra
import ResolvedType exposing (Reference, ResolvedType)
import TypePattern exposing (TypePattern)


type CodeGenerator
    = Generic
        { id : String
        , searchPattern : TypePattern
        , resolvers : List Resolver
        , dependency : String
        , makeName : String -> String
        , lambdaBreaker : Maybe { condition : Condition, implementation : Expression -> Expression }
        }
    | Amendment String (List Resolver)


type alias Resolver =
    { implementation : ResolverImpl
    , condition : Condition
    }


type ResolverImpl
    = PrimitiveResolver Reference (List ResolvedType -> List Expression -> Maybe Expression)
    | UniversalResolver (ResolvedType -> Maybe Expression)
    | Combiner (ResolvedType -> Expression -> List Expression -> Maybe Expression)
    | CustomTypeResolver (List ( Reference, List ResolvedType ) -> List ( String, Expression ) -> Expression)


type Condition
    = Always
    | Dependencies (List String)



-- Configuration stuff


evalCondition : List String -> Condition -> Bool
evalCondition dependencies cond =
    case cond of
        Always ->
            True

        Dependencies deps ->
            List.all (\dep -> List.member dep dependencies) deps


processResolvers : List String -> List Resolver -> List ResolverImpl
processResolvers dependencies resolvers =
    List.filterMap
        (\resolver ->
            if evalCondition dependencies resolver.condition then
                Just resolver.implementation

            else
                Nothing
        )
        resolvers


type alias ConfiguredCodeGenerator =
    { id : String
    , searchPattern : TypePattern
    , dependency : String
    , resolvers : List ResolverImpl
    , makeName : String -> String
    , lambdaBreaker : Expression -> Expression
    }


configureCodeGenerators : List String -> List CodeGenerator -> List ConfiguredCodeGenerator
configureCodeGenerators dependencies codeGens =
    List.foldr
        (\codeGen ( amendments, resolved ) ->
            case codeGen of
                Amendment id resolvers ->
                    ( Dict.update id (Maybe.map (\e -> Just (resolvers ++ e)) >> Maybe.withDefault (Just resolvers)) amendments, resolved )

                Generic gen ->
                    if List.member gen.dependency dependencies then
                        ( Dict.remove gen.id amendments
                        , { id = gen.id
                          , dependency = gen.dependency
                          , searchPattern = gen.searchPattern
                          , resolvers = processResolvers dependencies (Dict.get gen.id amendments |> Maybe.withDefault []) ++ processResolvers dependencies gen.resolvers
                          , makeName = gen.makeName
                          , lambdaBreaker =
                                Maybe.andThen
                                    (\breaker ->
                                        if evalCondition dependencies breaker.condition then
                                            Just breaker.implementation

                                        else
                                            Nothing
                                    )
                                    gen.lambdaBreaker
                                    |> Maybe.withDefault identity
                          }
                            :: resolved
                        )

                    else
                        ( Dict.remove gen.id amendments, resolved )
        )
        ( Dict.empty, [] )
        codeGens
        |> Tuple.second


type alias ExistingFunctionProvider =
    { codeGenId : String
    , moduleName : ModuleName
    , fromDependency : Bool
    , functionName : String
    , childType : ResolvedType
    , genericArguments : List String
    , privateTo : Maybe (List String)
    }


type alias GenerationContext =
    { codeGen : ConfiguredCodeGenerator
    , existingImports : List ExistingImport
    , currentModule : ModuleName
    , existingFunctionProviders : List ExistingFunctionProvider
    , genericArguments : Dict.Dict String String
    }


type alias RecursionStack =
    List
        { name : String
        , ref : Reference
        , genericArguments : Dict.Dict String String
        , isLambdaProtected : Bool
        }


type alias CodeGenerationResult =
    Result
        -- Error msg
        String
        ( -- The expression being currently generated
          Expression
          -- Auxiliary definitions
        , List Function
          -- Bindings resulting from generics
        , List ( String, Expression )
        )


generate : Bool -> GenerationContext -> RecursionStack -> ResolvedType -> CodeGenerationResult
generate isTopLevel context stack type_ =
    case Helpers.find (\provider -> ResolvedType.matchType provider.childType type_) context.existingFunctionProviders of
        Just provider ->
            let
                assignments =
                    ResolvedType.findGenericAssignments provider.childType type_
            in
            if Dict.isEmpty assignments then
                Ok ( CG.fqFun provider.moduleName provider.functionName, [], [] )

            else
                let
                    childExprsAndDefs =
                        provider.genericArguments
                            |> List.filterMap (\name -> Dict.get name assignments)
                            |> List.map (generate False context stack)
                            |> combineResults
                in
                childExprsAndDefs
                    |> Result.map (\( x, y, z ) -> ( CG.apply (CG.fqFun provider.moduleName provider.functionName :: x), y, z ))

        Nothing ->
            case type_ of
                ResolvedType.GenericType name Nothing ->
                    Dict.get name context.genericArguments
                        |> Result.fromMaybe ("Could not generate definition for generic variable `" ++ name ++ "`. You need to supply an argument of that type.")
                        |> Result.map
                            (\n ->
                                ( CG.val n
                                , []
                                , [ ( name, CG.val n ) ]
                                )
                            )

                ResolvedType.GenericType name (Just child) ->
                    case generate True context stack child of
                        Ok ( expr, defs, bindings ) ->
                            Ok ( CG.val name, defs, ( name, expr ) :: bindings )

                        Err e ->
                            Err e

                ResolvedType.Opaque ref args ->
                    case Helpers.find (\recurse -> recurse.ref == ref) stack of
                        Just recurse ->
                            if recurse.isLambdaProtected then
                                Ok ( CG.apply (CG.fun recurse.name :: List.map CG.val (Dict.values recurse.genericArguments)), [], [] )

                            else
                                Ok ( context.codeGen.lambdaBreaker (CG.apply (CG.fun recurse.name :: List.map CG.val (Dict.values recurse.genericArguments))), [], [] )

                        Nothing ->
                            applyResolvers
                                -- (\resolver ->
                                --     case resolver of
                                --         PrimitiveResolver reference fn ->
                                --             if reference == ref then
                                --                 List.map (generate False context stack) args
                                --                     |> combineResults
                                --                     |> Result.map (\( x, y, z ) -> ( fn args x, y, z ))
                                --                     |> transmogrify
                                --             else
                                --                 Nothing
                                --         _ ->
                                --             Nothing
                                (always Nothing)
                                -- )
                                (Helpers.writeExpression (ResolvedType.refToExpr context.currentModule context.existingImports ref))
                                context
                                stack
                                type_

                ResolvedType.Function _ _ ->
                    applyResolvers (always Nothing) "<function>" context stack type_

                ResolvedType.TypeAlias ref generics (ResolvedType.AnonymousRecord _ children) ->
                    applyCombiner type_ (ResolvedType.refToExpr context.currentModule context.existingImports ref) (List.map Tuple.second children) context stack
                        |> makeExternalDeclaration isTopLevel context.codeGen ref generics

                ResolvedType.TypeAlias _ _ childType ->
                    generate isTopLevel context stack childType

                ResolvedType.AnonymousRecord _ children ->
                    applyCombiner type_
                        (List.map (\( name, _ ) -> ( name, CG.val name )) children
                            |> CG.record
                            |> CG.lambda (List.map (\( name, _ ) -> CG.varPattern name) children)
                        )
                        (List.map Tuple.second children)
                        context
                        stack

                ResolvedType.Tuple args ->
                    case List.length args of
                        0 ->
                            generate False context stack (ResolvedType.Opaque { modulePath = [ "Basics" ], name = "()" } [])

                        2 ->
                            applyCombiner type_ (CG.fqFun [ "Tuple" ] "pair") args context stack

                        3 ->
                            applyCombiner type_
                                (CG.lambda [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ] (CG.tuple [ CG.val "a", CG.val "b", CG.val "c" ]))
                                args
                                context
                                stack

                        _ ->
                            Err "You tried to use an illegal tuple type"

                ResolvedType.CustomType ref generics ctors ->
                    makeExternalDeclaration isTopLevel
                        context.codeGen
                        ref
                        generics
                        (case ctors of
                            _ ->
                                applyResolvers
                                    (\resolver ->
                                        case resolver of
                                            CustomTypeResolver fn ->
                                                ctors
                                                    |> List.map
                                                        (\( ctorRef, args ) ->
                                                            applyCombiner (ResolvedType.Opaque ctorRef args)
                                                                (ResolvedType.refToExpr context.currentModule context.existingImports ctorRef)
                                                                args
                                                                context
                                                                (if isTopLevel then
                                                                    stack

                                                                 else
                                                                    { name = context.codeGen.makeName ref.name
                                                                    , ref = ref
                                                                    , genericArguments = Dict.empty
                                                                    , isLambdaProtected = False
                                                                    }
                                                                        :: stack
                                                                )
                                                        )
                                                    |> combineResults
                                                    |> Result.map (\( x, y, z ) -> ( x |> List.map2 (\( ctorRef, _ ) -> Tuple.pair ctorRef.name) ctors |> fn ctors, y, z ))
                                                    |> Just

                                            _ ->
                                                Nothing
                                    )
                                    ref.name
                                    context
                                    stack
                                    type_
                        )


makeExternalDeclaration : Bool -> ConfiguredCodeGenerator -> Reference -> List String -> CodeGenerationResult -> CodeGenerationResult
makeExternalDeclaration isTopLevel codeGen ref generics defExpr =
    if isTopLevel then
        Result.map
            (\( expr, defs, bindings ) ->
                ( applyBindings
                    (Dict.fromList bindings)
                    expr
                , defs
                , bindings
                )
            )
            defExpr

    else
        let
            name =
                codeGen.makeName ref.name

            annotation =
                List.foldr
                    (\r anno ->
                        CG.funAnn (TypePattern.generate codeGen.searchPattern (CG.typeVar r)) anno
                    )
                    (TypePattern.generate codeGen.searchPattern (CG.fqTyped ref.modulePath ref.name (List.map (\r -> CG.fqTyped [] r []) generics)))
                    generics
        in
        Result.map
            (\( expr, defs, bindings ) ->
                let
                    binds =
                        Dict.fromList bindings
                in
                ( CG.apply
                    (CG.fun name
                        :: List.filterMap
                            (\r -> Dict.get r binds)
                            generics
                    )
                , { documentation = Nothing
                  , signature =
                        Just
                            (Helpers.node
                                { name = Helpers.node name
                                , typeAnnotation = Helpers.node annotation
                                }
                            )
                  , declaration =
                        Helpers.node
                            { name = Helpers.node name
                            , arguments = List.map (\r -> Helpers.node (CG.varPattern r)) generics
                            , expression =
                                Helpers.node
                                    (applyBindings
                                        (List.filterMap
                                            (\r ->
                                                case Dict.get r binds of
                                                    Just (FunctionOrValue [] n) ->
                                                        Just ( n, FunctionOrValue [] r )

                                                    _ ->
                                                        Nothing
                                            )
                                            generics
                                            |> Dict.fromList
                                        )
                                        expr
                                    )
                            }
                  }
                    :: defs
                , bindings
                )
            )
            defExpr


combineResults : List CodeGenerationResult -> Result String ( List Expression, List Function, List ( String, Expression ) )
combineResults =
    List.foldr
        (Result.map2
            (\( itemExpr, itemDefs, itemBindings ) ( accuExprs, accuDefs, accuBindings ) ->
                ( itemExpr :: accuExprs, itemDefs ++ accuDefs, itemBindings ++ accuBindings )
            )
        )
        (Ok ( [], [], [] ))


applyResolvers : (ResolverImpl -> Maybe CodeGenerationResult) -> String -> GenerationContext -> RecursionStack -> ResolvedType -> CodeGenerationResult
applyResolvers fn name context stack t =
    Helpers.findMap
        (\resolver ->
            case resolver of
                PrimitiveResolver reference primRes ->
                    if ResolvedType.getRef t == Just reference then
                        let
                            args =
                                ResolvedType.getArgs t
                        in
                        List.map (generate False context stack) args
                            |> combineResults
                            |> Result.map (\( x, y, z ) -> ( primRes args x, y, z ))
                            |> transmogrify

                    else
                        Nothing

                UniversalResolver ur ->
                    Maybe.map (\expr -> Ok ( expr, [], [] )) (ur t)

                _ ->
                    fn resolver
        )
        context.codeGen.resolvers
        |> Result.fromMaybe ("Could not automatically generate a definition for `" ++ name ++ "`, as we don't know how to implement this type.")
        |> Result.andThen identity


applyCombiner : ResolvedType -> Expression -> List ResolvedType -> GenerationContext -> RecursionStack -> CodeGenerationResult
applyCombiner t ctor children context stack =
    applyResolvers
        (\resolver ->
            case resolver of
                Combiner fn ->
                    let
                        childExprsAndDefs =
                            List.map (generate False context stack) children
                                |> combineResults
                    in
                    childExprsAndDefs
                        |> Result.map (\( x, y, z ) -> ( fn t ctor x, y, z ))
                        |> transmogrify

                _ ->
                    Nothing
        )
        (Helpers.writeExpression ctor)
        context
        stack
        t


transmogrify : Result err ( Maybe a, b, c ) -> Maybe (Result err ( a, b, c ))
transmogrify input =
    case input of
        Ok ( Just a, b, c ) ->
            Just (Ok ( a, b, c ))

        Ok ( Nothing, _, _ ) ->
            Nothing

        Err err ->
            Just (Err err)


applyBindings : Dict.Dict String Expression -> Expression -> Expression
applyBindings bindings =
    Helpers.traverseExpression
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


{-| This simplifies expressions to make them easier on the eyes, but still super simple to program:

1.  Transforms `((\foo -> foo + 1) bar)` to `(bar + 1)`
2.  Transforms `(\\foo qux -> bar foo qux)` to `bar`
3.  Transforms `((foo bar) baz)` to `(foo bar baz)`

This means that generators can use the simple composability of explicit lambdas but get nice looking but still correct code.

-}
postprocessExpression : Expression -> Expression
postprocessExpression expression =
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
                        applyBindings bindings (Node.value lambda.expression)

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
        expression
        |> Tuple.first


isPlainPattern : Node Pattern -> Bool
isPlainPattern (Node _ arg) =
    case arg of
        VarPattern _ ->
            True

        _ ->
            False


{-| This handles cases where the generator generates lambdas, but the user expects to provide their own argument naming.
Also it promotes

     myFun : Arg -> Res
     myFun =
         \arg ->  arg ++ d

to

    myFun : Arg -> Res
    myFun arg =
        arg ++ d

-}
fixFunctionDeclaration : List (Node Pattern) -> Function -> Function
fixFunctionDeclaration applicableArgs func =
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
            case postprocessExpression (Node.value declaration.expression) of
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
                        , declaration.arguments
                        )

                    else
                        ( CG.apply (Node.value declaration.expression :: asVals) |> Helpers.node, declaration.arguments )

                exp ->
                    ( if List.isEmpty applicableArgs then
                        declaration.expression

                      else
                        CG.apply
                            (exp :: asVals)
                            |> postprocessExpression
                            |> Helpers.node
                    , declaration.arguments
                    )
    in
    { func | declaration = Helpers.node { declaration | expression = newExpr, arguments = newArgs } }
