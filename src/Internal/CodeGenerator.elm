module Internal.CodeGenerator exposing (CodeGenerator(..), Condition(..), ConfiguredCodeGenerator, ExistingFunctionProvider, GenerationContext, RecursionStack, Resolver, ResolverImpl(..), assemble, configureCodeGenerators)

import Dict
import Elm.CodeGen as CG
import Elm.Syntax.Expression exposing (Expression(..), Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Internal.CodeGenerationResult as CodeGenerationResult exposing (CodeGenerationResult)
import Internal.Helpers as Helpers
import Internal.Imports exposing (ExistingImport)
import Internal.ResolvedType as ResolvedType
import Internal.TypePattern as TypePattern
import List.Extra
import ResolvedType exposing (Reference, ResolvedType)
import TypePattern exposing (TypePattern)


type CodeGenerator
    = CodeGenerator (List String -> Maybe ConfiguredCodeGenerator)


type alias Resolver a =
    { implementation : ResolverImpl a
    , condition : Condition
    }


type ResolverImpl a
    = PrimitiveResolver Reference (a -> List ResolvedType -> List a) (a -> List ResolvedType -> List Expression -> Maybe Expression)
    | UniversalResolver (a -> ResolvedType -> Maybe Expression)
    | Combiner (a -> ResolvedType -> List ResolvedType -> List a) (a -> ResolvedType -> Expression -> List Expression -> Maybe Expression)
    | CustomTypeResolver (a -> List ( Reference, List ResolvedType ) -> List a) (a -> List ( Reference, List ResolvedType ) -> List ( String, Expression ) -> Expression)


type Condition
    = Always
    | Dependencies (List String)



-- Assemble


assemble :
    { id : String
    , searchPattern : TypePattern
    , resolvers : List (Resolver a)
    , dependency : String
    , makeName : Maybe (String -> String)
    , lambdaBreaker : Maybe { condition : Condition, implementation : Expression -> Expression }
    , blessedImplementations : List Reference
    , inputValue : a
    }
    -> CodeGenerator
assemble gen =
    CodeGenerator
        (\dependencies ->
            if List.member gen.dependency dependencies then
                Just
                    { id = gen.id
                    , searchPattern = gen.searchPattern
                    , dependency = gen.dependency
                    , blessedImplementations = gen.blessedImplementations
                    , generate =
                        \context stack type_ ->
                            generate
                                { id = gen.id
                                , searchPattern = gen.searchPattern
                                , resolvers = processResolvers dependencies gen.resolvers
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
                                True
                                context
                                stack
                                gen.inputValue
                                type_
                                |> CodeGenerationResult.projectResult
                    }

            else
                Nothing
        )


type alias AssembledGenerator a =
    { id : String
    , searchPattern : TypePattern
    , resolvers : List (ResolverImpl a)
    , makeName : Maybe (String -> String)
    , lambdaBreaker : Expression -> Expression
    }



-- Configuration stuff


evalCondition : List String -> Condition -> Bool
evalCondition dependencies cond =
    case cond of
        Always ->
            True

        Dependencies deps ->
            List.all (\dep -> List.member dep dependencies) deps


processResolvers : List String -> List (Resolver a) -> List (ResolverImpl a)
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
    , blessedImplementations : List Reference
    , generate : GenerationContext -> RecursionStack -> ResolvedType -> Result String ( Expression, List Function )
    }


configureCodeGenerators : List String -> List CodeGenerator -> List ConfiguredCodeGenerator
configureCodeGenerators dependencies codeGens =
    List.filterMap
        (\(CodeGenerator gen) ->
            gen dependencies
        )
        codeGens


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
    { existingImports : List ExistingImport
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


generate : AssembledGenerator a -> Bool -> GenerationContext -> RecursionStack -> a -> ResolvedType -> CodeGenerationResult
generate codeGen isTopLevel context stack inputValue type_ =
    case List.Extra.find (\provider -> ResolvedType.matchType provider.childType type_) context.existingFunctionProviders of
        Just provider ->
            let
                assignments =
                    ResolvedType.findGenericAssignments provider.childType type_
            in
            if Dict.isEmpty assignments then
                CodeGenerationResult.succeed (CG.fqFun provider.moduleName provider.functionName)

            else
                provider.genericArguments
                    |> List.filterMap (\name -> Dict.get name assignments)
                    |> List.map (generate codeGen False context stack inputValue)
                    |> CodeGenerationResult.combine (\exps -> CG.apply (CG.fqFun provider.moduleName provider.functionName :: exps))

        Nothing ->
            case type_ of
                ResolvedType.GenericType name Nothing ->
                    Dict.get name context.genericArguments
                        |> Result.fromMaybe ("Could not generate definition for generic variable `" ++ name ++ "`. You need to supply an argument of that type.")
                        |> Result.map
                            (\n ->
                                { expression = CG.val n
                                , auxiliaryDefinitions = []
                                , bindings = [ ( name, CG.val n ) ]
                                }
                            )

                ResolvedType.GenericType name (Just child) ->
                    case generate codeGen True context stack inputValue child of
                        Ok res ->
                            Ok { res | expression = CG.val name, bindings = ( name, res.expression ) :: res.bindings }

                        Err e ->
                            Err e

                ResolvedType.Opaque ref _ ->
                    case List.Extra.find (\recurse -> recurse.ref == ref) stack of
                        Just recurse ->
                            if recurse.isLambdaProtected then
                                CG.apply (CG.fun recurse.name :: List.map CG.val (Dict.values recurse.genericArguments))
                                    |> CodeGenerationResult.succeed

                            else
                                codeGen.lambdaBreaker (CG.apply (CG.fun recurse.name :: List.map CG.val (Dict.values recurse.genericArguments)))
                                    |> CodeGenerationResult.succeed

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
                                codeGen
                                context
                                stack
                                inputValue
                                type_

                ResolvedType.Function _ _ ->
                    applyResolvers (always Nothing) "<function>" codeGen context stack inputValue type_

                ResolvedType.TypeAlias ref generics (ResolvedType.AnonymousRecord _ children) ->
                    applyCombiner type_ (ResolvedType.refToExpr context.currentModule context.existingImports ref) inputValue (List.map Tuple.second children) codeGen context stack
                        |> makeExternalDeclaration isTopLevel codeGen ref generics

                ResolvedType.TypeAlias _ _ childType ->
                    generate codeGen isTopLevel context stack inputValue childType

                ResolvedType.AnonymousRecord _ children ->
                    applyCombiner type_
                        (List.map (\( name, _ ) -> ( name, CG.val name )) children
                            |> CG.record
                            |> CG.lambda (List.map (\( name, _ ) -> CG.varPattern name) children)
                        )
                        inputValue
                        (List.map Tuple.second children)
                        codeGen
                        context
                        stack

                ResolvedType.Tuple args ->
                    case List.length args of
                        0 ->
                            generate codeGen False context stack inputValue (ResolvedType.Opaque { modulePath = [ "Basics" ], name = "()" } [])

                        2 ->
                            applyCombiner type_ (CG.fqFun [ "Tuple" ] "pair") inputValue args codeGen context stack

                        3 ->
                            applyCombiner type_
                                (CG.lambda [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ] (CG.tuple [ CG.val "a", CG.val "b", CG.val "c" ]))
                                inputValue
                                args
                                codeGen
                                context
                                stack

                        _ ->
                            Err "You tried to use an illegal tuple type"

                ResolvedType.CustomType ref generics ctors ->
                    makeExternalDeclaration isTopLevel
                        codeGen
                        ref
                        generics
                        (applyResolvers
                            (\resolver ->
                                case resolver of
                                    CustomTypeResolver outputFn fn ->
                                        List.map2
                                            (\( ctorRef, args ) input ->
                                                applyCombiner (ResolvedType.Opaque ctorRef args)
                                                    (ResolvedType.refToExpr context.currentModule context.existingImports ctorRef)
                                                    input
                                                    args
                                                    codeGen
                                                    context
                                                    (if isTopLevel then
                                                        stack

                                                     else
                                                        case codeGen.makeName of
                                                            Just makeName ->
                                                                { name = makeName ref.name
                                                                , ref = ref
                                                                , genericArguments = Dict.empty
                                                                , isLambdaProtected = False
                                                                }
                                                                    :: stack

                                                            Nothing ->
                                                                stack
                                                    )
                                            )
                                            ctors
                                            (outputFn inputValue ctors)
                                            |> CodeGenerationResult.combine (\x -> x |> List.map2 (\( ctorRef, _ ) -> Tuple.pair ctorRef.name) ctors |> fn inputValue ctors)
                                            |> Just

                                    _ ->
                                        Nothing
                            )
                            ref.name
                            codeGen
                            context
                            stack
                            inputValue
                            type_
                        )


makeExternalDeclaration : Bool -> AssembledGenerator a -> Reference -> List String -> CodeGenerationResult -> CodeGenerationResult
makeExternalDeclaration isTopLevel codeGen ref generics defExpr =
    if isTopLevel then
        CodeGenerationResult.applyBindings defExpr

    else
        case codeGen.makeName of
            Just makeName ->
                Result.map
                    (\res ->
                        let
                            annotation =
                                List.foldr
                                    (\r anno ->
                                        CG.funAnn (TypePattern.generate codeGen.searchPattern (CG.typeVar r)) anno
                                    )
                                    (TypePattern.generate codeGen.searchPattern (CG.fqTyped ref.modulePath ref.name (List.map (\r -> CG.fqTyped [] r []) generics)))
                                    generics

                            name =
                                makeName ref.name

                            binds =
                                Dict.fromList res.bindings
                        in
                        { res
                            | expression =
                                CG.apply
                                    (CG.fun name
                                        :: List.filterMap
                                            (\r -> Dict.get r binds)
                                            generics
                                    )
                            , auxiliaryDefinitions =
                                { documentation = Nothing
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
                                                (Helpers.applyBindings
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
                                                    res.expression
                                                )
                                        }
                                }
                                    :: res.auxiliaryDefinitions
                        }
                    )
                    defExpr

            Nothing ->
                CodeGenerationResult.applyBindings defExpr


applyResolvers : (ResolverImpl a -> Maybe CodeGenerationResult) -> String -> AssembledGenerator a -> GenerationContext -> RecursionStack -> a -> ResolvedType -> CodeGenerationResult
applyResolvers fn name codeGen context stack inputValue t =
    List.Extra.findMap
        (\resolver ->
            case resolver of
                PrimitiveResolver reference outputFn primRes ->
                    if ResolvedType.getRef t == Just reference then
                        let
                            args =
                                ResolvedType.getArgs t
                        in
                        List.map2 (generate codeGen False context stack) (outputFn inputValue args) args
                            |> CodeGenerationResult.combineMaybe (primRes inputValue args)

                    else
                        Nothing

                UniversalResolver ur ->
                    Maybe.map (\expr -> CodeGenerationResult.succeed expr) (ur inputValue t)

                _ ->
                    fn resolver
        )
        codeGen.resolvers
        |> Result.fromMaybe ("Could not automatically generate a definition for `" ++ name ++ "`, as we don't know how to implement this type.")
        |> Result.andThen identity


applyCombiner : ResolvedType -> Expression -> a -> List ResolvedType -> AssembledGenerator a -> GenerationContext -> RecursionStack -> CodeGenerationResult
applyCombiner t ctor input children codeGen context stack =
    applyResolvers
        (\resolver ->
            case resolver of
                Combiner distributor fn ->
                    List.map2 (generate codeGen False context stack) (distributor input t children) children
                        |> CodeGenerationResult.combineMaybe (fn input t ctor)

                _ ->
                    Nothing
        )
        (Helpers.writeExpression ctor)
        codeGen
        context
        stack
        input
        t
