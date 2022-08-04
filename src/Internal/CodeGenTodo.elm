module Internal.CodeGenTodo exposing (CodeGenTodo, declarationsVisitor, todoErrors)

import AssocList exposing (Dict)
import AssocSet as Set
import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation(..))
import Internal.CodeGenerator as CodeGenerator exposing (ConfiguredCodeGenerator, ExistingFunctionProvider)
import Internal.ExistingImport exposing (ExistingImport)
import Internal.Helpers as Helpers
import Internal.ResolvedType as ResolvedType
import List.Extra
import ResolvedType exposing (ResolvedType)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Review.Rule as Rule exposing (ModuleKey)


type alias CodeGenTodo =
    { codeGenId : String
    , functionName : String
    , childType : ResolvedType
    , range : Range
    , parameters : List (Node Pattern)
    , signature : Signature
    , genericArguments : Dict.Dict String String
    }


type alias ProjectContext a =
    { a
        | types : List ( ModuleName, ResolvedType )
        , existingFunctionProviders : List ExistingFunctionProvider
        , codeGenTodos : List ( ModuleName, CodeGenTodo )
        , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
        , exports : Dict ModuleName (List ( String, Bool ))
        , moduleKeys : Dict ModuleName ModuleKey
        , codeGens : List ConfiguredCodeGenerator
    }


findMatchingPatternWithGenerics : List ConfiguredCodeGenerator -> TypeAnnotation -> Maybe ( String, TypeAnnotation, List String )
findMatchingPatternWithGenerics codeGenerators annotation =
    case codeGenerators of
        h :: t ->
            case h.searchPattern annotation of
                Just childType ->
                    Just ( h.id, childType, [] )

                Nothing ->
                    case annotation of
                        TypeAnnotation.FunctionTypeAnnotation (Node _ inp) (Node _ out) ->
                            case ( h.searchPattern inp, findMatchingPatternWithGenerics [ h ] out ) of
                                ( Just (TypeAnnotation.GenericType varName), Just ( _, childType, otherAssignments ) ) ->
                                    if containsGenericType varName childType then
                                        Just ( h.id, childType, varName :: otherAssignments )

                                    else
                                        Nothing

                                _ ->
                                    findMatchingPatternWithGenerics t annotation

                        _ ->
                            findMatchingPatternWithGenerics t annotation

        [] ->
            Nothing


containsGenericType : String -> TypeAnnotation -> Bool
containsGenericType varName t =
    case t of
        GenericType var ->
            varName == var

        Unit ->
            False

        Typed _ args ->
            List.any (Node.value >> containsGenericType varName) args

        Tupled args ->
            List.any (Node.value >> containsGenericType varName) args

        Record def ->
            List.any (\(Node _ ( _, Node _ v )) -> containsGenericType varName v) def

        GenericRecord (Node _ var) (Node _ def) ->
            varName == var || List.any (\(Node _ ( _, Node _ v )) -> containsGenericType varName v) def

        FunctionTypeAnnotation (Node _ lv) (Node _ rv) ->
            containsGenericType varName lv || containsGenericType varName rv


normalizeReference : ModuleNameLookupTable.ModuleNameLookupTable -> Node ( List String, String ) -> Node ( List String, String )
normalizeReference table ((Node r ( _, valName )) as node) =
    case ModuleNameLookupTable.moduleNameFor table node of
        Just moduleName ->
            Node r ( moduleName, valName )

        Nothing ->
            node


normalizeTypes : ModuleNameLookupTable.ModuleNameLookupTable -> TypeAnnotation -> TypeAnnotation
normalizeTypes lookupTable signature =
    case signature of
        GenericType var ->
            GenericType var

        Unit ->
            Unit

        Typed t args ->
            Typed (normalizeReference lookupTable t) (List.map (\(Node r v) -> Node r (normalizeTypes lookupTable v)) args)

        Tupled args ->
            Tupled (List.map (\(Node r v) -> Node r (normalizeTypes lookupTable v)) args)

        Record def ->
            Record (List.map (\(Node r ( k, Node rv v )) -> Node r ( k, Node rv (normalizeTypes lookupTable v) )) def)

        GenericRecord var (Node defr def) ->
            GenericRecord var (Node defr (List.map (\(Node r ( k, Node rv v )) -> Node r ( k, Node rv (normalizeTypes lookupTable v) )) def))

        FunctionTypeAnnotation (Node lr lv) (Node rr rv) ->
            FunctionTypeAnnotation (Node lr (normalizeTypes lookupTable lv)) (Node rr (normalizeTypes lookupTable rv))


type DeclarationSearchResult
    = FoundTodo CodeGenTodo
    | FoundProvider ExistingFunctionProvider
    | NotFound


declarationsVisitor :
    { a | codeGens : List ConfiguredCodeGenerator, lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> List ResolvedType
    -> List (Node Declaration)
    -> { todos : List CodeGenTodo, providers : List ExistingFunctionProvider }
declarationsVisitor context availableTypes =
    let
        go todos providers decls =
            case decls of
                [] ->
                    { todos = todos, providers = providers }

                (Node range (Declaration.FunctionDeclaration function)) :: rest ->
                    case declarationVisitor context availableTypes range function of
                        NotFound ->
                            go todos providers rest

                        FoundTodo todo ->
                            go (todo :: todos) providers rest

                        FoundProvider provider ->
                            go todos (provider :: providers) rest

                _ :: rest ->
                    go todos providers rest
    in
    go [] []


declarationVisitor :
    { a | codeGens : List ConfiguredCodeGenerator, lookupTable : ModuleNameLookupTable.ModuleNameLookupTable, currentModule : ModuleName }
    -> List ResolvedType
    -> Range
    -> Function
    -> DeclarationSearchResult
declarationVisitor context availableTypes declarationRange function =
    let
        declaration =
            Node.value function.declaration
    in
    function.signature
        |> Maybe.map Node.value
        |> Maybe.andThen
            (\signature ->
                findMatchingPatternWithGenerics context.codeGens (normalizeTypes context.lookupTable (Node.value signature.typeAnnotation))
                    |> Maybe.andThen
                        (\( codeGenId, childType, assignments ) ->
                            if Helpers.hasDebugTodo declaration then
                                List.map2
                                    (\var pattern ->
                                        case pattern of
                                            Node _ (Elm.Syntax.Pattern.VarPattern name) ->
                                                Just ( var, name )

                                            _ ->
                                                Nothing
                                    )
                                    assignments
                                    declaration.arguments
                                    |> List.foldr (Maybe.map2 (::)) (Just [])
                                    |> Maybe.map
                                        (\genericAssigments ->
                                            FoundTodo
                                                { functionName = Node.value signature.name
                                                , childType = ResolvedType.fromTypeSignature context.lookupTable availableTypes context.currentModule childType
                                                , range = declarationRange
                                                , parameters = declaration.arguments
                                                , signature = signature
                                                , codeGenId = codeGenId
                                                , genericArguments = Dict.fromList genericAssigments
                                                }
                                        )

                            else
                                Just
                                    (FoundProvider
                                        { functionName = Node.value signature.name
                                        , childType = ResolvedType.fromTypeSignature context.lookupTable availableTypes context.currentModule childType
                                        , codeGenId = codeGenId
                                        , moduleName = context.currentModule
                                        , genericArguments = assignments
                                        , privateTo = Nothing
                                        }
                                    )
                        )
            )
        |> Maybe.withDefault NotFound


todoErrors : ProjectContext a -> ModuleName -> CodeGenTodo -> Maybe (Rule.Error { useErrorForModule : () })
todoErrors projectContext currentModule todo =
    case ( Helpers.find (\codeGen -> codeGen.id == todo.codeGenId) projectContext.codeGens, AssocList.get currentModule projectContext.moduleKeys, AssocList.get currentModule projectContext.imports ) of
        ( Just codeGen, Just moduleKey, Just imports ) ->
            case createFixes projectContext codeGen imports currentModule todo of
                Ok fixes ->
                    Rule.errorForModuleWithFix moduleKey
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details =
                            [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                            ]
                        }
                        todo.range
                        fixes
                        |> Just

                Err reason ->
                    Rule.errorForModule moduleKey
                        { message = "Remove the use of `Debug.todo` before shipping to production"
                        , details =
                            [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                            , reason
                            ]
                        }
                        todo.range
                        |> Just

        _ ->
            Nothing


createFixes : ProjectContext a -> ConfiguredCodeGenerator -> { newImportStartRow : Int, existingImports : List ExistingImport } -> ModuleName -> CodeGenTodo -> Result String (List Review.Fix.Fix)
createFixes projectContext codeGen imports currentModule todo =
    let
        childType =
            ResolvedType.computeVisibility currentModule projectContext.exports todo.childType
    in
    CodeGenerator.generate True
        { codeGen = codeGen
        , existingImports = imports.existingImports
        , currentModule = currentModule
        , existingFunctionProviders =
            projectContext.existingFunctionProviders
                |> List.filter (\provider -> provider.codeGenId == codeGen.id)
        , genericArguments = todo.genericArguments
        }
        (case childType of
            ResolvedType.CustomType ref _ _ ->
                [ { name = todo.functionName
                  , ref = ref
                  , genericArguments = todo.genericArguments
                  , isLambdaProtected = False
                  }
                ]

            _ ->
                []
        )
        childType
        |> Result.map
            (\( expr, declarations_, _ ) ->
                let
                    ( expression, newImports_ ) =
                        Helpers.fixNamesAndImportsInExpression (CodeGenerator.postprocessExpression expr) currentModule imports.existingImports

                    ( declarations, newImports ) =
                        List.foldr
                            (\decl ( soFar, imports_ ) ->
                                let
                                    ( newDecl, foundImports ) =
                                        Helpers.fixNamesAndImportsInFunctionDeclaration decl currentModule imports.existingImports
                                in
                                ( (Helpers.writeDeclaration (CodeGenerator.fixFunctionDeclaration [] newDecl) ++ "\n") :: soFar, foundImports ++ imports_ )
                            )
                            ( [], newImports_ )
                            declarations_

                    genericArgs =
                        Dict.values todo.genericArguments

                    applicableArgs =
                        List.filter
                            (\arg ->
                                case arg of
                                    Node _ (Elm.Syntax.Pattern.VarPattern n) ->
                                        not (List.member n genericArgs)

                                    _ ->
                                        False
                            )
                            todo.parameters
                in
                Review.Fix.replaceRangeBy todo.range
                    ({ documentation = Nothing
                     , signature = Helpers.node todo.signature |> Just
                     , declaration =
                        Helpers.node
                            { name = Helpers.node todo.functionName
                            , arguments = todo.parameters
                            , expression = Helpers.node expression
                            }
                     }
                        |> CodeGenerator.fixFunctionDeclaration applicableArgs
                        |> Helpers.writeDeclaration
                    )
                    :: List.indexedMap
                        (\index decl ->
                            Review.Fix.insertAt { column = 0, row = 99999 - index } decl
                        )
                        (List.Extra.unique declarations)
                    ++ (case
                            Helpers.importsFix
                                currentModule
                                imports.existingImports
                                imports.newImportStartRow
                                (Set.fromList newImports)
                        of
                            Just importsFix_ ->
                                [ importsFix_ ]

                            Nothing ->
                                []
                       )
            )
