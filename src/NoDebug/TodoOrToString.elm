module NoDebug.TodoOrToString exposing (rule)

import AssocList exposing (Dict)
import Dict
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Internal.Builtin.Codec
import Internal.Builtin.FromString
import Internal.Builtin.JsonEncoder
import Internal.Builtin.ListAllVariants
import Internal.Builtin.Random
import Internal.Builtin.ToString
import Internal.CodeGenTodo exposing (CodeGenTodo)
import Internal.CodeGenerator exposing (CodeGenerator, ConfiguredCodeGenerator)
import Internal.ExistingImport exposing (ExistingImport)
import Internal.Helpers
import Internal.ResolvedType as ResolvedType
import ResolvedType exposing (ResolvedType)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, ModuleKey, Rule)


rule : List CodeGenerator -> Rule
rule generators =
    let
        codeGens =
            generators
                ++ [ Internal.Builtin.Random.codeGen
                   , Internal.Builtin.JsonEncoder.codeGen
                   , Internal.Builtin.Codec.codeGen
                   , Internal.Builtin.ListAllVariants.codeGen
                   , Internal.Builtin.ToString.codeGen
                   , Internal.Builtin.FromString.codeGen
                   ]
    in
    Rule.newProjectRuleSchema "CodeGen" initialProjectContext
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule |> Rule.withModuleNameLookupTable |> Rule.withMetadata
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withDependenciesProjectVisitor (initializeCodeGens codeGens)
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List (Error {}), ModuleContext )
moduleDefinitionVisitor (Node _ module_) context =
    ( []
    , { context
        | exports =
            case module_ of
                NormalModule { exposingList } ->
                    case Node.value exposingList of
                        Elm.Syntax.Exposing.All _ ->
                            []

                        Elm.Syntax.Exposing.Explicit list ->
                            List.map
                                (\(Node _ val) ->
                                    case val of
                                        Elm.Syntax.Exposing.InfixExpose v ->
                                            ( v, False )

                                        Elm.Syntax.Exposing.FunctionExpose v ->
                                            ( v, False )

                                        Elm.Syntax.Exposing.TypeOrAliasExpose v ->
                                            ( v, False )

                                        Elm.Syntax.Exposing.TypeExpose exposeType ->
                                            ( exposeType.name, exposeType.open /= Nothing )
                                )
                                list

                _ ->
                    []
      }
    )


importVisitor : Node Import -> ModuleContext -> ( List (Error a), ModuleContext )
importVisitor (Node _ import_) context =
    ( []
    , { context
        | imports =
            { moduleName = Node.value import_.moduleName
            , moduleAlias = Maybe.andThen (Node.value >> List.head) import_.moduleAlias
            , exposingList =
                Maybe.map Node.value import_.exposingList
                    |> Maybe.withDefault (Elm.Syntax.Exposing.Explicit [])
            }
                :: context.imports
      }
    )


initializeCodeGens : List CodeGenerator -> Dict.Dict String Dependency -> ProjectContext -> ( List (Error { useErrorForModule : () }), ProjectContext )
initializeCodeGens codeGens deps context =
    ( []
    , { context | codeGens = Internal.CodeGenerator.configureCodeGenerators (Dict.keys deps) codeGens }
    )


type alias ProjectContext =
    { types : List ( ModuleName, ResolvedType )
    , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
    , moduleKeys : Dict ModuleName ModuleKey
    , codeGens : List ConfiguredCodeGenerator
    , codeGenTodos : List ( ModuleName, CodeGenTodo )
    , otherTodos : List ( ModuleKey, Range )
    , existingFunctionProviders : List { moduleName : ModuleName, codeGenId : String, functionName : String, childType : ResolvedType }
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable.ModuleNameLookupTable
    , types : List ResolvedType
    , availableTypes : List ( ModuleName, ResolvedType )
    , exports : List ( String, Bool )
    , importStartRow : Maybe Int
    , imports : List ExistingImport
    , currentModule : ModuleName
    , codeGens : List ConfiguredCodeGenerator
    , codeGenTodos : List CodeGenTodo
    , otherTodos : List Range
    , existingFunctionProviders : List { codeGenId : String, functionName : String, childType : ResolvedType }
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { types = []
    , imports = AssocList.empty
    , moduleKeys = AssocList.empty
    , codeGens = []
    , codeGenTodos = []
    , existingFunctionProviders = []
    , otherTodos = []
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    let
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { lookupTable = lookupTable
    , types = []
    , exports = []
    , availableTypes = projectContext.types
    , importStartRow = Nothing
    , imports = []
    , currentModule = moduleName
    , codeGens = projectContext.codeGens
    , codeGenTodos = []
    , existingFunctionProviders = []
    , otherTodos = []
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    let
        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata

        mapTodo : List a -> List ( ModuleName, a )
        mapTodo =
            List.map (\todo -> ( moduleName, todo ))

        codeGenRanges =
            List.map .range moduleContext.codeGenTodos

        filterTodos =
            List.filter
                (\r ->
                    List.all (Internal.Helpers.rangeContains r >> not) codeGenRanges
                )
    in
    { types =
        if List.isEmpty moduleContext.exports then
            List.map (Tuple.pair moduleName) moduleContext.types

        else
            List.filterMap
                (\t ->
                    case t of
                        ResolvedType.CustomType ref _ _ ->
                            Maybe.map (always ( moduleName, t )) (Internal.Helpers.find (\( exp, open ) -> ref.name == exp && open) moduleContext.exports)

                        _ ->
                            Just ( moduleName, t )
                )
                moduleContext.types
    , imports =
        AssocList.singleton
            moduleName
            { newImportStartRow = Maybe.withDefault 3 moduleContext.importStartRow
            , existingImports = moduleContext.imports
            }
    , moduleKeys = AssocList.singleton moduleName moduleKey
    , codeGens = moduleContext.codeGens
    , codeGenTodos = mapTodo moduleContext.codeGenTodos
    , existingFunctionProviders = List.map (\a -> { moduleName = moduleName, codeGenId = a.codeGenId, functionName = a.functionName, childType = a.childType }) moduleContext.existingFunctionProviders
    , otherTodos = List.map (Tuple.pair moduleKey) (filterTodos moduleContext.otherTodos)
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { types = newContext.types ++ previousContext.types
    , imports = AssocList.union newContext.imports previousContext.imports
    , moduleKeys = AssocList.union newContext.moduleKeys previousContext.moduleKeys
    , codeGens =
        if List.isEmpty newContext.codeGens then
            previousContext.codeGens

        else
            newContext.codeGens
    , codeGenTodos = newContext.codeGenTodos ++ previousContext.codeGenTodos
    , existingFunctionProviders = newContext.existingFunctionProviders ++ previousContext.existingFunctionProviders
    , otherTodos = newContext.otherTodos ++ previousContext.otherTodos
    }


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor _ projectContext =
    ( [], projectContext )


declarationVisitor : List (Node Declaration) -> ModuleContext -> ( List a, ModuleContext )
declarationVisitor declarations context =
    let
        externalAvailableTypes =
            List.filterMap
                (\( moduleName, t ) ->
                    if List.any (\import_ -> import_.moduleName == moduleName) context.imports then
                        Just t

                    else
                        Nothing
                )
                context.availableTypes

        unresolvedTypes =
            List.filterMap (Node.value >> ResolvedType.fromDeclaration context.lookupTable externalAvailableTypes context.currentModule) declarations

        types =
            ResolvedType.resolveLocalReferences context.currentModule unresolvedTypes

        availableTypes =
            types ++ externalAvailableTypes

        getTodos getTodoFunc =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node range (Declaration.FunctionDeclaration function) ->
                            getTodoFunc range function

                        _ ->
                            Nothing
                )
                declarations
    in
    ( []
    , { context
        | types = types ++ context.types
        , importStartRow =
            List.map (Node.range >> .start >> .row) declarations |> List.minimum
        , existingFunctionProviders =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node _ (Declaration.FunctionDeclaration function) ->
                            Internal.CodeGenTodo.declarationVisitorGetExistingFunctionProviders context function
                                |> Maybe.map (\rec -> { codeGenId = rec.codeGenId, functionName = rec.functionName, childType = ResolvedType.fromTypeSignature context.lookupTable availableTypes context.currentModule rec.childType })

                        _ ->
                            Nothing
                )
                declarations
                ++ context.existingFunctionProviders
        , codeGenTodos = getTodos (Internal.CodeGenTodo.getTodos context availableTypes) ++ context.codeGenTodos
      }
    )


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node context =
    case Node.value node of
        Expression.FunctionOrValue _ name ->
            if name == "toString" then
                case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                    Just [ "Debug" ] ->
                        ( [ Rule.error
                                { message = "Remove the use of `Debug.toString` before shipping to production"
                                , details =
                                    [ "`Debug.toString` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                                    ]
                                }
                                (Node.range node)
                          ]
                        , context
                        )

                    _ ->
                        ( [], context )

            else if name == "todo" then
                case ModuleNameLookupTable.moduleNameFor context.lookupTable node of
                    Just [ "Debug" ] ->
                        ( []
                        , { context | otherTodos = Node.range node :: context.otherTodos }
                        )

                    _ ->
                        ( [], context )

            else
                ( [], context )

        _ ->
            ( [], context )


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    List.map
        (\( moduleKey, range ) ->
            Rule.errorForModule moduleKey
                { message = "Remove the use of `Debug.todo` before shipping to production"
                , details =
                    [ "`Debug.todo` can be useful when developing, but is not meant to be shipped to production or published in a package. I suggest removing its use before committing and attempting to push to production."
                    ]
                }
                range
        )
        projectContext.otherTodos
        ++ List.filterMap
            (\( moduleName, todo ) ->
                Internal.CodeGenTodo.todoErrors projectContext moduleName todo
            )
            projectContext.codeGenTodos
