module NoDebug.Todo exposing (rule)

import AssocList exposing (Dict)
import CodeGen.Builtin.Codec
import CodeGen.Builtin.FromString
import CodeGen.Builtin.JsonEncoder
import CodeGen.Builtin.ListAllVariants
import CodeGen.Builtin.Random
import CodeGen.Builtin.ToString
import CodeGen.Helpers
import Dict
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import GenericTodo
import Internal.ExistingImport exposing (ExistingImport)
import ResolvedType exposing (ResolvedType)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, ModuleKey, Rule)


rule : Rule
rule =
    let
        generics =
            [ CodeGen.Builtin.Random.generic
            , CodeGen.Builtin.JsonEncoder.generic
            , CodeGen.Builtin.Codec.generic
            , CodeGen.Builtin.ListAllVariants.generic
            , CodeGen.Builtin.ToString.generic
            , CodeGen.Builtin.FromString.generic
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
        |> Rule.withDependenciesProjectVisitor (initializeGenerics generics)
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationVisitor


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


initializeGenerics : List GenericTodo.CodeGenerator -> Dict.Dict String Dependency -> ProjectContext -> ( List (Error { useErrorForModule : () }), ProjectContext )
initializeGenerics generics deps context =
    ( []
    , { context | generics = GenericTodo.buildFullGeneric (Dict.keys deps) generics }
    )


type alias ProjectContext =
    { types : List ( ModuleName, ResolvedType )
    , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
    , moduleKeys : Dict ModuleName ModuleKey
    , generics : List GenericTodo.ResolvedGeneric
    , genericTodos : List ( ModuleName, GenericTodo.GenericTodo )
    , genericProviders : List { moduleName : ModuleName, genericId : String, functionName : String, childType : ResolvedType }
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable.ModuleNameLookupTable
    , types : List ResolvedType
    , availableTypes : List ( ModuleName, ResolvedType )
    , exports : List ( String, Bool )
    , importStartRow : Maybe Int
    , imports : List ExistingImport
    , currentModule : ModuleName
    , generics : List GenericTodo.ResolvedGeneric
    , genericTodos : List GenericTodo.GenericTodo
    , genericProviders : List { genericId : String, functionName : String, childType : ResolvedType }
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { types = []
    , imports = AssocList.empty
    , moduleKeys = AssocList.empty
    , generics = []
    , genericTodos = []
    , genericProviders = []
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
    , generics = projectContext.generics
    , genericTodos = []
    , genericProviders = []
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
    in
    { types =
        if List.isEmpty moduleContext.exports then
            List.map (Tuple.pair moduleName) moduleContext.types

        else
            List.filterMap
                (\t ->
                    case t of
                        ResolvedType.CustomType ref _ _ ->
                            Maybe.map (always ( moduleName, t )) (CodeGen.Helpers.find (\( exp, open ) -> ref.name == exp && open) moduleContext.exports)

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
    , generics = moduleContext.generics
    , genericTodos = mapTodo moduleContext.genericTodos
    , genericProviders = List.map (\a -> { moduleName = moduleName, genericId = a.genericId, functionName = a.functionName, childType = a.childType }) moduleContext.genericProviders
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { types = newContext.types ++ previousContext.types
    , imports = AssocList.union newContext.imports previousContext.imports
    , moduleKeys = AssocList.union newContext.moduleKeys previousContext.moduleKeys
    , generics =
        if List.isEmpty newContext.generics then
            previousContext.generics

        else
            newContext.generics
    , genericTodos = newContext.genericTodos ++ previousContext.genericTodos
    , genericProviders = newContext.genericProviders ++ previousContext.genericProviders
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
        , genericProviders =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node _ (Declaration.FunctionDeclaration function) ->
                            GenericTodo.declarationVisitorGetGenericTypes context function
                                |> Maybe.map (\rec -> { genericId = rec.genericId, functionName = rec.functionName, childType = ResolvedType.fromTypeSignature context.lookupTable availableTypes context.currentModule rec.childType })

                        _ ->
                            Nothing
                )
                declarations
                ++ context.genericProviders
        , genericTodos = getTodos (GenericTodo.getTodos context availableTypes) ++ context.genericTodos
      }
    )


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    List.filterMap
        (\( moduleName, todo ) ->
            GenericTodo.todoErrors projectContext moduleName todo
        )
        projectContext.genericTodos
