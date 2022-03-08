module CodeGen exposing (rule)

import AssocList exposing (Dict)
import AssocSet exposing (Set)
import CodeGen.Builtin.Codec
import CodeGen.Builtin.FromString
import CodeGen.Builtin.JsonEncoder
import CodeGen.Builtin.ListAllVariants
import CodeGen.Builtin.Random
import CodeGen.Builtin.ToString
import CodeGen.CodecTodo as CodecTodo exposing (CodecTodo)
import CodeGen.FromStringTodo as FromStringTodo exposing (FromStringTodo)
import CodeGen.GenericTodo as GenericTodo
import CodeGen.Helpers
import CodeGen.ListVariantsTodo as ListVariantsTodo exposing (ListVariantsTodo)
import CodeGen.MigrateTodo as MigrateTodo exposing (MigrateTodo)
import CodeGen.RandomGeneratorTodo as RandomGeneratorTodo
import CodeGen.ToStringTodo as ToStringTodo exposing (ToStringTodo)
import Dict
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import QualifiedType exposing (ExistingImport, QualifiedType, TypeAnnotation_, TypeOrTypeAlias(..), Type_)
import ResolvedType exposing (ResolvedType)
import Review.Fix
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


initializeGenerics : List GenericTodo.Generic -> Dict.Dict String Dependency -> ProjectContext -> ( List (Error { useErrorForModule : () }), ProjectContext )
initializeGenerics generics deps context =
    ( []
    , { context | generics = GenericTodo.buildFullGeneric (Dict.keys deps) generics }
    )


type alias ProjectContext =
    { types : List ( ModuleName, ResolvedType )

    -- , codecs : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
    -- , generators : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
    -- , migrateFunctions :
    --     List
    --         { moduleName : ModuleName
    --         , functionName : String
    --         , oldType : QualifiedType
    --         , newType : QualifiedType
    --         }
    -- , codecTodos : List ( ModuleName, CodecTodo )
    -- , toStringTodos : List ( ModuleName, ToStringTodo )
    -- , fromStringTodos : List ( ModuleName, FromStringTodo )
    -- , listVariantsTodos : List ( ModuleName, ListVariantsTodo )
    -- , randomGeneratorTodos : List ( ModuleName, CodecTodo )
    -- , migrateTodos : List ( ModuleName, MigrateTodo )
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

    -- , codecs : List { functionName : String, typeVar : QualifiedType }
    -- , generators : List { functionName : String, typeVar : QualifiedType }
    -- , migrateFunctions : List { functionName : String, oldType : QualifiedType, newType : QualifiedType }
    -- , autoCodecs : List { functionName : String, typeVar : QualifiedType }
    -- , codecTodos : List CodecTodo
    -- , toStringTodos : List ToStringTodo
    -- , fromStringTodos : List FromStringTodo
    -- , listVariantsTodos : List ListVariantsTodo
    -- , randomGeneratorTodos : List CodecTodo
    -- , migrateTodos : List MigrateTodo
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

    -- , codecs = []
    -- , generators = []
    -- , migrateFunctions = []
    -- , codecTodos = []
    -- , toStringTodos = []
    -- , fromStringTodos = []
    -- , listVariantsTodos = []
    -- , randomGeneratorTodos = []
    -- , migrateTodos = []
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

    -- , codecs = []
    -- , generators = []
    -- , autoCodecs = []
    -- , migrateFunctions = []
    -- , codecTodos = []
    -- , toStringTodos = []
    -- , fromStringTodos = []
    -- , listVariantsTodos = []
    -- , randomGeneratorTodos = []
    -- , migrateTodos = []
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

    -- , codecs =
    --     List.map
    --         (\a -> { moduleName = moduleName, functionName = a.functionName, typeVar = a.typeVar })
    --         moduleContext.codecs
    -- , generators =
    --     List.map
    --         (\a -> { moduleName = moduleName, functionName = a.functionName, typeVar = a.typeVar })
    --         moduleContext.generators
    -- , migrateFunctions =
    --     List.map
    --         (\a ->
    --             { moduleName = moduleName
    --             , functionName = a.functionName
    --             , oldType = a.oldType
    --             , newType = a.newType
    --             }
    --         )
    --         moduleContext.migrateFunctions
    -- , codecTodos = mapTodo moduleContext.codecTodos
    -- , toStringTodos = mapTodo moduleContext.toStringTodos
    -- , fromStringTodos = mapTodo moduleContext.fromStringTodos
    -- , listVariantsTodos = mapTodo moduleContext.listVariantsTodos
    -- , randomGeneratorTodos = mapTodo moduleContext.randomGeneratorTodos
    -- , migrateTodos = mapTodo moduleContext.migrateTodos
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

    -- , codecs = newContext.codecs ++ previousContext.codecs
    -- , generators = newContext.generators ++ previousContext.generators
    -- , migrateFunctions = newContext.migrateFunctions ++ previousContext.migrateFunctions
    -- , codecTodos = newContext.codecTodos ++ previousContext.codecTodos
    -- , toStringTodos = newContext.toStringTodos ++ previousContext.toStringTodos
    -- , fromStringTodos = newContext.fromStringTodos ++ previousContext.fromStringTodos
    -- , listVariantsTodos = newContext.listVariantsTodos ++ previousContext.listVariantsTodos
    -- , randomGeneratorTodos = newContext.randomGeneratorTodos ++ previousContext.randomGeneratorTodos
    -- , migrateTodos = newContext.migrateTodos ++ previousContext.migrateTodos
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


convertTypeAnnotation : ModuleContext -> TypeAnnotation -> TypeAnnotation_
convertTypeAnnotation moduleContext typeAnnotation =
    case typeAnnotation of
        TypeAnnotation.GenericType string ->
            QualifiedType.GenericType_ string

        TypeAnnotation.Typed a nodes ->
            case QualifiedType.create moduleContext.lookupTable moduleContext.currentModule a of
                Just qualified ->
                    QualifiedType.Typed_
                        qualified
                        (List.map (Node.value >> convertTypeAnnotation moduleContext) nodes)

                Nothing ->
                    QualifiedType.Unit_

        TypeAnnotation.Unit ->
            QualifiedType.Unit_

        TypeAnnotation.Tupled nodes ->
            QualifiedType.Tupled_ (List.map (Node.value >> convertTypeAnnotation moduleContext) nodes)

        TypeAnnotation.Record recordDefinition ->
            QualifiedType.Record_ (convertRecordDefinition moduleContext recordDefinition)

        TypeAnnotation.GenericRecord a (Node _ recordDefinition) ->
            QualifiedType.GenericRecord_
                (Node.value a)
                (convertRecordDefinition moduleContext recordDefinition)

        TypeAnnotation.FunctionTypeAnnotation (Node _ a) (Node _ b) ->
            QualifiedType.FunctionTypeAnnotation_
                (convertTypeAnnotation moduleContext a)
                (convertTypeAnnotation moduleContext b)


convertRecordDefinition : ModuleContext -> List (Node ( Node a, Node TypeAnnotation )) -> List ( a, TypeAnnotation_ )
convertRecordDefinition moduleContext recordDefinition =
    List.map
        (\(Node _ ( Node _ fieldName, Node _ fieldValue )) ->
            ( fieldName, convertTypeAnnotation moduleContext fieldValue )
        )
        recordDefinition


convertType : ModuleContext -> Elm.Syntax.Type.Type -> Type_
convertType moduleContext type_ =
    { qualifiedType = QualifiedType.createFromType moduleContext.currentModule type_
    , generics = List.map Node.value type_.generics
    , constructors =
        List.map
            (\(Node _ constructor) ->
                { name = Node.value constructor.name
                , arguments =
                    List.map
                        (Node.value >> convertTypeAnnotation moduleContext)
                        constructor.arguments
                }
            )
            type_.constructors
    }


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

        -- codecTodos =
        --     getTodos (CodecTodo.getTodos context)
        -- toStringTodos =
        --     getTodos (ToStringTodo.getTodos context)
        -- fromStringTodos =
        --     getTodos (FromStringTodo.getTodos context)
        -- listVariantsTodos =
        --     getTodos (ListVariantsTodo.getListAllTodo context)
        -- randomGeneratorTodos =
        --     getTodos (RandomGeneratorTodo.getTodos context)
        -- migrateTodos =
        --     if
        --         List.isEmpty codecTodos
        --             && List.isEmpty toStringTodos
        --             && List.isEmpty fromStringTodos
        --             && List.isEmpty listVariantsTodos
        --             && List.isEmpty randomGeneratorTodos
        --     then
        --         getTodos (MigrateTodo.getTodos context)
        --     else
        --         []
    in
    ( []
    , { context
        | --codecTodos = codecTodos ++ context.codecTodos
          -- , toStringTodos = toStringTodos ++ context.toStringTodos
          -- , fromStringTodos = fromStringTodos ++ context.fromStringTodos
          -- , listVariantsTodos = listVariantsTodos ++ context.listVariantsTodos
          -- , randomGeneratorTodos = randomGeneratorTodos ++ context.randomGeneratorTodos
          -- , migrateTodos = migrateTodos ++ context.migrateTodos
          -- types =
          --     List.filterMap (declarationVisitorGetTypes context) declarations
          --         ++ context.types
          types = types ++ context.types

        -- , codecs =
        --     List.filterMap
        --         (\declaration ->
        --             case declaration of
        --                 Node _ (Declaration.FunctionDeclaration function) ->
        --                     CodecTodo.declarationVisitorGetCodecs context function
        --                 _ ->
        --                     Nothing
        --         )
        --         declarations
        --         ++ context.codecs
        -- , generators =
        --     List.filterMap
        --         (\declaration ->
        --             case declaration of
        --                 Node _ (Declaration.FunctionDeclaration function) ->
        --                     RandomGeneratorTodo.declarationVisitorGetGenerators context function
        --                 _ ->
        --                     Nothing
        --         )
        --         declarations
        --         ++ context.generators
        -- , migrateFunctions =
        --     List.filterMap
        --         (\declaration ->
        --             case declaration of
        --                 Node _ (Declaration.FunctionDeclaration function) ->
        --                     MigrateTodo.declarationVisitorGetMigrateFunctions context function
        --                 _ ->
        --                     Nothing
        --         )
        --         declarations
        --         ++ context.migrateFunctions
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


declarationVisitorGetTypes : ModuleContext -> Node Declaration -> Maybe TypeOrTypeAlias
declarationVisitorGetTypes context node_ =
    case Node.value node_ of
        Declaration.CustomTypeDeclaration customType ->
            convertType context customType |> TypeValue |> Just

        Declaration.AliasDeclaration typeAlias ->
            case Node.value typeAlias.typeAnnotation of
                TypeAnnotation.Record record ->
                    TypeAliasValue
                        (QualifiedType.createFromTypeAlias context.currentModule typeAlias)
                        (convertRecordDefinition context record)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    -- let
    --     codecTypeTodoFixes : List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
    --     codecTypeTodoFixes =
    --         CodecTodo.codecTypeTodoFixes projectContext
    --     randomGeneratorTypeTodoFixes : List ( ModuleName, Review.Fix.Fix )
    --     randomGeneratorTypeTodoFixes =
    --         RandomGeneratorTodo.randomGeneratorTypeTodoFixes projectContext
    --     migrateTodoFixes : List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
    --     migrateTodoFixes =
    --         MigrateTodo.migrateTypeTodoFixes projectContext
    -- in
    -- List.filterMap
    --     (\( moduleName, todo ) -> CodecTodo.todoErrors projectContext moduleName codecTypeTodoFixes todo)
    --     projectContext.codecTodos
    --     ++ List.filterMap
    --         (\( moduleName, todo ) -> ToStringTodo.todoErrors projectContext moduleName todo)
    --         projectContext.toStringTodos
    --     ++ List.filterMap
    --         (\( moduleName, todo ) -> FromStringTodo.todoErrors projectContext moduleName todo)
    --         projectContext.fromStringTodos
    --     ++ List.filterMap
    --         (\( moduleName, todo ) -> ListVariantsTodo.todoErrors projectContext moduleName todo)
    --         projectContext.listVariantsTodos
    --     ++ List.filterMap
    --         (\( moduleName, todo ) ->
    --             RandomGeneratorTodo.todoErrors projectContext moduleName randomGeneratorTypeTodoFixes todo
    --         )
    --         projectContext.randomGeneratorTodos
    --     ++ List.filterMap
    --         (\( moduleName, todo ) ->
    --             MigrateTodo.todoErrors projectContext moduleName migrateTodoFixes todo
    --         )
    --         projectContext.migrateTodos
    List.filterMap
        (\( moduleName, todo ) ->
            GenericTodo.todoErrors projectContext moduleName todo
        )
        projectContext.genericTodos
