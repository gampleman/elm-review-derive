module CodeGen exposing (rule)

import AssocList as Dict exposing (Dict)
import AssocSet exposing (Set)
import CodeGen.AutoCodecTodo as AutoCodecTodo exposing (AutoCodecTodo)
import CodeGen.CodecTodo as CodecTodo exposing (CodecTodo)
import CodeGen.FromStringTodo as FromStringTodo exposing (FromStringTodo)
import CodeGen.ListVariantsTodo as ListVariantsTodo exposing (ListVariantsTodo)
import CodeGen.RandomGeneratorTodo as RandomGeneratorTodo
import CodeGen.ToStringTodo as ToStringTodo exposing (ToStringTodo)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import QualifiedType exposing (ExistingImport, QualifiedType, TypeAnnotation_, TypeOrTypeAlias(..), Type_)
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, ModuleKey, Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "CodeGen" initialProjectContext
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule |> Rule.withModuleNameLookupTable |> Rule.withMetadata
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationVisitor


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


type alias ProjectContext =
    { types : List ( ModuleName, TypeOrTypeAlias )
    , codecs : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
    , autoCodecs : List { moduleName : ModuleName, functionName : String, typeVar : QualifiedType }
    , codecTodos : List ( ModuleName, CodecTodo )
    , autoCodecTodos : List ( ModuleName, AutoCodecTodo )
    , toStringTodos : List ( ModuleName, ToStringTodo )
    , fromStringTodos : List ( ModuleName, FromStringTodo )
    , listVariantsTodos : List ( ModuleName, ListVariantsTodo )
    , randomGeneratorTodos : List ( ModuleName, CodecTodo )
    , imports : Dict ModuleName { newImportStartRow : Int, existingImports : List ExistingImport }
    , moduleKeys : Dict ModuleName ModuleKey
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable.ModuleNameLookupTable
    , types : List TypeOrTypeAlias
    , codecs : List { functionName : String, typeVar : QualifiedType }
    , autoCodecs : List { functionName : String, typeVar : QualifiedType }
    , codecTodos : List CodecTodo
    , autoCodecTodos : List AutoCodecTodo
    , toStringTodos : List ToStringTodo
    , fromStringTodos : List FromStringTodo
    , listVariantsTodos : List ListVariantsTodo
    , randomGeneratorTodos : List CodecTodo
    , importStartRow : Maybe Int
    , imports : List ExistingImport
    , currentModule : ModuleName
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { types = []
    , codecs = []
    , autoCodecs = []
    , codecTodos = []
    , autoCodecTodos = []
    , toStringTodos = []
    , fromStringTodos = []
    , listVariantsTodos = []
    , randomGeneratorTodos = []
    , imports = Dict.empty
    , moduleKeys = Dict.empty
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    let
        moduleName =
            Rule.moduleNameFromMetadata metadata

        filterTodos : List ( ModuleName, a ) -> List a
        filterTodos =
            List.filterMap
                (\( moduleName_, todo ) ->
                    if moduleName_ == moduleName then
                        Just todo

                    else
                        Nothing
                )
    in
    { lookupTable = lookupTable
    , types = projectContext.types |> List.filter (Tuple.first >> (==) moduleName) |> List.map Tuple.second
    , codecs =
        List.filterMap
            (\a ->
                if a.moduleName == moduleName then
                    Just { functionName = a.functionName, typeVar = a.typeVar }

                else
                    Nothing
            )
            projectContext.codecs
    , autoCodecs =
        List.filterMap
            (\a ->
                if a.moduleName == moduleName then
                    Just { functionName = a.functionName, typeVar = a.typeVar }

                else
                    Nothing
            )
            projectContext.autoCodecs
    , codecTodos = filterTodos projectContext.codecTodos
    , autoCodecTodos = filterTodos projectContext.autoCodecTodos
    , toStringTodos = filterTodos projectContext.toStringTodos
    , fromStringTodos = filterTodos projectContext.fromStringTodos
    , listVariantsTodos = filterTodos projectContext.listVariantsTodos
    , randomGeneratorTodos = filterTodos projectContext.randomGeneratorTodos
    , importStartRow = Dict.get moduleName projectContext.imports |> Maybe.map .newImportStartRow
    , imports = Dict.get moduleName projectContext.imports |> Maybe.map .existingImports |> Maybe.withDefault []
    , currentModule = moduleName
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
    { types = List.map (Tuple.pair moduleName) moduleContext.types
    , codecs =
        List.map
            (\a -> { moduleName = moduleName, functionName = a.functionName, typeVar = a.typeVar })
            moduleContext.codecs
    , autoCodecs =
        List.map
            (\a -> { moduleName = moduleName, functionName = a.functionName, typeVar = a.typeVar })
            moduleContext.autoCodecs
    , codecTodos = mapTodo moduleContext.codecTodos
    , autoCodecTodos = mapTodo moduleContext.autoCodecTodos
    , toStringTodos = mapTodo moduleContext.toStringTodos
    , fromStringTodos = mapTodo moduleContext.fromStringTodos
    , listVariantsTodos = mapTodo moduleContext.listVariantsTodos
    , randomGeneratorTodos = mapTodo moduleContext.randomGeneratorTodos
    , imports =
        Dict.singleton
            moduleName
            { newImportStartRow = Maybe.withDefault 3 moduleContext.importStartRow
            , existingImports = moduleContext.imports
            }
    , moduleKeys = Dict.singleton moduleName moduleKey
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { types = newContext.types ++ previousContext.types
    , codecs = newContext.codecs ++ previousContext.codecs
    , autoCodecs = newContext.autoCodecs ++ previousContext.autoCodecs
    , codecTodos = newContext.codecTodos ++ previousContext.codecTodos
    , autoCodecTodos = newContext.autoCodecTodos ++ previousContext.autoCodecTodos
    , toStringTodos = newContext.toStringTodos ++ previousContext.toStringTodos
    , fromStringTodos = newContext.fromStringTodos ++ previousContext.fromStringTodos
    , listVariantsTodos = newContext.listVariantsTodos ++ previousContext.listVariantsTodos
    , randomGeneratorTodos = newContext.randomGeneratorTodos ++ previousContext.randomGeneratorTodos
    , imports = Dict.union newContext.imports previousContext.imports
    , moduleKeys = Dict.union newContext.moduleKeys previousContext.moduleKeys
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
        | codecTodos = getTodos (CodecTodo.getTodos context) ++ context.codecTodos
        , autoCodecTodos = getTodos (AutoCodecTodo.getTodos context) ++ context.autoCodecTodos
        , toStringTodos = getTodos (ToStringTodo.getTodos context) ++ context.toStringTodos
        , fromStringTodos = getTodos (FromStringTodo.getTodos context) ++ context.fromStringTodos
        , listVariantsTodos = getTodos (ListVariantsTodo.getListAllTodo context) ++ context.listVariantsTodos
        , randomGeneratorTodos = getTodos (RandomGeneratorTodo.getTodos context) ++ context.randomGeneratorTodos
        , types =
            List.filterMap (declarationVisitorGetTypes context) declarations
                ++ context.types
        , codecs =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node _ (Declaration.FunctionDeclaration function) ->
                            CodecTodo.declarationVisitorGetCodecs context function

                        _ ->
                            Nothing
                )
                declarations
                ++ context.codecs
        , autoCodecs =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node _ (Declaration.FunctionDeclaration function) ->
                            AutoCodecTodo.declarationVisitorGetCodecs context function

                        _ ->
                            Nothing
                )
                declarations
                ++ context.codecs
        , importStartRow =
            List.map (Node.range >> .start >> .row) declarations |> List.minimum
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
    let
        codecTypeTodoFixes : List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
        codecTypeTodoFixes =
            CodecTodo.codecTypeTodoFixes projectContext

        autoCodecTypeTodoFixes : List { moduleName : ModuleName, fix : Review.Fix.Fix, newImports : Set ModuleName }
        autoCodecTypeTodoFixes =
            AutoCodecTodo.codecTypeTodoFixes projectContext

        randomGeneratorTypeTodoFixes : List ( ModuleName, Review.Fix.Fix )
        randomGeneratorTypeTodoFixes =
            RandomGeneratorTodo.randomGeneratorTypeTodoFixes projectContext
    in
    List.filterMap
        (\( moduleName, todo ) -> CodecTodo.todoErrors projectContext moduleName codecTypeTodoFixes todo)
        projectContext.codecTodos
        ++ List.filterMap
            (\( moduleName, todo ) -> AutoCodecTodo.todoErrors projectContext moduleName autoCodecTypeTodoFixes todo)
            projectContext.autoCodecTodos
        ++ List.filterMap
            (\( moduleName, todo ) -> ToStringTodo.todoErrors projectContext moduleName todo)
            projectContext.toStringTodos
        ++ List.filterMap
            (\( moduleName, todo ) -> FromStringTodo.todoErrors projectContext moduleName todo)
            projectContext.fromStringTodos
        ++ List.filterMap
            (\( moduleName, todo ) -> ListVariantsTodo.todoErrors projectContext moduleName todo)
            projectContext.listVariantsTodos
        ++ List.filterMap
            (\( moduleName, todo ) ->
                RandomGeneratorTodo.todoErrors projectContext moduleName randomGeneratorTypeTodoFixes todo
            )
            projectContext.randomGeneratorTodos
