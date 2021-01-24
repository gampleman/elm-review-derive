module TodoItForMe exposing (rule)

import Dict exposing (Dict)
import Elm.Module
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Elm.Writer
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


rule : Rule
rule =
    Rule.newProjectRuleSchema "NoUnused.CustomTypeConstructors" initialProjectContext
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



-- MODULE VISITOR


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationEnterVisitor declarationVisitor



-- CONTEXT


type alias ModuleNameAsString =
    String


type alias CustomTypeName =
    String


type alias ConstructorName =
    String


type ExposedConstructors
    = ExposedConstructors
        { moduleKey : Rule.ModuleKey
        , customTypes : Dict CustomTypeName (Dict ConstructorName (Node ConstructorName))
        }


type alias ProjectContext =
    { exposedModules : Set ModuleNameAsString
    , exposedConstructors : Dict ModuleNameAsString ExposedConstructors
    , usedConstructors : Dict ModuleNameAsString (Set ConstructorName)
    , typeAliases : List ( ModuleName, TypeAlias )
    , todos : List ( ModuleName, Rule.ModuleKey, Todo )
    }


type alias Todo =
    { functionName : Node String, typeVar : ( ModuleName, String ), declaration : Node Declaration }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , exposedCustomTypesWithConstructors : Set CustomTypeName
    , isExposed : Bool
    , exposesEverything : Bool
    , exposedConstructors : Dict ModuleNameAsString ExposedConstructors
    , declaredTypesWithConstructors : Dict CustomTypeName (Dict ConstructorName (Node ConstructorName))
    , usedFunctionsOrValues : Dict ModuleNameAsString (Set ConstructorName)
    , typeAliases : List TypeAlias
    , todos : List Todo
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { exposedModules = Set.empty
    , exposedConstructors = Dict.empty
    , usedConstructors = Dict.empty
    , typeAliases = []
    , todos = []
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    let
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { lookupTable = lookupTable
    , exposedCustomTypesWithConstructors = Set.empty
    , isExposed = Set.member (moduleName |> String.join ".") projectContext.exposedModules
    , exposedConstructors = projectContext.exposedConstructors
    , exposesEverything = False
    , declaredTypesWithConstructors = Dict.empty
    , usedFunctionsOrValues = Dict.empty
    , typeAliases = projectContext.typeAliases |> List.filter (Tuple.first >> (==) moduleName) |> List.map Tuple.second
    , todos =
        List.filterMap
            (\( moduleName_, _, todo ) ->
                if moduleName_ == moduleName then
                    Just todo

                else
                    Nothing
            )
            projectContext.todos
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    let
        localUsed : Set ConstructorName
        localUsed =
            moduleContext.usedFunctionsOrValues
                |> Dict.get ""
                |> Maybe.withDefault Set.empty

        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata

        moduleNameAsString : ModuleNameAsString
        moduleNameAsString =
            String.join "." moduleName
    in
    { exposedModules = Set.empty
    , exposedConstructors =
        if moduleContext.isExposed then
            Dict.empty

        else
            Dict.singleton
                moduleNameAsString
                (ExposedConstructors
                    { moduleKey = moduleKey
                    , customTypes = moduleContext.declaredTypesWithConstructors
                    }
                )
    , usedConstructors =
        moduleContext.usedFunctionsOrValues
            |> Dict.remove ""
            |> Dict.insert moduleNameAsString localUsed
    , typeAliases = List.map (Tuple.pair moduleName) moduleContext.typeAliases
    , todos = List.map (\todo -> ( moduleName, moduleKey, todo )) moduleContext.todos
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { exposedModules = previousContext.exposedModules
    , exposedConstructors = Dict.union newContext.exposedConstructors previousContext.exposedConstructors
    , usedConstructors =
        Dict.merge
            Dict.insert
            (\key newUsed previousUsed dict -> Dict.insert key (Set.union newUsed previousUsed) dict)
            Dict.insert
            newContext.usedConstructors
            previousContext.usedConstructors
            Dict.empty
    , typeAliases = newContext.typeAliases ++ previousContext.typeAliases
    , todos = newContext.todos ++ previousContext.todos
    }



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case maybeElmJson |> Maybe.map .project of
        Just (Elm.Project.Package package) ->
            let
                exposedModules : List Elm.Module.Name
                exposedModules =
                    case package.exposed of
                        Elm.Project.ExposedList list ->
                            list

                        Elm.Project.ExposedDict list ->
                            List.concatMap Tuple.second list

                exposedNames : Set String
                exposedNames =
                    exposedModules
                        |> List.map Elm.Module.toString
                        |> Set.fromList
            in
            ( [], { projectContext | exposedModules = exposedNames } )

        Just (Elm.Project.Application _) ->
            ( [], projectContext )

        Nothing ->
            ( [], projectContext )



-- MODULE DEFINITION VISITOR


moduleDefinitionVisitor : Node Module -> ModuleContext -> ( List nothing, ModuleContext )
moduleDefinitionVisitor moduleNode context =
    case Module.exposingList (Node.value moduleNode) of
        Exposing.All _ ->
            ( [], { context | exposesEverything = True } )

        Exposing.Explicit list ->
            let
                names : List String
                names =
                    List.filterMap
                        (\node_ ->
                            case Node.value node_ of
                                Exposing.TypeExpose { name } ->
                                    Just name

                                _ ->
                                    Nothing
                        )
                        list
            in
            ( []
            , { context
                | exposedCustomTypesWithConstructors =
                    Set.union (Set.fromList names) context.exposedCustomTypesWithConstructors
              }
            )



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ( List nothing, ModuleContext )
declarationVisitor node_ context =
    case Node.value node_ of
        Declaration.CustomTypeDeclaration { name, constructors } ->
            let
                constructorsForCustomType : Dict String (Node String)
                constructorsForCustomType =
                    List.foldl
                        (\constructor dict ->
                            let
                                nameNode : Node String
                                nameNode =
                                    (Node.value constructor).name
                            in
                            Dict.insert
                                (Node.value nameNode)
                                nameNode
                                dict
                        )
                        Dict.empty
                        constructors
            in
            ( []
            , { context
                | declaredTypesWithConstructors =
                    Dict.insert
                        (Node.value name)
                        constructorsForCustomType
                        context.declaredTypesWithConstructors
              }
            )

        Declaration.FunctionDeclaration function ->
            ( []
            , { context
                | todos =
                    (case ( function.signature, function.declaration ) of
                        ( Just (Node _ signature), Node _ declaration ) ->
                            case signature.typeAnnotation of
                                Node _ (TypeAnnotation.Typed (Node _ ( [], "Codec" )) [ Node _ (TypeAnnotation.Typed (Node _ codecType) []) ]) ->
                                    case declaration.expression of
                                        Node _ (Expression.Application ((Node _ (Expression.FunctionOrValue [ "Debug" ] "todo")) :: _)) ->
                                            [ { functionName = signature.name, typeVar = codecType, declaration = node_ } ]

                                        _ ->
                                            []

                                _ ->
                                    []

                        _ ->
                            []
                    )
                        ++ context.todos
              }
            )

        Declaration.AliasDeclaration typeAlias ->
            ( [], { context | typeAliases = typeAlias :: context.typeAliases } )

        _ ->
            ( [], context )


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    projectContext.todos
        |> List.filterMap
            (\( moduleName, moduleKey, todo ) ->
                let
                    localTypeAliases : List TypeAlias
                    localTypeAliases =
                        List.filter (Tuple.first >> (==) moduleName) projectContext.typeAliases
                            |> List.map Tuple.second
                in
                case find (\typeAlias -> Node.value typeAlias.name == Tuple.second todo.typeVar) localTypeAliases of
                    Just typeAlias ->
                        case typeAlias.typeAnnotation of
                            Node _ (TypeAnnotation.Record fields) ->
                                let
                                    fix : String
                                    fix =
                                        List.foldl
                                            (\(Node _ ( Node _ fieldName, Node _ typeAnnotation )) code ->
                                                code
                                                    |> pipeLeft
                                                        (application
                                                            [ functionOrValue [ "Serialize" ] "field"
                                                            , Expression.RecordAccessFunction fieldName |> node
                                                            , case typeAnnotation of
                                                                TypeAnnotation.Typed (Node _ ( _, "Int" )) [] ->
                                                                    functionOrValue [ "Serialize" ] "int"

                                                                TypeAnnotation.Typed (Node _ ( _, "Float" )) [] ->
                                                                    functionOrValue [ "Serialize" ] "float"

                                                                TypeAnnotation.Typed (Node _ ( _, "String" )) [] ->
                                                                    functionOrValue [ "Serialize" ] "string"

                                                                TypeAnnotation.Typed (Node _ ( _, typed )) [] ->
                                                                    functionOrValue [] (uncapitalize typed ++ "Codec")

                                                                _ ->
                                                                    application
                                                                        [ functionOrValue [ "Debug" ] "todo"
                                                                        , Expression.Literal "" |> node
                                                                        ]
                                                            ]
                                                        )
                                            )
                                            (application
                                                [ functionOrValue [ "Codec" ] "record"
                                                , functionOrValue [] (Node.value typeAlias.name)
                                                ]
                                            )
                                            fields
                                            |> pipeLeft (application [ functionOrValue [ "Serialize" ] "finishRecord" ])
                                            |> Elm.Writer.writeExpression
                                            |> Elm.Writer.write
                                            |> String.replace "|>" "\n        |>"
                                            |> (++)
                                                ((Node.value todo.functionName ++ " : Codec " ++ moduleNameToString todo.typeVar ++ "\n")
                                                    ++ (Node.value todo.functionName ++ " =\n    ")
                                                )

                                    range =
                                        Node.range todo.declaration
                                in
                                Rule.errorForModuleWithFix
                                    moduleKey
                                    { message = "Here's my attempt to complete this stub"
                                    , details = [ "" ]
                                    }
                                    range
                                    [ Review.Fix.replaceRangeBy range fix ]
                                    |> Just

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing
            )


node =
    Node Elm.Syntax.Range.emptyRange


application =
    Expression.Application >> node


functionOrValue : ModuleName -> String -> Node Expression
functionOrValue moduleName functionOrValueName =
    Expression.FunctionOrValue moduleName functionOrValueName |> node


pipeLeft : Node Expression -> Node Expression -> Node Expression
pipeLeft eRight eLeft =
    Expression.OperatorApplication "|>" Infix.Left eLeft eRight |> node


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.
find (\\num -> num > 5) [ 2, 4, 6, 8 ]
--> Just 6

Borrowed from elm-community/list-extra

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest



-- TYPE ANNOTATION UTILITY FUNCTIONS


collectGenericsFromTypeAnnotation : Node TypeAnnotation -> List String
collectGenericsFromTypeAnnotation node_ =
    case Node.value node_ of
        TypeAnnotation.FunctionTypeAnnotation a b ->
            collectGenericsFromTypeAnnotation a ++ collectGenericsFromTypeAnnotation b

        TypeAnnotation.Typed _ params ->
            List.concatMap collectGenericsFromTypeAnnotation params

        TypeAnnotation.Record list ->
            list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.GenericRecord _ list ->
            Node.value list
                |> List.concatMap (Node.value >> Tuple.second >> collectGenericsFromTypeAnnotation)

        TypeAnnotation.Tupled list ->
            List.concatMap collectGenericsFromTypeAnnotation list

        TypeAnnotation.GenericType var ->
            [ var ]

        TypeAnnotation.Unit ->
            []


listAtIndex : Int -> List a -> Maybe a
listAtIndex index list =
    case ( index, list ) of
        ( 0, a :: [] ) ->
            Just a

        ( _, [] ) ->
            Nothing

        ( n, _ :: rest ) ->
            listAtIndex (n - 1) rest


uncapitalize : String -> String
uncapitalize text =
    String.toLower (String.left 1 text) ++ String.dropLeft 1 text


moduleNameToString : ( ModuleName, String ) -> String
moduleNameToString ( moduleName, name ) =
    String.join "." (moduleName ++ [ name ])
