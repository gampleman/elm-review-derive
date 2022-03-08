module QualifiedType exposing (ExistingImport, QualifiedType(..), TypeAnnotation_(..), TypeOrTypeAlias(..), Type_, ValueConstructor_, create, createFromType, createFromTypeAlias, getTypeData, isPrimitiveType, moduleName, name, qualifiedPath, toString)

import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import List.Extra as List
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


type QualifiedType
    = QualifiedType
        { qualifiedPath : List String
        , name : String
        }


type alias ExistingImport =
    { moduleName : ModuleName, moduleAlias : Maybe String, exposingList : Exposing }


create : ModuleNameLookupTable -> ModuleName -> Node ( a, String ) -> Maybe QualifiedType
create moduleNameLookupTable currentModule (Node range ( _, name_ )) =
    case ModuleNameLookupTable.moduleNameAt moduleNameLookupTable range of
        Just [] ->
            Just <| QualifiedType { qualifiedPath = currentModule, name = name_ }

        Just moduleName_ ->
            Just <| QualifiedType { qualifiedPath = moduleName_, name = name_ }

        Nothing ->
            Nothing


createFromType : ModuleName -> Elm.Syntax.Type.Type -> QualifiedType
createFromType currentModule type_ =
    QualifiedType { qualifiedPath = currentModule, name = Node.value type_.name }


createFromTypeAlias : ModuleName -> TypeAlias -> QualifiedType
createFromTypeAlias currentModule typeAlias =
    QualifiedType { qualifiedPath = currentModule, name = Node.value typeAlias.name }


qualifiedPath : QualifiedType -> List String
qualifiedPath (QualifiedType a) =
    a.qualifiedPath


name : QualifiedType -> String
name (QualifiedType a) =
    a.name


isExposed : Exposing -> String -> Bool
isExposed exposing_ functionOrValue =
    case exposing_ of
        Elm.Syntax.Exposing.All _ ->
            True

        Elm.Syntax.Exposing.Explicit exposings ->
            List.any
                (\(Node _ a) ->
                    case a of
                        Elm.Syntax.Exposing.InfixExpose _ ->
                            False

                        Elm.Syntax.Exposing.FunctionExpose function ->
                            functionOrValue == function

                        Elm.Syntax.Exposing.TypeOrAliasExpose typeOrAlias ->
                            functionOrValue == typeOrAlias

                        Elm.Syntax.Exposing.TypeExpose typeExpose ->
                            functionOrValue == typeExpose.name
                )
                exposings


toString : ModuleName -> List ExistingImport -> QualifiedType -> String
toString currentModule existingImports qualifiedType =
    helper moduleNameToString currentModule existingImports qualifiedType


moduleName : ModuleName -> List ExistingImport -> QualifiedType -> ModuleName
moduleName currentModule existingImports qualifiedType =
    helper (\moduleName_ _ -> moduleName_) currentModule existingImports qualifiedType


helper : (ModuleName -> String -> a) -> ModuleName -> List ExistingImport -> QualifiedType -> a
helper func currentModule existingImports (QualifiedType a) =
    if a.qualifiedPath == currentModule then
        func [] a.name

    else
        case List.find (.moduleName >> (==) a.qualifiedPath) existingImports of
            Just { moduleAlias, exposingList } ->
                if isExposed exposingList a.name then
                    func [] a.name

                else
                    case moduleAlias of
                        Just moduleAlias_ ->
                            func [ moduleAlias_ ] a.name

                        Nothing ->
                            func a.qualifiedPath a.name

            Nothing ->
                func a.qualifiedPath a.name


moduleNameToString : ModuleName -> String -> String
moduleNameToString moduleName_ name_ =
    String.join "." (moduleName_ ++ [ name_ ])


getTypeData : List ( ModuleName, TypeOrTypeAlias ) -> QualifiedType -> Maybe ( ModuleName, TypeOrTypeAlias )
getTypeData types qualifiedType =
    List.filterMap
        (\( typeModule, type_ ) ->
            if typeModule == qualifiedPath qualifiedType then
                case type_ of
                    TypeValue typeValue ->
                        if typeValue.qualifiedType == qualifiedType then
                            Just ( typeModule, type_ )

                        else
                            Nothing

                    TypeAliasValue typeAliasName _ ->
                        if typeAliasName == qualifiedType then
                            Just ( typeModule, type_ )

                        else
                            Nothing

            else
                Nothing
        )
        types
        |> List.head


type alias Type_ =
    { qualifiedType : QualifiedType
    , generics : List String
    , constructors : List ValueConstructor_
    }


{-| Syntax for a custom type value constructor
-}
type alias ValueConstructor_ =
    { name : String
    , arguments : List TypeAnnotation_
    }


type TypeAnnotation_
    = GenericType_ String
    | Typed_ QualifiedType (List TypeAnnotation_)
    | Unit_
    | Tupled_ (List TypeAnnotation_)
    | Record_ (List ( String, TypeAnnotation_ ))
    | GenericRecord_ String (List ( String, TypeAnnotation_ ))
    | FunctionTypeAnnotation_ TypeAnnotation_ TypeAnnotation_


type TypeOrTypeAlias
    = TypeValue Type_
    | TypeAliasValue QualifiedType (List ( String, TypeAnnotation_ ))


isPrimitiveType : QualifiedType -> Bool
isPrimitiveType qualifiedType =
    case ( qualifiedPath qualifiedType, name qualifiedType ) of
        ( [ "Basics" ], "Int" ) ->
            True

        ( [ "Basics" ], "Float" ) ->
            True

        ( [ "Basics" ], "String" ) ->
            True

        ( [ "Basics" ], "Bool" ) ->
            True

        ( [ "Basics" ], "Maybe" ) ->
            True

        ( [ "Dict" ], "Dict" ) ->
            True

        ( [ "Set" ], "Set" ) ->
            True

        ( [ "Basics" ], "Result" ) ->
            True

        ( [ "List" ], "List" ) ->
            True

        ( [ "Array" ], "Array" ) ->
            True

        _ ->
            False
