module QualifiedType exposing (QualifiedType, TypeOrTypeAlias(..), actualPath, create, getTypeData, name, qualifiedPath, toString, typeOrTypeAliasName)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


type QualifiedType
    = QualifiedType
        { qualifiedPath : List String
        , name : String
        , actualPath : List String
        }


create : ModuleNameLookupTable -> ModuleName -> Node ( ModuleName, String ) -> Maybe QualifiedType
create moduleNameLookupTable currentModule (Node range ( actualPath_, name_ )) =
    case ModuleNameLookupTable.moduleNameAt moduleNameLookupTable range of
        Just [] ->
            Just <| QualifiedType { qualifiedPath = currentModule, name = name_, actualPath = [] }

        Just moduleName ->
            Just <| QualifiedType { qualifiedPath = moduleName, name = name_, actualPath = actualPath_ }

        Nothing ->
            Nothing


qualifiedPath : QualifiedType -> List String
qualifiedPath (QualifiedType a) =
    a.qualifiedPath


name : QualifiedType -> String
name (QualifiedType a) =
    a.name


actualPath : QualifiedType -> List String
actualPath (QualifiedType a) =
    a.actualPath


toString : QualifiedType -> String
toString (QualifiedType a) =
    moduleNameToString a.actualPath a.name


moduleNameToString : ModuleName -> String -> String
moduleNameToString moduleName name_ =
    String.join "." (moduleName ++ [ name_ ])


getTypeData : List ( ModuleName, TypeOrTypeAlias ) -> QualifiedType -> Maybe ( ModuleName, TypeOrTypeAlias )
getTypeData types qualifiedType =
    List.filterMap
        (\( typeModule, type_ ) ->
            if typeModule == qualifiedPath qualifiedType then
                case type_ of
                    TypeValue typeValue ->
                        if Node.value typeValue.name == name qualifiedType then
                            Just ( typeModule, type_ )

                        else
                            Nothing

                    TypeAliasValue name_ _ ->
                        if name_ == name qualifiedType then
                            Just ( typeModule, type_ )

                        else
                            Nothing

            else
                Nothing
        )
        types
        |> List.head


type TypeOrTypeAlias
    = TypeValue Elm.Syntax.Type.Type
    | TypeAliasValue String (List (Node ( Node String, Node TypeAnnotation )))


typeOrTypeAliasName : TypeOrTypeAlias -> String
typeOrTypeAliasName typeOrTypeAlias =
    case typeOrTypeAlias of
        TypeValue type_ ->
            Node.value type_.name

        TypeAliasValue name_ _ ->
            name_
