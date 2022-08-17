module Internal.DependencyScanner exposing (findProviders)

import Dict exposing (Dict)
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Type as T exposing (Type)
import Internal.CodeGenerator exposing (ConfiguredCodeGenerator, ExistingFunctionProvider)
import List.Extra
import ResolvedType as RT exposing (ResolvedType)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Set
import TypePattern as TP exposing (TypePattern)


findProviders : List ConfiguredCodeGenerator -> Dict String Dependency -> List ExistingFunctionProvider
findProviders codeGens deps =
    Dict.values deps
        |> List.concatMap
            (\dep ->
                case Dependency.elmJson dep of
                    Elm.Project.Package package ->
                        let
                            qualifyingCodeGens =
                                List.filter (\provider -> (Elm.Package.toString package.name == provider.dependency) || List.member provider.dependency (List.map (Tuple.first >> Elm.Package.toString) package.deps)) codeGens
                        in
                        List.concatMap
                            (\mod ->
                                List.filterMap
                                    (\value ->
                                        case findMatchingPatternWithGenerics qualifyingCodeGens value.tipe of
                                            Just ( codeGenId, childType, assignments ) ->
                                                Just
                                                    { functionName = value.name
                                                    , childType = resolvedTypeFromDocsType mod.name childType
                                                    , codeGenId = codeGenId
                                                    , moduleName = String.split "." mod.name
                                                    , genericArguments = assignments
                                                    , privateTo = Nothing
                                                    }

                                            Nothing ->
                                                Nothing
                                    )
                                    mod.values
                                    |> heuristicRejectIfMultiplePatternsForSameType
                            )
                            (Dependency.modules dep)

                    Elm.Project.Application _ ->
                        []
            )


{-| This is an opinionated heuristic. Generally if a module provides more than one implementation for a type, than that's a good hint that there are multiple nonequivalent ways of doing it and it probably requires programmer discretion.
-}
heuristicRejectIfMultiplePatternsForSameType : List ExistingFunctionProvider -> List ExistingFunctionProvider
heuristicRejectIfMultiplePatternsForSameType providers =
    List.Extra.gatherEqualsBy .childType providers
        |> List.filterMap
            (\( provider, rest ) ->
                if List.isEmpty rest then
                    Just provider

                else
                    Nothing
            )


resolvedTypeFromDocsType : String -> Type -> ResolvedType
resolvedTypeFromDocsType currentModule tipe =
    case tipe of
        T.Var var ->
            RT.GenericType var Nothing

        T.Type name args ->
            RT.Opaque (parseRef currentModule name) (List.map (resolvedTypeFromDocsType currentModule) args)

        T.Tuple args ->
            RT.Tuple (List.map (resolvedTypeFromDocsType currentModule) args)

        T.Record def var ->
            RT.AnonymousRecord var (List.map (Tuple.mapSecond (resolvedTypeFromDocsType currentModule)) def)

        T.Lambda lv rv ->
            case resolvedTypeFromDocsType currentModule rv of
                RT.Function args ret ->
                    RT.Function (resolvedTypeFromDocsType currentModule lv :: args) ret

                ret ->
                    RT.Function [ resolvedTypeFromDocsType currentModule lv ] ret


parseRef : String -> String -> RT.Reference
parseRef mod str =
    case List.Extra.unconsLast (String.split "." str) of
        Just ( name, modPath ) ->
            { name = name, modulePath = modPath }

        Nothing ->
            { name = str, modulePath = String.split "." mod }


type MatchResult
    = Found Type
    | Matched
    | NotMatched


findMatchingPatternWithGenerics : List ConfiguredCodeGenerator -> Type -> Maybe ( String, Type, List String )
findMatchingPatternWithGenerics codeGenerators annotation =
    case codeGenerators of
        h :: t ->
            case typePatternMatches h.searchPattern annotation of
                Just childType ->
                    Just ( h.id, childType, [] )

                Nothing ->
                    case annotation of
                        T.Lambda inp out ->
                            case ( typePatternMatches h.searchPattern inp, findMatchingPatternWithGenerics [ h ] out ) of
                                ( Just (T.Var varName), Just ( _, childType, otherAssignments ) ) ->
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


containsGenericType : String -> Type -> Bool
containsGenericType varName t =
    case t of
        T.Var var ->
            varName == var

        T.Type _ args ->
            List.any (containsGenericType varName) args

        T.Tuple args ->
            List.any (containsGenericType varName) args

        T.Record def var ->
            Just varName == var || List.any (\( _, v ) -> containsGenericType varName v) def

        T.Lambda lv rv ->
            containsGenericType varName lv || containsGenericType varName rv


typePatternMatches : TypePattern -> Type -> Maybe Type
typePatternMatches typePattern tipe =
    let
        combineResults a b =
            case b of
                NotMatched ->
                    NotMatched

                Matched ->
                    a

                Found res ->
                    if a == NotMatched then
                        NotMatched

                    else
                        Found res

        formatName mods n =
            String.join "." (mods ++ [ n ])

        matchLists xs nys =
            if List.length xs /= List.length nys then
                NotMatched

            else
                List.map2 helper xs nys
                    |> List.foldr combineResults Matched

        helper tp ta =
            case ( tp, ta ) of
                ( TP.Target, _ ) ->
                    Found ta

                ( TP.Typed tpMod tpName tpArgs, T.Type tName tArgs ) ->
                    if formatName tpMod tpName == tName then
                        matchLists tpArgs tArgs

                    else
                        NotMatched

                ( TP.Function tpArg tpRes, T.Lambda taArg taRes ) ->
                    combineResults (helper tpArg taArg) (helper tpRes taRes)

                ( TP.GenericType _, T.Var _ ) ->
                    Matched

                ( TP.Tuple tpTs, T.Tuple taTs ) ->
                    matchLists tpTs taTs

                ( TP.Record tpDef, T.Record taDef Nothing ) ->
                    if List.all identity (List.map2 (\( tpName, _ ) ( taName, _ ) -> tpName == taName) tpDef taDef) then
                        matchLists (List.map Tuple.second tpDef) (List.map Tuple.second taDef)

                    else
                        NotMatched

                _ ->
                    NotMatched
    in
    case helper typePattern tipe of
        Found ta ->
            Just ta

        _ ->
            Nothing
