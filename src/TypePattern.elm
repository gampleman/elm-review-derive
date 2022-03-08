module TypePattern exposing (TypePattern(..), generate, matches)

import Elm.CodeGen as CG
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TA exposing (TypeAnnotation)



-- TODO: Figure out how to handle generics. In some cases we may want to have them behave like wildcards, i.e. say for the  error type in a serializer


type TypePattern
    = Target
    | Typed (List String) String (List TypePattern)
    | Function TypePattern TypePattern
    | GenericType String
    | Tuple (List TypePattern)
    | Record (List ( String, TypePattern ))


type MatchResult
    = Found TypeAnnotation
    | Matched
    | NotMatched


matches : TypePattern -> TypeAnnotation -> Maybe TypeAnnotation
matches typePattern typeAnnotation =
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

        matchLists xs nys =
            if List.length xs /= List.length nys then
                NotMatched

            else
                List.map2 (\x (Node _ y) -> helper x y) xs nys
                    |> List.foldr combineResults Matched

        helper tp ta =
            case ( tp, ta ) of
                ( Target, _ ) ->
                    Found ta

                ( Typed tpMod tpName tpArgs, TA.Typed (Node _ ( taMod, taName )) taArgs ) ->
                    if tpMod == taMod && tpName == taName then
                        matchLists tpArgs taArgs

                    else
                        NotMatched

                ( Function tpArg tpRes, TA.FunctionTypeAnnotation (Node _ taArg) (Node _ taRes) ) ->
                    combineResults (helper tpArg taArg) (helper tpRes taRes)

                ( GenericType _, TA.GenericType _ ) ->
                    Matched

                ( Tuple tpTs, TA.Tupled taTs ) ->
                    matchLists tpTs taTs

                ( Tuple [], TA.Unit ) ->
                    Matched

                ( Record tpDef, TA.Record taDef ) ->
                    if List.all identity (List.map2 (\( tpName, _ ) (Node _ ( Node _ taName, _ )) -> tpName == taName) tpDef taDef) then
                        matchLists (List.map Tuple.second tpDef) (List.map (Node.value >> Tuple.second) taDef)

                    else
                        NotMatched

                _ ->
                    NotMatched
    in
    case helper typePattern typeAnnotation of
        Found ta ->
            Just ta

        _ ->
            Nothing


generate : TypePattern -> TypeAnnotation -> TypeAnnotation
generate typePattern childType =
    case typePattern of
        Target ->
            childType

        Typed mod name args ->
            CG.fqTyped mod name (List.map (\arg -> generate arg childType) args)

        Function arg res ->
            CG.funAnn (generate arg childType) (generate res childType)

        GenericType name ->
            TA.GenericType name

        Tuple [] ->
            CG.unitAnn

        Tuple children ->
            CG.tupleAnn (List.map (\arg -> generate arg childType) children)

        Record args ->
            CG.recordAnn (List.map (Tuple.mapSecond (\arg -> generate arg childType)) args)
