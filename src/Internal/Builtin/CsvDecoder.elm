module Internal.Builtin.CsvDecoder exposing (..)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


csv_decode : String -> CG.Expression
csv_decode =
    CG.fqFun [ "Csv", "Decode" ]


codeGen : CodeGenerator
codeGen =
    CodeGenerator.defineWithComputation
        { id = "BrianHicks/elm-csv/Csv.Decode.Decoder"
        , dependency = "BrianHicks/elm-csv"
        , typePattern = Typed [ "Csv", "Decode" ] "Decoder" [ Target ]
        , makeName = Nothing
        , input = []
        }
        [ CodeGenerator.customTypeWithInput
            (\inp ctors ->
                List.map (\( ref, _ ) -> ref.name :: inp) ctors
            )
            (\path _ args ->
                CG.pipe
                    (CG.apply [ csv_decode "field", CG.string (makePath ("tag" :: path)), CG.apply [ csv_decode "string" ] ])
                    [ CG.apply [ csv_decode "andThen", CG.lambda [ CG.varPattern "ctor" ] (CG.caseExpr (CG.val "ctor") (List.map (\( name, expr ) -> ( CG.stringPattern name, expr )) args ++ [ ( CG.allPattern, CG.apply [ csv_decode "fail", CG.string "Unrecognized constructor" ] ) ])) ] ]
            )
        , CodeGenerator.combinerWithInput
            (\inp t refs ->
                case t of
                    ResolvedType.AnonymousRecord _ fields ->
                        List.map (\( f, _ ) -> f :: inp) fields

                    ResolvedType.TypeAlias _ _ (ResolvedType.AnonymousRecord _ fields) ->
                        List.map (\( f, _ ) -> f :: inp) fields

                    ResolvedType.Tuple args ->
                        List.indexedMap (\i _ -> String.fromInt (i + 1) :: inp) args

                    _ ->
                        List.map (always inp) refs
            )
            (\path t ctor exprs ->
                case t of
                    ResolvedType.AnonymousRecord _ fields ->
                        makeMap path
                            ctor
                            (List.map2
                                (\( name, fieldType ) expr ->
                                    ( name, fieldType, expr )
                                )
                                fields
                                exprs
                            )
                            |> Just

                    ResolvedType.TypeAlias _ _ (ResolvedType.AnonymousRecord _ fields) ->
                        makeMap path
                            ctor
                            (List.map2
                                (\( name, fieldType ) expr ->
                                    ( name, fieldType, expr )
                                )
                                fields
                                exprs
                            )
                            |> Just

                    ResolvedType.Opaque { name } args ->
                        case exprs of
                            [] ->
                                Just (CG.apply [ csv_decode "succeed", ctor ])

                            _ ->
                                makeMap path
                                    ctor
                                    (List.map3
                                        (\index fieldType expr ->
                                            ( String.fromInt index, fieldType, expr )
                                        )
                                        (List.range 0 (List.length args))
                                        args
                                        exprs
                                    )
                                    |> Just

                    ResolvedType.Tuple args ->
                        makeMap path
                            ctor
                            (List.map3
                                (\index fieldType expr ->
                                    ( String.fromInt index, fieldType, expr )
                                )
                                (List.range 0 (List.length args))
                                args
                                exprs
                            )
                            |> Just

                    _ ->
                        Nothing
            )
        ]


makePath : List String -> String
makePath =
    List.reverse >> String.join "_"


makeMap : List String -> CG.Expression -> List ( String, ResolvedType.ResolvedType, CG.Expression ) -> CG.Expression
makeMap path expr exprMap =
    let
        n =
            List.length exprMap

        exprs =
            List.map
                (\( name, t, expre ) ->
                    case t of
                        ResolvedType.Opaque _ _ ->
                            CG.apply [ csv_decode "field", CG.string (makePath (name :: path)), expre ]

                        ResolvedType.GenericType _ (Just (ResolvedType.Opaque _ _)) ->
                            CG.apply [ csv_decode "field", CG.string (makePath (name :: path)), expre ]

                        ResolvedType.CustomType ref _ (( _, [ ResolvedType.GenericType _ (Just (ResolvedType.Opaque _ _)) ] ) :: _) ->
                            if ref.modulePath == [ "Maybe" ] && ref.name == "Maybe" then
                                CG.apply [ csv_decode "field", CG.string (makePath (name :: path)), expre ]

                            else
                                expre

                        _ ->
                            expre
                )
                exprMap
    in
    if n == 1 then
        CG.apply
            ([ csv_decode "map"
             , expr
             ]
                ++ exprs
            )

    else if n <= 3 then
        CG.apply
            ([ csv_decode
                ("map" ++ String.fromInt (List.length exprs))
             , expr
             ]
                ++ exprs
            )

    else
        CG.pipe
            (CG.apply
                [ csv_decode "into"
                , expr
                ]
            )
            (List.map (\field -> CG.apply [ csv_decode "pipeline", field ]) exprs)
