module Internal.Builtin.JsonEncoder exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


lambdaWrap : String -> (CG.Expression -> CG.Expression) -> CG.Expression
lambdaWrap name fn =
    CG.lambda [ CG.varPattern name ] (fn (CG.val name))


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define "elm/json/Json.Encode.Value"
        "elm/json"
        (Function Target (Typed [ "Json", "Encode" ] "Value" []))
        (\name -> "encode" ++ name)
        [ CodeGenerator.char (lambdaWrap "char" (\char -> CG.apply [ CG.fqFun [ "Json", "Encode" ] "string", CG.apply [ CG.fqFun [ "String" ] "fromChar", char ] ]))
        , CodeGenerator.customDict
            (\( keyType, keyEncoder ) ( _, valEncoder ) ->
                CG.apply
                    [ CG.fqFun [ "Json", "Encode" ] "dict"
                    , case keyType of
                        ResolvedType.Opaque ref _ ->
                            if ref.modulePath == [ "String" ] && ref.name == "String" then
                                CG.val "identity"

                            else
                                -- TODO: For int and float, we could probably more idiomatically do String.fromInt etc
                                CG.chain keyEncoder [ CG.apply [ CG.fqFun [ "Json", "Encode" ] "encode", CG.int 0 ] ]

                        _ ->
                            CG.chain keyEncoder [ CG.apply [ CG.fqFun [ "Json", "Encode" ] "encode", CG.int 0 ] ]
                    , valEncoder
                    ]
            )
        , CodeGenerator.maybe
            (\encoder ->
                lambdaWrap "arg"
                    (\arg ->
                        CG.caseExpr arg
                            [ ( CG.fqNamedPattern [] "Just" [ CG.varPattern "val" ]
                              , CG.apply [ encoder, CG.val "val" ]
                              )
                            , ( CG.fqNamedPattern [] "Nothing" []
                              , CG.fqVal [ "Json", "Encode" ] "null"
                              )
                            ]
                    )
            )
        , CodeGenerator.customType
            (\ctors variants ->
                CG.lambda [ CG.varPattern "arg" ]
                    (List.map2
                        (\( ref, args ) ( _, expr ) ->
                            ( CG.fqNamedPattern ref.modulePath ref.name (thingsToPatterns args)
                            , case args of
                                [] ->
                                    CG.apply [ CG.fqFun [ "Json", "Encode" ] "string", CG.string ref.name ]

                                _ ->
                                    CG.apply (expr :: thingsToValues args)
                            )
                        )
                        ctors
                        variants
                        |> CG.caseExpr (CG.val "arg")
                    )
            )
        , CodeGenerator.combiner
            (\t _ exprs ->
                case t of
                    ResolvedType.AnonymousRecord _ fields ->
                        CG.lambda [ CG.varPattern "rec" ]
                            (CG.apply
                                [ CG.fqFun [ "Json", "Encode" ] "object", CG.list (List.map2 (\( field, _ ) expr -> CG.tuple [ CG.string field, CG.apply [ expr, CG.access (CG.val "rec") field ] ]) fields exprs) ]
                            )
                            |> Just

                    ResolvedType.Opaque ref _ ->
                        case exprs of
                            [] ->
                                Just (CG.apply [ CG.fqFun [ "Json", "Encode" ] "string", CG.string ref.name ])

                            --[ expr ] ->
                            -- Just (CG.lambda [ CG.fqNamedPattern ref.modulePath ref.name (thingsToPatterns args) ] (CG.apply (expr :: thingsToValues args)))
                            _ ->
                                CG.lambda (thingsToPatterns exprs)
                                    (CG.apply
                                        [ CG.fqFun [ "Json", "Encode" ] "object"
                                        , CG.list
                                            (CG.tuple [ CG.string "tag", CG.apply [ CG.fqFun [ "Json", "Encode" ] "string", CG.string ref.name ] ]
                                                :: List.indexedMap (\i expr -> CG.tuple [ CG.string (String.fromInt i), CG.apply [ expr, CG.val ("arg" ++ String.fromInt i) ] ]) exprs
                                            )
                                        ]
                                    )
                                    |> Just

                    ResolvedType.TypeAlias _ _ (ResolvedType.AnonymousRecord _ fields) ->
                        CG.lambda [ CG.varPattern "rec" ]
                            (CG.apply
                                [ CG.fqFun [ "Json", "Encode" ] "object", CG.list (List.map2 (\( field, _ ) expr -> CG.tuple [ CG.string field, CG.apply [ expr, CG.access (CG.val "rec") field ] ]) fields exprs) ]
                            )
                            |> Just

                    ResolvedType.TypeAlias _ _ _ ->
                        List.head exprs

                    ResolvedType.Tuple _ ->
                        case exprs of
                            [ fst, snd ] ->
                                lambdaWrap "tuple"
                                    (\tuple ->
                                        CG.apply
                                            [ CG.fqFun [ "Json", "Encode" ] "list"
                                            , CG.val "identity"
                                            , CG.list
                                                [ CG.apply [ fst, CG.apply [ CG.fqFun [ "Tuple" ] "first", tuple ] ]
                                                , CG.apply [ snd, CG.apply [ CG.fqFun [ "Tuple" ] "second", tuple ] ]
                                                ]
                                            ]
                                    )
                                    |> Just

                            [ fst, snd, thrd ] ->
                                lambdaWrap "triple"
                                    (\triple ->
                                        CG.letExpr [ CG.letDestructuring (CG.tuplePattern [ CG.varPattern "a", CG.varPattern "b", CG.varPattern "c" ]) triple ]
                                            (CG.apply
                                                [ CG.fqFun [ "Json", "Encode" ] "list"
                                                , CG.val "identity"
                                                , CG.list
                                                    [ CG.apply [ fst, CG.val "a" ]
                                                    , CG.apply [ snd, CG.val "b" ]
                                                    , CG.apply [ thrd, CG.val "c" ]
                                                    ]
                                                ]
                                            )
                                    )
                                    |> Just

                            _ ->
                                Nothing

                    -- ResolvedType.CustomType _ _ [ ( ref, [] ) ] ->
                    --     Just (CG.apply [ CG.fqFun [ "Json", "Encode" ] "string", CG.string ref.name ])
                    _ ->
                        Nothing
            )
        ]


thingsToPatterns : List a -> List CG.Pattern
thingsToPatterns =
    List.indexedMap (\i _ -> CG.varPattern ("arg" ++ String.fromInt i))


thingsToValues : List a -> List CG.Expression
thingsToValues =
    List.indexedMap (\i _ -> CG.val ("arg" ++ String.fromInt i))
