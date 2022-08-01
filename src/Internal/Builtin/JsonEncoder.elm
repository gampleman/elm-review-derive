module Internal.Builtin.JsonEncoder exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define "elm/json/Json.Encode.Value"
        "elm/json"
        (Function Target (Typed [ "Json", "Encode" ] "Value" []))
        (\name -> "encode" ++ name)
        [ CodeGenerator.int (CG.fqFun [ "Json", "Encode" ] "int")
        , CodeGenerator.string (CG.fqFun [ "Json", "Encode" ] "string")
        , CodeGenerator.float (CG.fqFun [ "Json", "Encode" ] "float")
        , CodeGenerator.bool (CG.fqFun [ "Json", "Encode" ] "bool")
        , CodeGenerator.list (\arg -> CG.apply [ CG.fqFun [ "Json", "Encode" ] "list", arg ])
        , CodeGenerator.array (\arg -> CG.apply [ CG.fqFun [ "Json", "Encode" ] "array", arg ])
        , CodeGenerator.set (\arg -> CG.apply [ CG.fqFun [ "Json", "Encode" ] "set", arg ])
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
                CG.lambda [ CG.varPattern "arg" ]
                    (CG.caseExpr (CG.val "arg")
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
