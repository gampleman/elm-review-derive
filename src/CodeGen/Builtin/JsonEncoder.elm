module CodeGen.Builtin.JsonEncoder exposing (generic)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation as TA exposing (TypeAnnotation)
import ResolvedType
import TypePattern exposing (TypePattern(..))


lambdaApply : String -> CG.Expression -> CG.Expression
lambdaApply name expr =
    CG.lambda [ CG.varPattern name ] (CG.apply [ expr, CG.val name ])


generic : CodeGenerator
generic =
    CodeGenerator.define "elm/json/Json.Encode.Value"
        "elm/json"
        (Function Target (Typed [ "Json", "Encode" ] "Value" []))
        (\name -> "encode" ++ name)
        [ CodeGenerator.int (CG.fqFun [ "Json", "Encode" ] "int")
        , CodeGenerator.string (CG.fqFun [ "Json", "Encode" ] "string")
        , CodeGenerator.float (CG.fqFun [ "Json", "Encode" ] "float")
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
            (\t fn exprs ->
                case t of
                    ResolvedType.AnonymousRecord _ fields ->
                        CG.lambda [ CG.varPattern "rec" ]
                            (CG.apply
                                [ CG.fqFun [ "Json", "Encode" ] "object", CG.list (List.map2 (\( field, _ ) expr -> CG.tuple [ CG.string field, CG.apply [ expr, CG.access (CG.val "rec") field ] ]) fields exprs) ]
                            )
                            |> Just

                    ResolvedType.Opaque ref args ->
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
