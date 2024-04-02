module Internal.Builtin.JsonDecoder exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import ResolvedType
import TypePattern exposing (TypePattern(..))


json_decode : String -> CG.Expression
json_decode =
    CG.fqFun [ "Json", "Decode" ]


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define
        { id = "elm/json/Json.Decode.Decoder"
        , dependency = "elm/json"
        , typePattern = Typed [ "Json", "Decode" ] "Decoder" [ Target ]
        , makeName = \name -> "decode" ++ name
        }
        [ CodeGenerator.customType
            (\_ args ->
                CG.pipe
                    (CG.apply [ json_decode "field", CG.string "tag", CG.apply [ json_decode "string" ] ])
                    [ CG.apply [ json_decode "andThen", CG.lambda [ CG.varPattern "ctor" ] (CG.caseExpr (CG.val "ctor") (List.map (\( name, expr ) -> ( CG.stringPattern name, expr )) args ++ [ ( CG.allPattern, CG.apply [ json_decode "fail", CG.string "Unrecognized constructor" ] ) ])) ] ]
            )
        , CodeGenerator.map (\comb arg -> CG.apply [ json_decode "map", comb, arg ])
        , CodeGenerator.mapN 8 (\name comb args -> CG.apply ([ json_decode name, comb ] ++ args))
        , CodeGenerator.combiner
            (\t ctor exprs ->
                case t of
                    ResolvedType.AnonymousRecord _ fields ->
                        makeMap ctor (List.map2 (\( name, _ ) expr -> CG.apply [ json_decode "field", CG.string name, expr ]) fields exprs)
                            |> Just

                    ResolvedType.TypeAlias _ _ (ResolvedType.AnonymousRecord _ fields) ->
                        makeMap ctor (List.map2 (\( name, _ ) expr -> CG.apply [ json_decode "field", CG.string name, expr ]) fields exprs)
                            |> Just

                    ResolvedType.Opaque _ _ ->
                        case exprs of
                            [] ->
                                Just (CG.apply [ json_decode "succeed", ctor ])

                            _ ->
                                makeMap ctor (List.indexedMap (\i expr -> CG.apply [ json_decode "field", CG.string (String.fromInt i), expr ]) exprs)
                                    |> Just

                    ResolvedType.Tuple _ ->
                        makeMap ctor (List.indexedMap (\i expr -> CG.apply [ json_decode "index", CG.int i, expr ]) exprs)
                            |> Just

                    _ ->
                        Nothing
            )
        , CodeGenerator.use "Json.Decode.maybe"

        -- , CodeGenerator.maybe (\expr -> CG.apply [ json_decode "maybe", expr ])
        ]


makeMap expr exprs =
    let
        n =
            List.length exprs
    in
    if n == 1 then
        CG.apply
            ([ json_decode "map"
             , expr
             ]
                ++ exprs
            )

    else if n <= 8 then
        CG.apply
            ([ json_decode
                ("map" ++ String.fromInt (List.length exprs))
             , expr
             ]
                ++ exprs
            )

    else
        CG.pipe
            (CG.apply
                [ json_decode "succeed"
                , expr
                ]
            )
            (List.map (\field -> CG.apply [ json_decode "map2", CG.parens (CG.binOp CG.piper), field ]) exprs)
