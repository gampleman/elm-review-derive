module Internal.Builtin.Fuzzer exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node exposing (Node(..))
import Maybe.Extra
import String.Extra
import TypePattern exposing (TypePattern(..))


fuzz : String -> CG.Expression
fuzz =
    CG.fqFun [ "Fuzz" ]


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define
        { id = "elm-explorations/test/Fuzz.Fuzzer"
        , dependency = "elm-explorations/test"
        , typePattern = Typed [ "Fuzz" ] "Fuzzer" [ Target ]
        , makeName = \name -> String.Extra.decapitalize name ++ "Fuzzer"
        }
        [ CodeGenerator.customType
            (\_ exps ->
                case Maybe.Extra.combineMap extractFromConstant exps of
                    Just constants ->
                        CG.apply [ fuzz "oneOfValues", CG.list constants ]

                    Nothing ->
                        CG.apply [ fuzz "oneOf", CG.list (List.map Tuple.second exps) ]
            )
        , CodeGenerator.pipeline (\c -> CG.apply [ fuzz "constant", c ]) (\m -> CG.apply [ fuzz "andMap", m ])
        , CodeGenerator.mapN 8 (\name a bs -> CG.apply (fuzz name :: a :: bs))
        , CodeGenerator.map (\a b -> CG.apply [ fuzz "map", a, b ])
        , CodeGenerator.string (fuzz "string")
        , CodeGenerator.dict (\key val -> CG.apply [ fuzz "map", CG.fqFun [ "Dict" ] "fromList", CG.apply [ fuzz "list", CG.apply [ fuzz "pair", key, val ] ] ])
        , CodeGenerator.float (fuzz "niceFloat")
        , CodeGenerator.tuple (\a b -> CG.apply [ fuzz "pair", a, b ])
        , CodeGenerator.triple (\a b c -> CG.apply [ fuzz "triple", a, b, c ])
        , CodeGenerator.char (fuzz "char")
        , CodeGenerator.lambdaBreaker (\inner -> CG.apply [ fuzz "lazy", CG.lambda [ CG.unitPattern ] inner ])
        ]


extractFromConstant : ( String, CG.Expression ) -> Maybe CG.Expression
extractFromConstant ( _, expr ) =
    case expr of
        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Fuzz" ] "constant"), Node _ inner ] ->
            Just inner

        _ ->
            Nothing
