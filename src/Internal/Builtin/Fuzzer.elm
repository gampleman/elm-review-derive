module Internal.Builtin.Fuzzer exposing (codeGen)

import CodeGenerator exposing (CodeGenerator)
import Elm.CodeGen as CG
import Internal.Helpers exposing (toValueCase)
import ResolvedType
import TypePattern exposing (TypePattern(..))


fuzz : String -> CG.Expression
fuzz =
    CG.fqFun [ "Fuzz" ]


codeGen : CodeGenerator
codeGen =
    CodeGenerator.define "elm-explorations/test/Fuzz.Fuzzer"
        "elm-explorations/test"
        (Typed [ "Fuzz" ] "Fuzzer" [ Target ])
        (\name -> toValueCase name ++ "Fuzzer")
        [ CodeGenerator.customType (\_ exps -> CG.apply [ fuzz "oneOf", CG.list (List.map Tuple.second exps) ])
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
