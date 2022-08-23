module RandomCodeGenTest exposing (..)

import CodeGenerator.Test exposing (FakeDependency, codeGenTest, codeGenTestFailsWith)
import Review.Project exposing (dependencies)
import Test exposing (Test, describe)


elmRandom : FakeDependency
elmRandom =
    CodeGenerator.Test.fakeDependency
        { name = "elm/random"
        , dependencies = []
        , modules =
            [ { name = "Random"
              , values =
                    [ ( "andThen", "(a -> Random.Generator b) -> Random.Generator a -> Random.Generator b" )
                    , ( "constant", "a -> Random.Generator a" )
                    , ( "float", "Basics.Float -> Basics.Float -> Random.Generator Basics.Float" )
                    , ( "generate", "(a -> msg) -> Random.Generator a -> Platform.Cmd.Cmd msg" )
                    , ( "independentSeed", "Random.Generator Random.Seed" )
                    , ( "initialSeed", "Basics.Int -> Random.Seed" )
                    , ( "int", "Basics.Int -> Basics.Int -> Random.Generator Basics.Int" )
                    , ( "lazy", "(() -> Random.Generator a) -> Random.Generator a" )
                    , ( "list", "Basics.Int -> Random.Generator a -> Random.Generator (List.List a)" )
                    , ( "map", "(a -> b) -> Random.Generator a -> Random.Generator b" )
                    , ( "map2", "(a -> b -> c) -> Random.Generator a -> Random.Generator b -> Random.Generator c" )
                    , ( "map3", "(a -> b -> c -> d) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d" )
                    , ( "map4", "(a -> b -> c -> d -> e) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d -> Random.Generator e" )
                    , ( "map5", "(a -> b -> c -> d -> e -> f) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d -> Random.Generator e -> Random.Generator f" )
                    , ( "maxInt", "Basics.Int" )
                    , ( "minInt", "Basics.Int" )
                    , ( "pair", "Random.Generator a -> Random.Generator b -> Random.Generator ( a, b )" )
                    , ( "step", "Random.Generator a -> Random.Seed -> ( a, Random.Seed )" )
                    , ( "uniform", "a -> List.List a -> Random.Generator a" )
                    , ( "weighted", "( Basics.Float, a ) -> List.List ( Basics.Float, a ) -> Random.Generator a" )
                    ]
              }
            ]
        }


randomExtra : FakeDependency
randomExtra =
    CodeGenerator.Test.fakeDependency
        { name = "elm-community/random-extra"
        , dependencies = [ "elm/random" ]
        , modules =
            [ { name = "Random.Array"
              , values =
                    [ ( "array", "Basics.Int -> Random.Generator a -> Random.Generator (Array.Array a)" )
                    , ( "choose", "Array.Array a -> Random.Generator ( Maybe.Maybe a, Array.Array a )" )
                    , ( "rangeLengthArray", "Basics.Int -> Basics.Int -> Random.Generator a -> Random.Generator (Array.Array a)" )
                    , ( "sample", "Array.Array a -> Random.Generator (Maybe.Maybe a)" )
                    , ( "shuffle", "Array.Array a -> Random.Generator (Array.Array a)" )
                    ]
              }
            , { name = "Random.Char"
              , values =
                    [ ( "aegeanNumber", "Random.Generator Char.Char" )
                    , ( "alchemicalSymbol", "Random.Generator Char.Char" )
                    , ( "alphabeticPresentationForm", "Random.Generator Char.Char" )
                    , ( "ancientGreekMusicalNotationSymbol", "Random.Generator Char.Char" )
                    , ( "ancientGreekNumber", "Random.Generator Char.Char" )
                    , ( "ancientSymbol", "Random.Generator Char.Char" )
                    , ( "arabic", "Random.Generator Char.Char" )
                    , ( "arabicExtendedA", "Random.Generator Char.Char" )
                    , ( "arabicMathematicalAlphabeticSymbol", "Random.Generator Char.Char" )
                    , ( "arabicPresentationFormA", "Random.Generator Char.Char" )
                    , ( "arabicPresentationFormB", "Random.Generator Char.Char" )
                    , ( "arabicSupplement", "Random.Generator Char.Char" )
                    , ( "armenian", "Random.Generator Char.Char" )
                    , ( "arrow", "Random.Generator Char.Char" )
                    , ( "ascii", "Random.Generator Char.Char" )
                    , ( "avestan", "Random.Generator Char.Char" )
                    , ( "balinese", "Random.Generator Char.Char" )
                    , ( "bamum", "Random.Generator Char.Char" )
                    , ( "bamumSupplement", "Random.Generator Char.Char" )
                    , ( "basicLatin", "Random.Generator Char.Char" )
                    , ( "batak", "Random.Generator Char.Char" )
                    , ( "bengali", "Random.Generator Char.Char" )
                    , ( "blockElement", "Random.Generator Char.Char" )
                    , ( "bopomofo", "Random.Generator Char.Char" )
                    , ( "bopomofoExtended", "Random.Generator Char.Char" )
                    , ( "boxDrawing", "Random.Generator Char.Char" )
                    , ( "brahmi", "Random.Generator Char.Char" )
                    , ( "braillePattern", "Random.Generator Char.Char" )
                    , ( "buginese", "Random.Generator Char.Char" )
                    , ( "buhid", "Random.Generator Char.Char" )
                    , ( "byzantineMusicalSymbol", "Random.Generator Char.Char" )
                    , ( "carian", "Random.Generator Char.Char" )
                    , ( "chakma", "Random.Generator Char.Char" )
                    , ( "cham", "Random.Generator Char.Char" )
                    , ( "char", "Basics.Int -> Basics.Int -> Random.Generator Char.Char" )
                    , ( "cherokee", "Random.Generator Char.Char" )
                    , ( "cjkCompatibility", "Random.Generator Char.Char" )
                    , ( "cjkCompatibilityForm", "Random.Generator Char.Char" )
                    , ( "cjkCompatibilityIdeograph", "Random.Generator Char.Char" )
                    , ( "cjkCompatibilityIdeographSupplement", "Random.Generator Char.Char" )
                    , ( "cjkRadicalSupplement", "Random.Generator Char.Char" )
                    , ( "cjkStroke", "Random.Generator Char.Char" )
                    , ( "cjkSymbolOrPunctuation", "Random.Generator Char.Char" )
                    , ( "cjkUnifiedIdeograph", "Random.Generator Char.Char" )
                    , ( "cjkUnifiedIdeographExtensionA", "Random.Generator Char.Char" )
                    , ( "cjkUnifiedIdeographExtensionB", "Random.Generator Char.Char" )
                    , ( "cjkUnifiedIdeographExtensionC", "Random.Generator Char.Char" )
                    , ( "cjkUnifiedIdeographExtensionD", "Random.Generator Char.Char" )
                    , ( "combiningDiacriticalMarks", "Random.Generator Char.Char" )
                    , ( "combiningDiacriticalMarksForSymbols", "Random.Generator Char.Char" )
                    , ( "combiningDiacriticalMarksSupplement", "Random.Generator Char.Char" )
                    , ( "combiningHalfMark", "Random.Generator Char.Char" )
                    , ( "commonIndicNumberForm", "Random.Generator Char.Char" )
                    , ( "controlPicture", "Random.Generator Char.Char" )
                    , ( "coptic", "Random.Generator Char.Char" )
                    , ( "countingRodNumeral", "Random.Generator Char.Char" )
                    , ( "cuneiform", "Random.Generator Char.Char" )
                    , ( "cuneiformNumberOrPunctuation", "Random.Generator Char.Char" )
                    , ( "currencySymbol", "Random.Generator Char.Char" )
                    , ( "cypriotSyllable", "Random.Generator Char.Char" )
                    , ( "cyrillic", "Random.Generator Char.Char" )
                    , ( "cyrillicExtendedA", "Random.Generator Char.Char" )
                    , ( "cyrillicExtendedB", "Random.Generator Char.Char" )
                    , ( "cyrillicSupplement", "Random.Generator Char.Char" )
                    , ( "deseret", "Random.Generator Char.Char" )
                    , ( "devanagari", "Random.Generator Char.Char" )
                    , ( "devanagariExtended", "Random.Generator Char.Char" )
                    , ( "dingbat", "Random.Generator Char.Char" )
                    , ( "dominoTile", "Random.Generator Char.Char" )
                    , ( "egyptianHieroglyph", "Random.Generator Char.Char" )
                    , ( "emoticon", "Random.Generator Char.Char" )
                    , ( "enclosedAlphanumeric", "Random.Generator Char.Char" )
                    , ( "enclosedAlphanumericSupplement", "Random.Generator Char.Char" )
                    , ( "enclosedCJKLetterOrMonth", "Random.Generator Char.Char" )
                    , ( "enclosedIdeographicSupplement", "Random.Generator Char.Char" )
                    , ( "english", "Random.Generator Char.Char" )
                    , ( "ethiopic", "Random.Generator Char.Char" )
                    , ( "ethiopicExtended", "Random.Generator Char.Char" )
                    , ( "ethiopicExtendedA", "Random.Generator Char.Char" )
                    , ( "ethiopicSupplement", "Random.Generator Char.Char" )
                    , ( "generalPunctuation", "Random.Generator Char.Char" )
                    , ( "geometricShape", "Random.Generator Char.Char" )
                    , ( "georgian", "Random.Generator Char.Char" )
                    , ( "georgianSupplement", "Random.Generator Char.Char" )
                    , ( "glagolitic", "Random.Generator Char.Char" )
                    , ( "gothic", "Random.Generator Char.Char" )
                    , ( "greekAndCoptic", "Random.Generator Char.Char" )
                    , ( "greekExtended", "Random.Generator Char.Char" )
                    , ( "gujarati", "Random.Generator Char.Char" )
                    , ( "gurmukhi", "Random.Generator Char.Char" )
                    , ( "halfwidthOrFullwidthForm", "Random.Generator Char.Char" )
                    , ( "hangulCompatibilityJamo", "Random.Generator Char.Char" )
                    , ( "hangulJamo", "Random.Generator Char.Char" )
                    , ( "hangulJamoExtendedA", "Random.Generator Char.Char" )
                    , ( "hangulJamoExtendedB", "Random.Generator Char.Char" )
                    , ( "hangulSyllable", "Random.Generator Char.Char" )
                    , ( "hanunoo", "Random.Generator Char.Char" )
                    , ( "hebrew", "Random.Generator Char.Char" )
                    , ( "highPrivateUseSurrogate", "Random.Generator Char.Char" )
                    , ( "highSurrogate", "Random.Generator Char.Char" )
                    , ( "hiragana", "Random.Generator Char.Char" )
                    , ( "ideographicDescription", "Random.Generator Char.Char" )
                    , ( "imperialAramaic", "Random.Generator Char.Char" )
                    , ( "inscriptionalPahlavi", "Random.Generator Char.Char" )
                    , ( "inscriptionalParthian", "Random.Generator Char.Char" )
                    , ( "ipaExtensions", "Random.Generator Char.Char" )
                    , ( "javanese", "Random.Generator Char.Char" )
                    , ( "kaithi", "Random.Generator Char.Char" )
                    , ( "kanaSupplement", "Random.Generator Char.Char" )
                    , ( "kanbun", "Random.Generator Char.Char" )
                    , ( "kangxiRadical", "Random.Generator Char.Char" )
                    , ( "kannada", "Random.Generator Char.Char" )
                    , ( "katakana", "Random.Generator Char.Char" )
                    , ( "katakanaPhoneticExtension", "Random.Generator Char.Char" )
                    , ( "kayahLi", "Random.Generator Char.Char" )
                    , ( "kharoshthi", "Random.Generator Char.Char" )
                    , ( "khmer", "Random.Generator Char.Char" )
                    , ( "khmerSymbol", "Random.Generator Char.Char" )
                    , ( "lao", "Random.Generator Char.Char" )
                    , ( "latin", "Random.Generator Char.Char" )
                    , ( "latin1Supplement", "Random.Generator Char.Char" )
                    , ( "latinExtendedA", "Random.Generator Char.Char" )
                    , ( "latinExtendedAdditional", "Random.Generator Char.Char" )
                    , ( "latinExtendedB", "Random.Generator Char.Char" )
                    , ( "latinExtendedC", "Random.Generator Char.Char" )
                    , ( "latinExtendedD", "Random.Generator Char.Char" )
                    , ( "lepcha", "Random.Generator Char.Char" )
                    , ( "letterlikeSymbol", "Random.Generator Char.Char" )
                    , ( "limbu", "Random.Generator Char.Char" )
                    , ( "linearBIdeogram", "Random.Generator Char.Char" )
                    , ( "linearBSyllable", "Random.Generator Char.Char" )
                    , ( "lisu", "Random.Generator Char.Char" )
                    , ( "lowSurrogate", "Random.Generator Char.Char" )
                    , ( "lowerCaseLatin", "Random.Generator Char.Char" )
                    , ( "lycian", "Random.Generator Char.Char" )
                    , ( "lydian", "Random.Generator Char.Char" )
                    , ( "mahjongTile", "Random.Generator Char.Char" )
                    , ( "malayalam", "Random.Generator Char.Char" )
                    , ( "mandaic", "Random.Generator Char.Char" )
                    , ( "mathematicalAlphanumericSymbol", "Random.Generator Char.Char" )
                    , ( "mathematicalOperator", "Random.Generator Char.Char" )
                    , ( "meeteiMayek", "Random.Generator Char.Char" )
                    , ( "meeteiMayekExtension", "Random.Generator Char.Char" )
                    , ( "meroiticCursive", "Random.Generator Char.Char" )
                    , ( "meroiticHieroglyph", "Random.Generator Char.Char" )
                    , ( "miao", "Random.Generator Char.Char" )
                    , ( "miscellaneousMathematicalSymbolA", "Random.Generator Char.Char" )
                    , ( "miscellaneousMathematicalSymbolB", "Random.Generator Char.Char" )
                    , ( "miscellaneousSymbol", "Random.Generator Char.Char" )
                    , ( "miscellaneousSymbolOrArrow", "Random.Generator Char.Char" )
                    , ( "miscellaneousSymbolOrPictograph", "Random.Generator Char.Char" )
                    , ( "miscellaneousTechnical", "Random.Generator Char.Char" )
                    , ( "modifierToneLetter", "Random.Generator Char.Char" )
                    , ( "mongolian", "Random.Generator Char.Char" )
                    , ( "musicalSymbol", "Random.Generator Char.Char" )
                    , ( "myanmar", "Random.Generator Char.Char" )
                    , ( "myanmarExtendedA", "Random.Generator Char.Char" )
                    , ( "newTaiLue", "Random.Generator Char.Char" )
                    , ( "nko", "Random.Generator Char.Char" )
                    , ( "numberForm", "Random.Generator Char.Char" )
                    , ( "ogham", "Random.Generator Char.Char" )
                    , ( "olChiki", "Random.Generator Char.Char" )
                    , ( "oldItalic", "Random.Generator Char.Char" )
                    , ( "oldPersian", "Random.Generator Char.Char" )
                    , ( "oldSouthArabian", "Random.Generator Char.Char" )
                    , ( "oldTurkic", "Random.Generator Char.Char" )
                    , ( "opticalCharacterRecognition", "Random.Generator Char.Char" )
                    , ( "oriya", "Random.Generator Char.Char" )
                    , ( "osmanya", "Random.Generator Char.Char" )
                    , ( "phagsPa", "Random.Generator Char.Char" )
                    , ( "phaistosDisc", "Random.Generator Char.Char" )
                    , ( "phoenician", "Random.Generator Char.Char" )
                    , ( "phoneticExtensions", "Random.Generator Char.Char" )
                    , ( "phoneticExtensionsSupplement", "Random.Generator Char.Char" )
                    , ( "playingCard", "Random.Generator Char.Char" )
                    , ( "privateUseArea", "Random.Generator Char.Char" )
                    , ( "rejang", "Random.Generator Char.Char" )
                    , ( "rumiNumericalSymbol", "Random.Generator Char.Char" )
                    , ( "runic", "Random.Generator Char.Char" )
                    , ( "samaritan", "Random.Generator Char.Char" )
                    , ( "saurashtra", "Random.Generator Char.Char" )
                    , ( "sharada", "Random.Generator Char.Char" )
                    , ( "shavian", "Random.Generator Char.Char" )
                    , ( "sinhala", "Random.Generator Char.Char" )
                    , ( "smallFormVariant", "Random.Generator Char.Char" )
                    , ( "soraSompeng", "Random.Generator Char.Char" )
                    , ( "spacingModifier", "Random.Generator Char.Char" )
                    , ( "special", "Random.Generator Char.Char" )
                    , ( "sundanese", "Random.Generator Char.Char" )
                    , ( "sundaneseSupplement", "Random.Generator Char.Char" )
                    , ( "superscriptOrSubscript", "Random.Generator Char.Char" )
                    , ( "supplementalArrowA", "Random.Generator Char.Char" )
                    , ( "supplementalArrowB", "Random.Generator Char.Char" )
                    , ( "supplementalMathematicalOperator", "Random.Generator Char.Char" )
                    , ( "supplementalPunctuation", "Random.Generator Char.Char" )
                    , ( "supplementaryPrivateUseAreaA", "Random.Generator Char.Char" )
                    , ( "supplementaryPrivateUseAreaB", "Random.Generator Char.Char" )
                    , ( "sylotiNagri", "Random.Generator Char.Char" )
                    , ( "syriac", "Random.Generator Char.Char" )
                    , ( "tag", "Random.Generator Char.Char" )
                    , ( "tagalog", "Random.Generator Char.Char" )
                    , ( "tagbanwa", "Random.Generator Char.Char" )
                    , ( "taiLe", "Random.Generator Char.Char" )
                    , ( "taiTham", "Random.Generator Char.Char" )
                    , ( "taiViet", "Random.Generator Char.Char" )
                    , ( "taiXuanJingSymbol", "Random.Generator Char.Char" )
                    , ( "takri", "Random.Generator Char.Char" )
                    , ( "tamil", "Random.Generator Char.Char" )
                    , ( "telugu", "Random.Generator Char.Char" )
                    , ( "thaana", "Random.Generator Char.Char" )
                    , ( "thai", "Random.Generator Char.Char" )
                    , ( "tibetan", "Random.Generator Char.Char" )
                    , ( "tifinagh", "Random.Generator Char.Char" )
                    , ( "transportOrMapSymbol", "Random.Generator Char.Char" )
                    , ( "ugaritic", "Random.Generator Char.Char" )
                    , ( "unicode", "Random.Generator Char.Char" )
                    , ( "unifiedCanadianAboriginalSyllabic", "Random.Generator Char.Char" )
                    , ( "unifiedCanadianAboriginalSyllabicExtended", "Random.Generator Char.Char" )
                    , ( "upperCaseLatin", "Random.Generator Char.Char" )
                    , ( "vai", "Random.Generator Char.Char" )
                    , ( "variationSelector", "Random.Generator Char.Char" )
                    , ( "variationSelectorSupplement", "Random.Generator Char.Char" )
                    , ( "vedicExtensions", "Random.Generator Char.Char" )
                    , ( "verticalForm", "Random.Generator Char.Char" )
                    , ( "yiRadical", "Random.Generator Char.Char" )
                    , ( "yiSyllable", "Random.Generator Char.Char" )
                    , ( "yijingHexagramSymbol", "Random.Generator Char.Char" )
                    ]
              }
            , { name = "Random.Date"
              , values =
                    [ ( "month", "Random.Generator Time.Month" )
                    , ( "weekday", "Random.Generator Time.Weekday" )
                    ]
              }
            , { name = "Random.Dict"
              , values =
                    [ ( "dict", "Basics.Int -> Random.Generator comparable -> Random.Generator value -> Random.Generator (Dict.Dict comparable value)" )
                    , ( "rangeLengthDict", "Basics.Int -> Basics.Int -> Random.Generator comparable -> Random.Generator value -> Random.Generator (Dict.Dict comparable value)" )
                    ]
              }
            , { name = "Random.Extra"
              , values =
                    [ ( "andMap", "Random.Generator a -> Random.Generator (a -> b) -> Random.Generator b" )
                    , ( "andThen2", "(a -> b -> Random.Generator c) -> Random.Generator a -> Random.Generator b -> Random.Generator c" )
                    , ( "andThen3", "(a -> b -> c -> Random.Generator d) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d" )
                    , ( "andThen4", "(a -> b -> c -> d -> Random.Generator e) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d -> Random.Generator e" )
                    , ( "andThen5", "(a -> b -> c -> d -> e -> Random.Generator f) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d -> Random.Generator e -> Random.Generator f" )
                    , ( "andThen6", "(a -> b -> c -> d -> e -> f -> Random.Generator g) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d -> Random.Generator e -> Random.Generator f -> Random.Generator g" )
                    , ( "bool", "Random.Generator Basics.Bool" )
                    , ( "choice", "a -> a -> Random.Generator a" )
                    , ( "choices", "Random.Generator a -> List.List (Random.Generator a) -> Random.Generator a" )
                    , ( "combine", "List.List (Random.Generator a) -> Random.Generator (List.List a)" )
                    , ( "filter", "(a -> Basics.Bool) -> Random.Generator a -> Random.Generator a" )
                    , ( "frequency", "( Basics.Float, Random.Generator a ) -> List.List ( Basics.Float, Random.Generator a ) -> Random.Generator a" )
                    , ( "map6", "(a -> b -> c -> d -> e -> f -> g) -> Random.Generator a -> Random.Generator b -> Random.Generator c -> Random.Generator d -> Random.Generator e -> Random.Generator f -> Random.Generator g" )
                    , ( "maybe", "Random.Generator Basics.Bool -> Random.Generator a -> Random.Generator (Maybe.Maybe a)" )
                    , ( "oneIn", "Basics.Int -> Random.Generator Basics.Bool" )
                    , ( "rangeLengthList", "Basics.Int -> Basics.Int -> Random.Generator a -> Random.Generator (List.List a)" )
                    , ( "result", "Random.Generator Basics.Bool -> Random.Generator err -> Random.Generator val -> Random.Generator (Result.Result err val)" )
                    , ( "sample", "List.List a -> Random.Generator (Maybe.Maybe a)" )
                    , ( "sequence", "List.List (Random.Generator a) -> Random.Generator (List.List a)" )
                    , ( "traverse", "(a -> Random.Generator b) -> List.List a -> Random.Generator (List.List b)" )
                    ]
              }
            , { name = "Random.Float"
              , values =
                    [ ( "anyFloat", "Random.Generator Basics.Float" )
                    , ( "floatGreaterThan", "Basics.Float -> Random.Generator Basics.Float" )
                    , ( "floatLessThan", "Basics.Float -> Random.Generator Basics.Float" )
                    , ( "negativeFloat", "Random.Generator Basics.Float" )
                    , ( "normal", "Basics.Float -> Basics.Float -> Random.Generator Basics.Float" )
                    , ( "positiveFloat", "Random.Generator Basics.Float" )
                    , ( "standardNormal", "Random.Generator Basics.Float" )
                    ]
              }
            , { name = "Random.Int"
              , values =
                    [ ( "anyInt", "Random.Generator Basics.Int" )
                    , ( "intGreaterThan", "Basics.Int -> Random.Generator Basics.Int" )
                    , ( "intLessThan", "Basics.Int -> Random.Generator Basics.Int" )
                    , ( "negativeInt", "Random.Generator Basics.Int" )
                    , ( "positiveInt", "Random.Generator Basics.Int" )
                    ]
              }
            , { name = "Random.List"
              , values =
                    [ ( "choices", "Basics.Int -> List.List a -> Random.Generator ( List.List a, List.List a )" )
                    , ( "choose", "List.List a -> Random.Generator ( Maybe.Maybe a, List.List a )" )
                    , ( "shuffle", "List.List a -> Random.Generator (List.List a)" )
                    ]
              }
            , { name = "Random.Order", values = [ ( "order", "Random.Generator Basics.Order" ) ] }
            , { name = "Random.Set"
              , values =
                    [ ( "notInSet", "Set.Set comparable -> Random.Generator comparable -> Random.Generator comparable" )
                    , ( "sample", "Set.Set comparable -> Random.Generator (Maybe.Maybe comparable)" )
                    , ( "set", "Basics.Int -> Random.Generator comparable -> Random.Generator (Set.Set comparable)" )
                    ]
              }
            , { name = "Random.String"
              , values =
                    [ ( "rangeLengthString", "Basics.Int -> Basics.Int -> Random.Generator Char.Char -> Random.Generator String.String" )
                    , ( "string", "Basics.Int -> Random.Generator Char.Char -> Random.Generator String.String" )
                    ]
              }
            ]
        }


suite : Test
suite =
    describe "RandomGeneratorTodo"
        [ codeGenTest "Generates a generator for a int"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

generator : Random.Generator Int
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

generator : Random.Generator Int
generator =
    Random.int Random.minInt Random.maxInt
"""
        , codeGenTest "Generates a generator for a basic custom type"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo =
    Foo
    
generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo =
    Foo
    
generator : Random.Generator Foo
generator =
    Random.constant Foo
"""
        , codeGenTest "Generates a generator for an inline record"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

generator : Random.Generator { a : Int, b : String }
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

generator : Random.Generator { a : Int, b : String }
generator =
    Random.map2
        (\\a b -> { a = a, b = b })
        (Random.int Random.minInt Random.maxInt)
        (Random.uniform "TODO: Define string options" [])
"""
        , codeGenTest "Generates a generator for a declared record"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : String }

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : String }

generator : Random.Generator Foo
generator =
    Random.map2 Foo (Random.int Random.minInt Random.maxInt) (Random.uniform "TODO: Define string options" [])
"""
        , codeGenTest "Generates a generator for a declared record with > 5 fields"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Random.constant Foo
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
        |> Random.map2 (|>) (Random.int Random.minInt Random.maxInt)
"""
        , codeGenTest "Generates a generator for a declared record with > 5 fields nicer with random-extra"
            [ elmRandom, randomExtra ]
            []
            [ """module A exposing (..)
import Random

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

import Random.Extra
import Random.Int

type alias Foo =
  { a : Int, b : Int, c : Int, d : Int, e : Int, f : Int }

generator : Random.Generator Foo
generator =
    Random.constant Foo
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
        |> Random.Extra.andMap Random.Int.anyInt
"""
        , codeGenTest "Generates a generator for a enum"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo =
  A | B

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo =
  A | B

generator : Random.Generator Foo
generator =
    Random.uniform A [ B ]
"""
        , codeGenTest "Generates a generator for a type aliased enum"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo =
  A | B

type alias Bar = Foo

generator : Random.Generator Bar
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo =
  A | B

type alias Bar = Foo

generator : Random.Generator Bar
generator =
    Random.uniform A [ B ]
"""
        , codeGenTest "Generates a generator for a custom type"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Random.uniform
        (Random.map A (Random.int Random.minInt Random.maxInt))
        [ Random.map B (Random.uniform "TODO: Define string options" []) ]
        |> Random.andThen identity
"""
        , codeGenTest "Picks up random-extra for nicer code"
            [ elmRandom, randomExtra ]
            []
            [ """module A exposing (..)
import Random

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

import Random.Extra
import Random.Int

type Foo
  = A Int
  | B String

generator : Random.Generator Foo
generator =
    Random.Extra.choices
        (Random.map A Random.Int.anyInt)
        [ Random.map B (Random.uniform "TODO: Define string options" []) ]
"""
        , codeGenTest "Picks up a generator from another file"
            [ elmRandom ]
            []
            [ """module A exposing (A, generator)
import Random

type A
  = A Int
 

generator : Random.Generator A
generator =
    Random.map A (Random.int Random.minInt Random.maxInt)
""", """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Random.map B A.generator
"""
        , codeGenTest "Picks up a generator from another file with different import notation"
            [ elmRandom ]
            []
            [ """module A exposing (A, generator)
import Random as R exposing (Generator)

type A
  = A Int
 

generator : Generator A
generator =
    R.map A (R.int R.minInt R.maxInt)
""", """module B exposing (..)
import Random exposing (Generator)
import A exposing (A)

type B =
    B A

generator : Generator B
generator =
    Debug.todo ""
""" ]
            """module B exposing (..)
import Random exposing (Generator)
import A exposing (A)

type B =
    B A

generator : Generator B
generator =
    Random.map B A.generator
"""
        , codeGenTestFailsWith "Fails when no way to generate opaque type"
            [ elmRandom ]
            []
            [ """module A exposing (A)
import Random

type A
  = A Int
 
""", """module B exposing (..)
import Random
import A exposing (A)

type B =
    B A

generator : Random.Generator B
generator =
    Debug.todo ""
""" ]
            "Could not automatically generate a definition for `A`, as we don't know how to implement this type."
        , codeGenTest "Generates a generator for a subtype"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type A
    = A B

type B 
    = B Int

generator : Random.Generator A
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type A
    = A B

type B 
    = B Int

generator : Random.Generator A
generator =
    Random.map A randomB

randomB : Random.Generator B
randomB =
    Random.map B (Random.int Random.minInt Random.maxInt)
"""
        , codeGenTest "Applied generic"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator (Foo Int)
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator (Foo Int)
generator =
    Random.map Foo (Random.int Random.minInt Random.maxInt)
"""
        , codeGenTest "full generic 1 arg"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator a -> Random.Generator (Foo a)
generator a =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator a -> Random.Generator (Foo a)
generator a =
    Random.map Foo a
"""
        , codeGenTest "full generic 1 arg with aliasing"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator b -> Random.Generator (Foo b)
generator x =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a
    = Foo a

generator : Random.Generator b -> Random.Generator (Foo b)
generator x =
    Random.map Foo x
"""
        , codeGenTest "full generic 2 arg"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo a b
    = Foo a b

generator : Random.Generator a -> Random.Generator b -> Random.Generator (Foo a b)
generator a b =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a b
    = Foo a b

generator : Random.Generator a -> Random.Generator b -> Random.Generator (Foo a b)
generator a b =
    Random.map2 Foo a b
"""
        , codeGenTest "full generic 2 arg aliasing"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo a b
    = Foo b a

generator : Random.Generator x -> Random.Generator y -> Random.Generator (Foo y x)
generator d s =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo a b
    = Foo b a

generator : Random.Generator x -> Random.Generator y -> Random.Generator (Foo y x)
generator d s =
    Random.map2 Foo d s
"""
        , codeGenTest "partial generic"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type alias Bar x =
    { x : x }

type Foo a
    = Foo (Bar a) (Bar Int)

generator : Random.Generator x -> Random.Generator (Foo x)
generator y =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type alias Bar x =
    { x : x }

type Foo a
    = Foo (Bar a) (Bar Int)

generator : Random.Generator x -> Random.Generator (Foo x)
generator y =
    Random.map2 Foo (randomBar y) (randomBar (Random.int Random.minInt Random.maxInt))

randomBar : Random.Generator x -> Random.Generator (Bar x)
randomBar x =
    Random.map Bar x
"""
        , codeGenTest "recursive"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Tree a
    = Node (Tree a) a (Tree a)
    | Empty

generator : Random.Generator a -> Random.Generator (Tree a)
generator childGenerator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Tree a
    = Node (Tree a) a (Tree a)
    | Empty

generator : Random.Generator a -> Random.Generator (Tree a)
generator childGenerator =
    Random.uniform
        (Random.map3
            Node
            (Random.lazy (\\() -> generator childGenerator))
            childGenerator
            (Random.lazy (\\() -> generator childGenerator))
        )
        [ Random.constant Empty ]
        |> Random.andThen identity
"""
        , codeGenTest "complex mutually recursive"
            [ elmRandom ]
            []
            [ """module A exposing (..)
import Random

type Foo
    = Foo Bar 

type Bar
    = Bar (List Baz)

type Baz
    = Baz (Maybe Bar)

generator : Random.Generator Foo
generator =
    Debug.todo ""
""" ]
            """module A exposing (..)
import Random

type Foo
    = Foo Bar 

type Bar
    = Bar (List Baz)

type Baz
    = Baz (Maybe Bar)

generator : Random.Generator Foo
generator =
    Random.map Foo randomBar

randomBar : Random.Generator Bar
randomBar =
    Random.map Bar (Random.list 3 randomBaz)

randomBaz : Random.Generator Baz
randomBaz =
    Random.map Baz (randomMaybe (Random.lazy (\\() -> randomBar)))

randomMaybe : Random.Generator a -> Random.Generator (Maybe a)
randomMaybe a =
    Random.uniform (Random.map Just a) [ Random.constant Nothing ] |> Random.andThen identity
"""
        ]
