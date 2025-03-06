module HelperTest exposing (suite)

import Expect
import Internal.Helpers
import Test exposing (Test)


suite : Test
suite =
    Test.describe "intToLetter"
        (List.map
            (\( input, expected ) ->
                Test.test (String.fromInt input ++ " -> " ++ expected) <|
                    \_ -> Internal.Helpers.intToLetter input |> Expect.equal expected
            )
            [ ( 0, "A" )
            , ( 25, "Z" )
            , ( 26, "AA" )
            , ( 51, "AZ" )
            , ( 52, "BA" )
            , ( 53, "BB" )
            , ( 701, "ZZ" )
            ]
        )
