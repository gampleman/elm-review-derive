module E2E exposing (suite)

-- Please edit this only in the e2e directory. The point of this is that the code is generated.

import Standard exposing (..)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Expect

decoder : Decoder (A Int)
decoder =
    Debug.todo ""

encode : A Int -> Value 
encode =
    Debug.todo ""

fuzz : Fuzzer (A Int)
fuzz =
    Debug.todo ""

type Foo 
    = X | Y | Z

all : List Foo 
all =
    Debug.todo ""

toString : Foo -> String 
toString =
    Debug.todo ""

fromString : String -> Maybe Foo 
fromString =
    Debug.todo ""

suite : Test
suite =
    Test.describe "E2E test" [
        Test.fuzz fuzz "JSON encoding roundtrips on fuzzed value" <|
            \val ->
                encode val 
                    |> Json.Decode.decodeValue decoder
                    |> Expect.equal (Ok val)

        , List.map (\foo ->
            Test.test (toString foo ++ " roundtrips from string") <|
                \() ->
                    toString foo
                        |> fromString 
                        |> Expect.equal (Just foo)
        ) all 
            |> Test.describe "toString/fromString"
    ]
