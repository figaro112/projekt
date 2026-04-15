module CodecTest exposing (suite)

import Expect
import Automaton.Codec as Codec
import Automaton.Core as Core
import Json.Decode as D
import Test exposing (..)


suite : Test
suite =
    describe "Codec"
        [ test "encode/decode roundtrip" <|
            \_ ->
                let
                    a =
                        Core.dfaExample

                    json =
                        Codec.encode a
                in
                case D.decodeValue Codec.decode json of
                    Ok b ->
                        Expect.equal a b

                    Err _ ->
                        Expect.fail "decode failed"
        ]
