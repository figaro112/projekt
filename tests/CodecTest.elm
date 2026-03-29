module CodecTest exposing (suite)


import Expect
import Test exposing (..)
import Automaton.Core as Core
import Automaton.Codec as Codec
import Json.Decode as D


suite : Test
suite =
describe "Codec"
[ test "encode/decode roundtrip" <| \_ ->
let
a = Core.dfaExample
json = Codec.encode a
in
case D.decodeValue Codec.decode json of
Ok b -> Expect.equal a b
Err _ -> Expect.fail "decode failed"
]