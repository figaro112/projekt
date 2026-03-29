module ValidateTest exposing (suite)

import Automaton.Core as Core
import Automaton.Validate as Validate
import Dict
import Expect
import Test exposing (..)


nfaWithDuplicateSymbol : Core.Automaton
nfaWithDuplicateSymbol =
    { states = [ 0, 1, 2 ]
    , alphabet = [ "a" ]
    , transitions =
        [ { from = 0, symbol = "a", to_ = 1 }
        , { from = 0, symbol = "a", to_ = 2 }
        ]
    , start = Just 0
    , accepting = [ 1 ]
    , positions = Dict.empty
    }


suite : Test
suite =
    describe "Validate"
        [ test "detects nondeterministic transitions" <|
            \_ ->
                Validate.isDeterministic nfaWithDuplicateSymbol
                    |> Expect.equal False
        , test "reports duplicate transition group" <|
            \_ ->
                case Validate.duplicateTransitionGroups nfaWithDuplicateSymbol of
                    firstGroup :: _ ->
                        Expect.equal [ 1, 2 ] firstGroup.targets

                    [] ->
                        Expect.fail "expected duplicate transition group"
        ]
