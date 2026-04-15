module AlgorithmTest exposing (suite)

import Algorithms.Minimize as Minimize
import Algorithms.Operations as Operations
import Algorithms.Subset as Subset
import Automaton.Core as Core
import Dict
import Expect
import Test exposing (..)


baseAutomaton : Core.Automaton
baseAutomaton =
    { states = [ 0 ]
    , alphabet = [ "0", "1" ]
    , transitions = []
    , start = Just 0
    , accepting = [ 0 ]
    , positions = Dict.fromList [ ( 0, { x = 120, y = 120 } ) ]
    }


nfaWithMissingTransitions : Core.Automaton
nfaWithMissingTransitions =
    { states = [ 0, 1 ]
    , alphabet = [ "a", "b" ]
    , transitions =
        [ { from = 0, symbol = "a", to_ = 1 } ]
    , start = Just 0
    , accepting = [ 1 ]
    , positions =
        Dict.fromList
            [ ( 0, { x = 120, y = 120 } )
            , ( 1, { x = 240, y = 120 } )
            ]
    }


suite : Test
suite =
    describe "Algorithms"
        [ test "minimize respects declared alphabet when totalizing" <|
            \_ ->
                let
                    minimized =
                        Minimize.minimize baseAutomaton
                in
                Expect.equal 2 (List.length minimized.states)
        , test "complement adds missing transitions for declared alphabet" <|
            \_ ->
                let
                    complemented =
                        Operations.complement baseAutomaton
                in
                complemented.transitions
                    |> List.any (\transition -> transition.from == 0 && transition.symbol == "1")
                    |> Expect.equal True
        , test "subset construction keeps the empty set as a DFA state" <|
            \_ ->
                let
                    dfa =
                        Subset.nfaToDfa nfaWithMissingTransitions
                in
                Expect.all
                    [ \_ -> Expect.equal 3 (List.length dfa.states)
                    , \_ -> Expect.equal 6 (List.length dfa.transitions)
                    ]
                    ()
        ]
