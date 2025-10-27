module Automaton.Core exposing (Automaton, StateId, Symbol, Transition, dfaExample, empty)


type alias StateId =
    Int


type alias Symbol =
    String


type alias Transition =
    { from : StateId
    , symbol : Symbol
    , to_ : StateId
    }


type alias Automaton =
    { states : List StateId
    , alphabet : List Symbol
    , transitions : List Transition
    , start : Maybe StateId
    , accepting : List StateId
    }


empty : Automaton
empty =
    { states = []
    , alphabet = []
    , transitions = []
    , start = Nothing
    , accepting = []
    }


dfaExample : Automaton
dfaExample =
    { states = [ 0, 1 ]
    , alphabet = [ "0", "1" ]
    , transitions =
        [ { from = 0, symbol = "0", to_ = 1 }
        , { from = 0, symbol = "1", to_ = 0 }
        , { from = 1, symbol = "0", to_ = 0 }
        , { from = 1, symbol = "1", to_ = 1 }
        ]
    , start = Just 0
    , accepting = [ 1 ]
    }
