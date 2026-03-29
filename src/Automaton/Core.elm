module Automaton.Core exposing (Automaton, Position, StateId, Symbol, Transition, defaultPositions, dfaExample, empty)

import Dict exposing (Dict)


type alias StateId =
    Int


type alias Symbol =
    String


type alias Transition =
    { from : StateId
    , symbol : Symbol
    , to_ : StateId
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Automaton =
    { states : List StateId
    , alphabet : List Symbol
    , transitions : List Transition
    , start : Maybe StateId
    , accepting : List StateId
    , positions : Dict StateId Position
    }


empty : Automaton
empty =
    { states = []
    , alphabet = []
    , transitions = []
    , start = Nothing
    , accepting = []
    , positions = Dict.empty
    }


defaultPositions : List StateId -> Dict StateId Position
defaultPositions states =
    let
        n =
            List.length states

        radius =
            180

        centerX =
            420

        centerY =
            280

        makePosition idx stateId =
            let
                angle =
                    2 * pi * toFloat idx / toFloat (max 1 n)

                x =
                    centerX + radius * cos angle

                y =
                    centerY + radius * sin angle
            in
            ( stateId, { x = x, y = y } )
    in
    states
        |> List.indexedMap makePosition
        |> Dict.fromList


dfaExample : Automaton
dfaExample =
    let
        states =
            [ 0, 1 ]
    in
    { states = states
    , alphabet = [ "0", "1" ]
    , transitions =
        [ { from = 0, symbol = "0", to_ = 1 }
        , { from = 0, symbol = "1", to_ = 0 }
        , { from = 1, symbol = "0", to_ = 0 }
        , { from = 1, symbol = "1", to_ = 1 }
        ]
    , start = Just 0
    , accepting = [ 1 ]
    , positions = defaultPositions states
    }