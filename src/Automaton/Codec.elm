module Automaton.Codec exposing (decode, encode)

import Automaton.Core exposing (..)
import Dict
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


encode : Automaton -> E.Value
encode a =
    E.object
        [ ( "states", E.list E.int a.states )
        , ( "alphabet", E.list E.string a.alphabet )
        , ( "transitions"
          , E.list
                (\t ->
                    E.object
                        [ ( "from", E.int t.from )
                        , ( "symbol", E.string t.symbol )
                        , ( "to", E.int t.to_ )
                        ]
                )
                a.transitions
          )
        , ( "start"
          , case a.start of
                Nothing ->
                    E.null

                Just s ->
                    E.int s
          )
        , ( "accepting", E.list E.int a.accepting )
        , ( "positions"
          , a.positions
                |> Dict.toList
                |> E.list
                    (\( stateId, position ) ->
                        E.object
                            [ ( "state", E.int stateId )
                            , ( "x", E.float position.x )
                            , ( "y", E.float position.y )
                            ]
                    )
          )
        ]


decode : Decoder Automaton
decode =
    D.map6
        (\states alphabet transitions start accepting positions ->
            { states = states
            , alphabet = alphabet
            , transitions = transitions
            , start = start
            , accepting = accepting
            , positions =
                if Dict.isEmpty positions then
                    defaultPositions states

                else
                    positions
            }
        )
        (D.field "states" (D.list D.int))
        (D.field "alphabet" (D.list D.string))
        (D.field "transitions"
            (D.list
                (D.map3 Transition
                    (D.field "from" D.int)
                    (D.field "symbol" D.string)
                    (D.field "to" D.int)
                )
            )
        )
        (D.field "start" (D.nullable D.int))
        (D.field "accepting" (D.list D.int))
        (D.oneOf
            [ D.field "positions"
                (D.list
                    (D.map3
                        (\stateId x y -> ( stateId, { x = x, y = y } ))
                        (D.field "state" D.int)
                        (D.field "x" D.float)
                        (D.field "y" D.float)
                    )
                    |> D.map Dict.fromList
                )
            , D.succeed Dict.empty
            ]
        )
