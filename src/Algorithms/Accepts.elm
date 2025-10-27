module Algorithms.Accepts exposing (acceptsDfa)

import Automaton.Core as A


acceptsDfa : List A.Symbol -> A.Automaton -> Bool
acceptsDfa word a =
    case a.start of
        Nothing ->
            False

        Just s0 ->
            let
                step : A.Symbol -> A.StateId -> Maybe A.StateId
                step sym s =
                    a.transitions
                        |> List.filter (\t -> t.from == s && t.symbol == sym)
                        |> List.head
                        |> Maybe.map .to_

                endState =
                    List.foldl
                        (\sym ms ->
                            case ms of
                                Nothing ->
                                    Nothing

                                Just s ->
                                    step sym s
                        )
                        (Just s0)
                        word
            in
            case endState of
                Just s ->
                    List.member s a.accepting

                Nothing ->
                    False
