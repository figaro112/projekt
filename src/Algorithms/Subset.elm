module Algorithms.Subset exposing (nfaToDfa)

import Automaton.Core as A
import Dict exposing (Dict)
import Set exposing (Set)


type alias Acc =
    { se : Dict String Int
    , qs : List (Set A.StateId)
    , tr : List A.Transition
    , nid : Int
    , sets : Dict String (Set A.StateId)
    }


nfaToDfa : A.Automaton -> A.Automaton
nfaToDfa a =
    case a.start of
        Nothing ->
            A.empty

        Just s0 ->
            let
                alphabet : List A.Symbol
                alphabet =
                    if List.isEmpty a.alphabet then
                        a.transitions
                            |> List.map .symbol
                            |> List.filter (not << String.isEmpty)
                            |> Set.fromList
                            |> Set.toList

                    else
                        a.alphabet
                            |> List.filter (not << String.isEmpty)
                            |> Set.fromList
                            |> Set.toList

                startSet : Set A.StateId
                startSet =
                    epsilonClosure (Set.fromList [ s0 ])

                keyOf : Set A.StateId -> String
                keyOf ss =
                    ss
                        |> Set.toList
                        |> List.sort
                        |> List.map String.fromInt
                        |> String.join ","

                epsilonClosure : Set A.StateId -> Set A.StateId
                epsilonClosure initialStates =
                    let
                        walkClosure : List A.StateId -> Set A.StateId -> Set A.StateId
                        walkClosure pending visited =
                            case pending of
                                [] ->
                                    visited

                                stateId :: rest ->
                                    let
                                        epsilonTargets =
                                            a.transitions
                                                |> List.filter (\transition -> transition.from == stateId && String.isEmpty transition.symbol)
                                                |> List.map .to_
                                                |> List.filter (\target -> not (Set.member target visited))

                                        visitedNext =
                                            List.foldl Set.insert visited epsilonTargets
                                    in
                                    walkClosure (rest ++ epsilonTargets) visitedNext
                    in
                    walkClosure (Set.toList initialStates) initialStates

                move : Set A.StateId -> A.Symbol -> Set A.StateId
                move ss sym =
                    ss
                        |> Set.toList
                        |> List.concatMap
                            (\stateId ->
                                a.transitions
                                    |> List.filter (\transition -> transition.from == stateId && transition.symbol == sym)
                                    |> List.map .to_
                            )
                        |> Set.fromList
                        |> epsilonClosure

                step : Acc -> Acc
                step st =
                    case st.qs of
                        [] ->
                            st

                        sset :: rest ->
                            let
                                sid =
                                    Dict.get (keyOf sset) st.se |> Maybe.withDefault -1

                                foldSym : A.Symbol -> Acc -> Acc
                                foldSym sym acc =
                                    let
                                        dest =
                                            move sset sym

                                        key =
                                            keyOf dest
                                    in
                                    case Dict.get key acc.se of
                                        Just did ->
                                            { acc
                                                | tr = { from = sid, symbol = sym, to_ = did } :: acc.tr
                                                , sets = Dict.insert key dest acc.sets
                                            }

                                        Nothing ->
                                            let
                                                did =
                                                    acc.nid
                                            in
                                            { se = Dict.insert key did acc.se
                                            , qs = dest :: acc.qs
                                            , tr = { from = sid, symbol = sym, to_ = did } :: acc.tr
                                            , nid = acc.nid + 1
                                            , sets = Dict.insert key dest acc.sets
                                            }

                                acc2 : Acc
                                acc2 =
                                    List.foldl
                                        foldSym
                                        { se = st.se, qs = rest, tr = st.tr, nid = st.nid, sets = st.sets }
                                        alphabet
                            in
                            step acc2

                startKey =
                    keyOf startSet

                initSt : Acc
                initSt =
                    { qs = [ startSet ]
                    , se = Dict.fromList [ ( startKey, 0 ) ]
                    , sets = Dict.fromList [ ( startKey, startSet ) ]
                    , tr = []
                    , nid = 1
                    }

                final =
                    step initSt

                statesList =
                    List.range 0 (final.nid - 1)

                acceptingList =
                    final.sets
                        |> Dict.toList
                        |> List.filterMap
                            (\( key, sset ) ->
                                let
                                    sid =
                                        Dict.get key final.se |> Maybe.withDefault -1

                                    inter =
                                        Set.intersect sset (Set.fromList a.accepting)
                                in
                                if Set.isEmpty inter then
                                    Nothing

                                else
                                    Just sid
                            )
            in
            { states = statesList
            , alphabet = alphabet
            , transitions = List.reverse final.tr
            , start = Just 0
            , accepting = acceptingList
            , positions = A.defaultPositions statesList
            }
