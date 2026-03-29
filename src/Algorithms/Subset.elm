module Algorithms.Subset exposing (nfaToDfa)

import Automaton.Core as A
import Dict exposing (Dict)
import Set exposing (Set)



-- Record akumulátor (TOP-LEVEL, nie v let!)


type alias Acc =
    { se : Dict String Int
    , qs : List (Set A.StateId)
    , tr : List A.Transition
    , nid : Int
    , sets : Dict String (Set A.StateId)
    }


{-| Prevod NFA -> DFA (bez ε-prechodov): metóda množín.
-}
nfaToDfa : A.Automaton -> A.Automaton
nfaToDfa a =
    case a.start of
        Nothing ->
            A.empty

        Just s0 ->
            let
                startSet : Set A.StateId
                startSet =
                    Set.fromList [ s0 ]

                keyOf ss =
                    ss
                        |> Set.toList
                        |> List.sort
                        |> List.map String.fromInt
                        |> String.join ","

                move ss sym =
                    ss
                        |> Set.toList
                        |> List.concatMap
                            (\s ->
                                a.transitions
                                    |> List.filter (\t -> t.from == s && t.symbol == sym)
                                    |> List.map .to_
                            )
                        |> Set.fromList

                step st =
                    case st.qs of
                        [] ->
                            st

                        sset :: rest ->
                            let
                                sid =
                                    Dict.get (keyOf sset) st.se |> Maybe.withDefault -1

                                -- FIX: foldl má typ (a -> b -> b). Tu: A.Symbol -> Acc -> Acc
                                foldSym : A.Symbol -> Acc -> Acc
                                foldSym sym acc =
                                    let
                                        dest =
                                            move sset sym
                                    in
                                    if Set.isEmpty dest then
                                        acc

                                    else
                                        let
                                            k =
                                                keyOf dest
                                        in
                                        case Dict.get k acc.se of
                                            Just did ->
                                                { acc
                                                    | tr = { from = sid, symbol = sym, to_ = did } :: acc.tr
                                                    , sets = Dict.insert k dest acc.sets
                                                }

                                            Nothing ->
                                                let
                                                    did =
                                                        acc.nid
                                                in
                                                { se = Dict.insert k did acc.se
                                                , qs = dest :: acc.qs  -- Pridaj na začiatok namiesto ++
                                                , tr = { from = sid, symbol = sym, to_ = did } :: acc.tr
                                                , nid = acc.nid + 1
                                                , sets = Dict.insert k dest acc.sets
                                                }

                                acc2 : Acc
                                acc2 =
                                    List.foldl
                                        foldSym
                                        { se = st.se, qs = rest, tr = st.tr, nid = st.nid, sets = st.sets }
                                        a.alphabet

                                st2 =
                                    { qs = acc2.qs
                                    , se = acc2.se
                                    , sets = acc2.sets
                                    , tr = acc2.tr
                                    , nid = acc2.nid
                                    }
                            in
                            step st2

                startKey =
                    keyOf startSet

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
                            (\( k, sset ) ->
                                let
                                    sid =
                                        Dict.get k final.se |> Maybe.withDefault -1

                                    inter =
                                        Set.intersect sset (Set.fromList a.accepting)
                                in
                                if Set.isEmpty inter then
                                    Nothing

                                else
                                    Just sid
                            )
            in
            let
                resultStates = statesList
            in
            { states = resultStates
            , alphabet = a.alphabet
            , transitions = List.reverse final.tr
            , start = Just 0
            , accepting = acceptingList
            , positions = A.defaultPositions resultStates
            }