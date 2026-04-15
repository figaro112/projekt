module Algorithms.Minimize exposing (minimize)

import Automaton.Core as A
import Dict exposing (Dict)
import Set exposing (Set)


minimize : A.Automaton -> A.Automaton
minimize a0 =
    case a0.start of
        Nothing ->
            A.empty

        Just _ ->
            let
                aReachable =
                    removeUnreachable a0

                syms =
                    if List.isEmpty aReachable.alphabet then
                        aReachable.transitions
                            |> List.map .symbol
                            |> Set.fromList
                            |> Set.toList

                    else
                        aReachable.alphabet
                            |> Set.fromList
                            |> Set.toList

                a =
                    totalizeWithSymbols syms aReachable

                delta : A.StateId -> A.Symbol -> A.StateId
                delta s sym =
                    a.transitions
                        |> List.filter (\t -> t.from == s && t.symbol == sym)
                        |> List.head
                        |> Maybe.map .to_
                        |> Maybe.withDefault s

                acceptingSet =
                    Set.fromList a.accepting

                nonaccSet =
                    Set.fromList (List.filter (\s -> not (Set.member s acceptingSet)) a.states)

                parts0 =
                    List.filter (not << Set.isEmpty) [ acceptingSet, nonaccSet ]

                partitionIndex : List (Set A.StateId) -> A.StateId -> Int
                partitionIndex parts stateId =
                    parts
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( _, part ) -> Set.member stateId part)
                        |> List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault -1

                signature : List (Set A.StateId) -> A.StateId -> List Int
                signature parts s =
                    syms
                        |> List.map (\sym -> delta s sym |> partitionIndex parts)

                toKey : List Int -> String
                toKey xs =
                    xs |> List.map String.fromInt |> String.join ","

                splitPart : List (Set A.StateId) -> Set A.StateId -> List (Set A.StateId)
                splitPart parts part =
                    part
                        |> Set.toList
                        |> List.foldl
                            (\stateId groups ->
                                let
                                    k =
                                        signature parts stateId |> toKey

                                    old =
                                        Dict.get k groups |> Maybe.withDefault Set.empty
                                in
                                Dict.insert k (Set.insert stateId old) groups
                            )
                            Dict.empty
                        |> Dict.values

                refine : List (Set A.StateId) -> List (Set A.StateId)
                refine parts =
                    parts
                        |> List.concatMap (splitPart parts)

                fix : List (Set A.StateId) -> List (Set A.StateId)
                fix prev =
                    let
                        nxt =
                            refine prev
                    in
                    if samePartition prev nxt then
                        prev

                    else
                        fix nxt

                finalParts =
                    fix parts0

                partIndex : Dict A.StateId Int
                partIndex =
                    finalParts
                        |> List.indexedMap (\i setp -> setp |> Set.toList |> List.map (\s -> ( s, i )))
                        |> List.concat
                        |> Dict.fromList

                statesNew =
                    List.range 0 (List.length finalParts - 1)

                startNew =
                    case a.start of
                        Nothing ->
                            Nothing

                        Just s ->
                            Dict.get s partIndex

                acceptingNew =
                    a.accepting
                        |> List.filterMap (\s -> Dict.get s partIndex)
                        |> Set.fromList
                        |> Set.toList

                transitionsNew =
                    finalParts
                        |> List.indexedMap
                            (\i setp ->
                                case Set.toList setp |> List.head of
                                    Nothing ->
                                        []

                                    Just representative ->
                                        syms
                                            |> List.map
                                                (\sym ->
                                                    let
                                                        destOld =
                                                            delta representative sym

                                                        destNew =
                                                            Dict.get destOld partIndex |> Maybe.withDefault i
                                                    in
                                                    { from = i, symbol = sym, to_ = destNew }
                                                )
                            )
                        |> List.concat
            in
            { states = statesNew
            , alphabet = syms
            , transitions = transitionsNew
            , start = startNew
            , accepting = acceptingNew
            , positions = A.defaultPositions statesNew
            }


removeUnreachable : A.Automaton -> A.Automaton
removeUnreachable a =
    case a.start of
        Nothing ->
            a

        Just start ->
            let
                bfs : List A.StateId -> Set A.StateId -> List A.StateId -> Set A.StateId
                bfs queue visited acc =
                    case queue of
                        [] ->
                            if List.isEmpty acc then
                                visited

                            else
                                bfs (List.reverse acc) visited []

                        current :: rest ->
                            if Set.member current visited then
                                bfs rest visited acc

                            else
                                let
                                    newVisited =
                                        Set.insert current visited

                                    neighbors =
                                        a.transitions
                                            |> List.filter (\t -> t.from == current)
                                            |> List.map .to_

                                    newAcc =
                                        neighbors ++ acc
                                in
                                bfs rest newVisited newAcc

                reachable =
                    bfs [ start ] Set.empty []

                statesNew =
                    a.states |> List.filter (\s -> Set.member s reachable)

                transitionsNew =
                    a.transitions
                        |> List.filter (\t -> Set.member t.from reachable && Set.member t.to_ reachable)

                acceptingNew =
                    a.accepting |> List.filter (\s -> Set.member s reachable)

                positionsNew =
                    a.positions
                        |> Dict.filter (\sid _ -> Set.member sid reachable)
            in
            { a
                | states = statesNew
                , transitions = transitionsNew
                , accepting = acceptingNew
                , positions = positionsNew
            }


samePartition : List (Set Int) -> List (Set Int) -> Bool
samePartition a b =
    let
        norm xs =
            xs |> List.map Set.toList |> List.map List.sort |> List.sort
    in
    norm a == norm b


totalizeWithSymbols : List A.Symbol -> A.Automaton -> A.Automaton
totalizeWithSymbols symbols a =
    case a.start of
        Nothing ->
            a

        Just _ ->
            let
                syms =
                    symbols
                        |> Set.fromList
                        |> Set.toList

                sink =
                    (List.maximum a.states |> Maybe.withDefault -1) + 1

                hasTransition s sym trs =
                    trs
                        |> List.any (\t -> t.from == s && t.symbol == sym)

                ensure s sym trs =
                    if hasTransition s sym trs then
                        trs

                    else
                        { from = s, symbol = sym, to_ = sink } :: trs

                trs1 =
                    List.foldl
                        (\s acc -> List.foldl (\sym ac2 -> ensure s sym ac2) acc syms)
                        a.transitions
                        a.states

                sinkIsUsed =
                    List.any (\t -> t.to_ == sink) trs1

                trs2 =
                    if sinkIsUsed then
                        List.foldl (\sym ac -> { from = sink, symbol = sym, to_ = sink } :: ac) trs1 syms

                    else
                        trs1

                states2 =
                    if sinkIsUsed && not (List.member sink a.states) then
                        a.states ++ [ sink ]

                    else
                        a.states

                positions2 =
                    if sinkIsUsed && not (Dict.member sink a.positions) then
                        Dict.insert sink { x = 760, y = 120 } a.positions

                    else
                        a.positions
            in
            { a
                | alphabet = syms
                , transitions = trs2
                , states = states2
                , positions = positions2
            }
