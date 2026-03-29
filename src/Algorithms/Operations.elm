module Algorithms.Operations exposing (complement, intersection, union)

import Automaton.Core as A
import Dict exposing (Dict)
import Set


effectiveAlphabet : A.Automaton -> List A.Symbol
effectiveAlphabet automaton =
    if List.isEmpty automaton.alphabet then
        usedSymbols automaton

    else
        unique automaton.alphabet


type alias W =
    { queue : List ( Int, Int )
    , seen : Dict ( Int, Int ) Int
    , trAcc : List A.Transition
    , nextId : Int
    }


type alias Acc =
    { se : Dict ( Int, Int ) Int
    , qu : List ( Int, Int )
    , tr : List A.Transition
    , nid : Int
    }


complement : A.Automaton -> A.Automaton
complement a0 =
    let
        syms =
            effectiveAlphabet a0

        a =
            totalizeWithSymbols syms a0
    in
    case a.start of
        Nothing ->
            A.empty

        Just _ ->
            let
                accSet =
                    Set.fromList a.accepting

                acceptingNew =
                    a.states
                        |> List.filter (\s -> not (Set.member s accSet))
            in
            { a
                | alphabet = syms
                , accepting = acceptingNew
            }


union : A.Automaton -> A.Automaton -> A.Automaton
union a0 b0 =
    productOp (\accA accB -> accA || accB) a0 b0


intersection : A.Automaton -> A.Automaton -> A.Automaton
intersection a0 b0 =
    productOp (\accA accB -> accA && accB) a0 b0


productOp :
    (Bool -> Bool -> Bool)
    -> A.Automaton
    -> A.Automaton
    -> A.Automaton
productOp acceptRule a0 b0 =
    case ( a0.start, b0.start ) of
        ( Just sa, Just sb ) ->
            let
                syms =
                    unique (effectiveAlphabet a0 ++ effectiveAlphabet b0)

                a =
                    totalizeWithSymbols syms a0

                b =
                    totalizeWithSymbols syms b0

                delta trTab s sym =
                    trTab
                        |> List.filter (\t -> t.from == s && t.symbol == sym)
                        |> List.head
                        |> Maybe.map .to_
                        |> Maybe.withDefault -1

                startPair =
                    ( sa, sb )

                step : W -> W
                step w =
                    case w.queue of
                        [] ->
                            w

                        ( pA, pB ) :: rest ->
                            let
                                sid =
                                    Dict.get ( pA, pB ) w.seen |> Maybe.withDefault -1

                                acc2 : Acc
                                acc2 =
                                    List.foldl
                                        (\sym acc ->
                                            let
                                                da =
                                                    delta a.transitions pA sym

                                                db =
                                                    delta b.transitions pB sym

                                                pair =
                                                    ( da, db )
                                            in
                                            case Dict.get pair acc.se of
                                                Just did ->
                                                    { acc | tr = { from = sid, symbol = sym, to_ = did } :: acc.tr }

                                                Nothing ->
                                                    let
                                                        did =
                                                            acc.nid
                                                    in
                                                    { se = Dict.insert pair did acc.se
                                                    , qu = acc.qu ++ [ pair ]
                                                    , tr = { from = sid, symbol = sym, to_ = did } :: acc.tr
                                                    , nid = acc.nid + 1
                                                    }
                                        )
                                        { se = w.seen, qu = rest, tr = w.trAcc, nid = w.nextId }
                                        syms

                                w2 : W
                                w2 =
                                    { queue = acc2.qu
                                    , seen = acc2.se
                                    , trAcc = acc2.tr
                                    , nextId = acc2.nid
                                    }
                            in
                            step w2

                seen0 =
                    Dict.fromList [ ( startPair, 0 ) ]

                wFinal : W
                wFinal =
                    step { queue = [ startPair ], seen = seen0, trAcc = [], nextId = 1 }

                statesNew =
                    List.range 0 (wFinal.nextId - 1)

                acceptingNew =
                    wFinal.seen
                        |> Dict.toList
                        |> List.filterMap
                            (\( ( x, y ), sid ) ->
                                let
                                    accA =
                                        List.member x a.accepting

                                    accB =
                                        List.member y b.accepting
                                in
                                if acceptRule accA accB then
                                    Just sid

                                else
                                    Nothing
                            )
            in
            { states = statesNew
            , alphabet = syms
            , transitions = List.reverse wFinal.trAcc
            , start = Just 0
            , accepting = acceptingNew
            , positions = A.defaultPositions statesNew
            }

        _ ->
            A.empty


usedSymbols : A.Automaton -> List A.Symbol
usedSymbols a =
    a.transitions
        |> List.map .symbol
        |> Set.fromList
        |> Set.toList


unique : List comparable -> List comparable
unique xs =
    xs |> Set.fromList |> Set.toList


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
