module Automaton.Validate exposing (DuplicateTransitionGroup, Error(..), duplicateTransitionGroups, hasEpsilonTransitions, isDeterministic, validate)

import Automaton.Core exposing (..)
import Dict
import Set


type alias DuplicateTransitionGroup =
    { from : StateId
    , symbol : Symbol
    , targets : List StateId
    }


type Error
    = StartMissing
    | StartNotInStates
    | AcceptingNotInStates (List StateId)
    | SymbolsOutsideAlphabet (List Symbol)
    | TransitionStatesNotInStates (List ( StateId, StateId ))


duplicateTransitionGroups : Automaton -> List DuplicateTransitionGroup
duplicateTransitionGroups automaton =
    automaton.transitions
        |> List.foldl
            (\transition groups ->
                let
                    key =
                        ( transition.from, transition.symbol )

                    existing =
                        Dict.get key groups |> Maybe.withDefault []
                in
                Dict.insert key (transition.to_ :: existing) groups
            )
            Dict.empty
        |> Dict.toList
        |> List.filterMap
            (\( ( fromState, symbol ), targets ) ->
                let
                    uniqueTargets =
                        targets |> Set.fromList |> Set.toList |> List.sort
                in
                if List.length uniqueTargets > 1 then
                    Just
                        { from = fromState
                        , symbol = symbol
                        , targets = uniqueTargets
                        }

                else
                    Nothing
            )


isDeterministic : Automaton -> Bool
isDeterministic automaton =
    List.isEmpty (duplicateTransitionGroups automaton)
        && not (hasEpsilonTransitions automaton)


hasEpsilonTransitions : Automaton -> Bool
hasEpsilonTransitions automaton =
    automaton.transitions
        |> List.any (\transition -> String.isEmpty transition.symbol)


validate : Automaton -> List Error
validate a =
    let
        stateSet =
            Set.fromList a.states

        alphabetSet =
            Set.fromList a.alphabet

        badAcc =
            a.accepting
                |> List.filter (\s -> not (Set.member s stateSet))
                |> Set.fromList
                |> Set.toList

        badSym =
            a.transitions
                |> List.filter (\t -> not (String.isEmpty t.symbol) && not (Set.member t.symbol alphabetSet))
                |> List.map .symbol
                |> Set.fromList
                |> Set.toList

        badTransitionStates =
            a.transitions
                |> List.filter (\t -> not (Set.member t.from stateSet) || not (Set.member t.to_ stateSet))
                |> List.map (\t -> ( t.from, t.to_ ))
                |> Set.fromList
                |> Set.toList
    in
    (case a.start of
        Nothing ->
            [ StartMissing ]

        Just s ->
            if Set.member s stateSet then
                []

            else
                [ StartNotInStates ]
    )
        ++ (if List.isEmpty badAcc then
                []

            else
                [ AcceptingNotInStates badAcc ]
           )
        ++ (if List.isEmpty badSym then
                []

            else
                [ SymbolsOutsideAlphabet badSym ]
           )
        ++ (if List.isEmpty badTransitionStates then
                []

            else
                [ TransitionStatesNotInStates badTransitionStates ]
           )
