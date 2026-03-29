module Editor.Update exposing (History, Msg(..), apply, initHistory, push, redo, undo)

import Automaton.Core as A
import Dict


type alias History a =
    { past : List a
    , present : a
    , future : List a
    }


initHistory : a -> History a
initHistory a =
    { past = [], present = a, future = [] }


push : a -> History a -> History a
push a h =
    { past = h.present :: h.past, present = a, future = [] }


undo : History a -> History a
undo h =
    case h.past of
        p :: ps ->
            { past = ps, present = p, future = h.present :: h.future }

        [] ->
            h


redo : History a -> History a
redo h =
    case h.future of
        f :: fs ->
            { past = h.present :: h.past, present = f, future = fs }

        [] ->
            h


type Msg
    = AddState
    | RemoveState A.StateId
    | ToggleAccepting A.StateId
    | SetStart (Maybe A.StateId)
    | AddTransition A.StateId String A.StateId
    | RemoveTransition Int
    | MoveState A.StateId Float Float


apply : Msg -> A.Automaton -> A.Automaton
apply msg a =
    case msg of
        AddState ->
            let
                newId =
                    (List.maximum a.states |> Maybe.withDefault -1) + 1

                n =
                    List.length a.states + 1

                angle =
                    2 * pi * toFloat (List.length a.states) / toFloat (max 1 n)

                radius =
                    180

                centerX =
                    420

                centerY =
                    280

                newPos =
                    { x = centerX + radius * cos angle
                    , y = centerY + radius * sin angle
                    }
            in
            { a
                | states = a.states ++ [ newId ]
                , positions = Dict.insert newId newPos a.positions
            }

        RemoveState sid ->
            { a
                | states = List.filter ((/=) sid) a.states
                , transitions = List.filter (\t -> t.from /= sid && t.to_ /= sid) a.transitions
                , start =
                    if a.start == Just sid then
                        Nothing

                    else
                        a.start
                , accepting = List.filter ((/=) sid) a.accepting
                , positions = Dict.remove sid a.positions
            }

        ToggleAccepting sid ->
            if List.member sid a.accepting then
                { a | accepting = List.filter ((/=) sid) a.accepting }

            else
                { a | accepting = a.accepting ++ [ sid ] }

        SetStart m ->
            { a | start = m }

        AddTransition from sym to_ ->
            { a | transitions = a.transitions ++ [ { from = from, symbol = sym, to_ = to_ } ] }

        RemoveTransition idx ->
            { a
                | transitions =
                    a.transitions
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( i, _ ) -> i /= idx)
                        |> List.map Tuple.second
            }

        MoveState sid x y ->
            { a
                | positions = Dict.insert sid { x = x, y = y } a.positions
            }