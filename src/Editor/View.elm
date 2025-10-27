module Editor.View exposing (sidebar)

import Automaton.Core as A
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


sidebar :
    { onAddState : msg
    , onRemoveState : A.StateId -> msg
    , onToggleAccepting : A.StateId -> msg
    , onSetStart : Maybe A.StateId -> msg
    , onAddTransition : A.StateId -> String -> A.StateId -> msg
    , onRemoveTransition : Int -> msg
    , onUndo : msg
    , onRedo : msg
    }
    -> A.Automaton
    -> Html msg
sidebar cfg a =
    div [ class "sidebar" ]
        [ h3 [] [ text "Editor" ]
        , div [ class "toolbar" ]
            [ button [ onClick cfg.onAddState ] [ text "+ Stav" ]
            , button [ onClick cfg.onUndo ] [ text "Undo" ]
            , button [ onClick cfg.onRedo ] [ text "Redo" ]
            ]
        , h4 [] [ text "Stavy" ]
        , ul []
            (List.map
                (\s ->
                    li []
                        [ text ("q" ++ String.fromInt s)
                        , button [ onClick (cfg.onToggleAccepting s) ] [ text "✓" ]
                        , button [ onClick (cfg.onRemoveState s) ] [ text "×" ]
                        , button [ onClick (cfg.onSetStart (Just s)) ] [ text "★" ]
                        ]
                )
                a.states
            )
        , h4 [] [ text "Prechody" ]
        , ul []
            (a.transitions
                |> List.indexedMap
                    (\i t ->
                        li []
                            [ text ("(" ++ String.fromInt t.from ++ ", '" ++ t.symbol ++ "' → " ++ String.fromInt t.to_ ++ ")")
                            , button [ onClick (cfg.onRemoveTransition i) ] [ text "×" ]
                            ]
                    )
            )
        , p [] [ text "Pozn.: UI pre pridanie prechodu (výber z/do a symbol) príde po M1." ]
        ]
