module Main exposing (main)

import Algorithms.Accepts as Accepts
import Automaton.Core as A
import Browser
import Editor.Update as Ed
import Editor.View as EdView
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import View.Graph as Graph


type alias Model =
    { history : Ed.History A.Automaton
    , inputWord : String
    , msgInfo : String
    }


init : Model
init =
    { history = Ed.initHistory A.dfaExample
    , inputWord = ""
    , msgInfo = ""
    }


type Msg
    = Editor Ed.Msg
    | Undo
    | Redo
    | WordChanged String
    | CheckWord


update : Msg -> Model -> Model
update msg model =
    case msg of
        Editor eMsg ->
            let
                newAutomaton =
                    Ed.apply eMsg model.history.present

                newHistory =
                    Ed.push newAutomaton model.history
            in
            { model | history = newHistory }

        Undo ->
            { model | history = Ed.undo model.history }

        Redo ->
            { model | history = Ed.redo model.history }

        WordChanged s ->
            { model | inputWord = s }

        CheckWord ->
            let
                word =
                    String.toList model.inputWord |> List.map String.fromChar

                ok =
                    Accepts.acceptsDfa word model.history.present
            in
            { model
                | msgInfo =
                    if ok then
                        "word ACCEPTED"

                    else
                        "word REJECTED"
            }


view : Model -> Html Msg
view model =
    div [ class "layout" ]
        [ EdView.sidebar
            { onAddState = Editor Ed.AddState
            , onRemoveState = \sid -> Editor (Ed.RemoveState sid)
            , onToggleAccepting = \sid -> Editor (Ed.ToggleAccepting sid)
            , onSetStart = \m -> Editor (Ed.SetStart m)
            , onAddTransition = \from sym to_ -> Editor (Ed.AddTransition from sym to_)
            , onRemoveTransition = \i -> Editor (Ed.RemoveTransition i)
            , onUndo = Undo
            , onRedo = Redo
            }
            model.history.present
        , div [ class "canvas" ]
            [ h3 [] [ text "Automat" ]
            , Graph.view model.history.present
            , h3 [] [ text "Simulácia slova" ]
            , input [ placeholder "napr. 0101", value model.inputWord, onInput WordChanged ] []
            , button [ onClick CheckWord ] [ text "Skontroluj" ]
            , p [] [ text model.msgInfo ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
