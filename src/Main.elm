port module Main exposing (main)

import Algorithms.Minimize as Min
import Algorithms.Operations as Ops
import Algorithms.Subset as Sub
import Automaton.Codec as Codec
import Automaton.Core as A
import Automaton.Validate as V
import Browser
import Browser.Events
import Dict
import Editor.Update as Ed
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Set
import View.Graph as Graph


usedAlphabet : A.Automaton -> List String
usedAlphabet automaton =
    automaton.transitions
        |> List.map .symbol
        |> List.filter (not << String.isEmpty)
        |> Set.fromList
        |> Set.toList


ensureAlphabetCoverage : A.Automaton -> A.Automaton
ensureAlphabetCoverage automaton =
    { automaton
        | alphabet =
            automaton.alphabet ++ usedAlphabet automaton
                |> Set.fromList
                |> Set.toList
    }


exportJsonString : A.Automaton -> String
exportJsonString automaton =
    automaton
        |> Codec.encode
        |> E.encode 2


type alias DragState =
    { dragging : Maybe A.StateId
    , offsetX : Float
    , offsetY : Float
    , original : Maybe A.Automaton
    }


type SimulationStatus
    = SimReady
    | SimRunning
    | SimAccepted
    | SimRejected
    | SimStuck


type PlaybackSpeed
    = Slow
    | Normal
    | Fast


type alias Simulation =
    { symbols : List String
    , currentIndex : Int
    , currentState : Maybe A.StateId
    , activeTransition : Maybe A.Transition
    , status : SimulationStatus
    , autoplay : Bool
    , elapsedMs : Float
    }


type alias Model =
    { history : Ed.History A.Automaton
    , inputWord : String
    , simulation : Simulation
    , playbackSpeed : PlaybackSpeed
    , msgInfo : String
    , fromSel : String
    , symSel : String
    , toSel : String
    , importText : String
    , exportText : String
    , otherText : String
    , selectedTab : Tab
    , editorSubTab : EditorSubTab
    , algorithmsSubTab : AlgorithmsSubTab
    , dragState : DragState
    }


type Tab
    = EditorTab
    | AlgorithmsTab
    | SimulationTab


type EditorSubTab
    = StatesSub
    | TransitionFormSub
    | TransitionListSub


type AlgorithmsSubTab
    = AlgoBasicSub
    | AlgoProductSub
    | AlgoDataSub


simulationStepMs : Float
simulationStepMs =
    650


playbackStepMs : PlaybackSpeed -> Float
playbackStepMs speed =
    case speed of
        Slow ->
            1100

        Normal ->
            650

        Fast ->
            280


port downloadJsonFile : String -> Cmd msg


port requestJsonFile : () -> Cmd msg


port jsonFileSelected : (String -> msg) -> Sub msg


port exportGraphSvgFile : () -> Cmd msg


port exportGraphPngFile : () -> Cmd msg


wordToSymbols : String -> List String
wordToSymbols word =
    String.toList word |> List.map String.fromChar


currentTapeSymbol : Simulation -> Maybe String
currentTapeSymbol simulation =
    simulation.symbols
        |> List.drop simulation.currentIndex
        |> List.head


findTransition : A.Automaton -> A.StateId -> String -> Maybe A.Transition
findTransition automaton stateId symbol =
    automaton.transitions
        |> List.filter (\transition -> transition.from == stateId && transition.symbol == symbol)
        |> List.head


simulationFinished : Simulation -> Bool
simulationFinished simulation =
    case simulation.status of
        SimAccepted ->
            True

        SimRejected ->
            True

        SimStuck ->
            True

        _ ->
            False


resetSimulation : A.Automaton -> String -> Simulation
resetSimulation automaton word =
    { symbols = wordToSymbols word
    , currentIndex = 0
    , currentState = automaton.start
    , activeTransition = Nothing
    , status =
        case automaton.start of
            Just _ ->
                SimReady

            Nothing ->
                SimStuck
    , autoplay = False
    , elapsedMs = 0
    }


stepSimulation : A.Automaton -> Simulation -> Simulation
stepSimulation automaton simulation =
    case simulation.currentState of
        Nothing ->
            { simulation | status = SimStuck, autoplay = False, elapsedMs = 0, activeTransition = Nothing }

        Just stateId ->
            case currentTapeSymbol simulation of
                Nothing ->
                    { simulation
                        | status =
                            if List.member stateId automaton.accepting then
                                SimAccepted

                            else
                                SimRejected
                        , autoplay = False
                        , elapsedMs = 0
                        , activeTransition = Nothing
                    }

                Just symbol ->
                    case findTransition automaton stateId symbol of
                        Just transition ->
                            let
                                nextIndex =
                                    simulation.currentIndex + 1

                                finalStatus =
                                    if nextIndex >= List.length simulation.symbols then
                                        if List.member transition.to_ automaton.accepting then
                                            SimAccepted

                                        else
                                            SimRejected

                                    else
                                        SimRunning
                            in
                            { simulation
                                | currentIndex = nextIndex
                                , currentState = Just transition.to_
                                , activeTransition = Just transition
                                , status = finalStatus
                                , autoplay =
                                    if finalStatus == SimRunning then
                                        simulation.autoplay

                                    else
                                        False
                                , elapsedMs = 0
                            }

                        Nothing ->
                            { simulation
                                | status = SimStuck
                                , autoplay = False
                                , elapsedMs = 0
                                , activeTransition = Nothing
                            }


runSimulationToEnd : A.Automaton -> Simulation -> Simulation
runSimulationToEnd automaton simulation =
    let
        nextSimulation =
            stepSimulation automaton simulation
    in
    if simulationFinished nextSimulation then
        nextSimulation

    else
        runSimulationToEnd automaton nextSimulation


simulationStatusText : Simulation -> String
simulationStatusText simulation =
    case simulation.status of
        SimReady ->
            case simulation.currentState of
                Just stateId ->
                    "Simulacia je pripravena v stave q" ++ String.fromInt stateId ++ "."

                Nothing ->
                    "Automat nema startovaci stav."

        SimRunning ->
            case simulation.currentState of
                Just stateId ->
                    "Aktualne si v stave q" ++ String.fromInt stateId ++ "."

                Nothing ->
                    "Automat sa nevie pohnut dalej."

        SimAccepted ->
            case simulation.currentState of
                Just stateId ->
                    "Slovo bolo prijate. Automat skoncil v stave q" ++ String.fromInt stateId ++ "."

                Nothing ->
                    "Slovo bolo prijate."

        SimRejected ->
            case simulation.currentState of
                Just stateId ->
                    "Slovo bolo zamietnute. Automat skoncil v stave q" ++ String.fromInt stateId ++ "."

                Nothing ->
                    "Slovo bolo zamietnute."

        SimStuck ->
            case ( simulation.currentState, currentTapeSymbol simulation ) of
                ( Just stateId, Just symbol ) ->
                    "Automat sa zasekol v stave q" ++ String.fromInt stateId ++ " na symbole '" ++ symbol ++ "'."

                ( Nothing, _ ) ->
                    "Automat nema definovany startovaci stav."

                _ ->
                    "Automat sa zasekol."


dfaOnlyMessage : String -> String
dfaOnlyMessage actionLabel =
    actionLabel ++ " je dostupne iba pre DFA. Najprv pouzi NFA â†’ DFA."


guardDeterministic : String -> A.Automaton -> Model -> Maybe Model
guardDeterministic actionLabel automaton model =
    if V.isDeterministic automaton then
        Nothing

    else
        let
            currentSimulation =
                model.simulation

            stoppedSimulation =
                { currentSimulation | autoplay = False }
        in
        Just
            { model
                | simulation = stoppedSimulation
                , msgInfo = dfaOnlyMessage actionLabel
            }


showSimulationOverlay : Model -> Bool
showSimulationOverlay model =
    not
        (String.isEmpty model.inputWord
            && model.simulation.currentIndex == 0
            && model.simulation.status == SimReady
            && model.simulation.activeTransition == Nothing
        )


graphHighlight : Model -> Graph.Highlight
graphHighlight model =
    if showSimulationOverlay model then
        { currentState = model.simulation.currentState
        , activeTransition = model.simulation.activeTransition
        }

    else
        { currentState = Nothing
        , activeTransition = Nothing
        }


playbackSpeedValue : PlaybackSpeed -> String
playbackSpeedValue speed =
    case speed of
        Slow ->
            "1"

        Normal ->
            "2"

        Fast ->
            "3"


playbackSpeedLabel : PlaybackSpeed -> String
playbackSpeedLabel speed =
    case speed of
        Slow ->
            "Slow"

        Normal ->
            "Normal"

        Fast ->
            "Fast"


parsePlaybackSpeed : String -> PlaybackSpeed
parsePlaybackSpeed rawValue =
    case rawValue of
        "1" ->
            Slow

        "3" ->
            Fast

        _ ->
            Normal


init : Model
init =
    let
        initialAutomaton =
            ensureAlphabetCoverage A.dfaExample
    in
    { history = Ed.initHistory initialAutomaton
    , inputWord = ""
    , simulation = resetSimulation initialAutomaton ""
    , playbackSpeed = Normal
    , msgInfo = "Vitaj v editore. MĂ´ĹľeĹˇ pridĂˇvaĹĄ stavy, prechody a okamĹľite testovaĹĄ slovĂˇ."
    , fromSel = "0"
    , symSel = ""
    , toSel = "0"
    , importText = ""
    , exportText = ""
    , otherText = ""
    , selectedTab = EditorTab
    , editorSubTab = StatesSub
    , algorithmsSubTab = AlgoBasicSub
    , dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing }
    }


type Msg
    = Editor Ed.Msg
    | Undo
    | Redo
    | WordChanged String
    | CheckWord
    | ResetSimulation
    | StepSimulation
    | ToggleSimulationPlayback
    | SetPlaybackSpeed String
    | SimulationTick Float
    | FromChanged String
    | SymChanged String
    | ToChanged String
    | AddTransitionClicked
    | ExportJson
    | DownloadJsonFile
    | ExportGraphSvg
    | ExportGraphPng
    | ImportJson
    | PickJsonFile
    | JsonFileLoaded String
    | OtherJsonChanged String
    | ImportTextChanged String
    | NfaToDfa
    | MinimizeDfa
    | ComplementDfa
    | UnionWithOther
    | IntersectWithOther
    | SelectTab Tab
    | SelectEditorSubTab EditorSubTab
    | SelectAlgorithmsSubTab AlgorithmsSubTab
    | GraphMsg Graph.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectTab tab ->
            { model | selectedTab = tab }

        SelectEditorSubTab subTab ->
            { model | editorSubTab = subTab }

        SelectAlgorithmsSubTab subTab ->
            { model | algorithmsSubTab = subTab }

        Editor eMsg ->
            let
                tmpAutomaton =
                    model.history.present
                        |> Ed.apply eMsg

                errs =
                    V.validate (ensureAlphabetCoverage tmpAutomaton)

                newAutomaton =
                    ensureAlphabetCoverage tmpAutomaton

                newHistory =
                    Ed.push newAutomaton model.history

                info =
                    if List.isEmpty errs then
                        "Zmena uloĹľenĂˇ."

                    else
                        renderErrors errs
            in
            { model
                | history = newHistory
                , simulation = resetSimulation newAutomaton model.inputWord
                , msgInfo = info
            }

        Undo ->
            let
                restored =
                    Ed.undo model.history
            in
            { model
                | history = restored
                , simulation = resetSimulation restored.present model.inputWord
                , msgInfo = "Vratena posledna zmena."
            }

        Redo ->
            let
                restored =
                    Ed.redo model.history
            in
            { model
                | history = restored
                , simulation = resetSimulation restored.present model.inputWord
                , msgInfo = "Obnovena dalsia zmena."
            }

        WordChanged s ->
            { model
                | inputWord = s
                , simulation = resetSimulation model.history.present s
                , msgInfo = "Paska je pripravena na krokovanie."
            }

        CheckWord ->
            case guardDeterministic "Simulacia slova" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
                    let
                        finishedSimulation =
                            resetSimulation model.history.present model.inputWord
                                |> runSimulationToEnd model.history.present
                    in
                    { model
                        | simulation = finishedSimulation
                        , msgInfo = simulationStatusText finishedSimulation
                    }

        ResetSimulation ->
            let
                simulation =
                    resetSimulation model.history.present model.inputWord
            in
            { model | simulation = simulation, msgInfo = simulationStatusText simulation }

        StepSimulation ->
            case guardDeterministic "Krokovanie simulacie" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
                    let
                        nextSimulation =
                            stepSimulation model.history.present model.simulation
                    in
                    { model | simulation = nextSimulation, msgInfo = simulationStatusText nextSimulation }

        ToggleSimulationPlayback ->
            case guardDeterministic "Automaticke prehravanie" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
                    let
                        baseSimulation =
                            if simulationFinished model.simulation then
                                resetSimulation model.history.present model.inputWord

                            else
                                model.simulation

                        updatedSimulation =
                            { baseSimulation | autoplay = not baseSimulation.autoplay, elapsedMs = 0 }
                    in
                    { model | simulation = updatedSimulation, msgInfo = simulationStatusText updatedSimulation }

        SetPlaybackSpeed rawValue ->
            { model | playbackSpeed = parsePlaybackSpeed rawValue }

        SimulationTick delta ->
            if model.simulation.autoplay && V.isDeterministic model.history.present then
                let
                    elapsed =
                        model.simulation.elapsedMs + delta
                in
                if elapsed < playbackStepMs model.playbackSpeed then
                    let
                        currentSimulation =
                            model.simulation

                        waitingSimulation =
                            { currentSimulation | elapsedMs = elapsed }
                    in
                    { model | simulation = waitingSimulation }

                else
                    let
                        currentSimulation =
                            model.simulation

                        progressedSimulation =
                            { currentSimulation | elapsedMs = elapsed - playbackStepMs model.playbackSpeed }

                        stepped =
                            stepSimulation model.history.present progressedSimulation
                    in
                    { model | simulation = stepped, msgInfo = simulationStatusText stepped }

            else
                model

        FromChanged s ->
            { model | fromSel = s }

        SymChanged s ->
            { model | symSel = s }

        ToChanged s ->
            { model | toSel = s }

        AddTransitionClicked ->
            let
                mf =
                    String.toInt model.fromSel

                mt =
                    String.toInt model.toSel
            in
            case ( mf, mt, String.trim model.symSel ) of
                ( Just f, Just t, sym ) ->
                    if String.isEmpty sym then
                        { model | msgInfo = "Zadaj symbol prechodu." }

                    else
                        update (Editor (Ed.AddTransition f sym t)) { model | symSel = "" }

                _ ->
                    { model | msgInfo = "VyplĹ From, To a Symbol." }

        ExportJson ->
            let
                jsonStr =
                    exportJsonString model.history.present
            in
            { model | exportText = jsonStr, msgInfo = "JSON export je pripravenĂ˝." }

        DownloadJsonFile ->
            let
                jsonStr =
                    exportJsonString model.history.present
            in
            { model | exportText = jsonStr, msgInfo = "JSON subor je pripraveny na stiahnutie." }

        ExportGraphSvg ->
            { model | msgInfo = "SVG export grafu je pripraveny na stiahnutie." }

        ExportGraphPng ->
            { model | msgInfo = "PNG export grafu je pripraveny na stiahnutie." }

        ImportTextChanged s ->
            { model | importText = s }

        PickJsonFile ->
            { model | msgInfo = "Vyber JSON subor na import." }

        JsonFileLoaded fileContent ->
            { model | importText = fileContent, msgInfo = "Obsah JSON suboru bol nacitany." }

        ImportJson ->
            case D.decodeString Codec.decode model.importText of
                Ok automaton ->
                    let
                        enriched =
                            ensureAlphabetCoverage automaton

                        errs =
                            V.validate enriched

                        hist =
                            Ed.push enriched model.history
                    in
                    if List.isEmpty errs then
                        { model
                            | history = hist
                            , simulation = resetSimulation enriched model.inputWord
                            , msgInfo = "Import prebehol uspesne."
                            , importText = ""
                        }

                    else
                        { model | msgInfo = "Import obsahuje chyby: " ++ renderErrors errs }

                Err e ->
                    { model | msgInfo = "Import error: " ++ D.errorToString e }

        OtherJsonChanged s ->
            { model | otherText = s }

        NfaToDfa ->
            let
                dfa =
                    model.history.present
                        |> Sub.nfaToDfa
                        |> ensureAlphabetCoverage

                hist =
                    Ed.push dfa model.history
            in
            { model
                | history = hist
                , simulation = resetSimulation dfa model.inputWord
                , msgInfo = "Subset construction hotova."
            }

        MinimizeDfa ->
            case guardDeterministic "Minimalizacia" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
                    let
                        mini =
                            model.history.present
                                |> Min.minimize
                                |> ensureAlphabetCoverage

                        hist =
                            Ed.push mini model.history
                    in
                    { model
                        | history = hist
                        , simulation = resetSimulation mini model.inputWord
                        , msgInfo = "Minimalizacia dokoncena."
                    }

        ComplementDfa ->
            case guardDeterministic "Komplement" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
                    let
                        comp =
                            model.history.present
                                |> Ops.complement
                                |> ensureAlphabetCoverage

                        hist =
                            Ed.push comp model.history
                    in
                    { model
                        | history = hist
                        , simulation = resetSimulation comp model.inputWord
                        , msgInfo = "Komplement bol vytvoreny."
                    }

        UnionWithOther ->
            case D.decodeString Codec.decode model.otherText of
                Ok b ->
                    if V.isDeterministic model.history.present && V.isDeterministic b then
                        let
                            u =
                                Ops.union model.history.present b
                                    |> ensureAlphabetCoverage

                            hist =
                                Ed.push u model.history
                        in
                        { model
                            | history = hist
                            , simulation = resetSimulation u model.inputWord
                            , msgInfo = "Zjednotenie A U B je hotove."
                            , otherText = ""
                        }

                    else
                        { model | msgInfo = dfaOnlyMessage "Zjednotenie" }

                Err e ->
                    { model | msgInfo = "JSON druhĂ©ho automatu je neplatnĂ˝: " ++ D.errorToString e }

        IntersectWithOther ->
            case D.decodeString Codec.decode model.otherText of
                Ok b ->
                    if V.isDeterministic model.history.present && V.isDeterministic b then
                        let
                            i =
                                Ops.intersection model.history.present b
                                    |> ensureAlphabetCoverage

                            hist =
                                Ed.push i model.history
                        in
                        { model
                            | history = hist
                            , simulation = resetSimulation i model.inputWord
                            , msgInfo = "Prienik A n B je hotovy."
                            , otherText = ""
                        }

                    else
                        { model | msgInfo = dfaOnlyMessage "Prienik" }

                Err e ->
                    { model | msgInfo = "JSON druhĂ©ho automatu je neplatnĂ˝: " ++ D.errorToString e }

        GraphMsg graphMsg ->
            case graphMsg of
                Graph.StartDrag stateId clientX clientY ->
                    let
                        pos =
                            Dict.get stateId model.history.present.positions
                                |> Maybe.withDefault { x = 100, y = 100 }
                    in
                    { model
                        | dragState =
                            { dragging = Just stateId
                            , offsetX = clientX - pos.x
                            , offsetY = clientY - pos.y
                            , original = Just model.history.present
                            }
                        , msgInfo = "PresĂşvaj stav myĹˇou priamo na plĂˇtne."
                    }

                Graph.Drag clientX clientY ->
                    case model.dragState.dragging of
                        Just stateId ->
                            let
                                newX =
                                    clamp 48 1172 (clientX - model.dragState.offsetX)

                                newY =
                                    clamp 48 792 (clientY - model.dragState.offsetY)

                                updatedAutomaton =
                                    model.history.present
                                        |> Ed.apply (Ed.MoveState stateId newX newY)

                                updatedHistory =
                                    { past = model.history.past
                                    , present = updatedAutomaton
                                    , future = model.history.future
                                    }
                            in
                            { model | history = updatedHistory }

                        Nothing ->
                            model

                Graph.EndDrag ->
                    case model.dragState.original of
                        Just oldAutomaton ->
                            { model
                                | history =
                                    { past = oldAutomaton :: model.history.past
                                    , present = model.history.present
                                    , future = []
                                    }
                                , dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing }
                                , msgInfo = "PozĂ­cia stavu bola uloĹľenĂˇ."
                            }

                        Nothing ->
                            { model | dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing } }

                Graph.NoOp ->
                    model


renderErrors : List V.Error -> String
renderErrors errs =
    case errs of
        [] ->
            ""

        _ ->
            errs
                |> List.map
                    (\e ->
                        case e of
                            V.StartMissing ->
                                "ChĂ˝ba ĹˇtartovacĂ­ stav."

                            V.StartNotInStates ->
                                "Ĺ tartovacĂ­ stav nie je v mnoĹľine states."

                            V.AcceptingNotInStates xs ->
                                "AkceptaÄŤnĂ© stavy mimo states: "
                                    ++ String.join ", " (List.map String.fromInt xs)

                            V.SymbolsOutsideAlphabet syms ->
                                "Symboly mimo abecedy: "
                                    ++ String.join ", " syms

                            V.TransitionStatesNotInStates pairs ->
                                "Prechody odkazujĂş na neexistujĂşce stavy: "
                                    ++ String.join ", "
                                        (List.map
                                            (\( fromState, toState ) ->
                                                "(" ++ String.fromInt fromState ++ " â†’ " ++ String.fromInt toState ++ ")"
                                            )
                                            pairs
                                        )
                    )
                |> String.join " | "


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-[#120f0d] text-[#f5ede3]" ]
        [ div [ class "flex min-h-screen" ]
            [ viewSidebar model
            , viewMain model
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    let
        automaton =
            model.history.present

        alphabetChips =
            usedAlphabet automaton
    in
    div [ class "w-[400px] shrink-0 border-r border-[#3a2c23] bg-[#15110f]/95 backdrop-blur-xl flex flex-col" ]
        [ div [ class "p-6 border-b border-[#3a2c23]" ]
            [ div [ class "rounded-3xl border border-[#3a2c23] bg-gradient-to-br from-[#241d19] via-[#191411] to-[#120f0d] p-5 shadow-2xl" ]
                [ div [ class "flex items-start justify-between gap-4" ]
                    [ div []
                        [ div [ class "inline-flex items-center gap-2 rounded-full border border-amber-400/20 bg-amber-400/10 px-3 py-1 text-xs font-semibold uppercase tracking-[0.18em] text-amber-300" ]
                            [ i [ class "fas fa-star" ] []
                            , text "Elm DFA Studio"
                            ]
                        , h1 [ class "mt-4 text-3xl font-black tracking-tight text-white" ]
                            [ text "SimulĂˇtor DFA" ]
                        ]
                    , div [ class "flex h-12 w-12 items-center justify-center rounded-2xl bg-amber-400/15 text-amber-300 shadow-lg shadow-amber-500/10" ]
                        [ i [ class "fas fa-project-diagram text-xl" ] [] ]
                    ]
                , div [ class "mt-5 grid grid-cols-3 gap-3" ]
                    [ viewMiniMetric "Stavy" (String.fromInt (List.length automaton.states))
                    , viewMiniMetric "Prechody" (String.fromInt (List.length automaton.transitions))
                    , viewMiniMetric "Abeceda" (String.fromInt (List.length alphabetChips))
                    ]
                ]
            ]
        , div [ class "px-4 pt-4" ]
            [ div [ class "rounded-2xl border border-[#45352b] bg-[#1a1411]/85 p-1.5 flex gap-1" ]
                [ viewTabButton EditorTab "fas fa-edit" "Editor" model.selectedTab
                , viewTabButton AlgorithmsTab "fas fa-cogs" "Algoritmy" model.selectedTab
                , viewTabButton SimulationTab "fas fa-play" "SimulĂˇcia" model.selectedTab
                ]
            ]
        , div [ class "flex-1 overflow-y-auto px-4 py-4 scrollbar-thin" ]
            [ case model.selectedTab of
                EditorTab ->
                    viewEditorPanel model automaton

                AlgorithmsTab ->
                    viewAlgorithmsPanel model

                SimulationTab ->
                    viewSimulationPanel model
            ]
        ]


viewMiniMetric : String -> String -> Html Msg
viewMiniMetric labelText valueText =
    div [ class "rounded-2xl border border-[#4a392f] bg-[#261e1a]/90 p-3" ]
        [ div [ class "text-[11px] uppercase tracking-[0.16em] text-[#bca48d]" ] [ text labelText ]
        , div [ class "mt-2 text-lg font-bold text-white" ] [ text valueText ]
        ]


viewTabButton : Tab -> String -> String -> Tab -> Html Msg
viewTabButton tab icon labelText currentTab =
    let
        active =
            tab == currentTab
    in
    button
        [ class <|
            "flex-1 rounded-xl px-3 py-3 text-sm font-semibold transition-all duration-200 "
                ++ (if active then
                        "bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] text-[#1b120e] shadow-lg shadow-amber-900/30"

                    else
                        "text-[#d8c1aa] hover:bg-[#2b211b] hover:text-white"
                   )
        , onClick (SelectTab tab)
        ]
        [ i [ class (icon ++ " mr-2") ] []
        , text labelText
        ]


viewInnerTabButton : Bool -> String -> String -> Msg -> Html Msg
viewInnerTabButton isActive icon labelText buttonMsg =
    button
        [ class <|
            "flex-1 rounded-xl px-3 py-3 text-sm font-semibold transition-all duration-200 "
                ++ (if isActive then
                        "bg-[#c26a2d] text-white shadow-lg shadow-amber-900/30"

                    else
                        "text-[#d8c1aa] hover:bg-[#2b211b] hover:text-white"
                   )
        , onClick buttonMsg
        ]
        [ i [ class (icon ++ " mr-2") ] []
        , text labelText
        ]


viewEditorPanel : Model -> A.Automaton -> Html Msg
viewEditorPanel model automaton =
    div [ class "space-y-4" ]
        [ div [ class "rounded-2xl border border-[#45352b] bg-[#1a1411]/85 p-1.5 flex gap-1" ]
            [ viewInnerTabButton (model.editorSubTab == StatesSub) "fas fa-circle-nodes" "Stavy" (SelectEditorSubTab StatesSub)
            , viewInnerTabButton (model.editorSubTab == TransitionFormSub) "fas fa-arrow-right-arrow-left" "Prechod" (SelectEditorSubTab TransitionFormSub)
            , viewInnerTabButton (model.editorSubTab == TransitionListSub) "fas fa-list" "Zoznam" (SelectEditorSubTab TransitionListSub)
            ]
        , case model.editorSubTab of
            StatesSub ->
                viewSectionCard
                    "Stavy"
                    "PridĂˇvaj stavy, oznaÄŤ Ĺˇtart a akceptaÄŤnĂ© stavy."
                    [ button
                        [ class "w-full rounded-2xl bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] px-4 py-3 text-sm font-semibold text-[#1b120e] shadow-lg shadow-amber-900/30 transition hover:translate-y-[-1px] hover:shadow-amber-900/40"
                        , onClick (Editor Ed.AddState)
                        ]
                        [ i [ class "fas fa-plus-circle mr-2" ] []
                        , text "PridaĹĄ stav"
                        ]
                    , div [ class "mt-4 space-y-3 max-h-[480px] overflow-y-auto scrollbar-thin" ]
                        (if List.isEmpty automaton.states then
                            [ viewEmptyPanel "ZatiaÄľ nemĂˇĹˇ Ĺľiadny stav." ]

                         else
                            List.map (viewStateCard automaton) automaton.states
                        )
                    ]

            TransitionFormSub ->
                viewSectionCard
                    "NovĂ˝ prechod"
                    "Definuj smer a symbol. Prechody sa okamĹľite vykreslia do grafu."
                    [ div [ class "grid grid-cols-1 gap-3" ]
                        [ viewSelectField "Z (From)" model.fromSel automaton.states FromChanged
                        , viewInputField "Symbol" model.symSel "napr. 0, a, x" SymChanged
                        , viewSelectField "Do (To)" model.toSel automaton.states ToChanged
                        , button
                            [ class "w-full rounded-2xl bg-emerald-500 px-4 py-3 text-sm font-semibold text-white transition hover:bg-emerald-400"
                            , onClick AddTransitionClicked
                            ]
                            [ i [ class "fas fa-plus mr-2" ] []
                            , text "PridaĹĄ prechod"
                            ]
                        ]
                    ]

            TransitionListSub ->
                viewSectionCard
                    "AktĂ­vne prechody"
                    "PrehÄľad vĹˇetkĂ˝ch definovanĂ˝ch prechodov v automate."
                    [ if List.isEmpty automaton.transitions then
                        viewEmptyPanel "ZatiaÄľ nemĂˇĹˇ Ĺľiadne prechody."

                      else
                        div [ class "space-y-2 max-h-[520px] overflow-y-auto scrollbar-thin" ]
                            (automaton.transitions |> List.indexedMap viewTransitionItem)
                    ]
        ]


viewSectionCard : String -> String -> List (Html Msg) -> Html Msg
viewSectionCard titleText subtitleText contentNodes =
    div [ class "rounded-3xl border border-[#45352b] bg-[#1a1411]/88 p-4 shadow-xl shadow-black/10" ]
        ([ div [ class "mb-4" ]
            [ h3 [ class "text-base font-bold text-white" ] [ text titleText ]
            , p [ class "mt-1 text-sm leading-6 text-[#bca48d]" ] [ text subtitleText ]
            ]
         ]
            ++ contentNodes
        )


viewSelectField : String -> String -> List Int -> (String -> Msg) -> Html Msg
viewSelectField labelText selectedValue states toMsg =
    div []
        [ label [ class "mb-2 block text-xs font-semibold uppercase tracking-[0.16em] text-[#bca48d]" ] [ text labelText ]
        , select
            [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-white outline-none transition focus:border-amber-400"
            , value selectedValue
            , onInput toMsg
            ]
            (if List.isEmpty states then
                [ option [ value "" ] [ text "Najprv pridaj stav" ] ]

             else
                List.map (\s -> option [ value (String.fromInt s) ] [ text ("q" ++ String.fromInt s) ]) states
            )
        ]


viewInputField : String -> String -> String -> (String -> Msg) -> Html Msg
viewInputField labelText currentValue placeholderText toMsg =
    div []
        [ label [ class "mb-2 block text-xs font-semibold uppercase tracking-[0.16em] text-[#bca48d]" ] [ text labelText ]
        , input
            [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-white outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
            , value currentValue
            , onInput toMsg
            , placeholder placeholderText
            ]
            []
        ]


viewStateCard : A.Automaton -> A.StateId -> Html Msg
viewStateCard automaton stateId =
    let
        isAccepting =
            List.member stateId automaton.accepting

        isStart =
            automaton.start == Just stateId
    in
    div [ class "group rounded-2xl border border-[#45352b] bg-[#120f0d]/90 p-3 transition hover:border-[#5a4638] hover:bg-[#16110f]" ]
        [ div [ class "flex items-center justify-between gap-3" ]
            [ div [ class "flex items-center gap-3" ]
                [ div [ class "flex h-11 w-11 items-center justify-center rounded-2xl bg-gradient-to-br from-amber-500/20 to-[#c26a2d]/20 text-base font-black text-amber-100 ring-1 ring-amber-500/20" ]
                    [ text ("q" ++ String.fromInt stateId) ]
                , div []
                    [ div [ class "font-semibold text-white" ] [ text ("Stav " ++ String.fromInt stateId) ]
                    , div [ class "mt-1 flex flex-wrap gap-2" ]
                        ((if isStart then
                            [ span [ class "rounded-full bg-blue-500/15 px-2.5 py-1 text-[11px] font-semibold text-blue-200" ] [ text "Ĺ tart" ] ]

                          else
                            []
                         )
                            ++ (if isAccepting then
                                    [ span [ class "rounded-full bg-emerald-500/15 px-2.5 py-1 text-[11px] font-semibold text-emerald-200" ] [ text "AkceptaÄŤnĂ˝" ] ]

                                else
                                    []
                               )
                        )
                    ]
                ]
            , div [ class "flex items-center gap-2" ]
                [ viewIconButton
                    (if isAccepting then "bg-emerald-500 text-white" else "bg-[#2a201a] text-[#c9b29a] hover:bg-emerald-500 hover:text-white")
                    "fas fa-check"
                    "AkceptaÄŤnĂ˝ stav"
                    (Editor (Ed.ToggleAccepting stateId))
                , viewIconButton
                    (if isStart then "bg-amber-400 text-slate-950" else "bg-[#2a201a] text-[#c9b29a] hover:bg-amber-400 hover:text-slate-950")
                    "fas fa-star"
                    "Ĺ tartovacĂ­ stav"
                    (Editor (Ed.SetStart (Just stateId)))
                , viewIconButton
                    "bg-[#2a201a] text-[#c9b29a] hover:bg-rose-500 hover:text-white"
                    "fas fa-trash"
                    "OdstrĂˇniĹĄ stav"
                    (Editor (Ed.RemoveState stateId))
                ]
            ]
        ]


viewIconButton : String -> String -> String -> Msg -> Html Msg
viewIconButton extra icon titleText buttonMsg =
    button
        [ class ("flex h-9 w-9 items-center justify-center rounded-xl transition " ++ extra)
        , title titleText
        , onClick buttonMsg
        ]
        [ i [ class (icon ++ " text-xs") ] [] ]


viewTransitionItem : Int -> A.Transition -> Html Msg
viewTransitionItem idx transition =
    div [ class "group flex items-center justify-between gap-3 rounded-2xl border border-[#45352b] bg-[#120f0d]/80 px-3 py-3" ]
        [ div [ class "flex items-center gap-2 text-sm text-[#f2e6d7]" ]
            [ span [ class "font-semibold" ] [ text ("q" ++ String.fromInt transition.from) ]
            , i [ class "fas fa-arrow-right text-xs text-slate-500" ] []
            , span [ class "rounded-full border border-amber-500/20 bg-amber-500/10 px-3 py-1 font-mono text-xs font-bold text-amber-100" ] [ text transition.symbol ]
            , i [ class "fas fa-arrow-right text-xs text-slate-500" ] []
            , span [ class "font-semibold" ] [ text ("q" ++ String.fromInt transition.to_) ]
            ]
        , button
            [ class "flex h-8 w-8 items-center justify-center rounded-xl bg-[#2a201a] text-[#c9b29a] transition hover:bg-rose-500 hover:text-white"
            , onClick (Editor (Ed.RemoveTransition idx))
            , title "OdstrĂˇniĹĄ prechod"
            ]
            [ i [ class "fas fa-times text-xs" ] [] ]
        ]


viewAlgorithmsPanel : Model -> Html Msg
viewAlgorithmsPanel model =
    div [ class "space-y-4" ]
        [ div [ class "rounded-2xl border border-[#45352b] bg-[#1a1411]/85 p-1.5 flex gap-1" ]
            [ viewInnerTabButton (model.algorithmsSubTab == AlgoBasicSub) "fas fa-cogs" "ZĂˇklad" (SelectAlgorithmsSubTab AlgoBasicSub)
            , viewInnerTabButton (model.algorithmsSubTab == AlgoProductSub) "fas fa-object-group" "MnoĹľiny" (SelectAlgorithmsSubTab AlgoProductSub)
            , viewInnerTabButton (model.algorithmsSubTab == AlgoDataSub) "fas fa-file-code" "JSON" (SelectAlgorithmsSubTab AlgoDataSub)
            ]
        , case model.algorithmsSubTab of
            AlgoBasicSub ->
                viewSectionCard
                    "Algoritmy"
                    "OperĂˇcie nad DFA a NFA s okamĹľitĂ˝m prepĂ­sanĂ­m vĂ˝sledku do plĂˇtna."
                    [ div [ class "space-y-3" ]
                        [ viewAlgorithmButton "fas fa-compress" "MinimalizĂˇcia" "OdstrĂˇni redundantnĂ© stavy a zjednoduĹˇĂ­ automat." MinimizeDfa
                        , viewAlgorithmButton "fas fa-not-equal" "Komplement" "Invertuje akceptaÄŤnĂ© stavy pri totalizovanom DFA." ComplementDfa
                        , viewAlgorithmButton "fas fa-code-branch" "NFA â†’ DFA" "Prevedie nedeterministickĂ˝ automat na deterministickĂ˝." NfaToDfa
                        ]
                    ]

            AlgoProductSub ->
                viewSectionCard
                    "MnoĹľinovĂ© operĂˇcie"
                    "VloĹľ druhĂ˝ automat v JSON a vytvor zjednotenie alebo prienik."
                    [ textarea
                        [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 font-mono text-sm text-slate-200 outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
                        , rows 8
                        , placeholder "{\"states\":[0,1],\"alphabet\":[\"0\",\"1\"],...}"
                        , value model.otherText
                        , onInput OtherJsonChanged
                        ]
                        []
                    , div [ class "mt-3 space-y-3" ]
                        [ viewAlgorithmButton "fas fa-object-group" "Zjednotenie A âŞ B" "ProduktovĂˇ konĹˇtrukcia nad pouĹľitĂ˝mi symbolmi." UnionWithOther
                        , viewAlgorithmButton "fas fa-layer-group" "Prienik A â© B" "ProduktovĂˇ konĹˇtrukcia s AND prijĂ­manĂ­m." IntersectWithOther
                        ]
                    ]

            AlgoDataSub ->
                viewSectionCard
                    "Import / Export"
                    "Prenes automat medzi projektmi alebo si priprav JSON do odovzdania."
                    [ button
                        [ class "w-full rounded-2xl bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] px-4 py-3 text-sm font-semibold text-[#1b120e] transition hover:translate-y-[-1px]"
                        , onClick DownloadJsonFile
                        ]
                        [ i [ class "fas fa-download mr-2" ] []
                        , text "Stiahnut JSON subor"
                        ]
                    , button
                        [ class "mt-3 w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-slate-100 transition hover:border-amber-400 hover:text-white"
                        , onClick ExportJson
                        ]
                        [ i [ class "fas fa-code mr-2" ] []
                        , text "Zobrazit JSON v appke"
                        ]
                    , div [ class "mt-3 grid grid-cols-2 gap-3" ]
                        [ button
                            [ class "rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-slate-100 transition hover:border-amber-400 hover:text-white"
                            , onClick ExportGraphSvg
                            ]
                            [ i [ class "fas fa-vector-square mr-2" ] []
                            , text "Export SVG"
                            ]
                        , button
                            [ class "rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-slate-100 transition hover:border-amber-400 hover:text-white"
                            , onClick ExportGraphPng
                            ]
                            [ i [ class "fas fa-image mr-2" ] []
                            , text "Export PNG"
                            ]
                        ]
                    , if String.isEmpty model.exportText then
                        text ""

                      else
                        textarea
                            [ class "mt-3 w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 font-mono text-xs text-slate-200 outline-none"
                            , rows 8
                            , readonly True
                            , value model.exportText
                            ]
                            []
                    , div [ class "mt-3" ]
                        [ textarea
                            [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 font-mono text-sm text-slate-200 outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
                            , rows 8
                            , placeholder "VloĹľ JSON pre import..."
                            , value model.importText
                            , onInput ImportTextChanged
                            ]
                            []
                        , button
                            [ class "mt-3 w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-slate-100 transition hover:border-amber-400 hover:text-white"
                            , onClick PickJsonFile
                            ]
                            [ i [ class "fas fa-folder-open mr-2" ] []
                            , text "Vybrat JSON subor"
                            ]
                        , button
                            [ class "mt-3 w-full rounded-2xl bg-emerald-500 px-4 py-3 text-sm font-semibold text-white transition hover:bg-emerald-400"
                            , onClick ImportJson
                            ]
                            [ i [ class "fas fa-upload mr-2" ] []
                            , text "ImportovaĹĄ automat"
                            ]
                        ]
                    ]
        ]


viewAlgorithmButton : String -> String -> String -> Msg -> Html Msg
viewAlgorithmButton icon titleText descriptionText buttonMsg =
    button
        [ class "w-full rounded-2xl border border-[#45352b] bg-[#120f0d]/90 p-4 text-left transition hover:border-amber-500/30 hover:bg-[#16110f]"
        , onClick buttonMsg
        ]
        [ div [ class "flex items-start gap-4" ]
            [ div [ class "flex h-11 w-11 items-center justify-center rounded-2xl bg-amber-500/10 text-amber-200 ring-1 ring-amber-500/20" ]
                [ i [ class (icon ++ " text-lg") ] [] ]
            , div [ class "flex-1" ]
                [ div [ class "font-semibold text-white" ] [ text titleText ]
                , div [ class "mt-1 text-sm leading-6 text-slate-400" ] [ text descriptionText ]
                ]
            , i [ class "fas fa-chevron-right mt-1 text-slate-600" ] []
            ]
        ]


simulationBadgeClass : SimulationStatus -> String
simulationBadgeClass status =
    case status of
        SimAccepted ->
            "border-emerald-400/30 bg-emerald-500/15 text-emerald-200"

        SimRejected ->
            "border-rose-400/30 bg-rose-500/15 text-rose-200"

        SimStuck ->
            "border-amber-400/30 bg-amber-500/15 text-amber-200"

        SimRunning ->
            "border-amber-400/30 bg-amber-500/15 text-amber-100"

        SimReady ->
            "border-slate-600 bg-slate-800 text-slate-200"


simulationBadgeText : SimulationStatus -> String
simulationBadgeText status =
    case status of
        SimAccepted ->
            "Prijate"

        SimRejected ->
            "Zamietnute"

        SimStuck ->
            "Zaseknute"

        SimRunning ->
            "Bezi"

        SimReady ->
            "Pripravene"


viewTapeCell : Int -> Int -> String -> Html Msg
viewTapeCell activeIndex cellIndex symbol =
    let
        isActive =
            activeIndex == cellIndex

        displaySymbol =
            if String.isEmpty symbol then
                "â–ˇ"

            else
                symbol
    in
    div
        [ class <|
            "flex h-11 min-w-[42px] items-center justify-center border-r border-amber-400/50 px-3 text-base font-bold transition "
                ++ (if isActive then
                        "bg-amber-300 text-[#1b120e] shadow-[0_0_24px_rgba(245,158,11,0.24)]"

                    else
                        "bg-[#1a1411] text-slate-100"
                   )
        ]
        [ text displaySymbol ]


viewSimulationPanel : Model -> Html Msg
viewSimulationPanel model =
    let
        simulation =
            model.simulation

        tapeSymbols =
            simulation.symbols ++ [ "" ]

        activeTapeIndex =
            Basics.min simulation.currentIndex (List.length simulation.symbols)

        currentStateLabel =
            simulation.currentState
                |> Maybe.map (\stateId -> "q" ++ String.fromInt stateId)
                |> Maybe.withDefault "â€”"
    in
    div [ class "space-y-5" ]
        [ viewSectionCard
            "Simulacia slova"
            "Krokuj vstupne slovo po symboloch alebo spusti prehravanie automatu."
            [ input
                [ type_ "text"
                , placeholder "napr. 010110 alebo abba"
                , class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-white outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
                , value model.inputWord
                , onInput WordChanged
                ]
                []
            , div [ class "mt-4 rounded-3xl border border-amber-400/20 bg-[#120f0d]/80 p-4 shadow-inner shadow-amber-900/10" ]
                [ div [ class "mb-3 flex items-center justify-between gap-3" ]
                    [ div [ class "text-xs font-semibold uppercase tracking-[0.18em] text-amber-200" ] [ text "Tape" ]
                    , span [ class ("rounded-full border px-3 py-1 text-[11px] font-semibold uppercase tracking-[0.16em] " ++ simulationBadgeClass simulation.status) ] [ text (simulationBadgeText simulation.status) ]
                    ]
                , div [ class "overflow-x-auto" ]
                    [ div [ class "inline-flex rounded-2xl border border-amber-400/60 bg-[#120f0d]" ]
                        (List.indexedMap (viewTapeCell activeTapeIndex) tapeSymbols)
                    ]
                ]
            , div [ class "mt-4 grid grid-cols-2 gap-3 text-sm" ]
                [ div [ class "rounded-2xl border border-[#5a4638] bg-[#1e1713] px-4 py-3 text-[#e7d3bf]" ]
                    [ div [ class "text-[11px] uppercase tracking-[0.16em] text-[#bca48d]" ] [ text "Aktualny stav" ]
                    , div [ class "mt-2 text-lg font-bold text-white" ] [ text currentStateLabel ]
                    ]
                , div [ class "rounded-2xl border border-[#5a4638] bg-[#1e1713] px-4 py-3 text-[#e7d3bf]" ]
                    [ div [ class "text-[11px] uppercase tracking-[0.16em] text-[#bca48d]" ] [ text "Aktualny symbol" ]
                    , div [ class "mt-2 text-lg font-bold text-white" ] [ text (Maybe.withDefault "â–ˇ" (currentTapeSymbol simulation)) ]
                    ]
                ]
            , p [ class "mt-3 text-sm text-[#c9b29a]" ] [ text (simulationStatusText simulation) ]
            , div [ class "mt-4 rounded-2xl border border-[#5a4638] bg-[#1e1713] px-4 py-4" ]
                [ div [ class "flex items-center justify-between gap-3" ]
                    [ div [ class "text-sm font-semibold text-white" ] [ text "Rychlost prehravania" ]
                    , div [ class "rounded-full border border-[#7a5a40] bg-[#2a201a] px-3 py-1 text-xs font-semibold uppercase tracking-[0.16em] text-[#f1dfbf]" ] [ text (playbackSpeedLabel model.playbackSpeed) ]
                    ]
                , input
                    [ type_ "range"
                    , Html.Attributes.min "1"
                    , Html.Attributes.max "3"
                    , step "1"
                    , value (playbackSpeedValue model.playbackSpeed)
                    , onInput SetPlaybackSpeed
                    , class "mt-4 h-2 w-full cursor-pointer appearance-none rounded-full bg-[#4a392f] accent-amber-300"
                    ]
                    []
                , div [ class "mt-2 flex justify-between text-[11px] font-semibold uppercase tracking-[0.16em] text-[#a98d73]" ]
                    [ span [] [ text "Slow" ]
                    , span [] [ text "Normal" ]
                    , span [] [ text "Fast" ]
                    ]
                ]
            , div [ class "mt-4 grid grid-cols-2 gap-3" ]
                [ button
                    [ class "rounded-2xl bg-[#3d3027] px-4 py-3 text-sm font-semibold text-[#f5ede3] transition hover:bg-[#4b392d]"
                    , onClick ResetSimulation
                    ]
                    [ i [ class "fas fa-rotate-left mr-2" ] []
                    , text "Reset"
                    ]
                , button
                    [ class "rounded-2xl bg-amber-500 px-4 py-3 text-sm font-semibold text-slate-950 transition hover:bg-amber-400"
                    , onClick StepSimulation
                    , disabled (simulationFinished simulation)
                    ]
                    [ i [ class "fas fa-forward-step mr-2" ] []
                    , text "Krok"
                    ]
                , button
                    [ class "rounded-2xl bg-[#c26a2d] px-4 py-3 text-sm font-semibold text-white transition hover:bg-[#d78649]"
                    , onClick ToggleSimulationPlayback
                    ]
                    [ i [ class ((if simulation.autoplay then "fas fa-pause" else "fas fa-play") ++ " mr-2") ] []
                    , text (if simulation.autoplay then "Pauza" else "Auto")
                    ]
                , button
                    [ class "rounded-2xl bg-emerald-500 px-4 py-3 text-sm font-semibold text-white transition hover:bg-emerald-400"
                    , onClick CheckWord
                    ]
                    [ i [ class "fas fa-bolt mr-2" ] []
                    , text "Vyhodnot hned"
                    ]
                ]
            ]
        , viewSectionCard
            "Co uvidis v grafe"
            "Simulacny rezim priamo prepoji pasku s automatovou vizualizaciou."
            [ ul [ class "space-y-3 text-sm leading-6 text-slate-300" ]
                [ li [] [ text "Zlty ring oznacuje aktualny stav, v ktorom sa automat prave nachadza." ]
                , li [] [ text "Oranzovo sa zvyrazni posledny pouzity prechod aj jeho label." ]
                , li [] [ text "Pri zaseknuti ostane zvyrazneny stav a na paske uvidis symbol, na ktorom chybala hrana." ]
                ]
            ]
        ]


viewEmptyPanel : String -> Html Msg
viewEmptyPanel messageText =
    div [ class "rounded-2xl border border-dashed border-slate-700 bg-slate-950/60 px-4 py-6 text-center text-sm text-slate-400" ]
        [ text messageText ]


viewMain : Model -> Html Msg
viewMain model =
    let
        automaton =
            model.history.present
    in
    div [ class "flex-1 bg-[#120f0d]" ]
        [ div [ class "h-full overflow-y-auto" ]
            [ div [ class "mx-auto max-w-[1500px] p-6" ]
                [ div [ class "rounded-[32px] border border-[#3a2c23] bg-[#1a1512]/90 p-5 shadow-2xl shadow-black/20" ]
                    [ div [ class "mb-4 flex flex-col gap-4 lg:flex-row lg:items-center lg:justify-between" ]
                        [ div []
                            [ h3 [ class "text-xl font-bold text-white" ] [ text "PlĂˇtno automatu" ]
                            ]
                        , div [ class "flex flex-wrap gap-3" ]
                            [ viewToolbarButton "fas fa-undo" "Undo" "bg-[#2a201a] text-[#f5ede3] hover:bg-[#3a2c23]" Undo
                            , viewToolbarButton "fas fa-redo" "Redo" "bg-[#2a201a] text-[#f5ede3] hover:bg-[#3a2c23]" Redo
                            ]
                        ]
                    , if List.isEmpty automaton.states then
                        div [ class "grid min-h-[840px] place-items-center rounded-[28px] border border-dashed border-[#4b392d] bg-[#16110f]/70 text-center" ]
                            [ div [ class "max-w-md px-6" ]
                                [ div [ class "mx-auto flex h-16 w-16 items-center justify-center rounded-3xl bg-amber-500/10 text-amber-300 ring-1 ring-amber-500/20" ]
                                    [ i [ class "fas fa-project-diagram text-2xl" ] [] ]
                                , h4 [ class "mt-5 text-2xl font-bold text-white" ] [ text "PlĂˇtno je pripravenĂ©" ]
                                , p [ class "mt-3 text-sm leading-7 text-slate-400" ] [ text "ZaÄŤni pridanĂ­m prvĂ©ho stavu v Äľavom paneli. Potom doplĹ prechody a spusti simulĂˇciu alebo algoritmy." ]
                                ]
                            ]

                      else
                        div [ class "graph-shell overflow-hidden rounded-[28px] border border-[#3a2c23] bg-[#171311]/50 p-3" ]
                            [ Html.map GraphMsg (Graph.view (graphHighlight model) automaton) ]
                    ]
                , viewBottomStats automaton
                ]
            ]
        ]


viewToolbarButton : String -> String -> String -> Msg -> Html Msg
viewToolbarButton icon labelText extra buttonMsg =
    button
        [ class ("inline-flex items-center gap-2 rounded-2xl px-4 py-3 text-sm font-semibold transition " ++ extra)
        , onClick buttonMsg
        ]
        [ i [ class icon ] []
        , text labelText
        ]


viewTopStat : String -> String -> String -> String -> Html Msg
viewTopStat labelText valueText icon subtitleText =
    div [ class "rounded-3xl border border-[#45352b] bg-[#1a1411]/88 p-4" ]
        [ div [ class "flex items-start justify-between gap-4" ]
            [ div []
                [ div [ class "text-xs font-semibold uppercase tracking-[0.16em] text-[#bca48d]" ] [ text labelText ]
                , div [ class "mt-3 text-3xl font-black text-white" ] [ text valueText ]
                , div [ class "mt-2 text-sm leading-6 text-[#bca48d]" ] [ text subtitleText ]
                ]
            , div [ class "flex h-11 w-11 items-center justify-center rounded-2xl bg-amber-500/10 text-amber-200 ring-1 ring-amber-500/20" ]
                [ i [ class (icon ++ " text-lg") ] [] ]
            ]
        ]


viewBottomStats : A.Automaton -> Html Msg
viewBottomStats automaton =
    let
        alphabetList =
            usedAlphabet automaton

        alphabetPreview =
            if List.isEmpty alphabetList then
                "â…"

            else
                "{ " ++ String.join ", " alphabetList ++ " }"

        duplicateGroups =
            V.duplicateTransitionGroups automaton

        deterministic =
            V.isDeterministic automaton
    in
    div [ class "mt-6 rounded-[32px] border border-[#45352b] bg-[#1a1411]/88 p-5 shadow-2xl shadow-black/10" ]
        [ div [ class "mb-4" ]
            [ h3 [ class "text-xl font-bold text-white" ] [ text "Ĺ tatistickĂ© Ăşdaje" ]
            , p [ class "mt-1 text-sm leading-6 text-slate-400" ] [ text "PrehÄľad aktuĂˇlneho automatu presunutĂ˝ pod plĂˇtno." ]
            ]
        , div [ class "grid gap-4 xl:grid-cols-5" ]
            [ viewTopStat "PoÄŤet stavov" (String.fromInt (List.length automaton.states)) "fas fa-circle" "ZĂˇkladnĂ© uzly automatu"
            , viewTopStat "PoÄŤet prechodov" (String.fromInt (List.length automaton.transitions)) "fas fa-random" "VĹˇetky definovanĂ© hrany"
            , viewTopStat "PouĹľitĂˇ abeceda" (String.fromInt (List.length alphabetList)) "fas fa-font" alphabetPreview
            , viewTopStat "Ĺ tart" (Maybe.withDefault "â€”" (Maybe.map (\s -> "q" ++ String.fromInt s) automaton.start)) "fas fa-play" "VstupnĂ˝ stav automatu"
            , viewTopStat "Rezim" (if deterministic then "DFA" else "NFA") "fas fa-code-branch" (if deterministic then "Bez duplicitnych hran pre rovnaky symbol" else "Nasli sa viacnasobne prechody pre rovnaky symbol")
            ]
        , div [ class "mt-4 rounded-3xl border border-[#45352b] bg-[#120f0d]/80 p-4" ]
            [ div [ class "flex items-center justify-between gap-3" ]
                [ div [ class "text-sm font-semibold text-white" ] [ text "Kontrola deterministickosti" ]
                , span
                    [ class <|
                        "rounded-full border px-3 py-1 text-[11px] font-semibold uppercase tracking-[0.16em] "
                            ++ (if deterministic then
                                    "border-emerald-400/30 bg-emerald-500/15 text-emerald-200"

                                else
                                    "border-amber-400/30 bg-amber-500/15 text-amber-200"
                               )
                    ]
                    [ text (if deterministic then "Deterministicky" else "Nedeterministicky") ]
                ]
            , if deterministic then
                p [ class "mt-3 text-sm leading-6 text-slate-400" ] [ text "Automat nema ziadny stav, z ktoreho by viedlo viac prechodov na rovnaky symbol." ]

              else
                div [ class "mt-3 space-y-2" ]
                    (duplicateGroups
                        |> List.map
                            (\group ->
                                div [ class "rounded-2xl border border-amber-400/20 bg-amber-500/10 px-4 py-3 text-sm text-amber-100" ]
                                    [ text
                                        ("q"
                                            ++ String.fromInt group.from
                                            ++ " ma pre symbol '"
                                            ++ group.symbol
                                            ++ "' viac cielov: "
                                            ++ String.join ", " (List.map (\target -> "q" ++ String.fromInt target) group.targets)
                                        )
                                    ]
                            )
                    )
            ]
        ]


viewInfoBanner : String -> Html Msg
viewInfoBanner messageText =
    let
        toneClasses =
            if String.startsWith "âś—" messageText then
                "border-rose-500/30 bg-rose-500/10 text-rose-100"

            else if String.startsWith "âś“" messageText then
                "border-emerald-500/30 bg-emerald-500/10 text-emerald-100"

            else
                "border-cyan-500/30 bg-cyan-500/10 text-cyan-100"
    in
    div [ class ("mt-6 rounded-3xl border px-5 py-4 shadow-lg shadow-black/5 " ++ toneClasses) ]
        [ div [ class "flex items-start gap-3" ]
            [ div [ class "mt-0.5 flex h-9 w-9 items-center justify-center rounded-2xl bg-black/10" ]
                [ i [ class "fas fa-circle-info" ] [] ]
            , div []
                [ div [ class "font-semibold" ] [ text "Stav aplikĂˇcie" ]
                , p [ class "mt-1 text-sm leading-6" ] [ text messageText ]
                ]
            ]
        ]


commandFor : Msg -> Model -> Cmd Msg
commandFor msg model =
    case msg of
        DownloadJsonFile ->
            downloadJsonFile model.exportText

        ExportGraphSvg ->
            exportGraphSvgFile ()

        ExportGraphPng ->
            exportGraphPngFile ()

        PickJsonFile ->
            requestJsonFile ()

        _ ->
            Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        dragSubscription =
            case model.dragState.dragging of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (mouseMoveDecoder |> D.map GraphMsg)
                        , Browser.Events.onMouseUp (D.succeed (GraphMsg Graph.EndDrag))
                        ]

                Nothing ->
                    Sub.none

        playbackSubscription =
            if model.simulation.autoplay then
                Browser.Events.onAnimationFrameDelta SimulationTick

            else
                Sub.none
    in
    Sub.batch [ dragSubscription, playbackSubscription, jsonFileSelected JsonFileLoaded ]


mouseMoveDecoder : D.Decoder Graph.Msg
mouseMoveDecoder =
    D.map2 Graph.Drag
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , view = view
        , update =
            \message model ->
                let
                    updatedModel =
                        update message model
                in
                ( updatedModel, commandFor message updatedModel )
        , subscriptions = subscriptions
        }
