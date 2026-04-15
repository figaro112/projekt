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
    , guideOpen : Bool
    , guideTab : GuideTab
    , jsonFileTarget : JsonFileTarget
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


type GuideTab
    = GuideEditorTab
    | GuideSimulationTab
    | GuideConversionTab
    | GuideDataTab
    | GuideErrorsTab
    | GuideProjectTab


type JsonFileTarget
    = MainImportFile
    | OtherAutomatonFile


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
    actionLabel ++ " je dostupne iba pre DFA. Najprv pouzi NFA -> DFA."


invalidAutomatonMessage : String -> List V.Error -> String
invalidAutomatonMessage actionLabel errs =
    actionLabel ++ " nie je mozne spustit: " ++ renderErrors errs


stopAutoplay : Model -> Model
stopAutoplay model =
    let
        currentSimulation =
            model.simulation
    in
    { model | simulation = { currentSimulation | autoplay = False } }


guardValid : String -> A.Automaton -> Model -> Maybe Model
guardValid actionLabel automaton model =
    let
        errs =
            V.validate automaton

        stoppedModel =
            stopAutoplay model
    in
    if List.isEmpty errs then
        Nothing

    else
        Just
            { stoppedModel | msgInfo = invalidAutomatonMessage actionLabel errs }


guardDeterministic : String -> A.Automaton -> Model -> Maybe Model
guardDeterministic actionLabel automaton model =
    if V.isDeterministic automaton then
        Nothing

    else
        let
            stoppedModel =
                stopAutoplay model
        in
        Just
            { stoppedModel | msgInfo = dfaOnlyMessage actionLabel }


guardValidDeterministic : String -> A.Automaton -> Model -> Maybe Model
guardValidDeterministic actionLabel automaton model =
    case guardValid actionLabel automaton model of
        Just invalidModel ->
            Just invalidModel

        Nothing ->
            guardDeterministic actionLabel automaton model


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


guideTabForAppTab : Tab -> GuideTab
guideTabForAppTab tab =
    case tab of
        EditorTab ->
            GuideEditorTab

        AlgorithmsTab ->
            GuideConversionTab

        SimulationTab ->
            GuideSimulationTab


guidePrimaryAppTab : GuideTab -> Tab
guidePrimaryAppTab guideTab =
    case guideTab of
        GuideEditorTab ->
            EditorTab

        GuideSimulationTab ->
            SimulationTab

        GuideConversionTab ->
            AlgorithmsTab

        GuideDataTab ->
            AlgorithmsTab

        GuideErrorsTab ->
            EditorTab

        GuideProjectTab ->
            EditorTab


guidePrimaryActionLabel : GuideTab -> String
guidePrimaryActionLabel guideTab =
    case guidePrimaryAppTab guideTab of
        EditorTab ->
            "Prejst do editora"

        AlgorithmsTab ->
            "Prejst do algoritmov"

        SimulationTab ->
            "Prejst do simulacie"


guideTabTitle : GuideTab -> String
guideTabTitle guideTab =
    case guideTab of
        GuideEditorTab ->
            "Editor"

        GuideSimulationTab ->
            "Simulator"

        GuideConversionTab ->
            "Konverzia NFA -> DFA"

        GuideDataTab ->
            "JSON a export"

        GuideErrorsTab ->
            "Chybove spravy"

        GuideProjectTab ->
            "O projekte"


guideTabIcon : GuideTab -> String
guideTabIcon guideTab =
    case guideTab of
        GuideEditorTab ->
            "fas fa-pen-ruler"

        GuideSimulationTab ->
            "fas fa-play"

        GuideConversionTab ->
            "fas fa-code-branch"

        GuideDataTab ->
            "fas fa-file-code"

        GuideErrorsTab ->
            "fas fa-triangle-exclamation"

        GuideProjectTab ->
            "fas fa-circle-info"


guideTabSubtitle : GuideTab -> String
guideTabSubtitle guideTab =
    case guideTab of
        GuideEditorTab ->
            "Prakticky navod na budovanie stavov, prechodov a upravu grafu priamo na platne."

        GuideSimulationTab ->
            "Ako funguje krokovanie slova, prehravanie a vizualne zvyraznenie automatu."

        GuideConversionTab ->
            ""

        GuideDataTab ->
            "Import, export, JSON format a praca s druhym automatom pri mnozinovych operaciach."

        GuideErrorsTab ->
            "Najcastejsie validacne problemy a co presne znamenaju pri tvorbe alebo importe automatu."

        GuideProjectTab ->
            "Ako je appka poskladana, co uklada a ake ma aktualne limity."


guideTabs : List GuideTab
guideTabs =
    [ GuideEditorTab
    , GuideSimulationTab
    , GuideConversionTab
    , GuideDataTab
    , GuideErrorsTab
    , GuideProjectTab
    ]


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
    , guideOpen = False
    , guideTab = GuideEditorTab
    , jsonFileTarget = MainImportFile
    , msgInfo = "Vitaj v editore. Mozes pridavat stavy, prechody a okamzite testovat slova."
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
    | ToggleGuide
    | CloseGuide
    | SelectGuideTab GuideTab
    | CloseGuideAndSelectTab Tab
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
    | PickOtherJsonFile
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

        ToggleGuide ->
            if model.guideOpen then
                { model | guideOpen = False }

            else
                { model | guideOpen = True, guideTab = guideTabForAppTab model.selectedTab }

        CloseGuide ->
            { model | guideOpen = False }

        SelectGuideTab guideTab ->
            { model | guideTab = guideTab }

        CloseGuideAndSelectTab tab ->
            { model
                | guideOpen = False
                , selectedTab = tab
                , guideTab = guideTabForAppTab tab
            }

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
                        "Zmena ulozena."

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
                    { model | msgInfo = "Vypln From, To a Symbol." }

        ExportJson ->
            let
                jsonStr =
                    exportJsonString model.history.present
            in
            { model | exportText = jsonStr, msgInfo = "JSON export je pripraveny." }

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
            { model | jsonFileTarget = MainImportFile, msgInfo = "Vyber JSON subor na import." }

        PickOtherJsonFile ->
            { model | jsonFileTarget = OtherAutomatonFile, msgInfo = "Vyber JSON subor pre druhy automat." }

        JsonFileLoaded fileContent ->
            case model.jsonFileTarget of
                MainImportFile ->
                    { model
                        | importText = fileContent
                        , jsonFileTarget = MainImportFile
                        , msgInfo = "Obsah JSON suboru bol nacitany."
                    }

                OtherAutomatonFile ->
                    { model
                        | otherText = fileContent
                        , jsonFileTarget = MainImportFile
                        , msgInfo = "Druhy automat bol nacitany zo suboru."
                    }

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
            case guardValid "NFA -> DFA" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
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
            case guardValidDeterministic "Minimalizacia" model.history.present model of
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
            case guardValidDeterministic "Komplement" model.history.present model of
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
                    case guardValidDeterministic "Zjednotenie" model.history.present model of
                        Just guardedModel ->
                            guardedModel

                        Nothing ->
                            case guardValidDeterministic "Zjednotenie" b model of
                                Just guardedModel ->
                                    guardedModel

                                Nothing ->
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

                Err e ->
                    { model | msgInfo = "JSON druheho automatu je neplatny: " ++ D.errorToString e }

        IntersectWithOther ->
            case D.decodeString Codec.decode model.otherText of
                Ok b ->
                    case guardValidDeterministic "Prienik" model.history.present model of
                        Just guardedModel ->
                            guardedModel

                        Nothing ->
                            case guardValidDeterministic "Prienik" b model of
                                Just guardedModel ->
                                    guardedModel

                                Nothing ->
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

                Err e ->
                    { model | msgInfo = "JSON druheho automatu je neplatny: " ++ D.errorToString e }

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
                        , msgInfo = "Presuvaj stav mysou priamo na platne."
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
                                , msgInfo = "Pozicia stavu bola ulozena."
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
                                "Chyba startovaci stav."

                            V.StartNotInStates ->
                                "Startovaci stav nie je v mnozine states."

                            V.AcceptingNotInStates xs ->
                                "Akceptacne stavy mimo states: "
                                    ++ String.join ", " (List.map String.fromInt xs)

                            V.SymbolsOutsideAlphabet syms ->
                                "Symboly mimo abecedy: "
                                    ++ String.join ", " syms

                            V.TransitionStatesNotInStates pairs ->
                                "Prechody odkazuju na neexistujuce stavy: "
                                    ++ String.join ", "
                                        (List.map
                                            (\( fromState, toState ) ->
                                                "(" ++ String.fromInt fromState ++ " -> " ++ String.fromInt toState ++ ")"
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
        , if model.guideOpen then
            viewGuideOverlay model

          else
            text ""
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
                        , h1 [ class "mt-4 text-3xl font-black tracking-tight text-[#f5ede3]" ]
                            [ text "Simulator DFA" ]
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
                , viewTabButton SimulationTab "fas fa-play" "Simulacia" model.selectedTab
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
        , div [ class "mt-2 text-lg font-bold text-[#f5ede3]" ] [ text valueText ]
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
                        "text-[#d8c1aa] hover:bg-[#2b211b] hover:text-[#f3e4d2]"
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
                        "bg-[#c26a2d] text-[#f7ead9] shadow-lg shadow-amber-900/30"

                    else
                        "text-[#d8c1aa] hover:bg-[#2b211b] hover:text-[#f3e4d2]"
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
                    "Pridavaj stavy, oznac start a akceptacne stavy."
                    [ button
                        [ class "w-full rounded-2xl bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] px-4 py-3 text-sm font-semibold text-[#1b120e] shadow-lg shadow-amber-900/30 transition hover:translate-y-[-1px] hover:shadow-amber-900/40"
                        , onClick (Editor Ed.AddState)
                        ]
                        [ i [ class "fas fa-plus-circle mr-2" ] []
                        , text "Pridat stav"
                        ]
                    , div [ class "mt-4 space-y-3 max-h-[480px] overflow-y-auto scrollbar-thin" ]
                        (if List.isEmpty automaton.states then
                            [ viewEmptyPanel "Zatial nemas ziadny stav." ]

                         else
                            List.map (viewStateCard automaton) automaton.states
                        )
                    ]

            TransitionFormSub ->
                viewSectionCard
                    "Novy prechod"
                    "Definuj smer a symbol. Prechody sa okamzite vykreslia do grafu."
                    [ div [ class "grid grid-cols-1 gap-3" ]
                        [ viewSelectField "Z (From)" model.fromSel automaton.states FromChanged
                        , viewInputField "Symbol" model.symSel "napr. 0, a, x" SymChanged
                        , viewSelectField "Do (To)" model.toSel automaton.states ToChanged
                        , button
                            [ class "w-full rounded-2xl bg-[#a86434] px-4 py-3 text-sm font-semibold text-[#f7ead9] transition hover:bg-[#b77745]"
                            , onClick AddTransitionClicked
                            ]
                            [ i [ class "fas fa-plus mr-2" ] []
                            , text "Pridat prechod"
                            ]
                        ]
                    ]

            TransitionListSub ->
                viewSectionCard
                    "Aktivne prechody"
                    "Prehlad vsetkych definovanych prechodov v automate."
                    [ if List.isEmpty automaton.transitions then
                        viewEmptyPanel "Zatial nemas ziadne prechody."

                      else
                        div [ class "space-y-2 max-h-[520px] overflow-y-auto scrollbar-thin" ]
                            (automaton.transitions |> List.indexedMap viewTransitionItem)
                    ]
        ]


viewSectionCard : String -> String -> List (Html Msg) -> Html Msg
viewSectionCard titleText subtitleText contentNodes =
    div [ class "rounded-3xl border border-[#45352b] bg-[#1a1411]/88 p-4 shadow-xl shadow-black/10" ]
        ([ div [ class "mb-4" ]
            [ h3 [ class "text-base font-bold text-[#f5ede3]" ] [ text titleText ]
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
            [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-[#f5ede3] outline-none transition focus:border-amber-400"
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
            [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-[#f5ede3] outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
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
                    [ div [ class "font-semibold text-[#f5ede3]" ] [ text ("Stav " ++ String.fromInt stateId) ]
                    , div [ class "mt-1 flex flex-wrap gap-2" ]
                        ((if isStart then
                            [ span [ class "rounded-full border border-[#c78a4a]/25 bg-[#6a4328]/35 px-2.5 py-1 text-[11px] font-semibold text-[#f1d2aa]" ] [ text "Start" ] ]

                          else
                            []
                         )
                            ++ (if isAccepting then
                                    [ span [ class "rounded-full border border-[#a86b3b]/25 bg-[#4d3020]/45 px-2.5 py-1 text-[11px] font-semibold text-[#e6c3a2]" ] [ text "Akceptacny" ] ]

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
                    "Akceptacny stav"
                    (Editor (Ed.ToggleAccepting stateId))
                , viewIconButton
                    (if isStart then "bg-amber-400 text-[#1b120e]" else "bg-[#2a201a] text-[#c9b29a] hover:bg-amber-400 hover:text-[#1b120e]")
                    "fas fa-star"
                    "Startovaci stav"
                    (Editor (Ed.SetStart (Just stateId)))
                , viewIconButton
                    "bg-[#2a201a] text-[#c9b29a] hover:bg-rose-500 hover:text-white"
                    "fas fa-trash"
                    "Odstranit stav"
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
            , i [ class "fas fa-arrow-right text-xs text-[#8d705d]" ] []
            , span [ class "rounded-full border border-amber-500/20 bg-amber-500/10 px-3 py-1 font-mono text-xs font-bold text-amber-100" ] [ text transition.symbol ]
            , i [ class "fas fa-arrow-right text-xs text-[#8d705d]" ] []
            , span [ class "font-semibold" ] [ text ("q" ++ String.fromInt transition.to_) ]
            ]
        , button
            [ class "flex h-8 w-8 items-center justify-center rounded-xl bg-[#2a201a] text-[#c9b29a] transition hover:bg-[#76472c] hover:text-[#f7ead9]"
            , onClick (Editor (Ed.RemoveTransition idx))
            , title "Odstranit prechod"
            ]
            [ i [ class "fas fa-times text-xs" ] [] ]
        ]


viewAlgorithmsPanel : Model -> Html Msg
viewAlgorithmsPanel model =
    div [ class "space-y-4" ]
        [ div [ class "rounded-2xl border border-[#45352b] bg-[#1a1411]/85 p-1.5 flex gap-1" ]
            [ viewInnerTabButton (model.algorithmsSubTab == AlgoBasicSub) "fas fa-cogs" "Zaklad" (SelectAlgorithmsSubTab AlgoBasicSub)
            , viewInnerTabButton (model.algorithmsSubTab == AlgoProductSub) "fas fa-object-group" "Mnoziny" (SelectAlgorithmsSubTab AlgoProductSub)
            , viewInnerTabButton (model.algorithmsSubTab == AlgoDataSub) "fas fa-file-code" "JSON" (SelectAlgorithmsSubTab AlgoDataSub)
            ]
        , case model.algorithmsSubTab of
            AlgoBasicSub ->
                viewSectionCard
                    "Algoritmy"
                    "Operacie nad DFA a NFA s okamzitym prepisanim vysledku do platna."
                    [ div [ class "space-y-3" ]
                        [ viewAlgorithmButton "fas fa-compress" "Minimalizacia" "Odstrani redundantne stavy a zjednodusi automat." MinimizeDfa
                        , viewAlgorithmButton "fas fa-not-equal" "Komplement" "Invertuje akceptacne stavy pri totalizovanom DFA." ComplementDfa
                        , viewAlgorithmButton "fas fa-code-branch" "NFA -> DFA" "Prevedie nedeterministicky automat na deterministicky." NfaToDfa
                        ]
                    ]

            AlgoProductSub ->
                viewSectionCard
                    "Mnozinove operacie"
                    "Vloz druhy automat v JSON a vytvor zjednotenie alebo prienik."
                    [ button
                        [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                        , onClick PickOtherJsonFile
                        ]
                        [ i [ class "fas fa-folder-open mr-2" ] []
                        , text "Vybrat JSON subor druheho automatu"
                        ]
                    , textarea
                        [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 font-mono text-sm text-[#e6d2be] outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
                        , rows 8
                        , placeholder "{\"states\":[0,1],\"alphabet\":[\"0\",\"1\"],...}"
                        , value model.otherText
                        , onInput OtherJsonChanged
                        ]
                        []
                    , div [ class "mt-3 space-y-3" ]
                        [ viewAlgorithmButton "fas fa-object-group" "Zjednotenie A U B" "Produktova konstrukcia nad pouzitymi symbolmi." UnionWithOther
                        , viewAlgorithmButton "fas fa-layer-group" "Prienik A n B" "Produktova konstrukcia s AND prijimanim." IntersectWithOther
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
                        [ class "mt-3 w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                        , onClick ExportJson
                        ]
                        [ i [ class "fas fa-code mr-2" ] []
                        , text "Zobrazit JSON v appke"
                        ]
                    , div [ class "mt-3 grid grid-cols-2 gap-3" ]
                        [ button
                            [ class "rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                            , onClick ExportGraphSvg
                            ]
                            [ i [ class "fas fa-vector-square mr-2" ] []
                            , text "Export SVG"
                            ]
                        , button
                            [ class "rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
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
                            [ class "mt-3 w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 font-mono text-xs text-[#e6d2be] outline-none"
                            , rows 8
                            , readonly True
                            , value model.exportText
                            ]
                            []
                    , div [ class "mt-3" ]
                        [ textarea
                            [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 font-mono text-sm text-[#e6d2be] outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
                            , rows 8
                            , placeholder "Vloz JSON pre import..."
                            , value model.importText
                            , onInput ImportTextChanged
                            ]
                            []
                        , button
                            [ class "mt-3 w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                            , onClick PickJsonFile
                            ]
                            [ i [ class "fas fa-folder-open mr-2" ] []
                            , text "Vybrat JSON subor"
                            ]
                        , button
                            [ class "mt-3 w-full rounded-2xl bg-[#a86434] px-4 py-3 text-sm font-semibold text-[#f7ead9] transition hover:bg-[#b77745]"
                            , onClick ImportJson
                            ]
                            [ i [ class "fas fa-upload mr-2" ] []
                            , text "Importovat automat"
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
                [ div [ class "font-semibold text-[#f5ede3]" ] [ text titleText ]
                , div [ class "mt-1 text-sm leading-6 text-[#bca48d]" ] [ text descriptionText ]
                ]
            , i [ class "fas fa-chevron-right mt-1 text-[#7f6756]" ] []
            ]
        ]


simulationBadgeClass : SimulationStatus -> String
simulationBadgeClass status =
    case status of
        SimAccepted ->
            "border-[#c48a4a]/30 bg-[#8c5d33]/20 text-[#f0d4ae]"

        SimRejected ->
            "border-[#8a5938]/35 bg-[#5a3825]/25 text-[#d9baa0]"

        SimStuck ->
            "border-[#b97a3c]/30 bg-[#7b4f2c]/22 text-[#edc89b]"

        SimRunning ->
            "border-[#d39a57]/30 bg-[#9a6534]/20 text-[#f3dfc2]"

        SimReady ->
            "border-[#5b473b] bg-[#2a201a] text-[#d8c1aa]"


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
                "_"

            else
                symbol
    in
    div
        [ class <|
            "flex h-11 min-w-[42px] items-center justify-center border-r border-amber-400/50 px-3 text-base font-bold transition "
                ++ (if isActive then
                        "bg-amber-300 text-[#1b120e] shadow-[0_0_24px_rgba(245,158,11,0.24)]"

                    else
                        "bg-[#1a1411] text-[#eadbcf]"
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
                |> Maybe.withDefault "-"
    in
    div [ class "space-y-5" ]
        [ viewSectionCard
            "Simulacia slova"
            "Krokuj vstupne slovo po symboloch alebo spusti prehravanie automatu."
            [ input
                [ type_ "text"
                , placeholder "napr. 010110 alebo abba"
                , class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-[#f5ede3] outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
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
                    , div [ class "mt-2 text-lg font-bold text-[#f5ede3]" ] [ text currentStateLabel ]
                    ]
                , div [ class "rounded-2xl border border-[#5a4638] bg-[#1e1713] px-4 py-3 text-[#e7d3bf]" ]
                    [ div [ class "text-[11px] uppercase tracking-[0.16em] text-[#bca48d]" ] [ text "Aktualny symbol" ]
                    , div [ class "mt-2 text-lg font-bold text-[#f5ede3]" ] [ text (Maybe.withDefault "_" (currentTapeSymbol simulation)) ]
                    ]
                ]
            , p [ class "mt-3 text-sm text-[#c9b29a]" ] [ text (simulationStatusText simulation) ]
            , div [ class "mt-4 rounded-2xl border border-[#5a4638] bg-[#1e1713] px-4 py-4" ]
                [ div [ class "flex items-center justify-between gap-3" ]
                    [ div [ class "text-sm font-semibold text-[#f5ede3]" ] [ text "Rychlost prehravania" ]
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
                    [ class "rounded-2xl bg-[#d59652] px-4 py-3 text-sm font-semibold text-[#1b120e] transition hover:bg-[#e0a866]"
                    , onClick StepSimulation
                    , disabled (simulationFinished simulation)
                    ]
                    [ i [ class "fas fa-forward-step mr-2" ] []
                    , text "Krok"
                    ]
                , button
                    [ class "rounded-2xl bg-[#8f5a31] px-4 py-3 text-sm font-semibold text-[#f7ead9] transition hover:bg-[#a56c3f]"
                    , onClick ToggleSimulationPlayback
                    ]
                    [ i [ class ((if simulation.autoplay then "fas fa-pause" else "fas fa-play") ++ " mr-2") ] []
                    , text (if simulation.autoplay then "Pauza" else "Auto")
                    ]
                , button
                    [ class "rounded-2xl bg-[#a86434] px-4 py-3 text-sm font-semibold text-[#f7ead9] transition hover:bg-[#b77745]"
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
            [ ul [ class "space-y-3 text-sm leading-6 text-[#d8c1aa]" ]
                [ li [] [ text "Zlty ring oznacuje aktualny stav, v ktorom sa automat prave nachadza." ]
                , li [] [ text "Oranzovo sa zvyrazni posledny pouzity prechod aj jeho label." ]
                , li [] [ text "Pri zaseknuti ostane zvyrazneny stav a na paske uvidis symbol, na ktorom chybala hrana." ]
                ]
            ]
        ]


viewEmptyPanel : String -> Html Msg
viewEmptyPanel messageText =
    div [ class "rounded-2xl border border-dashed border-[#5a4638] bg-[#15110f]/80 px-4 py-6 text-center text-sm text-[#bca48d]" ]
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
                            [ h3 [ class "text-xl font-bold text-[#f5ede3]" ] [ text "Platno automatu" ]
                            ]
                        , div [ class "flex flex-wrap gap-3" ]
                            [ viewToolbarButton "fas fa-book-open" "Guide" "bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] text-[#1b120e] hover:brightness-110" ToggleGuide
                            , viewToolbarButton "fas fa-undo" "Undo" "bg-[#2a201a] text-[#f5ede3] hover:bg-[#3a2c23]" Undo
                            , viewToolbarButton "fas fa-redo" "Redo" "bg-[#2a201a] text-[#f5ede3] hover:bg-[#3a2c23]" Redo
                            ]
                        ]
                    , if List.isEmpty automaton.states then
                        div [ class "grid min-h-[840px] place-items-center rounded-[28px] border border-dashed border-[#4b392d] bg-[#16110f]/70 text-center" ]
                            [ div [ class "max-w-md px-6" ]
                                [ div [ class "mx-auto flex h-16 w-16 items-center justify-center rounded-3xl bg-amber-500/10 text-amber-300 ring-1 ring-amber-500/20" ]
                                    [ i [ class "fas fa-project-diagram text-2xl" ] [] ]
                                , h4 [ class "mt-5 text-2xl font-bold text-[#f5ede3]" ] [ text "Platno je pripravene" ]
                                , p [ class "mt-3 text-sm leading-7 text-[#bca48d]" ] [ text "Zacni pridanim prveho stavu v lavom paneli. Potom dopln prechody a spusti simulaciu alebo algoritmy." ]
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


viewGuideOverlay : Model -> Html Msg
viewGuideOverlay model =
    let
        currentTab =
            model.guideTab

        primaryTab =
            guidePrimaryAppTab currentTab
    in
    div [ class "fixed inset-0 z-50 grid place-items-center bg-black/60 px-4 py-6 backdrop-blur-sm" ]
        [ div [ class "flex max-h-[92vh] w-full max-w-[1080px] flex-col overflow-hidden rounded-[32px] border border-[#4a392f] bg-[#120f0d]/98 shadow-[0_28px_100px_rgba(0,0,0,0.5)]" ]
            [ div [ class "border-b border-[#34271f] bg-gradient-to-r from-[#1b1512] via-[#171210] to-[#120f0d]" ]
                [ div [ class "flex flex-col gap-4 px-6 py-6 lg:flex-row lg:items-start lg:justify-between" ]
                    [ div [ class "max-w-3xl" ]
                        ([ div [ class "inline-flex items-center gap-2 rounded-full border border-amber-400/20 bg-amber-400/10 px-3 py-1 text-xs font-semibold uppercase tracking-[0.18em] text-amber-300" ]
                            [ i [ class "fas fa-book-open" ] []
                            , text "Sprievodca aplikaciou"
                            ]
                         , h2 [ class "mt-4 text-3xl font-black tracking-tight text-[#f5ede3]" ] [ text "Guide" ]
                         ]
                            ++ (if String.isEmpty (guideTabSubtitle currentTab) then
                                    []

                                else
                                    [ p [ class "mt-3 text-sm leading-7 text-[#cbb39a]" ] [ text (guideTabSubtitle currentTab) ] ]
                               )
                        )
                    , div [ class "flex items-center gap-3 self-start" ]
                        [ button
                            [ class "inline-flex items-center gap-2 rounded-2xl bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] px-4 py-3 text-sm font-semibold text-[#1b120e] shadow-lg shadow-amber-900/30 transition hover:brightness-110"
                            , onClick (CloseGuideAndSelectTab primaryTab)
                            ]
                            [ i [ class "fas fa-arrow-right" ] []
                            , text (guidePrimaryActionLabel currentTab)
                            ]
                        , button
                            [ class "flex h-11 w-11 items-center justify-center rounded-2xl bg-[#211914] text-[#d8c1aa] transition hover:bg-[#76472c] hover:text-[#f7ead9]"
                            , onClick CloseGuide
                            , title "Zavriet guide"
                            ]
                            [ i [ class "fas fa-times" ] [] ]
                        ]
                    ]
                , div [ class "overflow-x-auto border-t border-[#2b211b]" ]
                    [ div [ class "flex min-w-max gap-1 px-3 py-3" ]
                        (List.map (viewGuideTabButton currentTab) guideTabs)
                    ]
                ]
            , div [ class "flex-1 overflow-y-auto bg-[#15110f] px-6 py-6 scrollbar-thin" ]
                [ viewGuideTabContent currentTab ]
            ]
        ]


viewGuideTabButton : GuideTab -> GuideTab -> Html Msg
viewGuideTabButton activeTab tab =
    let
        isActive =
            activeTab == tab
    in
    button
        [ class <|
            "inline-flex items-center gap-2 rounded-2xl px-4 py-3 text-sm font-semibold transition-all duration-200 "
                ++ (if isActive then
                        "bg-[#e6a95f] text-[#1b120e] shadow-lg shadow-amber-900/25"

                    else
                        "text-[#d8c1aa] hover:bg-[#241b16] hover:text-[#f3e4d2]"
                   )
        , onClick (SelectGuideTab tab)
        ]
        [ i [ class (guideTabIcon tab) ] []
        , text (guideTabTitle tab)
        ]


viewGuideTabContent : GuideTab -> Html Msg
viewGuideTabContent guideTab =
    case guideTab of
        GuideEditorTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-pen-ruler" "Editor automatov" "Panel Editor je urceny na tvorbu stavov, prechodov a upravu grafu s okamzitou vizualnou odozvou." [ "Stavy", "Prechody", "Drag & drop", "Undo / Redo" ]
                , viewGuideActionTable
                    "Praca so stavmi"
                    "Zakladne akcie pre stavovy diagram."
                    [ ( "Pridanie stavu", "V podkarte Stavy klikni na Pridat stav. Appka vytvori dalsie volne ID a vlozi novy uzol na platno." )
                    , ( "Startovaci stav", "Hviezdicka nastavi vybrany stav ako jediny start automatu." )
                    , ( "Akceptacny stav", "Fajka prepina akceptacny stav. V grafe ho spoznas podla dvojitej kruznice." )
                    , ( "Odstranenie stavu", "Kos odstrani stav aj vsetky prechody, ktore do neho vedu alebo z neho vychadzaju." )
                    , ( "Presun na platne", "Stav mozes chytit mysou priamo v grafe. Nova pozicia sa po pusteni ulozi do historie." )
                    ]
                , viewGuideActionTable
                    "Praca s prechodmi"
                    "Ako definovat jazyk automatu v editore."
                    [ ( "Novy prechod", "V podkarte Prechod zvol From, symbol a To. Hrana sa hned vykresli v diagrame." )
                    , ( "Symbol prechodu", "Pole Symbol akceptuje lubovolny textovy symbol. Pouzite symboly sa doplnaju do abecedy." )
                    , ( "Zoznam prechodov", "Podkarta Zoznam ukazuje vsetky prechody a dovoluje ich mazat po jednom." )
                    , ( "DFA vs NFA", "Ak z jedneho stavu vedu pre rovnaky symbol rozne ciele, automat je NFA a cast algoritmov sa zablokuje." )
                    ]
                ]

        GuideSimulationTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-play" "Simulator slova" "Simulacia cita vstup zlava doprava po jednom symbole a synchronizuje pasku s grafom automatu." [ "Krok", "Auto", "Vyhodnot hned", "Tape" ]
                , viewGuideActionTable
                    "Ovladanie simulacie"
                    "Tri rozne sposoby behu nad jednym vstupom."
                    [ ( "Vstupne slovo", "Do pola zadas retazec, ktory sa rozbije na jednotlive symboly. Paska sa obnovi okamzite." )
                    , ( "Krok", "Vykona presne jeden prechod a posunie citanie o jeden symbol." )
                    , ( "Auto", "Spusti prehravanie s nastavitelou rychlostou Slow / Normal / Fast." )
                    , ( "Vyhodnot hned", "Prebehne cely vstup bez medzikrokov." )
                    , ( "Reset", "Vrati simulaciu na zaciatok a nastavi aktualny stav spat na start." )
                    ]
                , viewGuideActionTable
                    "Co uvidis v grafe"
                    "Vizualne napovedy pocas simulacie."
                    [ ( "Aktualny stav", "Zvyrazni sa ringom okolo uzla, aby bolo vidno, kde sa automat prave nachadza." )
                    , ( "Posledny prechod", "Pouzita hrana aj jej label sa po kroku zvyraznia." )
                    , ( "Zaseknutie", "Ak pre aktualny stav a symbol chyba prechod, simulacia sa zastavi na danom mieste." )
                    , ( "DFA obmedzenie", "Krokovanie aj autoplay su urcene pre validny DFA." )
                    ]
                ]

        GuideConversionTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-code-branch" "Konverzia a algoritmy" "Panel Algoritmy meni aktualny automat priamo na platne, preto sa hodi na analyzu aj formalne overovanie." [ "NFA -> DFA", "Minimalizacia", "Komplement", "Produkt" ]
                , viewGuideActionTable
                    "Dostupne algoritmy"
                    "Kazdy vysledok prepise aktualne platno."
                    [ ( "NFA -> DFA", "Pouziva subset construction. Aktualna verzia nepodporuje epsilon prechody." )
                    , ( "Minimalizacia", "Odstrani nedosiahnutelne stavy, totalizuje automat a potom zluci ekvivalentne stavy." )
                    , ( "Komplement", "Doplni chybajuce prechody do sink stavu a invertuje accepting mnozinu." )
                    , ( "Zjednotenie a prienik", "Nacita druhy automat z JSON a spravi produktovu konstrukciu nad spolocnou abecedou." )
                    ]
                , viewGuideActionTable
                    "Formalne podmienky"
                    "Predpoklady pre korektny vysledok."
                    [ ( "Validny automat", "Start musi existovat, accepting stavy musia byt v states a prechody mozu odkazovat len na existujuce stavy." )
                    , ( "Deterministicky vstup", "Minimalizacia, komplement, zjednotenie, prienik aj simulacia ocakavaju validny DFA." )
                    , ( "Pouzita abeceda", "Pri produktovych operaciach sa pracuje so spolocnou abecedou oboch automatov." )
                    ]
                ]

        GuideDataTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-file-code" "JSON, import a export" "Automaty vies serializovat do JSON, nacitat zo suboru a exportovat aj samotny diagram." [ "JSON text", "JSON subor", "SVG", "PNG" ]
                , viewGuideActionTable
                    "Import / Export workflow"
                    "Vsetko, co potrebujes na prenos automatu."
                    [ ( "Zobrazit JSON v appke", "Vygeneruje serializovany vystup aktualneho automatu do textoveho pola." )
                    , ( "Stiahnut JSON subor", "Pripravi a stiahne JSON subor aktualneho automatu." )
                    , ( "Import z textu", "Vloz JSON do pola a klikni Importovat automat." )
                    , ( "Import zo suboru", "Vybrat JSON subor nacita obsah z disku do importneho pola." )
                    , ( "Druhy automat", "Pri A U B a A n B vies druhy automat vlozit ako text alebo vybrat ako subor." )
                    , ( "Export grafu", "Diagram vies ulozit ako SVG aj PNG." )
                    ]
                , viewGuideInfoGrid
                    "Co musi obsahovat JSON"
                    "Toto su casti, ktore appka cita pri importe."
                    [ ( "states", "Zoznam identifikatorov stavov ako 0, 1, 2..." )
                    , ( "alphabet", "Zoznam symbolov abecedy." )
                    , ( "transitions", "Pole prechodov tvaru { from, symbol, to }." )
                    , ( "start", "Jediny startovaci stav." )
                    , ( "accepting", "Zoznam akceptacnych stavov." )
                    , ( "positions", "Suradnice uzlov na platne." )
                    ]
                , viewGuideCodeBlock
                    "Priklad validneho JSON"
                    "{\n  \"states\": [0, 1],\n  \"alphabet\": [\"0\", \"1\"],\n  \"transitions\": [\n    { \"from\": 0, \"symbol\": \"0\", \"to\": 1 },\n    { \"from\": 0, \"symbol\": \"1\", \"to\": 0 }\n  ],\n  \"start\": 0,\n  \"accepting\": [1],\n  \"positions\": [\n    { \"state\": 0, \"x\": 240, \"y\": 180 },\n    { \"state\": 1, \"x\": 420, \"y\": 180 }\n  ]\n}"
                ]

        GuideErrorsTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-triangle-exclamation" "Chybove spravy a validacia" "Pred algoritmami aj importom aplikacia kontroluje, ci je automat formalne konzistentny." [ "Start", "Accepting", "Transitions", "Determinism" ]
                , viewGuideActionTable
                    "Validacne chyby"
                    "Problemy na urovni dat automatu."
                    [ ( "Chyba startovaci stav", "Automat nema nastaveny start a neda sa korektne spustit." )
                    , ( "Start nie je v states", "Start odkazuje na stav, ktory v mnozine states neexistuje." )
                    , ( "Akceptacne stavy mimo states", "Niektory stav v poli accepting sa nenachadza v states." )
                    , ( "Prechody na neexistujuce stavy", "Hrana odkazuje na stav, ktory nie je definovany." )
                    , ( "Symboly mimo abecedy", "JSON obsahuje symboly, ktore nie su uvedene v alphabet." )
                    ]
                , viewGuideActionTable
                    "Algoritmicke obmedzenia"
                    "Aj validny automat moze narazit na obmedzenie algoritmu."
                    [ ( "DFA only", "Simulacia slova, minimalizacia, komplement, zjednotenie a prienik su urcene pre validny DFA." )
                    , ( "Nedeterministicke prechody", "Ak z jedneho stavu vedu pre rovnaky symbol rozne ciele, treba najprv spustit NFA -> DFA." )
                    , ( "Epsilon prechody", "Subset construction v tejto verzii epsilon prechody nepodporuje." )
                    , ( "Neplatny JSON druheho automatu", "Pri mnozinovych operaciach musi byt aj druhy automat v korektnom JSON formate." )
                    ]
                ]

        GuideProjectTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-circle-info" "O projekte" "Projekt je napisany v Elm a pouziva funkcionalny model aktualizacii, co je dobre pre bezpecne experimentovanie s automatmi." [ "Elm", "History", "SVG graph", "JSON codec" ]
                , viewGuideInfoGrid
                    "Ako je appka poskladana"
                    "Strucny technicky prehlad."
                    [ ( "Datovy model", "Automat tvoria stavy, abeceda, prechody, start, accepting a positions." )
                    , ( "Historia zmien", "Editor uklada zmeny do undo/redo historie." )
                    , ( "Vizualizacia", "Graf sa kresli ako SVG s loopmi, obojsmernymi hranami a zoskupenymi labelmi." )
                    , ( "Abeceda", "Pri editacii sa alphabet vie automaticky doplnat o realne pouzite symboly." )
                    ]
                , viewGuideInfoGrid
                    "Aktualne limity"
                    "Dolezite poznamky pred odovzdanim."
                    [ ( "DFA operacie", "Simulacia, minimalizacia, komplement, zjednotenie a prienik ocakavaju validny DFA." )
                    , ( "NFA -> DFA", "Subset construction nepodporuje epsilon prechody." )
                    , ( "Prepis vysledku", "Algoritmy prepisu aktualny automat v editore, preto sa oplati vyuzivat undo/redo alebo export." )
                    , ( "Ciselne ID stavov", "Stavy su identifikovane cislami a novy stav dostane dalsie volne ID." )
                    ]
                ]


viewGuideSummaryCard : String -> String -> String -> List String -> Html Msg
viewGuideSummaryCard icon titleText descriptionText chips =
    div [ class "rounded-[28px] border border-amber-500/15 bg-gradient-to-br from-[#261b15] via-[#1a1411] to-[#120f0d] p-6 shadow-xl shadow-black/20" ]
        [ div [ class "flex flex-col gap-4 lg:flex-row lg:items-start lg:justify-between" ]
            [ div [ class "flex items-start gap-4" ]
                [ div [ class "flex h-14 w-14 shrink-0 items-center justify-center rounded-3xl bg-amber-500/12 text-amber-200 ring-1 ring-amber-500/20" ]
                    [ i [ class (icon ++ " text-xl") ] [] ]
                , div []
                    [ h3 [ class "text-2xl font-black text-[#f5ede3]" ] [ text titleText ]
                    , p [ class "mt-2 max-w-3xl text-sm leading-7 text-[#ccb49b]" ] [ text descriptionText ]
                    ]
                ]
            , div [ class "flex flex-wrap gap-2" ]
                (List.map viewGuideChip chips)
            ]
        ]


viewGuideChip : String -> Html Msg
viewGuideChip chipText =
    span [ class "rounded-full border border-[#5a4638] bg-[#1a1411]/85 px-3 py-1 text-[11px] font-semibold uppercase tracking-[0.16em] text-[#e2c7a9]" ]
        [ text chipText ]


viewGuideActionTable : String -> String -> List ( String, String ) -> Html Msg
viewGuideActionTable titleText subtitleText rows =
    div [ class "overflow-hidden rounded-[28px] border border-[#45352b] bg-[#16110f]/92 shadow-xl shadow-black/10" ]
        [ div [ class "border-b border-[#34271f] px-5 py-5" ]
            [ h3 [ class "text-lg font-bold text-[#f5ede3]" ] [ text titleText ]
            , p [ class "mt-1 text-sm leading-6 text-[#bca48d]" ] [ text subtitleText ]
            ]
        , div [ class "divide-y divide-[#2f241d]" ]
            (List.map viewGuideActionRow rows)
        ]


viewGuideActionRow : ( String, String ) -> Html Msg
viewGuideActionRow ( labelText, descriptionText ) =
    div [ class "grid gap-2 px-5 py-4 md:grid-cols-[240px,1fr] md:gap-6 md:px-6" ]
        [ div [ class "text-sm font-semibold text-[#f1deca]" ] [ text labelText ]
        , div [ class "text-sm leading-7 text-[#c9b29a]" ] [ text descriptionText ]
        ]


viewGuideInfoGrid : String -> String -> List ( String, String ) -> Html Msg
viewGuideInfoGrid titleText subtitleText entries =
    div [ class "rounded-[28px] border border-[#45352b] bg-[#16110f]/92 p-5 shadow-xl shadow-black/10" ]
        [ h3 [ class "text-lg font-bold text-[#f5ede3]" ] [ text titleText ]
        , p [ class "mt-1 text-sm leading-6 text-[#bca48d]" ] [ text subtitleText ]
        , div [ class "mt-4 grid gap-3 md:grid-cols-2" ]
            (List.map viewGuideInfoCard entries)
        ]


viewGuideInfoCard : ( String, String ) -> Html Msg
viewGuideInfoCard ( titleText, descriptionText ) =
    div [ class "rounded-3xl border border-[#3a2c23] bg-[#120f0d]/88 p-4" ]
        [ h4 [ class "text-sm font-semibold text-[#f5ede3]" ] [ text titleText ]
        , p [ class "mt-2 text-sm leading-7 text-[#c9b29a]" ] [ text descriptionText ]
        ]


viewGuideCodeBlock : String -> String -> Html Msg
viewGuideCodeBlock titleText codeText =
    div [ class "rounded-[28px] border border-[#45352b] bg-[#16110f]/92 p-5 shadow-xl shadow-black/10" ]
        [ h3 [ class "text-lg font-bold text-[#f5ede3]" ] [ text titleText ]
        , pre [ class "mt-4 overflow-x-auto rounded-3xl border border-[#3a2c23] bg-[#120f0d]/95 p-5 text-xs leading-7 text-[#f3e4d2]" ]
            [ code [] [ text codeText ] ]
        ]


viewTopStat : String -> String -> String -> String -> Html Msg
viewTopStat labelText valueText icon subtitleText =
    div [ class "rounded-3xl border border-[#45352b] bg-[#1a1411]/88 p-4" ]
        [ div [ class "flex items-start justify-between gap-4" ]
            [ div []
                [ div [ class "text-xs font-semibold uppercase tracking-[0.16em] text-[#bca48d]" ] [ text labelText ]
                , div [ class "mt-3 text-3xl font-black text-[#f5ede3]" ] [ text valueText ]
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
                "{}"

            else
                "{ " ++ String.join ", " alphabetList ++ " }"

        duplicateGroups =
            V.duplicateTransitionGroups automaton

        deterministic =
            V.isDeterministic automaton
    in
    div [ class "mt-6 rounded-[32px] border border-[#45352b] bg-[#1a1411]/88 p-5 shadow-2xl shadow-black/10" ]
        [ div [ class "mb-4" ]
            [ h3 [ class "text-xl font-bold text-[#f5ede3]" ] [ text "Statisticke udaje" ]
            , p [ class "mt-1 text-sm leading-6 text-[#bca48d]" ] [ text "Prehlad aktualneho automatu presunuty pod platno." ]
            ]
        , div [ class "grid gap-4 xl:grid-cols-5" ]
            [ viewTopStat "Pocet stavov" (String.fromInt (List.length automaton.states)) "fas fa-circle" "Zakladne uzly automatu"
            , viewTopStat "Pocet prechodov" (String.fromInt (List.length automaton.transitions)) "fas fa-random" "Vsetky definovane hrany"
            , viewTopStat "Pouzita abeceda" (String.fromInt (List.length alphabetList)) "fas fa-font" alphabetPreview
            , viewTopStat "Start" (Maybe.withDefault "-" (Maybe.map (\s -> "q" ++ String.fromInt s) automaton.start)) "fas fa-play" "Vstupny stav automatu"
            , viewTopStat "Rezim" (if deterministic then "DFA" else "NFA") "fas fa-code-branch" (if deterministic then "Bez duplicitnych hran pre rovnaky symbol" else "Nasli sa viacnasobne prechody pre rovnaky symbol")
            ]
        , div [ class "mt-4 rounded-3xl border border-[#45352b] bg-[#120f0d]/80 p-4" ]
            [ div [ class "flex items-center justify-between gap-3" ]
                [ div [ class "text-sm font-semibold text-[#f5ede3]" ] [ text "Kontrola deterministickosti" ]
                , span
                    [ class <|
                        "rounded-full border px-3 py-1 text-[11px] font-semibold uppercase tracking-[0.16em] "
                            ++ (if deterministic then
                                    "border-[#c48a4a]/30 bg-[#8c5d33]/20 text-[#f0d4ae]"

                                else
                                    "border-[#8a5938]/35 bg-[#5a3825]/25 text-[#d9baa0]"
                               )
                    ]
                    [ text (if deterministic then "Deterministicky" else "Nedeterministicky") ]
                ]
            , if deterministic then
                p [ class "mt-3 text-sm leading-6 text-[#bca48d]" ] [ text "Automat nema ziadny stav, z ktoreho by viedlo viac prechodov na rovnaky symbol." ]

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

        PickOtherJsonFile ->
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
