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
import Html.Events exposing (on, onClick, onInput)
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


displaySymbol : String -> String
displaySymbol symbol =
    if String.isEmpty symbol then
        "ε"

    else
        symbol


normalizeTypedSymbol : String -> String
normalizeTypedSymbol rawSymbol =
    let
        trimmed =
            String.trim rawSymbol

        lowered =
            String.toLower trimmed
    in
    if trimmed == "ε" || lowered == "eps" || lowered == "epsilon" then
        ""

    else
        trimmed


isExplicitEpsilonInput : String -> Bool
isExplicitEpsilonInput rawSymbol =
    let
        trimmed =
            String.trim rawSymbol

        lowered =
            String.toLower trimmed
    in
    trimmed == "ε" || lowered == "eps" || lowered == "epsilon"


type alias DragState =
    { dragging : Maybe A.StateId
    , offsetX : Float
    , offsetY : Float
    , original : Maybe A.Automaton
    , moved : Bool
    }


type alias GraphTransitionDraft =
    { from : A.StateId
    , to_ : Maybe A.StateId
    , symbol : String
    }


type ConsoleKind
    = ConsoleInfo
    | ConsoleSuccess
    | ConsoleWarning
    | ConsoleError
    | ConsoleAlgorithm


type alias ConsoleEntry =
    { id : Int
    , title : String
    , body : List String
    , kind : ConsoleKind
    }


type alias ConsoleResizeState =
    { startY : Float
    , startHeight : Float
    }


type alias CanvasView =
    { zoom : Float
    , panX : Float
    , panY : Float
    }


type alias CanvasPanState =
    { startX : Float
    , startY : Float
    , startPanX : Float
    , startPanY : Float
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
    , graphTransitionDraft : Maybe GraphTransitionDraft
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
    , canvasView : CanvasView
    , canvasPan : Maybe CanvasPanState
    , consoleEntries : List ConsoleEntry
    , nextConsoleId : Int
    , consoleHeight : Float
    , consoleResize : Maybe ConsoleResizeState
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


port saveAutomatonToLocalStorage : String -> Cmd msg


port savedAutomatonLoaded : (String -> msg) -> Sub msg


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


consoleLimit : Int
consoleLimit =
    80


graphBaseWidth : Float
graphBaseWidth =
    1220


graphBaseHeight : Float
graphBaseHeight =
    840


formatState : A.StateId -> String
formatState stateId =
    "q" ++ String.fromInt stateId


formatStateSet : List A.StateId -> String
formatStateSet states =
    let
        content =
            states
                |> List.sort
                |> List.map formatState
                |> String.join ", "
    in
    if String.isEmpty content then
        "{}"

    else
        "{ " ++ content ++ " }"


formatAlphabet : List String -> String
formatAlphabet alphabet =
    if List.isEmpty alphabet then
        "{}"

    else
        "{ " ++ String.join ", " alphabet ++ " }"


slovakCount : Int -> String -> String -> String -> String
slovakCount count one few many =
    String.fromInt count
        ++ " "
        ++ (if count == 1 then
                one

            else if count >= 2 && count <= 4 then
                few

            else
                many
           )


stateCountText : Int -> String
stateCountText count =
    slovakCount count "stav" "stavy" "stavov"


transitionCountText : Int -> String
transitionCountText count =
    slovakCount count "prechod" "prechody" "prechodov"


symbolCountText : Int -> String
symbolCountText count =
    slovakCount count "symbol" "symboly" "symbolov"


transitionLabel : A.Transition -> String
transitionLabel transition =
    formatState transition.from
        ++ " --"
        ++ displaySymbol transition.symbol
        ++ "--> "
        ++ formatState transition.to_


transitionByIndex : Int -> List A.Transition -> Maybe A.Transition
transitionByIndex idx transitions =
    transitions
        |> List.indexedMap Tuple.pair
        |> List.filter (\( currentIndex, _ ) -> currentIndex == idx)
        |> List.head
        |> Maybe.map Tuple.second


transitionKey : A.Transition -> String
transitionKey transition =
    String.fromInt transition.from
        ++ "|"
        ++ transition.symbol
        ++ "|"
        ++ String.fromInt transition.to_


transitionMultiset : List A.Transition -> Dict.Dict String ( Int, A.Transition )
transitionMultiset transitions =
    transitions
        |> List.foldl
            (\transition acc ->
                let
                    key =
                        transitionKey transition

                    nextCount =
                        case Dict.get key acc of
                            Just ( count, _ ) ->
                                count + 1

                            Nothing ->
                                1
                in
                Dict.insert key ( nextCount, transition ) acc
            )
            Dict.empty


transitionDiff : List A.Transition -> List A.Transition -> ( List A.Transition, List A.Transition )
transitionDiff beforeTransitions afterTransitions =
    let
        beforeMap =
            transitionMultiset beforeTransitions

        afterMap =
            transitionMultiset afterTransitions

        expandedPositiveDiff source other =
            source
                |> Dict.toList
                |> List.concatMap
                    (\( key, ( count, transition ) ) ->
                        let
                            otherCount =
                                other
                                    |> Dict.get key
                                    |> Maybe.map Tuple.first
                                    |> Maybe.withDefault 0

                            diffCount =
                                count - otherCount
                        in
                        if diffCount > 0 then
                            List.repeat diffCount transition

                        else
                            []
                    )
    in
    ( expandedPositiveDiff afterMap beforeMap
    , expandedPositiveDiff beforeMap afterMap
    )


newestAddedTransition : A.Automaton -> A.Automaton -> Maybe A.Transition
newestAddedTransition before after =
    if List.length after.transitions > List.length before.transitions then
        after.transitions
            |> List.reverse
            |> List.head

    else
        Nothing


viewBoxForCanvas : CanvasView -> String
viewBoxForCanvas canvasView =
    let
        width =
            graphBaseWidth / canvasView.zoom

        height =
            graphBaseHeight / canvasView.zoom
    in
    String.join " "
        [ String.fromFloat canvasView.panX
        , String.fromFloat canvasView.panY
        , String.fromFloat width
        , String.fromFloat height
        ]


zoomCanvasTo : Float -> CanvasView -> CanvasView
zoomCanvasTo requestedZoom canvasView =
    let
        nextZoom =
            clamp 0.45 2.6 requestedZoom

        currentWidth =
            graphBaseWidth / canvasView.zoom

        currentHeight =
            graphBaseHeight / canvasView.zoom

        nextWidth =
            graphBaseWidth / nextZoom

        nextHeight =
            graphBaseHeight / nextZoom

        centerX =
            canvasView.panX + currentWidth / 2

        centerY =
            canvasView.panY + currentHeight / 2
    in
    { zoom = nextZoom
    , panX = centerX - nextWidth / 2
    , panY = centerY - nextHeight / 2
    }


appendConsoleEntry : ConsoleKind -> String -> List String -> Model -> Model
appendConsoleEntry kind title body model =
    let
        entry =
            { id = model.nextConsoleId
            , title = title
            , body = body
            , kind = kind
            }
    in
    { model
        | consoleEntries = List.take consoleLimit (entry :: model.consoleEntries)
        , nextConsoleId = model.nextConsoleId + 1
    }


consoleTitleForMsg : Msg -> String
consoleTitleForMsg msg =
    case msg of
        Editor _ ->
            "Editor"

        SavedAutomatonLoaded _ ->
            "localStorage"

        Undo ->
            "Undo"

        Redo ->
            "Redo"

        CheckWord ->
            "Simulacia"

        ResetSimulation ->
            "Simulacia"

        StepSimulation ->
            "Simulacia"

        ToggleSimulationPlayback ->
            "Simulacia"

        AddTransitionClicked ->
            "Prechod"

        AddEpsilonTransitionClicked ->
            "Epsilon prechod"

        ConfirmGraphTransition ->
            "Prechod z grafu"

        ConfirmGraphEpsilonTransition ->
            "Epsilon z grafu"

        CancelGraphTransition ->
            "Graf"

        GlobalKeyPressed _ ->
            "Klavesnica"

        ExportJson ->
            "JSON export"

        DownloadJsonFile ->
            "JSON subor"

        ExportGraphSvg ->
            "SVG export"

        ExportGraphPng ->
            "PNG export"

        PickJsonFile ->
            "JSON import"

        PickOtherJsonFile ->
            "Druhy automat"

        JsonFileLoaded _ ->
            "JSON subor"

        ImportJson ->
            "JSON import"

        NfaToDfa ->
            "NFA -> DFA"

        MinimizeDfa ->
            "Minimalizacia"

        ComplementDfa ->
            "Komplement"

        UnionWithOther ->
            "Zjednotenie"

        IntersectWithOther ->
            "Prienik"

        GraphMsg Graph.EndDrag ->
            "Platno"

        GraphMsg _ ->
            "Graf"

        _ ->
            "Info"


consoleKindForMsg : Msg -> String -> ConsoleKind
consoleKindForMsg msg infoText =
    let
        lowered =
            String.toLower infoText
    in
    if String.contains "chyba" lowered || String.contains "error" lowered || String.contains "neplatny" lowered || String.contains "nie je mozne" lowered || String.contains "iba pre dfa" lowered then
        ConsoleError

    else
        case msg of
            NfaToDfa ->
                ConsoleAlgorithm

            MinimizeDfa ->
                ConsoleAlgorithm

            ComplementDfa ->
                ConsoleAlgorithm

            UnionWithOther ->
                ConsoleAlgorithm

            IntersectWithOther ->
                ConsoleAlgorithm

            ImportJson ->
                ConsoleSuccess

            DownloadJsonFile ->
                ConsoleSuccess

            ExportJson ->
                ConsoleSuccess

            ExportGraphSvg ->
                ConsoleSuccess

            ExportGraphPng ->
                ConsoleSuccess

            _ ->
                ConsoleInfo


nfaToDfaConsoleSteps : A.Automaton -> A.Automaton -> List String
nfaToDfaConsoleSteps before after =
    let
        beforeStates =
            List.length before.states

        beforeTransitions =
            List.length before.transitions

        afterStates =
            List.length after.states

        afterTransitions =
            List.length after.transitions

        epsilonCount =
            before.transitions
                |> List.filter (.symbol >> String.isEmpty)
                |> List.length

        epsilonLine =
            if epsilonCount == 0 then
                "3. Vstup neobsahoval epsilon prechody, takze epsilon-closure nemenila mnoziny stavov."

            else
                "3. Vstup obsahoval " ++ transitionCountText epsilonCount ++ " typu epsilon, preto sa pri kazdej mnozine dopocitala epsilon-closure."
    in
    [ "1. Vstupny automat mal " ++ stateCountText beforeStates ++ ", " ++ transitionCountText beforeTransitions ++ " a abecedu " ++ formatAlphabet (usedAlphabet before) ++ "."
    , "2. Startovacia mnozina DFA vznikla ako epsilon-closure startovacieho stavu."
    , epsilonLine
    , "4. Vysledny DFA ma " ++ stateCountText afterStates ++ ", " ++ transitionCountText afterTransitions ++ " a accepting stavy " ++ formatStateSet after.accepting ++ "."
    ]


minimizeConsoleSteps : A.Automaton -> A.Automaton -> List String
minimizeConsoleSteps before after =
    let
        beforeCount =
            List.length before.states

        afterCount =
            List.length after.states

        stateSummary =
            if beforeCount == afterCount then
                "4. Pocet stavov sa nezmenil: automat uz mal " ++ stateCountText afterCount ++ "."

            else if afterCount < beforeCount then
                "4. Pocet stavov sa zmensil z " ++ stateCountText beforeCount ++ " na " ++ stateCountText afterCount ++ "."

            else
                "4. Vysledok ma " ++ stateCountText afterCount ++ ", pretoze pred minimalizaciou bolo potrebne doplnit totalny DFA."
    in
    [ "1. Overil sa validny DFA nad abecedou " ++ formatAlphabet before.alphabet ++ "."
    , "2. Pred minimalizaciou mal automat " ++ stateCountText beforeCount ++ " a " ++ transitionCountText (List.length before.transitions) ++ "."
    , "3. Stavy sa rozdelili na triedy accepting / neaccepting a potom sa triedy spresnovali podla prechodov."
    , stateSummary
    , "5. Vysledok ma " ++ transitionCountText (List.length after.transitions) ++ " a accepting stavy " ++ formatStateSet after.accepting ++ "."
    ]


operationConsoleSteps : String -> A.Automaton -> A.Automaton -> List String
operationConsoleSteps label before after =
    [ "1. Spustena operacia: " ++ label ++ "."
    , "2. Vstupny automat A mal " ++ stateCountText (List.length before.states) ++ ", " ++ transitionCountText (List.length before.transitions) ++ " a abecedu " ++ formatAlphabet before.alphabet ++ "."
    , "3. Vysledny automat ma " ++ stateCountText (List.length after.states) ++ ", " ++ transitionCountText (List.length after.transitions) ++ " a abecedu " ++ formatAlphabet after.alphabet ++ "."
    , "4. Accepting stavy vysledku: " ++ formatStateSet after.accepting ++ "."
    ]


automatonChangeSummary : A.Automaton -> A.Automaton -> List String
automatonChangeSummary before after =
    let
        addedStates =
            after.states
                |> List.filter (\stateId -> not (List.member stateId before.states))
                |> List.sort

        removedStates =
            before.states
                |> List.filter (\stateId -> not (List.member stateId after.states))
                |> List.sort

        ( addedTransitions, removedTransitions ) =
            transitionDiff before.transitions after.transitions

        addedAccepting =
            after.accepting
                |> List.filter (\stateId -> not (List.member stateId before.accepting))
                |> List.sort

        removedAccepting =
            before.accepting
                |> List.filter (\stateId -> not (List.member stateId after.accepting))
                |> List.sort

        startLine =
            if before.start == after.start then
                []

            else
                [ "Zmeneny start: "
                    ++ Maybe.withDefault "-" (Maybe.map formatState before.start)
                    ++ " -> "
                    ++ Maybe.withDefault "-" (Maybe.map formatState after.start)
                    ++ "."
                ]

        stateLines =
            (if List.isEmpty addedStates then
                []

             else
                [ "Pridane stavy: " ++ formatStateSet addedStates ++ "." ]
            )
                ++ (if List.isEmpty removedStates then
                        []

                    else
                        [ "Odstranene stavy: " ++ formatStateSet removedStates ++ "." ]
                   )

        transitionLines =
            (if List.isEmpty addedTransitions then
                []

             else
                [ "Pridane prechody: " ++ String.join ", " (List.map transitionLabel addedTransitions) ++ "." ]
            )
                ++ (if List.isEmpty removedTransitions then
                        []

                    else
                        [ "Odstranene prechody: " ++ String.join ", " (List.map transitionLabel removedTransitions) ++ "." ]
                   )

        acceptingLines =
            (if List.isEmpty addedAccepting then
                []

             else
                [ "Pridane akceptacne stavy: " ++ formatStateSet addedAccepting ++ "." ]
            )
                ++ (if List.isEmpty removedAccepting then
                        []

                    else
                        [ "Odstranene akceptacne stavy: " ++ formatStateSet removedAccepting ++ "." ]
                   )
    in
    stateLines ++ transitionLines ++ startLine ++ acceptingLines


withChangeSummary : A.Automaton -> A.Automaton -> List String -> List String
withChangeSummary before after lines =
    let
        summary =
            automatonChangeSummary before after
    in
    if List.isEmpty summary then
        lines

    else
        lines ++ ("Zmeny automatu:" :: summary)


complementConsoleSteps : A.Automaton -> A.Automaton -> List String
complementConsoleSteps before after =
    [ "1. Vstupny DFA mal " ++ stateCountText (List.length before.states) ++ " a accepting stavy " ++ formatStateSet before.accepting ++ "."
    , "2. Automat sa najprv totalizoval nad abecedou " ++ formatAlphabet before.alphabet ++ ", aby bol komplement korektny."
    , "3. Accepting a neaccepting stavy sa prehodili."
    , "4. Vysledok ma accepting stavy " ++ formatStateSet after.accepting ++ " a " ++ transitionCountText (List.length after.transitions) ++ "."
    ]


addedTransitionConsoleSteps : A.Automaton -> A.Automaton -> String -> List String
addedTransitionConsoleSteps before after fallbackInfo =
    case newestAddedTransition before after of
        Just transition ->
            [ "Pridany prechod " ++ transitionLabel transition ++ "." ]

        Nothing ->
            [ fallbackInfo ]


editorConsoleSteps : Ed.Msg -> A.Automaton -> A.Automaton -> String -> List String
editorConsoleSteps editorMsg before after fallbackInfo =
    case editorMsg of
        Ed.AddState ->
            let
                addedStates =
                    after.states
                        |> List.filter (\stateId -> not (List.member stateId before.states))
            in
            case addedStates of
                stateId :: _ ->
                    [ "Pridany stav " ++ formatState stateId ++ "." ]

                [] ->
                    [ fallbackInfo ]

        Ed.RemoveState stateId ->
            [ "Odstraneny stav " ++ formatState stateId ++ " aj jeho suvisiace prechody." ]

        Ed.ToggleAccepting stateId ->
            if List.member stateId after.accepting then
                [ formatState stateId ++ " je teraz akceptacny stav." ]

            else
                [ formatState stateId ++ " uz nie je akceptacny stav." ]

        Ed.SetStart maybeState ->
            case maybeState of
                Just stateId ->
                    [ "Startovaci stav nastaveny na " ++ formatState stateId ++ "." ]

                Nothing ->
                    [ "Startovaci stav bol zruseny." ]

        Ed.AddTransition from symbol to_ ->
            addedTransitionConsoleSteps before after ("Pridany prechod " ++ transitionLabel { from = from, symbol = symbol, to_ = to_ } ++ ".")

        Ed.RemoveTransition idx ->
            case transitionByIndex idx before.transitions of
                Just transition ->
                    [ "Odstraneny prechod " ++ transitionLabel transition ++ "." ]

                Nothing ->
                    [ fallbackInfo ]

        Ed.MoveState stateId _ _ ->
            [ "Pozicia stavu " ++ formatState stateId ++ " bola ulozena." ]


simulationConsoleSteps : Simulation -> List String
simulationConsoleSteps simulation =
    let
        wordText =
            if List.isEmpty simulation.symbols then
                "ε"

            else
                String.join "" simulation.symbols

        stateText =
            simulation.currentState
                |> Maybe.map formatState
                |> Maybe.withDefault "-"
    in
    [ simulationStatusText simulation
    , "Vstup: " ++ wordText
    , "Koncovy stav: " ++ stateText
    , "Precitané symboly: " ++ String.fromInt simulation.currentIndex ++ " / " ++ String.fromInt (List.length simulation.symbols)
    ]


consoleBodyForMsg : Msg -> Model -> Model -> List String
consoleBodyForMsg msg previousModel updatedModel =
    let
        before =
            previousModel.history.present

        after =
            updatedModel.history.present
    in
    case msg of
        Editor editorMsg ->
            if automatonSignature before /= automatonSignature after then
                withChangeSummary before after (editorConsoleSteps editorMsg before after updatedModel.msgInfo)

            else
                [ updatedModel.msgInfo ]

        AddTransitionClicked ->
            withChangeSummary before after (addedTransitionConsoleSteps before after updatedModel.msgInfo)

        AddEpsilonTransitionClicked ->
            withChangeSummary before after (addedTransitionConsoleSteps before after updatedModel.msgInfo)

        ConfirmGraphTransition ->
            withChangeSummary before after (addedTransitionConsoleSteps before after updatedModel.msgInfo)

        ConfirmGraphEpsilonTransition ->
            withChangeSummary before after (addedTransitionConsoleSteps before after updatedModel.msgInfo)

        CheckWord ->
            if updatedModel.simulation.status == SimAccepted || updatedModel.simulation.status == SimRejected || updatedModel.simulation.status == SimStuck then
                simulationConsoleSteps updatedModel.simulation

            else
                [ updatedModel.msgInfo ]

        StepSimulation ->
            if updatedModel.simulation.status == SimAccepted || updatedModel.simulation.status == SimRejected || updatedModel.simulation.status == SimStuck then
                simulationConsoleSteps updatedModel.simulation

            else
                [ updatedModel.msgInfo ]

        SimulationTick _ ->
            if updatedModel.simulation.status == SimAccepted || updatedModel.simulation.status == SimRejected || updatedModel.simulation.status == SimStuck then
                simulationConsoleSteps updatedModel.simulation

            else
                [ updatedModel.msgInfo ]

        ToggleSimulationPlayback ->
            if previousModel.simulation.autoplay && not updatedModel.simulation.autoplay then
                simulationConsoleSteps updatedModel.simulation

            else
                [ updatedModel.msgInfo ]

        NfaToDfa ->
            if automatonSignature before /= automatonSignature after then
                withChangeSummary before after (nfaToDfaConsoleSteps before after)

            else
                [ updatedModel.msgInfo ]

        MinimizeDfa ->
            if automatonSignature before /= automatonSignature after then
                withChangeSummary before after (minimizeConsoleSteps before after)

            else
                [ updatedModel.msgInfo ]

        ComplementDfa ->
            if automatonSignature before /= automatonSignature after then
                withChangeSummary before after (complementConsoleSteps before after)

            else
                [ updatedModel.msgInfo ]

        UnionWithOther ->
            if automatonSignature before /= automatonSignature after then
                withChangeSummary before after (operationConsoleSteps "zjednotenie A U B" before after)

            else
                [ updatedModel.msgInfo ]

        IntersectWithOther ->
            if automatonSignature before /= automatonSignature after then
                withChangeSummary before after (operationConsoleSteps "prienik A n B" before after)

            else
                [ updatedModel.msgInfo ]

        _ ->
            [ updatedModel.msgInfo ]


shouldAppendConsole : Msg -> Model -> Model -> Bool
shouldAppendConsole msg previousModel updatedModel =
    let
        automatonChanged =
            automatonSignature previousModel.history.present /= automatonSignature updatedModel.history.present
    in
    case msg of
        FromChanged _ ->
            False

        SymChanged _ ->
            False

        ToChanged _ ->
            False

        ImportTextChanged _ ->
            False

        OtherJsonChanged _ ->
            False

        WordChanged _ ->
            False

        SetPlaybackSpeed _ ->
            False

        SimulationTick _ ->
            updatedModel.simulation.status == SimAccepted || updatedModel.simulation.status == SimRejected || updatedModel.simulation.status == SimStuck

        GraphTransitionSymbolChanged _ ->
            False

        GraphMsg (Graph.Drag _ _) ->
            False

        GraphMsg (Graph.Pan _ _) ->
            False

        GraphMsg (Graph.StartPan _ _) ->
            False

        GraphMsg Graph.EndPan ->
            False

        GraphMsg (Graph.Wheel _) ->
            False

        StartConsoleResize _ ->
            False

        ConsoleResize _ ->
            False

        EndConsoleResize ->
            False

        ClearConsole ->
            False

        CanvasZoomIn ->
            False

        CanvasZoomOut ->
            False

        ResetCanvasView ->
            False

        SelectTab _ ->
            False

        SelectEditorSubTab _ ->
            False

        SelectAlgorithmsSubTab _ ->
            False

        SelectGuideTab _ ->
            False

        ToggleGuide ->
            False

        CloseGuide ->
            False

        CloseGuideAndSelectTab _ ->
            False

        GraphMsg Graph.NoOp ->
            False

        _ ->
            automatonChanged || (previousModel.msgInfo /= updatedModel.msgInfo && not (String.isEmpty (String.trim updatedModel.msgInfo)))


appendConsoleForMessage : Msg -> Model -> Model -> Model
appendConsoleForMessage msg previousModel updatedModel =
    if shouldAppendConsole msg previousModel updatedModel then
        appendConsoleEntry
            (consoleKindForMsg msg updatedModel.msgInfo)
            (consoleTitleForMsg msg)
            (consoleBodyForMsg msg previousModel updatedModel)
            updatedModel

    else
        updatedModel


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
        , pendingSource = model.graphTransitionDraft |> Maybe.map .from
        , pendingTarget = model.graphTransitionDraft |> Maybe.andThen .to_
        }

    else
        { currentState = Nothing
        , activeTransition = Nothing
        , pendingSource = model.graphTransitionDraft |> Maybe.map .from
        , pendingTarget = model.graphTransitionDraft |> Maybe.andThen .to_
        }


startGraphTransition : A.StateId -> Model -> Model
startGraphTransition stateId model =
    { model
        | graphTransitionDraft = Just { from = stateId, to_ = Nothing, symbol = "" }
        , msgInfo = "Vybrany zdrojovy stav q" ++ String.fromInt stateId ++ ". Klikni na cielovy stav."
    }


commitTransition : A.StateId -> String -> A.StateId -> Model -> Model
commitTransition from symbol to_ model =
    update
        (Editor (Ed.AddTransition from symbol to_))
        { model
            | graphTransitionDraft = Nothing
            , fromSel = String.fromInt from
            , toSel = String.fromInt to_
            , symSel = ""
            , selectedTab = EditorTab
            , editorSubTab = TransitionFormSub
        }


automatonSignature : A.Automaton -> String
automatonSignature automaton =
    exportJsonString automaton


shouldPersistAutomaton : Msg -> Model -> Model -> Bool
shouldPersistAutomaton msg previousModel updatedModel =
    case msg of
        GraphMsg (Graph.Drag _ _) ->
            False

        GraphMsg Graph.EndDrag ->
            previousModel.dragState.moved

        SavedAutomatonLoaded _ ->
            False

        _ ->
            automatonSignature previousModel.history.present /= automatonSignature updatedModel.history.present


persistAutomatonCmd : Msg -> Model -> Model -> Cmd Msg
persistAutomatonCmd msg previousModel updatedModel =
    if shouldPersistAutomaton msg previousModel updatedModel then
        saveAutomatonToLocalStorage (exportJsonString updatedModel.history.present)

    else
        Cmd.none


handleGraphStateClick : A.StateId -> Model -> Model
handleGraphStateClick stateId model =
    case model.graphTransitionDraft of
        Nothing ->
            startGraphTransition stateId model

        Just draft ->
            case draft.to_ of
                Nothing ->
                    { model
                        | graphTransitionDraft =
                            Just
                                { draft
                                    | to_ = Just stateId
                                    , symbol = model.symSel
                                }
                        , fromSel = String.fromInt draft.from
                        , toSel = String.fromInt stateId
                        , selectedTab = EditorTab
                        , editorSubTab = TransitionFormSub
                        , msgInfo =
                            "Vybrany cielovy stav q"
                                ++ String.fromInt stateId
                                ++ ". Zadaj symbol alebo pouzi ε prechod."
                    }

                Just _ ->
                    startGraphTransition stateId model


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
            "Prakticky navod na budovanie stavov, prechodov, epsilon hran, zoomu a posunu platna."

        GuideSimulationTab ->
            "Ako funguje krokovanie slova, prehravanie a vizualne zvyraznenie automatu pri validnom DFA."

        GuideConversionTab ->
            ""

        GuideDataTab ->
            "Import, export, JSON format, localStorage a praca s druhym automatom pri mnozinovych operaciach."

        GuideErrorsTab ->
            "Najcastejsie validacne problemy a co presne znamenaju pri tvorbe alebo importe automatu."

        GuideProjectTab ->
            "Ako je appka poskladana, co uklada automaticky, ako funguje konzola a ake ma aktualne limity."


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

        welcomeText =
            "Vitaj v editore. Mozes pridavat stavy, prechody a okamzite testovat slova."
    in
    { history = Ed.initHistory initialAutomaton
    , inputWord = ""
    , simulation = resetSimulation initialAutomaton ""
    , playbackSpeed = Normal
    , guideOpen = False
    , guideTab = GuideEditorTab
    , jsonFileTarget = MainImportFile
    , graphTransitionDraft = Nothing
    , msgInfo = welcomeText
    , fromSel = "0"
    , symSel = ""
    , toSel = "0"
    , importText = ""
    , exportText = ""
    , otherText = ""
    , selectedTab = EditorTab
    , editorSubTab = StatesSub
    , algorithmsSubTab = AlgoBasicSub
    , dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing, moved = False }
    , canvasView = { zoom = 1, panX = 0, panY = 0 }
    , canvasPan = Nothing
    , consoleEntries =
        [ { id = 0
          , title = "Konzola pripravena"
          , body = [ welcomeText, "Hlasenia, chyby a kroky algoritmov sa budu vypisovat sem." ]
          , kind = ConsoleInfo
          }
        ]
    , nextConsoleId = 1
    , consoleHeight = 220
    , consoleResize = Nothing
    }


type Msg
    = Editor Ed.Msg
    | SavedAutomatonLoaded String
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
    | AddEpsilonTransitionClicked
    | GraphTransitionSymbolChanged String
    | ConfirmGraphTransition
    | ConfirmGraphEpsilonTransition
    | CancelGraphTransition
    | GlobalKeyPressed String
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
    | CanvasZoomIn
    | CanvasZoomOut
    | ResetCanvasView
    | StartConsoleResize Float
    | ConsoleResize Float
    | EndConsoleResize
    | ClearConsole
    | GraphMsg Graph.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        SavedAutomatonLoaded rawJson ->
            if String.isEmpty (String.trim rawJson) then
                model

            else
                case D.decodeString Codec.decode rawJson of
                    Ok restoredAutomaton ->
                        let
                            enrichedAutomaton =
                                ensureAlphabetCoverage restoredAutomaton
                        in
                        { model
                            | history = Ed.initHistory enrichedAutomaton
                            , simulation = resetSimulation enrichedAutomaton model.inputWord
                            , graphTransitionDraft = Nothing
                            , msgInfo = "Obnoveny posledny ulozeny automat z localStorage."
                        }

                    Err _ ->
                        { model | msgInfo = "Ulozeny automat v localStorage sa nepodarilo obnovit." }

        SelectTab tab ->
            { model | selectedTab = tab }

        SelectEditorSubTab subTab ->
            { model | editorSubTab = subTab }

        SelectAlgorithmsSubTab subTab ->
            { model | algorithmsSubTab = subTab }

        CanvasZoomIn ->
            { model | canvasView = zoomCanvasTo (model.canvasView.zoom * 1.18) model.canvasView }

        CanvasZoomOut ->
            { model | canvasView = zoomCanvasTo (model.canvasView.zoom / 1.18) model.canvasView }

        ResetCanvasView ->
            { model | canvasView = { zoom = 1, panX = 0, panY = 0 }, canvasPan = Nothing }

        StartConsoleResize clientY ->
            { model | consoleResize = Just { startY = clientY, startHeight = model.consoleHeight } }

        ConsoleResize clientY ->
            case model.consoleResize of
                Just resizeState ->
                    let
                        nextHeight =
                            resizeState.startHeight - (clientY - resizeState.startY)
                    in
                    { model | consoleHeight = clamp 120 460 nextHeight }

                Nothing ->
                    model

        EndConsoleResize ->
            { model | consoleResize = Nothing }

        ClearConsole ->
            { model | consoleEntries = [] }

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
                , graphTransitionDraft = Nothing
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
                , graphTransitionDraft = Nothing
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
                , graphTransitionDraft = Nothing
                , msgInfo = "Obnovena dalsia zmena."
            }

        WordChanged s ->
            { model
                | inputWord = s
                , simulation = resetSimulation model.history.present s
                , msgInfo = "Paska je pripravena na krokovanie."
            }

        CheckWord ->
            case guardValidDeterministic "Simulacia slova" model.history.present model of
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
            case guardValidDeterministic "Krokovanie simulacie" model.history.present model of
                Just guardedModel ->
                    guardedModel

                Nothing ->
                    let
                        nextSimulation =
                            stepSimulation model.history.present model.simulation
                    in
                    { model | simulation = nextSimulation, msgInfo = simulationStatusText nextSimulation }

        ToggleSimulationPlayback ->
            case guardValidDeterministic "Automaticke prehravanie" model.history.present model of
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

                normalizedSymbol =
                    normalizeTypedSymbol model.symSel

                explicitEpsilon =
                    isExplicitEpsilonInput model.symSel
            in
            case ( mf, mt, normalizedSymbol ) of
                ( Just f, Just t, sym ) ->
                    if String.isEmpty sym && not explicitEpsilon then
                        { model | msgInfo = "Zadaj symbol prechodu." }

                    else
                        commitTransition f sym t model

                _ ->
                    { model | msgInfo = "Vypln From, To a Symbol." }

        AddEpsilonTransitionClicked ->
            let
                mf =
                    String.toInt model.fromSel

                mt =
                    String.toInt model.toSel
            in
            case ( mf, mt ) of
                ( Just f, Just t ) ->
                    commitTransition f "" t model

                _ ->
                    { model | msgInfo = "Vypln From a To pre ε prechod." }

        GraphTransitionSymbolChanged symbol ->
            { model
                | graphTransitionDraft =
                    model.graphTransitionDraft
                        |> Maybe.map (\draft -> { draft | symbol = symbol })
            }

        ConfirmGraphTransition ->
            case model.graphTransitionDraft of
                Just draft ->
                    case draft.to_ of
                        Just targetState ->
                            let
                                normalizedSymbol =
                                    normalizeTypedSymbol draft.symbol

                                explicitEpsilon =
                                    isExplicitEpsilonInput draft.symbol
                            in
                            if String.isEmpty normalizedSymbol && not explicitEpsilon then
                                { model | msgInfo = "Zadaj symbol alebo pouzi ε prechod." }

                            else
                                commitTransition draft.from normalizedSymbol targetState { model | symSel = draft.symbol }

                        Nothing ->
                            { model | msgInfo = "Najprv klikni na cielovy stav v grafe." }

                Nothing ->
                    model

        ConfirmGraphEpsilonTransition ->
            case model.graphTransitionDraft of
                Just draft ->
                    case draft.to_ of
                        Just targetState ->
                            commitTransition draft.from "" targetState model

                        Nothing ->
                            { model | msgInfo = "Najprv klikni na cielovy stav v grafe." }

                Nothing ->
                    model

        CancelGraphTransition ->
            { model | graphTransitionDraft = Nothing, msgInfo = "Priama tvorba prechodu bola zrusena." }

        GlobalKeyPressed key ->
            if key == "Escape" && model.graphTransitionDraft /= Nothing then
                { model | graphTransitionDraft = Nothing, msgInfo = "Priama tvorba prechodu bola zrusena cez Esc." }

            else
                model

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
                            , graphTransitionDraft = Nothing
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
                        , graphTransitionDraft = Nothing
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
                        , graphTransitionDraft = Nothing
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
                        , graphTransitionDraft = Nothing
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
                                        , graphTransitionDraft = Nothing
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
                                        , graphTransitionDraft = Nothing
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
                            , moved = False
                            }
                    }

                Graph.Drag clientX clientY ->
                    case ( model.dragState.dragging, model.dragState.original ) of
                        ( Just stateId, Just originalAutomaton ) ->
                            let
                                originalPosition =
                                    Dict.get stateId originalAutomaton.positions
                                        |> Maybe.withDefault { x = 100, y = 100 }

                                startClientX =
                                    originalPosition.x + model.dragState.offsetX

                                startClientY =
                                    originalPosition.y + model.dragState.offsetY

                                newX =
                                    clamp 48 1172 (originalPosition.x + (clientX - startClientX) / model.canvasView.zoom)

                                newY =
                                    clamp 48 792 (originalPosition.y + (clientY - startClientY) / model.canvasView.zoom)

                                movedEnough =
                                    model.dragState.moved
                                        || abs (newX - originalPosition.x) > 5
                                        || abs (newY - originalPosition.y) > 5
                            in
                            if movedEnough then
                                let
                                    updatedAutomaton =
                                        model.history.present
                                            |> Ed.apply (Ed.MoveState stateId newX newY)

                                    updatedHistory =
                                        { past = model.history.past
                                        , present = updatedAutomaton
                                        , future = model.history.future
                                        }
                                in
                                { model
                                    | history = updatedHistory
                                    , dragState =
                                        { dragging = model.dragState.dragging
                                        , offsetX = model.dragState.offsetX
                                        , offsetY = model.dragState.offsetY
                                        , original = model.dragState.original
                                        , moved = True
                                        }
                                    , msgInfo = "Presuvaj stav mysou priamo na platne."
                                }

                            else
                                model

                        ( Nothing, _ ) ->
                            model

                        _ ->
                            model

                Graph.EndDrag ->
                    case ( model.dragState.dragging, model.dragState.original, model.dragState.moved ) of
                        ( Just stateId, _, False ) ->
                            handleGraphStateClick stateId
                                { model | dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing, moved = False } }

                        ( _, Just oldAutomaton, True ) ->
                            { model
                                | history =
                                    { past = oldAutomaton :: model.history.past
                                    , present = model.history.present
                                    , future = []
                                    }
                                , dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing, moved = False }
                                , msgInfo = "Pozicia stavu bola ulozena."
                            }

                        _ ->
                            { model | dragState = { dragging = Nothing, offsetX = 0, offsetY = 0, original = Nothing, moved = False } }

                Graph.NoOp ->
                    model

                Graph.StartPan clientX clientY ->
                    { model
                        | canvasPan =
                            Just
                                { startX = clientX
                                , startY = clientY
                                , startPanX = model.canvasView.panX
                                , startPanY = model.canvasView.panY
                                }
                    }

                Graph.Pan clientX clientY ->
                    case model.canvasPan of
                        Just panState ->
                            { model
                                | canvasView =
                                    { zoom = model.canvasView.zoom
                                    , panX = panState.startPanX - (clientX - panState.startX) / model.canvasView.zoom
                                    , panY = panState.startPanY - (clientY - panState.startY) / model.canvasView.zoom
                                    }
                            }

                        Nothing ->
                            model

                Graph.EndPan ->
                    { model | canvasPan = Nothing }

                Graph.Wheel deltaY ->
                    if deltaY < 0 then
                        { model | canvasView = zoomCanvasTo (model.canvasView.zoom * 1.12) model.canvasView }

                    else
                        { model | canvasView = zoomCanvasTo (model.canvasView.zoom / 1.12) model.canvasView }


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
    div [ class "h-screen overflow-hidden bg-[#120f0d] text-[#f5ede3]" ]
        [ div [ class "flex h-screen overflow-hidden" ]
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
    in
    div [ class "flex h-full w-[400px] shrink-0 flex-col overflow-hidden border-r border-[#3a2c23] bg-[#15110f]/95 backdrop-blur-xl" ]
        [ div [ class "px-4 pt-4" ]
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
                    "Definuj smer a symbol. Prechody sa okamzite vykreslia do grafu, epsilon zadas ako ε alebo samostatnym tlacidlom."
                    [ div [ class "grid grid-cols-1 gap-3" ]
                        [ viewSelectField "Z (From)" model.fromSel automaton.states FromChanged
                        , viewInputField "Symbol" model.symSel "napr. 0, a, x alebo ε" SymChanged
                        , viewSelectField "Do (To)" model.toSel automaton.states ToChanged
                        , div [ class "grid grid-cols-1 gap-3 md:grid-cols-2" ]
                            [ button
                                [ class "w-full rounded-2xl bg-[#a86434] px-4 py-3 text-sm font-semibold text-[#f7ead9] transition hover:bg-[#b77745]"
                                , onClick AddTransitionClicked
                                ]
                                [ i [ class "fas fa-plus mr-2" ] []
                                , text "Pridat prechod"
                                ]
                            , button
                                [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                                , onClick AddEpsilonTransitionClicked
                                ]
                                [ text "Pridat ε prechod" ]
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
            , span [ class "rounded-full border border-amber-500/20 bg-amber-500/10 px-3 py-1 font-mono text-xs font-bold text-amber-100" ] [ text (displaySymbol transition.symbol) ]
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

        cellDisplaySymbol =
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
        [ text cellDisplaySymbol ]


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
    div [ class "flex h-full min-w-0 flex-1 flex-col overflow-hidden bg-[#120f0d]" ]
        [ div [ class "shrink-0 border-b border-[#2b211b] bg-[#15110f]/95 px-6 py-4" ]
            [ div [ class "flex flex-col gap-4 xl:flex-row xl:items-center xl:justify-between" ]
                [ div []
                    [ h3 [ class "text-xl font-bold text-[#f5ede3]" ] [ text "Platno automatu" ] ]
                , div [ class "flex flex-wrap items-center gap-3" ]
                    ([ viewCanvasControlButton "fas fa-minus" "Oddialit" CanvasZoomOut
                     , div [ class "rounded-2xl border border-[#45352b] bg-[#211914] px-4 py-3 text-sm font-semibold text-[#eadbcf]" ]
                        [ text (String.fromInt (round (model.canvasView.zoom * 100)) ++ "%") ]
                     , viewCanvasControlButton "fas fa-plus" "Priblizit" CanvasZoomIn
                     , viewCanvasControlButton "fas fa-crosshairs" "Reset pohlad" ResetCanvasView
                     , viewToolbarButton "fas fa-book-open" "Guide" "bg-gradient-to-r from-[#f59e0b] to-[#c26a2d] text-[#1b120e] hover:brightness-110" ToggleGuide
                     , viewToolbarButton "fas fa-undo" "Undo" "bg-[#2a201a] text-[#f5ede3] hover:bg-[#3a2c23]" Undo
                     , viewToolbarButton "fas fa-redo" "Redo" "bg-[#2a201a] text-[#f5ede3] hover:bg-[#3a2c23]" Redo
                     ]
                        ++ [ viewModeBadge automaton ]
                    )
                ]
            ]
        , div [ class "relative min-h-0 flex-1 overflow-hidden p-5" ]
            [ div [ class "flex h-full min-h-0 flex-col" ]
                [ case model.graphTransitionDraft of
                    Just draft ->
                        viewGraphTransitionComposer draft

                    Nothing ->
                        text ""
                , viewGraphWorkspace model automaton
                ]
            , viewConsolePanel model
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


viewCanvasControlButton : String -> String -> Msg -> Html Msg
viewCanvasControlButton icon titleText buttonMsg =
    button
        [ class "inline-flex h-12 w-12 items-center justify-center rounded-2xl border border-[#3a2c23] bg-[#211914] text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
        , onClick buttonMsg
        , title titleText
        ]
        [ i [ class icon ] [] ]


viewModeBadge : A.Automaton -> Html Msg
viewModeBadge automaton =
    let
        deterministic =
            V.isDeterministic automaton
    in
    div
        [ class <|
            "rounded-2xl border px-4 py-3 text-sm font-bold "
                ++ (if deterministic then
                        "border-emerald-500/25 bg-emerald-500/10 text-emerald-200"

                    else
                        "border-amber-500/25 bg-amber-500/10 text-amber-100"
                   )
        ]
        [ text (if deterministic then "DFA" else "NFA") ]


viewGraphWorkspace : Model -> A.Automaton -> Html Msg
viewGraphWorkspace model automaton =
    div [ class "min-h-0 flex-1 overflow-hidden rounded-[32px] border border-[#3a2c23] bg-[#1a1512]/90 p-4 shadow-2xl shadow-black/20" ]
        [ if List.isEmpty automaton.states then
            div [ class "grid h-full place-items-center rounded-[28px] border border-dashed border-[#4b392d] bg-[#16110f]/70 text-center" ]
                [ div [ class "max-w-md px-6" ]
                    [ div [ class "mx-auto flex h-16 w-16 items-center justify-center rounded-3xl bg-amber-500/10 text-amber-300 ring-1 ring-amber-500/20" ]
                        [ i [ class "fas fa-project-diagram text-2xl" ] [] ]
                    , h4 [ class "mt-5 text-2xl font-bold text-[#f5ede3]" ] [ text "Platno je pripravene" ]
                    , p [ class "mt-3 text-sm leading-7 text-[#bca48d]" ] [ text "Zacni pridanim prveho stavu v lavom paneli. Potom dopln prechody a spusti simulaciu alebo algoritmy." ]
                    ]
                ]

          else
            div
                [ class "graph-shell relative h-full overflow-hidden rounded-[28px] border border-[#3a2c23] bg-[#171311]/50"
                , title "Tahaj prazdne miesto na platne pre posun pohladu."
                ]
                [ Html.map GraphMsg (Graph.view (viewBoxForCanvas model.canvasView) (graphHighlight model) automaton)
                , div [ class "pointer-events-none absolute bottom-4 left-4 rounded-2xl border border-[#45352b] bg-[#120f0d]/85 px-4 py-2 text-xs font-semibold text-[#c9b29a] shadow-lg shadow-black/20" ]
                    [ text "Tahaj prazdne miesto pre posun, koliesko alebo tlacidla hore menia zoom." ]
                ]
        ]


viewConsolePanel : Model -> Html Msg
viewConsolePanel model =
    div
        [ class "absolute bottom-0 left-0 right-0 z-30 border-t border-[#2b211b] bg-[#0d0a09]/96 shadow-[0_-18px_50px_rgba(0,0,0,0.42)] backdrop-blur-sm"
        , style "height" (String.fromFloat model.consoleHeight ++ "px")
        ]
        [ div
            [ class "flex h-full min-h-0 flex-col" ]
            [ div
                [ class "group flex h-3 shrink-0 cursor-row-resize items-center justify-center bg-[#15110f] hover:bg-[#211914]"
                , on "mousedown" (D.field "clientY" D.float |> D.map StartConsoleResize)
                , title "Tahaj hore alebo dole pre zmenu vysky konzoly"
                ]
                [ div [ class "h-1 w-20 rounded-full bg-[#4a392f] transition group-hover:bg-amber-400/70" ] [] ]
            , div [ class "flex shrink-0 items-center justify-between border-b border-[#2b211b] px-5 py-3" ]
                [ div []
                    [ h3 [ class "text-sm font-bold uppercase tracking-[0.16em] text-[#f5ede3]" ] [ text "Konzola" ]
                    , p [ class "mt-1 text-xs text-[#9f8670]" ] [ text "Hlasenia aplikacie a postup algoritmov." ]
                    ]
                , button
                    [ class "rounded-xl border border-[#3a2c23] bg-[#171210] px-3 py-2 text-xs font-semibold text-[#d8c1aa] transition hover:border-amber-400 hover:text-[#f7ead9]"
                    , onClick ClearConsole
                    ]
                    [ text "Vycistit" ]
                ]
            , div [ class "min-h-0 flex-1 overflow-y-auto px-5 py-4 font-mono text-xs leading-6 scrollbar-thin" ]
                (if List.isEmpty model.consoleEntries then
                    [ div [ class "rounded-2xl border border-dashed border-[#3a2c23] px-4 py-6 text-center text-[#8f7663]" ]
                        [ text "Konzola je prazdna." ]
                    ]

                 else
                    List.map viewConsoleEntry model.consoleEntries
                )
            ]
        ]


viewConsoleEntry : ConsoleEntry -> Html Msg
viewConsoleEntry entry =
    div [ class ("mb-3 rounded-2xl border px-4 py-3 " ++ consoleEntryClass entry.kind) ]
        [ div [ class "mb-2 flex items-center justify-between gap-3" ]
            [ div [ class "font-sans text-sm font-bold" ] [ text entry.title ]
            , span [ class "rounded-full border border-white/10 px-2 py-0.5 text-[10px] uppercase tracking-[0.14em] opacity-80" ]
                [ text (consoleKindLabel entry.kind) ]
            ]
        , div [ class "space-y-1 text-[#e9d6c2]" ]
            (entry.body |> List.map (\line -> div [] [ text line ]))
        ]


consoleEntryClass : ConsoleKind -> String
consoleEntryClass kind =
    case kind of
        ConsoleInfo ->
            "border-[#3a2c23] bg-[#15110f]"

        ConsoleSuccess ->
            "border-emerald-500/25 bg-emerald-500/10"

        ConsoleWarning ->
            "border-amber-500/25 bg-amber-500/10"

        ConsoleError ->
            "border-rose-500/30 bg-rose-500/10"

        ConsoleAlgorithm ->
            "border-blue-500/25 bg-blue-500/10"


consoleKindLabel : ConsoleKind -> String
consoleKindLabel kind =
    case kind of
        ConsoleInfo ->
            "info"

        ConsoleSuccess ->
            "ok"

        ConsoleWarning ->
            "pozor"

        ConsoleError ->
            "chyba"

        ConsoleAlgorithm ->
            "algoritmus"


viewGraphTransitionComposer : GraphTransitionDraft -> Html Msg
viewGraphTransitionComposer draft =
    let
        sourceLabel =
            "q" ++ String.fromInt draft.from
    in
    case draft.to_ of
        Nothing ->
            div [ class "mb-4 rounded-[28px] border border-amber-500/20 bg-[#16110f]/90 p-4 shadow-lg shadow-black/10" ]
                [ div [ class "flex flex-col gap-3 lg:flex-row lg:items-center lg:justify-between" ]
                    [ div []
                        [ div [ class "text-sm font-semibold text-[#f5ede3]" ] [ text ("Priama tvorba prechodu zo stavu " ++ sourceLabel) ]
                        , p [ class "mt-1 text-sm leading-6 text-[#c9b29a]" ] [ text "Zdrojovy stav je vybrany. Klikni v grafe na cielovy stav a potom zadaj symbol alebo epsilon." ]
                        ]
                    , button
                        [ class "inline-flex items-center gap-2 rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                        , onClick CancelGraphTransition
                        ]
                        [ i [ class "fas fa-xmark" ] []
                        , text "Zrusit"
                        ]
                    ]
                ]

        Just targetState ->
            let
                targetLabel =
                    "q" ++ String.fromInt targetState
            in
            div [ class "mb-4 rounded-[28px] border border-amber-500/20 bg-[#16110f]/90 p-4 shadow-lg shadow-black/10" ]
                [ div [ class "mb-4 flex flex-col gap-2 lg:flex-row lg:items-center lg:justify-between" ]
                    [ div []
                        [ div [ class "text-sm font-semibold text-[#f5ede3]" ] [ text ("Novy prechod " ++ sourceLabel ++ " -> " ++ targetLabel) ]
                        , p [ class "mt-1 text-sm leading-6 text-[#c9b29a]" ] [ text "Zadaj symbol prechodu. Ak chces epsilon prechod, napis epsilon alebo pouzi samostatne tlacidlo." ]
                        ]
                    , button
                        [ class "inline-flex items-center gap-2 rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                        , onClick CancelGraphTransition
                        ]
                        [ i [ class "fas fa-xmark" ] []
                        , text "Zrusit"
                        ]
                    ]
                , div [ class "grid grid-cols-1 gap-3 lg:grid-cols-[minmax(0,1fr),auto,auto]" ]
                    [ input
                        [ class "w-full rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm text-[#f5ede3] outline-none transition placeholder:text-[#7f6756] focus:border-amber-400"
                        , value draft.symbol
                        , onInput GraphTransitionSymbolChanged
                        , placeholder "napr. 0, a, x alebo epsilon"
                        ]
                        []
                    , button
                        [ class "rounded-2xl bg-[#a86434] px-4 py-3 text-sm font-semibold text-[#f7ead9] transition hover:bg-[#b77745]"
                        , onClick ConfirmGraphTransition
                        ]
                        [ i [ class "fas fa-plus mr-2" ] []
                        , text "Pridat prechod"
                        ]
                    , button
                        [ class "rounded-2xl border border-[#4a392f] bg-[#120f0d] px-4 py-3 text-sm font-semibold text-[#eadbcf] transition hover:border-amber-400 hover:text-[#f7ead9]"
                        , onClick ConfirmGraphEpsilonTransition
                        ]
                        [ text "epsilon prechod" ]
                    ]
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
                [ viewGuideSummaryCard "fas fa-pen-ruler" "Editor automatov" "Panel Editor je urceny na tvorbu stavov, beznych aj epsilon prechodov a upravu grafu s okamzitou vizualnou odozvou." [ "Stavy", "Prechody", "epsilon", "Undo / Redo" ]
                , viewGuideActionTable
                    "Praca so stavmi"
                    "Zakladne akcie pre stavovy diagram."
                    [ ( "Pridanie stavu", "V podkarte Stavy klikni na Pridat stav. Appka vytvori dalsie volne ID a vlozi novy uzol na platno." )
                    , ( "Startovaci stav", "Hviezdicka nastavi vybrany stav ako jediny start automatu." )
                    , ( "Akceptacny stav", "Fajka prepina akceptacny stav. V grafe ho spoznas podla dvojitej kruznice." )
                    , ( "Odstranenie stavu", "Kos odstrani stav aj vsetky prechody, ktore do neho vedu alebo z neho vychadzaju." )
                    , ( "Presun na platne", "Stav mozes chytit mysou priamo v grafe. Nova pozicia sa po pusteni ulozi do historie a do localStorage." )
                    , ( "Zoom a posun platna", "Tlacidlami nad grafom alebo kolieskom mysi vies platno priblizit a oddialit. Prazdne miesto na platne vies tahat mysou." )
                    ]
                , viewGuideActionTable
                    "Praca s prechodmi"
                    "Ako definovat jazyk automatu v editore."
                    [ ( "Novy prechod", "V podkarte Prechod zvol From, symbol a To. Hrana sa hned vykresli v diagrame." )
                    , ( "Priama tvorba v grafe", "Klikni na zdrojovy stav, potom na cielovy stav a dopln symbol v paneli nad grafom. Pre epsilon mozes napisat epsilon alebo pouzit samostatne tlacidlo." )
                    , ( "Klik alebo drag", "Kratky klik na stav sluzi na tvorbu prechodu. Tah mysi posuva stav po platne." )
                    , ( "Zrusenie cez Esc", "Ak mas rozpracovany prechod vytvarany klikmi v grafe, stlacenim Esc alebo tlacidlom Zrusit ho okamzite zrusis." )
                    , ( "Symbol prechodu", "Pole Symbol akceptuje lubovolny textovy symbol. Ak napises epsilon, eps alebo ε, vytvori sa epsilon prechod a symbol sa nezaradi do abecedy." )
                    , ( "Samostatne tlacidlo", "V klasickom editore aj v paneli nad grafom je tlacidlo na okamzite pridanie epsilon prechodu bez pisania symbolu." )
                    , ( "Zoznam prechodov", "Podkarta Zoznam ukazuje vsetky prechody a dovoluje ich mazat po jednom." )
                    , ( "Konzolovy zapis", "Editor zapisuje konkretne akcie do konzoly, napr. ktory stav bol pridany alebo ktory prechod bol vytvoreny." )
                    , ( "DFA vs NFA", "Ak z jedneho stavu vedu pre rovnaky symbol rozne ciele alebo epsilon prechody, automat je NFA a cast algoritmov sa zablokuje." )
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
                    , ( "Podmienka simulacie", "Simulacia je urcena pre validny DFA. Ak mas NFA alebo epsilon prechody, najprv pouzi NFA -> DFA." )
                    ]
                , viewGuideActionTable
                    "Co uvidis v grafe"
                    "Vizualne napovedy pocas simulacie."
                    [ ( "Aktualny stav", "Zvyrazni sa ringom okolo uzla, aby bolo vidno, kde sa automat prave nachadza." )
                    , ( "Posledny prechod", "Pouzita hrana aj jej label sa po kroku zvyraznia." )
                    , ( "Zaseknutie", "Ak pre aktualny stav a symbol chyba prechod, simulacia sa zastavi na danom mieste." )
                    , ( "DFA obmedzenie", "Krokovanie aj autoplay su urcene pre validny DFA." )
                    , ( "Konzola", "Vysledok simulacie sa zapise aj do spodnej konzoly spolu so vstupom, koncovym stavom a poctom precitanych symbolov." )
                    ]
                ]

        GuideConversionTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-code-branch" "Konverzia a algoritmy" "Panel Algoritmy meni aktualny automat priamo na platne, preto sa hodi na analyzu aj formalne overovanie." [ "NFA -> DFA", "Minimalizacia", "Komplement", "Produkt" ]
                , viewGuideActionTable
                    "Dostupne algoritmy"
                    "Kazdy vysledok prepise aktualne platno."
                    [ ( "NFA -> DFA", "Pouziva subset construction vratane epsilon-closure, takze podporuje aj epsilon prechody." )
                    , ( "Minimalizacia", "Odstrani nedosiahnutelne stavy, totalizuje automat a potom zluci ekvivalentne stavy. Konzola rozlisi, ci sa pocet stavov zmensil alebo zostal rovnaky." )
                    , ( "Komplement", "Doplni chybajuce prechody do sink stavu a invertuje accepting mnozinu." )
                    , ( "Zjednotenie a prienik", "Nacita druhy automat z JSON textu alebo zo suboru a spravi produktovu konstrukciu nad spolocnou abecedou." )
                    , ( "Po spusteni algoritmu", "Vysledok prepise aktualny automat, ulozi sa do historie undo/redo, priebezne do localStorage a vypise zhrnutie do konzoly." )
                    ]
                , viewGuideActionTable
                    "Formalne podmienky"
                    "Predpoklady pre korektny vysledok."
                    [ ( "Validny automat", "Start musi existovat, accepting stavy musia byt v states a prechody mozu odkazovat len na existujuce stavy." )
                    , ( "Deterministicky vstup", "Minimalizacia, komplement, zjednotenie, prienik aj simulacia ocakavaju validny DFA." )
                    , ( "Pouzita abeceda", "Pri produktovych operaciach sa pracuje so spolocnou abecedou oboch automatov." )
                    , ( "Epsilon vstup", "Ak vstup obsahuje epsilon prechody, korektny postup je najprv spustit NFA -> DFA a az potom dalsie DFA algoritmy." )
                    ]
                ]

        GuideDataTab ->
            div [ class "space-y-5" ]
                [ viewGuideSummaryCard "fas fa-file-code" "JSON, import a export" "Automaty vies serializovat do JSON, nacitat zo suboru, automaticky obnovit po refreshi a exportovat aj samotny diagram." [ "JSON text", "JSON subor", "SVG", "PNG" ]
                , viewGuideActionTable
                    "Import / Export workflow"
                    "Vsetko, co potrebujes na prenos automatu."
                    [ ( "Zobrazit JSON v appke", "Vygeneruje serializovany vystup aktualneho automatu do textoveho pola." )
                    , ( "Stiahnut JSON subor", "Pripravi a stiahne JSON subor aktualneho automatu." )
                    , ( "Import z textu", "Vloz JSON do pola a klikni Importovat automat." )
                    , ( "Import zo suboru", "Vybrat JSON subor nacita obsah z disku do importneho pola." )
                    , ( "Druhy automat", "Pri A U B a A n B vies druhy automat vlozit ako text alebo vybrat ako subor." )
                    , ( "Export grafu", "Diagram vies ulozit ako SVG aj PNG." )
                    , ( "Automaticke obnovenie", "Posledny automat sa priebezne uklada do localStorage, takze po refreshi sa automaticky obnovi bez dalsieho klikania." )
                    , ( "Kedy sa uklada", "Ukladanie prebieha po realnej zmene automatu, nie pri kazdom pohybe mysi pocas dragovania." )
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
                    , ( "epsilon v JSON", "Epsilon prechod sa v JSON uklada ako prazdny retazec v poli symbol." )
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
                    , ( "Symboly mimo abecedy", "JSON obsahuje symboly, ktore nie su uvedene v alphabet. Vynimkou je prazdny symbol pouzity pre epsilon prechod." )
                    ]
                , viewGuideActionTable
                    "Algoritmicke obmedzenia"
                    "Aj validny automat moze narazit na obmedzenie algoritmu."
                    [ ( "DFA only", "Simulacia slova, minimalizacia, komplement, zjednotenie a prienik su urcene pre validny DFA." )
                    , ( "Nedeterministicke prechody", "Ak z jedneho stavu vedu pre rovnaky symbol rozne ciele alebo epsilon prechody, treba najprv spustit NFA -> DFA." )
                    , ( "Epsilon prechody", "Epsilon prechody su podporene pri prevode NFA -> DFA, ale automat s epsilon hranami sa stale sprava ako NFA." )
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
                    , ( "Vizualizacia", "Graf sa kresli ako SVG s loopmi, obojsmernymi hranami, zoskupenymi labelmi a klikacou tvorbou prechodov." )
                    , ( "Abeceda", "Pri editacii sa alphabet vie automaticky doplnat o realne pouzite symboly." )
                    , ( "Perzistencia", "Obnovenie po refreshi je riesene cez localStorage a porty medzi Elm a index.html." )
                    , ( "Konzola", "Konzola je overlay nad platom, takze neodsúva graf. Jej vysku vies menit tahanim hornej listy." )
                    ]
                , viewGuideInfoGrid
                    "Aktualne limity"
                    "Dolezite poznamky pred odovzdanim."
                    [ ( "DFA operacie", "Simulacia, minimalizacia, komplement, zjednotenie a prienik ocakavaju validny DFA." )
                    , ( "NFA -> DFA", "Subset construction podporuje aj epsilon prechody cez epsilon-closure." )
                    , ( "Prepis vysledku", "Algoritmy prepisu aktualny automat v editore, preto sa oplati vyuzivat undo/redo alebo export." )
                    , ( "Perzistencia", "Aktualny automat sa uklada do localStorage pre pohodlne obnovenie po refreshi." )
                    , ( "Konzola", "Spodny panel sa da tahat hore alebo dole a zobrazuje hlasenia, chyby, konkretne editacne akcie, vysledky simulacie a zhrnutia algoritmov." )
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

        hasEpsilon =
            V.hasEpsilonTransitions automaton

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
            , viewTopStat "Rezim" (if deterministic then "DFA" else "NFA") "fas fa-code-branch" (if deterministic then "Bez ε prechodov a bez duplicitnych hran pre rovnaky symbol" else "Obsahuje ε prechody alebo viacnasobne ciele pre rovnaky symbol")
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
                p [ class "mt-3 text-sm leading-6 text-[#bca48d]" ] [ text "Automat nema ziadny stav, z ktoreho by viedlo viac prechodov na rovnaky symbol, ani ziadny ε prechod." ]

              else
                div [ class "mt-3 space-y-2" ]
                    ((if hasEpsilon then
                        [ div [ class "rounded-2xl border border-amber-400/20 bg-amber-500/10 px-4 py-3 text-sm text-amber-100" ]
                            [ text "Automat obsahuje aspon jeden ε prechod, preto sa sprava ako NFA." ]
                        ]

                      else
                        []
                     )
                        ++ (duplicateGroups
                                |> List.map
                                    (\group ->
                                        div [ class "rounded-2xl border border-amber-400/20 bg-amber-500/10 px-4 py-3 text-sm text-amber-100" ]
                                            [ text
                                                ("q"
                                                    ++ String.fromInt group.from
                                                    ++ " ma pre symbol '"
                                                    ++ displaySymbol group.symbol
                                                    ++ "' viac cielov: "
                                                    ++ String.join ", " (List.map (\target -> "q" ++ String.fromInt target) group.targets)
                                                )
                                            ]
                                    )
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

        panSubscription =
            case model.canvasPan of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (panMoveDecoder |> D.map GraphMsg)
                        , Browser.Events.onMouseUp (D.succeed (GraphMsg Graph.EndPan))
                        ]

                Nothing ->
                    Sub.none

        consoleResizeSubscription =
            case model.consoleResize of
                Just _ ->
                    Sub.batch
                        [ Browser.Events.onMouseMove (D.field "clientY" D.float |> D.map ConsoleResize)
                        , Browser.Events.onMouseUp (D.succeed EndConsoleResize)
                        ]

                Nothing ->
                    Sub.none

        playbackSubscription =
            if model.simulation.autoplay then
                Browser.Events.onAnimationFrameDelta SimulationTick

            else
                Sub.none

        keySubscription =
            if model.graphTransitionDraft /= Nothing then
                Browser.Events.onKeyDown (D.field "key" D.string |> D.map GlobalKeyPressed)

            else
                Sub.none
    in
    Sub.batch
        [ dragSubscription
        , panSubscription
        , consoleResizeSubscription
        , playbackSubscription
        , keySubscription
        , jsonFileSelected JsonFileLoaded
        , savedAutomatonLoaded SavedAutomatonLoaded
        ]


mouseMoveDecoder : D.Decoder Graph.Msg
mouseMoveDecoder =
    D.map2 Graph.Drag
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)


panMoveDecoder : D.Decoder Graph.Msg
panMoveDecoder =
    D.map2 Graph.Pan
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
                    rawUpdatedModel =
                        update message model

                    updatedModel =
                        appendConsoleForMessage message model rawUpdatedModel
                in
                ( updatedModel
                , Cmd.batch
                    [ commandFor message updatedModel
                    , persistAutomatonCmd message model updatedModel
                    ]
                )
        , subscriptions = subscriptions
        }
