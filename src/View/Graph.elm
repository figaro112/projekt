module View.Graph exposing (Highlight, Msg(..), view)

import Automaton.Core exposing (Automaton, StateId, Transition)
import Dict exposing (Dict)
import Json.Decode as D
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Events as SE


type Msg
    = StartDrag StateId Float Float
    | Drag Float Float
    | EndDrag
    | NoOp


type alias Highlight =
    { currentState : Maybe StateId
    , activeTransition : Maybe Transition
    }


view : Highlight -> Automaton -> Svg Msg
view highlight a =
    let
        pos : StateId -> ( Float, Float )
        pos s =
            case Dict.get s a.positions of
                Just p ->
                    ( p.x, p.y )

                Nothing ->
                    ( 140, 140 )

        hasReverse : StateId -> StateId -> Bool
        hasReverse from to_ =
            List.any (\t -> t.from == to_ && t.to_ == from) a.transitions

        indexOf : comparable -> List comparable -> Int
        indexOf x xs =
            xs
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, s ) -> s == x)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        groupTransitions : List Transition -> List ( StateId, StateId, List String )
        groupTransitions transitions =
            transitions
                |> List.foldl
                    (\t acc ->
                        let
                            key =
                                ( t.from, t.to_ )

                            existing =
                                Dict.get key acc |> Maybe.withDefault []
                        in
                        Dict.insert key (existing ++ [ t.symbol ]) acc
                    )
                    Dict.empty
                |> Dict.toList
                |> List.map (\( ( from, to_ ), symbols ) -> ( from, to_, symbols ))

        groupedTransitions =
            groupTransitions a.transitions

        displayLabel : List String -> String
        displayLabel symbols =
            let
                cleaned =
                    List.filter (not << String.isEmpty) symbols

                total =
                    List.length cleaned
            in
            if total <= 4 then
                String.join ", " cleaned

            else
                String.join ", " (List.take 4 cleaned) ++ " +" ++ String.fromInt (total - 4)

        labelBadge : Bool -> Float -> Float -> String -> Svg Msg
        labelBadge isActive x y txt =
            let
                w =
                    max 28 (String.length txt * 7 + 18)

                xStr =
                    String.fromFloat (x - toFloat w / 2)

                yStr =
                    String.fromFloat (y - 14)

                badgeFill =
                    if isActive then
                        "rgba(254,240,138,0.98)"

                    else
                        "rgba(255,248,240,0.96)"

                badgeStroke =
                    if isActive then
                        "#f59e0b"

                    else
                        "#705948"

                badgeText =
                    if isActive then
                        "#713f12"

                    else
                        "#4a3426"
            in
            g [ SA.pointerEvents "none" ]
                [ rect
                    [ SA.x xStr
                    , SA.y yStr
                    , SA.rx "11"
                    , SA.ry "11"
                    , SA.width (String.fromInt w)
                    , SA.height "28"
                    , SA.fill badgeFill
                    , SA.stroke badgeStroke
                    , SA.strokeWidth "1.2"
                    ]
                    []
                , text_
                    [ SA.x (String.fromFloat x)
                    , SA.y (String.fromFloat (y + 4))
                    , SA.textAnchor "middle"
                    , SA.fontSize "12"
                    , SA.fontWeight "700"
                    , SA.fill badgeText
                    ]
                    [ text txt ]
                ]

        startArrow : Svg Msg
        startArrow =
            case a.start of
                Nothing ->
                    g [] []

                Just sid ->
                    let
                        ( x, y ) =
                            pos sid

                        startX =
                            x - 86

                        startY =
                            y

                        endX =
                            x - 36

                        endY =
                            y
                    in
                    g []
                        [ line
                            [ SA.x1 (String.fromFloat startX)
                            , SA.y1 (String.fromFloat startY)
                            , SA.x2 (String.fromFloat endX)
                            , SA.y2 (String.fromFloat endY)
                            , SA.stroke "#22c55e"
                            , SA.strokeWidth "2.6"
                            , SA.strokeLinecap "round"
                            , SA.markerEnd "url(#arrowhead-start)"
                            ]
                            []
                        ]

        node : StateId -> Svg Msg
        node s =
            let
                ( x, y ) =
                    pos s

                isAcc =
                    List.member s a.accepting

                isStart =
                    a.start == Just s

                isCurrent =
                    highlight.currentState == Just s

                xStr =
                    String.fromFloat x

                yStr =
                    String.fromFloat y

                strokeColor =
                    if isStart then
                        "#d97706"

                    else
                        "#725c4c"

                haloNode =
                    if isStart then
                        [ circle
                            [ SA.cx xStr
                            , SA.cy yStr
                            , SA.r "37"
                            , SA.fill "rgba(217,119,6,0.08)"
                            , SA.stroke "rgba(217,119,6,0.28)"
                            , SA.strokeWidth "1.4"
                            ]
                            []
                        ]

                    else
                        []

                currentHalo =
                    if isCurrent then
                        [ circle
                            [ SA.cx xStr
                            , SA.cy yStr
                            , SA.r "43"
                            , SA.fill "rgba(250,204,21,0.10)"
                            , SA.stroke "rgba(245,158,11,0.60)"
                            , SA.strokeWidth "3"
                            , SA.strokeDasharray "7 6"
                            ]
                            []
                        ]

                    else
                        []

                acceptingNode =
                    if isAcc then
                        [ circle
                            [ SA.cx xStr
                            , SA.cy yStr
                            , SA.r "22"
                            , SA.fill "none"
                            , SA.stroke "#725c4c"
                            , SA.strokeWidth "2.4"
                            ]
                            []
                        ]

                    else
                        []
            in
            g
                [ SE.on "mousedown" (onMouseDown s)
                , SA.cursor "move"
                , SA.class "state-node"
                ]
                (currentHalo
                    ++ haloNode
                    ++ [ circle
                            [ SA.cx xStr
                            , SA.cy yStr
                            , SA.r "29"
                            , SA.fill "#fff8f0"
                            , SA.stroke strokeColor
                            , SA.strokeWidth "3"
                            ]
                            []
                       ]
                    ++ acceptingNode
                    ++ [ text_
                            [ SA.x xStr
                            , SA.y (String.fromFloat (y + 5))
                            , SA.textAnchor "middle"
                            , SA.fontSize "18"
                            , SA.fontWeight "800"
                            , SA.fill "#211813"
                            , SA.pointerEvents "none"
                            ]
                            [ text (String.fromInt s) ]
                       ]
                )

        edge : ( StateId, StateId, List String ) -> Svg Msg
        edge ( fromState, toState, symbols ) =
            let
                ( x1, y1 ) =
                    pos fromState

                ( x2, y2 ) =
                    pos toState

                idx1 =
                    indexOf fromState a.states

                idx2 =
                    indexOf toState a.states

                isBidirectional =
                    hasReverse fromState toState

                dx =
                    x2 - x1

                dy =
                    y2 - y1

                angle =
                    atan2 dy dx

                x1Short =
                    x1 + 31 * cos angle

                y1Short =
                    y1 + 31 * sin angle

                x2Short =
                    x2 - 31 * cos angle

                y2Short =
                    y2 - 31 * sin angle

                label =
                    displayLabel symbols

                isActiveEdge =
                    case highlight.activeTransition of
                        Just transition ->
                            transition.from == fromState
                                && transition.to_ == toState
                                && List.member transition.symbol symbols

                        Nothing ->
                            False

                edgeColor =
                    if isActiveEdge then
                        "#f59e0b"

                    else if isBidirectional && idx1 > idx2 then
                        "#ef4444"

                    else
                        "#c08457"

                edgeMarker =
                    if isActiveEdge then
                        "url(#arrowhead-active)"

                    else if isBidirectional && idx1 > idx2 then
                        "url(#arrowhead-red)"

                    else
                        "url(#arrowhead)"
            in
            if fromState == toState then
                let
                    loopSize =
                        56

                    startX =
                        x1 - 16

                    startY =
                        y1 - 30

                    endX =
                        x1 + 16

                    endY =
                        y1 - 30

                    ctrlX1 =
                        x1 - loopSize

                    ctrlY1 =
                        y1 - loopSize - 22

                    ctrlX2 =
                        x1 + loopSize

                    ctrlY2 =
                        y1 - loopSize - 22
                in
                g []
                    [ path
                        [ SA.d
                            ("M "
                                ++ String.fromFloat startX
                                ++ " "
                                ++ String.fromFloat startY
                                ++ " C "
                                ++ String.fromFloat ctrlX1
                                ++ " "
                                ++ String.fromFloat ctrlY1
                                ++ ", "
                                ++ String.fromFloat ctrlX2
                                ++ " "
                                ++ String.fromFloat ctrlY2
                                ++ ", "
                                ++ String.fromFloat endX
                                ++ " "
                                ++ String.fromFloat endY
                            )
                        , SA.stroke edgeColor
                        , SA.strokeWidth "3"
                        , SA.fill "none"
                        , SA.markerEnd edgeMarker
                        ]
                        []
                    , labelBadge isActiveEdge x1 (y1 - loopSize - 28) label
                    ]

            else if isBidirectional && idx1 < idx2 then
                let
                    midX =
                        (x1 + x2) / 2

                    midY =
                        (y1 + y2) / 2

                    ctrlX =
                        midX

                    ctrlY =
                        midY - 72
                in
                g []
                    [ path
                        [ SA.d
                            ("M "
                                ++ String.fromFloat x1Short
                                ++ " "
                                ++ String.fromFloat y1Short
                                ++ " Q "
                                ++ String.fromFloat ctrlX
                                ++ " "
                                ++ String.fromFloat ctrlY
                                ++ " "
                                ++ String.fromFloat x2Short
                                ++ " "
                                ++ String.fromFloat y2Short
                            )
                        , SA.stroke edgeColor
                        , SA.strokeWidth "3"
                        , SA.fill "none"
                        , SA.markerEnd edgeMarker
                        ]
                        []
                    , labelBadge isActiveEdge ctrlX (ctrlY - 12) label
                    ]

            else if isBidirectional && idx1 > idx2 then
                let
                    midX =
                        (x1 + x2) / 2

                    midY =
                        (y1 + y2) / 2

                    ctrlX =
                        midX

                    ctrlY =
                        midY + 72
                in
                g []
                    [ path
                        [ SA.d
                            ("M "
                                ++ String.fromFloat x1Short
                                ++ " "
                                ++ String.fromFloat y1Short
                                ++ " Q "
                                ++ String.fromFloat ctrlX
                                ++ " "
                                ++ String.fromFloat ctrlY
                                ++ " "
                                ++ String.fromFloat x2Short
                                ++ " "
                                ++ String.fromFloat y2Short
                            )
                        , SA.stroke edgeColor
                        , SA.strokeWidth "3"
                        , SA.fill "none"
                        , SA.markerEnd edgeMarker
                        ]
                        []
                    , labelBadge isActiveEdge ctrlX (ctrlY + 18) label
                    ]

            else
                let
                    midX =
                        (x1 + x2) / 2

                    midY =
                        (y1 + y2) / 2
                in
                g []
                    [ line
                        [ SA.x1 (String.fromFloat x1Short)
                        , SA.y1 (String.fromFloat y1Short)
                        , SA.x2 (String.fromFloat x2Short)
                        , SA.y2 (String.fromFloat y2Short)
                        , SA.stroke edgeColor
                        , SA.strokeWidth "3"
                        , SA.strokeLinecap "round"
                        , SA.markerEnd edgeMarker
                        ]
                        []
                    , labelBadge isActiveEdge midX (midY - 14) label
                    ]
    in
    svg
        [ SA.width "100%"
        , SA.height "840"
        , SA.viewBox "0 0 1220 840"
        , SA.id "automaton-canvas"
        ]
        [ defs []
            [ pattern
                [ SA.id "grid"
                , SA.width "40"
                , SA.height "40"
                , SA.patternUnits "userSpaceOnUse"
                ]
                [ path
                    [ SA.d "M 40 0 L 0 0 0 40"
                    , SA.fill "none"
                    , SA.stroke "rgba(124,94,71,0.22)"
                    , SA.strokeWidth "1"
                    ]
                    []
                ]
            , marker
                [ SA.id "arrowhead"
                , SA.markerWidth "12"
                , SA.markerHeight "12"
                , SA.refX "10"
                , SA.refY "4"
                , SA.orient "auto"
                , SA.markerUnits "strokeWidth"
                ]
                [ path
                    [ SA.d "M0,0 L0,8 L10,4 z"
                    , SA.fill "#c08457"
                    ]
                    []
                ]
            , marker
                [ SA.id "arrowhead-red"
                , SA.markerWidth "12"
                , SA.markerHeight "12"
                , SA.refX "10"
                , SA.refY "4"
                , SA.orient "auto"
                , SA.markerUnits "strokeWidth"
                ]
                [ path
                    [ SA.d "M0,0 L0,8 L10,4 z"
                    , SA.fill "#ef4444"
                    ]
                    []
                ]
            , marker
                [ SA.id "arrowhead-active"
                , SA.markerWidth "12"
                , SA.markerHeight "12"
                , SA.refX "10"
                , SA.refY "4"
                , SA.orient "auto"
                , SA.markerUnits "strokeWidth"
                ]
                [ path
                    [ SA.d "M0,0 L0,8 L10,4 z"
                    , SA.fill "#f59e0b"
                    ]
                    []
                ]
            , marker
                [ SA.id "arrowhead-start"
                , SA.markerWidth "10"
                , SA.markerHeight "10"
                , SA.refX "8.5"
                , SA.refY "4"
                , SA.orient "auto"
                , SA.markerUnits "strokeWidth"
                ]
                [ path
                    [ SA.d "M0,0 L0,8 L10,4 z"
                    , SA.fill "#22c55e"
                    ]
                    []
                ]
            ]
        , rect
            [ SA.x "12"
            , SA.y "12"
            , SA.width "1196"
            , SA.height "816"
            , SA.rx "24"
            , SA.ry "24"
            , SA.fill "rgba(28,22,18,0.96)"
            ]
            []
        , rect
            [ SA.x "12"
            , SA.y "12"
            , SA.width "1196"
            , SA.height "816"
            , SA.rx "24"
            , SA.ry "24"
            , SA.fill "url(#grid)"
            ]
            []
        , rect
            [ SA.x "12"
            , SA.y "12"
            , SA.width "1196"
            , SA.height "816"
            , SA.rx "24"
            , SA.ry "24"
            , SA.fill "none"
            , SA.stroke "rgba(120,89,65,0.58)"
            , SA.strokeWidth "1.2"
            ]
            []
        , startArrow
        , g [] (List.map edge groupedTransitions)
        , g [] (List.map node a.states)
        ]


onMouseDown : StateId -> D.Decoder Msg
onMouseDown stateId =
    D.map2 (StartDrag stateId)
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)
