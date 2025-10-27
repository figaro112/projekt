module View.Graph exposing (view)

import Automaton.Core exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Automaton -> Svg msg
view a =
    let
        n =
            List.length a.states

        radius =
            160

        centerX =
            360

        centerY =
            260

        indexOf : comparable -> List comparable -> Int
        indexOf x xs =
            xs
                |> List.indexedMap Tuple.pair
                |> List.filter (\( i, s ) -> s == x)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 0

        pos : StateId -> ( Float, Float )
        pos s =
            let
                i =
                    indexOf s a.states

                angle =
                    2 * pi * toFloat i / toFloat (Basics.max 1 n)
            in
            ( centerX + radius * cos angle, centerY + radius * sin angle )

        node : StateId -> Svg msg
        node s =
            let
                ( x, y ) =
                    pos s

                isAcc =
                    List.member s a.accepting
            in
            g []
                (circle
                    [ cx (String.fromFloat x)
                    , cy (String.fromFloat y)
                    , r "26"
                    , fill "#fff"
                    , stroke "#444"
                    ]
                    []
                    :: (if isAcc then
                            [ circle
                                [ cx (String.fromFloat x)
                                , cy (String.fromFloat y)
                                , r "20"
                                , fill "none"
                                , stroke "#444"
                                ]
                                []
                            ]

                        else
                            []
                       )
                    ++ [ text_
                            [ Svg.Attributes.x (String.fromFloat (x - 4))
                            , Svg.Attributes.y (String.fromFloat (y + 4))
                            , fontSize "14"
                            ]
                            [ text (String.fromInt s) ]
                       ]
                )

        edge : Transition -> Svg msg
        edge t =
            let
                ( x1c, y1c ) =
                    pos t.from

                ( x2c, y2c ) =
                    pos t.to_
            in
            g []
                [ line
                    [ x1 (String.fromFloat x1c)
                    , y1 (String.fromFloat y1c)
                    , x2 (String.fromFloat x2c)
                    , y2 (String.fromFloat y2c)
                    , stroke "#888"
                    ]
                    []
                , text_
                    [ Svg.Attributes.x (String.fromFloat ((x1c + x2c) / 2))
                    , Svg.Attributes.y (String.fromFloat ((y1c + y2c) / 2))
                    , fontSize "12"
                    ]
                    [ text t.symbol ]
                ]
    in
    svg [ width "100%", height "100%", viewBox "0 0 900 600" ]
        [ g [] (List.map edge a.transitions)
        , g [] (List.map node a.states)
        ]
