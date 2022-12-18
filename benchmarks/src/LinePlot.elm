module LinePlot exposing (BoxStats, Data, Datum, computeStatistics, view)

import Axis
import Color exposing (Color)
import Dict exposing (Dict)
import Html.Attributes
import Path exposing (Path)
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Set
import Shape
import Statistics exposing (quantile)
import TypedSvg exposing (defs, g, line, linearGradient, stop, svg)
import TypedSvg.Attributes exposing (class, fill, id, offset, opacity, stopColor, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Length(..), Opacity(..), Paint(..), Transform(..))


type alias Data =
    List Datum


type alias Datum =
    ( Color, Dict Int BoxStats )


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    30


xScale : Data -> BandScale Int
xScale model =
    model
        |> List.concatMap (\( _, points ) -> Dict.keys points)
        |> Set.fromList
        |> Set.toList
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : Float -> ContinuousScale Float
yScale max =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, max )


type alias BoxStats =
    { firstQuartile : Float
    , median : Float
    , thirdQuartile : Float
    , max : Float
    , min : Float
    }


computeStatistics : List Float -> BoxStats
computeStatistics yList =
    let
        sortedYList : List Float
        sortedYList =
            List.sort yList

        reverseSortedYList : List Float
        reverseSortedYList =
            List.reverse sortedYList

        -- Gather stats
        firstQuartile : Float
        firstQuartile =
            Statistics.quantile 0.25 sortedYList
                |> Maybe.withDefault 0

        thirdQuartile : Float
        thirdQuartile =
            Maybe.withDefault 0 <| quantile 0.75 sortedYList
    in
    { firstQuartile = firstQuartile
    , median = Statistics.quantile 0.5 sortedYList |> Maybe.withDefault 0
    , thirdQuartile = thirdQuartile
    , max = Maybe.withDefault 0 (List.head reverseSortedYList)
    , min = Maybe.withDefault 0 (List.head sortedYList)
    }


xAxis : Data -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable String.fromInt (xScale model))


yAxis : Float -> Svg msg
yAxis max =
    Axis.left [ Axis.tickCount 9 ] (yScale max)


column : Float -> BandScale Int -> Color -> Dict Int BoxStats -> List (Svg msg)
column max scale color statsDict =
    let
        stats : List ( Int, BoxStats )
        stats =
            Dict.toList statsDict

        transformToLineData : (BoxStats -> Float) -> ( Int, BoxStats ) -> Maybe ( Float, Float )
        transformToLineData selector ( x, y ) =
            Just ( Scale.convert scale x, Scale.convert (yScale max) (selector y) )

        tranfromToAreaData : ( Int, BoxStats ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        tranfromToAreaData ( x, y ) =
            Just
                ( ( Scale.convert scale x, Scale.convert (yScale max) y.min )
                , ( Scale.convert scale x, Scale.convert (yScale max) y.max )
                )

        line : (BoxStats -> Float) -> List ( Int, BoxStats ) -> Path
        line selector model =
            List.map (transformToLineData selector) model
                |> Shape.line Shape.monotoneInXCurve

        area : List ( Int, BoxStats ) -> Path
        area model =
            List.map tranfromToAreaData model
                |> Shape.area Shape.monotoneInXCurve

        qtColor : Color
        qtColor =
            color
                |> Color.toRgba
                |> (\rgba -> { rgba | alpha = 0.5 })
                |> Color.fromRgba

        bgColor : Color
        bgColor =
            color
                |> Color.toRgba
                |> (\rgba -> { rgba | alpha = 0.25 })
                |> Color.fromRgba
    in
    [ Path.element (area stats)
        [ stroke <| Paint color
        , strokeWidth 1
        , fill <| Paint bgColor
        ]
    , Path.element (line .median stats)
        [ stroke <| Paint color
        , strokeWidth 1
        , fill PaintNone
        ]
    , Path.element (line .firstQuartile stats)
        [ stroke <| Paint qtColor
        , strokeWidth 0.5
        , fill PaintNone
        ]
    , Path.element (line .thirdQuartile stats)
        [ stroke <| Paint qtColor
        , strokeWidth 0.5
        , fill PaintNone
        ]
    ]


yGridLine : Float -> Int -> Float -> Svg msg
yGridLine max index tick =
    line
        [ x1 0
        , x2 (w - 2 * padding)
        , y1 (Scale.convert (yScale max) tick)
        , y2 (Scale.convert (yScale max) tick)
        , stroke <| Paint Color.black
        , strokeWidth (toFloat (modBy 2 index) * 0.25 + 0.25)
        , opacity <| Opacity 0.3
        ]
        []


gradient : Svg msg
gradient =
    linearGradient
        [ id "linearGradient"
        , TypedSvg.Attributes.x1 <| Percent 0.0
        , TypedSvg.Attributes.y1 <| Percent 0.0
        , TypedSvg.Attributes.x2 <| Percent 0.0
        , TypedSvg.Attributes.y2 <| Percent 100.0
        ]
        [ stop [ offset "0%", stopColor "#e52d27" ] []
        , stop [ offset "100%", stopColor "#b31217" ] []
        ]


view : Data -> Svg msg
view model =
    let
        max : Float
        max =
            model
                |> List.concatMap (\( _, times ) -> Dict.values times)
                |> List.map .max
                |> List.maximum
                |> Maybe.withDefault 0
    in
    svg [ viewBox 0 0 w h, Html.Attributes.style "width" <| String.fromFloat w ++ "px" ]
        [ defs [] [ gradient ]
        , g [ transform [ Translate padding (padding + 0.5) ] ] <| List.indexedMap (yGridLine max) <| Scale.ticks (yScale max) 9
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis max ]
        , model
            |> List.concatMap (\( color, datum ) -> column max (xScale model) color datum)
            |> g [ transform [ Translate padding padding ], class [ "series" ] ]
        ]
