module Main exposing (Flags, Model, Msg, RunState, main)

import Benchmark.LowLevel exposing (Operation)
import Benchmark.Parametric exposing (BoxStats)
import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import DictDotDot as DDD
import Element exposing (Attribute, Element, alignTop, centerY, column, el, height, padding, px, row, shrink, spacing, table, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Intersect
import LinePlot
import List.Extra
import ParamDict exposing (Overlap(..), Param, ParamDict, Ratio)
import Process
import Random
import Task
import Union


type alias Flags =
    ()


type alias Model =
    { times : ParamDict (Dict Int BoxStats)
    , errors : List String
    , state : RunState
    }


type RunState
    = NotRunning
    | Running
    | Stopping


type alias ParamQueue =
    { current : Param
    , size : Int
    , queue : List Param
    }


type Msg
    = Run
    | Stop
    | Completed ParamQueue (Result String BoxStats)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = \model -> Element.layout [] (view model)
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { times = ParamDict.empty
      , state = NotRunning
      , errors = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Element Msg
view model =
    column
        [ padding 10
        , spacing 10
        ]
        [ wrappedRow [ spacing 10 ]
            [ case model.state of
                Stopping ->
                    button [ Background.color <| Element.rgb 0.8 0.8 0.8 ]
                        { onPress = Nothing
                        , label = text "Stopping..."
                        }

                Running ->
                    button []
                        { onPress = Just Stop
                        , label = text "Stop"
                        }

                NotRunning ->
                    button []
                        { onPress = Just Run
                        , label = text "Run"
                        }
            , text "(the ratio (x:y) means that the smallest dict will be of the indicated size, and the other will be 10 or 30 times bigger)"
            ]
        , if ParamDict.isEmpty model.times then
            Element.none

          else
            model.times
                |> ParamDict.toList
                |> List.Extra.gatherEqualsBy (\( param, _ ) -> ( param.section, param.ratio, param.overlap ))
                |> List.map
                    (\( ( { section, ratio, overlap }, _ ) as head, tail ) ->
                        let
                            ( lratio, rratio ) =
                                ratio
                        in
                        column [ spacing 10, alignTop ]
                            [ el [ Font.bold, Font.size 24 ] <| text <| section ++ " (" ++ String.fromInt lratio ++ ":" ++ String.fromInt rratio ++ ") " ++ overlapToString overlap
                            , (head :: tail)
                                |> List.map (\( { color }, times ) -> ( color, times ))
                                |> LinePlot.view
                                |> Element.html
                                |> el []
                            , viewTable (head :: tail)
                            ]
                    )
                |> wrappedRow [ spacing 10 ]
        , model.errors
            |> List.map (\err -> el [] <| text err)
            |> column [ spacing 10 ]
        ]


overlapToString : Overlap -> String
overlapToString overlap =
    case overlap of
        OverlapFull ->
            "100% shared"

        OverlapRandom ->
            "~50% shared"

        OverlapNoneLeftLower ->
            "0% shared (left < right)"

        OverlapNoneRightLower ->
            "0% shared (left > right)"

        OverlapNoneEvenOdd ->
            "0% shared (left odd, right even)"


viewTable : List ( Param, Dict Int BoxStats ) -> Element Msg
viewTable times =
    let
        data : List ( Int, Dict String BoxStats )
        data =
            times
                |> List.concatMap (\( { function }, dict ) -> List.map (\( size, stats ) -> ( size, function, stats )) (Dict.toList dict))
                |> List.Extra.gatherEqualsBy (\( size, _, _ ) -> size)
                |> List.map
                    (\( ( size, _, _ ) as head, tail ) ->
                        ( size
                        , (head :: tail)
                            |> List.map (\( _, key, stats ) -> ( key, stats ))
                            |> Dict.fromList
                        )
                    )

        keys : List ( String, Color )
        keys =
            List.map (\( param, _ ) -> ( param.function, param.color )) times

        header : String -> Maybe Color -> Element msg
        header label color =
            row [ Font.bold, spacing 3 ]
                [ case color of
                    Nothing ->
                        Element.none

                    Just c ->
                        let
                            rgba : { red : Float, green : Float, blue : Float, alpha : Float }
                            rgba =
                                Color.toRgba c
                        in
                        el
                            [ Background.color <| Element.rgb rgba.red rgba.green rgba.blue
                            , width <| px 10
                            , height <| px 10
                            , centerY
                            ]
                            Element.none
                , text label
                ]
    in
    table [ spacing 10 ]
        { data = data
        , columns =
            { header = header "size" Nothing
            , view = \( size, _ ) -> text <| String.fromInt size
            , width = shrink
            }
                :: List.map
                    (\( key, color ) ->
                        { header = header key (Just color)
                        , view =
                            \( _, vals ) ->
                                case Dict.get key vals of
                                    Just stats ->
                                        text <| formatFloat stats.min ++ "; " ++ formatFloat stats.median ++ "; " ++ formatFloat stats.max

                                    Nothing ->
                                        Element.none
                        , width = shrink
                        }
                    )
                    keys
        }


formatFloat : Float -> String
formatFloat f =
    round (f * 100)
        |> toFloat
        |> (\r -> r / 100)
        |> String.fromFloat


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs config =
    Input.button (Border.width 1 :: padding 10 :: attrs) config


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noCmd : Model -> ( Model, Cmd Msg )
        noCmd m =
            ( m, Cmd.none )

        withCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
        withCmd c m =
            ( m, c )
    in
    case msg of
        Stop ->
            { model | state = Stopping }
                |> noCmd

        Run ->
            { model | state = Running }
                |> withCmd
                    (case operations of
                        head :: tail ->
                            let
                                queue : ParamQueue
                                queue =
                                    { current = head
                                    , size = initialSize
                                    , queue = tail
                                    }
                            in
                            run queue

                        _ ->
                            Cmd.none
                    )

        Completed param (Ok times) ->
            let
                newTimes : ParamDict (Dict Int BoxStats)
                newTimes =
                    ParamDict.update
                        param.current
                        (\v ->
                            v
                                |> Maybe.withDefault Dict.empty
                                |> Dict.insert param.size times
                                |> Just
                        )
                        model.times

                modelWithTimes : Model
                modelWithTimes =
                    { model
                        | times = newTimes
                    }

                nextParam : Maybe ParamQueue
                nextParam =
                    if model.state == Stopping then
                        Nothing

                    else if incrementSize param.size <= maxSize && (times.median < maxTime) then
                        Just { param | size = incrementSize param.size }

                    else
                        case param.queue of
                            [] ->
                                Nothing

                            qhead :: qtail ->
                                Just
                                    { current = qhead
                                    , size = initialSize
                                    , queue = qtail
                                    }
            in
            case nextParam of
                Just np ->
                    modelWithTimes
                        |> withCmd (run np)

                Nothing ->
                    { modelWithTimes
                        | state = NotRunning
                    }
                        |> noCmd

        Completed _ (Err err) ->
            { model
                | state = NotRunning
                , errors = err :: model.errors
            }
                |> noCmd


maxTime : Float
maxTime =
    4


initialSize : Int
initialSize =
    100


maxSize : Int
maxSize =
    30000


incrementSize : Int -> Int
incrementSize size =
    size * 3 // 2


run : ParamQueue -> Cmd Msg
run param =
    let
        operation : Operation
        operation =
            param.current.op param.size
    in
    Process.sleep 0
        |> Task.andThen (\_ -> Benchmark.LowLevel.warmup operation)
        |> Task.andThen (\_ -> Process.sleep 0)
        |> Task.andThen (\_ -> Benchmark.LowLevel.findSampleSize operation)
        |> Task.andThen (\sampleSize -> Process.sleep 0 |> Task.map (\_ -> sampleSize))
        |> Task.andThen
            (\sampleSize ->
                let
                    batchSize : Int
                    batchSize =
                        4
                in
                List.range 0 (max 1 <| (sampleSize + 2) // batchSize)
                    |> List.map
                        (\_ ->
                            Benchmark.LowLevel.sample batchSize operation
                                |> Task.map Just
                        )
                    |> intersperseInTotal 20 (Process.sleep 0 |> Task.map (\_ -> Nothing))
                    |> Task.sequence
                    |> Task.map (List.filterMap identity >> Benchmark.Parametric.computeStatistics)
            )
        |> Task.mapError
            (\e ->
                case e of
                    Benchmark.LowLevel.StackOverflow ->
                        "Stack overflow"

                    Benchmark.LowLevel.UnknownError msg ->
                        msg
            )
        |> Task.attempt (Completed param)


intersperseInTotal : Int -> a -> List a -> List a
intersperseInTotal count elem list =
    let
        skip : Int
        skip =
            max 1 <| List.length list // (count + 1)

        go : List a -> Int -> List a -> List a
        go acc cnt left =
            case left of
                [] ->
                    List.reverse acc

                head :: tail ->
                    if cnt <= 0 then
                        go (elem :: acc) skip left

                    else
                        go (head :: acc) (cnt - 1) tail
    in
    go [] skip list


{-| `generate n` generates a list of n numbers between 0 and 2n
-}
generate : Int -> Both Int Int
generate size =
    let
        generator : Random.Generator (Both Int Int)
        generator =
            Random.int 0 (2 * size)
                |> Random.map (\t -> ( t, t ))
                |> Random.list size
                |> Random.map (\lst -> { core = Dict.fromList lst, dotdot = DDD.fromList lst })
    in
    Random.step generator (Random.initialSeed size)
        |> Tuple.first


type alias Args =
    { overlap : Overlap
    , ratio : Ratio
    , section : String
    }


operations : List Param
operations =
    let
        intersections : List (Args -> Param)
        intersections =
            if False then
                [ --     compareCore "library" Color.red Dict.intersect
                  -- , compareCore "toList" Color.green Intersect.toList
                  -- , compareCore "folding" Color.blue Intersect.folding
                  -- ,
                  compareDotDot "library (ddd)" Color.darkRed DDD.intersect

                -- , compareDotDot "toList (ddd)" Color.darkBrown Intersect.toList_DotDot
                -- , compareDotDot "folding (ddd)" Color.darkBlue Intersect.folding_DotDot
                -- , compareDotDot "recursion (ddd)" Color.darkBrown Intersect.recursion_DotDot
                --  , compareDotDot "recursion² (ddd)" Color.darkGray Intersect.recursion_twice_DotDot
                , compareDotDot "recursion³ (ddd)" Color.darkGreen Intersect.recursion_thrice_DotDot
                , compareDotDot "recursion³ + fromArray (ddd)" Color.darkYellow Intersect.recursion_thrice_fromArray_DotDot
                , compareDotDot "recursion³ + fromList (ddd)" Color.darkYellow Intersect.recursion_thrice_fromList_DotDot
                ]

            else
                [ compareDotDot "library" Color.darkRed DDD.intersect
                , compareDotDot "alternative" Color.darkYellow Intersect.recursion_thrice_fromList_DotDot
                ]

        unions : List (Args -> Param)
        unions =
            if False then
                [ compareCore "library" Color.red Dict.union
                , compareCore "toList" Color.green Union.toList

                -- , compareCore "folding" Color.blue Union.folding
                , compareDotDot "library (ddd)" Color.darkRed DDD.union
                , compareDotDot "toList (ddd)" Color.darkGreen Union.toList_DotDot

                -- , compareDotDot "folding (ddd)" Color.darkBlue Union.folding_DotDot
                -- , compareDotDot "recursion (ddd)" Color.darkYellow Union.recursion_DotDot
                ]

            else
                []
    in
    [ buildSection "intersection" intersections
    , buildSection "union" unions
    ]
        |> List.concat


compareCore : String -> Color -> (Dict Int Int -> Dict Int Int -> Dict Int Int) -> Args -> Param
compareCore =
    compare .core


compareDotDot : String -> Color -> (DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int) -> Args -> Param
compareDotDot =
    compare .dotdot


buildSection : String -> List (Args -> Param) -> List Param
buildSection label list =
    List.Extra.lift3
        (\ratio overlap f ->
            f
                { overlap = overlap
                , ratio = ratio
                , section = label
                }
        )
        [ ( 1, 0 )
        , ( 30, 1 )
        , ( 10, 1 )
        , ( 1, 1 )
        , ( 1, 10 )
        , ( 1, 30 )
        ]
        [ OverlapRandom, OverlapFull, OverlapNoneEvenOdd, OverlapNoneLeftLower, OverlapNoneRightLower ]
        list


compare :
    (Both Int Int -> dict)
    -> String
    -> Color
    -> (dict -> dict -> dict)
    -> Args
    -> Param
compare selector label color op { overlap, ratio, section } =
    let
        ( lratio, rratio ) =
            ratio
    in
    { overlap = overlap
    , section = section
    , function = label
    , color = color
    , ratio = ratio
    , op =
        \size ->
            let
                lsize : Int
                lsize =
                    size * lratio

                rsize : Int
                rsize =
                    size * rratio

                rsizeFixed : Int
                rsizeFixed =
                    if rsize == lsize then
                        -- Prevent having the exact same size, and thus random seed
                        rsize + 1

                    else
                        rsize

                ls : dict
                ls =
                    if overlap == OverlapNoneEvenOdd then
                        selector (mapBoth (\_ n -> n * 2) (generate lsize))

                    else
                        selector (generate lsize)

                rs : Both Int Int
                rs =
                    generate rsizeFixed

                rsFixed : dict
                rsFixed =
                    case overlap of
                        OverlapRandom ->
                            selector rs

                        OverlapFull ->
                            ls

                        OverlapNoneLeftLower ->
                            selector <| mapBoth (\_ n -> n + max lsize rsizeFixed * 3) rs

                        OverlapNoneRightLower ->
                            selector <| mapBoth (\_ n -> -n) rs

                        OverlapNoneEvenOdd ->
                            selector <| mapBoth (\_ n -> n * 2 + 1) rs
            in
            Benchmark.LowLevel.operation (\_ -> op ls rsFixed)
    }


mapBoth : (comparable -> a -> b) -> Both comparable a -> Both comparable b
mapBoth f { core, dotdot } =
    { core = Dict.map f core
    , dotdot = DDD.map f dotdot
    }


type alias Both k v =
    { core : Dict k v, dotdot : DDD.Dict k v }
