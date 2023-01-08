module Main exposing (Flags, Model, Msg, main)

import Benchmark.LowLevel exposing (Operation)
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
import LinePlot exposing (BoxStats)
import List.Extra
import ParamDict exposing (Param, ParamDict, Ratio)
import Process
import Random
import Result.Extra
import Task
import Union


type alias Flags =
    ()


type alias Model =
    { times : ParamDict (Dict Int BoxStats)
    , errors : List String
    , running : Bool
    , slowBenchmark : Bool
    }


type alias ParamQueue =
    { current : Param
    , size : Int
    , queue : List Param
    }


type Msg
    = Run
    | Completed ParamQueue (Result String BoxStats)
    | SlowBenchmark Bool


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
      , running = False
      , errors = []
      , slowBenchmark = False
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
            [ if model.running then
                button [ Background.color <| Element.rgb 0.8 0.8 0.8 ]
                    { onPress = Nothing
                    , label = text <| "Running"
                    }

              else
                button []
                    { onPress = Just Run
                    , label = text <| "Run"
                    }
            , Input.checkbox []
                { onChange = SlowBenchmark
                , checked = model.slowBenchmark
                , label = Input.labelRight [] <| text "Full benchmark (slow!)"
                , icon = Input.defaultCheckbox
                }
            , text "(the ratio (x:y) means that the smallest dict will be of the indicated size, and the other will be 10 or 30 times bigger)"
            ]
        , if ParamDict.isEmpty model.times then
            Element.none

          else
            model.times
                |> ParamDict.toList
                |> List.Extra.gatherEqualsBy (\( param, _ ) -> ( param.section, param.ratio ))
                |> List.map
                    (\( ( { section, ratio }, _ ) as head, tail ) ->
                        let
                            ( lratio, rratio ) =
                                ratio
                        in
                        column [ spacing 10, alignTop ]
                            [ el [ Font.bold, Font.size 24 ] <| text <| section ++ " (" ++ String.fromInt lratio ++ ":" ++ String.fromInt rratio ++ ")"
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
        Run ->
            { model | running = True }
                |> withCmd
                    (case operations of
                        Ok (head :: tail) ->
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
                continue : Bool
                continue =
                    if model.slowBenchmark then
                        incrementSize param.size <= maxSize

                    else
                        incrementSize param.size <= maxSize && (times.median < 5)

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
            in
            if continue then
                { model
                    | times = newTimes
                }
                    |> withCmd
                        (run
                            { param | size = incrementSize param.size }
                        )

            else
                case param.queue of
                    [] ->
                        { model
                            | running = False
                            , times = newTimes
                        }
                            |> noCmd

                    qhead :: qtail ->
                        { model | times = newTimes }
                            |> withCmd
                                (run
                                    { current = qhead
                                    , size = initialSize
                                    , queue = qtail
                                    }
                                )

        Completed _ (Err err) ->
            { model
                | running = False
                , errors = err :: model.errors
            }
                |> noCmd

        SlowBenchmark slowBenchmark ->
            { model | slowBenchmark = slowBenchmark }
                |> noCmd


initialSize : Int
initialSize =
    300


maxSize : Int
maxSize =
    3000


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
                    |> Task.map (List.filterMap identity >> LinePlot.computeStatistics)
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

        go : Int -> List a -> List a
        go cnt left =
            case left of
                [] ->
                    []

                head :: tail ->
                    if cnt == 0 then
                        elem :: go skip left

                    else
                        head :: go (cnt - 1) tail
    in
    go skip list


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


operations : Result Error (List Param)
operations =
    let
        intersections : List ({ ratio : Ratio, core : Dict Int Int -> Dict Int Int -> Dict Int Int, section : String } -> Result Error Param)
        intersections =
            if True then
                [ --   compareCore "library" Color.red Dict.intersect
                  -- , compareCore "toList" Color.green Intersect.toList
                  -- , compareCore "folding" Color.blue Intersect.folding,
                  compareDotDot "library (ddd)" Color.darkRed DDD.intersect
                , compareDotDot "toList (ddd)" Color.darkGreen Intersect.toList_DotDot
                , compareDotDot "folding (ddd)" Color.darkBlue Intersect.folding_DotDot
                , compareDotDot "recursion (ddd)" Color.darkYellow Intersect.recursion_DotDot
                ]

            else
                []

        unions : List ({ ratio : Ratio, core : Dict Int Int -> Dict Int Int -> Dict Int Int, section : String } -> Result Error Param)
        unions =
            if True then
                [ --   compareCore "library" Color.red Dict.union
                  -- , compareCore "toList" Color.green Union.toList
                  -- , compareCore "folding" Color.blue Union.folding,
                  compareDotDot "library (ddd)" Color.darkRed DDD.union
                , compareDotDot "toList (ddd)" Color.darkGreen Union.toList_DotDot

                -- , compareDotDot "folding (ddd)" Color.darkBlue Union.folding_DotDot
                -- , compareDotDot "recursion (ddd)" Color.darkYellow Union.recursion_DotDot
                ]

            else
                []
    in
    [ buildSection "intersection" Dict.intersect intersections
    , buildSection "union" Dict.union unions
    ]
        |> Result.Extra.combine
        |> Result.map List.concat


buildSection : String -> (Dict Int Int -> Dict Int Int -> Dict Int Int) -> List ({ ratio : Ratio, core : Dict Int Int -> Dict Int Int -> Dict Int Int, section : String } -> Result Error Param) -> Result Error (List Param)
buildSection label core list =
    case list of
        [] ->
            Ok []

        _ ->
            [ ( 30, 1 ), ( 10, 1 ), ( 1, 1 ), ( 1, 10 ), ( 1, 30 ) ]
                |> List.map
                    (\ratio ->
                        list
                            |> List.map (\f -> f { ratio = ratio, core = core, section = label })
                            |> Result.Extra.combine
                    )
                |> Result.Extra.combine
                |> Result.map List.concat


compareCore : String -> Color -> (Dict Int Int -> Dict Int Int -> Dict Int Int) -> { ratio : Ratio, core : Dict Int Int -> Dict Int Int -> Dict Int Int, section : String } -> Result Error Param
compareCore =
    compare Dict.toList .core


compareDotDot : String -> Color -> (DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int) -> { ratio : Ratio, core : Dict Int Int -> Dict Int Int -> Dict Int Int, section : String } -> Result Error Param
compareDotDot =
    compare DDD.toList .dotdot


type alias Error =
    { label : String
    , left : Dict Int Int
    , right : Dict Int Int
    , expected : List ( Int, Int )
    , actual : List ( Int, Int )
    }


compare :
    (dict -> List ( Int, Int ))
    -> (Both Int Int -> dict)
    -> String
    -> Color
    -> (dict -> dict -> dict)
    -> { ratio : Ratio, core : Dict Int Int -> Dict Int Int -> Dict Int Int, section : String }
    -> Result Error Param
compare toList selector label color op { ratio, core, section } =
    let
        ( lratio, rratio ) =
            ratio

        ltest : Both Int Int
        ltest =
            generate 150

        rtest : Both Int Int
        rtest =
            generate 151

        expected : List ( Int, Int )
        expected =
            core ltest.core rtest.core |> Dict.toList

        actual : List ( Int, Int )
        actual =
            op (selector ltest) (selector rtest) |> toList
    in
    if expected == actual then
        Ok
            { section = section
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
                            selector (generate lsize)

                        rs : dict
                        rs =
                            selector (generate rsizeFixed)
                    in
                    Benchmark.LowLevel.operation (\_ -> op ls rs)
            }

    else
        Err
            { label = label
            , left = ltest.core
            , right = rtest.core
            , expected = expected
            , actual = actual
            }


type alias Both k v =
    { core : Dict k v, dotdot : DDD.Dict k v }
