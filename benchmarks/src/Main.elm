module Main exposing (Flags, Model, Msg, Times, main)

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
import Process
import Random
import Result.Extra
import Task


type alias Flags =
    ()


type alias Model =
    { times : Dict String Times
    , errors : List String
    , running : Bool
    }


type alias Times =
    Dict String ( Color, Dict Int BoxStats )


type alias ParamQueue =
    { current : Param
    , size : Int
    , queue : List Param
    }


type alias Param =
    { section : String
    , key : String
    , color : Color
    , op : Int -> Operation
    }


type Msg
    = Run
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
    ( { times = Dict.empty
      , running = False
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
        , if Dict.isEmpty model.times then
            Element.none

          else
            model.times
                |> Dict.toList
                |> List.map
                    (\( sectionName, sectionTimes ) ->
                        column [ spacing 10, alignTop ]
                            [ el [ Font.bold, Font.size 24 ] <| text sectionName
                            , viewTable sectionTimes
                            , sectionTimes
                                |> Dict.values
                                |> LinePlot.view
                                |> Element.html
                                |> el []
                            ]
                    )
                |> wrappedRow [ spacing 10 ]
        , model.errors
            |> List.map (\err -> el [] <| text err)
            |> column [ spacing 10 ]
        ]


viewTable : Times -> Element Msg
viewTable times =
    let
        data : List ( Int, Dict String BoxStats )
        data =
            times
                |> Dict.toList
                |> List.concatMap (\( key, ( _, dict ) ) -> List.map (\( size, stats ) -> ( size, key, stats )) (Dict.toList dict))
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
            times
                |> Dict.toList
                |> List.map (\( key, ( color, _ ) ) -> ( key, color ))

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
            let
                sectionToQueue :
                    ( String, Dict String ( Color, Int -> Operation ) )
                    -> List { section : String, key : String, color : Color, op : Int -> Operation }
                sectionToQueue ( section, ops ) =
                    ops
                        |> Dict.toList
                        |> List.map
                            (\( key, ( color, op ) ) ->
                                { section = section
                                , key = key
                                , color = color
                                , op = op
                                }
                            )
            in
            { model | running = True }
                |> withCmd
                    (case
                        Result.map
                            (Dict.toList
                                >> List.concatMap sectionToQueue
                            )
                            operations
                     of
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
                    incrementSize param.size <= maxSize

                newTimes : Dict String Times
                newTimes =
                    Dict.update param.current.section
                        (Maybe.withDefault Dict.empty
                            >> Dict.update
                                param.current.key
                                (\v ->
                                    Just ( param.current.color, Dict.insert param.size times (Maybe.withDefault Dict.empty <| Maybe.map Tuple.second v) )
                                )
                            >> Just
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


initialSize : Int
initialSize =
    100


maxSize : Int
maxSize =
    2000


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


operations : Result Error (Dict String (Dict String ( Color, Int -> Operation )))
operations =
    [ ( 100, 1 ), ( 10, 1 ), ( 1, 1 ), ( 1, 10 ), ( 1, 100 ) ]
        |> List.map
            (\(( lr, rr ) as ratio) ->
                [ -- intersectCore ratio "core" Color.red Dict.intersect
                  -- , intersectCore ratio "toList" Color.green Intersect.toList
                  -- , intersectCore ratio "folding" Color.blue Intersect.folding,
                  intersectDotDot ratio "dotdot" Color.darkRed DDD.intersect
                , intersectDotDot ratio "toList_dotdot" Color.darkGreen Intersect.toList_DotDot
                , intersectDotDot ratio "folding_dotdot" Color.darkBlue Intersect.folding_DotDot
                , intersectDotDot ratio "recursion_dotdot" Color.darkYellow Intersect.recursion_DotDot
                ]
                    |> Result.Extra.combine
                    |> Result.map
                        (Dict.fromList
                            >> Tuple.pair ("intersection (" ++ String.fromInt lr ++ ":" ++ String.fromInt rr ++ ")")
                        )
            )
        |> Result.Extra.combine
        |> Result.map Dict.fromList


type alias Error =
    { label : String
    , left : Dict Int Int
    , right : Dict Int Int
    , expected : List ( Int, Int )
    , actual : List ( Int, Int )
    }


intersectCore : ( Int, Int ) -> String -> Color -> (Dict Int Int -> Dict Int Int -> Dict Int Int) -> Result Error ( String, ( Color, Int -> Operation ) )
intersectCore ratio label =
    compare ratio label Dict.intersect .core Dict.toList


intersectDotDot : ( Int, Int ) -> String -> Color -> (DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int) -> Result Error ( String, ( Color, Int -> Operation ) )
intersectDotDot ratio label =
    compare ratio label Dict.intersect .dotdot DDD.toList


compare :
    ( Int, Int )
    -> String
    -> (Dict Int Int -> Dict Int Int -> Dict Int Int)
    -> (Both Int Int -> dict)
    -> (dict -> List ( Int, Int ))
    -> Color
    -> (dict -> dict -> dict)
    -> Result Error ( String, ( Color, Int -> Operation ) )
compare ( lratio, rratio ) label core selector toList color op =
    let
        ltest : Both Int Int
        ltest =
            generate (150 // rratio)

        rtest : Both Int Int
        rtest =
            generate (151 // lratio)

        expected : List ( Int, Int )
        expected =
            core ltest.core rtest.core |> Dict.toList

        actual : List ( Int, Int )
        actual =
            op (selector ltest) (selector rtest) |> toList
    in
    if expected == actual then
        Ok
            ( label
            , ( color
              , \size ->
                    let
                        ls : dict
                        ls =
                            selector (generate size)

                        rs : dict
                        rs =
                            selector (generate (size + 1))
                    in
                    Benchmark.LowLevel.operation (\_ -> op ls rs)
              )
            )

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
