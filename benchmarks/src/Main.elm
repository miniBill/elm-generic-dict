module Main exposing (Flags, Model, Msg, main)

import Benchmark.LowLevel exposing (Operation)
import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import DictDotDot as DDD
import Element exposing (Attribute, Element, centerY, column, el, height, padding, px, row, shrink, spacing, table, text, width)
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
    { times : Dict String ( Color, Dict Int BoxStats )
    , errors : List String
    , running : Bool
    }


type alias Param =
    { key : String
    , color : Color
    , size : Int
    , queue : List ( String, Color )
    }


type Msg
    = Run
    | Completed Param (Result String BoxStats)


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
        [ button [] <|
            if model.running then
                { onPress = Nothing
                , label = text <| "Running"
                }

            else
                { onPress = Just Run
                , label = text <| "Run"
                }
        , if Dict.isEmpty model.times then
            Element.none

          else
            viewTable model
        , if Dict.isEmpty model.times then
            Element.none

          else
            model.times
                |> Dict.values
                |> LinePlot.view
                |> Element.html
                |> el []
        , model.errors
            |> List.map (\err -> el [] <| text err)
            |> column [ spacing 10 ]
        ]


viewTable : Model -> Element Msg
viewTable model =
    let
        data : List ( Int, Dict String BoxStats )
        data =
            model.times
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
            model.times
                |> Dict.toList
                |> List.map (\( key, ( color, _ ) ) -> ( key, color ))

        header : String -> Maybe Color -> Element msg
        header label color =
            row [ Font.bold, spacing 1 ]
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
            { header = header "key" Nothing
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
        noCmd m =
            ( m, Cmd.none )

        withCmd c m =
            ( m, c )
    in
    case msg of
        Run ->
            { model | running = True }
                |> withCmd
                    (run
                        { key = "intersect core"
                        , color = Color.red
                        , size = 100
                        , queue =
                            [ ( "intersect toList", Color.green )
                            , ( "intersect folding", Color.blue )
                            , ( "intersect dotdot", Color.darkRed )
                            , ( "intersect toList_dotdot", Color.darkGreen )
                            , ( "intersect folding_dotdot", Color.darkBlue )
                            ]
                        }
                    )

        Completed param (Ok times) ->
            let
                continue : Bool
                continue =
                    param.size < 1000

                newTimes : Dict String ( Color, Dict Int BoxStats )
                newTimes =
                    Dict.update
                        param.key
                        (\v ->
                            Just ( param.color, Dict.insert param.size times (Maybe.withDefault Dict.empty <| Maybe.map Tuple.second v) )
                        )
                        model.times
            in
            if continue then
                { model
                    | times = newTimes
                }
                    |> withCmd
                        (run
                            { param | size = param.size + 100 }
                        )

            else
                case param.queue of
                    [] ->
                        { model
                            | running = False
                            , times = newTimes
                        }
                            |> noCmd

                    ( qhead, color ) :: qtail ->
                        { model | times = newTimes }
                            |> withCmd
                                (run
                                    { key = qhead
                                    , color = color
                                    , size = 100
                                    , queue = qtail
                                    }
                                )

        Completed _ (Err err) ->
            { model
                | running = False
                , errors = err :: model.errors
            }
                |> noCmd


run : Param -> Cmd Msg
run param =
    case operations |> Result.toMaybe |> Maybe.andThen (Dict.get param.key) of
        Nothing ->
            Task.succeed (Err "Key not found")
                |> Task.perform (Completed param)

        Just f ->
            let
                operation : Operation
                operation =
                    f param.size
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
                |> Task.mapError Debug.toString
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


operations : Result Error (Dict String (Int -> Operation))
operations =
    [ intersectCore "core" Dict.intersect
    , intersectCore "toList" Intersect.toList
    , intersectCore "folding" Intersect.folding
    , intersectDotDot "dotdot" DDD.intersect
    , intersectDotDot "toList_dotdot" Intersect.toList_DotDot
    , intersectDotDot "folding_dotdot" Intersect.folding_DotDot
    ]
        |> Result.Extra.combine
        |> Result.map Dict.fromList


type alias Error =
    { label : String
    , left : Dict Int Int
    , right : Dict Int Int
    , expected : List ( Int, Int )
    , actual : List ( Int, Int )
    }


intersectCore : String -> (Dict Int Int -> Dict Int Int -> Dict Int Int) -> Result Error ( String, Int -> Operation )
intersectCore label =
    intersect label .core Dict.toList


intersectDotDot : String -> (DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int) -> Result Error ( String, Int -> Operation )
intersectDotDot label =
    intersect label .dotdot DDD.toList


intersect :
    String
    -> (Both Int Int -> dict)
    -> (dict -> List ( Int, Int ))
    -> (dict -> dict -> dict)
    -> Result Error ( String, Int -> Operation )
intersect label =
    compare Dict.intersect ("intersect " ++ label)


compare :
    (Dict Int Int -> Dict Int Int -> Dict Int Int)
    -> String
    -> (Both Int Int -> dict)
    -> (dict -> List ( Int, Int ))
    -> (dict -> dict -> dict)
    -> Result Error ( String, Int -> Operation )
compare core label selector toList op =
    let
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
            ( label
            , \size ->
                let
                    ls =
                        selector (generate size)

                    rs =
                        selector (generate (size + 1))
                in
                Benchmark.LowLevel.operation (\_ -> op ls rs)
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
