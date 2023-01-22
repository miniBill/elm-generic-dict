module Benchmark.Parametric exposing (BoxStats, computeStatistics, run)

import Benchmark.LowLevel exposing (Operation)
import Statistics
import Task exposing (Task)


run : Operation -> Task String (List Float)
run operation =
    Benchmark.LowLevel.warmup operation
        |> Task.andThen (\_ -> Benchmark.LowLevel.findSampleSize operation)
        |> Task.andThen
            (\sampleSize ->
                let
                    batchCount : Int
                    batchCount =
                        min sampleSize 100

                    batchSize : Int
                    batchSize =
                        (sampleSize + 99) // 100
                in
                List.range 0 batchCount
                    |> List.map (\_ -> Benchmark.LowLevel.sample batchSize operation)
                    |> Task.sequence
            )
        |> Task.mapError
            (\e ->
                case e of
                    Benchmark.LowLevel.StackOverflow ->
                        "Stack overflow"

                    Benchmark.LowLevel.UnknownError msg ->
                        msg
            )


type alias BoxStats =
    { firstQuartile : Float
    , median : Float
    , thirdQuartile : Float
    , max : Float
    , min : Float
    , outliers : List Float
    }


computeStatistics : List Float -> BoxStats
computeStatistics yList =
    let
        sortedYList : List Float
        sortedYList =
            List.sort yList

        -- Gather stats
        firstQuartile : Float
        firstQuartile =
            Statistics.quantile 0.25 sortedYList
                |> Maybe.withDefault 0

        median : Float
        median =
            Statistics.quantile 0.5 sortedYList
                |> Maybe.withDefault 0

        thirdQuartile : Float
        thirdQuartile =
            Statistics.quantile 0.75 sortedYList
                |> Maybe.withDefault 0

        interQuartileRange : Float
        interQuartileRange =
            thirdQuartile - firstQuartile

        whiskerTopMax : Float
        whiskerTopMax =
            thirdQuartile + 1.5 * interQuartileRange

        whiskerBottomMin : Float
        whiskerBottomMin =
            firstQuartile - (1.5 * interQuartileRange)

        ( outliersUnder, min, midAndOutliersOver ) =
            splitWhen (\n -> n >= whiskerBottomMin) sortedYList

        ( max, outliersOver ) =
            findLastSuchThat (\n -> n <= whiskerTopMax) midAndOutliersOver
    in
    { firstQuartile = firstQuartile
    , median = median
    , thirdQuartile = thirdQuartile
    , min = Maybe.withDefault 0 min
    , max = Maybe.withDefault 0 max
    , outliers = outliersUnder ++ outliersOver
    }


{-| The `Maybe a` is the first element which makes the function true
-}
splitWhen : (a -> Bool) -> List a -> ( List a, Maybe a, List a )
splitWhen f orig =
    let
        go : List a -> List a -> ( List a, Maybe a, List a )
        go acc input =
            case input of
                [] ->
                    ( orig, Nothing, [] )

                h :: t ->
                    if f h then
                        ( List.reverse acc, Just h, t )

                    else
                        go (h :: acc) t
    in
    go [] orig


findLastSuchThat : (a -> Bool) -> List a -> ( Maybe a, List a )
findLastSuchThat f orig =
    let
        go : Maybe a -> List a -> ( Maybe a, List a )
        go eacc input =
            case input of
                [] ->
                    ( eacc, [] )

                h :: t ->
                    if f h then
                        go (Just h) t

                    else
                        ( Just h, t )
    in
    go Nothing orig
