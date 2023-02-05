module ToBenchmark exposing (Function, Graph, Overlap, Ratio, config)

import Codec exposing (Codec)
import Dict exposing (Dict)
import DictDotDot as DDD
import FastBenchmark.Types exposing (Config, Param)
import Intersect
import List.Extra
import Random


config : Config Graph Function
config =
    { graphToString = graphToString
    , graphCodec = graphCodec
    , functionToString = functionToString
    , functionCodec = functionCodec
    , graphs = graphs
    , functions = functions
    , sizes = sizes
    , toFunction = toFunction
    , timeout = timeout
    }


type alias Graph =
    { ratio : Ratio
    , overlap : Overlap
    }


type alias Ratio =
    ( Int, Int )


type Overlap
    = OverlapRandom
    | OverlapFull
    | OverlapNoneLeftLower
    | OverlapNoneRightLower
    | OverlapNoneEvenOdd


graphToString : Graph -> String
graphToString graph =
    ratioToString graph.ratio ++ " " ++ overlapToString graph.overlap


ratioToString : Ratio -> String
ratioToString ( l, r ) =
    String.fromInt l ++ ":" ++ String.fromInt r


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


graphCodec : Codec Graph
graphCodec =
    Codec.object (\ratio overlap -> { ratio = ratio, overlap = overlap })
        |> Codec.field "ratio" .ratio (Codec.tuple Codec.int Codec.int)
        |> Codec.field "overlap" .overlap overlapCodec
        |> Codec.buildObject


overlapCodec : Codec Overlap
overlapCodec =
    Codec.custom
        (\frandom ffull fnoneLeftLower fnoneRightLower fnoneEvenOdd value ->
            case value of
                OverlapRandom ->
                    frandom

                OverlapFull ->
                    ffull

                OverlapNoneLeftLower ->
                    fnoneLeftLower

                OverlapNoneRightLower ->
                    fnoneRightLower

                OverlapNoneEvenOdd ->
                    fnoneEvenOdd
        )
        |> Codec.variant0 "OverlapRandom" OverlapRandom
        |> Codec.variant0 "OverlapFull" OverlapFull
        |> Codec.variant0 "OverlapNoneLeftLower" OverlapNoneLeftLower
        |> Codec.variant0 "OverlapNoneRightLower" OverlapNoneRightLower
        |> Codec.variant0 "OverlapNoneEvenOdd" OverlapNoneEvenOdd
        |> Codec.buildCustom


type Function
    = Core
    | Folding
    | ToList
    | Folding_DotDot
    | ToList_DotDot
    | Recursion_1_DotDot
    | Recursion_2_DotDot
    | Recursion_3_DotDot
    | Recursion_fromArray_DotDot
    | Recursion_fromList_DotDot


functionToString : Function -> String
functionToString function =
    case function of
        Core ->
            "elm/core"

        Folding ->
            "folding"

        ToList ->
            "toList"

        Folding_DotDot ->
            "folding (..)"

        ToList_DotDot ->
            "toList (..)"

        Recursion_1_DotDot ->
            "recursion [1] (..)"

        Recursion_2_DotDot ->
            "recursion [2] (..)"

        Recursion_3_DotDot ->
            "recursion [3] (..)"

        Recursion_fromArray_DotDot ->
            "recursion + fromArray (..)"

        Recursion_fromList_DotDot ->
            "recursion + fromList (..)"


functionCodec : Codec Function
functionCodec =
    Codec.custom
        (\fCore fFolding fToList fFolding_DotDot fToList_DotDot fRecursion_1_DotDot fRecursion_2_DotDot fRecursion_3_DotDot fRecursion_fromArray_DotDot fRecursion_fromList_DotDot value ->
            case value of
                Core ->
                    fCore

                Folding ->
                    fFolding

                ToList ->
                    fToList

                Folding_DotDot ->
                    fFolding_DotDot

                ToList_DotDot ->
                    fToList_DotDot

                Recursion_1_DotDot ->
                    fRecursion_1_DotDot

                Recursion_2_DotDot ->
                    fRecursion_2_DotDot

                Recursion_3_DotDot ->
                    fRecursion_3_DotDot

                Recursion_fromArray_DotDot ->
                    fRecursion_fromArray_DotDot

                Recursion_fromList_DotDot ->
                    fRecursion_fromList_DotDot
        )
        |> Codec.variant0 "Core" Core
        |> Codec.variant0 "Folding" Folding
        |> Codec.variant0 "ToList" ToList
        |> Codec.variant0 "Folding_DotDot" Folding_DotDot
        |> Codec.variant0 "ToList_DotDot" ToList_DotDot
        |> Codec.variant0 "Recursion_1_DotDot" Recursion_1_DotDot
        |> Codec.variant0 "Recursion_2_DotDot" Recursion_2_DotDot
        |> Codec.variant0 "Recursion_3_DotDot" Recursion_3_DotDot
        |> Codec.variant0 "Recursion_fromArray_DotDot" Recursion_fromArray_DotDot
        |> Codec.variant0 "Recursion_fromList_DotDot" Recursion_fromList_DotDot
        |> Codec.buildCustom


{-| Timeout, in milliseconds
-}
timeout : Maybe number
timeout =
    Just 3


graphs : List Graph
graphs =
    List.Extra.lift2
        (\ratio overlap ->
            { overlap = overlap
            , ratio = ratio
            }
        )
        ratios
        overlaps
        |> List.filter (\{ overlap, ratio } -> ratio == ( 1, 1 ) || overlap /= OverlapFull)


ratios : List Ratio
ratios =
    [ ( 1, 0 )
    , ( 30, 1 )
    , ( 10, 1 )
    , ( 1, 1 )
    , ( 1, 10 )
    , ( 1, 30 )
    ]


overlaps : List Overlap
overlaps =
    [ OverlapRandom
    , OverlapFull
    , OverlapNoneEvenOdd
    , OverlapNoneLeftLower
    , OverlapNoneRightLower
    ]


functions : List Function
functions =
    [ Core
    , Folding
    , ToList
    , Folding_DotDot
    , ToList_DotDot
    , Recursion_1_DotDot
    , Recursion_2_DotDot
    , Recursion_3_DotDot
    , Recursion_fromArray_DotDot
    , Recursion_fromList_DotDot
    ]


sizes : List Int
sizes =
    List.range 1 14
        |> List.map (\i -> 100 * 3 ^ i // 2 ^ i)


type alias Both k v =
    { core : Dict k v
    , dotdot : DDD.Dict k v
    }


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


toFunction : Param Graph Function -> (() -> ())
toFunction { graph, function, size } =
    let
        ( lratio, rratio ) =
            graph.ratio

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

        ls : Both Int Int
        ls =
            if graph.overlap == OverlapNoneEvenOdd then
                mapBoth (\_ n -> n * 2) (generate lsize)

            else
                generate lsize

        rs : Both Int Int
        rs =
            generate rsizeFixed

        rsFixed : Both Int Int
        rsFixed =
            case graph.overlap of
                OverlapRandom ->
                    rs

                OverlapFull ->
                    ls

                OverlapNoneLeftLower ->
                    mapBoth (\_ n -> n + max lsize rsizeFixed * 3) rs

                OverlapNoneRightLower ->
                    mapBoth (\_ n -> -n) rs

                OverlapNoneEvenOdd ->
                    mapBoth (\_ n -> n * 2 + 1) rs
    in
    \_ ->
        case function of
            Core ->
                ignore <| Dict.intersect ls.core rsFixed.core

            Folding ->
                ignore <| Intersect.folding ls.core rsFixed.core

            ToList ->
                ignore <| Intersect.toList ls.core rsFixed.core

            Folding_DotDot ->
                ignore <| Intersect.folding_DotDot ls.dotdot rsFixed.dotdot

            ToList_DotDot ->
                ignore <| Intersect.toList_DotDot ls.dotdot rsFixed.dotdot

            Recursion_1_DotDot ->
                ignore <| Intersect.recursion_1_DotDot ls.dotdot rsFixed.dotdot

            Recursion_2_DotDot ->
                ignore <| Intersect.recursion_2_DotDot ls.dotdot rsFixed.dotdot

            Recursion_3_DotDot ->
                ignore <| Intersect.recursion_3_DotDot ls.dotdot rsFixed.dotdot

            Recursion_fromArray_DotDot ->
                ignore <| Intersect.recursion_fromArray_DotDot ls.dotdot rsFixed.dotdot

            Recursion_fromList_DotDot ->
                ignore <| Intersect.recursion_fromList_DotDot ls.dotdot rsFixed.dotdot


mapBoth : (k -> v -> v) -> Both k v -> Both k v
mapBoth f both =
    { core = Dict.map f both.core
    , dotdot = DDD.map f both.dotdot
    }


ignore : a -> ()
ignore _ =
    ()
