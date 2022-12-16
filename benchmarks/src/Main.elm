module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Alternative
import Benchmark.Runner.Alternative
import Browser
import Dict exposing (Dict)
import DictDotDot as DDD
import Html
import List.Extra
import Result.Extra


main : Benchmark.Runner.Alternative.Program
main =
    case suite of
        Ok s ->
            Benchmark.Runner.Alternative.program s

        Err e ->
            Browser.sandbox
                { init = { suite = Benchmark.describe "" [] }
                , view = \_ -> Html.pre [] [ Html.text e ]
                , update = \_ model -> model
                }


suite : Result String Benchmark
suite =
    [ compare "intersect"
        { core = Dict.intersect
        , toList = intersect_toList
        , folding = intersect_folding
        , dotdot = DDD.intersect
        , toList_dotdot = intersect_toList_DotDot
        , folding_dotdot = intersect_folding_DotDot
        }
    ]
        |> Result.Extra.combine
        |> Result.map (List.concat >> Benchmark.describe "Dict")


intersect_toList : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect_toList l r =
    let
        go : Dict comparable v -> List ( comparable, v ) -> List comparable -> Dict comparable v
        go acc lleft rleft =
            case lleft of
                [] ->
                    acc

                ( lheadKey, lheadValue ) :: ltail ->
                    case List.Extra.dropWhile (\rk -> rk < lheadKey) rleft of
                        [] ->
                            acc

                        (rheadKey :: rtail) as rNext ->
                            if lheadKey == rheadKey then
                                go (Dict.insert lheadKey lheadValue acc) ltail rtail

                            else
                                go acc (List.Extra.dropWhile (\( lk, _ ) -> lk < rheadKey) ltail) rNext
    in
    go Dict.empty (Dict.toList l) (Dict.keys r)


intersect_folding : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect_folding l r =
    Dict.foldl
        (\lkey lvalue ( acc, queue ) ->
            case List.Extra.dropWhile (\rkey -> rkey < lkey) queue of
                [] ->
                    ( acc, [] )

                (qhead :: qtail) as newQueue ->
                    if qhead == lkey then
                        ( Dict.insert lkey lvalue acc, qtail )

                    else
                        ( acc, newQueue )
        )
        ( Dict.empty, Dict.keys r )
        l
        |> Tuple.first


intersect_toList_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
intersect_toList_DotDot l r =
    let
        go : DDD.Dict comparable v -> List ( comparable, v ) -> List comparable -> DDD.Dict comparable v
        go acc lleft rleft =
            case lleft of
                [] ->
                    acc

                ( lheadKey, lheadValue ) :: ltail ->
                    case List.Extra.dropWhile (\rk -> rk < lheadKey) rleft of
                        [] ->
                            acc

                        (rheadKey :: rtail) as rNext ->
                            if lheadKey == rheadKey then
                                go (DDD.insert lheadKey lheadValue acc) ltail rtail

                            else
                                go acc (List.Extra.dropWhile (\( lk, _ ) -> lk < rheadKey) ltail) rNext
    in
    go DDD.empty (DDD.toList l) (DDD.keys r)


intersect_folding_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
intersect_folding_DotDot l r =
    DDD.foldl
        (\lkey lvalue ( acc, queue ) ->
            case List.Extra.dropWhile (\rkey -> rkey < lkey) queue of
                [] ->
                    ( acc, [] )

                (qhead :: qtail) as newQueue ->
                    if qhead == lkey then
                        ( DDD.insert lkey lvalue acc, qtail )

                    else
                        ( acc, newQueue )
        )
        ( DDD.empty, DDD.keys r )
        l
        |> Tuple.first


type alias Both k v =
    { core : Dict k v, dotdot : DDD.Dict k v }


compare :
    String
    ->
        { core : Dict Int Int -> Dict Int Int -> Dict Int Int
        , toList : Dict Int Int -> Dict Int Int -> Dict Int Int
        , folding : Dict Int Int -> Dict Int Int -> Dict Int Int
        , dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , toList_dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , folding_dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        }
    -> Result String (List Benchmark)
compare label ({ core, toList, folding, dotdot, toList_dotdot, folding_dotdot } as functions) =
    List.range 0 3
        |> Result.Extra.combineMap
            (\pow ->
                let
                    cap : Int
                    cap =
                        10 ^ pow

                    toDict : (Int -> Bool) -> Both Int Int
                    toDict f =
                        let
                            list =
                                List.map (\t -> ( t, t )) <| List.filter f (List.range 1 cap)
                        in
                        { core = Dict.fromList list
                        , dotdot = DDD.fromList list
                        }

                    all : Both Int Int
                    all =
                        toDict (\_ -> True)

                    even : Both Int Int
                    even =
                        toDict (\n -> modBy 2 n == 0)

                    odd : Both Int Int
                    odd =
                        toDict (\n -> modBy 2 n == 1)

                    alternatives : List ( String, Both Int Int -> Both Int Int -> Bool )
                    alternatives =
                        [ ( "elm/core", \l r -> Dict.isEmpty <| core l.core r.core )
                        , ( "converting to lists, using elm/core", \l r -> Dict.isEmpty <| toList l.core r.core )
                        , ( "folding, using elm/core", \l r -> Dict.isEmpty <| folding l.core r.core )
                        , ( "showell/dict-dot-dot", \l r -> DDD.isEmpty <| dotdot l.dotdot r.dotdot )
                        , ( "converting to lists, using showell/dict-dot-dot", \l r -> DDD.isEmpty <| toList_dotdot l.dotdot r.dotdot )
                        , ( "folding, using showell/dict-dot-dot", \l r -> DDD.isEmpty <| folding_dotdot l.dotdot r.dotdot )
                        ]

                    broken =
                        check toList
                            || check folding
                            || checkDD dotdot
                            || checkDD toList_dotdot
                            || checkDD folding_dotdot

                    check method =
                        (core even.core odd.core /= method even.core odd.core)
                            || (core even.core all.core /= method even.core all.core)
                            || (core even.core even.core /= method even.core even.core)

                    checkDD method =
                        (core even.core odd.core /= fromDD (method even.dotdot odd.dotdot))
                            || (core even.core all.core /= fromDD (method even.dotdot all.dotdot))
                            || (core even.core even.core /= fromDD (method even.dotdot even.dotdot))
                in
                if broken then
                    [ compToString "no" functions even odd
                    , compToString "half" functions even all
                    , compToString "full" functions even even
                    ]
                        |> String.join "\n\n"
                        |> Err

                else
                    [ Benchmark.Alternative.rank (label ++ "- no intersection - n = " ++ String.fromInt cap) (\f -> f even odd) alternatives
                    , Benchmark.Alternative.rank (label ++ "- half intersection - n = " ++ String.fromInt cap) (\f -> f even all) alternatives
                    , Benchmark.Alternative.rank (label ++ "- full intersection - n = " ++ String.fromInt cap) (\f -> f even even) alternatives
                    ]
                        |> Ok
            )
        |> Result.map List.concat


compToString :
    String
    ->
        { core : Dict Int Int -> Dict Int Int -> Dict Int Int
        , toList : Dict Int Int -> Dict Int Int -> Dict Int Int
        , folding : Dict Int Int -> Dict Int Int -> Dict Int Int
        , dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , toList_dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , folding_dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        }
    -> Both Int Int
    -> Both Int Int
    -> String
compToString label { core, toList, folding, toList_dotdot, dotdot, folding_dotdot } l r =
    let
        inner d =
            Dict.toList d
                |> List.map (\( k, v ) -> String.fromInt k ++ " " ++ String.fromInt v)
                |> String.join ", "
    in
    [ label ++ ":"
    , "  core: " ++ inner (core l.core r.core)
    , "  toList: " ++ inner (toList l.core r.core)
    , "  folding: " ++ inner (folding l.core r.core)
    , "  dotdot: " ++ inner (fromDD <| dotdot l.dotdot r.dotdot)
    , "  toList_dotdot: " ++ inner (fromDD <| toList_dotdot l.dotdot r.dotdot)
    , "  folding_dotdot: " ++ inner (fromDD <| folding_dotdot l.dotdot r.dotdot)
    ]
        |> String.join "\n"


fromDD : DDD.Dict comparable v -> Dict comparable v
fromDD =
    DDD.toList >> Dict.fromList
