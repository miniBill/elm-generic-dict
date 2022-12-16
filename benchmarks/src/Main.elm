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
        , public = intersect
        , public2 = intersect2
        , dotdot = DDD.intersect
        , private = intersectDotdot
        }
    ]
        |> Result.Extra.combine
        |> Result.map (List.concat >> Benchmark.describe "Dict")


intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect l r =
    let
        llist : List ( comparable, v )
        llist =
            Dict.toList l

        rlist : List ( comparable, v )
        rlist =
            Dict.toList r

        go : Dict comparable v -> List ( comparable, v ) -> List ( comparable, v ) -> Dict comparable v
        go acc lleft rleft =
            case lleft of
                [] ->
                    acc

                ( lheadKey, lheadValue ) :: ltail ->
                    case rleft of
                        [] ->
                            acc

                        ( rheadKey, _ ) :: rtail ->
                            if lheadKey == rheadKey then
                                go (Dict.insert lheadKey lheadValue acc) ltail rtail

                            else if lheadKey < rheadKey then
                                go acc ltail rleft

                            else
                                go acc lleft rtail
    in
    go Dict.empty llist rlist


intersect2 : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect2 l r =
    let
        rlist : List comparable
        rlist =
            Dict.keys r
    in
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
        ( Dict.empty, rlist )
        l
        |> Tuple.first


intersectDotdot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
intersectDotdot l r =
    let
        llist : List ( comparable, v )
        llist =
            DDD.toList l

        rlist : List ( comparable, v )
        rlist =
            DDD.toList r

        go : DDD.Dict comparable v -> List ( comparable, v ) -> List ( comparable, v ) -> DDD.Dict comparable v
        go acc lleft rleft =
            case lleft of
                [] ->
                    acc

                ( lheadKey, lheadValue ) :: ltail ->
                    case rleft of
                        [] ->
                            acc

                        ( rheadKey, _ ) :: rtail ->
                            if lheadKey == rheadKey then
                                go (DDD.insert lheadKey lheadValue acc) ltail rtail

                            else if lheadKey < rheadKey then
                                go acc ltail rleft

                            else
                                go acc lleft rtail
    in
    go DDD.empty llist rlist


type alias Both k v =
    { core : Dict k v, dotdot : DDD.Dict k v }


compare :
    String
    ->
        { core : Dict Int Int -> Dict Int Int -> Dict Int Int
        , public : Dict Int Int -> Dict Int Int -> Dict Int Int
        , public2 : Dict Int Int -> Dict Int Int -> Dict Int Int
        , dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , private : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        }
    -> Result String (List Benchmark)
compare label ({ core, public, public2, dotdot, private } as functions) =
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
                        , ( "my algo, using elm/core", \l r -> Dict.isEmpty <| public l.core r.core )
                        , ( "my second algo, using elm/core", \l r -> Dict.isEmpty <| public2 l.core r.core )
                        , ( "showell/dict-dot-dot", \l r -> DDD.isEmpty <| dotdot l.dotdot r.dotdot )
                        , ( "my algo, using showell/dict-dot-dot", \l r -> DDD.isEmpty <| private l.dotdot r.dotdot )
                        ]

                    broken =
                        (core even.core odd.core /= public even.core odd.core)
                            || (core even.core all.core /= public even.core all.core)
                            || (core even.core even.core /= public even.core even.core)
                            || (core even.core odd.core /= public2 even.core odd.core)
                            || (core even.core all.core /= public2 even.core all.core)
                            || (core even.core even.core /= public2 even.core even.core)
                            || (core even.core odd.core /= fromDD (dotdot even.dotdot odd.dotdot))
                            || (core even.core all.core /= fromDD (dotdot even.dotdot all.dotdot))
                            || (core even.core even.core /= fromDD (dotdot even.dotdot even.dotdot))
                            || (core even.core odd.core /= fromDD (private even.dotdot odd.dotdot))
                            || (core even.core all.core /= fromDD (private even.dotdot all.dotdot))
                            || (core even.core even.core /= fromDD (private even.dotdot even.dotdot))
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
        , public : Dict Int Int -> Dict Int Int -> Dict Int Int
        , public2 : Dict Int Int -> Dict Int Int -> Dict Int Int
        , private : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        }
    -> Both Int Int
    -> Both Int Int
    -> String
compToString label { core, public, public2, private, dotdot } l r =
    let
        inner d =
            Dict.toList d
                |> List.map (\( k, v ) -> String.fromInt k ++ " " ++ String.fromInt v)
                |> String.join ", "
    in
    [ label ++ ":"
    , "  core: " ++ inner (core l.core r.core)
    , "  public: " ++ inner (public l.core r.core)
    , "  public2: " ++ inner (public2 l.core r.core)
    , "  dotdot: " ++ inner (fromDD <| dotdot l.dotdot r.dotdot)
    , "  private: " ++ inner (fromDD <| private l.dotdot r.dotdot)
    ]
        |> String.join "\n"


fromDD : DDD.Dict comparable v -> Dict comparable v
fromDD =
    DDD.toList >> Dict.fromList
