module Main exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Alternative
import Benchmark.Runner.Alternative
import Dict exposing (Dict)
import DictDotDot as DDD


main : Benchmark.Runner.Alternative.Program
main =
    Benchmark.Runner.Alternative.program suite


suite : Benchmark
suite =
    Benchmark.describe "Dict"
        [ compare "intersect"
            { core = Dict.intersect
            , public = intersect
            , dotdot = DDD.intersect
            , private = intersectDotdot
            }
        ]


intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect l r =
    let
        llist : List ( comparable, v )
        llist =
            Dict.toList l

        rlist : List ( comparable, v )
        rlist =
            Dict.toList r

        go : List ( comparable, v ) -> List ( comparable, v ) -> List ( comparable, v )
        go lleft rleft =
            case lleft of
                [] ->
                    []

                (( lheadKey, _ ) as lhead) :: ltail ->
                    case rleft of
                        [] ->
                            []

                        ( rheadKey, _ ) :: rtail ->
                            if lheadKey == rheadKey then
                                lhead :: go ltail rtail

                            else if lheadKey < rheadKey then
                                go ltail rleft

                            else
                                go lleft rtail
    in
    Dict.fromList (go llist rlist)


intersectDotdot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
intersectDotdot l r =
    let
        llist : List ( comparable, v )
        llist =
            DDD.toList l

        rlist : List ( comparable, v )
        rlist =
            DDD.toList r

        go : List ( comparable, v ) -> List ( comparable, v ) -> List ( comparable, v )
        go lleft rleft =
            case lleft of
                [] ->
                    []

                (( lheadKey, _ ) as lhead) :: ltail ->
                    case rleft of
                        [] ->
                            []

                        ( rheadKey, _ ) :: rtail ->
                            if lheadKey == rheadKey then
                                lhead :: go ltail rtail

                            else if lheadKey < rheadKey then
                                go ltail rleft

                            else
                                go lleft rtail
    in
    DDD.fromList (go llist rlist)


type alias Both k v =
    { core : Dict k v, dotdot : DDD.Dict k v }


compare :
    String
    ->
        { core : Dict Int Int -> Dict Int Int -> Dict Int Int
        , public : Dict Int Int -> Dict Int Int -> Dict Int Int
        , dotdot : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        , private : DDD.Dict Int Int -> DDD.Dict Int Int -> DDD.Dict Int Int
        }
    -> Benchmark
compare label { core, public, dotdot, private } =
    List.range 1 1
        |> List.map
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
                        , ( "using public API", \l r -> Dict.isEmpty <| public l.core r.core )
                        , ( "elm-dot-dot", \l r -> DDD.isEmpty <| dotdot l.dotdot r.dotdot )
                        , ( "using private API", \l r -> DDD.isEmpty <| private l.dotdot r.dotdot )
                        ]

                    broken =
                        (core even.core odd.core /= public even.core odd.core)
                            || (core even.core all.core /= public even.core all.core)
                            || (core even.core even.core /= public even.core even.core)
                            || (core even.core odd.core /= fromDD (dotdot even.dotdot odd.dotdot))
                            || (core even.core all.core /= fromDD (dotdot even.dotdot all.dotdot))
                            || (core even.core even.core /= fromDD (dotdot even.dotdot even.dotdot))
                            || (core even.core odd.core /= fromDD (private even.dotdot odd.dotdot))
                            || (core even.core all.core /= fromDD (private even.dotdot all.dotdot))
                            || (core even.core even.core /= fromDD (private even.dotdot even.dotdot))
                in
                Benchmark.describe
                    (label ++ " (n = " ++ String.fromInt cap ++ ")")
                    (if broken then
                        [ Debug.toString
                            { no =
                                { core = core even.core odd.core
                                , public = public even.core odd.core
                                , dotdot = dotdot even.dotdot odd.dotdot
                                , private = private even.dotdot odd.dotdot
                                }
                            , half =
                                { core = core even.core all.core
                                , public = public even.core all.core
                                , dotdot = dotdot even.dotdot all.dotdot
                                , private = private even.dotdot all.dotdot
                                }
                            , full =
                                { core = core even.core even.core
                                , public = public even.core even.core
                                , dotdot = dotdot even.dotdot even.dotdot
                                , private = private even.dotdot even.dotdot
                                }
                            }
                            |> String.split "{"
                            |> String.join "{\n"
                            |> String.split ", "
                            |> String.join ",\n"
                            |> String.split "\npublic"
                            |> String.join "\n public"
                            |> Debug.todo
                        ]

                     else
                        [ Benchmark.Alternative.rank "no intersection" (\f -> f even odd) alternatives
                        , Benchmark.Alternative.rank "half intersection" (\f -> f even all) alternatives
                        , Benchmark.Alternative.rank "full intersection" (\f -> f even even) alternatives
                        ]
                    )
            )
        |> Benchmark.describe label


fromDD : DDD.Dict comparable v -> Dict comparable v
fromDD =
    DDD.toList >> Dict.fromList
