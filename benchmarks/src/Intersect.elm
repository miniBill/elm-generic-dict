module Intersect exposing (folding, folding_DotDot, toList, toList_DotDot)

import Dict exposing (Dict)
import DictDotDot as DDD
import List.Extra


toList : Dict comparable v -> Dict comparable v -> Dict comparable v
toList l r =
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


folding : Dict comparable v -> Dict comparable v -> Dict comparable v
folding l r =
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


toList_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
toList_DotDot l r =
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


folding_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
folding_DotDot l r =
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
