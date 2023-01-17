module Intersect exposing (folding, folding_DotDot, recursion_DotDot, recursion_thrice_DotDot, recursion_thrice_fromArray_DotDot, recursion_thrice_fromList_DotDot, recursion_twice_DotDot, toList, toList_DotDot)

import Array exposing (Array)
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


recursion_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
recursion_DotDot l r =
    let
        rkeys : List comparable
        rkeys =
            DDD.keys r

        go : ( DDD.Dict comparable v, List comparable ) -> DDD.Dict comparable v -> ( DDD.Dict comparable v, List comparable )
        go (( dacc, rleft ) as acc) lnode =
            case lnode of
                DDD.RBEmpty_elm_builtin ->
                    acc

                DDD.RBBlackMissing_elm_builtin c ->
                    go acc c

                DDD.RBNode_elm_builtin _ lkey lvalue childLT childGT ->
                    case rleft of
                        [] ->
                            acc

                        rhead :: rtail ->
                            if rhead > lkey then
                                -- We can skip the left tree and this node
                                go acc childGT

                            else if rhead == lkey then
                                -- We can skip the left tree, and insert this node
                                go ( DDD.insert lkey lvalue dacc, rtail ) childGT

                            else
                                let
                                    (( daccAL, rleftAL ) as afterLeft) =
                                        go acc childLT
                                in
                                case List.Extra.dropWhile (\rkey -> rkey < lkey) rleftAL of
                                    [] ->
                                        afterLeft

                                    rheadAL :: rtailAL ->
                                        if rheadAL == lkey then
                                            go ( DDD.insert lkey lvalue daccAL, rtailAL ) childGT

                                        else
                                            go afterLeft childGT
    in
    go ( DDD.empty, rkeys ) l
        |> Tuple.first


type QueueNode comparable v
    = KeyValue comparable v
    | Tree (DDD.Dict comparable v)


type alias QueueState comparable v =
    Maybe ( comparable, v, List (QueueNode comparable v) )


recursion_twice_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
recursion_twice_DotDot l r =
    let
        unpack : List (QueueNode comparable v) -> QueueState comparable v
        unpack queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case h of
                        KeyValue k v ->
                            Just ( k, v, t )

                        Tree (DDD.RBNode_elm_builtin _ key value DDD.RBEmpty_elm_builtin childGT) ->
                            Just ( key, value, Tree childGT :: t )

                        Tree (DDD.RBNode_elm_builtin _ key value childLT childGT) ->
                            unpack (Tree childLT :: KeyValue key value :: Tree childGT :: t)

                        Tree DDD.RBEmpty_elm_builtin ->
                            unpack t

                        Tree (DDD.RBBlackMissing_elm_builtin c) ->
                            -- This doesn't happen in practice, performance is irrelevant
                            unpack (Tree c :: t)

        unpackWhileDroppingLT : comparable -> List (QueueNode comparable v) -> QueueState comparable v
        unpackWhileDroppingLT compareKey queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case h of
                        KeyValue k v ->
                            if k < compareKey then
                                unpackWhileDroppingLT compareKey t

                            else
                                Just ( k, v, t )

                        Tree (DDD.RBNode_elm_builtin _ key value DDD.RBEmpty_elm_builtin childGT) ->
                            if key < compareKey then
                                unpackWhileDroppingLT compareKey (Tree childGT :: t)

                            else
                                Just ( key, value, Tree childGT :: t )

                        Tree (DDD.RBNode_elm_builtin _ key value childLT childGT) ->
                            if key < compareKey then
                                unpackWhileDroppingLT compareKey (Tree childGT :: t)

                            else if key == compareKey then
                                unpackWhileDroppingLT compareKey (KeyValue key value :: Tree childGT :: t)

                            else
                                unpackWhileDroppingLT compareKey (Tree childLT :: KeyValue key value :: Tree childGT :: t)

                        Tree DDD.RBEmpty_elm_builtin ->
                            unpackWhileDroppingLT compareKey t

                        Tree (DDD.RBBlackMissing_elm_builtin c) ->
                            -- This doesn't happen in practice, performance is irrelevant
                            unpackWhileDroppingLT compareKey (Tree c :: t)

        go : DDD.Dict comparable v -> QueueState comparable v -> QueueState comparable v -> DDD.Dict comparable v
        go dacc lleft rleft =
            case lleft of
                Nothing ->
                    dacc

                Just ( lkey, lvalue, ltail ) ->
                    case rleft of
                        Nothing ->
                            dacc

                        Just ( rkey, _, rtail ) ->
                            if lkey < rkey then
                                go dacc (unpackWhileDroppingLT rkey ltail) rleft

                            else if lkey > rkey then
                                go dacc lleft (unpackWhileDroppingLT lkey rtail)

                            else
                                go (DDD.insert lkey lvalue dacc) (unpack ltail) (unpack rtail)
    in
    go DDD.empty (unpack [ Tree l ]) (unpack [ Tree r ])


type alias State comparable v =
    Maybe ( comparable, v, List (DDD.Dict comparable v) )


recursion_thrice_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
recursion_thrice_DotDot l r =
    let
        unpack : List (DDD.Dict comparable v) -> State comparable v
        unpack queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case h of
                        DDD.RBNode_elm_builtin _ key value DDD.RBEmpty_elm_builtin childGT ->
                            Just ( key, value, childGT :: t )

                        DDD.RBNode_elm_builtin color key value childLT childGT ->
                            unpack (childLT :: DDD.RBNode_elm_builtin color key value DDD.RBEmpty_elm_builtin childGT :: t)

                        DDD.RBEmpty_elm_builtin ->
                            unpack t

                        DDD.RBBlackMissing_elm_builtin c ->
                            -- This doesn't happen in practice, performance is irrelevant
                            unpack (c :: t)

        unpackWhileDroppingLT : comparable -> List (DDD.Dict comparable v) -> State comparable v
        unpackWhileDroppingLT compareKey queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case h of
                        DDD.RBNode_elm_builtin color key value childLT childGT ->
                            if key < compareKey then
                                unpackWhileDroppingLT compareKey (childGT :: t)

                            else if key == compareKey then
                                Just ( key, value, childGT :: t )

                            else
                                case childLT of
                                    DDD.RBEmpty_elm_builtin ->
                                        Just ( key, value, childGT :: t )

                                    _ ->
                                        unpackWhileDroppingLT compareKey (childLT :: DDD.RBNode_elm_builtin color key value DDD.RBEmpty_elm_builtin childGT :: t)

                        DDD.RBEmpty_elm_builtin ->
                            unpackWhileDroppingLT compareKey t

                        DDD.RBBlackMissing_elm_builtin c ->
                            -- This doesn't happen in practice, performance is irrelevant
                            unpackWhileDroppingLT compareKey (c :: t)

        go : DDD.Dict comparable v -> State comparable v -> State comparable v -> DDD.Dict comparable v
        go dacc lleft rleft =
            case lleft of
                Nothing ->
                    dacc

                Just ( lkey, lvalue, ltail ) ->
                    case rleft of
                        Nothing ->
                            dacc

                        Just ( rkey, _, rtail ) ->
                            if lkey < rkey then
                                go dacc (unpackWhileDroppingLT rkey ltail) rleft

                            else if lkey > rkey then
                                go dacc lleft (unpackWhileDroppingLT lkey rtail)

                            else
                                go (DDD.insert lkey lvalue dacc) (unpack ltail) (unpack rtail)
    in
    go DDD.empty (unpack [ l ]) (unpack [ r ])


recursion_thrice_fromArray_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
recursion_thrice_fromArray_DotDot l r =
    let
        unpack : List (DDD.Dict comparable v) -> State comparable v
        unpack queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case h of
                        DDD.RBNode_elm_builtin _ key value DDD.RBEmpty_elm_builtin childGT ->
                            Just ( key, value, childGT :: t )

                        DDD.RBNode_elm_builtin color key value childLT childGT ->
                            unpack (childLT :: DDD.RBNode_elm_builtin color key value DDD.RBEmpty_elm_builtin childGT :: t)

                        DDD.RBEmpty_elm_builtin ->
                            unpack t

                        DDD.RBBlackMissing_elm_builtin c ->
                            -- This doesn't happen in practice, performance is irrelevant
                            unpack (c :: t)

        unpackWhileDroppingLT : comparable -> List (DDD.Dict comparable v) -> State comparable v
        unpackWhileDroppingLT compareKey queue =
            case queue of
                [] ->
                    Nothing

                h :: t ->
                    case h of
                        DDD.RBNode_elm_builtin color key value childLT childGT ->
                            if key < compareKey then
                                unpackWhileDroppingLT compareKey (childGT :: t)

                            else if key == compareKey then
                                Just ( key, value, childGT :: t )

                            else
                                case childLT of
                                    DDD.RBEmpty_elm_builtin ->
                                        Just ( key, value, childGT :: t )

                                    _ ->
                                        unpackWhileDroppingLT compareKey (childLT :: DDD.RBNode_elm_builtin color key value DDD.RBEmpty_elm_builtin childGT :: t)

                        DDD.RBEmpty_elm_builtin ->
                            unpackWhileDroppingLT compareKey t

                        DDD.RBBlackMissing_elm_builtin c ->
                            -- This doesn't happen in practice, performance is irrelevant
                            unpackWhileDroppingLT compareKey (c :: t)

        go : Array ( comparable, v ) -> State comparable v -> State comparable v -> Array ( comparable, v )
        go dacc lleft rleft =
            case lleft of
                Nothing ->
                    dacc

                Just ( lkey, lvalue, ltail ) ->
                    case rleft of
                        Nothing ->
                            dacc

                        Just ( rkey, _, rtail ) ->
                            if lkey < rkey then
                                go dacc (unpackWhileDroppingLT rkey ltail) rleft

                            else if lkey > rkey then
                                go dacc lleft (unpackWhileDroppingLT lkey rtail)

                            else
                                go (Array.push ( lkey, lvalue ) dacc) (unpack ltail) (unpack rtail)
    in
    go Array.empty (unpack [ l ]) (unpack [ r ])
        |> fromSortedArray


fromSortedArray : Array ( comparable, v ) -> DDD.Dict comparable v
fromSortedArray arr =
    let
        len : Int
        len =
            Array.length arr

        redLayer : Int
        redLayer =
            floor (logBase 2 (toFloat len))

        go : Int -> Int -> Int -> DDD.Dict comparable v
        go layer fromIncluded toExcluded =
            if fromIncluded >= toExcluded then
                DDD.RBEmpty_elm_builtin

            else
                let
                    mid : Int
                    mid =
                        fromIncluded + (toExcluded - fromIncluded) // 2
                in
                case Array.get mid arr of
                    Nothing ->
                        DDD.RBEmpty_elm_builtin

                    Just ( k, v ) ->
                        DDD.RBNode_elm_builtin
                            (if layer == redLayer then
                                DDD.Red

                             else
                                DDD.Black
                            )
                            k
                            v
                            (go (layer + 1) fromIncluded mid)
                            (go (layer + 1) (mid + 1) toExcluded)
    in
    go 0 0 len


recursion_thrice_fromList_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
recursion_thrice_fromList_DotDot l r =
    intersectFromZipper
        ( 0, [] )
        (unconsBiggest [ l ])
        (unconsBiggest [ r ])
        |> fromSortedList


unconsBiggest : List (DDD.Dict comparable v) -> State comparable v
unconsBiggest queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                DDD.RBNode_elm_builtin _ key value childLT DDD.RBEmpty_elm_builtin ->
                    Just ( key, value, childLT :: t )

                DDD.RBNode_elm_builtin color key value childLT childGT ->
                    unconsBiggest (childGT :: DDD.RBNode_elm_builtin color key value childLT DDD.RBEmpty_elm_builtin :: t)

                DDD.RBEmpty_elm_builtin ->
                    unconsBiggest t

                DDD.RBBlackMissing_elm_builtin c ->
                    -- This doesn't happen in practice, performance is irrelevant
                    unconsBiggest (c :: t)


unconsBiggestWhileDroppingGT : comparable -> List (DDD.Dict comparable v) -> State comparable v
unconsBiggestWhileDroppingGT compareKey queue =
    case queue of
        [] ->
            Nothing

        h :: t ->
            case h of
                DDD.RBNode_elm_builtin color key value childLT childGT ->
                    if key > compareKey then
                        unconsBiggestWhileDroppingGT compareKey (childLT :: t)

                    else if key == compareKey then
                        Just ( key, value, childLT :: t )

                    else
                        case childGT of
                            DDD.RBEmpty_elm_builtin ->
                                Just ( key, value, childLT :: t )

                            _ ->
                                unconsBiggestWhileDroppingGT compareKey (childGT :: DDD.RBNode_elm_builtin color key value childLT DDD.RBEmpty_elm_builtin :: t)

                DDD.RBEmpty_elm_builtin ->
                    unconsBiggestWhileDroppingGT compareKey t

                DDD.RBBlackMissing_elm_builtin c ->
                    -- This doesn't happen in practice, performance is irrelevant
                    unconsBiggestWhileDroppingGT compareKey (c :: t)


intersectFromZipper : ( Int, List ( comparable, v ) ) -> State comparable v -> State comparable v -> ( Int, List ( comparable, v ) )
intersectFromZipper (( dsize, dlist ) as dacc) lleft rleft =
    case lleft of
        Nothing ->
            dacc

        Just ( lkey, lvalue, ltail ) ->
            case rleft of
                Nothing ->
                    dacc

                Just ( rkey, _, rtail ) ->
                    if lkey > rkey then
                        intersectFromZipper dacc (unconsBiggestWhileDroppingGT rkey ltail) rleft

                    else if lkey < rkey then
                        intersectFromZipper dacc lleft (unconsBiggestWhileDroppingGT lkey rtail)

                    else
                        intersectFromZipper ( dsize + 1, ( lkey, lvalue ) :: dlist ) (unconsBiggest ltail) (unconsBiggest rtail)


fromSortedList : ( Int, List ( comparable, v ) ) -> DDD.Dict comparable v
fromSortedList ( len, arr ) =
    let
        redLayer : Int
        redLayer =
            floor (logBase 2 (toFloat len))

        go : Int -> Int -> Int -> List ( comparable, v ) -> ( DDD.Dict comparable v, List ( comparable, v ) )
        go layer fromIncluded toExcluded acc =
            if fromIncluded >= toExcluded then
                ( DDD.RBEmpty_elm_builtin, acc )

            else
                let
                    mid : Int
                    mid =
                        fromIncluded + (toExcluded - fromIncluded) // 2

                    ( lchild, accAfterLeft ) =
                        go (layer + 1) fromIncluded mid acc
                in
                case accAfterLeft of
                    [] ->
                        ( DDD.RBEmpty_elm_builtin, acc )

                    ( k, v ) :: tail ->
                        let
                            ( rchild, accAfterRight ) =
                                go (layer + 1) (mid + 1) toExcluded tail

                            color : DDD.NColor
                            color =
                                if layer == redLayer then
                                    DDD.Red

                                else
                                    DDD.Black
                        in
                        ( DDD.RBNode_elm_builtin color k v lchild rchild
                        , accAfterRight
                        )
    in
    go 0 0 len arr
        |> Tuple.first
