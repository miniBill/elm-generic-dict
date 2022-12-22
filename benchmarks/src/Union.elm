module Union exposing (toList, toList_DotDot)

import Dict exposing (Dict)
import DictDotDot as DDD



-- import List.Extra


toList : Dict comparable v -> Dict comparable v -> Dict comparable v
toList l r =
    let
        go : Dict comparable v -> List ( comparable, v ) -> List ( comparable, v ) -> Dict comparable v
        go acc lleft rleft =
            case lleft of
                [] ->
                    List.foldl (\( rkey, rvalue ) -> Dict.insert rkey rvalue) acc rleft

                ( lheadKey, lheadValue ) :: ltail ->
                    case rleft of
                        [] ->
                            List.foldl (\( lkey, lvalue ) -> Dict.insert lkey lvalue) acc lleft

                        ( rheadKey, rheadValue ) :: rtail ->
                            if lheadKey < rheadKey then
                                go (Dict.insert lheadKey lheadValue acc) ltail rleft

                            else if lheadKey == rheadKey then
                                go (Dict.insert lheadKey lheadValue acc) ltail rtail

                            else
                                go (Dict.insert rheadKey rheadValue acc) lleft rtail
    in
    go Dict.empty (Dict.toList l) (Dict.toList r)



-- folding : Dict comparable v -> Dict comparable v -> Dict comparable v
-- folding l r =
--     Dict.foldl
--         (\lkey lvalue ( acc, queue ) ->
--             case List.Extra.dropWhile (\rkey -> rkey < lkey) queue of
--                 [] ->
--                     ( acc, [] )
--                 (qhead :: qtail) as newQueue ->
--                     if qhead == lkey then
--                         ( Dict.insert lkey lvalue acc, qtail )
--                     else
--                         ( acc, newQueue )
--         )
--         ( Dict.empty, Dict.keys r )
--         l
--         |> Tuple.first


toList_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
toList_DotDot l r =
    let
        go : DDD.Dict comparable v -> List ( comparable, v ) -> List ( comparable, v ) -> DDD.Dict comparable v
        go acc lleft rleft =
            case lleft of
                [] ->
                    List.foldl (\( rkey, rvalue ) -> DDD.insert rkey rvalue) acc rleft

                ( lheadKey, lheadValue ) :: ltail ->
                    case rleft of
                        [] ->
                            List.foldl (\( lkey, lvalue ) -> DDD.insert lkey lvalue) acc lleft

                        ( rheadKey, rheadValue ) :: rtail ->
                            if lheadKey < rheadKey then
                                go (DDD.insert lheadKey lheadValue acc) ltail rleft

                            else if lheadKey == rheadKey then
                                go (DDD.insert lheadKey lheadValue acc) ltail rtail

                            else
                                go (DDD.insert rheadKey rheadValue acc) lleft rtail
    in
    go DDD.empty (DDD.toList l) (DDD.toList r)



-- folding_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
-- folding_DotDot l r =
--     DDD.foldl
--         (\lkey lvalue ( acc, queue ) ->
--             case List.Extra.dropWhile (\rkey -> rkey < lkey) queue of
--                 [] ->
--                     ( acc, [] )
--                 (qhead :: qtail) as newQueue ->
--                     if qhead == lkey then
--                         ( DDD.insert lkey lvalue acc, qtail )
--                     else
--                         ( acc, newQueue )
--         )
--         ( DDD.empty, DDD.keys r )
--         l
--         |> Tuple.first
-- recursion_DotDot : DDD.Dict comparable v -> DDD.Dict comparable v -> DDD.Dict comparable v
-- recursion_DotDot l r =
--     let
--         rkeys : List comparable
--         rkeys =
--             DDD.keys r
--         go : ( DDD.Dict comparable v, List comparable ) -> DDD.Dict comparable v -> ( DDD.Dict comparable v, List comparable )
--         go (( dacc, rleft ) as acc) lnode =
--             case lnode of
--                 DDD.RBEmpty_elm_builtin ->
--                     acc
--                 DDD.RBBlackMissing_elm_builtin c ->
--                     go acc c
--                 DDD.RBNode_elm_builtin _ lkey lvalue childLT childGT ->
--                     case rleft of
--                         [] ->
--                             acc
--                         rhead :: rtail ->
--                             if rhead > lkey then
--                                 -- We can skip the left tree and this node
--                                 go acc childGT
--                             else if rhead == lkey then
--                                 -- We can skip the left tree, and insert this node
--                                 go ( DDD.insert lkey lvalue dacc, rtail ) childGT
--                             else
--                                 let
--                                     (( daccAL, rleftAL ) as afterLeft) =
--                                         go acc childLT
--                                 in
--                                 case List.Extra.dropWhile (\rkey -> rkey < lkey) rleftAL of
--                                     [] ->
--                                         afterLeft
--                                     rheadAL :: rtailAL ->
--                                         if rheadAL == lkey then
--                                             go ( DDD.insert lkey lvalue daccAL, rtailAL ) childGT
--                                         else
--                                             go afterLeft childGT
--     in
--     go ( DDD.empty, rkeys ) l
--         |> Tuple.first
