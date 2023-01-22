module Benchmark.Parametric.Input exposing (Input(..), allTuples, fromList, getCodec, map, toList, zip)

import Codec exposing (Codec)


type Input t
    = Input (List t) (Codec t)


fromList : List t -> Codec t -> Input t
fromList =
    Input


toList : Input t -> List t
toList (Input list _) =
    list


getCodec : Input t -> Codec t
getCodec (Input _ codec) =
    codec


{-| Return all possible combinations of the two inputs.
-}
allTuples : Input a -> Input b -> Input ( a, b )
allTuples (Input llist lcodec) (Input rlist rcodec) =
    Input
        (List.concatMap (\lelem -> List.map (Tuple.pair lelem) rlist) llist)
        (Codec.tuple lcodec rcodec)


{-| Zip the two inputs together, discarding extra elements from the longer list.
-}
zip : Input a -> Input b -> Input ( a, b )
zip (Input llist lcodec) (Input rlist rcodec) =
    Input
        (List.map2 Tuple.pair llist rlist)
        (Codec.tuple lcodec rcodec)


map : (a -> b) -> Codec b -> Input a -> Input b
map f codec (Input list _) =
    Input (List.map f list) codec
