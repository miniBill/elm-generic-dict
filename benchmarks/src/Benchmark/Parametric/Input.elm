module Benchmark.Parametric.Input exposing (Input(..), list, map2, step)


type Input t
    = Input (Maybe ( t, () -> Input t ))


list : List t -> Input t
list lst =
    case lst of
        [] ->
            Input Nothing

        h :: t ->
            Input (Just ( h, \_ -> list t ))


step : Input t -> Maybe ( t, Input t )
step (Input p) =
    Maybe.map
        (\( current, next ) -> ( current, next () ))
        p


map2 : (a -> b -> c) -> Input a -> Input b -> Input c
map2 f (Input l) (Input r) =
    case ( l, r ) of
        ( Just ( a, nextL ), Just ( b, nextR ) ) ->
            Input (Just ( f a b, \_ -> map2 f (nextL ()) (nextR ()) ))

        _ ->
            Input Nothing
