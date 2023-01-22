module List.MyExtra exposing (gatherEqualsByAnd)

import List.Extra


gatherEqualsByAnd : (item -> key) -> (item -> value) -> List item -> List ( key, List value )
gatherEqualsByAnd toKey toValue items =
    items
        |> List.Extra.gatherEqualsBy toKey
        |> List.map (\( h, t ) -> ( toKey h, List.map toValue (h :: t) ))
