module Generate exposing (main)

{-|

@docs main

-}

import CustomDict
import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Maybe


{-| Generate an example custom dictionary for `comparable`.
-}
main : Program {} () ()
main =
    Generate.run
        [ CustomDict.withKey (Type.list (Type.var "comparable"))
            |> CustomDict.generate
        , CustomDict.withKey (Gen.Maybe.annotation_.maybe Type.int)
            |> CustomDict.withToComparable (Gen.Maybe.withDefault (Elm.int 0))
            |> CustomDict.generate
        ]
