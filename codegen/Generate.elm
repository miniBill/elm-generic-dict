module Generate exposing (main)

{-|

@docs main

-}

import CustomDict
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate


{-| Generate an example custom dictionary for `comparable`.
-}
main : Program {} () ()
main =
    Generate.run
        [ CustomDict.withKey (Type.list (Type.var "comparable"))
            |> CustomDict.generate
        ]
