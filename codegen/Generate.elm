module Generate exposing (main)

{-| -}

import CustomDict
import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.Helper


main : Program {} () ()
main =
    Generate.run
        [ CustomDict.withKey (Type.var "comparable")
            |> CustomDict.generate
        ]
