module CustomDict exposing
    ( withKey
    , generate
    )

{-|

@docs withKey

@docs generate

-}

import Elm
import Elm.Annotation as Type


type Config
    = Config
        { keyType : Type.Annotation
        , namespace : List String
        }


generate : Config -> Elm.File
generate (Config { namespace, keyType }) =
    let
        typeName =
            "CD"

        decls =
            [ Elm.declaration "hello"
                (Elm.string "World!")
            ]
    in
    Elm.file (namespace ++ [ typeName ])
        decls


withKey : Type.Annotation -> Config
withKey keyType =
    Config
        { keyType = keyType
        , namespace = []
        }
