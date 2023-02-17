module Generate exposing (main)

{-|

@docs main

-}

import Elm
import Elm.Annotation as Type
import Elm.Case
import Gen.CodeGen.Generate as Generate
import Gen.Maybe
import GenericDict


{-| Generate an example custom dictionary for `comparable`.
-}
main : Program {} () ()
main =
    [ GenericDict.init
        { keyType = Type.list (Type.var "comparable")
        , namespace = []
        , toComparable = identity
        }
        |> GenericDict.generateFile
    , GenericDict.init
        { keyType = Gen.Maybe.annotation_.maybe Type.int
        , toComparable = Gen.Maybe.withDefault (Elm.int 0)
        , namespace = []
        }
        |> GenericDict.generateFile
    , let
        idType : Type.Annotation
        idType =
            Type.namedWith [] "Id" []

        idDecl : Elm.Declaration
        idDecl =
            Elm.customType "Id"
                [ Elm.variantWith "Id" [ Type.string ]
                ]

        decls : List Elm.Declaration
        decls =
            GenericDict.init
                { keyType = idType
                , toComparable =
                    \e ->
                        Elm.Case.custom e
                            idType
                            [ Elm.Case.branch1 "Id" ( "id", Type.string ) identity
                            ]
                , namespace = []
                }
                |> GenericDict.useElmFastDict
                |> GenericDict.generateDeclarations
      in
      Elm.file [ "IdDict" ] (idDecl :: decls)
    ]
        |> Generate.run
