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
        , CustomDict.withKey (Type.named [] "Param")
            |> CustomDict.withToComparable (\param -> Elm.apply (Elm.val "toComparable") [ param ] |> Elm.withType (Type.named [] "Key"))
            |> CustomDict.withAdditionalDeclarations
                [ Elm.alias "Param" paramAnnotation |> Elm.exposeWith { exposeConstructor = False, group = Just "Types" }
                , Elm.alias "Ratio" (Type.tuple Type.int Type.int) |> Elm.exposeWith { exposeConstructor = False, group = Just "Types" }
                , Elm.alias "Key" keyAnnotation
                , Elm.fn ( "param", Just (Type.named [] "Param") )
                    (\param ->
                        Elm.triple
                            (Elm.get "section" param)
                            (Elm.get "ratio" param)
                            (Elm.get "function" param)
                    )
                    |> Elm.withType (Type.function [ Type.named [] "Param" ] keyAnnotation)
                    |> Elm.declaration "toComparable"
                ]
            |> CustomDict.generate
        ]


keyAnnotation : Type.Annotation
keyAnnotation =
    Type.triple Type.string (Type.named [] "Ratio") Type.string


paramAnnotation : Type.Annotation
paramAnnotation =
    Type.record
        [ ( "section", Type.string )
        , ( "ratio", Type.named [] "Ratio" )
        , ( "function", Type.string )
        , ( "color", Type.named [ "Color" ] "Color" )
        , ( "op", Type.function [ Type.int ] (Type.named [ "Benchmark", "LowLevel" ] "Operation") )
        ]
