module Generate exposing (main)

{-|

@docs main

-}

import CustomDict
import Elm
import Elm.Annotation as Type
import Elm.Case
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
                [ Elm.alias "Param" paramAnnotation
                    |> Elm.exposeWith { exposeConstructor = False, group = Just "Types" }
                , overlapCases
                    |> List.map Elm.variant
                    |> Elm.customType "Overlap"
                    |> Elm.exposeWith { exposeConstructor = True, group = Just "Types" }
                , Elm.alias "Ratio" (Type.tuple Type.int Type.int)
                    |> Elm.exposeWith { exposeConstructor = False, group = Just "Types" }
                , Elm.alias "Key" paramComparableAnnotation
                , paramToComparable
                , overlapToString
                ]
            |> CustomDict.generate
        ]


overlapCases : List String
overlapCases =
    [ "OverlapRandom"
    , "OverlapFull"
    , "OverlapNoneLeftLower"
    , "OverlapNoneRightLower"
    , "OverlapNoneEvenOdd"
    ]


paramToComparable : Elm.Declaration
paramToComparable =
    Elm.fn ( "param", Just (Type.named [] "Param") )
        (\param ->
            Elm.tuple
                (Elm.tuple (Elm.get "section" param)
                    (Elm.get "ratio" param)
                )
                (Elm.tuple
                    (Elm.get "function" param)
                    (Elm.apply (Elm.val "overlapToString") [ Elm.get "overlap" param ])
                )
        )
        |> Elm.withType (Type.function [ Type.named [] "Param" ] paramComparableAnnotation)
        |> Elm.declaration "toComparable"


overlapToString : Elm.Declaration
overlapToString =
    Elm.fn ( "overlap", Just (Type.named [] "Overlap") )
        (\overlap ->
            overlapCases
                |> List.map (\case_ -> Elm.Case.branch0 case_ (Elm.string case_))
                |> Elm.Case.custom overlap (Type.named [] "Overlap")
        )
        |> Elm.declaration "overlapToString"


paramComparableAnnotation : Type.Annotation
paramComparableAnnotation =
    Type.tuple
        (Type.tuple Type.string (Type.named [] "Ratio"))
        (Type.tuple Type.string Type.string)


paramAnnotation : Type.Annotation
paramAnnotation =
    Type.record
        [ ( "section", Type.string )
        , ( "ratio", Type.named [] "Ratio" )
        , ( "function", Type.string )
        , ( "color", Type.named [ "Color" ] "Color" )
        , ( "op", Type.function [ Type.int ] (Type.named [ "Benchmark", "LowLevel" ] "Operation") )
        , ( "overlap", Type.named [] "Overlap" )
        ]
