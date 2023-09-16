module Internal exposing (getVariables, isLower)

import Elm.Annotation exposing (Annotation)
import Elm.ToString



--- Hic Sunt Leones ---


getVariables : Annotation -> List String
getVariables annotation =
    -- TODO: fix this
    Elm.ToString.annotation annotation
        |> .signature
        -- I'm sorry
        |> String.replace "(" " "
        |> String.replace ")" " "
        |> String.replace "," " "
        -- Really sorry
        |> String.split " "
        |> List.map String.trim
        |> List.filter isLower


isLower : String -> Bool
isLower s =
    let
        first : String
        first =
            String.left 1 s
    in
    String.toLower first == first
