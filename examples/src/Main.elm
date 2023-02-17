module Main exposing (main)

import Html exposing (Html)
import IdDict exposing (Id(..), IdDict)


main : Html msg
main =
    Html.text <|
        if test then
            "Empty"

        else
            "Nonempty"


dict : IdDict String
dict =
    [ ( Id "foo", "bar" ) ]
        |> IdDict.fromList


test : Bool
test =
    IdDict.isEmpty dict
