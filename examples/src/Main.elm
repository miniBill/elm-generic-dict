module Main exposing (main)

import Html exposing (Html)
import IdDict exposing (Id(..), IdDict)


main : Html msg
main =
    dict
        |> IdDict.get (Id "foo")
        |> Maybe.withDefault "not found"
        |> Html.text


dict : IdDict String
dict =
    [ ( Id "foo", "bar" ) ]
        |> IdDict.fromList


test : Bool
test =
    IdDict.isEmpty dict
