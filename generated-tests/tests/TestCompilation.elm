module TestCompilation exposing (suite)

import Expect
import MaybeDict
import MaybeSet
import Test exposing (Test, describe, test)


suite : Test
suite =
    -- These tests are very dull, they're there to typecheck the generated files
    describe "Dict and set"
        [ test "Dict" <|
            \_ ->
                MaybeDict.empty
                    |> MaybeDict.size
                    |> Expect.equal 0
        , test "Set" <|
            \_ ->
                MaybeSet.empty
                    |> MaybeSet.size
                    |> Expect.equal 0
        ]
