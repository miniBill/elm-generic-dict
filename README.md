# `elm-generic-dict` [![Build Status](https://github.com/miniBill/elm-generic-dict/workflows/CI/badge.svg)](https://github.com/miniBill/elm-generic-dict/actions?query=branch%3Amain)

`elm-generic-dict` can be used to codegen dictionaries with arbitrary types as keys. It is meant to be used as a library with [`elm-codegen`](https://github.com/mdgriffith/elm-codegen).

The dictionaries don't contain functions, and the API (of the generated code) doesn't require you to pass any function.

The main disadvantage of using this approach is code duplication, while the advantages are:
1. does not contain functions (useful for debugging and usage in lamdera),
2. does not require passing in functions when using it (cleaner API, no possibility of mistakes),
3. live code inclusion will trim unused functions.


### Example

Say you'd like a `Dict` of `UUID.UUID`: 

* First, follow elm-codegen's [docs](https://github.com/mdgriffith/elm-codegen/blob/main/guide/UsingHelpers.md), on installing packages
* edit the `codegen/Generate.elm` to look something like

```elm
module Generate exposing (main)

import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate
import Gen.UUID as UUID
import GenericDict


main : Program {} () ()
main =
    Generate.run
        [ customDictFile
        ]


customDictFile : Elm.File
customDictFile =
    GenericDict.init
        { keyType = Type.namedWith [ "UUID" ] "UUID" []
        , namespace = []
        , toComparable = UUID.toString
        }
        |> GenericDict.withTypeName "UuidDict"
        |> GenericDict.generateFile

```

* run `elm-codegen run` to have it create the `generated/UuidDict.elm`
* update your `elm.json` to include `generated/` as a `source-directory`
* import it as `Import UuidDict` to use it like a `UuidDict UUID.UUID a`
