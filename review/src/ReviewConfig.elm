module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.Modules.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Gen/" ]
    , NoUnused.Dependencies.rule
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Variables.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule

    --, NoInconsistentAliases.config
    --    [ ( "Html.Attributes", "Attr" )
    --    , ( "Json.Decode", "Decode" )
    --    , ( "Json.Encode", "Encode" )
    --    ]
    --    |> NoInconsistentAliases.noMissingAliases
    --    |> NoInconsistentAliases.rule
    , NoModuleOnExposedNames.rule
    , Docs.NoMissing.rule
        { document = onlyExposed
        , from = exposedModules
        }
    , Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    , NoConfusingPrefixOperator.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
    , NoPrematureLetComputation.rule
    , Simplify.rule Simplify.defaults
    ]
        |> List.map
            (\rule ->
                rule
                    |> Rule.ignoreErrorsForDirectories
                        [ "tests/VerifyExamples" -- this is a generated folder
                        , "src/Gen"
                        ]
            )
