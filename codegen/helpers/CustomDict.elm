module CustomDict exposing
    ( Config
    , withKey
    , withToComparable
    , generate
    )

{-|

@docs Config

@docs withKey

@docs withToComparable

@docs generate

-}

import Elm
import Elm.Annotation as Type
import Elm.Case
import Elm.ToString
import Gen.Dict
import String.Extra


type Config
    = Config
        { keyType : Type.Annotation
        , namespace : List String
        , toComparable : Elm.Expression -> Elm.Expression
        }


withKey : Type.Annotation -> Config
withKey keyType =
    Config
        { keyType = keyType
        , namespace = []
        , toComparable = identity
        }


withToComparable : (Elm.Expression -> Elm.Expression) -> Config -> Config
withToComparable toComparable (Config config) =
    Config { config | toComparable = toComparable }


generate : Config -> Elm.File
generate (Config { keyType, toComparable, namespace }) =
    let
        keyTypeName : String
        keyTypeName =
            (Elm.ToString.annotation keyType).signature
                |> String.split " "
                |> List.head
                |> Maybe.withDefault "???"
                |> String.Extra.classify

        dictTypeName : String
        dictTypeName =
            keyTypeName ++ "Dict"

        comparableType : Type.Annotation
        comparableType =
            -- TODO: fix this
            Elm.val "value"
                |> Elm.withType keyType
                |> toComparable
                |> Elm.ToString.expression
                |> .signature
                |> String.split " "
                |> (\l ->
                        case l of
                            [] ->
                                Type.var "unknown"

                            h :: ts ->
                                Type.namedWith []
                                    h
                                    (List.map
                                        (\t ->
                                            if isLower t then
                                                Type.var t

                                            else
                                                Type.named [] t
                                        )
                                        ts
                                    )
                   )

        annotation : String -> Type.Annotation
        annotation value =
            Type.namedWith namespace dictTypeName <|
                List.map Type.var <|
                    getVariables keyType
                        ++ [ value ]

        utils : Utils
        utils =
            { dictTypeName = dictTypeName
            , annotation = annotation
            , comparableType = comparableType
            , keyType = keyType
            , toComparable = toComparable
            }

        decls : List Elm.Declaration
        decls =
            [ typeDeclaration
            , emptyDeclaration
            , singletonDeclaration
            , insertDeclaration
            , updateDeclaration
            , removeDeclaration
            , isEmptyDeclaration
            , memberDeclaration
            , getDeclaration
            , sizeDeclaration
            ]
                |> List.map (\f -> f utils)
    in
    Elm.file (namespace ++ [ dictTypeName ]) decls


type alias Utils =
    { dictTypeName : String
    , keyType : Type.Annotation
    , annotation : String -> Type.Annotation
    , comparableType : Type.Annotation
    , toComparable : Elm.Expression -> Elm.Expression
    }


typeDeclaration : Utils -> Elm.Declaration
typeDeclaration { dictTypeName, comparableType } =
    Elm.customType dictTypeName
        [ Elm.variantWith dictTypeName
            [ Gen.Dict.annotation_.dict
                comparableType
                (Type.var "v")
            ]
        ]
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Dictionaries" }


emptyDeclaration : Utils -> Elm.Declaration
emptyDeclaration utils =
    build utils "v" Gen.Dict.empty
        |> Elm.declaration "empty"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


singletonDeclaration : Utils -> Elm.Declaration
singletonDeclaration ({ keyType, toComparable } as utils) =
    Elm.fn2
        ( "key", Just keyType )
        ( "value", Just <| Type.var "v" )
        (\key value ->
            build utils "v" (Gen.Dict.singleton (toComparable key) value)
        )
        |> Elm.declaration "singleton"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


insertDeclaration : Utils -> Elm.Declaration
insertDeclaration ({ annotation, keyType, toComparable } as utils) =
    Elm.fn3
        ( "key", Just keyType )
        ( "value", Just <| Type.var "v" )
        ( "d", Just (annotation "v") )
        (\key value ->
            decomposeDict utils
                (\dict ->
                    build utils
                        "v"
                        (Gen.Dict.insert (toComparable key) value dict)
                )
        )
        |> Elm.declaration "insert"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


updateDeclaration : Utils -> Elm.Declaration
updateDeclaration ({ keyType, annotation, toComparable } as utils) =
    Elm.fn3
        ( "key", Just keyType )
        ( "f"
        , Just <|
            Type.function
                [ Type.maybe <| Type.var "v" ]
                (Type.maybe <| Type.var "v")
        )
        ( "d", Just (annotation "v") )
        (\key updater ->
            decomposeDict utils
                (\dict ->
                    build utils
                        "v"
                        (Gen.Dict.update (toComparable key)
                            (\e ->
                                Elm.apply updater [ e ]
                            )
                            dict
                        )
                )
        )
        |> Elm.declaration "update"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


removeDeclaration : Utils -> Elm.Declaration
removeDeclaration ({ keyType, annotation, toComparable } as utils) =
    Elm.fn2
        ( "key", Just keyType )
        ( "d", Just (annotation "v") )
        (\key ->
            decomposeDict utils
                (\dict ->
                    build utils
                        "v"
                        (Gen.Dict.remove (toComparable key)
                            dict
                        )
                )
        )
        |> Elm.declaration "remove"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


isEmptyDeclaration : Utils -> Elm.Declaration
isEmptyDeclaration ({ annotation } as utils) =
    Elm.fn
        ( "d", Just (annotation "v") )
        (decomposeDict utils
            (\dict ->
                build utils
                    "v"
                    (Gen.Dict.isEmpty
                        dict
                    )
            )
        )
        |> Elm.declaration "isEmpty"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


memberDeclaration : Utils -> Elm.Declaration
memberDeclaration ({ keyType, annotation, toComparable } as utils) =
    Elm.fn2
        ( "key", Just keyType )
        ( "d", Just (annotation "v") )
        (\key ->
            decomposeDict utils
                (\dict ->
                    build utils
                        "v"
                        (Gen.Dict.member (toComparable key)
                            dict
                        )
                )
        )
        |> Elm.declaration "member"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


getDeclaration : Utils -> Elm.Declaration
getDeclaration ({ keyType, annotation, toComparable } as utils) =
    Elm.fn2
        ( "key", Just keyType )
        ( "d", Just (annotation "v") )
        (\key ->
            decomposeDict utils
                (\dict ->
                    Gen.Dict.get (toComparable key)
                        dict
                )
        )
        |> Elm.declaration "get"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


sizeDeclaration : Utils -> Elm.Declaration
sizeDeclaration ({ annotation } as utils) =
    Elm.fn ( "d", Just (annotation "v") )
        (decomposeDict utils Gen.Dict.size)
        |> Elm.declaration "size"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


build : Utils -> String -> Elm.Expression -> Elm.Expression
build { dictTypeName, annotation } var dict =
    Elm.apply (Elm.val dictTypeName) [ dict ]
        |> Elm.withType (annotation var)


decomposeDict : Utils -> (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
decomposeDict { comparableType, dictTypeName } f d =
    Elm.Case.custom d
        (Type.named [] dictTypeName)
        [ Elm.Case.branch1 dictTypeName ( "dict", Gen.Dict.annotation_.dict comparableType (Type.var "v") ) f
        ]



--- Hic Sunt Monsters ---


getVariables : Type.Annotation -> List String
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
        |> List.filter
            isLower


isLower : String -> Bool
isLower s =
    let
        first =
            String.left 1 s
    in
    String.toLower first == first
