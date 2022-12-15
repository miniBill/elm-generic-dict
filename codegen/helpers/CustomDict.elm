module CustomDict exposing
    ( Config
    , withKey
    , withToComparable
    , generate
    , withSize
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
import Gen.Debug
import Gen.Dict
import Gen.List
import Gen.Maybe
import Gen.Tuple
import String.Extra


type Config
    = Config
        { keyType : Type.Annotation
        , namespace : List String
        , size : Bool
        , toComparable : Elm.Expression -> Elm.Expression
        }


withKey : Type.Annotation -> Config
withKey keyType =
    Config
        { keyType = keyType
        , namespace = []
        , size = False
        , toComparable = identity
        }


withToComparable : (Elm.Expression -> Elm.Expression) -> Config -> Config
withToComparable toComparable (Config config) =
    Config { config | toComparable = toComparable }


{-| NOT IMPLEMENTED YET. This makes the generated code bigger but makes `size` constant time.
-}
withSize : Bool -> Config -> Config
withSize size (Config config) =
    Config { config | size = size }


generate : Config -> Elm.File
generate (Config { size, keyType, toComparable, namespace }) =
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
            if size then
                [ Gen.Debug.todo "Keeping track of size not supported yet"
                    |> Elm.declaration "todo"
                    |> Elm.expose
                ]

            else
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
                , keysDeclaration
                , valuesDeclaration
                , toListDeclaration
                , fromListDeclaration
                , mapDeclaration
                , foldlDeclaration
                , foldrDeclaration
                , filterDeclaration
                , partitionDeclaration
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
typeDeclaration { dictTypeName, keyType, comparableType } =
    Elm.customType dictTypeName
        [ Elm.variantWith dictTypeName
            [ Gen.Dict.annotation_.dict
                comparableType
                (Type.tuple keyType (Type.var "v"))
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
            build utils "v" (Gen.Dict.singleton (toComparable key) (Elm.tuple key value))
        )
        |> Elm.declaration "singleton"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


insertDeclaration : Utils -> Elm.Declaration
insertDeclaration ({ annotation, keyType, toComparable } as utils) =
    Elm.fn3
        ( "key", Just keyType )
        ( "value", Just <| Type.var "v" )
        ( "d", Just <| annotation "v" )
        (\key value ->
            decomposeDict utils
                (\dict ->
                    build utils
                        "v"
                        (Gen.Dict.insert (toComparable key) (Elm.tuple key value) dict)
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
        ( "d", Just <| annotation "v" )
        (\key updater ->
            decomposeDict utils
                (\dict ->
                    build utils
                        "v"
                        (Gen.Dict.update
                            (toComparable key)
                            (\e ->
                                e
                                    |> Gen.Maybe.map Gen.Tuple.second
                                    |> (\s ->
                                            Elm.apply updater [ s ]
                                                |> identity
                                       )
                                    |> Gen.Maybe.map (Gen.Tuple.pair key)
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
        ( "d", Just <| annotation "v" )
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
        ( "d", Just <| annotation "v" )
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
        ( "d", Just <| annotation "v" )
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
        ( "d", Just <| annotation "v" )
        (\key ->
            decomposeDict utils
                (\dict ->
                    Gen.Dict.get (toComparable key)
                        dict
                        |> Gen.Maybe.map Gen.Tuple.second
                )
        )
        |> Elm.declaration "get"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


sizeDeclaration : Utils -> Elm.Declaration
sizeDeclaration ({ annotation } as utils) =
    Elm.fn ( "d", Just <| annotation "v" )
        (decomposeDict utils Gen.Dict.size)
        |> Elm.declaration "size"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


keysDeclaration : Utils -> Elm.Declaration
keysDeclaration ({ annotation } as utils) =
    Elm.fn ( "d", Just <| annotation "v" )
        (decomposeDict utils
            (\d ->
                d
                    |> Gen.Dict.values
                    |> Gen.List.call_.map Gen.Tuple.values_.first
            )
        )
        |> Elm.declaration "keys"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Lists" }


valuesDeclaration : Utils -> Elm.Declaration
valuesDeclaration ({ annotation } as utils) =
    Elm.fn ( "d", Just <| annotation "v" )
        (decomposeDict utils
            (\d ->
                d
                    |> Gen.Dict.values
                    |> Gen.List.call_.map Gen.Tuple.values_.second
            )
        )
        |> Elm.declaration "values"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Lists" }


toListDeclaration : Utils -> Elm.Declaration
toListDeclaration ({ annotation } as utils) =
    Elm.fn ( "d", Just <| annotation "v" )
        (decomposeDict utils Gen.Dict.values)
        |> Elm.declaration "toList"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Lists" }


fromListDeclaration : Utils -> Elm.Declaration
fromListDeclaration ({ keyType } as utils) =
    Elm.fn
        ( "l"
        , Just
            (Type.list
                (Type.tuple keyType (Type.var "v"))
            )
        )
        (\l ->
            l
                |> Gen.List.call_.map
                    (Elm.functionReduced "e" <|
                        \e -> Elm.Case.tuple e "k" "v" (\k _ -> Elm.tuple k e)
                    )
                |> Gen.Dict.call_.fromList
                |> build
                    utils
                    "v"
        )
        |> Elm.declaration "fromList"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Lists" }


mapDeclaration : Utils -> Elm.Declaration
mapDeclaration ({ keyType, annotation } as utils) =
    Elm.fn2
        ( "f"
        , Just <|
            Type.function
                [ keyType, Type.var "a" ]
                (Type.var "b")
        )
        ( "d", Just <| annotation "a" )
        (\f d ->
            build utils "b" <|
                decomposeDict utils
                    (Gen.Dict.map
                        (\_ kv ->
                            Elm.Case.tuple kv "k" "a" (\k v -> Elm.apply f [ k, v ])
                        )
                    )
                    d
        )
        |> Elm.declaration "map"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


foldlDeclaration : Utils -> Elm.Declaration
foldlDeclaration =
    foldDeclaration "foldl" Gen.Dict.call_.foldl


foldrDeclaration : Utils -> Elm.Declaration
foldrDeclaration =
    foldDeclaration "foldr" Gen.Dict.call_.foldr


foldDeclaration : String -> (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression) -> Utils -> Elm.Declaration
foldDeclaration name fold ({ keyType, annotation } as utils) =
    Elm.fn3
        ( "f"
        , Just <|
            Type.function
                [ keyType, Type.var "v", Type.var "b" ]
                (Type.var "b")
        )
        ( "b0", Just <| Type.var "b" )
        ( "d", Just <| annotation "v" )
        (\f init ->
            decomposeDict utils
                (fold
                    (Elm.fn3
                        ( "_", Nothing )
                        ( "kv", Nothing )
                        ( "b", Nothing )
                     <|
                        \_ kv acc ->
                            Elm.Case.tuple kv "k" "v" (\k v -> Elm.apply f [ k, v, acc ])
                    )
                    init
                )
        )
        |> Elm.declaration name
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


filterDeclaration : Utils -> Elm.Declaration
filterDeclaration ({ keyType, annotation } as utils) =
    Elm.fn2
        ( "f"
        , Just <|
            Type.function
                [ keyType, Type.var "v" ]
                Type.bool
        )
        ( "d", Just <| annotation "v" )
        (\f d ->
            build utils "v" <|
                decomposeDict utils
                    (Gen.Dict.filter
                        (\_ kv ->
                            Elm.Case.tuple kv "k" "v" (\k v -> Elm.apply f [ k, v ])
                        )
                    )
                    d
        )
        |> Elm.declaration "filter"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


partitionDeclaration : Utils -> Elm.Declaration
partitionDeclaration ({ keyType, annotation } as utils) =
    Elm.fn2
        ( "f"
        , Just <|
            Type.function
                [ keyType, Type.var "v" ]
                Type.bool
        )
        ( "d", Just <| annotation "v" )
        (\f d ->
            decomposeDict utils
                (\dict ->
                    dict
                        |> Gen.Dict.partition
                            (\_ kv ->
                                Elm.Case.tuple kv "k" "v" (\k v -> Elm.apply f [ k, v ])
                            )
                        |> Gen.Tuple.mapBoth
                            (build utils "v")
                            (build utils "v")
                )
                d
        )
        |> Elm.declaration "partition"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


build : Utils -> String -> Elm.Expression -> Elm.Expression
build { dictTypeName, annotation } var dict =
    Elm.apply (Elm.val dictTypeName) [ dict ]
        |> Elm.withType (annotation var)


decomposeDict : Utils -> (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
decomposeDict { keyType, comparableType, dictTypeName } f d =
    Elm.Case.custom d
        (Type.named [] dictTypeName)
        [ Elm.Case.branch1 dictTypeName
            ( "dict"
            , Gen.Dict.annotation_.dict comparableType
                (Type.tuple
                    keyType
                    (Type.var "v")
                )
            )
            f
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
