module GenericSet exposing
    ( Config
    , init, withTypeName, useElmFastDict
    , generateFile, generateDeclarations
    )

{-| This module can be use to codegen a set with a custom type as value.


# Types

@docs Config


# Configuration

@docs init, withTypeName, useElmFastDict


# Generation

@docs generateFile, generateDeclarations

-}

import Elm
import Elm.Annotation as Type
import Elm.Case
import Elm.ToString
import Gen.Dict
import Gen.FastDict
import Gen.List
import Gen.Tuple
import Internal
import String.Extra


{-| Configuration used to build a generic set.
-}
type Config
    = Config
        { valueType : Type.Annotation
        , namespace : List String
        , toComparable : Elm.Expression -> Elm.Expression
        , typeName : Maybe String
        , useFast : Bool
        }


{-| Starts building a custom set, given the type of the value, the namespace of the resulting file, and a `toComparable` function.

The `toComparable` function will be _copied_ in each declaration, so it should be kept very simple (or extracted to a function, and then passed in like `{ toComparable = Gen.YourType.toString }`, or similar).

-}
init :
    { valueType : Type.Annotation
    , namespace : List String
    , toComparable : Elm.Expression -> Elm.Expression
    }
    -> Config
init cfg =
    Config
        { valueType = cfg.valueType
        , namespace = cfg.namespace
        , toComparable = cfg.toComparable
        , typeName = Nothing
        , useFast = False
        }


{-| Use a custom type name for the set type.
-}
withTypeName : String -> Config -> Config
withTypeName name (Config config) =
    Config { config | typeName = Just name }


{-| Use `miniBill/elm-fast-dict` as the backing container.
This means that generated code will depend on that package but gives the advantages of that package (read `elm-fast-dict`'s README for pros and cons).
-}
useElmFastDict : Config -> Config
useElmFastDict (Config config) =
    Config { config | useFast = True }


{-| Generates declarations from the given configuration.

This can be useful if you want to add your own custom declarations to the file.

-}
generateDeclarations : Config -> List Elm.Declaration
generateDeclarations ((Config { valueType, toComparable, useFast }) as config) =
    let
        setTypeName : String
        setTypeName =
            toSetTypeName config

        comparableType : Type.Annotation
        comparableType =
            toComparableType config

        annotation : Type.Annotation
        annotation =
            Type.namedWith [] setTypeName <|
                List.map Type.var <|
                    Internal.getVariables valueType

        utils : Utils
        utils =
            { setTypeName = setTypeName
            , annotation = annotation
            , comparableType = comparableType
            , valueType = valueType
            , toComparable = toComparable
            , useFast = useFast
            }

        baseDecls : List (Utils -> Elm.Declaration)
        baseDecls =
            [ typeDeclaration
            , emptyDeclaration
            , singletonDeclaration
            , insertDeclaration
            , removeDeclaration
            , isEmptyDeclaration
            , memberDeclaration
            , sizeDeclaration
            , toListDeclaration
            , fromListDeclaration
            , foldlDeclaration
            , foldrDeclaration
            , filterDeclaration
            , partitionDeclaration
            ]
    in
    List.map (\f -> f utils) baseDecls


toComparableType : Config -> Type.Annotation
toComparableType (Config { valueType, toComparable }) =
    Elm.val "value"
        |> Elm.withType valueType
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
                                    if Internal.isLower t then
                                        Type.var t

                                    else
                                        Type.named [] t
                                )
                                ts
                            )
           )


{-| Generates a file from the given configuration.
-}
generateFile : Config -> Elm.File
generateFile ((Config cfg) as config) =
    let
        setTypeName : String
        setTypeName =
            toSetTypeName config

        decls : List Elm.Declaration
        decls =
            generateDeclarations config
    in
    Elm.file (cfg.namespace ++ [ setTypeName ]) decls


toSetTypeName : Config -> String
toSetTypeName ((Config cfg) as config) =
    cfg.typeName
        |> Maybe.withDefault (toValueTypeName config ++ "Set")


toValueTypeName : Config -> String
toValueTypeName (Config { valueType }) =
    (Elm.ToString.annotation valueType).signature
        |> String.split " "
        |> List.head
        |> Maybe.withDefault "???"
        |> String.Extra.classify


type alias Utils =
    { setTypeName : String
    , valueType : Type.Annotation
    , annotation : Type.Annotation
    , comparableType : Type.Annotation
    , toComparable : Elm.Expression -> Elm.Expression
    , useFast : Bool
    }


typeDeclaration : Utils -> Elm.Declaration
typeDeclaration ({ setTypeName } as utils) =
    Elm.customType setTypeName
        [ Elm.variantWith setTypeName
            [ containerAnnotation utils
            ]
        ]
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Sets" }


containerAnnotation : Utils -> Type.Annotation
containerAnnotation { useFast, comparableType, valueType } =
    if useFast then
        Gen.FastDict.annotation_.dict comparableType valueType

    else
        Gen.Dict.annotation_.dict comparableType valueType


emptyDeclaration : Utils -> Elm.Declaration
emptyDeclaration utils =
    build utils
        (if utils.useFast then
            Gen.FastDict.empty

         else
            Gen.Dict.empty
        )
        |> Elm.declaration "empty"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


singletonDeclaration : Utils -> Elm.Declaration
singletonDeclaration ({ valueType, toComparable, useFast } as utils) =
    Elm.fn
        ( "value", Just valueType )
        (\value ->
            build utils
                ((if useFast then
                    Gen.FastDict.singleton

                  else
                    Gen.Dict.singleton
                 )
                    (toComparable value)
                    value
                )
        )
        |> Elm.declaration "singleton"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


insertDeclaration : Utils -> Elm.Declaration
insertDeclaration ({ annotation, valueType, toComparable, useFast } as utils) =
    Elm.fn2
        ( "value", Just valueType )
        ( "d", Just annotation )
        (\value ->
            decomposeDict utils
                (\dict ->
                    build utils
                        ((if useFast then
                            Gen.FastDict.insert

                          else
                            Gen.Dict.insert
                         )
                            (toComparable value)
                            value
                            dict
                        )
                )
        )
        |> Elm.declaration "insert"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


removeDeclaration : Utils -> Elm.Declaration
removeDeclaration ({ valueType, annotation, toComparable, useFast } as utils) =
    Elm.fn2
        ( "value", Just valueType )
        ( "d", Just annotation )
        (\value ->
            decomposeDict utils
                (\dict ->
                    build utils
                        ((if useFast then
                            Gen.FastDict.remove

                          else
                            Gen.Dict.remove
                         )
                            (toComparable value)
                            dict
                        )
                )
        )
        |> Elm.declaration "remove"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Build" }


isEmptyDeclaration : Utils -> Elm.Declaration
isEmptyDeclaration ({ annotation, useFast } as utils) =
    Elm.fn
        ( "d", Just annotation )
        (decomposeDict utils
            (if useFast then
                Gen.FastDict.isEmpty

             else
                Gen.Dict.isEmpty
            )
        )
        |> Elm.declaration "isEmpty"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


memberDeclaration : Utils -> Elm.Declaration
memberDeclaration ({ valueType, annotation, toComparable, useFast } as utils) =
    Elm.fn2
        ( "value", Just valueType )
        ( "d", Just annotation )
        (\value ->
            decomposeDict utils
                ((if useFast then
                    Gen.FastDict.member

                  else
                    Gen.Dict.member
                 )
                    (toComparable value)
                )
        )
        |> Elm.declaration "member"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


sizeDeclaration : Utils -> Elm.Declaration
sizeDeclaration ({ annotation, useFast } as utils) =
    Elm.fn ( "d", Just annotation )
        (decomposeDict utils
            (if useFast then
                Gen.FastDict.size

             else
                Gen.Dict.size
            )
        )
        |> Elm.declaration "size"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Query" }


toListDeclaration : Utils -> Elm.Declaration
toListDeclaration ({ annotation, useFast } as utils) =
    Elm.fn ( "d", Just annotation )
        (decomposeDict utils
            (if useFast then
                Gen.FastDict.values

             else
                Gen.Dict.values
            )
        )
        |> Elm.declaration "toList"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Lists" }


fromListDeclaration : Utils -> Elm.Declaration
fromListDeclaration ({ valueType, toComparable, useFast } as utils) =
    Elm.fn
        ( "l"
        , Just
            (Type.list
                valueType
            )
        )
        (\l ->
            l
                |> Gen.List.call_.map
                    (Elm.fn ( "e", Just valueType ) <|
                        \e -> Elm.tuple (toComparable e) e
                    )
                |> (if useFast then
                        Gen.FastDict.call_.fromList

                    else
                        Gen.Dict.call_.fromList
                   )
                |> build utils
        )
        |> Elm.declaration "fromList"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Lists" }


foldlDeclaration : Utils -> Elm.Declaration
foldlDeclaration ({ useFast } as utils) =
    foldDeclaration "foldl"
        (if useFast then
            Gen.FastDict.call_.foldl

         else
            Gen.Dict.call_.foldl
        )
        utils


foldrDeclaration : Utils -> Elm.Declaration
foldrDeclaration ({ useFast } as utils) =
    foldDeclaration "foldr"
        (if useFast then
            Gen.FastDict.call_.foldr

         else
            Gen.Dict.call_.foldr
        )
        utils


foldDeclaration : String -> (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression) -> Utils -> Elm.Declaration
foldDeclaration name fold ({ valueType, annotation } as utils) =
    Elm.fn3
        ( "f"
        , Just <|
            Type.function
                [ valueType, Type.var "b" ]
                (Type.var "b")
        )
        ( "b0", Just <| Type.var "b" )
        ( "d", Just annotation )
        (\f initAcc ->
            decomposeDict utils
                (fold
                    (Elm.fn3
                        ( "_", Nothing )
                        ( "e", Nothing )
                        ( "b", Nothing )
                     <|
                        \_ e acc ->
                            Elm.apply f [ e, acc ]
                    )
                    initAcc
                )
        )
        |> Elm.declaration name
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


filterDeclaration : Utils -> Elm.Declaration
filterDeclaration ({ valueType, annotation, useFast } as utils) =
    Elm.fn2
        ( "f"
        , Just <|
            Type.function
                [ valueType ]
                Type.bool
        )
        ( "d", Just annotation )
        (\f d ->
            build utils <|
                decomposeDict utils
                    ((if useFast then
                        Gen.FastDict.filter

                      else
                        Gen.Dict.filter
                     )
                        (\_ e ->
                            Elm.apply f [ e ]
                        )
                    )
                    d
        )
        |> Elm.declaration "filter"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


partitionDeclaration : Utils -> Elm.Declaration
partitionDeclaration ({ valueType, annotation, useFast } as utils) =
    Elm.fn2
        ( "f"
        , Just <|
            Type.function
                [ valueType ]
                Type.bool
        )
        ( "d", Just annotation )
        (\f d ->
            decomposeDict utils
                (\dict ->
                    dict
                        |> (if useFast then
                                Gen.FastDict.partition

                            else
                                Gen.Dict.partition
                           )
                            (\_ e ->
                                Elm.apply f [ e ]
                            )
                        |> Gen.Tuple.mapBoth
                            (build utils)
                            (build utils)
                )
                d
        )
        |> Elm.declaration "partition"
        |> Elm.exposeWith { exposeConstructor = False, group = Just "Transform" }


build : Utils -> Elm.Expression -> Elm.Expression
build { setTypeName, annotation } dict =
    Elm.apply (Elm.val setTypeName) [ dict ]
        |> Elm.withType annotation


decomposeDict : Utils -> (Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
decomposeDict ({ setTypeName } as utils) f d =
    Elm.Case.custom d
        (Type.named [] setTypeName)
        [ Elm.Case.branch1 setTypeName
            ( "dict"
            , containerAnnotation utils
            )
            f
        ]
