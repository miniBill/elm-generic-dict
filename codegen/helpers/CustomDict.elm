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
import Elm.Let
import Elm.Op
import Elm.ToString
import Gen.Dict
import Gen.Maybe
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
            [ typeDeclaration utils
            , emptyDeclaration utils
            , singletonDeclaration utils
            , insertDeclaration utils
            , updateDeclaration utils
            , sizeDeclaration utils
            ]
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
            [ Type.int
            , Gen.Dict.annotation_.dict
                comparableType
                (Type.var "v")
            ]
        ]
        |> Elm.expose


build : Utils -> Elm.Expression -> Elm.Expression -> Elm.Expression
build { dictTypeName, annotation } size dict =
    Elm.apply (Elm.val dictTypeName) [ size, dict ]
        |> Elm.withType
            (dict
                |> Elm.ToString.expression
                |> .signature
                -- TODO: fix this
                |> String.split " "
                |> List.reverse
                |> List.head
                |> Maybe.withDefault "??"
                |> annotation
            )


emptyDeclaration : Utils -> Elm.Declaration
emptyDeclaration ({ annotation } as utils) =
    build utils (Elm.int 0) Gen.Dict.empty
        |> Elm.withType (annotation "v")
        |> Elm.declaration "empty"
        |> Elm.expose


singletonDeclaration : Utils -> Elm.Declaration
singletonDeclaration ({ annotation, keyType, toComparable } as utils) =
    Elm.fn2
        ( "key", Just keyType )
        ( "value", Just <| Type.var "v" )
        (\key value ->
            build utils (Elm.int 1) (Gen.Dict.singleton (toComparable key) value)
                |> Elm.withType (annotation "v")
        )
        |> Elm.declaration "singleton"
        |> Elm.expose


insertDeclaration : Utils -> Elm.Declaration
insertDeclaration ({ annotation, keyType, toComparable } as utils) =
    Elm.fn3
        ( "key", Just keyType )
        ( "value", Just <| Type.var "v" )
        ( "d", Just (annotation "v") )
        (\key value ->
            decomposeDict utils
                (\sz dict ->
                    build utils
                        (Elm.Op.plus sz (Elm.int 1))
                        (Gen.Dict.insert (toComparable key) value dict)
                 -- |> Elm.withType (annotation "v")
                )
        )
        -- |> Elm.withType
        --     (Type.function
        --         [ keyType, Type.var "v", annotation "v" ]
        --         (annotation "v")
        --     )
        |> Elm.declaration "insert"
        |> Elm.expose


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
        (\key updater d ->
            decomposeDict utils
                (\sz dict ->
                    Elm.Let.letIn
                        (\comparableKey ->
                            Elm.Case.maybe
                                (Gen.Dict.get comparableKey dict)
                                { just =
                                    ( "value"
                                    , \value ->
                                        Elm.Case.maybe (Elm.apply updater [ value ])
                                            { just =
                                                ( "newvalue"
                                                , \newvalue ->
                                                    build utils
                                                        sz
                                                        (Gen.Dict.insert comparableKey newvalue dict)
                                                )
                                            , nothing =
                                                build utils
                                                    (Elm.Op.minus sz (Elm.int 1))
                                                    (Gen.Dict.remove comparableKey dict)
                                            }
                                    )
                                , nothing =
                                    Elm.Case.maybe (Elm.apply updater [ Gen.Maybe.make_.nothing ])
                                        { just =
                                            ( "newvalue"
                                            , \newvalue ->
                                                build utils
                                                    (Elm.Op.plus sz (Elm.int 1))
                                                    (Gen.Dict.insert comparableKey newvalue dict)
                                            )
                                        , nothing = d
                                        }
                                }
                        )
                        |> Elm.Let.value "comparableKey" (toComparable key)
                        |> Elm.Let.toExpression
                        |> Elm.withType (annotation "v")
                )
                d
        )
        |> Elm.declaration "update"
        |> Elm.expose


decomposeDict : Utils -> (Elm.Expression -> Elm.Expression -> Elm.Expression) -> Elm.Expression -> Elm.Expression
decomposeDict { keyType, dictTypeName } f d =
    Elm.Case.custom d
        (Type.named [] dictTypeName)
        [ Elm.Case.branch2 dictTypeName ( "sz", Type.int ) ( "dict", Gen.Dict.annotation_.dict keyType (Type.var "v") ) f
        ]


sizeDeclaration : Utils -> Elm.Declaration
sizeDeclaration ({ annotation } as utils) =
    Elm.fn ( "d", Just (annotation "v") )
        (decomposeDict utils (\sz _ -> sz))
        |> Elm.declaration "size"
        |> Elm.expose



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
