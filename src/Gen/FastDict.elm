module Gen.FastDict exposing (annotation_, call_, diff, empty, equals, filter, foldl, foldr, fromCoreDict, fromList, get, getMax, getMaxKey, getMin, getMinKey, insert, intersect, isEmpty, keys, map, member, merge, moduleName_, partition, popMax, popMin, remove, singleton, size, toCoreDict, toList, union, update, values, values_)

{-| 
@docs values_, call_, annotation_, fromCoreDict, toCoreDict, merge, diff, intersect, union, partition, filter, foldr, foldl, map, fromList, toList, values, keys, popMax, popMin, getMax, getMaxKey, getMin, getMinKey, equals, size, get, member, isEmpty, remove, update, insert, singleton, empty, moduleName_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "FastDict" ]


{-| Create an empty dictionary.

empty: FastDict.Dict k v
-}
empty : Elm.Expression
empty =
    Elm.value
        { importFrom = [ "FastDict" ]
        , name = "empty"
        , annotation =
            Just
                (Type.namedWith
                    [ "FastDict" ]
                    "Dict"
                    [ Type.var "k", Type.var "v" ]
                )
        }


{-| Create a dictionary with one key-value pair.

singleton: comparable -> v -> FastDict.Dict comparable v
-}
singleton : Elm.Expression -> Elm.Expression -> Elm.Expression
singleton singletonArg singletonArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "singleton"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "v" ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ singletonArg, singletonArg0 ]


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.

insert: comparable -> v -> FastDict.Dict comparable v -> FastDict.Dict comparable v
-}
insert : Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
insert insertArg insertArg0 insertArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "insert"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.var "v"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ insertArg, insertArg0, insertArg1 ]


{-| Update the value of a dictionary for a specific key with a given function.

update: 
    comparable
    -> (Maybe v -> Maybe v)
    -> FastDict.Dict comparable v
    -> FastDict.Dict comparable v
-}
update :
    Elm.Expression
    -> (Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
update updateArg updateArg0 updateArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.function
                            [ Type.maybe (Type.var "v") ]
                            (Type.maybe (Type.var "v"))
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ updateArg, Elm.functionReduced "updateUnpack" updateArg0, updateArg1 ]


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.

remove: comparable -> FastDict.Dict comparable v -> FastDict.Dict comparable v
-}
remove : Elm.Expression -> Elm.Expression -> Elm.Expression
remove removeArg removeArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "remove"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ removeArg, removeArg0 ]


{-| Determine if a dictionary is empty.

    isEmpty empty
    --> True

isEmpty: FastDict.Dict k v -> Bool
-}
isEmpty : Elm.Expression -> Elm.Expression
isEmpty isEmptyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "isEmpty"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        Type.bool
                    )
            }
        )
        [ isEmptyArg ]


{-| Determine if a key is in a dictionary.

member: comparable -> FastDict.Dict comparable v -> Bool
-}
member : Elm.Expression -> Elm.Expression -> Elm.Expression
member memberArg memberArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "member"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        Type.bool
                    )
            }
        )
        [ memberArg, memberArg0 ]


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals : Dict String String
    animals =
        fromList [ ("Tom", "Cat"), ("Jerry", "Mouse") ]

    get "Tom"   animals
    --> Just "Cat"

    get "Jerry" animals
    --> Just "Mouse"

    get "Spike" animals
    --> Nothing

get: comparable -> FastDict.Dict comparable v -> Maybe v
-}
get : Elm.Expression -> Elm.Expression -> Elm.Expression
get getArg getArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "get"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.maybe (Type.var "v"))
                    )
            }
        )
        [ getArg, getArg0 ]


{-| Determine the number of key-value pairs in the dictionary.

size: FastDict.Dict k v -> Int
-}
size : Elm.Expression -> Elm.Expression
size sizeArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "size"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        Type.int
                    )
            }
        )
        [ sizeArg ]


{-| Determine if two dictionaries are equal. This is needed because the structure could be different depending on insertion order.

equals: FastDict.Dict comparable v -> FastDict.Dict comparable v -> Bool
-}
equals : Elm.Expression -> Elm.Expression -> Elm.Expression
equals equalsArg equalsArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "equals"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        Type.bool
                    )
            }
        )
        [ equalsArg, equalsArg0 ]


{-| Gets the smallest key in the dictionary.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMinKey
    --> Just 1


    empty
        |> getMinKey
    --> Nothing

getMinKey: FastDict.Dict k v -> Maybe k
-}
getMinKey : Elm.Expression -> Elm.Expression
getMinKey getMinKeyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMinKey"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.var "k"))
                    )
            }
        )
        [ getMinKeyArg ]


{-| Gets the key-value pair with the smallest key.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMin
    --> Just ( 1, 'z' )


    empty
        |> getMin
    --> Nothing

getMin: FastDict.Dict k v -> Maybe ( k, v )
-}
getMin : Elm.Expression -> Elm.Expression
getMin getMinArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMin"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.tuple (Type.var "k") (Type.var "v")))
                    )
            }
        )
        [ getMinArg ]


{-| Gets the biggest key in the dictionary.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMaxKey
    --> Just 2


    empty
        |> getMaxKey
    --> Nothing

getMaxKey: FastDict.Dict k v -> Maybe k
-}
getMaxKey : Elm.Expression -> Elm.Expression
getMaxKey getMaxKeyArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMaxKey"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.var "k"))
                    )
            }
        )
        [ getMaxKeyArg ]


{-| Gets the key-value pair with the biggest key.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> getMax
    --> Just ( 2, 'a' )


    empty
        |> getMax
    --> Nothing

getMax: FastDict.Dict k v -> Maybe ( k, v )
-}
getMax : Elm.Expression -> Elm.Expression
getMax getMaxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMax"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.tuple (Type.var "k") (Type.var "v")))
                    )
            }
        )
        [ getMaxArg ]


{-| Removes the key-value pair with the smallest key from the dictionary, and returns it.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> popMin
    --> Just ( ( 1, 'z' ), fromList [ ( 2, 'a' ) ] )


    empty
        |> popMin
    --> Nothing

popMin: 
    FastDict.Dict comparable v
    -> Maybe ( ( comparable, v ), FastDict.Dict comparable v )
-}
popMin : Elm.Expression -> Elm.Expression
popMin popMinArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "popMin"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.maybe
                            (Type.tuple
                                (Type.tuple
                                    (Type.var "comparable")
                                    (Type.var "v")
                                )
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                        )
                    )
            }
        )
        [ popMinArg ]


{-| Removes the key-value pair with the biggest key from the dictionary, and returns it.

    [ ( 1, 'z' ), ( 2, 'a' ) ]
        |> fromList
        |> popMax
    --> Just ( ( 2, 'a' ), fromList [ ( 1, 'z' ) ] )


    empty
        |> popMax
    --> Nothing

popMax: 
    FastDict.Dict comparable v
    -> Maybe ( ( comparable, v ), FastDict.Dict comparable v )
-}
popMax : Elm.Expression -> Elm.Expression
popMax popMaxArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "popMax"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.maybe
                            (Type.tuple
                                (Type.tuple
                                    (Type.var "comparable")
                                    (Type.var "v")
                                )
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                        )
                    )
            }
        )
        [ popMaxArg ]


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ 0, 1 ]

keys: FastDict.Dict k v -> List k
-}
keys : Elm.Expression -> Elm.Expression
keys keysArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "keys"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.list (Type.var "k"))
                    )
            }
        )
        [ keysArg ]


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ])
    --> [ "Alice", "Bob" ]

values: FastDict.Dict k v -> List v
-}
values : Elm.Expression -> Elm.Expression
values valuesArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "values"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.list (Type.var "v"))
                    )
            }
        )
        [ valuesArg ]


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.

toList: FastDict.Dict k v -> List ( k, v )
-}
toList : Elm.Expression -> Elm.Expression
toList toListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "toList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.list (Type.tuple (Type.var "k") (Type.var "v")))
                    )
            }
        )
        [ toListArg ]


{-| Convert an association list into a dictionary.

fromList: List ( comparable, v ) -> FastDict.Dict comparable v
-}
fromList : List Elm.Expression -> Elm.Expression
fromList fromListArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "fromList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple (Type.var "comparable") (Type.var "v"))
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ Elm.list fromListArg ]


{-| Apply a function to all values in a dictionary.

map: (k -> a -> b) -> FastDict.Dict k a -> FastDict.Dict k b
-}
map :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
map mapArg mapArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "k", Type.var "a" ]
                            (Type.var "b")
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "b" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "mapUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (mapArg functionReducedUnpack)
            )
        , mapArg0
        ]


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    getAges : Dict String Int -> List Int
    getAges usersDict =
        FastDict.foldl addAge [] usersDict

    addAge : String -> Int -> List Int -> List Int
    addAge _ age ages =
        age :: ages

    users : Dict String Int
    users =
        FastDict.fromList
            [ ( "Abe", 28 )
            , ( "Beatrix", 19 )
            , ( "Charlotte", 33 )
            ]

    -- Note that the _fold_ is from lowest to highest,
    -- but because we're adding items to the beginning of the list
    -- the result will be from highest to lowest.

    getAges users
    --> [ 33, 19, 28 ]

foldl: (k -> v -> b -> b) -> b -> FastDict.Dict k v -> b
-}
foldl :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
foldl foldlArg foldlArg0 foldlArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "foldl"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "k", Type.var "v", Type.var "b" ]
                            (Type.var "b")
                        , Type.var "b"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.var "b")
                    )
            }
        )
        [ Elm.functionReduced
            "foldlUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (foldlArg functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , foldlArg0
        , foldlArg1
        ]


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    getAges : Dict String Int -> List Int
    getAges usersDict =
        FastDict.foldr addAge [] usersDict

    addAge : String -> Int -> List Int -> List Int
    addAge _ age ages =
        age :: ages

    users : Dict String Int
    users =
        FastDict.fromList
            [ ( "Abe", 28 )
            , ( "Beatrix", 19 )
            , ( "Charlotte", 33 )
            ]

    -- Note that the _fold_ is from highest to lowest,
    -- but because we're adding items to the beginning of the list
    -- the result will be from lowest to highest.

    getAges users
    --> [ 28, 19, 33 ]

foldr: (k -> v -> b -> b) -> b -> FastDict.Dict k v -> b
-}
foldr :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
foldr foldrArg foldrArg0 foldrArg1 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "foldr"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "k", Type.var "v", Type.var "b" ]
                            (Type.var "b")
                        , Type.var "b"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.var "b")
                    )
            }
        )
        [ Elm.functionReduced
            "foldrUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (foldrArg functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , foldrArg0
        , foldrArg1
        ]


{-| Keep only the key-value pairs that pass the given test.

filter: 
    (comparable -> v -> Bool)
    -> FastDict.Dict comparable v
    -> FastDict.Dict comparable v
-}
filter :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
filter filterArg filterArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "comparable", Type.var "v" ]
                            Type.bool
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "filterUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced "unpack" (filterArg functionReducedUnpack)
            )
        , filterArg0
        ]


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.

partition: 
    (comparable -> v -> Bool)
    -> FastDict.Dict comparable v
    -> ( FastDict.Dict comparable v, FastDict.Dict comparable v )
-}
partition :
    (Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
partition partitionArg partitionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "partition"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "comparable", Type.var "v" ]
                            Type.bool
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "FastDict" ]
                                "Dict"
                                [ Type.var "comparable", Type.var "v" ]
                            )
                            (Type.namedWith
                                [ "FastDict" ]
                                "Dict"
                                [ Type.var "comparable", Type.var "v" ]
                            )
                        )
                    )
            }
        )
        [ Elm.functionReduced
            "partitionUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (partitionArg functionReducedUnpack)
            )
        , partitionArg0
        ]


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.

union: 
    FastDict.Dict comparable v
    -> FastDict.Dict comparable v
    -> FastDict.Dict comparable v
-}
union : Elm.Expression -> Elm.Expression -> Elm.Expression
union unionArg unionArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "union"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ unionArg, unionArg0 ]


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.

intersect: 
    FastDict.Dict comparable v
    -> FastDict.Dict comparable v
    -> FastDict.Dict comparable v
-}
intersect : Elm.Expression -> Elm.Expression -> Elm.Expression
intersect intersectArg intersectArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "intersect"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ intersectArg, intersectArg0 ]


{-| Keep a key-value pair when its key does not appear in the second dictionary.

diff: 
    FastDict.Dict comparable a
    -> FastDict.Dict comparable b
    -> FastDict.Dict comparable a
-}
diff : Elm.Expression -> Elm.Expression -> Elm.Expression
diff diffArg diffArg0 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "diff"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "b" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        )
                    )
            }
        )
        [ diffArg, diffArg0 ]


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

merge: 
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> FastDict.Dict comparable a
    -> FastDict.Dict comparable b
    -> result
    -> result
-}
merge :
    (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> (Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression)
    -> (Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression)
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
    -> Elm.Expression
merge mergeArg mergeArg0 mergeArg1 mergeArg2 mergeArg3 mergeArg4 =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "merge"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "comparable"
                            , Type.var "a"
                            , Type.var "result"
                            ]
                            (Type.var "result")
                        , Type.function
                            [ Type.var "comparable"
                            , Type.var "a"
                            , Type.var "b"
                            , Type.var "result"
                            ]
                            (Type.var "result")
                        , Type.function
                            [ Type.var "comparable"
                            , Type.var "b"
                            , Type.var "result"
                            ]
                            (Type.var "result")
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "b" ]
                        , Type.var "result"
                        ]
                        (Type.var "result")
                    )
            }
        )
        [ Elm.functionReduced
            "mergeUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (mergeArg functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , Elm.functionReduced
            "mergeUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (\functionReducedUnpack_2_1_2_0_2_1_2_0_0 ->
                                Elm.functionReduced
                                    "unpack"
                                    (mergeArg0 functionReducedUnpack
                                         functionReducedUnpack0
                                        functionReducedUnpack_2_1_2_0_2_1_2_0_0
                                    )
                            )
                    )
            )
        , Elm.functionReduced
            "mergeUnpack"
            (\functionReducedUnpack ->
                Elm.functionReduced
                    "unpack"
                    (\functionReducedUnpack0 ->
                        Elm.functionReduced
                            "unpack"
                            (mergeArg1 functionReducedUnpack
                                functionReducedUnpack0
                            )
                    )
            )
        , mergeArg2
        , mergeArg3
        , mergeArg4
        ]


{-| Convert the dictionary into an equivalent one from elm/core.

toCoreDict: FastDict.Dict comparable v -> Dict.Dict comparable v
-}
toCoreDict : Elm.Expression -> Elm.Expression
toCoreDict toCoreDictArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "toCoreDict"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ toCoreDictArg ]


{-| Convert the dictionary from an equivalent one from elm/core.

fromCoreDict: Dict.Dict comparable v -> FastDict.Dict comparable v
-}
fromCoreDict : Elm.Expression -> Elm.Expression
fromCoreDict fromCoreDictArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "FastDict" ]
            , name = "fromCoreDict"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
        )
        [ fromCoreDictArg ]


annotation_ : { dict : Type.Annotation -> Type.Annotation -> Type.Annotation }
annotation_ =
    { dict =
        \dictArg0 dictArg1 ->
            Type.alias
                moduleName_
                "Dict"
                [ dictArg0, dictArg1 ]
                (Type.namedWith
                    [ "Internal" ]
                    "Dict"
                    [ Type.var "k", Type.var "v" ]
                )
    }


call_ :
    { singleton : Elm.Expression -> Elm.Expression -> Elm.Expression
    , insert :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , update :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , remove : Elm.Expression -> Elm.Expression -> Elm.Expression
    , isEmpty : Elm.Expression -> Elm.Expression
    , member : Elm.Expression -> Elm.Expression -> Elm.Expression
    , get : Elm.Expression -> Elm.Expression -> Elm.Expression
    , size : Elm.Expression -> Elm.Expression
    , equals : Elm.Expression -> Elm.Expression -> Elm.Expression
    , getMinKey : Elm.Expression -> Elm.Expression
    , getMin : Elm.Expression -> Elm.Expression
    , getMaxKey : Elm.Expression -> Elm.Expression
    , getMax : Elm.Expression -> Elm.Expression
    , popMin : Elm.Expression -> Elm.Expression
    , popMax : Elm.Expression -> Elm.Expression
    , keys : Elm.Expression -> Elm.Expression
    , values : Elm.Expression -> Elm.Expression
    , toList : Elm.Expression -> Elm.Expression
    , fromList : Elm.Expression -> Elm.Expression
    , map : Elm.Expression -> Elm.Expression -> Elm.Expression
    , foldl :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , foldr :
        Elm.Expression -> Elm.Expression -> Elm.Expression -> Elm.Expression
    , filter : Elm.Expression -> Elm.Expression -> Elm.Expression
    , partition : Elm.Expression -> Elm.Expression -> Elm.Expression
    , union : Elm.Expression -> Elm.Expression -> Elm.Expression
    , intersect : Elm.Expression -> Elm.Expression -> Elm.Expression
    , diff : Elm.Expression -> Elm.Expression -> Elm.Expression
    , merge :
        Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
        -> Elm.Expression
    , toCoreDict : Elm.Expression -> Elm.Expression
    , fromCoreDict : Elm.Expression -> Elm.Expression
    }
call_ =
    { singleton =
        \singletonArg singletonArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "singleton"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable", Type.var "v" ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ singletonArg, singletonArg0 ]
    , insert =
        \insertArg insertArg0 insertArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "insert"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable"
                                , Type.var "v"
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ insertArg, insertArg0, insertArg1 ]
    , update =
        \updateArg updateArg0 updateArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "update"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable"
                                , Type.function
                                    [ Type.maybe (Type.var "v") ]
                                    (Type.maybe (Type.var "v"))
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ updateArg, updateArg0, updateArg1 ]
    , remove =
        \removeArg removeArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "remove"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable"
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ removeArg, removeArg0 ]
    , isEmpty =
        \isEmptyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "isEmpty"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                Type.bool
                            )
                    }
                )
                [ isEmptyArg ]
    , member =
        \memberArg memberArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "member"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable"
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                Type.bool
                            )
                    }
                )
                [ memberArg, memberArg0 ]
    , get =
        \getArg getArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "get"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.var "comparable"
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.maybe (Type.var "v"))
                            )
                    }
                )
                [ getArg, getArg0 ]
    , size =
        \sizeArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "size"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                Type.int
                            )
                    }
                )
                [ sizeArg ]
    , equals =
        \equalsArg equalsArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "equals"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                Type.bool
                            )
                    }
                )
                [ equalsArg, equalsArg0 ]
    , getMinKey =
        \getMinKeyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "getMinKey"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.maybe (Type.var "k"))
                            )
                    }
                )
                [ getMinKeyArg ]
    , getMin =
        \getMinArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "getMin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.maybe
                                    (Type.tuple (Type.var "k") (Type.var "v"))
                                )
                            )
                    }
                )
                [ getMinArg ]
    , getMaxKey =
        \getMaxKeyArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "getMaxKey"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.maybe (Type.var "k"))
                            )
                    }
                )
                [ getMaxKeyArg ]
    , getMax =
        \getMaxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "getMax"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.maybe
                                    (Type.tuple (Type.var "k") (Type.var "v"))
                                )
                            )
                    }
                )
                [ getMaxArg ]
    , popMin =
        \popMinArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "popMin"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.maybe
                                    (Type.tuple
                                        (Type.tuple
                                            (Type.var "comparable")
                                            (Type.var "v")
                                        )
                                        (Type.namedWith
                                            [ "FastDict" ]
                                            "Dict"
                                            [ Type.var "comparable"
                                            , Type.var "v"
                                            ]
                                        )
                                    )
                                )
                            )
                    }
                )
                [ popMinArg ]
    , popMax =
        \popMaxArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "popMax"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.maybe
                                    (Type.tuple
                                        (Type.tuple
                                            (Type.var "comparable")
                                            (Type.var "v")
                                        )
                                        (Type.namedWith
                                            [ "FastDict" ]
                                            "Dict"
                                            [ Type.var "comparable"
                                            , Type.var "v"
                                            ]
                                        )
                                    )
                                )
                            )
                    }
                )
                [ popMaxArg ]
    , keys =
        \keysArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "keys"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.list (Type.var "k"))
                            )
                    }
                )
                [ keysArg ]
    , values =
        \valuesArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "values"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.list (Type.var "v"))
                            )
                    }
                )
                [ valuesArg ]
    , toList =
        \toListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "toList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.list
                                    (Type.tuple (Type.var "k") (Type.var "v"))
                                )
                            )
                    }
                )
                [ toListArg ]
    , fromList =
        \fromListArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "fromList"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.list
                                    (Type.tuple
                                        (Type.var "comparable")
                                        (Type.var "v")
                                    )
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ fromListArg ]
    , map =
        \mapArg mapArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "map"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "k", Type.var "a" ]
                                    (Type.var "b")
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "a" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "b" ]
                                )
                            )
                    }
                )
                [ mapArg, mapArg0 ]
    , foldl =
        \foldlArg foldlArg0 foldlArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "foldl"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "k", Type.var "v", Type.var "b" ]
                                    (Type.var "b")
                                , Type.var "b"
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.var "b")
                            )
                    }
                )
                [ foldlArg, foldlArg0, foldlArg1 ]
    , foldr =
        \foldrArg foldrArg0 foldrArg1 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "foldr"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "k", Type.var "v", Type.var "b" ]
                                    (Type.var "b")
                                , Type.var "b"
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "k", Type.var "v" ]
                                ]
                                (Type.var "b")
                            )
                    }
                )
                [ foldrArg, foldrArg0, foldrArg1 ]
    , filter =
        \filterArg filterArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "filter"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "comparable", Type.var "v" ]
                                    Type.bool
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ filterArg, filterArg0 ]
    , partition =
        \partitionArg partitionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "partition"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "comparable", Type.var "v" ]
                                    Type.bool
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.tuple
                                    (Type.namedWith
                                        [ "FastDict" ]
                                        "Dict"
                                        [ Type.var "comparable", Type.var "v" ]
                                    )
                                    (Type.namedWith
                                        [ "FastDict" ]
                                        "Dict"
                                        [ Type.var "comparable", Type.var "v" ]
                                    )
                                )
                            )
                    }
                )
                [ partitionArg, partitionArg0 ]
    , union =
        \unionArg unionArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "union"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ unionArg, unionArg0 ]
    , intersect =
        \intersectArg intersectArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "intersect"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ intersectArg, intersectArg0 ]
    , diff =
        \diffArg diffArg0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "diff"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "a" ]
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "b" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "a" ]
                                )
                            )
                    }
                )
                [ diffArg, diffArg0 ]
    , merge =
        \mergeArg mergeArg0 mergeArg1 mergeArg2 mergeArg3 mergeArg4 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "merge"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.function
                                    [ Type.var "comparable"
                                    , Type.var "a"
                                    , Type.var "result"
                                    ]
                                    (Type.var "result")
                                , Type.function
                                    [ Type.var "comparable"
                                    , Type.var "a"
                                    , Type.var "b"
                                    , Type.var "result"
                                    ]
                                    (Type.var "result")
                                , Type.function
                                    [ Type.var "comparable"
                                    , Type.var "b"
                                    , Type.var "result"
                                    ]
                                    (Type.var "result")
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "a" ]
                                , Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "b" ]
                                , Type.var "result"
                                ]
                                (Type.var "result")
                            )
                    }
                )
                [ mergeArg
                , mergeArg0
                , mergeArg1
                , mergeArg2
                , mergeArg3
                , mergeArg4
                ]
    , toCoreDict =
        \toCoreDictArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "toCoreDict"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "Dict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ toCoreDictArg ]
    , fromCoreDict =
        \fromCoreDictArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "FastDict" ]
                    , name = "fromCoreDict"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Dict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                ]
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                    }
                )
                [ fromCoreDictArg ]
    }


values_ :
    { empty : Elm.Expression
    , singleton : Elm.Expression
    , insert : Elm.Expression
    , update : Elm.Expression
    , remove : Elm.Expression
    , isEmpty : Elm.Expression
    , member : Elm.Expression
    , get : Elm.Expression
    , size : Elm.Expression
    , equals : Elm.Expression
    , getMinKey : Elm.Expression
    , getMin : Elm.Expression
    , getMaxKey : Elm.Expression
    , getMax : Elm.Expression
    , popMin : Elm.Expression
    , popMax : Elm.Expression
    , keys : Elm.Expression
    , values : Elm.Expression
    , toList : Elm.Expression
    , fromList : Elm.Expression
    , map : Elm.Expression
    , foldl : Elm.Expression
    , foldr : Elm.Expression
    , filter : Elm.Expression
    , partition : Elm.Expression
    , union : Elm.Expression
    , intersect : Elm.Expression
    , diff : Elm.Expression
    , merge : Elm.Expression
    , toCoreDict : Elm.Expression
    , fromCoreDict : Elm.Expression
    }
values_ =
    { empty =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "empty"
            , annotation =
                Just
                    (Type.namedWith
                        [ "FastDict" ]
                        "Dict"
                        [ Type.var "k", Type.var "v" ]
                    )
            }
    , singleton =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "singleton"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable", Type.var "v" ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , insert =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "insert"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.var "v"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , update =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "update"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.function
                            [ Type.maybe (Type.var "v") ]
                            (Type.maybe (Type.var "v"))
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , remove =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "remove"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , isEmpty =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "isEmpty"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        Type.bool
                    )
            }
    , member =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "member"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        Type.bool
                    )
            }
    , get =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "get"
            , annotation =
                Just
                    (Type.function
                        [ Type.var "comparable"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.maybe (Type.var "v"))
                    )
            }
    , size =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "size"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        Type.int
                    )
            }
    , equals =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "equals"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        Type.bool
                    )
            }
    , getMinKey =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMinKey"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.var "k"))
                    )
            }
    , getMin =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMin"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.tuple (Type.var "k") (Type.var "v")))
                    )
            }
    , getMaxKey =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMaxKey"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.var "k"))
                    )
            }
    , getMax =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "getMax"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.maybe (Type.tuple (Type.var "k") (Type.var "v")))
                    )
            }
    , popMin =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "popMin"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.maybe
                            (Type.tuple
                                (Type.tuple
                                    (Type.var "comparable")
                                    (Type.var "v")
                                )
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                        )
                    )
            }
    , popMax =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "popMax"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.maybe
                            (Type.tuple
                                (Type.tuple
                                    (Type.var "comparable")
                                    (Type.var "v")
                                )
                                (Type.namedWith
                                    [ "FastDict" ]
                                    "Dict"
                                    [ Type.var "comparable", Type.var "v" ]
                                )
                            )
                        )
                    )
            }
    , keys =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "keys"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.list (Type.var "k"))
                    )
            }
    , values =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "values"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.list (Type.var "v"))
                    )
            }
    , toList =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "toList"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.list (Type.tuple (Type.var "k") (Type.var "v")))
                    )
            }
    , fromList =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "fromList"
            , annotation =
                Just
                    (Type.function
                        [ Type.list
                            (Type.tuple (Type.var "comparable") (Type.var "v"))
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , map =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "map"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "k", Type.var "a" ]
                            (Type.var "b")
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "a" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "b" ]
                        )
                    )
            }
    , foldl =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "foldl"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "k", Type.var "v", Type.var "b" ]
                            (Type.var "b")
                        , Type.var "b"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.var "b")
                    )
            }
    , foldr =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "foldr"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "k", Type.var "v", Type.var "b" ]
                            (Type.var "b")
                        , Type.var "b"
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "k", Type.var "v" ]
                        ]
                        (Type.var "b")
                    )
            }
    , filter =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "filter"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "comparable", Type.var "v" ]
                            Type.bool
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , partition =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "partition"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "comparable", Type.var "v" ]
                            Type.bool
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.tuple
                            (Type.namedWith
                                [ "FastDict" ]
                                "Dict"
                                [ Type.var "comparable", Type.var "v" ]
                            )
                            (Type.namedWith
                                [ "FastDict" ]
                                "Dict"
                                [ Type.var "comparable", Type.var "v" ]
                            )
                        )
                    )
            }
    , union =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "union"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , intersect =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "intersect"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , diff =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "diff"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "b" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        )
                    )
            }
    , merge =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "merge"
            , annotation =
                Just
                    (Type.function
                        [ Type.function
                            [ Type.var "comparable"
                            , Type.var "a"
                            , Type.var "result"
                            ]
                            (Type.var "result")
                        , Type.function
                            [ Type.var "comparable"
                            , Type.var "a"
                            , Type.var "b"
                            , Type.var "result"
                            ]
                            (Type.var "result")
                        , Type.function
                            [ Type.var "comparable"
                            , Type.var "b"
                            , Type.var "result"
                            ]
                            (Type.var "result")
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "a" ]
                        , Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "b" ]
                        , Type.var "result"
                        ]
                        (Type.var "result")
                    )
            }
    , toCoreDict =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "toCoreDict"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    , fromCoreDict =
        Elm.value
            { importFrom = [ "FastDict" ]
            , name = "fromCoreDict"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith
                            [ "Dict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        ]
                        (Type.namedWith
                            [ "FastDict" ]
                            "Dict"
                            [ Type.var "comparable", Type.var "v" ]
                        )
                    )
            }
    }


