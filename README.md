# `elm-generic-dict` [![Build Status](https://github.com/miniBill/elm-generic-dict/workflows/CI/badge.svg)](https://github.com/miniBill/elm-generic-dict/actions?query=branch%3Amain)

`elm-generic-dict` can be used to codegen dictionaries with arbitrary types as keys. It is meant to be used as a library with [`elm-codegen`](https://github.com/mdgriffith/elm-codegen).

The dictionaries don't contain functions, and the API (of the generated code) doesn't require you to pass any function.

The main disadvantage of using this approach is code duplication, while the advantages are:
1. does not contain functions (useful for debugging and usage in lamdera),
2. does not require passing in functions when using it (cleaner API, no possibility of mistakes),
3. live code inclusion will trim unused functions.
