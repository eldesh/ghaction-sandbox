module Data.Result exposing
    ( withDefault, fromMaybe, isOk, isErr
    , map, map2, map3, map4, map5, ok, err, fromOk, fromErr, toList, catOks, catErrs, partition
    , andThen, and, or, apply
    )

{-| An extended toolbox for working with results: everything from the `elm/core`
`Result` module plus Haskell-inspired helpers such as `fromOk`, `isOk` and
`toList`, all implemented the Elm way.


# Definition

[Result](https://package.elm-lang.org/packages/elm/core/1.0.5/Result)


# Common Helpers

@docs withDefault, fromMaybe, isOk, isErr


# Result transformations

@docs map, map2, map3, map4, map5, ok, err, fromOk, fromErr, toList, catOks, catErrs, partition


# Chaining Results

@docs andThen, and, or, apply

-}

import Data.List as L
import Result



-- Common Helpers


{-| Provide a default value, turning an optional value into a normal
value. This comes in handy when paired with functions like
[`Dict.get`](Dict#get) which gives back a `Maybe`.

    withDefault 100 (Just 42) -- 42

    withDefault 100 Nothing -- 100

    withDefault "unknown" (Dict.get "Tom" Dict.empty) -- "unknown"

**Note:** This can be overused! Many cases are better handled by a `case`
expression. And if you end up using `withDefault` a lot, it can be a good sign
that a [custom type][ct] will clean your code up quite a bit!

[ct]: https://guide.elm-lang.org/types/custom_types.html

-}
withDefault : a -> Result x a -> a
withDefault =
    Result.withDefault


{-| Provide a default value, turning an optional value into a normal
value.
Alias of [`withDefault`](#withDefault).
-}
fromMaybe : x -> Maybe a -> Result x a
fromMaybe =
    Result.fromMaybe


{-| Return the contents of Ok value or a default value otherwise.
-}
fromOk : a -> Result e a -> a
fromOk v r =
    case r of
        Ok a ->
            a

        Err _ ->
            v


{-| Return the contents of Err value or a default value otherwise.
-}
fromErr : e -> Result e a -> e
fromErr v r =
    case r of
        Ok _ ->
            v

        Err e ->
            e


{-| Return True iff its argument is of the form `Ok _`.


## Examples

Basic usage:

    isOk (Ok 3) == True

    isOk (Ok ()) == True

    isOk (Err "error") == False

Only the outer constructor is taken into consideration:

    isOk (Ok (Err "err")) == True

-}
isOk : Result a b -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        _ ->
            False


{-| Return True iff its argument is `Err _`.


## Examples

Basic usage:

    isErr (Ok 3) == False

    isErr (Ok ()) == False

    isErr (Err ()) == True

Only the outer constructor is taken into consideration:

    isErr (Ok (Err "err")) == False

-}
isErr : Result a b -> Bool
isErr r =
    case r of
        Err _ ->
            True

        _ ->
            False



-- Result transformations


{-| Apply a function to a result. If the result is `Ok`, it will be converted.
If the result is an `Err`, the same error value will propagate through.

    map sqrt (Ok 4.0)          == Ok 2.0
    map sqrt (Err "bad input") == Err "bad input"

-}
map : (a -> b) -> Result e a -> Result e b
map =
    Result.map


{-| Apply a function if both results are `Ok`. If not, the first `Err` will
propagate through.

    map2 max (Ok 42)   (Ok 13)   == Ok 42
    map2 max (Err "x") (Ok 13)   == Err "x"
    map2 max (Ok 42)   (Err "y") == Err "y"
    map2 max (Err "x") (Err "y") == Err "x"

This can be useful if you have two computations that may fail, and you want
to put them together quickly.

-}
map2 : (a -> b -> value) -> Result e a -> Result e b -> Result e value
map2 =
    Result.map2


{-| -}
map3 : (a -> b -> c -> value) -> Result e a -> Result e b -> Result e c -> Result e value
map3 =
    Result.map3


{-| -}
map4 : (a -> b -> c -> d -> value) -> Result e a -> Result e b -> Result e c -> Result e d -> Result e value
map4 =
    Result.map4


{-| -}
map5 : (a -> b -> c -> d -> e -> value) -> Result e a -> Result e b -> Result e c -> Result e d -> Result e e -> Result e value
map5 =
    Result.map5


{-| Convert to `Maybe::Just` if given argument is `Ok _` and
`Nothing` otherwise.
-}
ok : Result e r -> Maybe r
ok r =
    case r of
        Err _ ->
            Nothing

        Ok v ->
            Just v


{-| Convert to `Maybe::Just` if given argument is `Err _` and
`Nothing` otherwise.
-}
err : Result e r -> Maybe e
err r =
    case r of
        Err e ->
            Just e

        Ok _ ->
            Nothing


{-| Return an empty list when given `Err _` or a singleton list when
given `Ok _`.


## Examples

Basic usage:

    toList (Ok 7) == [ 7 ]

    toList (Err _) == []

One can use maybeToList to avoid pattern matching when combined with a function that (safely) works on lists:

    (sum
        <| toList
        <| fromMaybe "parse error"
        <| String.toInt "3")
    == 3

    (sum
        <| toList
        <| fromMaybe "parse error"
        <| String.toInt "")
    == 0

-}
toList : Result e a -> List a
toList r =
    case r of
        Err _ ->
            []

        Ok x ->
            [ x ]


{-| Take a list of Results and return a list of all the Ok values.


## Examples

Basic usage:

    catOks [ Ok 1, Err (), Ok 3 ] == [ 1, 3 ]

When constructing a list of Result values, catOks can be used to return all
of the "success" results (if the list is the result of a map, then mapMaybe
would be more appropriate):

    type ParseError = ParseError String
    toInt s = String.toInt s |> fromMaybe (ParseError s)
    catOks (map toInt [ "1", "Foo", "3" ]) == [ 1, 3 ]

-}
catOks : List (Result e a) -> List a
catOks xs =
    case xs of
        [] ->
            []

        (Err _) :: ys ->
            catOks ys

        (Ok y) :: ys ->
            y :: catOks ys


{-| Take a list of Results and return a list of all the Err values.


## Examples

Basic usage:

    catErrs [ Err 1, Ok (), Err 3 ] == [ 1, 3 ]

When constructing a list of Result values, catErrs can be used to return all
of the "failure" results (if the list is the result of a map, then mapMaybe
would be more appropriate):

    type ParseError = ParseError String
    toInt s = String.toInt s |> fromMaybe (ParseError s)
    catOks (map toInt [ "1", "Foo", "3" ]) == [ 1, 3 ]

-}
catErrs : List (Result e a) -> List e
catErrs xs =
    case xs of
        [] ->
            []

        (Err y) :: ys ->
            y :: catErrs ys

        (Ok _) :: ys ->
            catErrs ys


{-| Return second argument if first argument is `Ok`, otherwise first.

    and (Ok 2)              (Err "late error")    == Err "late error"
    and (Err "early error") (Ok "foo")            == Err "early error"
    and (Err "not a 2")     (Err "late error")    == Err "not a 2"
    and (Ok 2)              (Ok "different type") == Ok "different type"

-}
and : Result e a -> Result e a -> Result e a
and r1 r2 =
    case r1 of
        Ok _ ->
            r2

        Err _ ->
            r1


{-| Return first argument if it is `Ok`, otherwise second.

    or (Ok 2)              (Err "late error") == Ok 2
    or (Err "early error") (Ok 2)             == Ok 2
    or (Err "not a 2")     (Err "late error") == Err "late error"
    or (Ok 2)              (Ok 100)           == Ok 2

-}
or : Result e a -> Result e a -> Result e a
or r1 r2 =
    case r1 of
        Ok _ ->
            r1

        Err _ ->
            r2


{-| Chain together a sequence of computations that may fail. It is helpful
to see its definition:

    andThen : (a -> Result e b) -> Result e a -> Result e b
    andThen callback result =
        case result of
          Ok value -> callback value
          Err msg -> Err msg

This means we only continue with the callback if things are going well. For
example, say you need to use (`toInt : String -> Result String Int`) to parse
a month and make sure it is between 1 and 12:


    toValidMonth : Int -> Result String Int
    toValidMonth month =
        if month >= 1 && month <= 12 then
            Ok month

        else
            Err "months must be between 1 and 12"

    toMonth : String -> Result String Int
    toMonth rawString =
        toInt rawString
            |> andThen toValidMonth

    -- toMonth "4" == Ok 4
    -- toMonth "9" == Ok 9
    -- toMonth "a" == Err "cannot parse to an Int"
    -- toMonth "0" == Err "months must be between 1 and 12"

This allows us to come out of a chain of operations with quite a specific error
message. It is often best to create a custom type that explicitly represents
the exact ways your computation may fail. This way it is easy to handle in your
code.

-}
andThen : (a -> Result e b) -> Result e a -> Result e b
andThen =
    Result.andThen


{-| Partition a list of Result into two lists. All the `Err` elements are
extracted, in order, to the first component of the output. Similarly the
`Ok` elements are extracted to the second component of the output.


## Examples

Basic usage:

    let list = [ Err "foo", Ok 3, Err "bar", Ok 7, Err "baz" ]
    partition list == (["foo","bar","baz"],[3,7])

The pair returned by partitionEithers x should be the same pair as (lefts x, rights x):

    let list = [ Err "foo", Ok 3, Err "bar", Ok 7, Err "baz" ]
    partition list == (errs list, oks list) == True

-}
partition : List (Result e a) -> ( List e, List a )
partition list =
    let
        ( res, ros ) =
            L.foldl
                (\r ( es, os ) ->
                    case r of
                        Ok v ->
                            ( es, v :: os )

                        Err e ->
                            ( e :: es, os )
                )
                ( [], [] )
                list
    in
    ( L.reverse res, L.reverse ros )


{-| Apply given function within `Ok` to the second argument if it is `Ok`,
pass throught `Err` otherwise.
-}
apply : Result e (a -> b) -> Result e a -> Result e b
apply rf rv =
    case ( rf, rv ) of
        ( Ok f, Ok v ) ->
            Ok (f v)

        ( Ok _, Err e ) ->
            Err e

        ( Err e, _ ) ->
            Err e
