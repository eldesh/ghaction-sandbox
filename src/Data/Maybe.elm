module Data.Maybe exposing
    ( withDefault, fromMaybe, maybe, isJust, isNothing
    , map, map2, map3, map4, map5, listToMaybe, maybeToList, catMaybes, mapMaybe
    , andThen
    )

{-| An extended toolbox for working with maybes: everything from the `elm/core`
`Maybe` module plus Haskell-inspired helpers such as `maybe`, `isJust` and
`maybeToList`, all implemented the Elm way.


# Definition

[Maybe](https://package.elm-lang.org/packages/elm/core/1.0.5/Maybe)


# Common Helpers

@docs withDefault, fromMaybe, maybe, isJust, isNothing


# Maybe transformations

@docs map, map2, map3, map4, map5, listToMaybe, maybeToList, catMaybes, mapMaybe


# Chaining Maybes

@docs andThen

-}

import Maybe



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
withDefault : a -> Maybe a -> a
withDefault =
    Maybe.withDefault


{-| Provide a default value, turning an optional value into a normal
value.
Alias of [`withDefault`](#withDefault).
-}
fromMaybe : a -> Maybe a -> a
fromMaybe =
    withDefault


{-| Take a default value, a function, and a Maybe value. If the Maybe value
is `Nothing`, the function returns the default value. Otherwise, it applies
the function to the value inside the `Just` and returns the result.


## Examples

Basic usage:

    maybe False odd (Just 3) == True

    maybe False odd Nothing == False

Read an integer from a string. If we succeed, return twice the integer;
that is, apply ((\*) 2) to it. If instead we fail to parse an integer,
return 0 by default:

    maybe 0 ((*) 2) (String.toInt "5") == 10

    maybe 0 ((*) 2) (String.toInt "") == 0

Apply [`fromInt`](#String.fromInt) to a `Maybe Int`. If we have `Just n`,
we want to show the underlying `Int n`. But if we have `Nothing`, we return
the empty string instead of (for example) "Nothing":

    maybe "" String.fromInt (Just 5) == "5"

    maybe "" String.fromInt Nothing == ""

-}
maybe : b -> (a -> b) -> Maybe a -> b
maybe default f m =
    case m of
        Nothing ->
            default

        Just x ->
            f x


{-| Return True iff its argument is of the form `Just _`.


## Examples

Basic usage:

    isJust (Just 3) == True

    isJust (Just ()) == True

    isJust Nothing == False

Only the outer constructor is taken into consideration:

    isJust (Just Nothing) == True

-}
isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        _ ->
            True


{-| Return True iff its argument is `Nothing`.


## Examples

Basic usage:

    isNothing (Just 3) == False

    isNothing (Just ()) == False

    isNothing Nothing == True

Only the outer constructor is taken into consideration:

    isNothing (Just Nothing) == False

-}
isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        _ ->
            False



-- Maybe transformations


{-| Transform a `Maybe` value with a given function:

    map sqrt (Just 9) == Just 3

    map sqrt Nothing == Nothing

    map sqrt (String.toFloat "9") == Just 3

    map sqrt (String.toFloat "x") == Nothing

-}
map : (a -> b) -> Maybe a -> Maybe b
map =
    Maybe.map


{-| Apply a function if all the arguments are `Just` a value.

    map2 (+) (Just 3) (Just 4) == Just 7

    map2 (+) (Just 3) Nothing == Nothing

    map2 (+) Nothing (Just 4) == Nothing

    map2 (+) (String.toInt "1") (String.toInt "123") == Just 124

    map2 (+) (String.toInt "x") (String.toInt "123") == Nothing

    map2 (+) (String.toInt "1") (String.toInt "1.3") == Nothing

-}
map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 =
    Maybe.map2


{-| -}
map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 =
    Maybe.map3


{-| -}
map4 : (a -> b -> c -> d -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe value
map4 =
    Maybe.map4


{-| -}
map5 : (a -> b -> c -> d -> e -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe value
map5 =
    Maybe.map5


{-| Return `Nothing` on an empty list or `Just a` where a is the first element
of the list.


## Examples

Basic usage:

    listToMaybe [] == Nothing

    listToMaybe [ 9 ] == Just 9

    listToMaybe [ 1, 2, 3 ] == Just 1

Composing maybeToList with listToMaybe should be the identity on
singleton/empty lists:

    (maybeToList <| listToMaybe [ 5 ]) == [ 5 ]

    (maybeToList <| listToMaybe []) == []

But not on lists with more than one element:

    (maybeToList <| listToMaybe [ 1, 2, 3 ]) == [ 1 ]

-}
listToMaybe : List a -> Maybe a
listToMaybe xs =
    case xs of
        [] ->
            Nothing

        x :: _ ->
            Just x


{-| Return an empty list when given `Nothing` or a singleton list when
given `Just`.


## Examples

Basic usage:

    maybeToList (Just 7) == [ 7 ]

    maybeToList Nothing == []

One can use maybeToList to avoid pattern matching when combined with a function that (safely) works on lists:

    from Data.List import sum
    (sum <| maybeToList <| String.toInt "3") == 3
    (sum <| maybeToList <| String.toInt "") == 0

-}
maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Nothing ->
            []

        Just x ->
            [ x ]


{-| Take a list of Maybes and return a list of all the Just values.


## Examples

Basic usage:

    catMaybes [ Just 1, Nothing, Just 3 ] == [ 1, 3 ]

When constructing a list of Maybe values, catMaybes can be used to return all of the "success" results (if the list is the result of a map, then mapMaybe would be more appropriate):

    catMaybes (map String.toInt [ "1", "Foo", "3" ]) == [ 1, 3 ]

-}
catMaybes : List (Maybe a) -> List a
catMaybes xs =
    case xs of
        [] ->
            []

        Nothing :: ys ->
            catMaybes ys

        (Just y) :: ys ->
            y :: catMaybes ys


{-| Apply the given function to each element of the list and collect the
results that are `Just _`, discarding any `Nothing`s.
Equivalent to to [`filterMap`](#Data.List.filterMap).


## Examples

Using mapMaybe f x is a shortcut for catMaybes $ map f x in most cases:

    mapMaybe String.toInt [ "1", "Foo", "3" ] == [ 1, 3 ]

    catMaybes (map readMaybeInt [ "1", "Foo", "3" ]) == [ 1, 3 ]

If we map the Just constructor, the entire list should be returned:

    mapMaybe Just [ 1, 2, 3 ] == [ 1, 2, 3 ]

-}
mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe f xs =
    case xs of
        [] ->
            []

        y :: ys ->
            case f y of
                Nothing ->
                    mapMaybe f ys

                Just v ->
                    v :: mapMaybe f ys



-- Chaining Maybes


{-| Chain together many computations that may fail. It is helpful to see its
definition:

    andThen : (a -> Maybe b) -> Maybe a -> Maybe b
    andThen callback maybe =
        case maybe of
            Just value ->
                callback value

            Nothing ->
                Nothing

This means we only continue with the callback if things are going well. For
example, say you need to parse some user input as a month:

    parseMonth : String -> Maybe Int
    parseMonth userInput =
        String.toInt userInput
            |> andThen toValidMonth

    toValidMonth : Int -> Maybe Int
    toValidMonth month =
        if 1 <= month && month <= 12 then
            Just month

        else
            Nothing

In the `parseMonth` function, if `String.toInt` produces `Nothing` (because
the `userInput` was not an integer) this entire chain of operations will
short-circuit and result in `Nothing`. If `toValidMonth` results in `Nothing`,
again the chain of computations will result in `Nothing`.

-}
andThen : (a -> Maybe b) -> Maybe a -> Maybe b
andThen =
    Maybe.andThen
