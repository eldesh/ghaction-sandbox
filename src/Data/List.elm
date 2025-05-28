module Data.List exposing
    ( append, head, last, tail, init, uncons, unsnoc, singleton, null, isEmpty, length, compareLength
    , map, map2, map3, map4, map5, indexedMap, reverse, intersperse, intercalate, transpose, subsequences, permutations, filterMap
    , foldl, foldr
    , concat, concatMap, and, or, any, all, sum, product, maximum, minimum
    , scanl, scanr
    , mapAccumL, mapAccumR
    , iterate, repeat, range, replicate
    , unfoldr
    , take, drop, splitAt, takeWhile, dropWhile, dropWhileEnd, span, break, stripPrefix, group, inits, tails
    , isPrefixOf, isSuffixOf, isInfixOf, isSubsequenceOf
    , elem, notElem, lookup, member
    , find, filter, partition
    , elemIndex, elemIndices, findIndex, findIndices
    , zip, zip3, zipWith, zipWith3, unzip, unzip3
    , nub, delete, deleteFirsts, union, intersect
    , sort, sortOn, insert
    , nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy, groupBy
    , sortBy, sortWith, insertBy, maximumBy, minimumBy
    , genericLength
    )

{-| An extended toolbox for working with lists: everything from the `elm/core`
`List` module plus Haskell-inspired helpers such as `nub`, `intercalate`, and
`groupBy`, all implemented the Elm way.


# Definition

[List](https://package.elm-lang.org/packages/elm/core/1.0.5/List)


# Basic functions

@docs append, head, last, tail, init, uncons, unsnoc, singleton, null, isEmpty, length, compareLength


# List transformations

@docs map, map2, map3, map4, map5, indexedMap, reverse, intersperse, intercalate, transpose, subsequences, permutations, filterMap


# Reducing lists

@docs foldl, foldr


# Special folds

@docs concat, concatMap, and, or, any, all, sum, product, maximum, minimum


# Building lists (Scans)

@docs scanl, scanr


# Building lists (Accumulating maps)

@docs mapAccumL, mapAccumR


# Building lists (infinite lists)

@docs iterate, repeat, range, replicate


# Building lists (Unfolding)

@docs unfoldr


# Sublists (Extracting sublists)

@docs take, drop, splitAt, takeWhile, dropWhile, dropWhileEnd, span, break, stripPrefix, group, inits, tails


# Sublists (Predicates)

@docs isPrefixOf, isSuffixOf, isInfixOf, isSubsequenceOf


# Searching lists (Searching by equality)

@docs elem, notElem, lookup, member


# Searching lists (Searching with a predicate)

@docs find, filter, partition


# Indexing lists

@docs elemIndex, elemIndices, findIndex, findIndices


# Zipping and unzipping lists

@docs zip, zip3, zipWith, zipWith3, unzip, unzip3


# Special lists ("Set" operations)

@docs nub, delete, deleteFirsts, union, intersect


# Special lists (Ordered lists)

@docs sort, sortOn, insert


# Generalized functions (The "By" operations)

@docs nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy, groupBy


# Generalized functions (User-supplied comparison)

@docs sortBy, sortWith, insertBy, maximumBy, minimumBy


# Generalized functions (The "generic" operations)

@docs genericLength

-}

import List
import Maybe



-- Create


{-| Create a list with only one element
-}
singleton : a -> List a
singleton =
    List.singleton


{-| Create a list with _n_ copies of a value:

    repeat 3 ( 0, 0 ) == [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ]

-}
repeat : Int -> a -> List a
repeat =
    List.repeat


{-| Create a list of numbers, every element increasing by one.
You give the lowest and highest number that should be in the list.

    range 3 6 == [ 3, 4, 5, 6 ]

    range 3 3 == [ 3 ]

    range 6 3 == []

-}
range : Int -> Int -> List Int
range =
    List.range



-- Transform


{-| Apply a function to every element of a list.

    map sqrt [ 1, 4, 9 ] == [ 1, 2, 3 ]

    map not [ True, False, True ] == [ False, True, False ]

So `map func [ a, b, c ]` is the same as `[ func a, func b, func c ]`

-}
map : (a -> b) -> List a -> List b
map =
    List.map


{-| Same as `map` but the function is also applied to the index of each
element (starting at zero).

    indexedMap Tuple.pair [ "Tom", "Sue", "Bob" ] == [ ( 0, "Tom" ), ( 1, "Sue" ), ( 2, "Bob" ) ]

-}
indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap =
    List.indexedMap


{-| Reduce a list from the left.

    foldl (+) 0 [ 1, 2, 3 ] == 6

    foldl (::) [] [ 1, 2, 3 ] == [ 3, 2, 1 ]

So `foldl step state [1,2,3]` is like saying:

    state
        |> step 1
        |> step 2
        |> step 3

-}
foldl : (a -> b -> b) -> b -> List a -> b
foldl =
    List.foldl


{-| Reduce a list from the right.

    foldr (+) 0 [ 1, 2, 3 ] == 6

    foldr (::) [] [ 1, 2, 3 ] == [ 1, 2, 3 ]

So `foldr step state [1,2,3]` is like saying:

    state
        |> step 3
        |> step 2
        |> step 1

-}
foldr : (a -> b -> b) -> b -> List a -> b
foldr =
    List.foldr


{-| Keep elements that satisfy the test.

    filter isEven [ 1, 2, 3, 4, 5, 6 ] == [ 2, 4, 6 ]

-}
filter : (a -> Bool) -> List a -> List a
filter =
    List.filter


{-| Filter out certain values. For example, maybe you have a bunch of strings
from an untrusted source and you want to turn them into numbers:


    numbers : List Int
    numbers =
        filterMap String.toInt [ "3", "hi", "12", "4th", "May" ]

    -- numbers == [3, 12]

-}
filterMap : (a -> Maybe b) -> List a -> List b
filterMap =
    List.filterMap



-- Utilities


{-| Determine the length of a list.

    length [ 1, 2, 3 ] == 3

-}
length : List a -> Int
length =
    List.length


{-| Reverse a list.

    reverse [ 1, 2, 3, 4 ] == [ 4, 3, 2, 1 ]

-}
reverse : List a -> List a
reverse =
    List.reverse


{-| Figure out whether a list contains a value.

    member 9 [ 1, 2, 3, 4 ] == False

    member 4 [ 1, 2, 3, 4 ] == True

-}
member : a -> List a -> Bool
member =
    List.member


{-| Determine if all elements satisfy some test.

    all isEven [ 2, 4 ] == True

    all isEven [ 2, 3 ] == False

    all isEven [] == True

-}
all : (a -> Bool) -> List a -> Bool
all =
    List.all


{-| Determine if any elements satisfy some test.

    any isEven [ 2, 3 ] == True

    any isEven [ 1, 3 ] == False

    any isEven [] == False

-}
any : (a -> Bool) -> List a -> Bool
any =
    List.any


{-| Find the maximum element in a non-empty list.

    maximum [ 1, 4, 2 ] == Just 4

    maximum [] == Nothing

-}
maximum : List comparable -> Maybe comparable
maximum =
    List.maximum


{-| Find the minimum element in a non-empty list.

    minimum [ 3, 2, 1 ] == Just 1

    minimum [] == Nothing

-}
minimum : List comparable -> Maybe comparable
minimum =
    List.minimum


{-| Get the sum of the list elements.

    sum [ 1, 2, 3 ] == 6

    sum [ 1, 1, 1 ] == 3

    sum [] == 0

-}
sum : List number -> number
sum =
    List.sum


{-| Get the product of the list elements.

    product [ 2, 2, 2 ] == 8

    product [ 3, 3, 3 ] == 27

    product [] == 1

-}
product : List number -> number
product =
    List.product



-- Combine


{-| Put two lists together.

    append [ 1, 1, 2 ] [ 3, 5, 8 ] == [ 1, 1, 2, 3, 5, 8 ]

    append [ 'a', 'b' ] [ 'c' ] == [ 'a', 'b', 'c' ]

You can also use [the `(++)` operator](Basics#++) to append lists.

-}
append : List a -> List a -> List a
append =
    List.append


{-| Concatenate a bunch of lists into a single list:

    concat [ [ 1, 2 ], [ 3 ], [ 4, 5 ] ] == [ 1, 2, 3, 4, 5 ]

-}
concat : List (List a) -> List a
concat =
    List.concat


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> List b) -> List a -> List b
concatMap =
    List.concatMap


{-| Places the given value between all members of the given list.

    intersperse "on" [ "turtles", "turtles", "turtles" ] == [ "turtles", "on", "turtles", "on", "turtles" ]

-}
intersperse : a -> List a -> List a
intersperse =
    List.intersperse


{-| Combine two lists, combining them with the given function.
If one list is longer, the extra elements are dropped.


    totals : List Int -> List Int -> List Int
    totals xs ys =
        List.map2 (+) xs ys

    -- totals [1,2,3] [4,5,6] == [5,7,9]
    pairs : List a -> List b -> List ( a, b )
    pairs xs ys =
        List.map2 Tuple.pair xs ys

    -- pairs ["alice","bob","chuck"] [2,5,7,8]
    --   == [("alice",2),("bob",5),("chuck",7)]

-}
map2 : (a -> b -> result) -> List a -> List b -> List result
map2 =
    List.map2


{-| -}
map3 : (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 =
    List.map3


{-| -}
map4 : (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
map4 =
    List.map4


{-| -}
map5 : (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
map5 =
    List.map5



-- Sort


{-| Sort values from lowest to highest

    sort [ 3, 1, 5 ] == [ 1, 3, 5 ]

-}
sort : List comparable -> List comparable
sort =
    List.sort


{-| Sort values with a custom comparison function.

    sortWith flippedComparison [1,2,3,4,5] == [5,4,3,2,1]

    flippedComparison a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

This is also the most general sort function, allowing you
to define any other: `sort == sortWith compare`

-}
sortWith : (a -> a -> Order) -> List a -> List a
sortWith =
    List.sortWith



-- Deconstruct


{-| Determine if a list is empty.

    isEmpty [] == True

-}
isEmpty : List a -> Bool
isEmpty =
    List.isEmpty


{-| Extract the first element of a list.

    head [ 1, 2, 3 ] == Just 1

    head [] == Nothing

-}
head : List a -> Maybe a
head =
    List.head


{-| Extract the rest of the list.

    tail [ 1, 2, 3 ] == Just [ 2, 3 ]

    tail [] == Nothing

-}
tail : List a -> Maybe (List a)
tail =
    List.tail


{-| Take the first _n_ members of a list.

    take 2 [ 1, 2, 3, 4 ] == [ 1, 2 ]

-}
take : Int -> List a -> List a
take =
    List.take


{-| Drop the first _n_ members of a list.

    drop 2 [ 1, 2, 3, 4 ] == [ 3, 4 ]

-}
drop : Int -> List a -> List a
drop =
    List.drop


{-| Partition a list based on some test. The first list contains all values
that satisfy the test, and the second list contains all the value that do not.

    partition (\x -> x < 3) [ 0, 1, 2, 3, 4, 5 ] == ( [ 0, 1, 2 ], [ 3, 4, 5 ] )

    partition isEven [ 0, 1, 2, 3, 4, 5 ] == ( [ 0, 2, 4 ], [ 1, 3, 5 ] )

-}
partition : (a -> Bool) -> List a -> ( List a, List a )
partition =
    List.partition


{-| Decompose a list of tuples into a tuple of lists.

    unzip [ ( 0, True ), ( 17, False ), ( 1337, True ) ] == ( [ 0, 17, 1337 ], [ True, False, True ] )

-}
unzip : List ( a, b ) -> ( List a, List b )
unzip =
    List.unzip



-- Extensions


{-| Get the last element of a list. Return `Just` the element if the list is
non-empty, or `Nothing` when the list is empty.

    last [ 1, 2, 3 ] == Just 3

    last [] == Nothing

-}
last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: xs ->
            last xs


{-| Return all but the last element of a list.
If the list is non-empty, returns `Just` the leading elements (possibly `[]` for a single-element list).
If the list is empty, returns `Nothing`.

    init [ 1, 2, 3 ] == Just [ 1, 2 ]

    init [ 1 ] == Just []

    init [] == Nothing

-}
init : List a -> Maybe (List a)
init list =
    case list of
        [] ->
            Nothing

        [ _ ] ->
            Just []

        x :: xs ->
            case init xs of
                Nothing ->
                    Just [ x ]

                Just ys ->
                    Just (x :: ys)


{-| Decompose a list into its first element and the remaining elements.

For a non-empty list, returns `Just ( x, xs )`.
For an empty list, returns `Nothing`.

    uncons [ 1, 2, 3 ] == Just ( 1, [ 2, 3 ] )

    uncons [] == Nothing

-}
uncons : List a -> Maybe ( a, List a )
uncons list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


{-| Decompose a list into its initial elements and its last element.

For a non-empty list, returns `Just ( ys, y )` where `ys ++ [ y ]` reconstructs the original list.
For an empty list, returns `Nothing`.

    unsnoc [ 1, 2, 3 ] == Just ( [ 1, 2 ], 3 )

    unsnoc [ 1 ] == Just ( [], 1 )

    unsnoc [] == Nothing

-}
unsnoc : List a -> Maybe ( List a, a )
unsnoc list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            case unsnoc xs of
                Nothing ->
                    Just ( [], x )

                Just ( ys, y ) ->
                    Just ( x :: ys, y )


{-| Determine whether a list is empty.

    null [] == True

    null [ 1 ] == False

Alias of [`isEmpty`](#isEmpty).

-}
null : List a -> Bool
null list =
    case list of
        [] ->
            True

        _ :: _ ->
            False


{-| Compare the length of a list with an integer.

Return `GT` if the list is longer, `LT` if shorter, `EQ` if equal.
Any positive-length list is `GT` for a negative integer.

    compareLength [ 1, 2, 3 ] 2 == GT

    compareLength [ 1, 2 ] 2 == EQ

    compareLength [ 1 ] 2 == LT

-}
compareLength : List a -> Int -> Order
compareLength list s =
    let
        go xs n =
            case ( xs, n ) of
                ( [], 0 ) ->
                    EQ

                ( [], m ) ->
                    compare 0 m

                ( _ :: _, 0 ) ->
                    GT

                ( _ :: ys, m ) ->
                    go ys (m - 1)
    in
    if s < 0 then
        GT

    else
        go list s



-- List transformation


{-| Concatenates the lists in the second argument, inserting the first argument
between each adjacent pair.

    intercalate [ 0, 1 ] [ [ 2, 3 ], [ 4, 5, 6 ], [] ] == [ 2, 3, 0, 1, 4, 5, 6, 0, 1 ]

    intercalate [ 1, 2, 3 ] [ [], [] ] == [ 1, 2, 3 ]

    let
        s =
            String.toList
    in
    intercalate (s ", ") [ s "Lorem", s "ipsum", s "dolor" ]
        == s "Lorem, ipsum, dolor"

-}
intercalate : List a -> List (List a) -> List a
intercalate xs xxs =
    case xxs of
        [] ->
            []

        [ yys ] ->
            yys

        ys :: yys ->
            (ys ++ xs) ++ intercalate xs yys


{-| Transpose the rows and columns of the argument list of lists.

    transpose [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] == [ [ 1, 4 ], [ 2, 5 ], [ 3, 6 ] ]

If some of the rows are shorter than the following rows, their elements are skipped:

    transpose [ [ 10, 11 ], [ 20 ], [], [ 30, 31, 32 ] ] == [ [ 10, 20, 30 ], [ 11, 31 ], [ 32 ] ]

-}
transpose : List (List a) -> List (List a)
transpose list =
    let
        go xs =
            case xs of
                [] ->
                    []

                [ [] ] ->
                    []

                xxs ->
                    let
                        ( hds, tls ) =
                            unzip (filterMap uncons xxs)
                    in
                    hds :: go tls

        isNotEmpty xs =
            not (isEmpty xs)
    in
    filter isNotEmpty (go list)


{-| Return the list of all subsequences of the argument.

    let
        s =
            String.toList
    in
    subsequences (s "abc") == map s [ "", "a", "b", "ab", "c", "ac", "bc", "abc" ]

-}
subsequences : List a -> List (List a)
subsequences list =
    case list of
        [] ->
            [ [] ]

        x :: xs ->
            let
                ss =
                    subsequences xs
            in
            append (map (\s -> x :: s) ss) ss


{-| Return the list of all permutations of the argument.
Note that the order of permutations is not lexicographic.


## Properties

    map (take n)
        (take (product (List.range 1 n)) (permutations (List.range 1 n)))
        == permutations (range 1 n)


## Examples

    permutations [ 1, 2 ] == [ [ 1, 2 ], [ 2, 1 ] ]

    permutations [] == [ [] ]

    let
        s =
            String.toList
    in
    permutations "abc" == [ "abc", "bac", "cba", "bca", "cab", "acb" ]

-}
permutations : List a -> List (List a)
permutations list =
    let
        perm : List a -> List a -> List (List a)
        perm xs ys =
            case uncons xs of
                Nothing ->
                    []

                Just ( z, zs ) ->
                    let
                        ps =
                            permutations (zs ++ ys)
                    in
                    perm zs (z :: ys) ++ map (\p -> z :: p) ps
    in
    case list of
        [] ->
            [ [] ]

        [ x ] ->
            [ [ x ] ]

        _ ->
            perm list []


{-| Return the logical conjunction of a list of `Bool`s.

Evaluation of this function stops at the first occurrence of `True`, and the result is `True`.

    and [] == True

    and [ True ] == True

    and [ False ] == False

    and [ True, True, False ] == False

-}
and : List Bool -> Bool
and list =
    case list of
        [] ->
            True

        x :: xs ->
            x && and xs


{-| Return the logical disjunction of a list of `Bool`s.

Evaluation of this function stops at the first occurrence of `True`, and the result is `True`.

    or [] == False

    or [ False ] == False

    or [ True ] == True

    or [ True, True, False ] == True

-}
or : List Bool -> Bool
or list =
    case list of
        [] ->
            False

        x :: xs ->
            x || or xs



-- Building Lists


{-| The list of successive reduced values from the left.

    scanl f z [x1, x2, ...] == [z, f x1 z, f x2 (f x1 z), ...]


## Properties

    last (scanl f z xs) == foldl f z xs


## Examples

    scanl (+) 0 (List.range 1 4) == [ 0, 1, 3, 6, 10 ]

    scanl (+) 42 [] == [ 42 ]

    scanl (-) 100 (List.range 1 4) == [ 100, 99, 97, 94, 90 ]

    scanl String.cons "foo" [ 'a', 'b', 'c', 'd' ]
        == [ "foo", "afoo", "bafoo", "cbafoo", "dcbafoo" ]

    scanl (+) 0 (range 1 10) == [ 0, 1, 3, 6, 10, 15, 21, 28, 36, 45 ]

-}
scanl : (a -> b -> b) -> b -> List a -> List b
scanl f e list =
    case list of
        [] ->
            [ e ]

        x :: xs ->
            e :: scanl f (f x e) xs


{-| The right-to-left dual of scanl. Note that the order of parameters on the
accumulating function are reversed compared to scanl. Also note that

    head (scanr f z xs) == foldr f z xs.


## Examples

    scanr (+) 0 (List.range 1 4) == [ 10, 9, 7, 4, 0 ]

    scanr (+) 42 [] == [ 42 ]

    scanr (-) 100 (List.range 1 4) == [ 98, -97, 99, -96, 100 ]

    scanr String.cons "foo" [ 'a', 'b', 'c', 'd' ]
        == [ "abcdfoo", "bcdfoo", "cdfoo", "dfoo", "foo" ]

-}
scanr : (a -> b -> b) -> b -> List a -> List b
scanr f e list =
    case list of
        [] ->
            [ e ]

        x :: xs ->
            case scanr f e xs of
                [] ->
                    -- unreachable
                    []

                y :: ys ->
                    f x y :: (y :: ys)



-- Accumulating maps


{-| Behaves like a combination of [`map`](#map) and [`foldl`](#foldl); it applies a function to
each element of a list, passing an accumulating parameter from left to right,
and returning a final value of this accumulator together with the new list.

    mapAccumL (\a b -> ( a + b, a )) 0 (range 1 10)
        == ( 55, [ 0, 1, 3, 6, 10, 15, 21, 28, 36, 45 ] )

    mapAccumL (\a b -> ( a ++ String.fromInt b, a )) "0" (range 1 5)
        == ( "012345", [ "0", "01", "012", "0123", "01234" ] )

-}
mapAccumL : (s -> a -> ( s, b )) -> s -> List a -> ( s, List b )
mapAccumL f acc list =
    case list of
        [] ->
            ( acc, [] )

        x :: xs ->
            let
                ( bcc, y ) =
                    f acc x

                ( ccc, ys ) =
                    mapAccumL f bcc xs
            in
            ( ccc, y :: ys )


{-| Behaves like a combination of [`map`](#map) and [`foldr`](#foldr); it applies a function to
each element of a list, passing an accumulating parameter from right to left,
and returning a final value of this accumulator together with the new list.

    mapAccumR (\a b -> ( a + b, a )) 0 (range 1 10)
        == ( 55, [ 54, 52, 49, 45, 40, 34, 27, 19, 10, 0 ] )

    mapAccumR (\a b -> ( a ++ String.fromInt b, a )) "0" (range 1 5)
        == ( "054321", [ "05432", "0543", "054", "05", "0" ] )

-}
mapAccumR : (s -> a -> ( s, b )) -> s -> List a -> ( s, List b )
mapAccumR f acc list =
    case list of
        [] ->
            ( acc, [] )

        x :: xs ->
            let
                ( bcc, ys ) =
                    mapAccumR f acc xs

                ( ccc, y ) =
                    f bcc x
            in
            ( ccc, y :: ys )



-- Generating list


{-| `n` repeated applications `f` to `x`


## Properties

    iterate n f x == [x, f x, f (f x), ...]


## Examples

    iterate 3 ((+) 1) 0 == [ 0, 1, 2 ]

-}
iterate : Int -> (a -> a) -> a -> List a
iterate n f x =
    if n <= 0 then
        []

    else
        x :: iterate (n - 1) f (f x)


{-| Alias to [`repeat`](#repeat)
-}
replicate : Int -> a -> List a
replicate =
    repeat



-- Unfolding


{-| A dual to [`foldr`](#foldr): while foldr reduces a list to a summary value,
unfoldr builds a list from a seed value. The function takes the element
and returns `Nothing` if it is done producing the list or returns `Just (a,b)`,
in which case, `a` is a prepended to the list
and `b` is used as the next element in a recursive call.

In some cases, unfoldr can undo a foldr operation:

    unfoldr g (foldr f z xs) == xs

if the following holds:

    g (f x y) = Just (x,y)
    g z       = Nothing

For example:

    unfoldr
        (\b ->
            if b == 0 then
                Nothing

            else
                Just ( b, b - 1 )
        )
        10
        == [ 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 ]

-}
unfoldr : (b -> Maybe ( a, b )) -> b -> List a
unfoldr f e =
    case f e of
        Nothing ->
            []

        Just ( x, next ) ->
            x :: unfoldr f next



-- Sublists


{-| Return a tuple where first element is xs prefix of length n
and second element is the remainder of the list:


## Examples

    splitAt 6 (String.toList "Hello World!")
        == ( String.toList "Hello ", String.toList "World!" )

    splitAt 3 [ 1, 2, 3, 4, 5 ] == ( [ 1, 2, 3 ], [ 4, 5 ] )

    splitAt 1 [ 1, 2, 3 ] == ( [ 1 ], [ 2, 3 ] )

    splitAt 3 [ 1, 2, 3 ] == ( [ 1, 2, 3 ], [] )

    splitAt 4 [ 1, 2, 3 ] == ( [ 1, 2, 3 ], [] )

    splitAt 0 [ 1, 2, 3 ] == ( [], [ 1, 2, 3 ] )

    splitAt -1 [ 1, 2, 3 ] == ( [], [ 1, 2, 3 ] )

-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n list =
    if n <= 0 then
        ( [], list )

    else
        case list of
            [] ->
                ( list, [] )

            x :: xs ->
                let
                    ( ys, zs ) =
                        splitAt (n - 1) xs
                in
                ( x :: ys, zs )


{-| Applied to a predicate p and a list xs, returns the longest
prefix (possibly empty) of xs of elements that satisfy p.


## Examples

    takeWhile (\x -> x < 3) [ 1, 2, 3, 4, 1, 2, 3, 4 ] == [ 1, 2 ]

    takeWhile (\x -> x < 9) [ 1, 2, 3 ] == [ 1, 2, 3 ]

    takeWhile (\x -> x < 0) [ 1, 2, 3 ] == []

-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list =
    case list of
        [] ->
            []

        x :: xs ->
            if p x then
                x :: takeWhile p xs

            else
                []


{-| Return the suffix remaining after [`takeWhile`](#takeWhile)


## Examples

    dropWhile (\x -> x < 3) [ 1, 2, 3, 4, 5, 1, 2, 3 ] == [ 3, 4, 5, 1, 2, 3 ]

    dropWhile (\x -> x < 9) [ 1, 2, 3 ] == []

    dropWhile (\x -> x < 0) [ 1, 2, 3 ] == [ 1, 2, 3 ]

-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile p list =
    case list of
        [] ->
            []

        x :: xs ->
            if p x then
                dropWhile p xs

            else
                list


{-| Drops the largest suffix of a list in which the given predicate holds for
all elements.


## Examples

    let
        isSpace c =
            0x09 <= Char.toCode c && Char.toCode c <= 0x0D
    in
    dropWhileEnd isSpace (String.toList "foo\n") == String.toList "foo"

    let
        isSpace c =
            0x09 <= Char.toCode c && Char.toCode c <= 0x0D
    in
    dropWhileEnd isSpace (String.toList "foo bar") == String.toList "foo bar"

    dropWhileEnd (\x -> x > 10) (List.range 1 20) == [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]

-}
dropWhileEnd : (a -> Bool) -> List a -> List a
dropWhileEnd p list =
    let
        go notps ps xs =
            case xs of
                [] ->
                    reverse notps

                y :: ys ->
                    if p y then
                        go notps (y :: ps) ys

                    else
                        go (y :: (ps ++ notps)) [] ys
    in
    go [] [] list


{-| Return a tuple where first element is the longest prefix (possibly empty)
of xs of elements that satisfy p and second element is the remainder of the
list.


## Examples

    span (\x -> x < 3) [ 1, 2, 3, 4, 1, 2, 3, 4 ] == ( [ 1, 2 ], [ 3, 4, 1, 2, 3, 4 ] )

    span (\x -> x < 9) [ 1, 2, 3 ] == ( [ 1, 2, 3 ], [] )

    span (\x -> x < 0) [ 1, 2, 3 ] == ( [], [ 1, 2, 3 ] )

-}
span : (a -> Bool) -> List a -> ( List a, List a )
span p list =
    let
        go acc xs =
            case xs of
                [] ->
                    ( reverse acc, [] )

                y :: ys ->
                    if p y then
                        go (y :: acc) ys

                    else
                        ( reverse acc, xs )
    in
    go [] list


{-| Return a tuple where first element is longest prefix (possibly empty)
of xs of elements that do not satisfy p and second element is the remainder of
the list:


## Properties

    break p
        == span (not << p)
        == ( takeWhile (not << p) xs, dropWhile (not << p) xs )


## Examples

    break (\x -> x > 3) [ 1, 2, 3, 4, 1, 2, 3, 4 ] == ( [ 1, 2, 3 ], [ 4, 1, 2, 3, 4 ] )

    break (\x -> x < 9) [ 1, 2, 3 ] == ( [], [ 1, 2, 3 ] )

    break (\x -> x > 9) [ 1, 2, 3 ] == ( [ 1, 2, 3 ], [] )

-}
break : (a -> Bool) -> List a -> ( List a, List a )
break p list =
    let
        go acc xs =
            case xs of
                [] ->
                    ( reverse acc, [] )

                y :: ys ->
                    if not (p y) then
                        go (y :: acc) ys

                    else
                        ( reverse acc, xs )
    in
    go [] list


{-| Drops the given prefix from a list. It returns Nothing if the list did not
start with the prefix given, or Just the list after the prefix, if it does.


## Examples

    stripPrefix (String.toList "foo") (String.toList "foobar")
        == Just (String.toList "bar")

    stripPrefix (String.toList "foo") (String.toList "foo")
        == Just (String.toList "")

    stripPrefix (String.toList "foo") (String.toList "barfoo")
        == Nothing

    stripPrefix (String.toList "foo") (String.toList "barfoobaz")
        == Nothing

-}
stripPrefix : List a -> List a -> Maybe (List a)
stripPrefix xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            Just ys

        ( _ :: _, [] ) ->
            Nothing

        ( x :: xss, y :: yss ) ->
            if x == y then
                stripPrefix xss yss

            else
                Nothing


{-| Return a list of lists such that the concatenation of the result is equal
to the argument. Moreover, each sublist in the result is non-empty,
all elements are equal to the first one, and consecutive equal
elements of the input end up in the same element of the output list.

`group` is a special case of [`groupBy`](#groupBy), which allows the programmer to supply
their own equality test.


## Examples

    group (String.toList "Mississippi")
        |> map String.fromList
        == [ "M", "i", "ss", "i", "ss", "i", "pp", "i" ]

    group [ 1, 1, 1, 2, 2, 3, 4, 5, 5 ] == [ [ 1, 1, 1 ], [ 2, 2 ], [ 3 ], [ 4 ], [ 5, 5 ] ]

-}
group : List a -> List (List a)
group list =
    case list of
        [] ->
            []

        x :: xs ->
            case group xs of
                [] ->
                    [ [ x ] ]

                ys :: yss ->
                    if Just x == head ys then
                        (x :: ys) :: yss

                    else
                        [ x ] :: ys :: yss


{-| Return all initial segments of the argument, shortest first.
inits is semantically equivalent to

    map reverse . scanl (flip (:)) []

, but under the hood uses a queue to amortize costs of reverse.


## Examples

    inits (String.toList "abc")
        |> map String.fromList
        == [ "", "a", "ab", "abc" ]

    inits [] == [ [] ]

-}
inits : List a -> List (List a)
inits list =
    case list of
        [] ->
            [ [] ]

        x :: xs ->
            [] :: map ((::) x) (inits xs)


{-| Return all final segments of the argument, longest first.


## Examples

    tails (String.toList "abc")
        |> map String.fromList
        == [ "abc", "bc", "c", "" ]

    tails [ 1, 2, 3 ] == [ [ 1, 2, 3 ], [ 2, 3 ], [ 3 ], [] ]

    tails [] == [ [] ]

-}
tails : List a -> List (List a)
tails list =
    case list of
        [] ->
            [ [] ]

        _ :: xs ->
            list :: tails xs



-- Predicates


{-| Takes two lists and returns True iff the first list is a prefix of the
second.


## Examples

    isPrefixOf (String.toList "Hello") (String.toList "Hello World!") == True

    isPrefixOf (String.toList "Hello") (String.toList "Wello Horld!") == False

isPrefixOf shortcuts when the first argument is empty:

    forall x, isPrefixOf [] x == True

-}
isPrefixOf : List a -> List a -> Bool
isPrefixOf prefix list =
    case ( prefix, list ) of
        ( [], _ ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( x :: xs, y :: ys ) ->
            if x == y then
                isPrefixOf xs ys

            else
                False


{-| Takes two lists and returns `True` iff the first
list is a suffix of the second.


## Examples

    isSuffixOf (String.toList "ld!") (String.toList "Hello World!") == True

    isSuffixOf (String.toList "World") (String.toList "Hello World!") == False

-}
isSuffixOf : List a -> List a -> Bool
isSuffixOf suffix list =
    let
        rlist =
            reverse list
    in
    isPrefixOf (reverse suffix) rlist


{-| Takes two lists and returns `True` iff the first list is contained,
wholly and intact, anywhere within the second.


## Examples

    isInfixOf (String.toList "Haskell")
        (String.toList "I really like Haskell.")
        == True

    isInfixOf (String.toList "Ial")
        (String.toList "I really like Haskell.")
        == False

-}
isInfixOf : List a -> List a -> Bool
isInfixOf xs list =
    isPrefixOf xs list
        || (case list of
                [] ->
                    False

                _ :: ys ->
                    isInfixOf xs ys
           )


{-| Return `True` if all the elements of the first list occur, in order,
in the second. The elements do not have to occur consecutively.


## Properties

    isSubsequenceOf x y == elem x (subsequences y)

Note:

    isSubsequenceOf is often used in infix form.


## Examples

    isSubsequenceOf (String.toList "GHC")
        (String.toList "The Glorious Haskell Compiler") == True
    isSubsequenceOf ['a'..'z'] == True
    isSubsequenceOf [1..10] [10,9..0] == False

-}
isSubsequenceOf : List a -> List a -> Bool
isSubsequenceOf xs list =
    case ( xs, list ) of
        ( [], _ ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( y :: ys, z :: zs ) ->
            if y == z then
                isSubsequenceOf ys zs

            else
                isSubsequenceOf xs zs



-- Searching lists


{-| Does the element occur in the structure?

Note:

    elem is often used in infix form.


## Examples

    elem 3 [] == False

    elem 3 [ 1, 2 ] == False

    elem 3 [ 1, 2, 3, 4, 5 ] == True

-}
elem : a -> List a -> Bool
elem =
    member


{-| The negation of `elem`.


## Examples

    notElem 3 [] == True

    notElem 3 [ 1, 2 ] == True

    notElem 3 [ 1, 2, 3, 4, 5 ] == False

-}
notElem : a -> List a -> Bool
notElem a list =
    case list of
        [] ->
            True

        x :: xs ->
            x /= a && notElem a xs


{-| Looks up the value associated with the key in an association list.


## Examples

    lookup 2 [] == Nothing

    lookup 2 [ ( 1, "first" ) ] == Nothing

    lookup 2 [ ( 1, "first" ), ( 2, "second" ), ( 3, "third" ) ]
        == Just "second"

-}
lookup : a -> List ( a, b ) -> Maybe b
lookup v list =
    case list of
        [] ->
            Nothing

        ( a, b ) :: abs ->
            if a == v then
                Just b

            else
                lookup v abs


{-| Return the leftmost element of the list matching the predicate,
or `Nothing` if there is no such element.


## Examples

    find (\x -> x > 42) [ 5, 10, 15, 20, 25, 30, 35, 40, 45, 50 ] == Just 45

    find (\x -> x > 12) (range 1 7) == Nothing

-}
find : (a -> Bool) -> List a -> Maybe a
find p list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if p x then
                Just x

            else
                find p xs


{-| Return the index of the first element in the given list which is equal to
the query element, or `Nothing` if there is no such element.


## Examples

    elemIndex 4 (range 0 10) == Just 4

    elemIndex 'o' (String.toList "haskell") == Nothing

-}
elemIndex : a -> List a -> Maybe Int
elemIndex a list =
    let
        go n xs =
            case xs of
                [] ->
                    Nothing

                y :: ys ->
                    if y == a then
                        Just n

                    else
                        go (n + 1) ys
    in
    go 0 list


{-| Return the indicies of all elements equal to the query element, in
ascending order.


## Examples

    elemIndices 'o' (String.toList "Hello World") == [ 4, 7 ]

    elemIndices 1 [ 1, 2, 3, 1, 2, 3 ] == [ 0, 3 ]

-}
elemIndices : a -> List a -> List Int
elemIndices a list =
    let
        go n xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    if y == a then
                        n :: go (n + 1) ys

                    else
                        go (n + 1) ys
    in
    go 0 list


{-| Return the index of the first element in the list satisfying the predicate,
or `Nothing` if there is no such element.


## Examples

    let
        isSpace c =
            0x09 <= Char.toCode c && Char.toCode c <= 0x0D
    in
    findIndex isSpace (String.toList "Hello World!") == Just 5

    findIndex ((/=) 0 << modBy 2) [ 0, 2, 4, 6 ] == Nothing

    findIndex ((==) 0 << modBy 2) [ 1, 2, 3, 4 ] == Just 1

-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex p list =
    let
        go n xs =
            case xs of
                [] ->
                    Nothing

                y :: ys ->
                    if p y then
                        Just n

                    else
                        go (n + 1) ys
    in
    go 0 list


{-| Extends `findIndex`, by returning the indices of all elements satisfying
the predicate, in ascending order.


## Examples

    findIndices (\x -> elem x (String.toList "aeiou"))
        (String.toList "Hello World!")
        == [ 1, 4, 7 ]

    findIndices (\l -> String.length l > 3)
        [ "a", "bcde", "fgh", "ijklmnop" ]
        == [ 1, 3 ]

-}
findIndices : (a -> Bool) -> List a -> List Int
findIndices p list =
    let
        go n xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    if p y then
                        n :: go (n + 1) ys

                    else
                        go (n + 1) ys
    in
    go 0 list



-- Zipping and unzipping lists


{-| Return a list of corresponding pairs.


## Examples

    zip [ 1, 2, 3 ] [ 'a', 'b', 'c' ] == [ ( 1, 'a' ), ( 2, 'b' ), ( 3, 'c' ) ]

If one input list is shorter than the other, excess elements of the longer
list are discarded.

    zip [ 1 ] [ 'a', 'b' ] == [ ( 1, 'a' ) ]

    zip [ 1, 2 ] [ 'a' ] == [ ( 1, 'a' ) ]

    zip [] [ 1, 2, 3 ] == []

    zip [ 1, 2, 3 ] [] == []

-}
zip : List a -> List b -> List ( a, b )
zip xs ys =
    zipWith Tuple.pair xs ys


{-| Return a list of triples, analogous to [`zip`](#zip).
-}
zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 xs ys zs =
    zipWith3 (\x y z -> ( x, y, z )) xs ys zs


{-| generalises zip by zipping with the function given as the first argument,
instead of a tupling function.

    zipWith (,) xs ys == zip xs ys
    zipWith f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]

zipWith is right-lazy:

    let
        bot () =
            bot ()
    in
    zipWith bot [] [ () ] == []


## Examples

    zipWith (+) [ 1, 2, 3 ] [ 4, 5, 6 ] == [ 5, 7, 9 ]

    zipWith (++) [ "hello ", "foo" ] [ "world!", "bar" ]
        == [ "hello world!", "foobar" ]

-}
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys =
    case ( xs, ys ) of
        ( x :: xss, y :: yss ) ->
            f x y :: zipWith f xss yss

        _ ->
            []


{-| Combines three elements, as well as three lists and returns a list of the
function applied to corresponding elements, analogous to [`zipWith`](#zipWith).

    zipWith3 (\x y z -> ( x, y, z )) xs ys zs == zip3 xs ys zs

    zipWith3 f [ x1, x2, x3 ] [ y1, y2, y3 ] [ z1, z2, z3 ]
        == [ f x1 y1 z1, f x2 y2 z2, f x3 y3 z3 ]


## Examples

    zipWith3 (\x y z -> [ x, y, z ])
        (String.toList "123")
        (String.toList "abc")
        (String.toList "xyz")
        |> map String.fromList
        == [ "1ax", "2by", "3cz" ]

    zipWith3 (\x y z -> (x * y) + z)
        [ 1, 2, 3 ]
        [ 4, 5, 6 ]
        [ 7, 8, 9 ]
        == [ 11, 18, 27 ]

-}
zipWith3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zipWith3 f xs ys zs =
    case ( xs, ys, zs ) of
        ( a :: xss, b :: yss, c :: zss ) ->
            f a b c :: zipWith3 f xss yss zss

        _ ->
            []


{-| Takes a list of triples and returns three lists of the respective
components, analogous to [`unzip`](#unzip).


## Examples

    unzip3 [] == ( [], [], [] )

    unzip3 [ ( 1, 'a', True ), ( 2, 'b', False ) ]
        == ( [ 1, 2 ], [ 'a', 'b' ], [ True, False ] )

-}
unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 list =
    let
        go (( xs, ys, zs ) as acc) ls =
            case ls of
                [] ->
                    ( reverse xs, reverse ys, reverse zs )

                ( x, y, z ) :: lss ->
                    go ( x :: xs, y :: ys, z :: zs ) lss
    in
    go ( [], [], [] ) list



-- Set operations


{-| Remove duplicate elements from a list. In particular, it keeps only the
first occurrence of each element. (The name nub means \`essence'.)

This is a special case of [`nubBy`](#nubBy), which allows the programmer to
supply their own equality test.

Performance: `nub` runs in _O(n^2)_ time because it compares each element with
all preceding ones. If you do not need to preserve the original order and
elements are `comparable`, you can get _O(nlogn)_ with:

    filterMap head << group << sort`

The result will be sorted rather than in the original order.


## Examples

    nub [ 1, 2, 3, 4, 3, 2, 1, 2, 4, 3, 5 ] == [ 1, 2, 3, 4, 5 ]

    nub (String.toList "hello, world!") == String.toList "helo, wrd!"

-}
nub : List a -> List a
nub =
    nubBy (==)


{-| Removes the first occurrence of x from its list argument.
It is a special case of deleteBy, which allows the programmer to supply their
own equality test.


## Examples

    delete 'a' (String.toList "banana") |> String.fromList == "bnana"

    delete "not" [ "haskell", "is", "not", "awesome" ] == [ "haskell", "is", "awesome" ]

-}
delete : a -> List a -> List a
delete =
    deleteBy (==)


{-| List difference (non-associative). In the result of delete xs ys, the
first occurrence of each element of ys in turn (if any) has been removed
from xs. Thus `delete (xs ++ ys) xs == ys`.

It is a special case of [`deleteFirstsBy`](#deleteFirstsBy), which allows the
programmer to supply their own equality test.


## Examples

    delete (String.toList "Hello World!") (String.toList "ell W")
        |> String.fromList
        == "Hoorld!"

-}
deleteFirsts : List a -> List a -> List a
deleteFirsts =
    deleteFirstsBy (==)


{-| Return the list union of the two lists. It is a special case of
[`unionBy`](#unionBy), which allows the programmer to supply their own
equality test.


## Examples

    union (String.toList "dog") (String.toList "cow")
        |> String.fromList
        == "dogcw"

All elements in the first list are kept including duplicates.
From the second list, each element is added only if it has not appeared
earlier in the result.

    union (String.toList "coot") (String.toList "duck")
        |> String.fromList
        == "cootduk"

    union (String.toList "duck") (String.toList "coot")
        |> String.fromList
        == "duckot"

-}
union : List a -> List a -> List a
union =
    unionBy (==)


{-| Takes the list intersection of two lists. It is a special case of
[`intersectBy`](#intersectBy), which allows the programmer to supply their own
equality test.


## Examples

    intersect [ 1, 2, 3, 4 ] [ 2, 4, 6, 8 ] == [ 2, 4 ]

If equal elements are present in both lists, an element from the first list
will be used, and all duplicates from the second list quashed:

    intersect [ 1, 1, 2 ] [ 1, 2, 3 ] == [ 1, 1, 2 ]

However if the first list contains duplicates, so will the result.

    intersect (String.toList "coot") (String.toList "heron")
        |> String.fromList
        == "oo"

    intersect (String.toList "heron") (String.toList "coot")
        |> String.fromList
        == "o"

-}
intersect : List a -> List a -> List a
intersect =
    intersectBy (==)


{-| Sort a list by comparing the results of a key function applied to
each element. `sortOn f` is equivalent to
`sortBy (\x y -> compare (f x) (f y))`, but has the performance advantage of
only evaluating f once for each element in the input list. This is called the
decorate-sort-undecorate paradigm, or Schwartzian transform.

Elements are arranged from lowest to highest, keeping duplicates in the
order they appeared in the input.


## Examples

    sortOn Tuple.first [ ( 2, "world" ), ( 4, "!" ), ( 1, "Hello" ) ]
        == [ ( 1, "Hello" ), ( 2, "world" ), ( 4, "!" ) ]

    sortOn String.length [ "jim", "creed", "pam", "michael", "dwight", "kevin" ]
        == [ "jim", "pam", "creed", "kevin", "dwight", "michael" ]

-}
sortOn : (a -> comparable) -> List a -> List a
sortOn =
    List.sortBy


{-| Inserts an element into a list at the first position where it is less
than or equal to the next element. In particular, if the list is sorted
before the call, the result will also be sorted. It is a special case of
[`insertBy`](#insertBy), which allows the programmer to supply their own
comparison function.


## Examples

    insert -1 [ 1, 2, 3 ] == [ -1, 1, 2, 3 ]

    insert 'd' (String.toList "abcefg") == String.toList "abcdefg"

    insert 4 [ 1, 2, 3, 5, 6, 7 ] == [ 1, 2, 3, 4, 5, 6, 7 ]

-}
insert : comparable -> List comparable -> List comparable
insert a list =
    case list of
        [] ->
            [ a ]

        x :: xs ->
            if a <= x then
                a :: x :: xs

            else
                x :: insert a xs



-- Generalized functions


{-| Behaves just like `nub`, except it uses a user-supplied equality predicate.


## Examples

    nubBy (\x y -> modBy 3 x == modBy 3 y) [ 1, 2, 4, 5, 6 ] == [ 1, 2, 6 ]

    nubBy (/=) [ 2, 7, 1, 8, 2, 8, 1, 8, 2, 8 ] == [ 2, 2, 2 ]

    nubBy (>) [ 1, 2, 3, 2, 1, 5, 4, 5, 3, 2 ] == [ 1, 2, 3, 5, 5 ]

-}
nubBy : (a -> a -> Bool) -> List a -> List a
nubBy f list =
    case list of
        [] ->
            []

        x :: xs ->
            x :: nubBy f (filter (not << f x) xs)


{-| Behaves like [`delete`](#delete), but takes a user-supplied equality
predicate.


## Examples

    deleteBy (<=) 4 (range 1 10) == [ 1, 2, 3, 5, 6, 7, 8, 9, 10 ]

    deleteBy (/=) 5 [ 5, 5, 4, 3, 5, 2 ] == [ 5, 5, 3, 5, 2 ]

-}
deleteBy : (a -> a -> Bool) -> a -> List a -> List a
deleteBy f a list =
    case list of
        [] ->
            []

        x :: xs ->
            if f a x then
                xs

            else
                x :: deleteBy f a xs


{-| Return the first list with the first occurrence of each element of
the second list removed.


## Properties

    deleteFirsts == deleteFirstsBy (==)


## Examples

    deleteFirstsBy (>) (range 1 10) [ 3, 4, 5 ] == [ 4, 5, 6, 7, 8, 9, 10 ]

    deleteFirstsBy (/=) (range 1 10) [ 1, 3, 5 ] == [ 4, 5, 6, 7, 8, 9, 10 ]

-}
deleteFirstsBy : (a -> a -> Bool) -> List a -> List a -> List a
deleteFirstsBy f xs ys =
    let
        go x list =
            case list of
                [] ->
                    []

                l :: ls ->
                    if f x l then
                        ls

                    else
                        l :: go x ls
    in
    foldl go xs ys


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        _ ->
            True


{-| Return the list union of the two lists using given equality function.

If equal elements are present in both lists, an element from the first list
will be used. If the second list contains equal elements, only the first one
will be retained:

    type Arg a b = Arg a b
    eq (Arg a1 b1) (Arg a2 b2) = a1 == a2
    unionBy eq [Arg () "dog"] [Arg () "cow"] == [Arg () "dog"]
    unionBy eq [] [Arg () "dog", Arg () "cow"] == [Arg () "dog"]


## Examples

    unionBy (>) [ 3, 4, 5 ] [ 1, 2, 3, 4, 5, 6 ] == [ 3, 4, 5, 4, 5, 6 ]

-}
unionBy : (a -> a -> Bool) -> List a -> List a -> List a
unionBy f alist blist =
    alist ++ foldl (deleteBy f) (nubBy f blist) alist


{-| Return the list intersection of two lists using given equality function.

If equal elements are present in both lists, an element from the first list
will be used, and all duplicates from the second list quashed:

    type Arg a b = Arg a b
    eq (Arg a1 b1) (Arg a2 b2) = a1 == a2
    intersect [Arg () "dog"] [Arg () "cow", Arg () "cat"] == [Arg () "dog"]

-}
intersectBy : (a -> a -> Bool) -> List a -> List a -> List a
intersectBy f alist blist =
    case alist of
        [] ->
            []

        x :: xs ->
            if isJust (find (f x) blist) then
                x :: intersect xs blist

            else
                intersect xs blist


{-| Group adjacent elements according to the given equality function.

When a supplied relation is not transitive, it is important to remember that
equality is checked against the first element in the group, not against the
nearest neighbour:

    groupBy (\a b -> b - a < 5) (range 0 19)
        == [ [ 0, 1, 2, 3, 4 ]
           , [ 5, 6, 7, 8, 9 ]
           , [ 10, 11, 12, 13, 14 ]
           , [ 15, 16, 17, 18, 19 ]
           ]


## Examples

    groupBy (/=) [1, 1, 1, 2, 3, 1, 4, 4, 5
        == [[1],[1],[1,2,3],[1,4,4,5]]

    groupBy (>) [1, 3, 5, 1, 4, 2, 6, 5, 4]
        == [[1],[3],[5,1,4,2],[6,5,4]]

    groupBy (const not) [True, False, True, False, False, False, True]
        == [[True,False],[True,False,False,False],[True]]

-}
groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy f list =
    let
        go : List (List a) -> List a -> List (List a)
        go grs xs =
            case xs of
                [] ->
                    reverse grs

                x :: xss ->
                    case grs of
                        [] ->
                            go [ [ x ] ] xss

                        gr :: grss ->
                            if Just True == Maybe.map (\m -> f m x) (head gr) then
                                go ((gr ++ [ x ]) :: grss) xss

                            else
                                go ([ x ] :: grs) xss
    in
    go [] list



-- User-supplied comparison


{-| Sort values by a derived property.

    alice = { name="Alice", height=1.62 }
    bob   = { name="Bob"  , height=1.85 }
    chuck = { name="Chuck", height=1.76 }

    sortBy .name   [chuck,alice,bob] == [alice,bob,chuck]
    sortBy .height [chuck,alice,bob] == [alice,chuck,bob]

    sortBy String.length ["mouse","cat"] == ["cat","mouse"]

-}
sortBy : (a -> a -> Order) -> List a -> List a
sortBy =
    List.sortWith


{-| Insert a value into a list using the given ordering function.


## Examples

    insertBy (\x y -> compare (length x) (length y))
        [ 1, 2 ]
        [ [ 1 ], [ 1, 2, 3 ], [ 1, 2, 3, 4 ] ]
        == [ [ 1 ], [ 1, 2 ], [ 1, 2, 3 ], [ 1, 2, 3, 4 ] ]

-}
insertBy : (a -> a -> Order) -> a -> List a -> List a
insertBy cmp a list =
    case list of
        [] ->
            [ a ]

        x :: xs ->
            case cmp a x of
                LT ->
                    a :: x :: xs

                EQ ->
                    a :: x :: xs

                GT ->
                    x :: insertBy cmp a xs


{-| The largest element of a list if exists with respect to the given
comparison function. If there are multiple largest elements, the rightmost
of them is chosen.


## Examples

    maximumBy (\x y -> compare (String.length x) (String.length y))
        [ "Hello", "World", "!", "Longest", "bar" ]
        == Just "Longest"

-}
maximumBy : (a -> a -> Order) -> List a -> Maybe a
maximumBy cmp list =
    let
        go m xs =
            case xs of
                [] ->
                    m

                v :: vs ->
                    if cmp m v == LT then
                        go v vs

                    else
                        go m vs
    in
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (go x xs)


{-| The least element of a list if exists with respect to the given
comparison function. If there are multiple least elements, the leftmost
of them is chosen.


## Examples

    minimumBy (\x y -> compare (String.length x) (String.length y))
        [ "Hello", "World", "!", "Longest", "bar" ]
        == Just "!"

-}
minimumBy : (a -> a -> Order) -> List a -> Maybe a
minimumBy cmp list =
    let
        go m xs =
            case xs of
                [] ->
                    m

                v :: vs ->
                    if cmp m v == GT then
                        go v vs

                    else
                        go m vs
    in
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (go x xs)


{-| An overloaded version of `length`. In particular, instead of returning an
`Int`, it returns `Float` type.


## Examples

    (genericLength [ 1, 2, 3 ] == 3) == True

    (genericLength [ 1, 2, 3 ] == 3.0) == True

-}
genericLength : List a -> number
genericLength list =
    case list of
        [] ->
            0

        _ :: xs ->
            1 + genericLength xs
