module Data.List exposing
    ( append, head, last, tail, init, uncons, unsnoc, null, length, compareLength
    , replicate
    , map, reverse, intersperse, intercalate, transpose, subsequences, permutations
    , foldl, foldr
    , concat, concatMap, and, or, any, all, sum, product, minimum
    , scanl, scanr
    , mapAccumL, mapAccumR
    , iterate, repeat
    , unfoldr
    , take, drop, splitAt, takeWhile, dropWhile, dropWhileEnd, span, break, stripPrefix, group, inits, tails
    , isPrefixOf, isSuffixOf, isInfixOf, isSubsequenceOf
    , elem, notElem, lookup
    , find, filter, partition
    , elemIndex, elemIndicies, findIndex, findIndicies
    , zip, zip3, zipWith, zipWith3, unzip, unzip3
    , nub, delete, deleteFirsts, union, intersect
    , sort, sortOn, insert
    , nubBy, deleteBy, deleteFirstsBy, unionBy, groupBy
    , sortBy, sortWith, insertBy, maximumBy, minimumBy
    , genericLength
    , filterMap, indexedMap, intersectBy, isEmpty, map2, map3, map4, map5, maximum, member, range, singleton
    )

{-| This library fills a bunch of important niches in Elm. A `Maybe` can help
you with optional arguments, error handling, and records with optional fields.


# Definition

[List](https://package.elm-lang.org/packages/elm/core/latest/List)


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

@docs elemIndex, elemIndicies, findIndex, findIndicies


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


singleton : a -> List a
singleton =
    List.singleton


repeat : Int -> a -> List a
repeat =
    List.repeat


range : Int -> Int -> List Int
range =
    List.range



-- Transform


map : (a -> b) -> List a -> List b
map =
    List.map


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap =
    List.indexedMap


foldl : (a -> b -> b) -> b -> List a -> b
foldl =
    List.foldl


foldr : (a -> b -> b) -> b -> List a -> b
foldr =
    List.foldr


filter : (a -> Bool) -> List a -> List a
filter =
    List.filter


filterMap : (a -> Maybe b) -> List a -> List b
filterMap =
    List.filterMap



-- Utilities


length : List a -> Int
length =
    List.length


reverse : List a -> List a
reverse =
    List.reverse


member : a -> List a -> Bool
member =
    List.member


all : (a -> Bool) -> List a -> Bool
all =
    List.all


any : (a -> Bool) -> List a -> Bool
any =
    List.any


maximum : List comparable -> Maybe comparable
maximum =
    List.maximum


minimum : List comparable -> Maybe comparable
minimum =
    List.minimum


sum : List number -> number
sum =
    List.sum


product : List number -> number
product =
    List.product



-- Combine


append : List a -> List a -> List a
append =
    List.append


concat : List (List a) -> List a
concat =
    List.concat


concatMap : (a -> List b) -> List a -> List b
concatMap =
    List.concatMap


intersperse : a -> List a -> List a
intersperse =
    List.intersperse


map2 : (a -> b -> result) -> List a -> List b -> List result
map2 =
    List.map2


map3 : (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 =
    List.map3


map4 : (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
map4 =
    List.map4


map5 : (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
map5 =
    List.map5



-- Sort


sort : List comparable -> List comparable
sort =
    List.sort


sortWith : (a -> a -> Order) -> List a -> List a
sortWith =
    List.sortWith



-- Deconstruct


isEmpty : List a -> Bool
isEmpty =
    List.isEmpty


head : List a -> Maybe a
head =
    List.head


tail : List a -> Maybe (List a)
tail =
    List.tail


take : Int -> List a -> List a
take =
    List.take


drop : Int -> List a -> List a
drop =
    List.drop


partition : (a -> Bool) -> List a -> ( List a, List a )
partition =
    List.partition


unzip : List ( a, b ) -> ( List a, List b )
unzip =
    List.unzip



-- Extentions


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: xs ->
            last xs


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


uncons : List a -> Maybe ( a, List a )
uncons list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


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


null : List a -> Bool
null list =
    case list of
        [] ->
            True

        _ :: _ ->
            False


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


intercalate : List a -> List (List a) -> List a
intercalate xs xxs =
    case xxs of
        [] ->
            []

        [ yys ] ->
            yys

        ys :: yys ->
            (ys ++ xs) ++ intercalate xs yys


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


and : List Bool -> Bool
and list =
    case list of
        [] ->
            True

        x :: xs ->
            x && and xs


or : List Bool -> Bool
or list =
    case list of
        [] ->
            False

        x :: xs ->
            x || or xs



-- Building Lists


scanl : (a -> b -> b) -> b -> List a -> List b
scanl f e list =
    case list of
        [] ->
            [ e ]

        x :: xs ->
            e :: scanl f (f x e) xs


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

    ```
    iterate n f x == [x, f x, f (f x), ...]
    ```

    iterate 3 ((+) 1) 0 == [ 0, 1, 2 ]

-}
iterate : Int -> (a -> a) -> a -> List a
iterate n f x =
    if n <= 0 then
        []

    else
        x :: iterate (n - 1) f (f x)


replicate : Int -> a -> List a
replicate =
    repeat


-- Unfolding


unfoldr : (b -> Maybe ( a, b )) -> b -> List a
unfoldr f e =
    case f e of
        Nothing ->
            []

        Just ( x, next ) ->
            x :: unfoldr f next



-- Sublists


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


inits : List a -> List (List a)
inits list =
    case list of
        [] ->
            [ [] ]

        x :: xs ->
            [] :: map ((::) x) (inits xs)


tails : List a -> List (List a)
tails list =
    case list of
        [] ->
            [ [] ]

        _ :: xs ->
            list :: tails xs



-- Predicates


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


isSuffixOf : List a -> List a -> Bool
isSuffixOf suffix list =
    let
        rlist =
            reverse list
    in
    isPrefixOf (reverse suffix) rlist


isInfixOf : List a -> List a -> Bool
isInfixOf xs list =
    isPrefixOf xs list
        || (case list of
                [] ->
                    False

                _ :: ys ->
                    isInfixOf xs ys
           )


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


elem : a -> List a -> Bool
elem =
    member


notElem : a -> List a -> Bool
notElem a list =
    case list of
        [] ->
            True

        x :: xs ->
            x /= a && notElem a xs


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


elemIndicies : a -> List a -> List Int
elemIndicies a list =
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


findIndicies : (a -> Bool) -> List a -> List Int
findIndicies p list =
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


zip : List a -> List b -> List ( a, b )
zip xs ys =
    zipWith (\x y -> ( x, y )) xs ys


zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 xs ys zs =
    zipWith3 (\x y z -> ( x, y, z )) xs ys zs


zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys =
    case ( xs, ys ) of
        ( x :: xss, y :: yss ) ->
            f x y :: zipWith f xss yss

        _ ->
            []


zipWith3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zipWith3 f xs ys zs =
    case ( xs, ys, zs ) of
        ( a :: xss, b :: yss, c :: zss ) ->
            f a b c :: zipWith3 f xss yss zss

        _ ->
            []


unzip3 : List ( a, b, c ) -> ( List a, List b, List c )
unzip3 list =
    let
        go (( xs, ys, zs ) as acc) ls =
            case ls of
                [] ->
                    acc

                ( x, y, z ) :: lss ->
                    go ( x :: xs, y :: ys, z :: zs ) lss
    in
    go ( [], [], [] ) list


insert : comparable -> List comparable -> List comparable
insert a list =
    case list of
        [] ->
            [ a ]

        x :: xs ->
            if a < x then
                a :: x :: xs

            else
                x :: insert a xs


nub : List a -> List a
nub =
    nubBy (==)


delete : a -> List a -> List a
delete =
    deleteBy (==)


{-| corresponds to (\\) in Data.List in the Haskell base.

    deleteFirsts (toList "Hello World!") (toList "ell W") == toList "Hoorld!"

-}
deleteFirsts : List a -> List a -> List a
deleteFirsts =
    deleteFirstsBy (==)


union : List a -> List a -> List a
union =
    unionBy (==)


intersect : List a -> List a -> List a
intersect =
    intersectBy (==)


sortOn : (a -> comparable) -> List a -> List a
sortOn =
    List.sortBy



-- Generalized functions


nubBy : (a -> a -> Bool) -> List a -> List a
nubBy f list =
    case list of
        [] ->
            []

        x :: xs ->
            x :: nubBy f (filter (not << f x) xs)


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


unionBy : (a -> a -> Bool) -> List a -> List a -> List a
unionBy f alist blist =
    alist ++ foldl (deleteBy f) (nubBy f blist) alist


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


sortBy : (a -> a -> Order) -> List a -> List a
sortBy =
    List.sortWith


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


genericLength : List a -> number
genericLength list =
    case list of
        [] ->
            0

        _ :: xs ->
            1 + genericLength xs
