module Data.List exposing
    ( and
    , compareLength
    , drop
    , filter
    , filterMap
    , foldl
    , foldr
    , head
    , indexedMap
    , intercalate
    , isEmpty
    , length
    , map
    , mapAccumL
    , mapAccumR
    , null
    , or
    , partition
    , permutations
    , range
    , repeat
    , reverse
    , scanl
    , scanr
    , singleton
    , sort
    , sortBy
    , sortWith
    , splitAt
    , subsequences
    , tail
    , take
    , transpose
    , uncons
    , unfoldr
    , unsnoc
    , unzip
    )

import List



-- Create


singleton : a -> List a
singleton =
    List.singleton


repeat =
    List.repeat


range =
    List.range



-- Transform


map =
    List.map


indexedMap =
    List.indexedMap


foldl =
    List.foldl


foldr =
    List.foldr


filter =
    List.filter


filterMap =
    List.filterMap



-- Utilities


length =
    List.length


reverse =
    List.reverse


member =
    List.member


all =
    List.all


any =
    List.any


maximum =
    List.maximum


minimum =
    List.minimum


sum =
    List.sum


product =
    List.product



-- Combine


append =
    List.append


concat =
    List.concat


concatMap =
    List.concatMap


intersperse =
    List.intersperse


map2 =
    List.map2


map3 =
    List.map3


map4 =
    List.map4


map5 =
    List.map5



-- Sort


sort =
    List.sort


sortBy =
    List.sortBy


sortWith =
    List.sortWith



-- Deconstruct


isEmpty =
    List.isEmpty


head =
    List.head


tail =
    List.tail


take =
    List.take


drop =
    List.drop


partition =
    List.partition


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

        [ x ] ->
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


scanl : (b -> a -> b) -> b -> List a -> List b
scanl f e list =
    case list of
        [] ->
            [ e ]

        x :: xs ->
            e :: scanl f (f e x) xs


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



-- Unfolding


unfoldr f e =
    case f e of
        Nothing ->
            []

        Just ( x, e_ ) ->
            x :: unfoldr f e_



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





