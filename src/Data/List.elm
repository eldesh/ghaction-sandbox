module Data.List exposing
    ( and
    , break
    , compareLength
    , delete
    , deleteBy
    , deleteFirsts
    , deleteFirstsBy
    , drop
    , dropWhile
    , dropWhileEnd
    , elem
    , elemIndex
    , elemIndicies
    , filter
    , filterMap
    , find
    , findIndex
    , findIndicies
    , foldl
    , foldr
    , group
    , groupBy
    , head
    , indexedMap
    , init
    , inits
    , intercalate
    , intersect
    , intersectBy
    , isEmpty
    , isInfixOf
    , isPrefixOf
    , isSubsequenceOf
    , isSuffixOf
    , length
    , lookup
    , map
    , mapAccumL
    , mapAccumR
    , notElem
    , nub
    , nubBy
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
    , span
    , splitAt
    , stripPrefix
    , subsequences
    , tail
    , tails
    , take
    , takeWhile
    , transpose
    , uncons
    , unfoldr
    , union
    , unionBy
    , unsnoc
    , unzip
    , unzip3
    , zip
    , zip3
    , zipWith
    , zipWith3
    )

import List
import Maybe



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

        x :: xs ->
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
    isPrefixOf suffix rlist


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
elem a list =
    case list of
        [] ->
            False

        x :: xs ->
            x == a || elem a xs


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
sortOn f =
    sortWith (\x y -> compare (f x) (f y))



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
