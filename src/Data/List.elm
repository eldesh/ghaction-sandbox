module Data.List exposing
    ( and
    , drop
    , filter
    , filterMap
    , foldl
    , foldr
    , head
    , indexedMap
    , isEmpty
    , length
    , map
    , or
    , partition
    , range
    , repeat
    , reverse
    , scanl
    , singleton
    , sort
    , sortBy
    , sortWith
    , tail
    , take
    , uncons
    , unzip
    )

import List



-- Create


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


uncons : List a -> Maybe ( a, List a )
uncons list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


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
scanl f init list =
    case list of
        [] ->
            [ init ]

        x :: xs ->
            init :: scanl f (f init x) xs
