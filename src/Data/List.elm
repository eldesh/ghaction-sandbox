module Data.List exposing
    ( and
    , map
    , or
    , scanl
    , uncons
    )

import List exposing (map)


map =
    List.map


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
            [init]

        x :: xs ->
            init :: scanl f (f init x) xs
