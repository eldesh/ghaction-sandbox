module Mat2d exposing
    ( Mat2d
    , fromList
    , mat2d
    , toList
    )

import Data.List exposing (length)
import Fuzz exposing (Fuzzer, andThen, constant, filter, list, listOfLength)
import Maybe


type Mat2d k
    = Mat2d (List (List k))


toList : Mat2d k -> List (List k)
toList (Mat2d xss) =
    xss


fromList : List (List k) -> Maybe (Mat2d k)
fromList xss =
    let
        go len list =
            case list of
                [] ->
                    True

                ys :: yss ->
                    if len == length ys then
                        go len yss

                    else
                        False

        isrect =
            case xss of
                [] ->
                    False

                xs :: xsss ->
                    go (length xs) xsss
    in
    if isrect then
        Just (Mat2d xss)

    else
        Nothing


mat2d : Fuzzer a -> Fuzzer (Mat2d a)
mat2d a =
    andThen
        (\xs ->
            andThen
                (\xss ->
                    constant (Mat2d (xs :: xss))
                )
                (list (listOfLength (List.length xs) a))
        )
        (filter (not << List.isEmpty) (list a))
