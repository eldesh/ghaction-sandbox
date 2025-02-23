module FuzzTest exposing (..)

import Data.List exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, andThen, constant, int, list, pair, string)
import Test exposing (..)
import Test.Distribution exposing (..)


isJust xs =
    case xs of
        Nothing ->
            False

        Just _ ->
            True


implication : (a -> Bool) -> (a -> Expectation) -> a -> Expectation
implication p exp a =
    if p a then
        exp a

    else
        Expect.pass


cond : (a -> Bool) -> String -> a -> Expectation
cond p label a =
    if p a then
        Expect.pass

    else
        Expect.fail label


fuzzList : Fuzzer (List a) -> String -> (List a -> Expectation) -> Test
fuzzList =
    fuzzWith
        { runs = 100
        , distribution =
            expectDistribution
                [ ( Test.Distribution.moreThanZero, "empty", \xs -> xs == [] )
                , ( Test.Distribution.moreThanZero, "not empty", \xs -> xs /= [] )
                ]
        }


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList a =
    Fuzz.filter (\xs -> xs /= []) (list a)


nil : Fuzzer (List a)
nil =
    constant []


curry : (( a, b ) -> c) -> a -> b -> c
curry f =
    \a b -> f ( a, b )


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


filterIndex : (a -> Int -> Bool) -> List a -> List a
filterIndex f list =
    let
        go xs i =
            case xs of
                [] ->
                    []

                x :: xxs ->
                    if f x i then
                        x :: go xxs (i + 1)

                    else
                        go xxs (i + 1)
    in
    go list 0


suite : Test
suite =
    describe "Extentions"
        [ fuzz (list int) "reverse . reverse is id" <|
            \ints ->
                ints
                    |> reverse
                    |> reverse
                    |> Expect.equal ints
        , fuzzList (list int) "the last element" <|
            implication (\x -> x /= []) <|
                cond (isJust << last) "last for list which is not empty should return Just value."
        , fuzz (list int) "uncons = head + tail " <|
            \ints ->
                let
                    expected =
                        case ( head ints, tail ints ) of
                            ( Just hd, Just tl ) ->
                                Just ( hd, tl )

                            _ ->
                                Nothing
                in
                ints
                    |> uncons
                    |> Expect.equal expected
        , fuzz (nonEmptyList int) "cons . unsnoc is id" <|
            \ints ->
                ints
                    |> unsnoc
                    |> Maybe.map (\( xs, x ) -> xs ++ [ x ])
                    |> Expect.equal (Just ints)
        , fuzz nil "unsnoc [] is Nothing" <|
            \xs ->
                xs
                    |> unsnoc
                    |> Expect.equal Nothing
        , fuzz (list int) "null is (== [])" <|
            \ints ->
                ints
                    |> null
                    |> Expect.equal (ints == [])
        , fuzz (pair (list int) int) "compareLength is length xs < n" <|
            \( ints, n ) ->
                let
                    len =
                        length ints
                in
                Expect.equal (compare len n) (compareLength ints n)
        , fuzz (pair (list int) (list (list int))) "intercalate xs xss is equivalent to (concat (intersperse xs xss))" <|
            \( xs, xss ) ->
                Expect.equal (Data.List.concat (intersperse xs xss)) (intercalate xs xss)
        ]
