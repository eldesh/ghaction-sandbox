module FuzzTest exposing (..)

import Data.List exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, andThen, bool, constant, int, list, listOfLengthBetween, pair, string)
import Mat2d exposing (..)
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


suite : Test
suite =
    describe "Extentions"
        [ fuzz int "singleton x <| length is 1" <|
            \int ->
                int |> singleton
                    |> List.length
                    |> Expect.equal 1
        , fuzz (list int) "reverse . reverse is id" <|
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
        , fuzz (mat2d int) "transpose . transpose is id for the rectangular list of lists" <|
            \mat ->
                mat
                    |> toList
                    |> transpose
                    |> transpose
                    |> Expect.equal (toList mat)
        , fuzz (listOfLengthBetween 0 10 int) "all `subsequences' are subsequences" <|
            \xs ->
                subsequences xs
                    |> all (\list -> isSubsequenceOf list xs)
                    |> Expect.equal True
        , fuzz (listOfLengthBetween 0 5 int) "forall ys in permutations xs, sort ys == sort xs" <|
            \xs ->
                xs
                    |> permutations
                    |> all (\ys -> sort ys == sort xs)
                    |> Expect.equal True
        , fuzz (constant []) "and [] is True" <|
            \xs ->
                xs
                    |> and
                    |> Expect.equal True
        , fuzz (list bool) "and xs == all ((==) True)" <|
            \xs ->
                xs
                    |> and
                    |> Expect.equal (all ((==) True) xs)
        , fuzz (constant []) "or [] is False" <|
            \xs ->
                xs
                    |> or
                    |> Expect.equal False
        , fuzz (list bool) "or xs == any ((==) True)" <|
            \xs ->
                xs
                    |> or
                    |> Expect.equal (any ((==) True) xs)
        ]
