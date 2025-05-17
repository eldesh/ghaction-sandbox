module FuzzTest exposing (..)

import Data.List exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, andThen, bool, constant, int, intRange, list, listOfLengthBetween, pair, string)
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


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


suite : Test
suite =
    describe "Extentions"
        [ fuzz int "singleton x <| length is 1" <|
            \int ->
                int
                    |> singleton
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
        , fuzz int "scanl f e [] == [e]" <|
            \e ->
                scanl (+) e []
                    |> Expect.equal [ e ]
        , fuzz2 int (list int) "last (scanl f e xs) == foldl f e xs" <|
            \e xs ->
                last (scanl (-) e xs)
                    |> Expect.equal (Just <| foldl (-) e xs)
        , fuzz int "scanr f e [] == [e]" <|
            \e ->
                scanr (+) e []
                    |> Expect.equal [ e ]
        , fuzz2 int (list int) "head (scanr f e xs) == foldr f e xs" <|
            \e xs ->
                head (scanr (-) e xs)
                    |> Expect.equal (Just <| foldr (-) e xs)
        , fuzz int "mapAccumL f e [] == (e, [])" <|
            \e ->
                let
                    f s ys =
                        ( s, [] )
                in
                mapAccumL f e []
                    |> Expect.equal ( e, [] )
        , fuzz (list int) "mapAccumL (\\s x-> (x::s, x)) e xs == (rev xs, xs)" <|
            \xs ->
                let
                    f s x =
                        ( x :: s, x )
                in
                mapAccumL f [] xs
                    |> Expect.equal ( reverse xs, xs )
        , fuzz int "mapAccumR f e [] == (e, [])" <|
            \e ->
                let
                    f s ys =
                        ( s, [] )
                in
                mapAccumR f e []
                    |> Expect.equal ( e, [] )
        , fuzz (list int) "mapAccumR (\\s x-> (x::s, x)) e xs == (rev xs, xs)" <|
            \xs ->
                let
                    f s x =
                        ( x :: s, x )
                in
                mapAccumR f [] xs
                    |> Expect.equal ( xs, xs )
        , fuzz2 (intRange 0 10) int "iterate n f == take n << unfoldr f" <|
            \n e ->
                unfoldr
                    (\( m, x ) ->
                        if m > 0 then
                            Just ( x, ( m - 1, x + 1 ) )

                        else
                            Nothing
                    )
                    ( n, e )
                    |> Expect.equal (iterate n ((+) 1) e)
        , fuzz2 int (list int) "splitAt n xs |> uncurry (++) == xs" <|
            \n xs ->
                splitAt n xs
                    |> uncurry (++)
                    |> Expect.equal xs
        , fuzz (list int) "isPrefixOf (dropWhileEnd p xs) xs" <|
            \xs ->
                isPrefixOf (dropWhileEnd ((>) 10) xs) xs
                    |> Expect.equal True
        , fuzz (list int) "span p xs == (takeWhile p xs, dropWhile p xs)" <|
            \xs ->
                let
                    p x =
                        modBy 2 x == 0
                in
                span p xs
                    |> Expect.equal ( takeWhile p xs, dropWhile p xs )
        , fuzz (list int) "break p xs == span (not >> p)" <|
            \xs ->
                let
                    p x =
                        modBy 2 x == 0
                in
                break p xs
                    |> Expect.equal (span (not << p) xs)
        , fuzz2 (list int) (list int) "stripPrefix xs xss |> (++) xs == xss" <|
            \prefix xs ->
                stripPrefix prefix (prefix ++ xs)
                    |> Maybe.map ((++) prefix)
                    |> Expect.equal (Just (prefix ++ xs))
        , fuzz (list int) "group xs |> concat == xs" <|
            \xs ->
                group xs
                    |> Data.List.concat
                    |> Expect.equal xs
        , fuzz (list int) "inits xs == map reverse << scanl (flip (::)) [] <| xs" <|
            \xs ->
                inits xs
                    |> Expect.equal (map reverse << scanl (::) [] <| xs)
        , fuzz (list int) "tails xs == scanr (::) [] <| xs" <|
            \xs ->
                tails xs
                    |> Expect.equal (scanr (::) [] xs)
        ]
