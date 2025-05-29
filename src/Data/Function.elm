module Data.Function exposing (id, const, flip, on, applyWhen, fix)

{-| Small but handy combinators for building and transforming functions.

These helpers make it easy to compose complex behaviour on the spot,
so you can avoid writing ad‑hoc lambdas or intermediate definitions.

@docs id, const, flip, on, applyWhen, fix

-}


{-| Return the given value unchanged.
-}
id : a -> a
id x =
    x


{-| Return the first argument, ignoring the second.
-}
const : a -> b -> a
const x _ =
    x


{-| Return a new function whose first two arguments are swapped.
-}
flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x


{-| The least fixed point of the function f, i.e. the least defined x
such that f x = x.


## Examples

    fix
        (\rec n ->
            if n <= 1 then
                1

            else
                n * rec (n - 1)
        )
        5
        == 120

    map
        (fix
            (\rec n ->
                if n < 2 then
                    n

                else
                    rec (n - 1) + rec (n - 2)
            )
        )
        (range 1 10)
        == [ 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 ]

-}
fix : ((a -> b) -> (a -> b)) -> a -> b
fix f x =
    f (fix f) x


{-| Apply a projection to two values and then combine the results with the given binary function.

Equivalent to `\x y -> f (p x) (p y)`.

-}
on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on f p x y =
    f (p x) (p y)


{-| Apply the function to the value only when the first argument is `True`
otherwise return the value unchanged.
-}
applyWhen : Bool -> (a -> a) -> a -> a
applyWhen c f v =
    if c then
        f v

    else
        v
