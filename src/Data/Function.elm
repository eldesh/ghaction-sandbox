module Data.Function exposing (id, const, flip, on, applyWhen)

{-| Small but handy combinators for building and transforming functions.

These helpers make it easy to compose complex behaviour on the spot,
so you can avoid writing ad‑hoc lambdas or intermediate definitions.

@docs id, const, flip, on, applyWhen

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
