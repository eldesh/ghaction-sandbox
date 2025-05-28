module Data.Ord exposing (comparing, clamp)

{-| Utilities working on comparable values.

@docs comparing, clamp

-}


{-| Compare given elements by ordering function.
-}
comparing : (b -> comparable) -> b -> b -> Order
comparing p x y =
    compare (p x) (p y)


{-| Cut off the upper and lower bounds.
-}
clamp : ( comparable, comparable ) -> comparable -> comparable
clamp ( low, high ) x =
    min high (max x low)
