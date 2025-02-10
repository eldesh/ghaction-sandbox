module Data.List exposing (List)


type List a
    = Nil
    | Cons a (List a)
