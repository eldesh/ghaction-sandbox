# alt-core-elm

**alt-core-elm** is an extension/alternative to the official
[`elm/core`](https://package.elm-lang.org/packages/elm/core/1.0.5/) package.
Its goal is to offer additional data-structure utilities and
combinator functions while following Elm’s design principles.

## Modules

- Data.Function

    Small but handy combinators for building and transforming functions.
    These helpers make it easy to compose complex behaviour on the spot,
    so you can avoid writing ad‑hoc lambdas or intermediate definitions.

- Data.List

    Expose all standard list functions except for `(::)` from `List` module,
    and provides many other utility functions on lists.

    Extended toolbox for working with lists: everything from `elm/core`
    `List` except for `(::)` plus Haskell-inspired helpers such as
    `nub`, `intercalate`, and `groupBy`, all implemented the Elm way.

- Data.Maybe

    Expose all standard maybe functions from `Maybe` module,
    and provides many other utility functions on maybes.

- Data.Ord

    Utilities working on comparable values.

- Data.Result

    Expose all standard functions from `Result` module,
    and provides many other utility functions on results.


