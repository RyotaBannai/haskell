### Haskell repo

-   [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
-   `::` is read as `"has type of"`
-   `Explicit types` are always denoted with the first letter in `capital case`.
-   Because it's not in `capital case` it's actually a `type variable`. That means that a can be of any type.
-   Functions that have `type variables` are called `polymorphic functions`.
-   Everything before the `=>` symbol is called a class constraint.
    -   `:t (==)`, results in
    -   `(==) :: Eq a => a -> a -> Bool`
    -   We can read the type declaration like this: the equality function `takes any two values` that are of the same type and returns a Bool. The type of those two values must be `a member of the Eq class` (this was the `class constraint`).
    -   All standard Haskell types except for `IO` (the type for dealing with input and output) and `functions` are `a part of the Eq typeclass`.
